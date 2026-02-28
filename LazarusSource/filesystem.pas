unit FileSystem;

{$MODE Delphi}

{
Copyright (c) 2002-2025 Damien Guard.  

Originally from Disk Image Manager https://github.com/damieng/diskimagemanager
Relicensed for this project under GNU GPL with permission.

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public Licence as published by the Free
Software Foundation; either version 3 of the Licence, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public Licence for more
details.

A copy of the GNU General Public Licence is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.
}

interface

uses
  DSKImage, Utils,
  Classes, SysUtils, FGL;

type
  TCPMFile = class;

  TCPMFileSystem = class(TObject)
  private
    FParentDisk: TDSKDisk;
    FEntryAllocationSize: TDSKAllocationSize;

    procedure TryPlus3DOSHeader(Data: array of byte; DiskFile: TCPMFile);
    procedure TryAMSDOSHeader(Data: array of byte; DiskFile: TCPMFile);
  public
    DiskLabel: string;

    property EntryAllocationSize: TDSKAllocationSize read FEntryAllocationSize;

    function ReadLabelEntry(Data: array of byte; Offset: integer): string;
    function ReadFileEntry(Data: array of byte; Offset: integer): TCPMFile;
    function Directory: TFPGList<TCPMFile>;

    constructor Create(ParentDisk: TDSKDisk);
    destructor Destroy; override;
  end;

  TCPMFile = class(TObject)
  private
    FParentFileSystem: TCPMFileSystem;
  public
    Blocks: TFPGList<integer>;
    FileName: string;
    SizeOnDisk: integer;
    User: byte;
    ReadOnly: boolean;
    System: boolean;
    Archived: boolean;
    FirstSector: TDSKSector;
    EntryIndex: integer;
    Extent: integer;

    HeaderType: string;
    Checksum: boolean;
    Size: integer;
    HeaderSize: integer;
    Meta: string;

    function GetData(WithHeader: boolean): TDiskByteArray;

    constructor Create(ParentFileSystem: TCPMFileSystem);
    destructor Destroy; override;
  end;


implementation

// File system

constructor TCPMFileSystem.Create(ParentDisk: TDSKDisk);
begin
  inherited Create;
  FParentDisk := ParentDisk;
end;

destructor TCPMFileSystem.Destroy;
begin
  FParentDisk := nil;
  inherited Destroy;
end;

const
  FILENAME_OFFSET: integer = 1;
  EXTENSION_OFFSET: integer = 9;
  READONLY_OFFSET: integer = 9;
  SYSTEM_OFFSET: integer = 10;
  ARCHIVED_OFFSET: integer = 11;
  EXTENT_LOW: integer = 12;
  BYTES_IN_LAST_RECORD_OFFSET: integer = 13;
  RECORD_COUNT_OFFSET: integer = 15;
  ALLOCATION_OFFSET: integer = 16;

function CompareByExtent(const Item1, Item2: TCPMFile): integer;
begin
  Result := Item1.Extent - Item2.Extent;
end;

function TCPMFileSystem.Directory: TFPGList<TCPMFile>;
const
  DIR_ENTRY_SIZE: integer = 32;
var
  MaxEntries, SectorOffset: integer;
  Sector: TDSKSector;
  Track: TDSKTrack;
  Spec: TDSKSpecification;
  Index: integer;
  Extents: TFPGList<TCPMFile>;
  PrimaryDiskFile, ExtentEntry, DiskFile: TCPMFile;
begin
  Spec := FParentDisk.Specification;
  case Spec.Format of
    dsFormatCPC_Data:
      MaxEntries := 32;
    dsFormatCPC_System:
      MaxEntries := 32;
    else
      MaxEntries := Spec.DirectoryBlocks * Spec.GetBlockSize() div DIR_ENTRY_SIZE;
  end;

  Result := TFPGList<TCPMFile>.Create;
  Extents := TFPGList<TCPMFile>.Create;

  Track := FParentDisk.GetLogicalTrack(Spec.ReservedTracks);
  if Track = nil then exit;
  Sector := Track.GetFirstLogicalSector();
  if Sector = nil then exit;

  SectorOffset := 0;
  for Index := 0 to MaxEntries - 1 do
  begin
    // Move to next sector if out of data
    if (SectorOffset + DIR_ENTRY_SIZE > Sector.DataSize) then
    begin
      Sector := FParentDisk.GetNextLogicalSector(Sector);
      if Sector = nil then break;
      SectorOffset := 0;
    end;

    if Sector.Data[SectorOffset] < 32 then
    begin
      DiskFile := ReadFileEntry(Sector.Data, SectorOffset);
      DiskFile.EntryIndex := Index;
      if (DiskFile.FileName <> '') and (DiskFile.Blocks.Count > 0) then
      begin
        if DiskFile.Extent = 0 then
          Result.Add(DiskFile)
        else
          Extents.Add(DiskFile);
      end
      else
        DiskFile.Free;
    end;

    if Sector.Data[SectorOffset] = 32 then
      DiskLabel := ReadLabelEntry(Sector.Data, SectorOffset);

    SectorOffset := SectorOffset + DIR_ENTRY_SIZE;
  end;

  Extents.Sort(CompareByExtent);

  while Extents.Count > 0 do
  begin
    ExtentEntry := Extents.First;
    Extents.Remove(ExtentEntry);
    for PrimaryDiskFile in Result do
    begin
      if PrimaryDiskFile.FileName = ExtentEntry.FileName then
      begin
        PrimaryDiskFile.Blocks.AddList(ExtentEntry.Blocks);
        PrimaryDiskFile.SizeOnDisk := PrimaryDiskFile.SizeOnDisk + ExtentEntry.SizeOnDisk;
        PrimaryDiskFile.Size := PrimaryDiskFile.Size + ExtentEntry.Size;
        break;
      end;
    end;
    ExtentEntry.Free;
  end;

  Extents.Free;
end;

function TCPMFileSystem.ReadLabelEntry(Data: array of byte; Offset: integer): string;
begin
  Result := StrBlockClean(Data, Offset + FILENAME_OFFSET, 11);
end;

function TCPMFileSystem.ReadFileEntry(Data: array of byte; Offset: integer): TCPMFile;
var
  Extension: string;
  AllocBlock, AllocOffset: integer;
  Spec: TDSKSpecification;
begin
  Result := TCPMFile.Create(self);
  Spec := FParentDisk.Specification;

  with Result do
  begin
    User := Data[Offset];
    FileName := StrBlockClean(Data, Offset + FILENAME_OFFSET, 8).TrimRight();
    Extension := StrBlockClean(Data, Offset + EXTENSION_OFFSET, 3).TrimRight();
    if FileName = '' then exit;
    if Extension <> '' then
      FileName := FileName + '.' + Extension;

    ReadOnly := Data[Offset + READONLY_OFFSET] > 127;
    System := Data[Offset + SYSTEM_OFFSET] > 127;
    Archived := Data[Offset + ARCHIVED_OFFSET] > 127;
    HeaderSize := 0;

    Extent := Data[Offset + EXTENT_LOW];
    AllocOffset := Offset + ALLOCATION_OFFSET;
    repeat
      begin
        if Spec.AllocationSize = asByte then
        begin
          AllocBlock := Data[AllocOffset];
          AllocOffset := AllocOffset + 1;
        end
        else
        begin
          AllocBlock := Data[AllocOffset] + (Data[AllocOffset + 1] << 8);
          AllocOffset := AllocOffset + 2;
        end;
        if AllocBlock > 0 then
          Result.Blocks.Add(AllocBlock);
      end;
    until (AllocBlock = 0) or (AllocOffset = Offset + 32);

    SizeOnDisk := Blocks.Count * Spec.GetBlockSize();
    Size := Data[Offset + RECORD_COUNT_OFFSET] * 128;
    if Data[Offset + BYTES_IN_LAST_RECORD_OFFSET] > 0 then
      Size := Size - 128 + Data[Offset + BYTES_IN_LAST_RECORD_OFFSET];

    if Blocks.Count > 0 then
    begin
      HeaderType := 'None';
      FirstSector := FParentDisk.GetSectorByBlock(Blocks[0]);
      if FirstSector <> nil then
      begin
        TryPlus3DOSHeader(FirstSector.Data, Result);
        TryAMSDOSHeader(FirstSector.Data, Result);
      end;
    end;
  end;
end;

procedure TCPMFileSystem.TryAMSDOSHeader(Data: array of byte; DiskFile: TCPMFile);
var
  CalcCheckSum: word;
  Idx: integer;
  ExecAddr, LoadAddr: word;
begin
  CalcChecksum := 0;
  for Idx := 0 to 66 do
    CalcChecksum := CalcChecksum + Data[Idx];
  if CalcCheckSum <> Data[67] + (Data[68] << 8) then exit;

  DiskFile.Checksum := True;
  DiskFile.HeaderType := 'AMSDOS';
  DiskFile.Size := Data[64] + (Data[65] << 8) + (Data[66] << 16);
  DiskFile.HeaderSize := 128;

  LoadAddr := Data[21] + (Data[22] << 8);
  ExecAddr := Data[26] + (Data[27] << 8);

  case Data[18] of
    0: DiskFile.Meta := 'BASIC';
    1: DiskFile.Meta := 'BASIC (protected)';
    2: DiskFile.Meta := Format('BINARY %d EXEC %d', [LoadAddr, ExecAddr]);
    3: DiskFile.Meta := Format('BINARY (protected) %d EXEC %d', [LoadAddr, ExecAddr]);
    4: DiskFile.Meta := 'SCREEN';
    5: DiskFile.Meta := 'SCREEN (protected)';
    6: DiskFile.Meta := 'ASCII';
    7: DiskFile.Meta := 'ASCII (protected)';
    else
      DiskFile.Meta := Format('Custom 0x%x', [Data[15]]);
  end;

end;

procedure TCPMFileSystem.TryPlus3DOSHeader(Data: array of byte; DiskFile: TCPMFile);
var
  Sig: string;
  CalcChecksum: byte;
  Idx: integer;
  Param1: word;
begin
  Sig := StrBlockClean(Data, 0, 8);
  if Sig <> 'PLUS3DOS' then exit;

  CalcChecksum := 0;
  for Idx := 0 to 126 do
    CalcChecksum := CalcChecksum + Data[Idx];

  DiskFile.Checksum := CalcChecksum = Data[127];
  DiskFile.HeaderType := Sig;
  DiskFile.Size := Data[11] + (Data[12] << 8) + (Data[13] << 16) + (Data[14] << 24);
  DiskFile.HeaderSize := 128;

  Param1 := Data[18] + (Data[19] << 8);

  case Data[15] of
    0: begin
      if Param1 <> $8000 then
        DiskFile.Meta := Format('BASIC LINE %d', [Param1])
      else
        DiskFile.Meta := 'BASIC';
    end;
    1: DiskFile.Meta := Format('DATA %s(%d)', [char(Data[19] - 64), Data[129] + (Data[130] << 8)]);
    2: DiskFile.Meta := Format('DATA %s$(%d)', [char(Data[19] - 128), Data[129] + (Data[130] << 8)]);
    3: DiskFile.Meta := Format('CODE %d,%d', [Param1,  Data[16] + (Data[17] << 8)]);
    else
      DiskFile.Meta := Format('Custom 0x%x', [Data[15]]);
  end;
end;

// File

constructor TCPMFile.Create(ParentFileSystem: TCPMFileSystem);
begin
  inherited Create;
  FParentFileSystem := ParentFileSystem;
  Blocks := TFPGList<integer>.Create;
end;

destructor TCPMFile.Destroy;
begin
  FParentFileSystem := nil;
  Blocks.Free;
  inherited Destroy;
end;

function TCPMFile.GetData(WithHeader: boolean): TDiskByteArray;
var
  Block, BytesLeft, TargetIdx, BlockSize, SectorsLeft, SectorsPerBlock: integer;
  Disk: TDSKDisk;
  Sector: TDSKSector;
  FileData: TDiskByteArray;
begin
  FileData := nil;
  SetLength(FileData, Size + HeaderSize);

  BytesLeft := Size + HeaderSize;
  TargetIdx := 0;
  Disk := FParentFileSystem.FParentDisk;
  BlockSize := Disk.Specification.GetBlockSize();
  SectorsPerBlock := BlockSize div Disk.Specification.SectorSize;

  // Extract the full data out
  for Block in Blocks do
  begin
    Sector := Disk.GetSectorByBlock(Block);
    if Sector = nil then break;
    SectorsLeft := SectorsPerBlock;
    repeat
      begin
        // Final (possibly partial) sector
        if (BytesLeft < Sector.DataSize) then
        begin
          Move(Sector.Data, FileData[TargetIdx], BytesLeft);
          SectorsLeft := 0;
        end
        else
        begin
          // Full sector
          Move(Sector.Data, FileData[TargetIdx], Sector.DataSize);
          BytesLeft := BytesLeft - Sector.DataSize;
          TargetIdx := TargetIdx + Sector.DataSize;
          Sector := Disk.GetNextLogicalSector(Sector);
          Dec(SectorsLeft);
        end;
      end
    until SectorsLeft = 0;
  end;

  // Strip any headers
  if (not WithHeader) and ((HeaderType = 'PLUS3DOS') or (HeaderType = 'AMSDOS')) then
    Result := Copy(FileData, HeaderSize, Size - HeaderSize)
  else
    Result := Copy(FileData, 0, Size + HeaderSize);
end;

end.
