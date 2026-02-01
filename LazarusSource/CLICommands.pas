unit CLICommands;

{
CLI Command Processor - Command-line interface for disc image operations.
This unit provides command processing without requiring any GUI dependencies.

Copyright (C) 2018-2025 Gerald Holdsworth gerald@hollypops.co.uk

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

{$MODE objFPC}{$H+}

interface

uses
  Classes, SysUtils, DiscImage, DiscImageContext;

type
  { TCLICommandProcessor - Processes CLI commands }
  TCLICommandProcessor = class
  private
    FContext: TDiscImageContext;
    FSettings: TRegistrySettings;
    FOwnsContext: Boolean;
    FOwnsSettings: Boolean;
    FConsoleWidth: Integer;
    // ANSI style modifiers
    FUseColors: Boolean;
    procedure WriteColored(const S: String; const Color: String = '');
    procedure WriteLnColored(const S: String; const Color: String = '');
    function GetCurrentPath: String;
    function ValidFile(const Filename: String; out Dir, Entry: Cardinal): Boolean;
    function BuildFilename(const AFile: TDirEntry): String;
    function GetListOfFiles(const FileSearch: String): TSearchResults;
    procedure ReportFreeSpace;
    function Confirm: Boolean;
    function GetDriveSize(const GivenSize: String): Cardinal;
    procedure ShowHelp;
    procedure ShowImageInfo;
    procedure ListCatalogue(ShowAll: Boolean);
    // Command handlers
    procedure CmdAccess(const Params: TStringArray);
    procedure CmdAdd(const Params: TStringArray);
    procedure CmdCat(const Params: TStringArray);
    procedure CmdCreate(const Params: TStringArray);
    procedure CmdDelete(const Params: TStringArray);
    procedure CmdDir(const Params: TStringArray);
    procedure CmdExtract(const Params: TStringArray);
    procedure CmdFree(const Params: TStringArray);
    procedure CmdInfo(const Params: TStringArray);
    procedure CmdInsert(const Params: TStringArray);
    procedure CmdNew(const Params: TStringArray);
    procedure CmdOpt(const Params: TStringArray);
    procedure CmdRename(const Params: TStringArray);
    procedure CmdSave(const Params: TStringArray);
    procedure CmdTitle(const Params: TStringArray);
    procedure CmdConfig(const Params: TStringArray);
    procedure CmdStatus(const Params: TStringArray);
    procedure CmdSearch(const Params: TStringArray);
  public
    constructor Create; overload;
    constructor Create(AContext: TDiscImageContext; ASettings: TRegistrySettings); overload;
    destructor Destroy; override;

    function ProcessCommand(const Command: TStringArray): Boolean;
    function ParseInput(const Input: String): TStringArray;

    property Context: TDiscImageContext read FContext;
    property Settings: TRegistrySettings read FSettings;
    property UseColors: Boolean read FUseColors write FUseColors;
    property ConsoleWidth: Integer read FConsoleWidth write FConsoleWidth;
  end;

  // ANSI color constants
const
  clNormal  = #$1B'[0m';
  clBold    = #$1B'[1m';
  clRed     = #$1B'[91m';
  clGreen   = #$1B'[92m';
  clYellow  = #$1B'[93m';
  clBlue    = #$1B'[94m';
  clMagenta = #$1B'[95m';
  clCyan    = #$1B'[96m';

  // Disc format strings
  DiscFormats =
    'DFSS80  DFSS40  DFSD80  DFSD40  WDFSS80 WDFSS40 WDFSD80 WDFSD40 ADFSS   ADFSM   ' +
    'ADFSL   ADFSD   ADFSE   ADFSE+  ADFSF   ADFSF+  C1541   C1571   C1581   AMIGADD ' +
    'AMIGAHD CFS     DOS+640 DOS+800 DOS360  DOS720  DOS1440 DOS2880 ';

  DiscNumber: array[1..28] of Integer = (
    $001, $000, $011, $010, $021, $020, $031, $030, $100, $110,
    $120, $130, $140, $150, $160, $170, $200, $210, $220, $400,
    $410, $500, $A00, $A01, $A02, $A03, $A04, $A05);

  BootOptions: array[0..3] of String = ('none', 'load', 'run', 'exec');
  Interleaves: array[0..3] of String = ('auto', 'seq', 'int', 'mux');

implementation

{ TCLICommandProcessor }

constructor TCLICommandProcessor.Create;
begin
  inherited Create;
  FContext := TDiscImageContext.Create;
  FSettings := TRegistrySettings.Create('DiscImageManager');
  FOwnsContext := True;
  FOwnsSettings := True;
  FConsoleWidth := 80;
  FUseColors := True;
end;

constructor TCLICommandProcessor.Create(AContext: TDiscImageContext;
  ASettings: TRegistrySettings);
begin
  inherited Create;
  FContext := AContext;
  FSettings := ASettings;
  FOwnsContext := False;
  FOwnsSettings := False;
  FConsoleWidth := 80;
  FUseColors := True;
end;

destructor TCLICommandProcessor.Destroy;
begin
  if FOwnsContext then
    FContext.Free;
  if FOwnsSettings then
    FSettings.Free;
  inherited Destroy;
end;

procedure TCLICommandProcessor.WriteColored(const S: String; const Color: String);
begin
  if FUseColors and (Color <> '') then
    Write(Color + S + clNormal)
  else
    Write(S);
end;

procedure TCLICommandProcessor.WriteLnColored(const S: String; const Color: String);
begin
  WriteColored(S, Color);
  WriteLn;
end;

function TCLICommandProcessor.GetCurrentPath: String;
begin
  if FContext.Image.FormatNumber <> diInvalidImg then
    Result := FContext.Image.GetParent(FContext.CurrentDir)
  else
    Result := '';
end;

function TCLICommandProcessor.ValidFile(const Filename: String;
  out Dir, Entry: Cardinal): Boolean;
var
  Temp: String;
begin
  if FContext.Image.FileExists(Filename, Dir, Entry) then
    Result := True
  else
  begin
    Temp := FContext.Image.GetParent(FContext.CurrentDir) +
            FContext.Image.GetDirSep(FContext.Image.Disc[FContext.CurrentDir].Partition) +
            Filename;
    Result := FContext.Image.FileExists(Temp, Dir, Entry);
  end;
end;

function TCLICommandProcessor.BuildFilename(const AFile: TDirEntry): String;
begin
  Result := '';
  if AFile.Parent <> '' then
    Result := AFile.Parent +
              FContext.Image.GetDirSep(FContext.Image.Disc[FContext.CurrentDir].Partition);
  Result := Result + AFile.Filename;
end;

function TCLICommandProcessor.GetListOfFiles(const FileSearch: String): TSearchResults;
var
  FileDetails: TDirEntry;
begin
  ResetDirEntry(FileDetails);
  FileDetails.Filename := FileSearch;
  FileDetails.Parent := FContext.Image.GetParent(FContext.CurrentDir);
  Result := FContext.Image.FileSearch(FileDetails);
end;

procedure TCLICommandProcessor.ReportFreeSpace;
var
  Free, Used, Total: QWord;
begin
  Free := FContext.Image.FreeSpace(FContext.Image.Disc[FContext.CurrentDir].Partition);
  Total := FContext.Image.DiscSize(FContext.Image.Disc[FContext.CurrentDir].Partition);
  Used := Total - Free;
  WriteColored(IntToStr(Free), clBold);
  Write(' bytes free. ');
  WriteColored(IntToStr(Used), clBold);
  Write(' bytes used. ');
  WriteColored(IntToStr(Total), clBold);
  WriteLn(' bytes total.');
end;

function TCLICommandProcessor.Confirm: Boolean;
var
  Response: String;
begin
  Result := True;
  if FContext.HasChanged then
  begin
    Result := False;
    WriteLn('Image has been modified.');
    Write('Are you sure you want to continue? (yes/no): ');
    ReadLn(Response);
    if (Length(Response) > 0) and (LowerCase(Response[1]) = 'y') then
      Result := True;
  end;
end;

function TCLICommandProcessor.GetDriveSize(const GivenSize: String): Cardinal;
begin
  Result := StrToIntDef(GivenSize, 0);
  if UpperCase(RightStr(GivenSize, 1)) = 'M' then
    Result := StrToIntDef(LeftStr(GivenSize, Length(GivenSize) - 1), 0) * 1024;
end;

procedure TCLICommandProcessor.ShowHelp;
begin
  WriteLnColored('Disc Image Manager CLI Help', clBlue + clBold);
  WriteLn;
  WriteLn('Image Commands:');
  WriteLn('  insert <file>      - Open a disc image');
  WriteLn('  new <format>       - Create a new disc image');
  WriteLn('  save [file]        - Save the current image');
  WriteLn('  info               - Show image information');
  WriteLn;
  WriteLn('Navigation:');
  WriteLn('  dir <path>         - Change current directory');
  WriteLn('  cat [all|root]     - Show catalogue listing');
  WriteLn('  free               - Show free space');
  WriteLn;
  WriteLn('File Operations:');
  WriteLn('  add <file>         - Add file(s) to image');
  WriteLn('  extract <file>     - Extract file(s) from image');
  WriteLn('  delete <file>      - Delete file(s) from image');
  WriteLn('  rename <old> <new> - Rename a file');
  WriteLn('  access <file> <a>  - Change file attributes');
  WriteLn('  search <pattern>   - Search for files');
  WriteLn;
  WriteLn('Directory Operations:');
  WriteLn('  create <name>      - Create a new directory');
  WriteLn;
  WriteLn('Disc Properties:');
  WriteLn('  title <name>       - Change disc title');
  WriteLn('  opt <option>       - Set boot option (none/load/run/exec)');
  WriteLn;
  WriteLn('Configuration:');
  WriteLn('  config             - Show configuration options');
  WriteLn('  status             - Show current settings');
  WriteLn;
  WriteLn('General:');
  WriteLn('  help               - Show this help');
  WriteLn('  exit               - Exit the CLI');
  WriteLn('  exittogui          - Exit to GUI mode');
  WriteLn;
  WriteLn('Available Formats for new command:');
  WriteLn('  DFSS80, DFSS40, DFSD80, DFSD40 - Acorn DFS');
  WriteLn('  ADFSS, ADFSM, ADFSL, ADFSD     - Acorn ADFS Floppy');
  WriteLn('  ADFSE, ADFSE+, ADFSF, ADFSF+   - Acorn ADFS Enhanced');
  WriteLn('  ADFSHDD                        - Acorn ADFS Hard Drive');
  WriteLn('  C1541, C1571, C1581            - Commodore');
  WriteLn('  AMIGADD, AMIGAHDD              - Commodore Amiga');
  WriteLn('  CFS                            - Acorn CFS/UEF');
  WriteLn('  DOS360, DOS720, DOS1440        - MS-DOS Floppy');
end;

procedure TCLICommandProcessor.ShowImageInfo;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  WriteLnColored('Image Information', clBlue + clBold);
  WriteLn(StringOfChar('-', 40));
  Write('Format: ');
  WriteLnColored(FContext.Image.FormatString, clBold);
  Write('Filename: ');
  WriteLnColored(FContext.Filename, clBold);
  if FContext.Image.MapTypeString <> '' then
  begin
    Write('Map Type: ');
    WriteLnColored(FContext.Image.MapTypeString, clBold);
  end;
  if FContext.Image.DirectoryTypeString <> '' then
  begin
    Write('Directory Type: ');
    WriteLnColored(FContext.Image.DirectoryTypeString, clBold);
  end;
  if FContext.Image.DoubleSided then
    WriteLn('Sides: Double-sided');
  Write('CRC32: ');
  WriteLnColored(FContext.Image.CRC32, clBold);
  WriteLn;
  ReportFreeSpace;
end;

procedure TCLICommandProcessor.ListCatalogue(ShowAll: Boolean);
var
  StartDir, EndDir, Dir, Entry: Integer;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if ShowAll then
  begin
    StartDir := 0;
    EndDir := Length(FContext.Image.Disc) - 1;
  end
  else
  begin
    StartDir := FContext.CurrentDir;
    EndDir := FContext.CurrentDir;
  end;

  for Dir := StartDir to EndDir do
  begin
    WriteLnColored(StringOfChar('-', FConsoleWidth), clBlue);
    WriteColored('Directory: ', clBold);
    WriteLn(FContext.Image.GetParent(Dir));
    WriteLn('Title: ' + FContext.Image.Disc[Dir].Title);
    WriteLn('Entries: ' + IntToStr(Length(FContext.Image.Disc[Dir].Entries)));
    WriteLn;

    if Length(FContext.Image.Disc[Dir].Entries) > 0 then
    begin
      for Entry := 0 to Length(FContext.Image.Disc[Dir].Entries) - 1 do
      begin
        with FContext.Image.Disc[Dir].Entries[Entry] do
        begin
          // Filename
          Write(Format('%-20s', [Filename]));
          // Attributes
          Write(' (' + Attributes + ')');
          // Directory indicator
          if DirRef >= 0 then
            Write(' <DIR>')
          else
          begin
            // Size
            Write(' ' + Format('%10s', [ConvertToKMG(Length)]));
          end;
          WriteLn;
        end;
      end;
    end;
  end;
end;

// Command handlers

procedure TCLICommandProcessor.CmdAccess(const Params: TStringArray);
var
  Files: TSearchResults;
  I: Integer;
  Temp, Attr: String;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: access <filename> [attributes]', clRed);
    Exit;
  end;

  Attr := '';
  if Length(Params) > 2 then
    Attr := Params[2];

  Files := GetListOfFiles(Params[1]);
  if Length(Files) > 0 then
  begin
    for I := 0 to Length(Files) - 1 do
    begin
      Temp := BuildFilename(Files[I]);
      Write('Changing attributes for ' + Temp + ' ');
      if FContext.Image.UpdateAttributes(Temp, Attr) then
      begin
        WriteLnColored('success.', clGreen);
        FContext.HasChanged := True;
      end
      else
        WriteLnColored('failed.', clRed);
    end;
  end
  else
    WriteLnColored('No files found.', clRed);
end;

procedure TCLICommandProcessor.CmdAdd(const Params: TStringArray);
var
  SearchList: TSearchRec;
  I: Integer;
  FileDetails: TDirEntry;
  Buffer: TDIByteArray;
  F: TFileStream;
  FS: TSearchRec;
  Ok: Boolean;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: add <filename> [filename2] ...', clRed);
    Exit;
  end;

  for I := 1 to Length(Params) - 1 do
  begin
    if FindFirst(Params[I], faAnyFile and not faDirectory, FS) = 0 then
    begin
      repeat
        if (FS.Name <> '.') and (FS.Name <> '..') then
        begin
          Write('Adding file: ''' + ExtractFilePath(Params[I]) + FS.Name + '''');

          // Read file contents
          try
            F := TFileStream.Create(ExtractFilePath(Params[I]) + FS.Name, fmOpenRead or fmShareDenyNone);
            try
              SetLength(Buffer, F.Size);
              if F.Size > 0 then
                F.ReadBuffer(Buffer[0], F.Size);

              // Set up file details
              ResetDirEntry(FileDetails);
              FileDetails.Filename := FS.Name;
              FileDetails.Parent := FContext.Image.GetParent(FContext.CurrentDir);
              FileDetails.Length := F.Size;

              // Try to add the file
              Ok := FContext.Image.WriteFile(FileDetails, Buffer) >= 0;
            finally
              F.Free;
            end;

            if Ok then
            begin
              WriteLnColored(' Success.', clGreen);
              FContext.HasChanged := True;
            end
            else
              WriteLnColored(' Failed.', clRed);
          except
            WriteLnColored(' Error reading file.', clRed);
          end;
        end;
      until FindNext(FS) <> 0;
      FindClose(FS);
    end
    else
      WriteLnColored('File not found: ' + Params[I], clRed);
  end;
end;

procedure TCLICommandProcessor.CmdCat(const Params: TStringArray);
var
  ShowAll: Boolean;
begin
  ShowAll := False;
  if Length(Params) > 1 then
    ShowAll := (LowerCase(Params[1]) = 'all') or (LowerCase(Params[1]) = 'root');

  ListCatalogue(ShowAll);
end;

procedure TCLICommandProcessor.CmdCreate(const Params: TStringArray);
var
  DirName, Parent, Attr: String;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  DirName := 'NewDir';
  if Length(Params) > 1 then
    DirName := Params[1];

  Parent := FContext.Image.GetParent(FContext.CurrentDir);
  Attr := 'DLR';

  Write('Creating directory ''' + DirName + ''' ');
  if FContext.Image.CreateDirectory(DirName, Parent, Attr) >= 0 then
  begin
    WriteLnColored('success.', clGreen);
    FContext.HasChanged := True;
  end
  else
    WriteLnColored('failed.', clRed);
end;

procedure TCLICommandProcessor.CmdDelete(const Params: TStringArray);
var
  I: Integer;
  Dir, Entry: Cardinal;
  Temp: String;
  Ok: Boolean;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: delete <filename> [filename2] ...', clRed);
    Exit;
  end;

  for I := 1 to Length(Params) - 1 do
  begin
    Temp := FContext.Image.GetParent(FContext.CurrentDir) +
            FContext.Image.GetDirSep(FContext.Image.Disc[FContext.CurrentDir].Partition) +
            Params[I];

    Ok := FContext.Image.FileExists(Temp, Dir, Entry);
    if not Ok then
    begin
      Temp := Params[I];
      Ok := FContext.Image.FileExists(Temp, Dir, Entry);
    end;

    if Ok then
    begin
      Write('Deleting ''' + Params[I] + ''' ');
      if FContext.Image.DeleteFile(Temp) then
      begin
        WriteLnColored('success.', clGreen);
        FContext.HasChanged := True;
      end
      else
        WriteLnColored('failed.', clRed);
    end
    else
      WriteLnColored('''' + Params[I] + ''' not found.', clRed);
  end;
end;

procedure TCLICommandProcessor.CmdDir(const Params: TStringArray);
var
  Temp: String;
  Dir, Entry: Cardinal;
  I: Integer;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: dir <path>', clRed);
    Exit;
  end;

  Temp := Params[1];

  // Handle parent directory specifier
  if (Length(Temp) > 0) and (Temp[1] = '^') then
  begin
    if FContext.Image.Disc[FContext.CurrentDir].Parent >= 0 then
      Temp := FContext.Image.GetParent(FContext.Image.Disc[FContext.CurrentDir].Parent) +
              Copy(Temp, 2)
    else
      Temp := FContext.Image.GetParent(0) + Copy(Temp, 2);
  end;

  if FContext.Image.FileExists(Temp, Dir, Entry) then
  begin
    if Dir < Cardinal(Length(FContext.Image.Disc)) then
    begin
      if Entry < Cardinal(Length(FContext.Image.Disc[Dir].Entries)) then
      begin
        if FContext.Image.Disc[Dir].Entries[Entry].DirRef >= 0 then
          FContext.CurrentDir := FContext.Image.Disc[Dir].Entries[Entry].DirRef
        else
        begin
          WriteLnColored('''' + Temp + ''' is a file.', clRed);
          Exit;
        end;
      end
      else
        FContext.CurrentDir := Dir;
    end;
    WriteLn('Directory ''' + FContext.Image.GetParent(FContext.CurrentDir) + ''' selected.');
  end
  else
  begin
    // Check for root directories on DFS
    if FContext.Image.MajorFormatNumber = diAcornDFS then
    begin
      if (Length(Temp) > 1) and (Temp[1] = ':') then
      begin
        I := StrToIntDef(Temp[2], 0);
        if FContext.Image.DoubleSided and (I = 2) then
          I := Length(FContext.Image.Disc) - 1;
        FContext.CurrentDir := I;
        WriteLn('Directory ''' + FContext.Image.GetParent(FContext.CurrentDir) + ''' selected.');
        Exit;
      end;
    end;
    WriteLnColored('''' + Temp + ''' does not exist.', clRed);
  end;
end;

procedure TCLICommandProcessor.CmdExtract(const Params: TStringArray);
var
  Files: TSearchResults;
  I: Integer;
  Temp: String;
  Dir, Entry: Cardinal;
  Buffer: TDIByteArray;
  F: TFileStream;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: extract <filename> [filename2] ...', clRed);
    Exit;
  end;

  Files := nil;
  for I := 1 to Length(Params) - 1 do
    Files := GetListOfFiles(Params[I]);

  if Length(Files) = 0 then
  begin
    WriteLnColored('No files found.', clRed);
    Exit;
  end;

  for I := 0 to Length(Files) - 1 do
  begin
    Temp := BuildFilename(Files[I]);
    if FContext.Image.FileExists(Temp, Dir, Entry) then
    begin
      Write('Extracting ' + Temp + ' ');
      if (Dir < Cardinal(Length(FContext.Image.Disc))) and
         (Entry < Cardinal(Length(FContext.Image.Disc[Dir].Entries))) then
      begin
        if FContext.Image.ExtractFile(Temp, Buffer, Entry) then
        begin
          try
            // Get safe filename
            F := TFileStream.Create(FContext.Image.GetWindowsFilename(Dir, Entry),
                                    fmCreate);
            try
              if Length(Buffer) > 0 then
                F.WriteBuffer(Buffer[0], Length(Buffer));
              WriteLnColored('success.', clGreen);
            finally
              F.Free;
            end;
          except
            WriteLnColored('failed to write.', clRed);
          end;
        end
        else
          WriteLnColored('failed to extract.', clRed);
      end;
    end;
  end;
end;

procedure TCLICommandProcessor.CmdFree(const Params: TStringArray);
begin
  if FContext.Image.FormatNumber = diInvalidImg then
    WriteLnColored('No image loaded.', clRed)
  else
    ReportFreeSpace;
end;

procedure TCLICommandProcessor.CmdInfo(const Params: TStringArray);
begin
  ShowImageInfo;
end;

procedure TCLICommandProcessor.CmdInsert(const Params: TStringArray);
begin
  if not Confirm then
    Exit;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: insert <filename>', clRed);
    Exit;
  end;

  if not FileExists(Params[1]) then
  begin
    WriteLnColored('File not found.', clRed);
    Exit;
  end;

  WriteLn('Reading image...');
  if FContext.LoadImage(Params[1]) then
  begin
    WriteColored(FContext.Image.FormatString, clBold);
    WriteLn(' image read OK.');
    FContext.CurrentDir := 0;
    ReportFreeSpace;
  end
  else
    WriteLnColored('Image not read.', clRed);
end;

procedure TCLICommandProcessor.CmdNew(const Params: TStringArray);
var
  Format: String;
  Index: Integer;
  Ok, Known, NewMap: Boolean;
  HardDriveSize: Cardinal;
  DirType: Byte;
begin
  if not Confirm then
    Exit;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: new <format> [size]', clRed);
    Exit;
  end;

  Known := False;
  Ok := False;
  Format := UpperCase(Params[1]);
  if Length(Params) > 2 then
    Format := Format + UpperCase(Params[2]);

  // ADFS HDD
  if UpperCase(Format) = 'ADFSHDD' then
  begin
    NewMap := False;
    DirType := 0;
    HardDriveSize := 20 * 1024 * 1024;

    if Length(Params) > 3 then
    begin
      if Length(Params[3]) > 3 then
      begin
        if UpperCase(Params[3][1]) = 'N' then NewMap := True;
        if UpperCase(Params[3][2]) = 'N' then DirType := 1;
        if UpperCase(Params[3][2]) = 'B' then DirType := 2;
        if NewMap and (DirType = 0) then DirType := 1;
        if (not NewMap) and (DirType = 2) then DirType := 1;
        HardDriveSize := GetDriveSize(Params[3]);
        if HardDriveSize < 20 * 1024 * 1024 then HardDriveSize := 20 * 1024 * 1024;
        if HardDriveSize > 1000 * 1024 * 1024 then HardDriveSize := 1000 * 1024 * 1024;
        if (not NewMap) and (HardDriveSize > 512 * 1024 * 1024) then
          HardDriveSize := 512 * 1024 * 1024;
      end;
    end;

    Ok := FContext.CreateHDDImage(diAcornADFS, HardDriveSize, DirType, NewMap, True);
    Known := True;
  end;

  // Standard floppy formats
  if not Known then
  begin
    Index := Pos(Format, DiscFormats);
    if Index > 0 then
    begin
      Index := (Index div 8) + 1;
      if (Index >= Low(DiscNumber)) and (Index <= High(DiscNumber)) then
      begin
        Ok := FContext.CreateNewImage(
          DiscNumber[Index] div $100,
          (DiscNumber[Index] div $10) mod $10,
          DiscNumber[Index] mod $10);
        Known := True;
      end;
    end;
  end;

  if Ok then
  begin
    WriteLn(UpperCase(Params[1]) + ' image created OK.');
    ReportFreeSpace;
    FContext.CurrentDir := 0;
  end
  else
  begin
    if Known then
      WriteLnColored('Failed to create image.', clRed)
    else
      WriteLnColored('Unknown format.', clRed);
  end;
end;

procedure TCLICommandProcessor.CmdOpt(const Params: TStringArray);
var
  Opt, Partition: Integer;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: opt <none|load|run|exec> [partition]', clRed);
    Exit;
  end;

  Partition := FContext.Image.Disc[FContext.CurrentDir].Partition;
  if Length(Params) > 2 then
    Partition := StrToIntDef(Params[2], Partition);

  // Find option by name
  Opt := 0;
  while (LowerCase(Params[1]) <> BootOptions[Opt]) and (Opt < High(BootOptions)) do
    Inc(Opt);
  if LowerCase(Params[1]) <> BootOptions[Opt] then
    Opt := StrToIntDef(Params[1], -1);

  if (Opt >= 0) and (Opt <= High(BootOptions)) then
  begin
    Write('Update boot option to ' + UpperCase(BootOptions[Opt]) + ' ');
    if FContext.Image.UpdateBootOption(Opt, Partition) then
    begin
      WriteLnColored('success.', clGreen);
      FContext.HasChanged := True;
    end
    else
      WriteLnColored('failed.', clRed);
  end
  else
    WriteLnColored('Invalid boot option.', clRed);
end;

procedure TCLICommandProcessor.CmdRename(const Params: TStringArray);
var
  Dir, Entry: Cardinal;
  Temp, NewName: String;
  Result: Integer;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 3 then
  begin
    WriteLnColored('Usage: rename <oldname> <newname>', clRed);
    Exit;
  end;

  if ValidFile(Params[1], Dir, Entry) then
  begin
    Temp := FContext.Image.GetParent(FContext.CurrentDir) +
            FContext.Image.GetDirSep(FContext.Image.Disc[FContext.CurrentDir].Partition) +
            Params[1];
    if not FContext.Image.FileExists(Temp, Dir, Entry) then
      Temp := Params[1];

    NewName := Params[2];
    Write('Rename ' + Temp + ' to ' + NewName + ' ');
    Result := FContext.Image.RenameFile(Temp, NewName);
    if Result >= 0 then
    begin
      WriteLnColored('success.', clGreen);
      FContext.HasChanged := True;
    end
    else
      WriteLnColored('failed (' + IntToStr(Result) + ').', clRed);
  end
  else
    WriteLnColored('''' + Params[1] + ''' not found.', clRed);
end;

procedure TCLICommandProcessor.CmdSave(const Params: TStringArray);
var
  Filename: String;
  Compress: Boolean;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) > 1 then
    Filename := Params[1]
  else
    Filename := FContext.Filename;

  if Filename = '' then
  begin
    WriteLnColored('No filename specified.', clRed);
    Exit;
  end;

  Compress := False;
  if Length(Params) > 2 then
    Compress := UpperCase(Params[2]) = 'TRUE';

  if FContext.SaveImage(Filename, Compress) then
    WriteLn('Image saved OK.')
  else
    WriteLnColored('Image failed to save.', clRed);
end;

procedure TCLICommandProcessor.CmdTitle(const Params: TStringArray);
var
  Partition: Integer;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: title <newtitle> [partition]', clRed);
    Exit;
  end;

  Partition := FContext.Image.Disc[FContext.CurrentDir].Partition;
  if Length(Params) > 2 then
    Partition := StrToIntDef(Params[2], Partition);

  Write('Update disc title ');
  if FContext.Image.UpdateDiscTitle(Params[1], Partition) then
  begin
    WriteLnColored('success.', clGreen);
    FContext.HasChanged := True;
  end
  else
    WriteLnColored('failed.', clRed);
end;

procedure TCLICommandProcessor.CmdConfig(const Params: TStringArray);
begin
  WriteLnColored('Configuration Options', clBlue + clBold);
  WriteLn('  AddImpliedAttributes : Boolean - Add RW to inf for DFS/CFS/RFS');
  WriteLn('  Create_DSC           : Boolean - Create *.dsc file with hard drives');
  WriteLn('  CreateINF            : Boolean - Create *.inf file when extracting');
  WriteLn('  DFS_Allow_Blanks     : Boolean - Allow blank filenames in DFS');
  WriteLn('  DFS_Beyond_Edge      : Boolean - Check for files going over disc edge');
  WriteLn('  DFS_Zero_Sectors     : Boolean - Allow DFS images with zero sectors');
  WriteLn('  Open_DOS             : Boolean - Automatically open DOS partitions');
  WriteLn('  Scan_SubDirs         : Boolean - Automatically scan sub-directories');
  WriteLn('  Spark_Is_FS          : Boolean - Treat Spark archives as file system');
  WriteLn;
  WriteLn('Use: config <key> <value> to set a configuration option');
end;

procedure TCLICommandProcessor.CmdStatus(const Params: TStringArray);
begin
  WriteLnColored('Current Settings', clBlue + clBold);
  WriteLn('  CreateINF            : ' + BoolToStr(FContext.CreateINF, 'true', 'false'));
  WriteLn('  AddImpliedAttributes : ' + BoolToStr(FContext.AddImpliedAttributes, 'true', 'false'));
  WriteLn('  ScanSubDirs          : ' + BoolToStr(FContext.ScanSubDirs, 'true', 'false'));
  WriteLn('  OpenDOS              : ' + BoolToStr(FContext.OpenDOS, 'true', 'false'));
  WriteLn('  CreateDSC            : ' + BoolToStr(FContext.CreateDSC, 'true', 'false'));
  WriteLn('  DFSZeroSecs          : ' + BoolToStr(FContext.DFSZeroSecs, 'true', 'false'));
  WriteLn('  DFSBeyondEdge        : ' + BoolToStr(FContext.DFSBeyondEdge, 'true', 'false'));
  WriteLn('  DFSAllowBlank        : ' + BoolToStr(FContext.DFSAllowBlank, 'true', 'false'));
  WriteLn('  SparkIsFS            : ' + BoolToStr(FContext.SparkIsFS, 'true', 'false'));
  WriteLn('  ADFSInterleave       : ' + IntToStr(FContext.ADFSInterleave));
end;

procedure TCLICommandProcessor.CmdSearch(const Params: TStringArray);
var
  Files: TSearchResults;
  I: Integer;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: search <pattern>', clRed);
    Exit;
  end;

  Files := nil;
  for I := 1 to Length(Params) - 1 do
    Files := GetListOfFiles(Params[I]);

  WriteLn(IntToStr(Length(Files)) + ' file(s) found.');

  for I := 0 to Length(Files) - 1 do
    WriteLn('  ' + BuildFilename(Files[I]));
end;

function TCLICommandProcessor.ProcessCommand(const Command: TStringArray): Boolean;
begin
  Result := True; // Continue running

  if Length(Command) = 0 then
    Exit;

  case LowerCase(Command[0]) of
    'access':     CmdAccess(Command);
    'add':        CmdAdd(Command);
    'cat', 'ls':  CmdCat(Command);
    'chdir':      if Length(Command) > 1 then SetCurrentDir(Command[1]);
    'config':     CmdConfig(Command);
    'create':     CmdCreate(Command);
    'delete':     CmdDelete(Command);
    'dir':        CmdDir(Command);
    'exit':       if Confirm then Result := False;
    'exittogui':  Result := False;
    'extract':    CmdExtract(Command);
    'free':       CmdFree(Command);
    'help':       ShowHelp;
    'info':       CmdInfo(Command);
    'insert':     CmdInsert(Command);
    'new':        CmdNew(Command);
    'opt':        CmdOpt(Command);
    'rename':     CmdRename(Command);
    'save':       CmdSave(Command);
    'search':     CmdSearch(Command);
    'status':     CmdStatus(Command);
    'title':      CmdTitle(Command);
    '':           ; // Ignore empty commands
  else
    WriteLnColored('Unknown command: ' + Command[0], clRed);
  end;
end;

function TCLICommandProcessor.ParseInput(const Input: String): TStringArray;
var
  Index, J: Integer;
begin
  // Split the string at each space, unless enclosed by quotes
  Result := Input.Split(' ', '"');

  // Remove blank entries
  if Length(Result) > 0 then
  begin
    Index := 0;
    while Index < Length(Result) do
    begin
      if (Result[Index] = '') or (Result[Index] = ' ') then
      begin
        if Index < Length(Result) - 1 then
          for J := Index + 1 to Length(Result) - 1 do
            Result[J - 1] := Result[J];
        SetLength(Result, Length(Result) - 1);
        Dec(Index);
      end;
      Inc(Index);
    end;
  end;

  // Remove quotes
  if Length(Result) > 0 then
    for Index := 0 to Length(Result) - 1 do
      Result[Index] := Result[Index].DeQuotedString('"')
  else
  begin
    SetLength(Result, 1);
    Result[0] := '';
  end;
end;

end.
