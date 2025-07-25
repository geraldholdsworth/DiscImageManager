//++++++++++++++++++ Sinclair Spectrum +3/Amstrad ++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a Spectrum disc
-------------------------------------------------------------------------------}
function TDiscImage.ID_Sinclair: Boolean;
begin
 Result:=False;
 if FFormat=diInvalidImg then
 begin
  if GetDataLength>0 then
  begin
   FDSKImage:=TDSKImage.CreateFromFile(FFilename);
   if not FDSKImage.Corrupt then
   begin
    if FDSKImage.FileFormat=diStandardDSK then FFormat:=diSinclair<<4+0;
    if FDSKImage.FileFormat=diExtendedDSK then FFormat:=diSinclair<<4+1;
   end;
   Result:=FFormat<>diInvalidImg;
   if not Result then 
   begin
    FDSKImage.Free;
    FDSKImage:=nil;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Read Spectrum Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadSinclairDisc: Boolean;
var
 LSide  : Integer=0;
 LTrack : Integer=0;
 LSector: Integer=0;
 LFile  : Integer=0;
 LEntry : Integer=0;
 FDSKFS : TDSKFileSystem=nil;
begin
 Result      :=FDSKImage.Disk.Sides>0;
 if Result then
 begin
  root_name   :='A:';
  FDSD        :=FDSKImage.Disk.Sides=2;
  FHasDirs    :=False;
  FDSKImage.Disk.Specification.Identify;
  secsize     :=FDSKImage.Disk.Specification.SectorSize;
  secspertrack:=FDSKImage.Disk.Specification.SectorsPerTrack;
  dir_sep     :='/';
  SetLength(free_space_map,FDSKImage.Disk.Sides);
  SetLength(disc_size     ,1);
  disc_size[0]:=0;
  SetLength(free_space    ,1);
  free_space[0]:=0;
  SetLength(disc_name     ,1);
  SetLength(FDisc         ,1);
  FDSKFS:=TDSKFileSystem.Create(FDSKImage.Disk);
  disc_name[0]:=FDSKFS.DiskLabel;
  //Root
  ResetDir(FDisc[0]);
  FDisc[LSide].Directory:=root_name;
  FDisc[LSide].Partition:=0;
  FDisc[LSide].Parent   :=-1;
  FDisc[LSide].BeenRead :=True;
  for LSide:=0 to FDSKImage.Disk.Sides-1 do
  begin
   //Update the progress indicator
   UpdateProgress('Reading free space map');
   //Free Space Map
   SetLength(free_space_map[LSide],FDSKImage.Disk.Side[LSide].Tracks);
   for LTrack:=0 to FDSKImage.Disk.Side[LSide].Tracks-1 do
   begin
    SetLength(free_space_map[LSide,LTrack],
              FDSKImage.Disk.Side[LSide].Track[LTrack].Sectors);
    for LSector:=0 to FDSKImage.Disk.Side[LSide].Track[LTrack].Sectors-1 do
     case FDSKImage.Disk.Side[LSide].Track[LTrack].Sector[LSector].Status of
      ssUnformatted    : free_space_map[LSide,LTrack,LSector]:=diFSMUnformat;
      ssFormattedBlank :
      begin
       free_space_map[LSide,LTrack,LSector]:=diFSMBlank;
       inc(disc_size[0],secsize);
      end;
      ssFormattedFilled,
      ssFormattedInUse :
      begin
       free_space_map[LSide,LTrack,LSector]:=diFSMUsed;
       inc(disc_size[0] ,secsize);
       inc(free_space[0],secsize);
      end;
     end;
   end;
   free_space[0]:=disc_size[0]-free_space[0];
  end;
  //Files
  for LFile:=0 to FDSKFS.Directory.Count-1 do
  begin
   //Update the progress indicator
   UpdateProgress('Reading '+FDSKFS.Directory[LFile].FileName);
   LEntry:=Length(FDisc[0].Entries);
   SetLength(FDisc[0].Entries,LEntry+1);
   ResetDirEntry(FDisc[0].Entries[LEntry]);
   FDisc[0].Entries[LEntry].Parent     :=FDisc[0].Directory;
   FDisc[0].Entries[LEntry].Filename   :=FDSKFS.Directory[LFile].FileName;
   FDisc[0].Entries[LEntry].Attributes :='';
   if FDSKFS.Directory[LFile].ReadOnly then
    FDisc[0].Entries[LEntry].Attributes:=FDisc[0].Entries[LEntry].Attributes+'R';
   if FDSKFS.Directory[LFile].System   then
    FDisc[0].Entries[LEntry].Attributes:=FDisc[0].Entries[LEntry].Attributes+'S';
   if FDSKFS.Directory[LFile].Archived then
    FDisc[0].Entries[LEntry].Attributes:=FDisc[0].Entries[LEntry].Attributes+'A';
   FDisc[0].Entries[LEntry].Length     :=FDSKFS.Directory[LFile].Size;
   FDisc[0].Entries[LEntry].Side       :=FDSKFS.Directory[LFile].FirstSector.Side;
   FDisc[0].Entries[LEntry].Track      :=FDSKFS.Directory[LFile].FirstSector.Track;
   FDisc[0].Entries[LEntry].Sector     :=FDSKFS.Directory[LFile].FirstSector.Sector;
   FDisc[0].Entries[LEntry].DirRef     :=-1;
   FDisc[0].Entries[LEntry].Sequence   :=LFile;
   FDisc[0].Entries[LEntry].TimeStamp  :=1;
  end;
  FDSKFS.Free;
 end;
end;

{-------------------------------------------------------------------------------
Write a file to Spectrum image
-------------------------------------------------------------------------------}
function TDiscImage.WriteSpectrumFile(file_details: TDirEntry;
                                             var buffer: TDIByteArray): Integer;
begin
 Result:=-1;
end;

{-------------------------------------------------------------------------------
Create a new Spectrum image
-------------------------------------------------------------------------------}
function TDiscImage.FormatSpectrum(minor: Byte): Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Rename a Spectrum file
-------------------------------------------------------------------------------}
function TDiscImage.RenameSpectrumFile(oldfilename: String;var newfilename: String):Integer;
begin
 Result:=-6; //Unsupported in this format
end;

{-------------------------------------------------------------------------------
Delete a Spectrum file
-------------------------------------------------------------------------------}
function TDiscImage.DeleteSinclairFile(filename: String):Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Update attributes on a Spectrum file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateSinclairFileAttributes(filename,attributes: String):Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Change the disc title for a Spectrum disc
-------------------------------------------------------------------------------}
function TDiscImage.UpdateSinclairDiscTitle(title: String): Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Extract a file from a Spectrum image
-------------------------------------------------------------------------------}
function TDiscImage.ExtractSpectrumFile(filename: String;
                                             var buffer: TDIByteArray): Boolean;
var
 FDSKFS : TDSKFileSystem=nil;
 dir    : Cardinal=0;
 entry  : Cardinal=0;
 ref    : Integer=0;
begin
 Result:=False;
 if FileExists(filename,dir,entry)then
 begin
  FDSKFS:=TDSKFileSystem.Create(FDSKImage.Disk);
  buffer:=FDSKFS.Directory[FDisc[dir].Entries[entry].Sequence].GetData(False);
  FDSKFS.Free;
  Result:=True;
 end;
end;
