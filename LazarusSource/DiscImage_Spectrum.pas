//++++++++++++++++++ Sinclair Spectrum +3/Amstrad ++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a Spectrum disc
-------------------------------------------------------------------------------}
function TDiscImage.ID_Sinclair: Boolean;
var
 IDStr: String='';
begin
 if FFormat=diInvalidImg then
 begin
  ResetVariables;
  //Check that the image is actually big enough for the smallest DSK
  if GetDataLength>=176*1024 then
  begin
   //Check for signature
   IDStr:=ReadString(0,-$21);
   if LeftStr(IDStr,11)='MV - CPCEMU'       then FFormat:=diSinclair<<4+0;
   if LeftStr(IDStr,16)='EXTENDED CPC DSK'  then FFormat:=diSinclair<<4+1;
   //Check for correct number of sides
   if(ReadByte($31)<>1)and(ReadByte($31)<>2)then FFormat:=diInvalidImg;
   if FFormat<>diInvalidImg then
   begin
    //Set the disc size
    disc_size[0]:=GetDataLength;
    //Set the directory separator
    dir_sep:='/';
    //and the root name
    root_name:='DF';
   end;
  end;
 end;
 Result:=FFormat<>diInvalidImg;
end;

{-------------------------------------------------------------------------------
Read Spectrum Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadSinclairDisc: Boolean;
var
 Index    : Integer=0;
 Index1   : Integer=0;
 Ptr      : Cardinal=0;
 Ref      : Cardinal=0;
 TrackID  : String='';
 S        : Byte=0;
 IDBase   : Byte=0;
 DirTrack : Byte=0;
 DirSector: Byte=0;
 LSide    : Byte=0;
 Temp     : TDirEntry=();
 ReadOnly : Boolean=False;
 Hidden   : Boolean=False;
 Archive  : Boolean=False;
 Deleted  : Boolean=False;
begin
 Result:=False;
 //Ensure that the format has been read and is valid
 if GetMajorFormatNumber=diSinclair then
 begin
  //Get the creator
  FCreator  :=ReadString($22,-14);
  FDSKImage.NumTracks:=ReadByte($30); //Number of tracks per side
  FDSKImage.Sides    :=ReadByte($31); //Number of tracks
  SetLength(FDSKImage.Tracks,0);
  //Setup the free space map
  SetLength(free_space_map,FDSKImage.Sides); //Sides
  for Index:=0 to FDSKImage.Sides-1 do
   SetLength(free_space_map[Index],FDSKImage.NumTracks); //Tracks
  //Reserved tracks, per side
  FDSKImage.Reserved[0]:=-1;
  FDSKImage.Reserved[1]:=-1;
  //Reset some counters
  FDSKImage.Capacity :=0;
  FDSKImage.DataAreas:=0;
  //Set up the container for the track information
  SetLength(FDSKImage.Tracks,FDSKImage.NumTracks*FDSKImage.Sides);
  Ptr:=$100;
  //Get the track information
  for Ref:=0 to Length(FDSKImage.Tracks)-1 do
  begin
   //Tracks are stored interleaved, but we'll have them linear
   Index:=(Ref MOD FDSKImage.Sides)*FDSKImage.NumTracks
         +(Ref DIV FDSKImage.Sides);
   //Track size
   if(FFormat AND$F)=0 then FDSKImage.Tracks[Index].Size:=Read16b($32);
   if(FFormat AND$F)=1 then FDSKImage.Tracks[Index].Size:=ReadByte($34+Index)<<8;
   //Add this to the total capacity
   inc(FDSKImage.Capacity,FDSKImage.Tracks[Index].Size);
   //Make a note of the offset for this track
   FDSKImage.Tracks[Index].Offset:=Ptr;
   //Make sure the track info signature is valid
   TrackID:='';
   for Index1:=$00 to $0B do TrackID:=TrackID+Chr(ReadByte(Ptr+Index1));
   if TrackID='Track-Info'#$0D#$0A then
   begin
    //Boot block?
    FDSKImage.Tracks[Index].Boot  :=CheckForDSKBoot(Ptr+$100);
    //Add this to our counter
    if FDSKImage.Tracks[Index].Boot then inc(FDSKImage.DataAreas);
    //Track number
    FDSKImage.Tracks[Index].Number:=ReadByte(Ptr+$10);
    //Side it is on
    FDSKImage.Tracks[Index].Side  :=ReadByte(Ptr+$11);
    //Size of each sector
    FDSKImage.Tracks[Index].Sector:=ReadByte(Ptr+$14)<<8;
    //Filler byte
    FDSKImage.Tracks[Index].Filler:=ReadByte(Ptr+$17);
    //It is a valid track
    FDSKImage.Tracks[Index].Valid :=True;
    //Set up the container for the sectors
    SetLength(FDSKImage.Tracks[Index].Sectors,ReadByte(Ptr+$15));
    //Get the Sector ID Base
    IDBase:=ReadByte(Ptr+$18+$2);
    SetLength(free_space_map[FDSKImage.Tracks[Index].Side
                            ,FDSKImage.Tracks[Index].Number]
             ,Length(FDSKImage.Tracks[Index].Sectors));
    //Now read the sector information for this track
    for Index1:=0 to Length(FDSKImage.Tracks[Index].Sectors)-1 do
    begin
     //Valid sector? Match size, track and sides
     if (ReadByte(Ptr+$18+Index1*$8+$3)=ReadByte(Ptr+$14))
     and(ReadByte(Ptr+$18+Index1*$8+$0)=ReadByte(Ptr+$10))
     and(ReadByte(Ptr+$18+Index1*$8+$1)=ReadByte(Ptr+$11))then
     begin
      free_space_map[FDSKImage.Tracks[Index].Side
                    ,FDSKImage.Tracks[Index].Number,Index1]:=diFSMBlank;
      //Sector ID
      S:=ReadByte(Ptr+$18+Index1*$8+$2);
      //We need to make sure it is not a duplicate
      if FDSKImage.Tracks[Index].Sectors[S-IDBase].ID<>S then
       FDSKImage.Tracks[Index].Sectors[S-IDBase].ID:=S
      else
      begin //Otherwise, we'll need to add it to the end
       FDSKImage.Tracks[Index].Sectors[Index1].ID:=S;
       S:=Index1+IDBase;
      end;
      //Each sector information contains it's own size
      FDSKImage.Tracks[Index].Sectors[S-IDBase].Size:=
                                                 Read16b(Ptr+$18+Index1*$8+$6);
      //Work out the offset - this is based on the track information
      FDSKImage.Tracks[Index].Sectors[S-IDBase].Offset:=
                                         FDSKImage.Tracks[Index].Offset+$100
                                        +Index1*FDSKImage.Tracks[Index].Sector;
     end;
    end;
    //Move the offset along
    inc(Ptr,FDSKImage.Tracks[Index].Size);
   end else FDSKImage.Tracks[Index].Valid:=False; //Invalid track
  end;
  if FDSKImage.DataAreas=0 then FDSKImage.DataAreas:=FDSKImage.Sides;
  //Calculate the total number of blocks
  FDSKImage.NumBlocks:=(FDSKImage.Capacity div FDSKImage.DataAreas)div$400;
  SetLength(FDisc     ,FDSKImage.DataAreas);
  SetLength(disc_size ,FDSKImage.DataAreas);
  //Set the disc sizes for each area (side)
  if FDSKImage.DataAreas=2 then
  begin
   disc_size[0]:=GetDataLength div 2;
   disc_size[1]:=GetDataLength div 2;
  end;
  //And the free space
  SetLength(free_space,FDSKImage.DataAreas);
  for Index:=0 to FDSKImage.DataAreas-1 do
   free_space[Index]:=disc_size[Index];
  //Intialise the disc name, per area (which will be blank)
  SetLength(disc_name ,FDSKImage.DataAreas);
  //Create each root
  for Index:=0 to FDSKImage.DataAreas-1 do
  begin
   ResetDir(FDisc[Index]);
   FDisc[Index].Directory:='DF'+IntToStr(Index)+':';
   FDisc[Index].BeenRead :=True;
   FDisc[Index].Parent   :=-1;
   SetLength(FDisc[Index].Entries,0);
  end;
  //Read in the boot blocks
  DirTrack:=0;
  FDSKImage.Used:=0;
  //So we go through the tracks and find the ones with boot blocks
  while DirTrack<Length(FDSKImage.Tracks) do
  begin
   if FDSKImage.Tracks[DirTrack].Boot then
   begin
    if FDSKImage.Reserved[FDSKImage.Tracks[DirTrack].Side]=-1 then
     FDSKImage.Reserved[FDSKImage.Tracks[DirTrack].Side]:=
                                                DirTrack
                                               -FDSKImage.NumTracks
                                               *FDSKImage.Tracks[DirTrack].Side;
    DirSector:=0; //Sector counter
    //Where are we?
    Ptr:=FDSKImage.Tracks[DirTrack].Sectors[DirSector].Offset;
    FDSKImage.Tracks[DirTrack].BootSize:=1;
    //Read in the data (which could span multiple sectors)
    while(CheckForDSKBoot(Ptr))and(Ptr<GetDataLength)do
    begin
     //Just makes things easier to read
     LSide :=FDSKImage.Tracks[DirTrack].Side;
     //Mark as system
     free_space_map[LSide,FDSKImage.Tracks[DirTrack].Number,DirSector]
                                                                  :=diFSMSystem;
     //Get the filename, ignoring all spaces
     Temp.Filename:='';
     for Index:=1 to $8 do
      if(ReadByte(Ptr+Index)>32)and(ReadByte(Ptr+Index)<127)then
       Temp.Filename:=Temp.Filename+Chr(ReadByte(Ptr+Index));
     //Now get the extension, removing the top bit and ignoring spaces
     Temp.ShortFileType:='';
     for Index:=$9 to $B do
      if ReadByte(Ptr+Index)AND$7F>32 then
       Temp.ShortFileType:=Temp.ShortFileType+Chr(ReadByte(Ptr+Index)AND$7F);
     //Check the extent is zero (user number of 0xE5 means a deleted file)
     if(ReadByte(Ptr+$C)=$00)or(ReadByte(Ptr)=$E5)then
     begin
      //We can then add a new entry
      Index1:=Length(FDisc[LSide].Entries);
      SetLength(FDisc[LSide].Entries,Index1+1);
      ResetDirEntry(FDisc[LSide].Entries[Index1]);
      FDisc[LSide].Entries[Index1].Length:=0;
     end
     else
     begin
      //Otherwise, we need to find the first entry
      Index1:=0;
      while(Index1<Length(FDisc[LSide].Entries))
        and(FDisc[LSide].Entries[Index1].Filename+'.'
           +FDisc[LSide].Entries[Index1].ShortFileType
            <>Temp.Filename+'.'+Temp.ShortFileType)
        and(Pos('D',FDisc[LSide].Entries[Index1].Attributes)=0)do inc(Index1);
      //But if we fail to find it, then just create a new one anyway
      if Index1>=Length(FDisc[LSide].Entries) then
      begin
       SetLength(FDisc[LSide].Entries,Index1+1);
       ResetDirEntry(FDisc[LSide].Entries[Index1]);
       FDisc[LSide].Entries[Index1].Length:=0;
      end;
     end;
     //Populate the fields
     FDisc[LSide].Entries[Index1].DirRef   :=-1;
     FDisc[LSide].Entries[Index1].TimeStamp:=1;
     //User number (<16)
     FDisc[LSide].Entries[Index1].UserNumber:=ReadByte(Ptr);
     //Filename
     FDisc[LSide].Entries[Index1].Filename  :=Temp.Filename;
     FDisc[LSide].Entries[Index1].Parent    :=root_name+IntToStr(LSide)+':';
     //Read only flag
     ReadOnly  :=(ReadByte(Ptr+$9)AND$80)=$80;
     //Hidden flag
     Hidden    :=(ReadByte(Ptr+$A)AND$80)=$80;
     //Archived flag
     Archive   :=(ReadByte(Ptr+$B)AND$80)=$80;
     //Deleted?
     Deleted   :=(ReadByte(Ptr   )       =$E5)
               or(ReadByte(Ptr+$C)       =$E5);
     FDisc[LSide].Entries[Index1].Attributes:=ConvertDSKAttributes(ReadOnly,
                                                                   Hidden,
                                                                   Archive,
                                                                   Deleted);
     //Extension (this gets added to the filename later on and cleared)
     FDisc[LSide].Entries[Index1].ShortFileType:=Temp.ShortFileType;
     //Reported length
     FDisc[LSide].Entries[Index1].Length:=FDisc[LSide].Entries[Index1].Length
                                         +ReadByte(Ptr+$F)*$80;
     //Which side is it on?
     FDisc[LSide].Entries[Index1].Side:=LSide;
     //Calculate the used space
     if not Deleted then
     begin
      inc(FDSKImage.Used,ReadByte(Ptr+$F)*$80);
      dec(free_space[LSide],ReadByte(Ptr+$F)*$80);
     end;
     //Read in the clusters
     Index:=$10;
     //Are we reading them as 8-bit or 16-bit values?
     if FDSKImage.NumBlocks<256 then S:=0 else S:=1;
     //So, read them until we reach the end or find a 'hole'
     while(Index<$20)
       and((ReadByte(Ptr+Index+S)<<(8*S)OR ReadByte(Ptr+Index))<>$00)do
     begin
      //Add new entry
      Ref:=Length(FDisc[LSide].Entries[Index1].Clusters);
      SetLength(FDisc[LSide].Entries[Index1].Clusters,Ref+1);
      FDisc[LSide].Entries[Index1].Clusters[Ref]:=ReadByte(Ptr+Index+S)<<(8*S)
                                               OR ReadByte(Ptr+Index);
      //Next entry
      inc(Index,S+1);
     end;
     if Length(FDisc[LSide].Entries[Index1].Clusters)>0
      then
      FDisc[LSide].Entries[Index1].HeaderType:=
                        GetDSKHeaderType(
                           GetDSKOffset(FDisc[LSide].Entries[Index1].Clusters[0]
                                       ,FDisc[LSide].Entries[Index1].Side)
                          ,FDisc[LSide].Entries[Index1].Length);
     //Next file
     Inc(Ptr,$20);
     //End of sector?
     if Ptr>=FDSKImage.Tracks[DirTrack].Sectors[DirSector].Offset
            +FDSKImage.Tracks[DirTrack].Sectors[DirSector].Size then
     begin
      //Next sector
      inc(DirSector);
      inc(FDSKImage.Tracks[DirTrack].BootSize);
      //Which could be the next track
      if DirSector>Length(FDSKImage.Tracks[DirTrack].Sectors) then
      begin
       inc(DirTrack);
       DirSector:=0;
      end;
      //Get the offset for this sector
      Ptr:=FDSKImage.Tracks[DirTrack].Sectors[DirSector].Offset;
     end;
    end;
   end;
   //Check next track
   inc(DirTrack);
  end;
  //No boot blocks have been found, so let's create them, per side.
  for Index:=0 to FDSKImage.Sides-1 do
  begin
   if FDSKImage.Reserved[Index]=-1 then
   begin
    //Base it on the sector ID
    case FDSKImage.Tracks[0].Sectors[0].ID of
     $01: FDSKImage.Reserved[Index]:=1; //IBM
     $41: FDSKImage.Reserved[Index]:=2; //Vendor
     $C1: FDSKImage.Reserved[Index]:=0; //Data
     else FDSKImage.Reserved[Index]:=0; //Unknown/Unrecognised
    end;
    FDSKImage.Tracks[FDSKImage.Reserved[Index]].Boot:=True;
    free_space_map[Index,FDSKImage.Reserved[Index],0]:=diFSMSystem; //Mark as system
   end;
   //Fill out the free space map with used sectors for the files
   //We'll also concatenate the filename/extensions while we're here
   if Length(FDisc[Index].Entries)>0 then
    for Index1:=0 to Length(FDisc[Index].Entries)-1 do
    begin
     for Ref:=0 to Length(FDisc[Index].Entries[Index1].Clusters)-1 do
     begin
      //Get the tracka and sector for the first sector of the cluster
      Ptr:=GetDSKTrackSector(FDisc[Index].Entries[Index1].Clusters[Ref],Index);
      //And set as used
      free_space_map[Index,FDSKImage.Tracks[Ptr>>8].Number,Ptr AND $FF]:=diFSMUsed;
      //Repeat for the second
      Ptr:=GetDSKTrackSector(FDisc[Index].Entries[Index1].Clusters[Ref],Index,False);
      free_space_map[Index,FDSKImage.Tracks[Ptr>>8].Number,Ptr AND $FF]:=diFSMUsed;
     end;
     //Concatenate the filename/extension, if need be
     if FDisc[Index].Entries[Index1].ShortFileType<>'' then
      FDisc[Index].Entries[Index1].Filename
                                   :=FDisc[Index].Entries[Index1].Filename
                                    +'.'
                                    +FDisc[Index].Entries[Index1].ShortFileType;
     //Clear the filetype field
     FDisc[Index].Entries[Index1].ShortFileType:='';
    end;
  end;
  //Return a positive result
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Convert attribute flags to a string
-------------------------------------------------------------------------------}
function TDiscImage.ConvertDSKAttributes(ReadOnly,Hidden,Archive,
                                                      Deleted: Boolean): String;
begin
 Result:='';
 if ReadOnly then Result:=Result+'R';
 if Hidden   then Result:=Result+'H';
 if Archive  then Result:=Result+'A';
 if Deleted  then Result:=Result+'D';
end;

{-------------------------------------------------------------------------------
Convert a cluster number and side to offset
-------------------------------------------------------------------------------}
function TDiscImage.GetDSKOffset(Cluster: Word;Side: Byte;
                                                 First: Boolean=True): Cardinal;
var
 LTrack : Byte=0;
 LSector: Byte=0;
 T      : Word=0;
begin
 Side:=Side mod 2;
 T:=GetDSKTrackSector(Cluster,Side,First);
 LTrack :=T>>8;
 LSector:=T AND $FF;
 Result:=FDSKImage.Tracks[LTrack].Sectors[LSector].Offset
        +FDSKImage.Reserved[Side]*FDSKImage.Tracks[0].Size;
end;

{-------------------------------------------------------------------------------
Convert a cluster number and side to track and sector
-------------------------------------------------------------------------------}
function TDiscImage.GetDSKTrackSector(Cluster: Word;Side: Byte;
                                                 First: Boolean=True): Word;
var
 LTrack : Byte=0;
 LSector: Byte=0;
 F      : Byte=0;
begin
 if First then F:=0 else F:=1;
 Side:=Side mod 2;
 LTrack :=((Cluster*2)+F)DIV Length(FDSKImage.Tracks[0].Sectors);
 LTrack :=LTrack+(FDSKImage.NumTracks*Side);
 LSector:=((Cluster*2)+F)MOD Length(FDSKImage.Tracks[0].Sectors);
 Result:=LTrack<<8+LSector;
end;

{-------------------------------------------------------------------------------
Check the offset for a boot block
-------------------------------------------------------------------------------}
function TDiscImage.CheckForDSKBoot(Ptr: Cardinal): Boolean;
var
 LI: Integer=0;
begin
 Result:=((ReadByte(Ptr   )<$10)or(ReadByte(Ptr   )=$E5))
      and((ReadByte(Ptr+$C)<$05)or(ReadByte(Ptr+$C)=$E5));
 if Result then
  for LI:=$1 to $8 do
   Result:=(Result)and(ReadByte(Ptr+LI)>$1F)and(ReadByte(Ptr+LI)<$80);
end;

{-------------------------------------------------------------------------------
Identify file header type
-------------------------------------------------------------------------------}
function TDiscImage.GetDSKHeaderType(Ptr: Cardinal;out len: Cardinal): TDSKHeaderType;
var
 Index: Integer=0;
 Ctr  : Word=0;
begin
 Result:=diNone;
 Ctr:=0;
 len:=0;
 for Index:=0 to $42 do
  inc(Ctr,ReadByte(Ptr+Index));
 if(Ctr=Read16b(Ptr+$43))and(Ctr<>0)then
 begin
  Result:=diSOFT968;
  len:=Read16b(Ptr+$18);
 end
 else
  if (ReadString(Ptr,-8)='PLUS3DOS')
  and(ReadByte(Ptr+8)=$1A)then
  begin
   Ctr:=0;
   for Index:=0 to $7E do
    inc(Ctr,ReadByte(Ptr+Index));
   if(Ctr AND $FF)=ReadByte(Ptr+$7F) then
   begin
    Result:=diPLUS3DOS;
    len:=Read32b(Ptr+$B);
   end;
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
 ptr        : Cardinal=0;
 Index      : Cardinal=0;
 cnt        : Cardinal=0;
 offset     : Cardinal=0;
 entry      : Cardinal=0;
 dir        : Cardinal=0;
 len        : Word=0;
 first      : Byte=0;
 T          : Word=0;
 Lsecsize   : Word=0;
 FileDetails: TDirEntry;
begin
 //Default return result
 Result:=False;
 if FileExists(filename,dir,entry) then
 begin
  FileDetails:=FDisc[dir].Entries[entry];
  if(FileDetails.Length>0)and(Length(FileDetails.Clusters)>0)
  and((FileDetails.Side=0)or(FileDetails.Side=1))then
  begin
   Result:=True;
   //Setup the buffer
   SetLength(buffer,FileDetails.Length);
   //And the pointers
   ptr:=0;
   Index:=0;
   while(ptr<Length(buffer))and(Index<Length(FileDetails.Clusters))do
   begin
    //Two loops per cluster. Each cluster is two sectors.
    for first:=0 to 1 do
    begin
     //Get the offset
     offset:=GetDSKOffset(FileDetails.Clusters[Index]
                         ,FileDetails.Side,first=0);
     //We'll also get the track and sector
     T:=GetDSKTrackSector(FileDetails.Clusters[Index]
                         ,FileDetails.Side,first=0);
     //So we can get the sector size
     Lsecsize:=FDSKImage.Tracks[T>>8].Sectors[T AND$FF].Size;
     //Set the amount of data to transfer
     if Length(buffer)-ptr>=Lsecsize then len:=Lsecsize
                                     else len:=Length(buffer)-ptr;
     //And transfer it.
     for cnt:=0 to len-1 do buffer[ptr+cnt]:=ReadByte(offset+cnt);
     //Next block
     inc(ptr,len);
    end;
    //Next cluster
    inc(Index);
   end;
   //Now we remove the header, if any
   if(FileDetails.HeaderType=diSOFT968)
   or(FileDetails.HeaderType=diPLUS3DOS)then
   begin
    //Get the length (not including header)
    if FileDetails.HeaderType=diSOFT968 then len:=buffer[$18]+buffer[$19]<<8;
    if FileDetails.HeaderType=diPLUS3DOS then
     len:=buffer[$B]+buffer[$C]<<8+buffer[$D]<<16+buffer[$E]<<24;
    //Copy the data downwards
    for ptr:=$80 to Length(buffer)-1 do buffer[ptr-$80]:=buffer[ptr];
    //Reduce the length
    SetLength(buffer,len);
   end;
  end;
 end;
end;
