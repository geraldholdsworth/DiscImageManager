//++++++++++++++++++ Acorn FileStore +++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
IDs an AFS disc
-------------------------------------------------------------------------------}
function TDiscImage.ID_AFS: Boolean;
var
 afspart1,
 afspart2 : Cardinal;
 index    : Integer;
 ok       : Boolean;
 ID       : String;
begin
 if FFormat=$FF then
 begin
  ResetVariables;
  //Is there actually any data?
  if GetDataLength>0 then
  begin
   //Find the AFS partitions
   index:=-$100;
   ID:='';
   while(index<GetDataLength)and(ID<>'AFS0')do
   begin
    inc(index,$100);
    ID:=ReadString(index,-4);
   end;
   if ID='AFS0' then
   begin
    afspart1:=index;
    //Now find the second copy
    ID:='';
    while(index<GetDataLength)and(ID<>'AFS0')do
    begin
     inc(index,$100);
     ID:=ReadString(index,-4);
    end;
    if ID='AFS0' then
    begin
     afspart2:=index;
     //Compare the two partition headers
     ok:=True;
     for index:=0 to $25 do
      if ReadByte(afspart1+index)<>ReadByte(afspart2+index)then ok:=False;
     //Both match? Do we have an ID string?
     if ok then
     begin
      if FFormat=$FF then FFormat:=diAcornFS<<4+$0F; //We'll set to hard disc, if not already set
      FAFSPresent:=True; //And mark as a partition present
      //If it is 640K it may be interleaved
      if GetDataLength=$A0000 then FFormat:=diAcornFS<<4+$02;
      //Set default sector size and sectors per track
      secspertrack:= 16;
      secsize     :=256;
      //Interleaved? Depending on the option
      if FForceInter<=1 then Finterleave:=True; //Auto or force on
      if FForceInter=2  then Finterleave:=False;//Force off
      if FForceInter=0 then //Auto
      begin
       afspart1:=OffsetToAFSDiscAddr(afspart1);
       afspart2:=Read24b(afspart1+$1F)*$100;
       ID:=ReadString(afspart2,-6);
       //Does it point to our ID with interleaving on?
       if ReadString(afspart2,-6)<>'JesMap' then
       begin
        afspart1:=AFSDiscAddrToOffset(afspart1);
        //No, toggle it and try again
        Finterleave:=not Finterleave;
        afspart2:=Read24b(afspart1+$1F)*$100;
        //Still no joy, so revert back
        if ReadString(afspart2,-6)<>'JesMap' then Finterleave:=not Finterleave;
       end;
      end;
      //Convert the header address to an interleaved one, if necessary
      afshead     :={OffsetToAFSDiscAddr(}afspart1;//);
     end else FFormat:=$FF; //Othewise, no format
    end;
   end;
  end;
 end;
 Result:=FFormat>>4=diAcornFS;
end;

{-------------------------------------------------------------------------------
Reads an AFS partition
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadAFSPartition;
var
 d,e      : Integer;
 allocmap : Cardinal;
 startdir : String;
begin
 //Is this an ADFS disc with Acorn FileStore partition?
 if((FFormat>>4=diAcornADFS)and(FAFSPresent))
 or(FFormat>>4=diAcornFS)then
 begin
  if FFormat>>4=diAcornADFS then afshead:=Read24b($0F6)*$100;
  //Confirm that there is a valid AFS partition here
  if ReadString(afshead,-4)='AFS0' then //Should be 'AFS0'
  begin
   //Disc title
   disc_name:=ReadString(afshead+4,-16);
   RemoveSpaces(disc_name); //Minus trailing spaces
   //Tracks
//   afstracks:=Read16b(afshead+$14);
   //Sectors
//   afssectors:=Read24b(afshead+$16);
   //Sectors per track
//   afssecpertrack:=Read16b(afshead+$1A);
   allocmap:=Read24b(afshead+$1F)*$100;
   //Is the ADFS root broken with no entries (i.e. valid?)
   if Length(FDisc)>0 then
    if(FDisc[0].Broken)and(Length(FDisc[0].Entries)=0)then
     SetLength(FDisc,0); //Yes, so overwrite it
   //Add a new entry
   d:=Length(FDisc);
   SetLength(FDisc,d+1);
   //Start the chain by reading the root
   if FFormat>>4=diAcornADFS then startdir:=':AFS$' else startdir:='$';
   FDisc[d]:=ReadAFSDirectory(startdir,allocmap);
   //Now go through the root's entries and read them in
   repeat
    if Length(FDisc[d].Entries)>0 then
     for e:=0 to Length(FDisc[d].Entries)-1 do
      if FDisc[d].Entries[e].Filename<>'' then
       //If it is a directory, read that in
       if Pos('D',FDisc[d].Entries[e].Attributes)>0 then
       begin
        //Making room for it
        SetLength(FDisc,Length(FDisc)+1);
        FDisc[Length(FDisc)-1]:=ReadAFSDirectory(FDisc[d].Entries[e].Parent
                                                +dir_sep
                                                +FDisc[d].Entries[e].Filename,
                                                FDisc[d].Entries[e].Sector*$100);
        //Reference to it
        FDisc[d].Entries[e].DirRef:=Length(FDisc)-1;
       end;
    inc(d);
   until d=Length(FDisc);
  end
  else FAFSPresent:=False; //No valid partition
 end;
end;

{-------------------------------------------------------------------------------
Reads an AFS directory
-------------------------------------------------------------------------------}
function TDiscImage.ReadAFSDirectory(dirname:String;addr: Cardinal):TDir;
var
 numentries,
 index,
 day,
 month,
 year       : Integer;
 entry,
 objaddr,
 lentot,
 segaddr    : Cardinal;
 attr       : String;
 access,
 lenLSB,
 ptr        : Byte;
 buffer     : TDIByteArray;
begin
 Result.Directory:='';
 //Reset the directory settings to a default value
 ResetDir(Result);
 //Update the progress indicator
 UpdateProgress('Reading '+dirname);
 if ReadString(addr,-6)='JesMap' then
 begin
  //Read, and assemble, the directory into a temporary buffer
  buffer:=ReadAFSObject(addr);
  //And set the partition flag to true
  Result.AFSPartition:=True;
  //Directory title
  Result.Directory   :=ReadString($03,-10,buffer);
  RemoveSpaces(Result.Directory);
  if Result.Directory='$' then
  begin
   Result.Directory:=dirname;
   Fafsroot:=addr div $100;
  end;
  //Number of entries in the directory
  numentries:=ReadByte($0F,buffer);
  SetLength(Result.Entries,numentries);
  //Pointer to first entry
  entry:=Read16b($00,buffer);
  //Read all the entries in
  if numentries>0 then
   for index:=0 to numentries-1 do
   begin
    //Reset all other entries
    Result.Entries[index].FileType:='';
    Result.Entries[index].ShortFileType:='';
    Result.Entries[index].Parent:=dirname;
    //Filename
    Result.Entries[index].Filename:=ReadString(entry+$02,-10,buffer);
    RemoveSpaces(Result.Entries[index].Filename);
    //Load address
    Result.Entries[index].LoadAddr:=Read32b(entry+$0C,buffer);
    //Execution address
    Result.Entries[index].ExecAddr:=Read32b(entry+$10,buffer);
    //Attributes
    attr:='';
    access:=ReadByte(entry+$14,buffer);
    if access AND $20=$20 then attr:=attr+'D';
    if access AND $10=$10 then attr:=attr+'L';
    if access AND $08=$08 then attr:=attr+'W';
    if access AND $04=$04 then attr:=attr+'R';
    if access AND $02=$02 then attr:=attr+'w';
    if access AND $01=$01 then attr:=attr+'r';
    Result.Entries[index].Attributes:=attr;
    //Modification Date
    segaddr:=Read16b(entry+$15,buffer);
    day:=segaddr AND$1F;//Day;
    month:=(segaddr AND$F00)>>8; //Month
    year:=((segaddr AND$F000)>>12)+((segaddr AND$E0)>>1)+1981; //Year
    if(day>0)and(day<32)and(month>0)and(month<13)then
     Result.Entries[index].TimeStamp:=EncodeDate(year,month,day)
    else
     Result.Entries[index].TimeStamp:=0;
    //Location  of the object
    objaddr:=Read24b(entry+$17,buffer)*$100;
    if ReadString(objaddr,-6)='JesMap' then
    begin
     //Location of the data
     Result.Entries[index].Sector:=objaddr>>8;
     //Length
     lenLSB:=ReadByte(objaddr+$08); //LSB of the length
     lentot:=0; //Running total of the length
     ptr:=$0A;
     segaddr:=$FF;
     while(ptr<$FA)and(segaddr<>$00)do
     begin
      //Read the segment address
      segaddr:=Read24b(objaddr+ptr);
      //Read the length of this segment and add to the total
      inc(lentot,Read16b(objaddr+ptr+3));
      //Next segment
      inc(ptr,5);
     end;
     //Total length read, but may be over
     if lenLSB>0 then
      lentot:=(lentot-1)*$100+lenLSB
     else
      lentot:=lentot*$100;
     Result.Entries[index].Length:=lentot;
    end
    else
    begin //Invalid block, so blank these parameters off
     Result.Entries[index].Sector:=0;
     Result.Entries[index].Length:=0;
    end;
    //Directory Reference
    Result.Entries[index].DirRef:=-1;
    //Next entry
    entry:=Read16b(entry+$00,buffer);
   end;
 end;
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path
-------------------------------------------------------------------------------}
function TDiscImage.ExtractAFSFile(filename: String;
                                             var buffer: TDIByteArray): Boolean;
var
 dir,
 entry : Cardinal;
begin
 Result:=False;
 if FileExists(filename,dir,entry) then //Does the file actually exist?
 begin
  //Just use the method for reading in objects
  buffer:=ReadAFSObject(FDisc[dir].Entries[entry].Sector*$100);
  //And return a positive result
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Calculate offset into image given the disc address (640K floppy only)
-------------------------------------------------------------------------------}
function TDiscImage.AFSDiscAddrToOffset(disc_addr: Cardinal): Cardinal;
var
 tracks,
 track_size,
 track,
 side,
 oldheads,
 data_offset : Cardinal;
begin
 Result:=disc_addr;
 //We could end up here as ADFS looking into AFS partition
 if FFormat=diAcornADFS<<4+$02 then Result:=OldDiscAddrToOffset(disc_addr);
 //640K floppy disc
 if(FFormat=diAcornFS<<4+$02)and(FInterleave)then
 begin
  //Number of tracks and heads
  tracks:=80;
  oldheads:=2;
  //Track Size;
  track_size:=secspertrack*secsize;
  //Track number
  track:=(disc_addr DIV track_size) MOD tracks;
  //Which side
  side:=disc_addr DIV (tracks*track_size);
  //Offset into the sector for the data
  data_offset:=disc_addr MOD track_size;
  //Final result
  Result:= (track_size*side)+((track*track_size)div oldheads)+data_offset;
 end;
end;

{-------------------------------------------------------------------------------
Calculate disc address given the offset into image (640K floppy only)
-------------------------------------------------------------------------------}
function TDiscImage.OffsetToAFSDiscAddr(offset: Cardinal): Cardinal;
var
 tracks,
 track_size,
 track,
 side,
 oldheads,
 data_offset : Cardinal;
begin
 Result:=offset;
 //We could end up here as ADFS looking into AFS partition
 if FFormat=diAcornADFS<<4+$02 then Result:=OffsetToAFSDiscAddr(offset);
 // 640K floppy
 if(FFormat=diAcornFS<<4+$02)and(FInterleave)then
 begin
  //Number of tracks and heads
  tracks:=80;
  oldheads:=2;
  //Track Size;
  track_size:=secspertrack*secsize;
  //Track number
  track:=offset DIV (track_size*tracks*oldheads);
  //Which side
  side:=(offset MOD (track_size*tracks*oldheads))DIV track_size;
  //Offset into the sector for the data
  data_offset:=offset MOD track_size;
  //Final result
  Result:= (track*track_size)+(tracks*track_size*side)+data_offset;
 end;
end;

{-------------------------------------------------------------------------------
Read an assemble an objects data
-------------------------------------------------------------------------------}
function TDiscImage.ReadAFSObject(offset: Cardinal): TDIByteArray;
var
 ptr   : Byte;
 addr,
 len,
 bufptr: Cardinal;
 index : Integer;
begin
 Result:=nil;
 //Make sure it is a valid allocation map
 if ReadString(offset,-6)='JesMap' then
 begin
  //Start of the list
  ptr:=$0A;
  //Read the first entry and length
  addr:=$FF;
  while(addr<>0)and(ptr<$FA)do
  begin
   addr:=Read24b(offset+ptr)*$100;
   len:=Read16b(offset+ptr+3)*$100;
   bufptr:=Length(Result);
   //Copy into the buffer
   SetLength(Result,Length(Result)+len);
   for index:=0 to len-1 do
    Result[bufptr+index]:=ReadByte(addr+index);
   //Move onto the next entry
   inc(ptr,5);
  end;
  //Truncate to the total length
  len:=ReadByte(offset+$08);
  if(addr=0)and(len>0)then
  begin
   len:=$100-len;
   SetLength(Result,Length(Result)-len);
  end;
 end;
end;
