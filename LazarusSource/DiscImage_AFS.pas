//++++++++++++++++++ Acorn FileStore +++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
IDs an AFS disc
-------------------------------------------------------------------------------}
function TDiscImage.ID_AFS: Boolean;
 function IDAFSPass(chgint:Boolean): Boolean;
 var
  afspart1,
  afspart2,
  Lafsroot : Cardinal;
  index    : Integer;
  ok       : Boolean;
 begin
  ResetVariables;
  //Is there actually any data?
  if GetDataLength>0 then
  begin
   //Set default sector size and sectors per track
   secspertrack:= 16;
   secsize     :=256;
   //Is an AFS Level 2?
   if ReadString(0,-4)='AFS0' then
   begin
    //Need to set a format to fire off the interleave options
    FFormat:=diAcornFS<<4+1;
    if(FForceInter=0)and(chgint)then //Auto detect interleave
     Finterleave:=1; //Start with SEQ for AFS L2
    //The afs header address will be sectors 0 and 10
    afspart1:=$0*secsize;
    afspart2:=$A*secsize;
   end
   else //Not an AFS Level 2, so see if it is Level 3
   begin
    //Need to set a format to fire off the interleave options
    FFormat:=diAcornFS<<4+2;
    if(FForceInter=0)and(chgint)then //Auto detect interleave
     Finterleave:=3; //Start with MUX for AFS L3
    //Get the afs header address from the ADFS map
    afspart1:=Read24b($0F6)*secsize;
    afspart2:=Read24b($1F6)*secsize;
   end;
   //Have we got an ID header?
   if(ReadString(afspart1,-4)='AFS0')and(ReadString(afspart2,-4)='AFS0')then
   begin
    //Compare the two partition headers
    ok:=True;
    for index:=0 to $25 do
     if ReadByte(afspart1+index)<>ReadByte(afspart2+index)then ok:=False;
    //Both match? Do we have an ID string?
    if ok then
    begin
     //Level 2
     if FFormat=diAcornFS<<4+1 then
      //Confirm the root is where it says it is
      if ReadByte((Read24b(afspart1+$16)*secsize)+3)=$24 then afshead:=afspart1
      else FFormat:=diInvalidImg; //It isn't, so invalid image
     //Level 3
     if FFormat=diAcornFS<<4+2 then
     begin
      //Now we confirm we are looking at the root
      //Read where the header is pointing towards the root
      Lafsroot:=Read24b(afspart1+$1F)*$100;
      //Read the ID string
      if ReadString(Lafsroot,-6)='JesMap' then
       //Confirm it is the root
       if ReadByte((Read24b(Lafsroot+$0A)*secsize)+3)=$24 then // $24='$'
       begin
        afshead:=afspart1; //Make a note of the header location
        afshead2:=afspart2;//And the copy
       end
       else FFormat:=diInvalidImg //No root, invalid image
      else FFormat:=diInvalidImg; //No map ID, invalid image
     end;
    end else FFormat:=diInvalidImg; //Headers not matching, no format
   end else FFormat:=diInvalidImg; //No header ID, invalid image
  end;
  Result:=FFormat>>4=diAcornFS;
 end;
var
 start: Byte;
begin
 Result:=False;
 if FFormat=diInvalidImg then
 begin
  //Interleaving, depending on the option
  Finterleave:=FForceInter;
  if Finterleave=0 then Finterleave:=1; //Don't know the format, so go with SEQ
  //Do a first pass for ID
  Result:=IDAFSPass(True);
  //Are we set to autodetect interleaving, if no positive ID result?
  if(FForceInter=0)and(not Result)then //Auto
  begin
   start:=FInterleave;
   repeat
    //Next method
    dec(FInterleave);
    if FInterleave=0 then FInterleave:=4; //Wrap around
    Result:=IDAFSPass(False);
   until(Result)or(FInterleave=start); //Continue until a result or back at the start
  end;
 end;
end;

{-------------------------------------------------------------------------------
Reads an AFS partition
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadAFSPartition;
var
 d,e,i    : Integer;
 allocmap : Cardinal;
 startdir : String;
 visited  : array of Cardinal;
begin
 visited:=nil;
 //Is this an ADFS disc with Acorn FileStore partition?
 if((FFormat>>4=diAcornADFS)and(FAFSPresent))
 or(FFormat>>4=diAcornFS)then
 begin
  if FFormat>>4=diAcornADFS then afshead:=Read24b($0F6)*secsize;
  //Confirm that there is a valid AFS partition here
  if ReadString(afshead,-4)='AFS0' then //Should be 'AFS0'
  begin
   //Update the progress indicator
   UpdateProgress('Reading Acorn FS partition');
   //Size of the disc
   if FFormat=diAcornFS<<4+1 then //Level 2
    disc_size[0]:=Read16b(afshead+$14)*2*secsize; //Only gives number of sectors for 1 side
   if FFormat=diAcornFS<<4+2 then //Level 3
    disc_size[0]:=Read16b(afshead+$16)*secsize;
   i:=0;
   if FFormat>>4=diAcornADFS then //Level 3/ADFS Hybrid
   begin
    SetLength(disc_size,2);
    SetLength(free_space,2);
    SetLength(disc_name,2);
    disc_size[1]:=(Read16b(afshead+$16)*secsize)-disc_size[0];
    i:=1;
   end;
   //Disc title
   disc_name[i]:=ReadString(afshead+4,-16);
   RemoveSpaces(disc_name[i]); //Minus trailing spaces
   //Where is the AFS root?
   if FFormat=diAcornFS<<4+1 then
    allocmap:=Read24b(afshead+$16)*secsize //Level 2
   else
    allocmap:=Read24b(afshead+$1F)*secsize;//Level 3
   //Is the ADFS root broken with no entries (i.e. valid?)
   if Length(FDisc)>0 then //This will also be zero if this is an AFS image
    if(FDisc[0].Broken)and(Length(FDisc[0].Entries)=0)then
     SetLength(FDisc,0); //Yes, so overwrite it
   //Add a new entry
   d:=Length(FDisc);
   SetLength(FDisc,d+1);
   //Start the chain by reading the root
   if FFormat>>4=diAcornADFS then startdir:=afsrootname else startdir:='$';
   FDisc[d]:=ReadAFSDirectory(startdir,allocmap);
   //Add the root as a visited directory
   SetLength(visited,1);
   visited[0]:=allocmap div secsize;
   //Now go through the root's entries and read them in
   repeat
    if Length(FDisc[d].Entries)>0 then
     for e:=0 to Length(FDisc[d].Entries)-1 do
     begin
      //Make sure we haven't seen this before. If a directory references a higher
      //directory we will end up in an infinite loop.
      if Length(visited)>0 then
       for i:=0 to Length(visited)-1 do
        if visited[i]=FDisc[d].Entries[e].Sector then
         FDisc[d].Entries[e].Filename:='';//Blank off the filename so we can remove it later
      if FDisc[d].Entries[e].Filename<>'' then //Needs to have an actual name
       //If it is a directory, read that in
       if Pos('D',FDisc[d].Entries[e].Attributes)>0 then
       begin
        //Making room for it
        SetLength(FDisc,Length(FDisc)+1);
        //And now read it in
        FDisc[Length(FDisc)-1]:=ReadAFSDirectory(FDisc[d].Entries[e].Parent
                                                +dir_sep
                                                +FDisc[d].Entries[e].Filename,
                                                FDisc[d].Entries[e].Sector*secsize);
        //Remember it
        SetLength(visited,Length(visited)+1);
        visited[Length(visited)-1]:=FDisc[d].Entries[e].Sector;
        //Reference to it
        FDisc[d].Entries[e].DirRef:=Length(FDisc)-1;
       end;
     end;
    inc(d);
   until d=Length(FDisc);
   //Update the progress indicator
   UpdateProgress('Removing blank entries');
   //Remove any blank entries or parent directory references
   if Length(FDisc)>0 then
   begin
    //Directory counter
    d:=0;
    //Iterate through all directories
    while d<Length(FDisc) do
    begin
     //Are there any entries?
     if Length(FDisc[d].Entries)>0 then
     begin
      //Entry counter
      e:=0;
      //Iterate through all entries
      while e<Length(FDisc[d].Entries) do
      begin
       //Do we have a blank filename?
       if FDisc[d].Entries[e].Filename='' then
       begin
        //If not the last entry
        if e<Length(FDisc[d].Entries)-1 then
        //Move the entries above it down by one
         for i:=e+1 to Length(FDisc[d].Entries)-1 do
          FDisc[d].Entries[i-1]:=FDisc[d].Entries[i];
        //Remove the final entry
        SetLength(FDisc[d].Entries,Length(FDisc[d].Entries)-1);
       end;
       //Next entry (which now could take us over the length)
       inc(e);
      end;
     end;
     //Next directory
     inc(d);
    end;
   end;
   //Read in the free space map
   ReadAFSFSM;
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
 side,
 entry,
 objaddr,
 segaddr    : Cardinal;
 attr       : String;
 access     : Byte;
 buffer     : TDIByteArray;
begin
 Result.Directory:='';
 buffer:=nil;
 //Reset the directory settings to a default value
 ResetDir(Result);
 //Update the progress indicator
 UpdateProgress('Reading '+dirname);
 //Read, and assemble, the directory into a temporary buffer, if valid
 buffer:=ReadAFSObject(addr);
 //Have we got any data to look at?
 if Length(buffer)>0 then
 begin
  //And set the partition flag to true
  Result.AFSPartition:=True;
  //ADFS hybrid?
  if FFormat>>4=diAcornADFS then side:=1 else side:=0;
  Result.Partition:=side;
  //Directory title
  Result.Directory:=ReadString($03,-10,buffer);
  RemoveSpaces(Result.Directory);
  if Result.Directory='$' then
  begin
   Result.Directory:=dirname; //Change the name, in case we already have a '$'
   Fafsroot:=addr div secsize; //Make a note of where the root is
   afsroot_size:=GetAFSObjLength(addr); //And a note of the root size
  end;
  //Number of entries in the directory
  numentries:=ReadByte($0F,buffer);
  SetLength(Result.Entries,numentries);
  //Pointer to first entry
  entry:=Read16b($00,buffer);
  //Read all the entries in
  if(numentries>0)and(entry>0)then
   for index:=0 to numentries-1 do
   begin
    //Reset all other entries
    Result.Entries[index].FileType:='';
    Result.Entries[index].ShortFileType:='';
    Result.Entries[index].Parent:=dirname;
    Result.Entries[index].Side:=side;
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
    objaddr:=Read24b(entry+$17,buffer)*secsize;
    if(ReadString(objaddr,-6)='JesMap')or(FFormat=diAcornFS<<4+1)then
    begin
     //Location of the data
     Result.Entries[index].Sector:=objaddr div secsize;
     //Length
     Result.Entries[index].Length:=GetAFSObjLength(objaddr);
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
  buffer:=ReadAFSObject(FDisc[dir].Entries[entry].Sector*secsize);
  //And return a positive result
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Read and assemble an object's data
-------------------------------------------------------------------------------}
function TDiscImage.ReadAFSObject(offset: Cardinal): TDIByteArray;
var
 ptr,
 addr,
 len,
 bufptr,
 almap : Cardinal;
 index : Integer;
begin
 Result:=nil;
 //Make sure it is a valid allocation map for Level 3
 if(ReadString(offset,-6)='JesMap')
 and((FFormat=diAcornFS<<4+2)or(FFormat>>4=diAcornADFS))then
 begin
  //Start of the list
  ptr:=$0A;
  //Read the first entry and length
  addr:=$FF;
  while(addr<>0)and(ptr<$FA)do
  begin
   addr:=Read24b(offset+ptr)*secsize;
   len:=Read16b(offset+ptr+3)*secsize;
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
 //We are looking at a Level 2 map, with no ID string
 if(FFormat=diAcornFS<<4+1)and(offset<>0)then
 begin
  offset:=offset div secsize;
  //Get the address of the current allocation map
  almap:=GetAllocationMap;
  //Find our entry and read it in
  repeat
   addr:=Read16b(almap+(offset*2)+5);
   len:=secsize; //Length of data to read in
   if (addr AND $4000)=$4000 then
   begin
    len:=addr AND $FF;//Last sector, so bytes used
    if len=0 then len:=secsize; //if zero, then it must be a full sector
   end;
   if (addr AND $1000)=$1000 then len:=0; //This entry is empty
   if len>0 then
   begin
    //Copy the data into the buffer
    bufptr:=Length(Result);
    SetLength(Result,bufptr+len);
    for index:=0 to len-1 do
     Result[bufptr+index]:=ReadByte(offset*secsize+index);
   end;
   if (addr AND $4000)<>$4000 then offset:=addr AND $FFF; //This is the next sector in the chain
  until (addr AND $4000)=$4000; //Continue until everything is read in
 end;
end;

{-------------------------------------------------------------------------------
Read the length of an object
-------------------------------------------------------------------------------}
function TDiscImage.GetAFSObjLength(offset: Cardinal): Cardinal;
var
 almap,
 addr,
 len,
 ptr,
 segaddr: Cardinal;
 lenLSB : Byte;
begin
 Result:=0;
 //Level 2
 if(FFormat=diAcornFS<<4+1)and(offset<>0)then
 begin
  offset:=offset div secsize;
  //Get the address of the current allocation map
  almap:=GetAllocationMap;
  //Find our entry and read it in
  repeat
   addr:=Read16b(almap+(offset*2)+5);
   //Length of data in this sector
   len:=secsize;
   if (addr AND $4000)=$4000 then
   begin
    //Last sector, so bytes used
    len:=addr AND $FF;
    //if zero, then it must be a full sector
    if len=0 then len:=secsize;
   end;
   if (addr AND $1000)=$1000 then len:=0; //This entry is empty
   //Add to the total
   inc(Result,len);
   if (addr AND $4000)<>$4000 then offset:=addr AND $FFF; //This is the next sector in the chain
  until (addr AND $4000)=$4000; //Continue until everything is read in
 end;
 //Level 3
 if(ReadString(offset,-6)='JesMap')
 and((FFormat=diAcornFS<<4+2)or(FFormat>>4=diAcornADFS))then
 begin
  lenLSB:=ReadByte(offset+$08); //LSB of the length
  ptr:=$0A;
  segaddr:=$FF;
  while(ptr<$FA)and(segaddr<>$00)do
  begin
   //Read the segment address
   segaddr:=Read24b(offset+ptr);
   //Read the length of this segment and add to the total
   inc(Result,Read16b(offset+ptr+3));
   //Next segment
   inc(ptr,5);
  end;
  //Total length read, but may be over
  if lenLSB>0 then
   Result:=(Result-1)*secsize+lenLSB
  else
   Result:=Result*secsize;
 end;
end;

{-------------------------------------------------------------------------------
Gets the Level 2 allocation map address
-------------------------------------------------------------------------------}
function TDiscImage.GetAllocationMap: Cardinal;
begin
 Result:=0; //Default
 if FFormat=diAcornFS<<4+1 then //Only Level 2
  if ReadByte(Read24b(afshead+$1B)*secsize)>ReadByte(Read24b(afshead+$1E)*secsize)then
   Result:=Read24b(afshead+$1B)*secsize else Result:=Read24b(afshead+$1E)*secsize;
end;

{-------------------------------------------------------------------------------
Read the free space map
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadAFSFSM;
var
 fragments: TFragmentArray;
 part     : Byte;
 spt,
 tracks,
 entry,
 index,
 t,s      : Cardinal;
begin
 //Get all the used sectors
 fragments:=AFSGetFreeSectors(True);
 //Initialise the variables
 if FFormat>>4=diAcornFS then //AFS Level 2 and 3
 begin
  SetLength(free_space_map,1);
  part:=0;
 end;
 if FFormat>>4=diAcornADFS then //Hybrids - AFS will take up the second 'side'
 begin
  SetLength(free_space_map,2);
  part:=1;
 end;
 //Get the local sectors per track
 if(FFormat>>4=diAcornADFS)or(FFormat=diAcornFS<<4+2)then
  spt:=Read16b(afshead+$1A) else spt:=secspertrack;
 //Initialise the free space
 free_space[part]:=disc_size[part];
 //Set up the array
 tracks:=Ceil((disc_size[part]div secsize)/spt);
 SetLength(free_space_map[part],tracks);
 for entry:=0 to Length(free_space_map[part])-1 do //Sectors per track
 begin
  SetLength(free_space_map[part,entry],spt);
  for index:=0 to spt-1 do free_space_map[part,entry,index]:=$00;
 end;
 //Go through the fragments and mark those as used
 if Length(fragments)>0 then
  for index:=0 to Length(fragments)-1 do
  begin
   for entry:=0 to (fragments[index].Length div secsize)-1 do
   begin
    //Get the track and sector
    t:=((fragments[index].Offset div secsize)+entry)div spt;
    s:=((fragments[index].Offset div secsize)+entry)mod spt;
    if t<tracks then //Make sure it is within range
     free_space_map[part,t,s]:=$FF-fragments[index].Zone;
   end;
   //Decrease the total free space
   dec(free_space[part],fragments[index].Length);
  end;
end;

{-------------------------------------------------------------------------------
Get an array of all the free sectors (or used, if set to True)
-------------------------------------------------------------------------------}
function TDiscImage.AFSGetFreeSectors(used: Boolean=False): TFragmentArray;
var
 allocmap,
 index,
 entry,
 szofbmp,
 bmploc,
 afsstart,
 trackstrt    : Cardinal;
 Lsecspertrack: Word;
 Ldiscsize    : Int64;
 status       : Byte;
begin
 Result:=nil;
 //Initialise the variables
 SetLength(Result,0);
 //Level 2
 if FFormat=diAcornFS<<4+1 then
 begin
  //Get the allocation map address
  allocmap:=GetAllocationMap;
  szofbmp :=Read24b(afshead+$21);//The size of the map
  //Get the other map address
  index   :=Read24b(afshead+$1E)*secsize;
  if index=allocmap then index:=Read24b(afshead+$1B)*secsize;
  //Now find the free sectors
  for entry:=0 to (disc_size[0]div secsize)-1 do
  begin
   //Read the status of this sector
   status:=ReadByte(allocmap+6+(entry*2));
   //If it has not been written then mark as free
   if((status AND $80)<>$80)and(not used)then
   begin
    SetLength(Result,Length(Result)+1);
    Result[Length(Result)-1].Offset:=entry*secsize;
    Result[Length(Result)-1].Length:=secsize;
    Result[Length(Result)-1].Zone  :=0;
   end;
   //If it has been written then mark as used
   if((status AND $80)=$80)and(used)then
   begin
    SetLength(Result,Length(Result)+1);
    Result[Length(Result)-1].Offset:=entry*secsize;
    Result[Length(Result)-1].Length:=secsize;
    Result[Length(Result)-1].Zone  :=0;
    //System?
    if(entry*secsize=afshead)or(entry*secsize=afshead2)
    or((entry>=Fafsroot)and(entry*secsize<=Fafsroot*secsize+afsroot_size))
    or((entry*secsize>=allocmap)and(entry*secsize<=allocmap+szofbmp))
    or((entry*secsize>=index)and(entry*secsize<=index+szofbmp))then
     Result[Length(Result)-1].Zone:=1;
   end;
  end;
 end;
 //Level 3 and ADFS/AFS Hybrid
 if FFormat<>diAcornFS<<4+1 then
 begin
  Lsecspertrack:=Read16b(afshead+$1A);
  //Level 3
  if FFormat=diAcornFS<<4+2 then
  begin
   Ldiscsize:=disc_size[0];     //Look at the entire image
   afsstart:=0;                 //But not below here, which is ADFS header
  end
  else
  begin //Hybrids - AFS will take up the second 'side'
   Ldiscsize:=disc_size[1];     //Only look at the AFS part of the image
   afsstart:=disc_size[0];      //And not below here, which is the ADFS partition
  end;
  //Read the size of the bitmap
  szofbmp:=ReadByte(afshead+$1C)*secsize;
  //Is it big enough to hold the entire disc?
  if szofbmp>=(((afsstart+Ldiscsize) div secsize)div 8)+1 then //Yes
  begin
   //Location of the bitmap (just before the root)
   bmploc:=(Fafsroot*secsize)-szofbmp;
   //Go through the bitmap, sector by sector
   for index:=0 to (Ldiscsize div secsize)-1 do
   begin
    //Is the bit set, then it is free
    if(IsBitSet(ReadByte(bmploc+(index div 8)),index mod 8))and(not used)then
    begin
     SetLength(Result,Length(Result)+1);
     Result[Length(Result)-1].Offset:=(index*secsize)-afsstart;
     Result[Length(Result)-1].Length:=secsize;
     Result[Length(Result)-1].Zone  :=0;
    end;
    //Is the bit not set, then it is used
    if(not IsBitSet(ReadByte(bmploc+(index div 8)),index mod 8))and(used)then
    begin
     SetLength(Result,Length(Result)+1);
     Result[Length(Result)-1].Offset:=(index*secsize)-afsstart;
     Result[Length(Result)-1].Length:=secsize;
     Result[Length(Result)-1].Zone  :=0;
     //System?
     if(index*secsize=afshead)or(index*secsize=afshead2)
     or((index>=Fafsroot)and(index*secsize<=Fafsroot*secsize+afsroot_size))
     or((index*secsize>=bmploc)and(index*secsize<=bmploc+szofbmp))then
      Result[Length(Result)-1].Zone:=1;
    end;
   end;
  end
  else //Not big enough for entire disc, so will be on every track
  begin
   //Get the sector offset of each track
   trackstrt:=Lsecspertrack*secsize;
   //Our counter - first track after the ADFS part
   bmploc:=(afsstart div trackstrt)*trackstrt;
   if bmploc<afsstart then bmploc:=trackstrt;
   while bmploc<afsstart+Ldiscsize do //Do the entire image, track by track
   begin
    //Every sector per track
    for index:=0 to Lsecspertrack-1 do
    begin
     //Is the bit set? Yes - free
     if(IsBitSet(ReadByte(bmploc+(index div 8)),index mod 8))and(not used)then
     begin
      SetLength(Result,Length(Result)+1);
      Result[Length(Result)-1].Offset:=(bmploc+index*secsize)-afsstart;
      Result[Length(Result)-1].Length:=secsize;
      Result[Length(Result)-1].Zone  :=0;
     end;
     //Is the bit set? No - used
     if(not IsBitSet(ReadByte(bmploc+(index div 8)),index mod 8))and(used)then
     begin
      SetLength(Result,Length(Result)+1);
      Result[Length(Result)-1].Offset:=(bmploc+index*secsize)-afsstart;
      Result[Length(Result)-1].Length:=secsize;
      Result[Length(Result)-1].Zone  :=0;
      //But is it system
      if(bmploc+index*secsize=afshead)or(bmploc+index*secsize=afshead2)
      or((bmploc+index*secsize>=Fafsroot*secsize)
      and(bmploc+index*secsize<=Fafsroot*secsize+afsroot_size))
      or(index*secsize<=szofbmp)then Result[Length(Result)-1].Zone:=1;
     end;
    end;
    //Move on to the next track
    inc(bmploc,trackstrt);
   end;
  end;
 end;
 //Now compact the array by joining adjacent sectors
 if Length(Result)>1 then
 begin
  index:=Length(Result);
  while index>0 do
  begin
   dec(index);
   //Is the one below it adjacent?
   if(Result[index].Offset=Result[index-1].Offset+secsize)
   and(Result[index].Zone=Result[index-1].Zone)then
   begin
    //Then join them
    inc(Result[index-1].Length,Result[index].Length);
    //Move the ones above down
    if index<Length(Result)-1 then
     for entry:=index to Length(Result)-2 do
      Result[entry]:=Result[entry+1];
    //And let the last one drop off
    SetLength(Result,Length(Result)-1);
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Find and allocate some free space
-------------------------------------------------------------------------------}
function TDiscImage.AFSAllocateFreeSpace(size :Cardinal;
                                       var fragments: TFragmentArray): Cardinal;
var
 FSM,
 alloc    : TFragmentArray;
 found    : Boolean;
 index    : Integer;
 sector,
 fragsize,
 allocmap : Cardinal;
begin
 //Return a erroronous result
 Result:=$FFFFFFFF;
 //Get the current free space fragments
 FSM:=AFSGetFreeSectors;
 //Are there any?
 if Length(FSM)>0 then
 begin
  //Level 3 includes a 256 byte object header
  if(FFormat=diAcornFS<<4+2)or(FFormat>>4=diAcornADFS)then inc(size,$100);
  //Are there any that will fit the data without fragmenting?
  index:=0;
  found:=False;
  while(index<Length(FSM))and(not found)do
  begin
   if FSM[index].Length>=size then //Yes
   begin
    //Mark as found so we stop looking
    found:=True;
    //Copy to the supplied array
    SetLength(fragments,1);
    fragments[0].Offset:=FSM[index].Offset;
    fragments[0].Length:=size;
   end
   else inc(index); //No, go to next one
  end;
  //If not
  if not found then
  begin
   exit; //Temporary
  end;
  //Split the fragments into sector sized fragments for allocation
  //If more than 47, on Level 3, then we'll need another JesMap block
  if(Length(fragments)>0)and(Length(fragments)<48)then //For now, it'll fail
  begin
   //Our destination array
   SetLength(alloc,0);
   for index:=0 to Length(fragments)-1 do
   begin
    //Make a note of the total length and start sector
    fragsize:=fragments[index].Length;
    sector  :=fragments[index].Offset div secsize;
    //Now split this into fragments
    while fragsize>0 do
    begin
     //New entry in the array
     SetLength(alloc,Length(alloc)+1);
     //Populate it
     alloc[Length(alloc)-1].Offset:=sector AND$FFF;
     //Length no bigger than a sector size
     alloc[Length(alloc)-1].Length:=fragsize MOD secsize;
     if alloc[Length(alloc)-1].Length=0 then //The above will return zero if it
      alloc[Length(alloc)-1].Length:=secsize;//is the sector size
     //Decrease this fragment size by what we've allocated
     dec(fragsize,alloc[Length(alloc)-1].Length);
     //And onto the next sector
     inc(sector);
    end;
   end;
   //Write it to the FSM
   if Length(alloc)>0 then
   begin                          
    //Current map (Level 2)
    if FFormat=diAcornFS<<4+1 then allocmap:=GetAllocationMap;
    for index:=0 to Length(alloc)-1 do
    begin
     //Level 2
     if FFormat=diAcornFS<<4+1 then
     begin
      //Clear the entry
      WriteByte(0,allocmap+(alloc[index].Offset*2)+6);
      if index<Length(alloc)-1 then //Write pointer to next sector
       Write16b(alloc[index+1].Offset,allocmap+(alloc[index].Offset*2)+5)
      else //Write the length
       WriteByte(alloc[index].Length mod$100,allocmap+(alloc[index].Offset*2)+5);
      //If this is the first, set the bit
      if index=0 then WriteBits(1,allocmap+(alloc[index].Offset*2)+6,5,1);
      //If this is the last, set the bit
      if index=Length(alloc)-1 then
       WriteBits(1,allocmap+(alloc[index].Offset*2)+6,6,1);
      //Mark as written
      WriteBits(1,allocmap+(alloc[index].Offset*2)+6,7,1);
     end;
    end;
    //Compact the free space map
    if FFormat=diAcornFS<<4+1 then FinaliseAFSL2Map;
    //Return the address of the first fragment
    Result:=fragments[0].Offset;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Write the headers and compact the Level 2 map
-------------------------------------------------------------------------------}
procedure TDiscImage.FinaliseAFSL2Map;
var
 freesecs,
 firstfree : Word;
 allocmap,
 mapsize   : Cardinal;
 index     : Integer;
begin
 if FFormat=diAcornFS<<4+1 then
 begin
  allocmap :=GetAllocationMap;
  mapsize  :=Read24b(afshead+$21);//The size of the map
  //Work out and set the number of free sectors and pointer to first free
  freesecs:=0;      //Counter for number of free sectors
  firstfree:=$FFFF; //Dummy start
  index:=5;         //Start at the beginning
  while index<mapsize do
  begin
   if ReadBits(allocmap+index+1,7,1)=0 then //Bit 7 is not set, so is unwritten
   begin
    inc(freesecs); //Count it as a free sector
    if firstfree=$FFFF then     //Is it the first?
     firstfree:=(index-5)div 2; //then record it
   end;
   inc(index,2);//Next sector
  end;
  Write16b(freesecs,allocmap+1); //Write the free sector count to the map
  Write16b(firstfree,allocmap+3);//Write the first free sector to the map
  //Now re-align the unwritten sector pointers so they jump over the written ones
  index:=mapsize-2;//Start at the end
  firstfree:=(index-5)div 2;//And take note of the next sector
  while index>5 do //Continue until we reach the start
  begin
   dec(index,2); //Entries are 2 bytes long
   if ReadBits(allocmap+index+1,7,1)=0 then //If unwritten
   begin
    Write16b(firstfree,allocmap+index);     //Write the last known free
    firstfree:=(index-5)div 2;               //And update to point to this one
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Create a blank AFS image
-------------------------------------------------------------------------------}
function TDiscImage.FormatAFS(harddrivesize: Cardinal;afslevel: Byte): Boolean;
var
 index     : Integer;
 y,m,d     : Byte;
 cdate     : Word;
 mapsize,
 mapsizeal,
 allocmap1,
 allocmap2 : Cardinal;
const
 afsID    = 'AFS0';
 objID    = 'JesMap';
 cycle    = 42; //Random...42 - the answer to life, the universe and everything
begin
 Result:=False;
 if(afslevel=2)or(afslevel=3)then
 begin
  //Blank everything
  ResetVariables;
  SetDataLength(0);
  //Set the format
  FFormat:=diAcornFS<<4+(afslevel-1);
  //Set the filename
  imagefilename:='Untitled.'+FormatExt;
  //Setup the data area
  disc_size[0]:=harddrivesize;
  SetDataLength(disc_size[0]);
  //Fill with zeros
  for index:=0 to disc_size[0]-1 do WriteByte(0,index);
  //Set the boot option
  SetLength(bootoption,1);
  bootoption[0]:=0;
  SetLength(free_space_map,1); //Free Space Map
  //Default sizes
  secsize:=256;
  secspertrack:=16;
  //Root is 2 sectors long
  afsroot_size:=$200;
  //Interleave option
  if FForceInter=0 then
   if afslevel=3 then Finterleave:=3 //MUX - Default for AFS L3
   else Finterleave:=1 //SEQ - default for AFS L2
  else Finterleave:=FForceInter;
  //Level 2
  if afslevel=2 then
  begin
   //Where the header is located
   afshead:=$000;
   afshead2:=afshead+$A00;
   //Write the AFS headers
   //ID at $00
   WriteString(afsID,afshead,0,0);
   //Title at $04
   WriteString(afsdisctitle,afshead+$04,16,32);
   //Sectors for one side at $14
   Write16b((harddrivesize>>8)div 2,afshead+$14);
   //Root SIN at $16
   Fafsroot:=afshead+$200;
   Write24b(Fafsroot>>8,afshead+$16);
   //Creation Date at $19
   y:=StrToIntDef(FormatDateTime('yyyy',Now),1981)-1981;//Year
   m:=StrToIntDef(FormatDateTime('m',Now),1);           //Month
   d:=StrToIntDef(FormatDateTime('d',Now),1);           //Date
   cdate:=((y AND$F)<<12)OR((y AND$F0)<<1)OR(d AND$1F)OR((m AND$F)<<8);
   Write16b(cdate,afshead+$19);
   //Map A location at $1B
   allocmap1:=afshead2+$100;
   Write24b(allocmap1>>8,afshead+$1B);
   //Map B location at $1E
   mapsize:=5+((harddrivesize>>8)*2);
   //Round up to the next sector boundary
   mapsizeal:=(mapsize>>8)<<8+Ceil((mapsize mod $100)/$100)<<8;
   allocmap2:=allocmap1+mapsizeal;
   Write24b(allocmap2>>8,afshead+$1E);
   //Size of map at $21
   Write24b(mapsize,afshead+$21);
   //Rest of the sector filled with $00000080
   index:=$24;
   while index<$FF do
   begin
    Write32b($00000080,afshead+index);
    inc(index,4);
   end;
   //Copy the primary header to the copy
   for index:=0 to $FF do WriteByte(ReadByte(afshead+index),afshead2+index);
   //Write the directory object for $
   CreateAFSDirectory('$','$','DLR');
   //Write the image allocation map
   for index:=0 to (harddrivesize>>8)-1 do   //Blank entries just reference the
    Write16b(index+1,allocmap1+5+(index*2)); //next sector.
   //AFS Headers
   WriteBits(1,allocmap1+6,7,1); //sector 0 mark as written
   WriteBits(1,allocmap1+26,7,1);//sector 10 mark as written
   WriteBits(1,allocmap1+6,6,1); //sector 0 end of chain
   WriteBits(1,allocmap1+26,6,1);//sector 10 end of chain
   WriteBits(1,allocmap1+6,5,1); //sector 0 start of chain
   WriteBits(1,allocmap1+26,5,1);//sector 10 start of chain
   WriteByte(0,allocmap1+5);     //No next sector
   WriteByte(0,allocmap1+25);    //No next sector
   //Sector 1 and sector 801 (blank sectors for DFS) are not used
   WriteBits(1,allocmap1+8,7,1);   //sector 1 mark as written
   WriteBits(1,allocmap1+1608,7,1);//sector 801 mark as written
   WriteBits(1,allocmap1+8,6,1);   //sector 1 end of chain
   WriteBits(1,allocmap1+1608,6,1);//sector 801 end of chain
   WriteBits(1,allocmap1+8,5,1);   //sector 1 start of chain
   WriteBits(1,allocmap1+1608,5,1);//sector 801 start of chain
   WriteByte(0,allocmap1+7);       //No next sector
   WriteByte(0,allocmap1+1607);    //No next sector
   //Root
   WriteBits(1,allocmap1+6+Fafsroot>>7,5,1);//Start
   WriteBits(1,allocmap1+6+Fafsroot>>7,7,1);//Has been written
   WriteBits(1,allocmap1+6+(Fafsroot+afsroot_size)>>7-2,6,1);//End
   WriteBits(1,allocmap1+6+(Fafsroot+afsroot_size)>>7-2,7,1);//Has been written
   WriteByte(0,allocmap1+5+(Fafsroot+afsroot_size)>>7-2);//Zero length LSB
   if afsroot_size>>8>2 then
    for index:=1 to (afsroot_size>>8)-1 do
     WriteBits(1,allocmap1+6+((Fafsroot>>8)+index)*2,7,1);
   //Allocation Maps
   for index:=0 to mapsize>>8 do
   begin
    WriteBits(1,allocmap1+6+((allocmap1>>8)+index)*2,7,1);
    WriteBits(1,allocmap1+6+((allocmap2>>8)+index)*2,7,1);
   end;
   dec(mapsizeal,$100);//Otherwise we will encroach onto the second map
   WriteBits(1,allocmap1+6+allocmap1>>7,5,1); //Start
   WriteBits(1,allocmap1+6+(allocmap1+mapsizeal)>>7,6,1); //End
   WriteByte(0,allocmap1+5+(allocmap1+mapsizeal)>>7);
   WriteBits(1,allocmap1+6+allocmap2>>7,5,1); //Start
   WriteBits(1,allocmap1+6+(allocmap2+mapsizeal)>>7,6,1); //End
   WriteByte(0,allocmap1+5+(allocmap2+mapsizeal)>>7);
   //Update the current map indicator
   WriteByte($02,allocmap1);
   //Finalise map
   FinaliseAFSL2Map;
   //Mark end of map
   Write16b(0,allocmap1+mapsize-2);
   WriteBits(1,allocmap1+mapsize-1,6,1);
   //Copy the primary allocation map to the copy
   for index:=0 to mapsize-1 do
    WriteByte(ReadByte(allocmap1+index),allocmap2+index);
   //Update the map indicator for the second map
   WriteByte($01,allocmap2);
   //Mark as a success
   Result:=True;
  end;
  //Level 3
  if afslevel=3 then
  begin
   //Where the header is located
   afshead:=$200;
   afshead2:=afshead+$100;
   //Install a basic ADFS map
   Write24b($20,$000);//FreeStart
   Write24b(afshead>>8,$0F6);//AFS Header copy 1
   Write24b($20,$0FC);//Disc size
   WriteByte(ByteCheckSum($0000,$100),$0FF);//Checksum sector 0
   Write24b($00,$100);//FreeEnd
   Write24b(afshead2>>8,$1F6);//AFS Header copy 2
   WriteByte(ByteCheckSum($0100,$100),$1FF);//Checksum sector 1
   //Write the AFS headers
   //ID at $00
   WriteString(afsID,afshead,0,0);
   //Title at $04
   WriteString(afsdisctitle,afshead+$04,16,32);
   //Tracks at $14
   Write16b((harddrivesize>>8)div secspertrack,afshead+$14);
   //Disc size (number of sectors) at $16
   Write24b(harddrivesize>>8,afshead+$16);
   //Partitions at $19
   WriteByte(1,afshead+$19);
   //Sectors per track at $1A
   Write16b(secspertrack,afshead+$1A);
   //Size of bitmap at $1C
   mapsize:=Ceil(((harddrivesize>>8)/8)/secsize);
   WriteByte(mapsize,afshead+$1C);
   mapsize:=mapsize<<8;
   //Next drive at $1D
   WriteByte($01,afshead+$1D);
   // $00 at $1E
   WriteByte($00,afshead+$1E);
   //Root SIN at $1F
   Fafsroot:=afshead2+$100+mapsize;
   Write24b(Fafsroot>>8,afshead+$1F);
   //Creation Date at $22
   y:=StrToIntDef(FormatDateTime('yyyy',Now),1981)-1981;//Year
   m:=StrToIntDef(FormatDateTime('m',Now),1);           //Month
   d:=StrToIntDef(FormatDateTime('d',Now),1);           //Date
   cdate:=((y AND$F)<<12)OR((y AND$F0)<<1)OR(d AND$1F)OR((m AND$F)<<8);
   Write16b(cdate,afshead+$22);
   //First free cylinder at $24
   Write16b(1,afshead+$24); //Not sure why it is always 1
   // $04 at $26
   WriteByte($04,afshead+$26);
   //Copy the primary header to the copy
   for index:=0 to $FF do WriteByte(ReadByte(afshead+index),afshead2+index);
   //Now to write the root (header)
   //ID at $00
   WriteString(objID,Fafsroot,0,0);
   //Map chain sequence number at $06
   WriteByte(cycle,Fafsroot+$06);
   // $00 at $07
   WriteByte($00,Fafsroot+$07);
   //LSB of object length at $08
   WriteByte($00,Fafsroot+$08);
   // $00 at $09
   WriteByte($00,Fafsroot+$09);
   //First group of allocated sectors at $0A
   //Sector at $0A (3 bytes)
   Write24b((Fafsroot>>8)+1,Fafsroot+$0A);
   //Length at $0D (2 bytes)
   Write16b(afsroot_size>>8,Fafsroot+$0D);
   //Copy of map chain sequence number at $FF
   WriteByte(cycle,Fafsroot+$FF);
   //Write the directory object for $
   CreateAFSDirectory('$','$','DLR');
   //Write the image bitmap
   for index:=0 to mapsize-1 do WriteByte($FF,afshead2+$100+index);
   for index:=0 to (Fafsroot>>8)+2 do
    WriteBits(0,afshead2+$100+(index div 8),index mod 8,1);
   //Mark as a success
   Result:=True;
  end;
  //Finalise the the variables by reading in the partition
  if Result then ReadAFSPartition;
 end;
end;

{-------------------------------------------------------------------------------
Create a new directory
-------------------------------------------------------------------------------}
function TDiscImage.CreateAFSDirectory(dirname,parent,attributes: String): Integer;
var
 buffer : TDIByteArray;
 addr,
 ptr,
 dir,
 entry,
 dirsize: Cardinal;
 newfile: TDirEntry;
 ok     : Boolean;
const
 cycle    = 42;
begin
 Result:=-3; //Directory already exists
 ok:=False;
 if dirname='$' then ok:=True
 else ok:=not FileExists(parent+dirsep+dirname,dir,entry);
 if ok then
 begin
  Result:=-5; //Unknown error
  //Set the size
  dirsize:=$200;
  if dirname='$' then dirsize:=afsroot_size;
  //Set up the area for the directory structure
  SetLength(buffer,dirsize);
  //Pointer to first entry at $00
  Write16b($0000,$00,buffer);
  //Cycle number at $02
  WriteByte(cycle,$02,buffer);
  //Directory name at $03
  WriteString(dirname,$03,10,32,buffer);
  //Write the free pointers
  addr:=$11;
  while addr+$1A<dirsize-$1A do
  begin
   Write16b(addr,addr+$1A,buffer);
   inc(addr,$1A);
  end;
  //Pointer to first free entry at $0D
  Write16b(addr,$0D,buffer);
  //Number of entries in directory at $0F
  WriteByte(0,$0F,buffer);
  //Copy of cycle number at end of root
  WriteByte(cycle,dirsize-1,buffer);
  //First entry at $11 (pointer is $FFFF to indicate parent), for Level 3
  if(FFormat>>4=diAcornADFS)or(FFormat=diAcornFS<<4+2)then
  begin
   //Get the parent directory address
   if dirname<>'$' then
   begin
    addr:=$0;
    if parent<>'$' then
     if FileExists(parent,dir,entry) then
      addr:=FDisc[dir].Entries[entry].Sector;
    if parent='$' then addr:=Fafsroot div secsize;
   end else addr:=Fafsroot div secsize;//Unless it is root
   //Update the pointer to the parent
   if addr<>$0 then
   begin
    Write16b($FFFF,$11,buffer); //Pointer to parent
    Write24b(addr,$28,buffer);  //Parent's SIN
   end;
  end;
  //Are we writing the root?
  if dirname='$' then
  begin
   //Then we'll just copy the data across and we're done
   addr:=Fafsroot;
   if(FFormat>>4=diAcornADFS)or(FFormat=diAcornFS<<4+2)then addr:=Fafsroot+$100;
   for ptr:=0 to Length(buffer)-1 do
    WriteByte(buffer[ptr],addr+ptr);
   Result:=0;
  end
  else //Not writing the root, so just treat as a file
  begin
   ResetDirEntry(NewFile);
   NewFile.Parent:=parent;
   NewFile.Filename:=dirname;
   NewFile.Attributes:='D'+attributes;
   Result:=WriteAFSFile(NewFile,buffer);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Create a blank AFS password file (this is a public method)
-------------------------------------------------------------------------------}
function TDiscImage.CreateAFSPassword(afslevel: Byte;
                                              Accounts: TUserAccounts): Boolean;
var
 buffer  : TDIByteArray;
 index   : Integer;
 newentry: TDirEntry;
 ptr     : Cardinal;
 ok      : Boolean;
 entry   : Byte;
begin
 //Default response
 Result:=False;
 buffer:=nil; //To stop 'hints' or 'warnings' from the compiler
 //Password file is 256 bytes long
 SetLength(buffer,$100);
 //Blank it off
 for index:=0 to Length(buffer)-1 do buffer[index]:=0;
 //Check to make sure that there is a Syst account
 ok:=False;
 if Length(Accounts)>0 then
  for index:=0 to Length(Accounts)-1 do
   if Accounts[index].UserName='Syst' then
   begin
    ok:=True;
    Accounts[index].AccessLevel:=$C0; //Make sure it has system-wide access
   end;
 //None found, so add one at the top
 if not ok then
 begin
  //Make room
  SetLength(Accounts,Length(Accounts)+1);
  //Move up by one
  if Length(Accounts)>1 then
   for index:=Length(Accounts)-2 downto 0 do
    Accounts[index+1]:=Accounts[index];
  //Add the system user account
  Accounts[0].Username   :='Syst';
  Accounts[0].Password   :='';
  Accounts[0].AccessLevel:=$C0;
 end;
 //Length per entry
 if afslevel=2 then entry:=$11 else entry:=$1F;
 //Start at the beginning
 ptr:=0;
 index:=0;
 while(index<Length(Accounts))and(ptr+entry<Length(buffer))do
 begin
  //Level 2
  if afslevel=2 then
  begin
   //00 : User name (System) - 10 characters terminated by 0D
   WriteString(Accounts[index].Username+#$0D,ptr+$00,10,0,buffer);
   //0A : Password (blank) - 6 characters terminated by 0D
   WriteString(Accounts[index].Password+#$0D,ptr+$0A,6,0,buffer);
   //10 : Status byte - b7 set, b6 system, b0-1 boot option
   buffer[ptr+$10]:=Accounts[index].AccessLevel;
  end;
  //Level 3
  if afslevel=3 then
  begin
   //00 : User name (System) - 20 characters terminated by 0D
   WriteString(Accounts[index].Username+#$0D,ptr+$00,20,0,buffer);
   //14 : Password (blank) - 6 characters terminated by 0D
   WriteString(Accounts[index].Password+#$0D,ptr+$14,6,0,buffer);
   //1A : Free space - $00040404 default for user, $00FF0000 for system
   if Accounts[index].Username='Syst' then
    Write32b($00FF0000,ptr+$1A,buffer)
   else
    Write32b($00040404,ptr+$1A,buffer);
   //1E : Status byte - b7 set, b6 system, b5 locked, b0-3 boot option
   buffer[$1E]:=Accounts[index].AccessLevel;
  end;
  //Next entry
  inc(ptr,entry);
  inc(index);
 end;
 //Set up the file entry
 ResetDirEntry(newentry);
 newentry.Filename:='Passwords';
 if FFormat>>4=diAcornADFS then
  newentry.Parent :=afsrootname
 else
  newentry.Parent :='$';
 //Write the file
 Result:=WriteAFSFile(newentry,buffer)>=0;
end;

{-------------------------------------------------------------------------------
Write a file to an AFS image
-------------------------------------------------------------------------------}
function TDiscImage.WriteAFSFile(var file_details: TDirEntry;
                                              var buffer: TDIByteArray):Integer;
var
 dir,pdir,
 entry,
 partition,
 ptr,addr,
 lastaddr,
 freeaddr,
 fragnum,
 sector    : Cardinal;
 fragments : TFragmentArray;
 block     : TDIByteArray;
 y,m,d     : Byte;
 cdate     : Word;
begin
 dir:=0;
 entry:=0;
 //Start with a negative result
 Result:=-3;//File already exists
 //Validate the proposed filename (ADFS rules the same as AFS)
 if file_details.Filename<>'$' then
  file_details.Filename:=ValidateADFSFilename(file_details.Filename);
 //First make sure it doesn't exist already
 if not FileExists(file_details.Parent+dirsep+file_details.Filename,pdir,entry)then
  //Get the directory where we are adding it to, and make sure it exists
  if FileExists(file_details.Parent,pdir,entry) then
  begin
   Result:=-2; //Image full
   file_details.Sector:=0;
   //Where we are inserting this into
   if file_details.Parent='$' then
   begin
    dir:=0;
    partition:=0;
    sector:=Fafsroot*secsize;
   end
   else
   begin
    dir:=FDisc[pdir].Entries[entry].DirRef;
    partition:=FDisc[dir].Partition;
    sector:=FDisc[pdir].Entries[entry].Sector*secsize;
   end;
   //Set the length
   file_details.Length:=Length(buffer);
   //Will if fit on the disc?
   if free_space[partition]>file_details.Length then
   begin
    Result:=-4;//Directory full
    //Is there enough space in the directory?
    if Read16b(sector+$0D)<>0 then
    begin
     Result:=-5;//Unknown error
     //Allocate some space
     file_details.Sector:=AFSAllocateFreeSpace(file_details.Length,fragments)div secsize;
     //Did it allocate anything?
     if Length(fragments)>0 then
     begin
      //Is the file actually a directory?
      if Pos('D',file_details.Attributes)>0 then
      begin
       //Increase the number of directories by one
       SetLength(FDisc,Length(FDisc)+1);
       //Then assign DirRef
       file_details.DirRef:=Length(FDisc)-1;
       //Assign the directory properties
       FDisc[Length(FDisc)-1].Directory:=file_details.Filename;
       FDisc[Length(FDisc)-1].Title    :=file_details.Filename;
       FDisc[Length(FDisc)-1].Broken   :=False;
       SetLength(FDisc[Length(FDisc)-1].Entries,0);
      end
      else //Mark as not a directory
       file_details.DirRef:=-1;
      //Set the date
      file_details.TimeStamp:=Floor(Now);
      //Insert it into the local copy of the catalogue
      Result:=ExtendADFSCat(dir,file_details); //We'll just 'borrow' the ADFS method
      //Write the file data
      if(FFormat=diAcornFS<<4+2)or(FFormat>>4=diAcornADFS)then
      begin
       //First, for level 3, we'll need a header
       SetLength(block,$100);
       //Blank the header
       for ptr:=0 to Length(block)-1 do block[ptr]:=0;
       //The add the header
       WriteString('JesMap',0,0,0,block);
       //LSB of the file length
       block[8]:=file_details.Length AND$FF;
       //File fragment offsets and lengths
       for fragnum:=0 to Length(fragments)-1 do
       begin
        //First sector is the JesMap, so we don't need to point to this
        if file_details.Sector=fragments[fragnum].Offset div secsize then
        begin
         Write24b((fragments[fragnum].Offset div secsize)+1,$0A+(fragnum*5),block);
         Write16b(Ceil(fragments[fragnum].Length/secsize)-1,$0D+(fragnum*5),block);
        end
        else //Thereafter, it is OK
        begin
         Write24b(fragments[fragnum].Offset div secsize,$0A+(fragnum*5),block);
         Write16b(Ceil(fragments[fragnum].Length/secsize),$0D+(fragnum*5),block);
        end;
       end;
       block[$FF]:=block[6];
       //Now we write the header to the image
       for ptr:=0 to Length(block)-1 do
        WriteByte(block[ptr],file_details.Sector*secsize+ptr);
      end;
      //Write the data to the image (we don't need the fragments now)
      WriteAFSObject(file_details.Sector*secsize,buffer);
      //Read it in so it is local
      block:=ReadAFSObject(sector);
      //Add in the new file
      ptr:=0;
      addr:=0;
      //Find where we need to go
      while ptr<Result+1 do
      begin
       inc(ptr);
       lastaddr:=addr;
       addr:=Read16b(lastaddr,block);
      end;
      //Get the next free slot
      freeaddr:=Read16b($0D,block);
      //Get the next free slot after that and write it to the header
      Write16b(Read16b(freeaddr,block),$0D,block);
      //Change the pointer to the new entry
      Write16b(freeaddr,lastaddr,block);
      //Point the new entry to the next one, which could be zero
      Write16b(addr,freeaddr,block);
      //Increase the counter
      WriteByte(ReadByte($0F,block)+1,$0F,block);
      //Insert all the fields
      //Filename
      WriteString(file_details.Filename,freeaddr+$02,10,32,block);
      //Load Address
      Write32b(file_details.LoadAddr,freeaddr+$0C,block);
      //Execution Address
      Write32b(file_details.ExecAddr,freeaddr+$10,block);
      //Attributes
      Write32b(AFSAttrToByte(file_details.Attributes),freeaddr+$14,block);
      //Modification Date
      y:=StrToIntDef(FormatDateTime('yyyy',file_details.TimeStamp),1981)-1981;//Year
      m:=StrToIntDef(FormatDateTime('m',file_details.TimeStamp),1);           //Month
      d:=StrToIntDef(FormatDateTime('d',file_details.TimeStamp),1);           //Date
      cdate:=((y AND$F)<<12)OR((y AND$F0)<<1)OR(d AND$1F)OR((m AND$F)<<8);
      Write16b(cdate,freeaddr+$15,block);
      //SIN of object
      Write24b(file_details.Sector,freeaddr+$17,block);
      //Now we can write it back out again
      WriteAFSObject(sector,block);
      //Refresh the free space map
      ReadAFSFSM;
     end;
    end;
   end;
  end;
end;

{-------------------------------------------------------------------------------
Converts an attribute string to a byte
-------------------------------------------------------------------------------}
function TDiscImage.AFSAttrToByte(attr: String):Byte;
begin
 Result:=0;
 if Pos('D',attr)>0 then Result:=Result OR $20;
 if Pos('L',attr)>0 then Result:=Result OR $10;
 if Pos('W',attr)>0 then Result:=Result OR $08;
 if Pos('R',attr)>0 then Result:=Result OR $04;
 if Pos('w',attr)>0 then Result:=Result OR $02;
 if Pos('r',attr)>0 then Result:=Result OR $01;
end;

{-------------------------------------------------------------------------------
Write an object to an AFS image
-------------------------------------------------------------------------------}
procedure TDiscImage.WriteAFSObject(offset: Cardinal;var buffer: TDIByteArray);
var
 ptr,
 addr,
 len,
 bufptr,
 almap : Cardinal;
 index : Integer;
begin
 //Make sure it is a valid allocation map for Level 3
 if(ReadString(offset,-6)='JesMap')
 and((FFormat=diAcornFS<<4+2)or(FFormat>>4=diAcornADFS))then
 begin
  //Start of the list
  ptr:=$0A;
  //Read the first entry and length
  addr:=$FF;
  //Pointer into the data
  bufptr:=0;
  while(addr<>0)and(ptr<$FA)and(bufptr<Length(buffer))do
  begin
   addr:=Read24b(offset+ptr)*secsize;
   len:=Read16b(offset+ptr+3)*secsize;
   //Copy from the buffer
   for index:=0 to len-1 do
    if bufptr+index<Length(buffer) then
     WriteByte(buffer[bufptr+index],addr+index);
   //Move the buffer pointer on
   inc(bufptr,len);
   //Move onto the next entry
   inc(ptr,5);
  end;
 end;
 //We are looking at a Level 2 map, with no ID string
 if(FFormat=diAcornFS<<4+1)and(offset<>0)then
 begin
  offset:=offset div secsize;
  //Get the address of the current allocation map
  almap:=GetAllocationMap;
  //Pointer into the buffer
  bufptr:=0;
  //Find our entry and read it in
  repeat
   addr:=Read16b(almap+(offset*2)+5);
   len:=secsize; //Length of data to write out
   if (addr AND $4000)=$4000 then
   begin
    len:=addr AND $FF;//Last sector, so bytes used
    if len=0 then len:=secsize; //if zero, then it must be a full sector
   end;
   if (addr AND $1000)=$1000 then len:=0; //This entry is empty
   if len>0 then //Copy the data from the buffer
    for index:=0 to len-1 do
     if bufptr+index<Length(buffer) then
      WriteByte(buffer[bufptr+index],offset*secsize+index);
   inc(bufptr,len);
   if (addr AND $4000)<>$4000 then offset:=addr AND $FFF; //This is the next sector in the chain
  until((addr AND $4000)=$4000)or(bufptr>=Length(buffer)); //Continue until everything is written
 end;
end;

{-------------------------------------------------------------------------------
Rename an object
-------------------------------------------------------------------------------}
function TDiscImage.RenameAFSFile(oldname:String;var newname: String): Integer;
var
 dir,
 entry,
 ptr    : Cardinal;
 swap   : TDirEntry;
 changed: Boolean;
begin
 Result:=-2;//Original file does not exist
 //Check that the new name meets the required AFS filename specs
 newname:=ValidateAFSFilename(newname);
 if FileExists(oldname,dir,entry) then
 begin
  Result:=-3;//New name already exists
  //Check that the new name does not already exist
  if(not FileExists(FDisc[dir].Entries[entry].Parent+dirsep+newname,ptr))
  // or the user is just changing case
  or(LowerCase(FDisc[dir].Entries[entry].Parent+dirsep+newname)=LowerCase(oldname))then
  begin
   Result:=-1;//Unknown error
   //Just update the entry
   FDisc[dir].Entries[entry].Filename:=newname;
   //If it is a directory itself, change the name in it's header
   if FDisc[dir].Entries[entry].DirRef<>-1 then
   begin
    //Change the name in the local copy
    FDisc[FDisc[dir].Entries[entry].DirRef].Directory:=newname;
    //Get the address of the directory
    ptr:=FDisc[dir].Entries[entry].Sector*secsize;
    if(FFormat>>4=diAcornADFS)or(FFormat=diAcornFS<<4+2)then //Level 3
     ptr:=Read24b(ptr+$0A)*secsize;
    //And write it to the header
    WriteString(newname,ptr+$03,10,32);
   end;
   //Reorder the entries
   if Length(FDisc[dir].Entries)>1 then
    repeat
     changed:=False;
     if entry<Length(FDisc[dir].Entries)-1 then
      //Move up the list
      if FDisc[dir].Entries[entry].Filename>FDisc[dir].Entries[entry+1].Filename then
      begin
       swap:=FDisc[dir].Entries[entry];
       FDisc[dir].Entries[entry]:=FDisc[dir].Entries[entry+1];
       FDisc[dir].Entries[entry+1]:=swap;
       entry:=entry+1;
       changed:=True;
      end;
     if entry>0 then
      //Move down the list
      if FDisc[dir].Entries[entry].Filename<FDisc[dir].Entries[entry-1].Filename then
      begin
       swap:=FDisc[dir].Entries[entry];
       FDisc[dir].Entries[entry]:=FDisc[dir].Entries[entry-1];
       FDisc[dir].Entries[entry-1]:=swap;
       entry:=entry-1;
       changed:=True;
      end;
     until not changed;
   //And update the parent directory
   UpdateAFSDirectory(FDisc[dir].Entries[entry].Parent);
   //Return the new entry
   Result:=entry;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update the contents of a directory
-------------------------------------------------------------------------------}
procedure TDiscImage.UpdateAFSDirectory(dirname: String);
var
 dir,
 entry,
 addr,
 ptr      : Cardinal;
 index    : Integer;
 pointers : array of Cardinal;
 buffer   : TDIByteArray;
begin
 pointers:=nil;
 if(FileExists(dirname,dir,entry))or(dirname='$')then
 begin
  //Get the directory reference and the address of the directory data
  if dirname='$' then //Root?
  begin
   dir:=0;
   addr:=Fafsroot*secsize;
  end
  else //Not root
  begin
   addr:=FDisc[dir].Entries[entry].Sector*secsize;
   dir:=FDisc[dir].Entries[entry].DirRef;
  end;
  //We'll only continue if there are any entries
  if Length(FDisc[dir].Entries)>0 then
  begin
   //Read in the directory so it is local
   buffer:=ReadAFSObject(addr);
   //Get the list of pointers
   //Add the starting zero
   SetLength(pointers,Length(pointers)+1);
   pointers[Length(pointers)-1]:=$000;
   //Go through each entry
   for index:=0 to Length(FDisc[dir].Entries)-1 do
   begin
    //Start at the beginning of the chain
    ptr:=Read16b($00,buffer);
    while(ptr<>$0000) //We're looking for this entry's position
    and(FDisc[dir].Entries[index].Sector<>Read16b(ptr+$17,buffer))do
     //Follow the chain until we find this entry
     ptr:=Read16b(ptr,buffer);
    //Record it
    SetLength(pointers,Length(pointers)+1);
    pointers[Length(pointers)-1]:=ptr;
    //Make sure the details are up to date
    WriteString(FDisc[dir].Entries[index].Filename,ptr+$02,10,32,buffer);
    Write32b(FDisc[dir].Entries[index].LoadAddr,ptr+$0C,buffer);
    Write32b(FDisc[dir].Entries[index].ExecAddr,ptr+$10,buffer);
    WriteByte(AFSAttrToByte(FDisc[dir].Entries[index].Attributes),ptr+$14,buffer);
   end;
   //Add the terminating zero
   SetLength(pointers,Length(pointers)+1);
   pointers[Length(pointers)-1]:=$000;
   //Now we have a list of pointers in the correct order. We just write them out
   //to the entries. Remember: there are 2 more pointers than entries.
   for index:=0 to Length(FDisc[dir].Entries) do
    Write16b(pointers[index+1],pointers[index],buffer);
   //Write out the modified directory
   WriteAFSObject(addr,buffer);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Validate a filename
-------------------------------------------------------------------------------}
function TDiscImage.ValidateAFSFilename(filename: String): String;
var
 i: Integer;
const
  illegal = '#* .:$&@';
begin
 for i:=1 to Length(filename) do
 begin
  //Remove top-bit set characters
  filename[i]:=chr(ord(filename[i])AND$7F);
  //and remove control codes
  if ord(filename[i])<32 then filename[i]:=chr(ord(filename[i])+32);
 end;
 //Is it not too long
 filename:=Copy(filename,1,10);
 //Remove any forbidden characters
 for i:=1 to Length(filename) do
  if Pos(filename[i],illegal)>0 then filename[i]:='_';
 Result:=filename;
end;
