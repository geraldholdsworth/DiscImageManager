//++++++++++++++++++ Acorn DOS Plus ++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a DOS Plus disc and which type
-------------------------------------------------------------------------------}
function TDiscImage.ID_DOSPlus: Boolean;
var
 idbyte: Byte;
begin
 ResetVariables;
 Result:=False;
 //This will only identify a plain DOS Plus disc, not an ADFS Hybrid
 if Read16b($0001)=$FFFF then
 begin
  //Disc ID Byte
  idbyte:=ReadByte($0000);
  //Set the format based on this ID
  if idbyte>$FB then
  begin
   FFormat:=diDOSPlus<<4;
   //Sectors per track
   secspertrack:=9;
   if idbyte>$FD then secspertrack:=8;
   //Sector size
   secsize:=256;
   doshead:=0;
   //Set root address and root size
   root:=$800;
   root_size:=$C0*$20;
   Fdosroot:=root;
   dosroot_size:=root_size;
   //Set the disc size
   disc_size[0]:=GetDataLength;
   //And cluster size
   cluster_size:=$400;
   dosalloc:=1;
   DOSBlocks:=disc_size[0]div cluster_size;
   //FAT Size and type
   DOSFATSize:=1; //This is one less than the actual size
   NumFATs:=1;
   FATType:=diFAT12;
   //Where is the FAT?
   dosmap:=doshead;
   dosmap2:=doshead;
   //Attribute of the first entry of the root will indicate the volume name (bit 3)
   if ReadByte(root+$B)<>$8 then FFormat:=diInvalidImg;
  end;
 end;
 //Normal DOS disc (i.e. has a header)?
 if FFormat=diInvalidImg then //Only check if nothing found
  if IDDOSPartition($0000) then
  begin
   ReadDOSHeader;
   disc_size[0]:=DOSBlocks*cluster_size;
   FFormat:=diDOSPlus<<4;
  end;
 Result:=FFormat>>4=diDOSPlus;
end;

{-------------------------------------------------------------------------------
ID a DOS Partition, with header
-------------------------------------------------------------------------------}
function TDiscImage.IDDOSPartition(ctr: Cardinal): Boolean;
var
 ds: Word;
begin
 Result:=False;
 //Is there E9 or EB stored here
 if(ReadByte(ctr)=$E9)or(ReadByte(ctr)=$EB)then
 begin
  //Read in the block size
  ds:=Read16b(ctr+$B);
  //Is there a FAT partition?
  if Read16b(ctr+ds+1)=$FFFF then
  begin
   //Mark as DOS Partition present
   FDOSPresent:=True;
   //DOS Header
   doshead:=ctr;
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Reads a DOS Plus partition
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadDOSPartition;
var
 i,d,e   : Integer;
 lenctr  : Cardinal;
 part    : Byte;
// startdir: String;
begin
 //Is this an ADFS disc with DOS Plus partition?
 if((FFormat>>4=diAcornADFS)and(FDOSPresent))
 or(FFormat>>4=diDOSPlus)then //Or a straight DOS Plus?
 begin
  i:=0;
  part:=0;
  if FFormat>>4=diDOSPlus then dir_sep:='\';
  //ADFS Hybrid?
  if FFormat>>4=diAcornADFS then
  begin
   part:=1;
   //Set up the second partition
   SetLength(disc_size,2);
   SetLength(free_space,2);
   SetLength(disc_name,2);
   if FFormat AND $F<>$F then //Not for hard disc partitions
   begin
    doshead   :=disc_size[0];
    //Set the DOS root parameters
    dosmap:=doshead;
    dosmap2:=doshead;
    dosroot_size:=$70*$20;
    //And cluster size
    cluster_size:=$100;
    dosalloc:=8;
    //FAT Size and type
    DOSFATSize:=1; //This is one less than the actual size
    NumFATs:=1;    //Number of FATs present
    FATType:=diFAT12;//FAT type
    disc_size[1]:=GetDataLength-disc_size[0]; //Partition size
    DOSBlocks:=disc_size[1]div cluster_size; //Number of blocks on disc
    Fdosroot:=doshead+((NumFATs*DOSFATSize)+1)*cluster_size; //Where the root is
   end;
   if FFormat AND $F=$F then //Hard disc partition - will have a DOS Header
   begin
    doshead:=disc_size[0]; //Where the DOS header is
    ReadDOSHeader; //Read the DOS Header
    disc_size[1]:=DOSBlocks*cluster_size;//Disc size in bytes
   end;
   i:=1;
  end;
  //Update the progress indicator
  UpdateProgress('Reading DOS Plus partition');
  //Read the volume title
  if ReadByte(Fdosroot+$B)AND$8=$8 then
   disc_name[i]:=ReadString(Fdosroot,-11)
  else
   disc_name[i]:='';
  RemoveSpaces(disc_name[i]);
  //Add a new entry
  d:=Length(FDisc);
  SetLength(FDisc,d+1);
  //Start the chain by reading the root
  if FFormat=diAcornADFS<<4+$F then dosrootname:='C:' else dosrootname:='A:';
  //Read the root
  FDisc[d]:=ReadDOSDirectory(dosrootname,Fdosroot,lenctr);
  //Now go through the root's entries and read them in
  repeat
   if Length(FDisc[d].Entries)>0 then
    for e:=0 to Length(FDisc[d].Entries)-1 do
     if FDisc[d].Entries[e].Filename<>'' then //Needs to have an actual name
      //If it is a directory, read that in
      if Pos('D',FDisc[d].Entries[e].Attributes)>0 then
      begin
       //Making room for it
       SetLength(FDisc,Length(FDisc)+1);
       //And now read it in
       FDisc[Length(FDisc)-1]:=ReadDOSDirectory(GetParent(d)
                                               +GetDirSep(part)
                                               +FDisc[d].Entries[e].Filename,
                                               FDisc[d].Entries[e].Sector,
                                               lenctr);
       FDisc[Length(FDisc)-1].Parent:=d;
       //Set the length of the directory
       FDisc[d].Entries[e].Length:=lenctr;
       //Reference to it
       FDisc[d].Entries[e].DirRef:=Length(FDisc)-1;
      end;
   inc(d);
  until d>=Length(FDisc);
  ReadDOSFSM;
 end;
end;

{-------------------------------------------------------------------------------
Reads a DOS Header. doshead must be set prior to calling this
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadDOSHeader;
begin
 dosroot_size:=Read16b(doshead+$11)*$20; //Size of the root
 cluster_size:=Read16b(doshead+$B);      //Block (cluster) size
 DOSFATSize  :=Read16b(doshead+$16);     //FAT size in blocks
 dosalloc    :=ReadByte(doshead+$D);     //Allocation unit in blocks
 NumFATs     :=ReadByte(doshead+$10);    //Number of FATs
 DOSBlocks   :=Read16b(doshead+$13);     //Disc Size (if <65536 blocks)
 if DOSBlocks=0 then
  DOSBlocks:=Read32b(doshead+$20);    //Disc Size (if >65535 blocks)
 FATType:=diFAT12;
 if(DOSBlocks>4086)and(DOSBlocks<65526)then FATType:=diFAT16;
 if DOSBlocks>65525 then FATType:=diFAT32;
 //Where the FAT(s) is(are)
 dosmap:=doshead+cluster_size;
 if NumFATs>1 then dosmap2:=dosmap+DOSFATSize*cluster_size else dosmap2:=dosmap;
 //Where the root is
 Fdosroot:=doshead+((NumFATs*DOSFATSize)+1)*cluster_size;
end;

{-------------------------------------------------------------------------------
Reads a DOS Plus Directory
-------------------------------------------------------------------------------}
function TDiscImage.ReadDOSDirectory(dirname: String;addr: Cardinal;
                                             var len: Cardinal): TDir;
var
 index,
 entry,
 side  : Integer;
 attr,
 status: Byte;
 ext   : String;
 buffer: TDIByteArray;
begin
 Result.Directory:='';
 //Setup the container
 ResetDir(Result);
 //Don't need the parent or self referrals
 if(RightStr(dirname,1)='.')or(RightStr(dirname,2)='..')then exit;
 //ADFS hybrid?
 if FFormat>>4=diAcornADFS then side:=1 else side:=0;
 //Directory name
 index:=Pos(GetDirSep(side),dirname);
 while Pos(GetDirSep(side),dirname,index+1)>index do
  index:=Pos(GetDirSep(side),dirname,index+1);
 if index>0 then
  Result.Directory:=Copy(dirname,index+1)
 else
  Result.Directory:=dirname;
 Result.Sector:=addr;
 //And set the partition flag to true
 Result.DOSPartition:=True;
 Result.Partition:=side;
 //Update the progress indicator
 UpdateProgress('Reading '+dirname);
 //First, read the data into a buffer
 if dirname=dosrootname then
 begin //This is the root, so the address will be a physical address
  SetLength(buffer,dosroot_size);
  ReadDiscData(addr,dosroot_size,side,0,buffer);
 end
 else //Otherwise it is the starting cluster
  buffer:=ReadDOSObject(addr);
 //Set the directory length
 len:=Length(buffer);
 //Start at the beginning
 index:=0;
 entry:=0;
 //Get the status byte (first byte of the filename)
 status:=ReadByte(index*32,buffer);
 while status<>0 do
 begin
  //Read the attribute byte
  attr:=ReadByte($B+index*32,buffer);
  //Continue if valid filename, and not volume name
  if(status<>$E5)and(status<>$00)and(attr AND$8=0)then
  begin
   //Add a new entry
   SetLength(Result.Entries,Length(Result.Entries)+1);
   entry:=Length(Result.Entries)-1;
   //Reset it to defaults
   ResetDirEntry(Result.Entries[entry]);
   //Read in the details
   Result.Entries[entry].Filename:=ReadString($00+index*32,-8,buffer); //Filename
   //Replace 0x05 with 0xE5
   if Ord(Result.Entries[entry].Filename[1])=$05 then
    Result.Entries[entry].Filename[5]:=Chr($E5);
   //Remove the trailing spaces
   RemoveSpaces(Result.Entries[entry].Filename);
   ext:=ReadString($08+index*32,-3,buffer); //Extension
   RemoveSpaces(ext);
   if ext<>'' then
    Result.Entries[entry].Filename:=Result.Entries[entry].Filename+'.'+ext;
   Result.Entries[entry].Timestamp:=ConvertDOSTimeDate(Read16b($16+index*32,buffer),
                                                       Read16b($18+index*32,buffer));//Time/date
   Result.Entries[entry].Attributes:=ConvertDOSAttributes(attr);//Attributes
   Result.Entries[entry].Sector   :=Read16b($1A+index*32,buffer); //Starting cluster
   Result.Entries[entry].Length   :=Read32b($1C+index*32,buffer); //Filelength
   Result.Entries[entry].DirRef   :=-1; //Directory Reference
   Result.Entries[entry].Parent   :=dirname; //Parent
   Result.Entries[entry].ShortFileType:=ext;
   Result.Entries[entry].FileType:=DOSExtToFileType(ext);
   Result.Entries[entry].Side    :=side;
   //If the filename is '.' or '..' then remove it
   if(Result.Entries[entry].Filename='.')
   or(Result.Entries[entry].Filename='..')then
    SetLength(Result.Entries,Length(Result.Entries)-1);
  end;
  //Move onto the next entry
  inc(index);
  status:=ReadByte(index*32,buffer);
 end;
end;

{-------------------------------------------------------------------------------
Convert a file extension to a filetype string
-------------------------------------------------------------------------------}
function TDiscImage.DOSExtToFileType(ext: String): String;
var
 index : Integer;
begin
 index:=Low(DOSFileTypes);
 while(ext<>UpperCase(LeftStr(DOSFileTypes[index],3)))
   and(index<=High(DOSFileTypes))do inc(index);
 if ext=UpperCase(LeftStr(DOSFileTypes[index],3)) then
  Result:=Copy(DOSFileTypes[index],4)
 else
  Result:='';
end;

{-------------------------------------------------------------------------------
Convert time and date words to a time date value
-------------------------------------------------------------------------------}
function TDiscImage.ConvertDOSTimeDate(time,date: Word): TDateTime;
var
 year    : Word;
 month,
 day,
 hour,
 minute,
 second  : Byte;
begin
 //Decode into the separate components
 year  :=1980+(date>>9);
 month :=(date AND$1E0)>>5;
 day   :=date AND$1F;
 hour  :=(time>>$B)mod 24;
 minute:=((time AND$7E0)>>5)mod 60;
 second:=((time AND$1F)*2)mod 60;
 //Range check
 if(month=0)or(month>12)then month :=1;
 if(day=0)or(day>31)    then day   :=1;
 //Encode
 Result:=EncodeDateTime(year,month,day,hour,minute,second,0);
end;

{-------------------------------------------------------------------------------
Convert time and date stamp to a time DOS word value
-------------------------------------------------------------------------------}
function TDiscImage.DOSTime(time: TDateTime): Word;
var
 year,
 month,
 day,
 hour,
 minute,
 second,
 ms      : Word;
begin
 DecodeDateTime(time,year,month,day,hour,minute,second,ms);
 Result:=(hour<<$B)OR(minute<<5)OR(second div 2);
end;

{-------------------------------------------------------------------------------
Convert time and date stamp to a date DOS word value
-------------------------------------------------------------------------------}
function TDiscImage.DOSDate(date: TDateTime): Word;
var
 year,
 month,
 day,
 hour,
 minute,
 second,
 ms      : Word;
begin
 DecodeDateTime(date,year,month,day,hour,minute,second,ms);
 Result:=((year-1980)<<9)OR(month<<5)OR day;
end;

{-------------------------------------------------------------------------------
Convert attribute byte to a string
-------------------------------------------------------------------------------}
function TDiscImage.ConvertDOSAttributes(attr: Byte): String;
begin
 Result:='';
 if attr AND $01=$01 then Result:=Result+'R'; //Read only
 if attr AND $02=$02 then Result:=Result+'H'; //Hidden
 if attr AND $04=$04 then Result:=Result+'S'; //System
 if attr AND $08=$08 then Result:=Result+'V'; //Volume label
 if attr AND $10=$10 then Result:=Result+'D'; //Directory
 if attr AND $20=$20 then Result:=Result+'A'; //Archive
end;

{-------------------------------------------------------------------------------
Convert attribute string to a byte
-------------------------------------------------------------------------------}
function TDiscImage.ConvertDOSAttributes(attr: String): Byte;
begin
 Result:=0;
 attr:=UpperCase(attr);
 if Pos('R',attr)>0 then Result:=Result OR $01; //Read only
 if Pos('H',attr)>0 then Result:=Result OR $02; //Hidden
 if Pos('S',attr)>0 then Result:=Result OR $04; //System
 if Pos('V',attr)>0 then Result:=Result OR $08; //Volume label
 if Pos('D',attr)>0 then Result:=Result OR $10; //Directory
 if Pos('A',attr)>0 then Result:=Result OR $20; //Archive
end;

{-------------------------------------------------------------------------------
Convert a cluster number to a disc offset
-------------------------------------------------------------------------------}
function TDiscImage.DOSClusterToOffset(cluster: Cardinal): Cardinal;
begin
 Result:=((cluster-2)*cluster_size*dosalloc)+Fdosroot+dosroot_size;
end;

{-------------------------------------------------------------------------------
Get the entry from the FAT for a given cluster number
-------------------------------------------------------------------------------}
function TDiscImage.GetClusterEntry(cluster: Cardinal): Cardinal;
var
 FAToffset: Cardinal;
begin
 Result:=$FFFFFFF7; //Bad cluster, by default
 if FATType=diFAT12 then
 begin
  //Get the offset into the FAT
  FAToffset:=(cluster*3)div 2;
  //Get the next cluster number
  if (cluster*3)mod 2=0 then
   Result:=Read16b(FAToffset+dosmap)AND$FFF
  else
   Result:=Read16b(FAToffset+dosmap)>>4;
 end;
 if FATType=diFAT16 then
 begin
  //Get the offset into the FAT
  FAToffset:=cluster*2;
  //Get the next cluster number
  Result:=Read16b(FAToffset+dosmap);
 end;
 if FATType=diFAT32 then
 begin
  //Get the offset into the FAT
  FAToffset:=cluster*4;
  //Get the next cluster number
  Result:=Read32b(FAToffset+dosmap);
 end;
end;

{-------------------------------------------------------------------------------
Set the entry from the FAT for a given cluster number
-------------------------------------------------------------------------------}
procedure TDiscImage.SetClusterEntry(cluster,entry: Cardinal);
var
 FAToffset,
 FATentry  : Cardinal;
begin
 if FATType=diFAT12 then
 begin
  //Make sure the entry is 12 bits
  entry:=entry AND $FFF;
  //Get the offset into the FAT
  FAToffset:=(cluster*3)div 2;
  //Get the current entry
  FATentry:=Read16b(FAToffset+dosmap);
  //Adjust to add our entry
  if (cluster*3)mod 2=0 then
   FATentry:=(FATentry AND$F000)OR entry
  else
   FATentry:=(FATentry AND$000F)OR(entry<<4);
  //Write it back
  Write16b(FATentry,FAToffset+dosmap);
  if dosmap<>dosmap2 then Write16b(FATentry,FAToffset+dosmap2);
 end;
 if FATType=diFAT16 then
 begin
  //Make sure the entry is 16 bits
  entry:=entry AND $FFFF;
  //Get the offset into the FAT
  FAToffset:=cluster*2;
  //Write the entry
  Write16b(entry,FAToffset+dosmap);
  if dosmap<>dosmap2 then Write16b(FATentry,FAToffset+dosmap2);
 end;
 if FATType=diFAT32 then
 begin
  //Make sure the entry is 32 bits
  entry:=entry AND $FFFFFFFF;
  //Get the offset into the FAT
  FAToffset:=cluster*4;
  //Get the next cluster number
  Write32b(entry,FAToffset+dosmap);
  if dosmap<>dosmap2 then Write32b(FATentry,FAToffset+dosmap2);
 end;
end;

{-------------------------------------------------------------------------------
Determines if the given cluster number is valid
-------------------------------------------------------------------------------}
function TDiscImage.IsClusterValid(cluster: Cardinal): Boolean;
begin
 Result:=cluster>1;
 if FATType=diFAT12 then Result:=(Result)AND(cluster<     $FF0);
 if FATType=diFAT16 then Result:=(Result)AND(cluster<    $FFF0);
 if FATType=diFAT32 then Result:=(Result)AND(cluster<$FFFFFFF0);
end;

{-------------------------------------------------------------------------------
Get the cluster chain as an array of fragments
-------------------------------------------------------------------------------}
function TDiscImage.GetClusterChain(cluster:Cardinal;len:Cardinal=0):TFragmentArray;
var
 nolengiven: Boolean;
 lenctr    : Cardinal;
begin
 Result:=nil;
 SetLength(Result,0);
 if len=0 then nolengiven:=True else nolengiven:=False;
 //Go through the FAT, start at the starting cluster, and build the fragment array
 if IsClusterValid(cluster)then //Add the first sector, if valid
 begin
  SetLength(Result,1);
  Result[0].Offset:=cluster;
  if(cluster_size*dosalloc<len)or(nolengiven)then
   Result[0].Length:=cluster_size*dosalloc
  else
   Result[0].Length:=len;
 end;
 //If no length given, set the length counter to a dummy value, other than zero
 if nolengiven then lenctr:=1
 else lenctr:=len-Result[0].Length; //Otherwise set it to the total length
 while(IsClusterValid(cluster))and(lenctr>0)do //Continue while valid
 begin
  //Get the next entry
  cluster:=GetClusterEntry(cluster);
  //If valid, add a fragment
  if IsClusterValid(cluster) then
  begin
   SetLength(Result,Length(Result)+1);
   Result[Length(Result)-1].Offset:=cluster;
   if(cluster_size*dosalloc<lenctr)or(nolengiven)then
    Result[Length(Result)-1].Length:=cluster_size*dosalloc
   else
    Result[Length(Result)-1].Length:=lenctr;
   if not nolengiven then dec(lenctr,Result[Length(Result)-1].Length);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Set the cluster chain given in the array of fragments
-------------------------------------------------------------------------------}
procedure TDiscImage.SetClusterChain(fragments: TFragmentArray);
var
 index: Integer;
begin
 if Length(fragments)>0 then
 begin
  //Update all but the last entry
  if Length(fragments)>1 then
   for index:=0 to Length(fragments)-2 do
   begin
    SetClusterEntry(fragments[index].Offset,fragments[index+1].Offset);
   end;
  //Now we can put the end of cluster chain marker in the last position
  SetClusterEntry(fragments[Length(fragments)-1].Offset,$FFFFFFFF);
 end;
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path
-------------------------------------------------------------------------------}
function TDiscImage.ExtractDOSFile(filename: String;
                                             var buffer: TDIByteArray): Boolean;
var
 dir,
 entry : Cardinal;
begin
 Result:=False;
 if FileExists(filename,dir,entry) then //Does the file actually exist?
 begin
  //Reset the buffer
  SetLength(buffer,0);
  //Read the file data
  buffer:=ReadDOSObject(FDisc[dir].Entries[entry].Sector,
                        FDisc[dir].Entries[entry].Length);
  //Return a result, +ve or -ve
  Result:=Length(buffer)>0;
 end;
end;

{-------------------------------------------------------------------------------
Extracts an object, given starting cluster
-------------------------------------------------------------------------------}
function TDiscImage.ReadDOSObject(cluster:Cardinal;len:Cardinal=0):TDIByteArray;
var
 frag,
 index,
 ptr       : Cardinal;
 fragments : TFragmentArray;
begin
 Result:=nil;
 //Reset the buffer
 SetLength(Result,0);
 //Read the chain of clusters into a fragment array
 fragments:=GetClusterChain(cluster,len);
 //Any fragments?
 if Length(fragments)>0 then
 begin
  //Go through each fragment
  for frag:=0 to Length(fragments)-1 do
  begin
   //Make a note of the current position in the buffer
   ptr:=Length(Result);
   //Then extend it
   SetLength(Result,Length(Result)+fragments[frag].Length);
   //Copy the data for this cluster across
   if fragments[frag].Length>0 then //Ensure that there is data there
    for index:=0 to fragments[frag].Length-1 do
     Result[ptr+index]:=ReadByte(DOSClusterToOffset(fragments[frag].Offset)+index);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Reads the free space map for a DOS Plus image
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadDOSFSM;
var
 fragments: TFragmentArray;
 part     : Byte;
 tracks,
 entry,
 index,
 t,s      : Cardinal;
const spt = 9;
begin
 //Get all the used sectors
 fragments:=DOSGetFreeSectors(True);
 //Initialise the variables
 if FFormat>>4=diDOSPlus then
 begin
  SetLength(free_space_map,1);
  part:=0;
 end;
 if FFormat>>4=diAcornADFS then //Hybrids - DOS will take up the second 'side'
 begin
  SetLength(free_space_map,2);
  part:=1;
 end;
 //Initialise the free space
 free_space[part]:=disc_size[part];
 //Set up the array
 tracks:=Ceil((disc_size[part]div cluster_size)/spt);
 SetLength(free_space_map[part],tracks);
 for entry:=0 to Length(free_space_map[part])-1 do //Sectors per track
 begin
  SetLength(free_space_map[part,entry],spt);
  for index:=0 to spt-1 do free_space_map[part,entry,index]:=$00;
 end;
 //If there are any used spaces, add them to the FSM
 if Length(fragments)>0 then
  for index:=0 to Length(fragments)-1 do
  begin
   for entry:=0 to (fragments[index].Length div cluster_size)-1 do
   begin
    //Get the track and sector
    t:=(((fragments[index].Offset-doshead)div cluster_size)+entry)div spt;
    s:=(((fragments[index].Offset-doshead)div cluster_size)+entry)mod spt;
    if t<Length(free_space_map[part]) then //Make sure it is within range
     if s<Length(free_space_map[part,t]) then
     begin
      free_space_map[part,t,s]:=$FF-fragments[index].Zone;
      dec(free_space[part],cluster_size); //Decrease the free space
     end;
   end;
  end;
end;

{-------------------------------------------------------------------------------
Get an array of all the free sectors (or used, if set to True)
-------------------------------------------------------------------------------}
function TDiscImage.DOSGetFreeSectors(used: Boolean=False): TFragmentArray;
var
 LFATSize,
 cluster,
 entry,
 index    : Cardinal;
begin
 Result:=nil;
 //Actual FAT Size
 LFATSize:=DOSBlocks div dosalloc;
 if FATType=diFAT12 then LFATSize:=((LFATSize*3)+1)div 2;//12 bits per cluster
 if FATType=diFAT16 then LFATSize:=(LFATSize+1)*2;       //16 bits per cluster
 //Add the system areas, if used is requested
 if used then
 begin
  SetLength(Result,Length(Result)+1);
  Result[Length(Result)-1].Offset:=doshead;
  Result[Length(Result)-1].Length:=(Fdosroot+dosroot_size)-doshead;
  Result[Length(Result)-1].Zone  :=$01; //Marker for system area
 end;
 //Add the rest of the FAT
 for cluster:=2 to DOSBlocks div dosalloc do
 begin
  entry:=GetClusterEntry(cluster); //Get the entry for this cluster number
  //Add a used fragment
  if(used)and(entry<>0)then
  begin
   SetLength(Result,Length(Result)+1);
   Result[Length(Result)-1].Offset:=DOSClusterToOffset(cluster);
   Result[Length(Result)-1].Length:=cluster_size*dosalloc;
   Result[Length(Result)-1].Zone  :=$00;
  end;
  //Add a free fragment
  if(not used)and(entry=0)then
  begin
   SetLength(Result,Length(Result)+1);
   Result[Length(Result)-1].Offset:=DOSClusterToOffset(cluster);
   Result[Length(Result)-1].Length:=cluster_size*dosalloc;
   Result[Length(Result)-1].Zone  :=$00;
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
   if Length(Result)>2 then
    if(Result[index].Offset=Result[index-1].Offset+Result[index-1].Length)
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
Rename a DOS file
-------------------------------------------------------------------------------}
function TDiscImage.RenameDOSFile(oldname:String;var newname: String): Integer;
var
 dir,
 entry,
 ptr   : Cardinal;
 ext   : String;
 side  : Byte;
begin
 //ADFS hybrid?
 if FFormat>>4=diAcornADFS then side:=1 else side:=0;
 Result:=-2; //Original file does not exist
 //Validate the filename
 newname:=ValidateDOSFilename(newname);
 if FileExists(oldname,dir,entry) then
 begin
  Result:=-3; //New filename already exists
  //Check that the proposed new name does not exist
  if(not FileExists(GetParent(dir)+GetDirSep(side)+newname,ptr))
  // or the user is just changing case
  or(LowerCase(GetParent(dir)+GetDirSep(side)+newname)=LowerCase(oldname))then
  begin
   Result:=-1; //Unknown error
   //Change the name
   FDisc[dir].Entries[entry].Filename:=newname;
   //And extension
   if Pos('.',newname)>0 then
   begin
    ext:=newname;
    while Pos('.',ext)>0 do ext:=Copy(ext,Pos('.',ext)+1);
   end
    else ext:='';
   FDisc[dir].Entries[entry].ShortFiletype:=ext;
   FDisc[dir].Entries[entry].FileType:=DOSExtToFileType(ext);
   //Update the directory
   UpdateDOSDirectory(GetParent(dir));
   //Return the entry
   Result:=entry;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Validate a DOS filename
-------------------------------------------------------------------------------}
function TDiscImage.ValidateDOSFilename(filename: String): String;
var
 fn,
 ext    : String;
 index1,
 index2 : Integer;
const
  illegal = '"*/:<>?\|+,.;=[]';
begin
 fn:=filename;
 Result:='';
 //Extract the extension
 if Pos('.',fn)>0 then
 begin
  ext:=fn;
  while Pos('.',ext)>0 do ext:=Copy(ext,Pos('.',ext)+1);
 end
  else ext:='';
 //Remove the extension from the filename
 if ext<>'' then fn:=LeftStr(fn,Length(fn)-(Length(ext)+1));
 //Validate the length (or, rather, truncate them)
 fn:=LeftStr(fn,8);
 ext:=LeftStr(ext,3);
 //Remove any trailing spaces
 RemoveSpaces(fn);
 RemoveSpaces(ext);
 //Now we can validate both
 for index1:=1 to Length(illegal) do
 begin
  for index2:=1 to Length(fn) do
   if fn[index2]=illegal[index1] then fn[index2]:='_';
  for index2:=1 to Length(ext) do
   if ext[index2]=illegal[index1] then ext[index2]:='_';
 end;
 //Return a validated filename
 Result:=Result+fn;
 if ext<>'' then Result:=Result+'.'+ext;
end;

{-------------------------------------------------------------------------------
Update the contents of a directory
-------------------------------------------------------------------------------}
procedure TDiscImage.UpdateDOSDirectory(dirname: String);
var
 dir,
 entry,
 addr,
 ptr,
 dirlenst,
 dirlen    : Cardinal;
 index     : Integer;
 isroot    : Boolean;
 temp      : String;
 buffer    : TDIByteArray;
 side      : Byte;
 fragments : TFragmentArray;
begin
 if(FileExists(dirname,dir,entry))or(dirname=dosrootname)then
 begin
  isroot:=False;
  //Get the directory reference and the address of the directory data
  if dirname=dosrootname then //Root?
  begin
   dir:=0;
   //Get the directory reference of the root
   if Length(FDisc)>0 then
    for index:=0 to Length(FDisc)-1 do
     if FDisc[index].Directory=dosrootname then dir:=index;
   addr:=Fdosroot;
   isroot:=True;
  end
  else //Not root
  begin
   if dir<Length(FDisc) then
    if entry<Length(FDisc[dir].Entries) then
    begin
     addr:=FDisc[dir].Entries[entry].Sector;
     dir:=FDisc[dir].Entries[entry].DirRef;
    end
    else //Must be the root (as 'A:' or 'C:')
    begin
     addr:=Fdosroot;
     isroot:=True;
     dir:=0;
    end;
  end;
  ptr:=0;
  //Write the volume title
  if isroot then
  begin
   //Get the volume name...but for which partition?
   if(dir>0)and(Length(disc_name)>1)then
    side:=1  //ADFS/DOS Hybrid
   else
    side:=0; //DOS only partition
   temp:=disc_name[side];
   //Read the directory into a temporary buffer
   SetLength(buffer,dosroot_size);
   ReadDiscData(addr,dosroot_size,side,0,buffer);
   //If the volume name is not blank
   if temp<>'' then
   begin
    //Move the directory entry pointer to the second entry
    ptr:=1;
    //And write the volume name
    WriteString(temp,0,11,32,buffer);
    //And the attribute for it
    WriteByte($08,$B,buffer);
    //Update the date/time stamps
    Write16b(DOSTime(Now),$16,buffer);
    Write16b(DOSDate(Now),$18,buffer);
    //And clear the final entries
    Write16b(0,$1A,buffer);
    Write32b(0,$1C,buffer);
   end;
  end
  else buffer:=ReadDOSObject(addr);
  //Extend or contract the buffer, if necessary
  if not isroot then
  begin
   //Make a note of the starting length
   dirlenst:=Length(buffer);
   //Work out what it should be
   dirlen:=(Length(FDisc[dir].Entries)+1)*$20; //An extra one for the terminator
   dirlen:=((dirlen+(cluster_size*dosalloc))div(cluster_size*dosalloc))
          *(cluster_size*dosalloc);
   //Then adjust the length, if required
   if Length(buffer)<>dirlen then SetLength(buffer,dirlen);
   //Skip the first two entries, unless it is root
   ptr:=2;
  end;
  //We'll only continue if there are any entries
  if Length(FDisc[dir].Entries)>0 then
  begin
   //Go through each entry in the array
   for index:=0 to Length(FDisc[dir].Entries)-1 do
   begin
    //Filename
    temp:=FDisc[dir].Entries[index].Filename;
    //Remove the extension
    if Pos('.',temp)>1 then temp:=LeftStr(temp,Pos('.',temp)-1);
    //Change the first character, if 0xE5
    if Ord(temp[1])=$E5 then temp[1]:=Chr($05);
    //Now write it
    WriteString(temp,ptr*$20,8,32,buffer);
    //Extension
    WriteString(FDisc[dir].Entries[index].ShortFileType,$08+ptr*$20,3,32,buffer);
    //Attributes
    WriteByte(ConvertDOSAttributes(FDisc[dir].Entries[index].Attributes),
                                   $0B+ptr*$20,buffer);
    //Creation/update time and date
    Write16b(DOSTime(FDisc[dir].Entries[index].Timestamp),$16+ptr*$20,buffer);
    Write16b(DOSDate(FDisc[dir].Entries[index].Timestamp),$18+ptr*$20,buffer);
    //Starting cluster
    Write16b(FDisc[dir].Entries[index].Sector,$1A+ptr*$20,buffer);
    //File length
    if FDisc[dir].Entries[index].DirRef<>-1 then
     Write32b(0,$1C+ptr*$20,buffer) //Directory - length is zero
    else
     Write32b(FDisc[dir].Entries[index].Length,$1C+ptr*$20,buffer);
    //Next entry in the directory
    inc(ptr);
   end;
  end;
  //Clear the end of the list (and mark the end of list with a zero)
  for index:=ptr*$20 to Length(buffer)-1 do
   WriteByte($00,index,buffer);
  //Write the buffer back, extending where required
  if not isroot then
  begin
   //Get the current cluster chain
   fragments:=GetClusterChain(addr);
   if dirlenst<>Length(buffer) then //Size has changed
   begin
    //Extending? Add some clusters
    if dirlenst<Length(buffer) then
     AllocateDOSClusters(Length(buffer),fragments);
    //Contracting? Remove some clusters
    if dirlenst>Length(buffer) then
     DeAllocateDOSClusters(Length(buffer),fragments);
   end;
   //Write the data
   WriteDOSObject(buffer,fragments);
  end;
  //Write the root - this does not get extended
  if isroot then WriteDiscData(addr,side,buffer,Length(buffer));
  //Refresh the free space map
  ReadDOSFSM;
 end;
end;

{-------------------------------------------------------------------------------
Allocate fragments and append to the given cluster chain
-------------------------------------------------------------------------------}
procedure TDiscImage.AllocateDOSClusters(len: Cardinal;
                                                 var fragments: TFragmentArray);
var
 lenctr,
 index    : Integer;
 found    : Boolean;
 original : TFragmentArray;
begin
 //Set up the length counter
 lenctr:=len;
 //Ready to copy the original fragment array
 SetLength(original,Length(fragments));
 //Remove the current clusters, if any, from the length
 if Length(fragments)>0 then
  for index:=0 to Length(fragments)-1 do
  begin
   //We'll remove it in cluster sizes, as the increase could fit in the final cluster
   dec(lenctr,cluster_size*dosalloc);
   //Copy the fragment across
   original[index]:=fragments[index];
  end;
 //If still have some data left to allocate, then allocate some clusters
 while lenctr>0 do
 begin
  //Go through each cluster
  index:=1;
  found:=False;
  while(index<=DOSBlocks div dosalloc)and(not found)do
  begin
   //Next cluster
   inc(index);
   //Is the cluster free?
   if GetClusterEntry(index)=$00 then
   begin
    //Add a new fragment to the array
    SetLength(fragments,Length(fragments)+1);
    fragments[Length(fragments)-1].Offset:=index;
    fragments[Length(fragments)-1].Length:=cluster_size*dosalloc;
    //Reduce the length counter
    dec(lenctr,fragments[Length(fragments)-1].Length);
    //And mark our found flag to true
    found:=True;
   end;
  end;
  //If nothing was found then we need to stop the search and revert the array
  if not found then
  begin
   //Set the length counter to zero to stop the search
   lenctr:=0;
   //Reduce the given array to the orignal length
   SetLength(fragments,Length(original));
  end;
 end;
 //Update the FSM, if need be
 if Length(fragments)>Length(original) then SetClusterChain(fragments);
end;

{-------------------------------------------------------------------------------
DeAllocate fragments and remove from the given cluster chain
-------------------------------------------------------------------------------}
procedure TDiscImage.DeAllocateDOSClusters(len: Cardinal;
                                                 var fragments: TFragmentArray);
var
 lenctr,
 index  : Integer;
begin
 //Get a length counter of the existing fragments
 lenctr:=0;
 if Length(fragments)>0 then
  for index:=0 to Length(fragments)-1 do
   inc(lenctr,cluster_size*dosalloc);//In cluster sizes
 //Continue if the specified length is shorter
 if len<lenctr then
 begin
  //Blank off all the cluster entries for the entire original chain
  for index:=0 to Length(fragments)-1 do
   SetClusterEntry(fragments[index].Offset,$00);
  //Remove the redundant fragments off the end of the array
  while(lenctr>len)and(Length(fragments)>0)do
  begin
   //Reduce the length counter
   dec(lenctr,cluster_size*dosalloc);
   //Remove the last entry in the array
   if Length(fragments)>0 then
    SetLength(fragments,Length(fragments)-1);
  end;
  //Reset the chain
  SetClusterChain(fragments);
 end;
end;

{-------------------------------------------------------------------------------
Write a DOS object
-------------------------------------------------------------------------------}
function TDiscImage.WriteDOSObject(buffer: TDIByteArray;
                                             fragments:TFragmentArray):Boolean;
var
 lenctr,
 frag,
 index  : Integer;
begin
 Result:=False;
 //Any fragments?
 if Length(fragments)>0 then
 begin
  lenctr:=0;
  //Go through each cluster
  for frag:=0 to Length(fragments)-1 do
  begin
   //Copy the data for this cluster across
   if fragments[frag].Length>0 then
    for index:=0 to fragments[frag].Length-1 do
     if lenctr+index<Length(buffer) then
      WriteByte(buffer[lenctr+index],
                DOSClusterToOffset(fragments[frag].Offset)+index);
   //Add the length to our pointer
   inc(lenctr,fragments[frag].Length);
  end;
  //Set a positive result
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Write a DOS file
-------------------------------------------------------------------------------}
function TDiscImage.WriteDOSFile(var file_details: TDirEntry;
                                            var buffer: TDIByteArray):Integer;
var
 dir,
 entry,
 pdir,
 partition : Cardinal;
 index     : Integer;
 fragments : TFragmentArray;
 space     : Boolean;
begin
 dir:=0;
 entry:=0;
 //ADFS hybrid?
 if FFormat>>4=diAcornADFS then partition:=1 else partition:=0;
 SetLength(fragments,0);
 //Start with a negative result
 Result:=-3;//File already exists
 //Validate the proposed filename (ADFS rules the same as AFS)
 if file_details.Filename<>dosrootname then
  file_details.Filename:=ValidateDOSFilename(file_details.Filename);
 //First make sure it doesn't exist already
 if not FileExists(file_details.Parent+GetDirSep(partition)+file_details.Filename,pdir,entry)then
  //Get the directory where we are adding it to, and make sure it exists
  if FileExists(file_details.Parent,pdir,entry) then
  begin
   Result:=-2; //Image full
   file_details.Sector:=0;
   //Where we are inserting this into
   if file_details.Parent=dosrootname then
   begin
    dir:=0;
    //Get the directory reference of the root
    if Length(FDisc)>0 then
     for index:=0 to Length(FDisc)-1 do
      if FDisc[index].Directory=dosrootname then dir:=index;
    partition:=FDisc[dir].Partition;
   end;
   //Somewhere else
   if file_details.Parent<>dosrootname then
   begin
    dir:=FDisc[pdir].Entries[entry].DirRef;
    partition:=FDisc[pdir].Partition;
   end;
   //Set the length
   file_details.Length:=Length(buffer);
   //Will if fit on the disc?
   if free_space[partition]>file_details.Length then
   begin
    Result:=-7; //Map (directory) full
    //Is there enough space in the directory?
    space:=True;
    if file_details.Parent=dosrootname then
     space:=Length(FDisc[dir].Entries)<dosroot_size div$20;
    if space then
    begin
     Result:=-5;//Unknown error
     //Allocate some space
     AllocateDOSClusters(file_details.Length,fragments);
     //Did it allocate anything?
     if Length(fragments)>0 then
     begin
      file_details.Sector:=fragments[0].Offset;
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
       FDisc[Length(FDisc)-1].Parent   :=dir;
       FDisc[Length(FDisc)-1].Sector   :=file_details.Sector;
       SetLength(FDisc[Length(FDisc)-1].Entries,0);
       //Update the 'self' location, if not the root
       if file_details.Filename<>dosrootname then
        Write16b(file_details.Sector,$1A,buffer);
      end
      else //Mark as not a directory
       file_details.DirRef:=-1;
      //Set the date
      file_details.TimeStamp:=Now;
      //Write the data to the image
      if WriteDOSObject(buffer,fragments) then
      begin
       //Insert it into the directory
       Result:=Length(FDisc[dir].Entries);
       SetLength(FDisc[dir].Entries,Result+1);
       FDisc[dir].Entries[Result]:=file_details;
       //Update the parent
       UpdateDOSDirectory(file_details.Parent);
       //And refresh the free space map
       ReadDOSFSM;
      end;
     end;
    end;
   end;
  end;
end;

{-------------------------------------------------------------------------------
Create a DOS directory
-------------------------------------------------------------------------------}
function TDiscImage.CreateDOSDirectory(dirname,parent,attributes: String): Integer;
var
 buffer : TDIByteArray;
 ptr,
 dir,
 entry  : Cardinal;
 ok     : Boolean;
 NewFile: TDirEntry;
 side   : Byte;
begin
 //ADFS hybrid?
 if FFormat>>4=diAcornADFS then side:=1 else side:=0;
 Result:=-3; //Directory already exists
 ok:=False;
 //Make sure that the directory does not already exists
 if dirname=dosrootname then ok:=True
 else ok:=not FileExists(parent+GetDirSep(side)+dirname,dir,entry);
 if ok then
 begin
  Result:=-5; //Unknown error
  //Not root
  if dirname<>dosrootname then
  begin
   //Set up the area for the directory structure
   SetLength(buffer,cluster_size*dosalloc);
   //Get the pointers for the parent
   FileExists(parent,dir,entry);
   //Self
   WriteString('.',$00,11,32,buffer); //Name
   WriteByte($10,$0B,buffer);         //Attribute
   Write16b(DOSTime(Now),$16,buffer); //Time
   Write16b(DOSDate(Now),$18,buffer); //Date
   Write16b($00,$1A,buffer);          //Starting cluster (to be filled later)
   Write16b($00,$1C,buffer);          //Length
   //Parent
   WriteString('..',$20,11,32,buffer);//Name
   WriteByte($10,$2B,buffer);         //Attribute
   Write16b(DOSTime(Now),$36,buffer); //Time
   Write16b(DOSDate(Now),$38,buffer); //Date
   if parent=dosrootname then
    Write16b(00,$1A,buffer)           //Starting cluster of root
   else
    Write16b(FDisc[dir].Entries[entry].Sector,$1A,buffer);//Starting cluster
   Write16b($00,$1C,buffer);          //Length
   //Not writing the root, so just treat as a file
   ResetDirEntry(NewFile);
   NewFile.Parent:=parent;
   NewFile.Filename:=dirname;
   if Pos('D',attributes)=0 then attributes:='D'+attributes;
   NewFile.Attributes:=attributes;
   Result:=WriteDOSFile(NewFile,buffer);
  end
  else //Root
  begin
   for ptr:=0 to dosroot_size-1 do
    WriteByte(0,Fdosroot+ptr);
   //Volume name
   WriteString(disctitle,$00+Fdosroot,11,32);
   WriteByte($08,$0B+Fdosroot);//Attribute for volume title
   Result:=0;
  end;
  //Refresh the free space map
  ReadDOSFSM;
 end;
end;

{-------------------------------------------------------------------------------
Delete a DOS file
-------------------------------------------------------------------------------}
function TDiscImage.DeleteDOSFile(filename: String): Boolean;
var
 dir,
 entry    : Cardinal;
 index    : Integer;
 success  : Boolean;
 clusters : TFragmentArray;
 side     : Byte;
begin
 //ADFS hybrid?
 if FFormat>>4=diAcornADFS then side:=1 else side:=0;
 Result:=False;
 //Make sure the file exists, and is not the root
 if filename<>dosrootname then
  if FileExists(filename,dir,entry) then
  begin
   success:=True;
   //Is this a directory being deleted?
   if FDisc[dir].Entries[entry].DirRef>=0 then
   begin
    index:=FDisc[dir].Entries[entry].DirRef;
    //Recursively delete the contents
    while(Length(FDisc[index].Entries)>0)and(success)do
     success:=DeleteDOSFile(filename+GetDirSep(side)+FDisc[index].Entries[0].Filename);
   end;
   //Remove the entry from the directory
   if success then
   begin
    //Update the FSM
    clusters:=GetClusterChain(FDisc[dir].Entries[entry].Sector);
    DeAllocateDOSClusters(0,clusters);
    //Remove the entry from the local copy
    if entry<Length(FDisc[dir].Entries)-2 then
     for index:=entry to Length(FDisc[dir].Entries)-2 do
      FDisc[dir].Entries[index]:=FDisc[dir].Entries[index+1];
    SetLength(FDisc[dir].Entries,Length(FDisc[dir].Entries)-1);
    //Update the directory
    UpdateDOSDirectory(GetParent(dir));
   end;
   //Return a result
   Result:=success;
  end;
end;

{-------------------------------------------------------------------------------
Update a DOS object's attributes
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDOSAttributes(filename,attributes: String): Boolean;
var
 dir,
 entry : Cardinal;
begin
 Result:=False;
 //Make sure that the file exists
 if FileExists(filename,dir,entry) then
 begin
  //Update the local copy
  FDisc[dir].Entries[entry].Attributes:=attributes;
  //And update the image directory
  UpdateDOSDirectory(GetParent(dir));
  //And return a positive result
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Update a DOS image's title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDOSDiscTitle(title: String): Boolean;
var
 side: Byte;
begin
 //Need to workout which partition
 if Length(disc_name)>1 then
  side:=1  //ADFS/DOS Hybrid
 else
  side:=0; //DOS only partition
 //Update the local copy, truncating to the maximum length
 disc_name[side]:=LeftStr(title,11);
 //Update the root
 UpdateDOSDirectory(dosrootname);
 Result:=True;
end;

{-------------------------------------------------------------------------------
Update a DOS object's timestamp
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDOSTimeStamp(filename:String;newtimedate:TDateTime):Boolean;
var
 dir,
 entry : Cardinal;
begin
 Result:=False;
 //Does the file exists
 if FileExists(filename,dir,entry) then
 begin
  //Set the local copy
  FDisc[dir].Entries[entry].Timestamp:=newtimedate;
  //Update the root
  UpdateDOSDirectory(GetParent(dir));
  //Return a positive result
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Add a DOS partition to an ADFS image
-------------------------------------------------------------------------------}
function TDiscImage.AddDOSPartition(size: Cardinal): Boolean;
var
 fsst,
 fsed       : Cardinal;
 fsptr      : Byte;
 index      : Integer;
 oldfilename: String;
begin
 Result:=False;
 exit; //Not fully tested yet
 if size<9*secsize then exit; //Minimum size is 9 sectors
 //Only for adding DOS partition to 8 bit ADFS
 if(FFormat>>4=diAcornADFS)and(not FMap)and(FDirType=diADFSOldDir)then
 begin
  fsed:=GetADFSMaxLength(False);
  //Is there enough free space?
  if fsed>=size div secsize then
  begin
   //Work out the start
   fsst:=GetDataLength-size;
   //Adjust the ADFS free space map
   fsptr:=GetADFSMaxLength(True);
   Write24b(Read24b($100+fsptr)-(size div secsize),$100+fsptr); //Just adjust the length
   //Adjust the ADFS disc size
   if GetDataLength=640*1024 then Write24b($000AA0,$FC);
   //Update our disc sizes
   disc_size[0]:=fsst;
   SetLength(disc_size,2);
   disc_size[1]:=size;
   SetLength(free_space,2);
   if FFormat AND $F<>$F then //Not for hard disc partitions
   begin
    doshead   :=disc_size[0];
    //Set the DOS root parameters
    dosmap:=doshead;
    dosmap2:=doshead;
    dosroot_size:=$70*$20;
    //And cluster size
    cluster_size:=$100;
    dosalloc:=8;
    //FAT Size and type
    DOSFATSize:=1; //This is one less than the actual size
    NumFATs:=1;    //Number of FATs present
    FATType:=diFAT12;//FAT type
    DOSBlocks:=disc_size[1]div cluster_size; //Number of blocks on disc
    Fdosroot:=doshead+((NumFATs*DOSFATSize)+1)*cluster_size; //Where the root is
   end;
   //Clear the partition of any left over data
   for index:=doshead to GetDataLength-1 do WriteByte(0,index);
   //Create the partition
   WriteDOSPartition;
   //Update the checksums
   WriteByte(ByteCheckSum($0000,$100),$0FF);//Checksum sector 0
   WriteByte(ByteCheckSum($0100,$100),$1FF);//Checksum sector 1
   //Now we re-ID and re-read the data
   oldfilename:=imagefilename;
   IDImage;
   ReadImage;
   imagefilename:=oldfilename;
   //Set a positive result
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Create a new DOS Plus or DOS image
-------------------------------------------------------------------------------}
function TDiscImage.FormatDOS(shape: Byte): TDisc;
begin
 //Default return value
 Result:=nil;
 //Reset everything
 ResetVariables;
 SetDataLength(0);
 //Set the format
 FFormat:=diDOSPlus<<4;
 //Set the data size
 case shape of
  0: SetDataLength( 800*1024);// 800KB DOS Plus
  1: SetDataLength( 360*1024);// 360KB DOS
  2: SetDataLength( 720*1024);// 720KB DOS
  3: SetDataLength(1440*1024);//1.44MB DOS
  4: SetDataLength(2880*1024);//2.88MB DOS
 end;
 disc_size[0]:=GetDataLength;
end;

{-------------------------------------------------------------------------------
Write a DOS Partition
-------------------------------------------------------------------------------}
procedure TDiscImage.WriteDOSPartition;
begin
 //
end;

{-------------------------------------------------------------------------------
Write a DOS Header
-------------------------------------------------------------------------------}
procedure TDiscImage.WriteDOSHeader;
begin
 //
end;

{-------------------------------------------------------------------------------
Move a file/directory
-------------------------------------------------------------------------------}
function TDiscImage.MoveDOSFile(filename,directory: String): Integer;
begin
 Result:=-12;
end;
