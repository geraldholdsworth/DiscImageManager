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
   //FC : SD 9 sectors per track
   //FD : DD 9 sectors per track (used by 800K)
   //FE : SD 8 sectors per track
   //FF : DD 8 sectors per track (used by 640K ADFS hybrid)
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
 i,d,e,
 lenctr  : Integer;
 startdir: String;
begin
 //Is this an ADFS disc with DOS Plus partition?
 if((FFormat>>4=diAcornADFS)and(FDOSPresent))
 or(FFormat>>4=diDOSPlus)then //Or a straight DOS Plus?
 begin
  i:=0;
  dir_sep:='\';
  //ADFS Hybrid?
  if FFormat>>4=diAcornADFS then
  begin 
   //Set up the second partition
   SetLength(disc_size,2);
   SetLength(free_space,2);
   SetLength(disc_name,2);
   if FFormat AND $F<>$F then //Not for hard disc partitions
   begin
    doshead   :=disc_size[0];
    //Set the DOS root parameters
    dosmap:=doshead;
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
  if FFormat=diAcornADFS<<4+$F then startdir:='C:' else startdir:='A:';
  //Read the root
  FDisc[d]:=ReadDOSDirectory(startdir,Fdosroot);
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
                                               +dir_sep
                                               +FDisc[d].Entries[e].Filename,
                                               DOSClusterToOffset(FDisc[d].Entries[e].Sector));
       FDisc[Length(FDisc)-1].Parent:=d;
       //Set the length of the directory
       lenctr:=Length(FDisc[Length(FDisc)-1].Entries)*$20;
       lenctr:=((lenctr+(cluster_size*dosalloc))div(cluster_size*dosalloc))
              *(cluster_size*dosalloc);
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
 //Where the FAT is
 dosmap:=doshead+cluster_size;
 //Where the root is
 Fdosroot:=doshead+((NumFATs*DOSFATSize)+1)*cluster_size;
end;

{-------------------------------------------------------------------------------
Reads a DOS Plus Directory
-------------------------------------------------------------------------------}
function TDiscImage.ReadDOSDirectory(dirname: String;addr: Cardinal): TDir;
var
 index,
 entry,
 side  : Integer;
 attr,
 status: Byte;
 ext   : String;
begin
 Result.Directory:='';
 //Setup the container
 ResetDir(Result);
 //Don't need the parent or self referrals
 if(RightStr(dirname,1)='.')or(RightStr(dirname,2)='..')then exit;
 //Directory name
 index:=Pos(dir_sep,dirname);
 while Pos(dir_sep,dirname,index+1)>index do
  index:=Pos(dir_sep,dirname,index+1);
 if index>0 then
  Result.Directory:=Copy(dirname,index+1)
 else
  Result.Directory:=dirname;
 //And set the partition flag to true
 Result.DOSPartition:=True;
 //ADFS hybrid?
 if FFormat>>4=diAcornADFS then side:=1 else side:=0;
 Result.Partition:=side;
 //Update the progress indicator
 UpdateProgress('Reading '+dirname);
 //Start at the beginning
 index:=0;
 entry:=0;
 //Get the status byte (first byte of the filename)
 status:=ReadByte(addr+index*32);
 while status<>0 do
 begin
  //Read the attribute byte
  attr:=ReadByte(addr+$B+index*32);
  //Continue if valid filename, and not volume name
  if(status<>$05)and(status<>$E5)and(status<>$00)and(attr AND$8=0)then
  begin
   //Add a new entry
   SetLength(Result.Entries,Length(Result.Entries)+1);
   entry:=Length(Result.Entries)-1;
   //Reset it to defaults
   ResetDirEntry(Result.Entries[entry]);
   //Read in the details
   Result.Entries[entry].Filename:=ReadString(addr+$00+index*32,-8); //Filename
   RemoveSpaces(Result.Entries[entry].Filename);
   ext:=ReadString(addr+$08+index*32,-3); //Extension
   RemoveSpaces(ext);
   if ext<>'' then
    Result.Entries[entry].Filename:=Result.Entries[entry].Filename+'.'+ext;
   Result.Entries[entry].Timestamp:=ConvertDOSTimeDate(Read16b(addr+$16),
                                                       Read16b(addr+$18));//Time/date
   Result.Entries[entry].Attributes:=ConvertDOSAttributes(attr);//Attributes
   Result.Entries[entry].Sector   :=Read16b(addr+$1A+index*32); //Starting cluster
   Result.Entries[entry].Length   :=Read32b(addr+$1C+index*32); //Filelength
   Result.Entries[entry].DirRef   :=-1; //Directory Reference
   Result.Entries[entry].Parent   :=dirname; //Parent
   Result.Entries[entry].ShortFileType:=ext;
   //If the filename is '.' or '..' then remove it
   if(Result.Entries[entry].Filename='.')
   or(Result.Entries[entry].Filename='..')then
    SetLength(Result.Entries,Length(Result.Entries)-1);
  end;
  //Move onto the next entry
  inc(index);
  status:=ReadByte(addr+index*32);
 end;
end;

{-------------------------------------------------------------------------------
Convert time and date words to a time date value
-------------------------------------------------------------------------------}
function TDiscImage.ConvertDOSTimeDate(time,date: Word): TDateTime;
var
 year    : Cardinal;
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
Determines if the given cluster number is valid
-------------------------------------------------------------------------------}
function TDiscImage.IsClusterValid(cluster: Cardinal): Boolean;
begin
 Result:=cluster>1;
 if(FATType=diFAT12)and(cluster<     $FF0)then Result:=Result AND True;
 if(FATType=diFAT16)and(cluster<    $FFF0)then Result:=Result AND True;
 if(FATType=diFAT32)and(cluster<$FFFFFFF0)then Result:=Result AND True;
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path
-------------------------------------------------------------------------------}
function TDiscImage.ExtractDOSFile(filename: String;
                                             var buffer: TDIByteArray): Boolean;
var
 dir,
 entry,
 frag,
 index,
 lenctr,
 ptr       : Cardinal;
 cluster   : Word;
 fragments : TFragmentArray;
begin
 Result:=False;
 SetLength(fragments,0);
 if FileExists(filename,dir,entry) then //Does the file actually exist?
 begin
  //Reset the buffer
  SetLength(buffer,0);
  //Go through the FAT, start at the starting cluster, and build the fragment array
  cluster:=FDisc[dir].Entries[entry].Sector; //Starting cluster
  //Add the first sector, if valid
  if IsClusterValid(cluster)then
  begin
   SetLength(fragments,1);
   fragments[0].Offset:=FDisc[dir].Entries[entry].Sector;
   if cluster_size*dosalloc<FDisc[dir].Entries[entry].Length then
    fragments[0].Length:=cluster_size*dosalloc
   else
    fragments[0].Length:=FDisc[dir].Entries[entry].Length;
  end;
  lenctr:=FDisc[dir].Entries[entry].Length-fragments[0].Length;
  while(IsClusterValid(cluster))and(lenctr>0)do //Continue while valid
  begin
   //Get the next entry
   cluster:=GetClusterEntry(cluster);
   //If valid, add a fragment
   if IsClusterValid(cluster) then
   begin
    SetLength(fragments,Length(fragments)+1);
    fragments[Length(fragments)-1].Offset:=cluster;
    if cluster_size*dosalloc<lenctr then
     fragments[Length(fragments)-1].Length:=cluster_size*dosalloc
    else
     fragments[Length(fragments)-1].Length:=lenctr;
    dec(lenctr,fragments[Length(fragments)-1].Length);
   end;
  end;
  //Any fragments?
  if Length(fragments)>0 then
  begin
   //Go through each fragment
   for frag:=0 to Length(fragments)-1 do
   begin
    //Make a note of the current position in the buffer
    ptr:=Length(buffer);
    //Then extend it
    SetLength(buffer,Length(buffer)+fragments[frag].Length);
    //Copy the data for this cluster across
    if fragments[frag].Length>0 then //Ensure that there is data there
     for index:=0 to fragments[frag].Length-1 do
      buffer[ptr+index]:=ReadByte(DOSClusterToOffset(fragments[frag].Offset)+index);
   end;
  end;
  //And return a positive result
  Result:=True;
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
    if t<tracks then //Make sure it is within range
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
begin
 Result:=-2;
end;

{-------------------------------------------------------------------------------
Validate a DOS filename
-------------------------------------------------------------------------------}
function TDiscImage.ValidateDOSFilename(filename: String): String;
begin
 Result:=filename;
end;
