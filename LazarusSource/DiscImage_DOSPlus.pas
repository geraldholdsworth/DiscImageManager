//++++++++++++++++++ Acorn DOS Plus ++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a DOS Plus disc and which type
-------------------------------------------------------------------------------}
function TDiscImage.ID_DOSPlus: Boolean;
var
 idbyte: Byte;
begin
 Result:=False;
 if FFormat=diInvalidImg then
 begin
  ResetVariables;
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
    //ReadDOSHeader;
    disc_size[0]:=DOSBlocks*cluster_size;
    //Set the format
    FFormat:=diDOSPlus<<4;
    //And update it with the appropriate FAT
    if FATType=diFAT12 then inc(FFormat,1);
    if FATType=diFAT16 then inc(FFormat,2);
    if FATType=diFAT32 then inc(FFormat,3);
   end;
  Result:=GetMajorFormatNumber=diDOSPlus;
 end;
end;

{-------------------------------------------------------------------------------
ID a DOS Partition, with header
-------------------------------------------------------------------------------}
function TDiscImage.IDDOSPartition(ctr: Cardinal): Boolean;
var
 ds,
 rs : Word;
 md : Byte;
begin
 Result:=False;
 //Is there E9 or EB stored here
 if(ReadByte(ctr)=$E9)or(ReadByte(ctr)=$EB)then
 begin
  //Read in the block size
  ds:=Read16b(ctr+$B);
  //And the reserved sectors size
  rs:=Read16b(ctr+$E);
  //Media Descriptor Byte - should be repeated at the start of the FAT
  md:=ReadByte(ctr+$15);
  //Are they within range?
  if ds*rs<GetDataLength then
   //Is there a FAT partition at the location pointed to by above?
   if(Read16b(ctr+(ds*rs)+1)=$FFFF)and(ReadByte(ctr+(ds*rs))=md)then
   begin
    //DOS Header location
    doshead:=ctr;
    Result:=ReadDOSHeader;
    //Mark as DOS Partition present, if the checks return OK
    FDOSPresent:=Result;
    if not Result then doshead:=0; //Reset the header location on failure
   end;
 end;
end;

{-------------------------------------------------------------------------------
Reads a DOS Plus partition
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadDOSPartition;
var
 i,d,e    : Integer;
 lenctr   : Cardinal;
 part     : Byte;
begin
 //Is this an ADFS disc with DOS Plus partition?
 if((GetMajorFormatNumber=diAcornADFS)and(FDOSPresent))
 or(GetMajorFormatNumber=diDOSPlus)then //Or a straight DOS Plus?
 begin
  i:=0;
  part:=0;
  if GetMajorFormatNumber=diDOSPlus then dir_sep:='\';
  //ADFS Hybrid?
  if GetMajorFormatNumber=diAcornADFS then
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
    //doshead:=disc_size[0]; //Where the DOS header is
    //ReadDOSHeader; //Read the DOS Header
    disc_size[1]:=DOSBlocks*cluster_size;//Disc size in bytes
   end;
   i:=1;
  end;
  //Update the progress indicator
  UpdateProgress('Reading DOS Plus partition');
  //Read the volume title
  disc_name[i]:='';
  //Volume name in the header for DOS Version 29
  if DOSVersion=$29 then
   if FATType=diFAT32 then disc_name[i]:=ReadString(doshead+$47,-11)
   else disc_name[i]:=ReadString(doshead+$2B,-11);
  //Remove the spaces
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
       if FScanSubDirs then
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
function TDiscImage.ReadDOSHeader: Boolean;
var
 RootFrags      : TFragmentArray;
 index          : Integer;
 RootDirSectors,
 DataSec,
 ClusterCount   : Cardinal;
begin
 Result:=False;                                                     
 cluster_size:=Read16b(doshead+$B);      //Block (cluster) size
 if (cluster_size<>$0200)
 and(cluster_size<>$0400)
 and(cluster_size<>$0800)
 and(cluster_size<>$1000)then exit; //Must be one of these values
 dosalloc    :=ReadByte(doshead+$D);     //Allocation unit in blocks
 if (dosalloc<>  1)
 and(dosalloc<>  2)
 and(dosalloc<>  4)
 and(dosalloc<>  8)
 and(dosalloc<> 16)
 and(dosalloc<> 32)
 and(dosalloc<> 64)
 and(dosalloc<>128)then exit; //Must be one of these values
 if dosalloc*cluster_size>32*1024 then exit; //Cannot be greater than 32K
 DOSResSecs  :=Read16b(doshead+$E);     //Reserved sectors
 if DOSResSecs=0 then exit; //Cannot be zero
 NumFATs     :=ReadByte(doshead+$10);    //Number of FATs
 if NumFATs=0 then exit; //Can't have zero FATs
 dosroot_size:=Read16b(doshead+$11)*$20; //Size of the root
 DOSBlocks   :=Read16b(doshead+$13);     //Total number of blocks
 if DOSBlocks=0 then DOSBlocks:=Read32b(doshead+$20);//if >65535 blocks
 if DOSBlocks=0 then exit; //Still zero? then fail
 DOSFATSize  :=Read16b(doshead+$16);     //FAT size in blocks
 if DOSFATSize=0 then DOSFATSize:=Read32b(doshead+$24);//FAT size in blocks
 if DOSFATSize=0 then exit; //Still zero? then fail
 //Determine the FAT type, as per Microsoft spec
 RootDirSectors:=(dosroot_size+(cluster_size-1))div cluster_size;
 DataSec:=DOSBlocks-(DOSResSecs+(NumFATs*DOSFATSize)+RootDirSectors);
 ClusterCount:=Floor(DataSec/dosalloc);
 if ClusterCount<4085 then FATType:=diFAT12
 else if ClusterCount<65525 then FATType:=diFAT16
 else FATType:=diFAT32;
 //Sanity checks now we know the FAT
 if(dosroot_size=0)and(FATType<>diFAT32)then exit; //Can't be zero, unless FAT32
 if(dosroot_size<>0)and(FATType=diFAT32)then exit; //But, must be zero if FAT32
 if FATType<>diFAT32 then
  if dosroot_size mod cluster_size<>0 then exit;//Must be a multiple
 if FATType=diFAT32 then
 begin
  if(Read16b(doshead+$13)<>0)          //This must be zero for FAT32
  or(Read32b(doshead+$20)=0)then exit; //This must be non-zero for FAT32
  if(Read16b(doshead+$16)<>0)          //This must be zero for FAT32
  or(Read32b(doshead+$24)=0)then exit; //This must be non-zero for FAT32
 end;
 //Read in the specific parts, dependant on FATType
 if(FATType=diFAT12)or(FATType=diFAT16)then
  DOSVersion  :=ReadByte(doshead+$26);    //DOS Version - $28 or $29 (or 0)
 if FATType=diFAT32 then
 begin
  doshead2    :=Read16b(doshead+$32)*cluster_size;//Location of the second DOS header
  DOSVersion  :=ReadByte(doshead+$42);    //DOS Version - $28 or $29 (or 0)
 end;
 //For version less than $29, the volume name will be in the root
 if DOSVersion<$29 then FDOSVolInRoot:=True; //But $29 can be both root and header
 //Where the FAT(s) is(are)
 dosmap:=doshead+(DOSResSecs*cluster_size);
 if NumFATs>1 then dosmap2:=dosmap+DOSFATSize*cluster_size else dosmap2:=dosmap;
 //Where the root is
 if(FATType=diFAT12)or(FATType=diFAT16)then
  Fdosroot:=doshead+((NumFATs*DOSFATSize)+DOSResSecs)*cluster_size;
 if FATType=diFAT32 then
 begin
  //Root is pointed to by location $2C (cluster number)
  Fdosroot:=Read32b(doshead+$2C);
  //Root size should then be worked out from the number of clusters used.
  RootFrags:=GetClusterChain(Fdosroot);
  dosroot_size:=00;
  if Length(RootFrags)>0 then
   for index:=0 to Length(RootFrags)-1 do
    inc(dosroot_size,RootFrags[index].Length);
 end;
 //Got this far? Then it must be a success
 Result:=True;
end;

{-------------------------------------------------------------------------------
Reads a DOS Plus Directory
-------------------------------------------------------------------------------}
function TDiscImage.ReadDOSDirectory(dirname: String;addr: Cardinal;
                                             var len: Cardinal): TDir;
var
 index,
 entry,
 side   : Integer;
 attr,
 status,
 LFNInd,
 LFNEnd : Byte;
 C      : Char;
 ext,
 volname: String;
 buffer : TDIByteArray;
 created: Boolean;
begin
 Result.Directory:='';
 //Setup the container
 ResetDir(Result);
 //Don't need the parent or self referrals
 if(RightStr(dirname,1)='.')or(RightStr(dirname,2)='..')then exit;
 //ADFS hybrid?
 if GetMajorFormatNumber=diAcornADFS then side:=1 else side:=0;
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
 if(dirname=dosrootname)and((FATType=diFAT12)or(FATType=diFAT16))then
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
 created:=False;
 //Get the status byte (first byte of the filename)
 status:=ReadByte(index*32,buffer);
 while status<>0 do
 begin
  //Read the attribute byte
  attr:=ReadByte($B+index*32,buffer);
  //Is it the volume name?
  if(status<>$E5)and(attr AND$8=$8)then
  begin
   volname:=ReadString(Fdosroot,-11,buffer);
   FDOSVolInRoot:=True;//Volume name is in the root flag
   //Has it already been set?
   if disc_name[Length(disc_name)-1]='' then
    disc_name[Length(disc_name)-1]:=volname;//If no, then set it
  end;
  //Long filename will have attr as $F.
  if(status<>$E5)and(status<>$00)and(attr=$F)then
  begin
   //Long entries always precede the short entry, and in reverse order
   if not created then SetLength(Result.Entries,Length(Result.Entries)+1);
   entry:=Length(Result.Entries)-1;
   if not created then ResetDirEntry(Result.Entries[entry]);
   //So we need a flag to indicate whether we have created the entry for it
   created:=True;
   //Extract each part of the filename
   LFNInd:=1;
   LFNEnd:=10;
   volname:='';
   //Part 1 is characters  1, 3, 5, 7, 9
   //Part 2 is characters 14,16,18,20,22,24
   //Part 3 is characters 28,30
   while LFNInd<31 do
   begin
    while LFNInd<LFNEnd do
    begin
     C:=' ';
     if Read16b(LFNInd+index*32,buffer)<>$FFFF then // $FFFF is end of filename
      C:=Chr(Read16b(LFNInd+index*32,buffer)); //Read the character in
     volname:=volname+C; //And add it to this part
     inc(LFNInd,2);
    end;
    //Next part
    if LFNEnd=25 then begin LFNEnd:=31;LFNInd:=28;end;
    if LFNEnd=10 then begin LFNEnd:=25;LFNInd:=14;end;
   end;
   //Add to the entry
   Result.Entries[entry].Filename:=volname+Result.Entries[entry].Filename;
  end;
  //Continue if valid filename, and not volume name
  if(status<>$E5)and(status<>$00)and(attr AND$8=0)then
  begin
   //Add a new entry
   if not created then SetLength(Result.Entries,Length(Result.Entries)+1);
   entry:=Length(Result.Entries)-1;
   //Reset it to defaults
   if not created then ResetDirEntry(Result.Entries[entry]);
   //Read in the details
   volname:=ReadString($00+index*32,-8,buffer); //Filename
   //Replace 0x05 with 0xE5
   if volname<>'' then
    if Ord(volname[1])=$05 then
     volname[1]:=Chr($E5);
   //Remove the trailing spaces
   RemoveSpaces(volname);
   //Read in the extension
   ext:=ReadString($08+index*32,-3,buffer); //Extension
   RemoveSpaces(ext);
   //And assign to the record
   Result.Entries[entry].ShortFilename:=BuildDOSFilename(volname,ext);
   //If we don't already have a long filename, then use the short one
   if Result.Entries[entry].Filename='' then
    Result.Entries[entry].Filename:=Result.Entries[entry].ShortFilename;
   //Read the rest of the attributes
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
   //Reset the flag as we have finished with this entry
   created:=False;
  end;
  //Move onto the next entry
  inc(index);
  status:=ReadByte(index*32,buffer);
 end;
 Result.BeenRead:=True;
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
 if(FATType=diFAT12)or(FATType=diFAT16)then
  Result:=((cluster-2)*cluster_size*dosalloc)+Fdosroot+dosroot_size;
 if FATType=diFAT32 then
  Result:=((cluster-2)*cluster_size*dosalloc)+dosmap2+DOSFATSize*dosalloc*cluster_size;
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
  if numFATs>1 then Write32b(FATentry,FAToffset+dosmap2);
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
 //if filename[Length(dosrootname)+1]='.' then filename[Length(dosrootname)+1]:='\';
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
 if GetMajorFormatNumber=diDOSPlus then
 begin
  SetLength(free_space_map,1);
  part:=0;
 end;
 if GetMajorFormatNumber=diAcornADFS then //Hybrids - DOS will take up the second 'side'
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
 if GetMajorFormatNumber=diAcornADFS then side:=1 else side:=0;
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
function TDiscImage.ValidateDOSFilename(filename: String;long: Boolean=False): String;
var
 fn,
 fn2,
 ext,
 ext2   : String;
 index  : Integer;
const
 illegal = ' "*+,./:;<=>?[\]|';//As per MS FAT32 Specification
 longillegal = ' "*./:<>?\|';  //As per MS FAT32 Specification
begin
 if FFormat=diDOSPlus then long:=False;//Override this for Master 512
 if FDOSUseSFN        then long:=False;//Use Short FileNames is set
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
 if long then //Long filename
 begin
  fn:=LeftStr(fn,255);
  ext:=LeftStr(ext,255);
  if Length(BuildDOSFilename(fn,ext))>255 then fn:=LeftStr(fn,254-Length(ext));
 end
 else
 begin
  fn:=LeftStr(fn,8);
  ext:=LeftStr(ext,3);
 end;
 //Remove any trailing spaces
 RemoveSpaces(fn);
 RemoveSpaces(ext);
 //Now we can validate both
 fn2:='';
 if Length(fn)>0 then
  for index:=1 to Length(fn) do
  begin
   if(Pos(fn[index],illegal)=0)and(not long)then fn2:=fn2+fn[index];
   if(Pos(fn[index],longillegal)=0)and(long)then fn2:=fn2+fn[index];
  end;
 //Convert to uppercase
 if not long then fn2:=UpperCase(fn2);
 //And the extension
 ext2:='';
 if Length(ext)>0 then
  for index:=1 to Length(ext) do
  begin
   if(Pos(ext[index],illegal)=0)and(not long)then ext2:=ext2+ext[index];
   if(Pos(ext[index],longillegal)=0)and(long)then ext2:=ext2+ext[index];
  end;
 if not long then ext2:=UpperCase(ext2);
 //Return a validated filename
 Result:=BuildDOSFilename(fn2,ext2);
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
 index,
 partindex,
 chrindex,
 numentries: Integer;
 isroot    : Boolean;
 temp,
 SFN       : String;
 buffer    : TDIByteArray;
 side,
 chsum     : Byte;
 fragments : TFragmentArray;
 parts     : array of String;
const
 chrpos: array[1..13] of Byte=($01,$03,$05,$07,$09,$0E,$10,$12,$14,$16,$18,$1C,$1E);
 procedure AdjustDirLength(entries: Integer);
 begin
  inc(numentries,entries);
  //Make a note of the starting length
  dirlenst:=Length(buffer);
  //Work out what it should be
  dirlen:=numentries*$20; //An extra one for the terminator
  dirlen:=((dirlen+(cluster_size*dosalloc))div(cluster_size*dosalloc))
         *(cluster_size*dosalloc);
  //Then adjust the length, if required
  if Length(buffer)<>dirlen then SetLength(buffer,dirlen);
 end;
begin
 if(FileExists(dirname,dir,entry))or(dirname=dosrootname)then
 begin
  isroot:=False;
  numentries:=1; //The terminator entry
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
  if(isroot)and(FATType<>diFAT32)then
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
   //If the volume name is not blank, and is stored here
   if(temp<>'')and(FDOSVolInRoot)then
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
  if(not isroot)or(FATType=diFAT32)then
  begin
   AdjustDirLength(Length(FDisc[dir].Entries));
   //Skip the first two entries, unless it is root
   if not isroot then ptr:=2;//First two entries are '.' and '..'
  end;
  //We'll only continue if there are any entries
  if Length(FDisc[dir].Entries)>0 then
  begin
   //Go through each entry in the array
   for index:=0 to Length(FDisc[dir].Entries)-1 do
   begin
    //Short Filename
    temp:=DOSShortFilename(dirname,
                           FDisc[dir].Entries[index].Filename,
                           FDisc[dir].Entries[index].ShortFilename);
    //Remove the extension
    if Pos('.',temp)>1 then temp:=LeftStr(temp,Pos('.',temp)-1);
    //Change the first character, if 0xE5
    if Ord(temp[1])=$E5 then temp[1]:=Chr($05);
    //Compute the checksum
    chsum:=0;
    //Make sure that the string to compute the sum is 11 characters long
    SFN:=PadRight(temp,8)+PadRight(FDisc[dir].Entries[index].ShortFileType,3);
    //This is as per Microsoft spec, in the FAT32 specification White Paper
    for partindex:=1 to 11 do
     if chsum AND 1=1 then
      chsum:=$80+(chsum>>1)+Ord(SFN[partindex])
     else
      chsum:=(chsum>>1)+Ord(SFN[partindex]);
    //Long filename
    if(FDisc[dir].Entries[index].Filename<>FDisc[dir].Entries[index].ShortFilename)
    and(not FDOSUseSFN)then
    begin
     //Set up the array for the various parts of the filename
     SetLength(parts,Ceil(Length(FDisc[dir].Entries[index].Filename)/13));
     //Now split the filename into parts of 13 characters or less
     for partindex:=0 to Length(parts)-1 do
      parts[partindex]:=Copy(FDisc[dir].Entries[index].Filename,
                             (partindex*13)+1,13);
     //Extend the directory size to accomodate the extra entries
     if(not isroot)or(FATType=diFAT32)then AdjustDirLength(Length(parts));
     //Now add an entry for each one
     for partindex:=Length(parts)-1 downto 0 do
     begin
      //Status byte - in this case a counter
      if partindex=Length(parts)-1 then
       WriteByte($41+partindex,$00+ptr*$20,buffer) //bit 6 is the end of chain marker
      else
       WriteByte(partindex+1,$00+ptr*$20,buffer);
      //Attribute - to indicate an LFN
      WriteByte($0F,$0B+ptr*$20,buffer);
      //Write the characters
      for chrindex:=1 to 13 do
      begin
       if Length(parts[partindex])>=chrindex then //Each character
        Write16b(Ord(parts[partindex][chrindex]),chrpos[chrindex]+ptr*$20,buffer);
       if Length(parts[partindex])+1=chrindex then //Zero terminator
        Write16b($0000,chrpos[chrindex]+ptr*$20,buffer);
       if Length(parts[partindex])+1<chrindex then //FFFF as no more characters
        Write16b($FFFF,chrpos[chrindex]+ptr*$20,buffer);
      end;
      //zeros at 1A/1B
      Write16b($0000,$1A+ptr*$20,buffer);
      //Checksum at 0D
      WriteByte(chsum,$0D+ptr*$20,buffer);
      //Next entry
      inc(ptr);
     end;
    end;
    //Write the short filename
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
  if(not isroot)or(FATType=diFAT32)then
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
  if(isroot)and(FATType<>diFAT32)then WriteDiscData(addr,side,buffer,Length(buffer));
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
 if GetMajorFormatNumber=diAcornADFS then partition:=1 else partition:=0;
 SetLength(fragments,0);
 //Start with a negative result
 Result:=-3;//File already exists
 //Fill out the rest of the attributes
 if file_details.ShortFilename='' then //If a long filename, then get a short version
  file_details.ShortFilename:=DOSShortFilename(file_details.Parent,file_details.Filename);
 //If the long is the same as the short, ensure the case matches
 if(UpperCase(file_details.Filename)=file_details.ShortFilename)or(FDOSUseSFN)then
  file_details.Filename:=file_details.ShortFilename;
 //Move the extension to the short filetype
 if file_details.ShortFileType='' then
  if Pos('.',file_details.ShortFilename)>0 then
  begin
   index:=Length(file_details.ShortFilename);
   while file_details.ShortFilename[index]<>'.' do dec(index);
   file_details.ShortFileType:=Copy(file_details.ShortFilename,index+1);
  end;
 //Validate the proposed filename (ADFS rules the same as AFS)
 if(file_details.Filename<>dosrootname)
 and((GetMajorFormatNumber=diAcornADFS)or(FFormat=diDOSPlus))then //ADFS partition or DOS Plus
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
       Result:=InsertDOSEntry(dir,file_details);
       //And refresh the free space map
       ReadDOSFSM;
      end;
     end;
    end;
   end;
  end;
end;

{-------------------------------------------------------------------------------
Insert a file into a directory
-------------------------------------------------------------------------------}
function TDiscImage.InsertDOSEntry(dir: Cardinal;direntry: TDirEntry): Integer;
begin
 Result:=Length(FDisc[dir].Entries);
 SetLength(FDisc[dir].Entries,Result+1);
 FDisc[dir].Entries[Result]:=direntry;
 //Update the parent
 UpdateDOSDirectory(direntry.Parent);
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
 if GetMajorFormatNumber=diAcornADFS then side:=1 else side:=0;
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
 if GetMajorFormatNumber=diAcornADFS then side:=1 else side:=0;
 Result:=False;
 //Make sure the file exists, and is not the root
 if filename<>dosrootname then
  if FileExists(filename,dir,entry) then
  begin
   success:=True;
   index:=-1;
   //Is this a directory being deleted?
   if FDisc[dir].Entries[entry].DirRef>=0 then
   begin
    index:=FDisc[dir].Entries[entry].DirRef;
    FDisc[index].Deleted:=True;
    //Make sure it has been read in
    if not FDisc[index].BeenRead then ReadDirectory(filename);
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
    //Remove the entry
    RemoveDOSEntry(dir,entry);
    //Update all the directory references
    if index>=0 then UpdateDirRef(index);
   end;
   //Return a result
   Result:=success;
  end;
end;

{-------------------------------------------------------------------------------
Remove an entry from a directory
-------------------------------------------------------------------------------}
procedure TDiscImage.RemoveDOSEntry(dir, entry: Cardinal);
var
 index: Integer;
begin
 //Remove the entry from the local copy
 if entry<Length(FDisc[dir].Entries)-2 then
  for index:=entry to Length(FDisc[dir].Entries)-2 do
   FDisc[dir].Entries[index]:=FDisc[dir].Entries[index+1];
 SetLength(FDisc[dir].Entries,Length(FDisc[dir].Entries)-1);
 //Update the directory
 UpdateDOSDirectory(GetParent(dir));
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
 side,
 ptr  : Byte;
begin
 //Need to workout which partition
 if Length(disc_name)>1 then
  side:=1  //ADFS/DOS Hybrid
 else
  side:=0; //DOS only partition
 //Update the local copy, truncating to the maximum length
 disc_name[side]:=LeftStr(title,11);
 //Update the root
 if FDOSVolInRoot then UpdateDOSDirectory(dosrootname);
 //Is there a volume title in the header?
 if DOSVersion=$29 then
 begin
  if FATType<>diFAT32 then ptr:=$2B else ptr:=$47;
  WriteString(disc_name[side],doshead+ptr,11,32);
  //Update the copy (FAT32)
  if FATType=diFAT32 then WriteString(disc_name[side],doshead2+ptr,11,32);
 end;
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
 if size<9*secsize then exit; //Minimum size is 9 sectors
 //Only for adding DOS partition to 8 bit ADFS
 if(GetMajorFormatNumber=diAcornADFS)and(not FMap)and(FDirType=diADFSOldDir)then
 begin
  if disc_size[0]=$A0000 then size:=$9F000; //640K 'L' has 4K of ADFS
  fsed:=GetADFSMaxLength(False);
  //Is there enough free space?
  if fsed>=size then
  begin
   //Work out the start
   fsst:=GetDataLength-size;
   //Adjust the ADFS free space map
   fsptr:=GetADFSMaxLength(True);
   Write24b(Read24b($100+fsptr)-(size div secsize),$100+fsptr); //Just adjust the length
   //Adjust the disc size for 640K
   if disc_size[0]=$A0000 then WriteByte($A0,$FC);
   //Update our disc sizes
   disc_size[0]:=fsst;
   SetLength(disc_size,2);
   disc_size[1]:=size;
   SetLength(free_space,2);
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
   //Clear the partition of any left over data
   for index:=doshead to GetDataLength-1 do WriteByte(0,index);
   //Create the partition
   WriteDOSHeader(doshead,size,diMaster512,False);
   //Sort out the FSM
   ConsolidateADFSFreeSpaceMap;
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
function TDiscImage.FormatDOS(size: QWord;fat: Byte): Boolean;
var
 index: Cardinal;
const
 KB = 1024;
 MB = 1024*1024;
 GB = 1024*1024*1024;
begin
 //Default return value
 Result:=False;
 FDisc:=nil;
 //Ensure that we don't create an illegal shape
 //Min sizes - these are Microsoft specs
 if                     size<  180*KB then size:=  180*KB; //  180KB
 if(fat=diFAT16)    and(size< 2075*KB)then size:= 2075*KB; // 2075KB ( ~2MB)
 if(fat=diFAT32)    and(size<33300*KB)then size:=33300*KB; //33300KB (~33MB)
 //Max sizes - these are maximium, theoretical, sizes
 if(fat=diMaster512)and(size> 1015*MB)then size:= 1015*MB; // 1015MB ( ~1GB)
 if(fat=diFAT12)    and(size>  507*MB)then size:=  507*MB; //  507MB
 if(fat=diFAT16)    and(size> 8157*MB)then size:= 8157*MB; // 8157MB ( ~8GB)
 if(fat=diFAT32)    and(size> 2048*GB)then size:= 2048*GB; //    2TB
 //Max theoretical size of FAT32 is actually 32640GB, but MS spec is 2TB
 //Reset everything
 ResetVariables;
 SetDataLength(0);
 //Reset the format (it'll get set later when we ID and Read the new image)
 FFormat:=diInvalidImg;
 disc_size[0]:=size;
 SetDataLength(size);
 //Empty the area
 UpdateProgress('Formatting...');
 for index:=0 to size-1 do WriteByte(0,index);
 //Master 512 DOS Plus or Standard DOS?
 if(size<>800*KB)and(size<>640*KB)then //Standard DOS
  WriteDOSHeader(0,size,fat,True)
 else
 begin //Master 512
  //BBC Master DOS Plus images do not have a DOS header, which makes them easier.
  if size=640*KB then //Master 512 hybrid image
  begin
   //Create the ADFS image (ADFS 'L')
   FormatADFSFloppy(2);
   Result:=Length(FDisc)>0;
   //Create the 636K DOS partition
   for index:=0 to $1FA do WriteByte(0,index);//Most of the ADFS header needs blanked
   Write24b($000AA0,$FC);  //Signature for 640K DOS Plus
   Write24b($FFFFFF,$1000);//FAT signature
   WriteByte(0,$1FE);//Blank off the free space count
   //Update the checksums
   WriteByte(ByteCheckSum($0000,$100,False),$0FF);//Checksum sector 0
   WriteByte(ByteCheckSum($0100,$100,False),$1FF);//Checksum sector 1
  end;
  if size=800*KB then //Master 512 800K image
  begin
   //Very simplistic this format.
   WriteByte($FD,0); //Media descriptor byte
   WriteByte($FF,1); //Start of FAT
   WriteByte($FF,2);
   WriteByte($8,$80B); //Indicates volume name in the root
  end;
 end;
 //ID the image to set up all the variables
 if IDImage then
 begin
  //And read it in
  ReadImage;
  //Returning a result
  Result:=Length(FDisc)>0;
  //Set the filename
  imagefilename:='Untitled.'+FormatExt;
 end;
end;

{-------------------------------------------------------------------------------
Write a DOS Header
-------------------------------------------------------------------------------}
procedure TDiscImage.WriteDOSHeader(offset, size: QWord;fat: Byte;bootable: Boolean);
var
 buffer: TDIByteArray;
begin
 SetLength(buffer,0);
 WriteDOSHeader(offset,size,fat,bootable,buffer);
end;
procedure TDiscImage.WriteDOSHeader(offset, size: QWord;fat: Byte;
                                    bootable: Boolean;var buffer: TDIByteArray);
var
 maxCluster,
 totBlocks,
 FATloc,
 FATsize,
 Lrootsize,
 bcloc,
 RootDirSectors,
 TmpVal1,
 TmpVal2    : Cardinal;
 sectorSize : Word;
 LnumFATs,
 allocSize,
 reserved,
 mdb        : Byte;
 index      : Integer;
 master512  : Boolean;
const
  //Boot code - this is actual code which'll get run on boot
  bootCode : array[0..40] of Byte = ($FA,$31,$C0,$8E,$D0,$BC,$00,$7C,$FB,$8E,
                                     $D8,$E8,$00,$00,$5E,$83,$C6,$19,$BB,$07,
                                     $00,$FC,$AC,$84,$C0,$74,$06,$B4,$0E,$CD,
                                     $10,$EB,$F5,$30,$E4,$CD,$16,$CD,$19,$0D,
                                     $0A);
begin
 UpdateProgress('Intialising...');
 master512:=False; //BBC Master 512 image?
 sectorSize:=$200; //Sector size. $200 is most DOS
 LnumFATs:=2; //Number of FATs. Most DOS is 2
 mdb:=$F0; //Media descriptor byte
 Lrootsize:=$200; //Root size
 reserved:=1; //Reserved sectors
 if fat=diFAT32 then reserved:=$20;
 maxCluster:=1;
 //Maximum number of clusters
 if fat=diFAT12 then maxCluster:=$FF6;      //FAT12
 if fat=diFAT16 then maxCluster:=$FFF6;     //FAT16
 if fat=diFAT32 then maxCluster:=$10000000; //FAT32
 if fat=diMaster512 then                    //BBC Master 512 DOS Plus (FAT12)
 begin //These have different values to standard DOS
  maxCluster:=$FF6;
  sectorSize:=$200;
  Lrootsize :=$70;
  LnumFATs  :=1;
  mdb       :=$FF;
  reserved  :=8;
  bootable  :=False;
  master512 :=True;
 end;
 //Not a valid FAT type, so exit
 if maxCluster=1 then exit; //Error 
 //Clear the area
 for index:=0 to (sectorSize*reserved)-1 do WriteByte(0,offset+index,buffer);
 //Total Blocks
 totBlocks:=size div sectorSize;
 //JMP Instruction for Boot sector
 if master512 then
 begin
  WriteByte($FF,offset+$00,buffer);// $EB
  WriteByte($FF,offset+$01,buffer);// $FE
  WriteByte($FF,offset+$02,buffer);// $91
 end
 else
 begin
  WriteByte($EB,offset+$00,buffer);
  WriteByte($3C,offset+$01,buffer);
  WriteByte($90,offset+$02,buffer);
 end;
 //OEM Name
 WriteString('DiImgMgr',offset+$03,8,32,buffer);
 //Block Size
 Write16b(sectorSize,offset+$0B,buffer);
 //Cluster size
 if not master512 then
 begin
  if fat<>diFAT32 then allocSize:=(totBlocks div maxCluster)+1
  else
  begin
   allocSize:=$01; //34MB to 260MB
   if size>$010400000 then allocSize:=$08; //261MB to 8GB
   if size>$200000000 then allocSize:=$10; //8GB to 16GB
   if size>$400000000 then allocSize:=$20; //16GB to 32GB
   if size>$800000000 then allocSize:=$40; //32GB to 2TB
  end;
 end
 else
  allocSize:=4;
 WriteByte(allocSize,offset+$0D,buffer);
 //Reserved sectors
 Write16b(reserved,offset+$0E,buffer);
 //Number of FATs
 WriteByte(LnumFATs,offset+$10,buffer);
 //Size of root : maximum number of entries
 if fat<>diFAT32 then Write16b(Lrootsize,offset+$11,buffer);
 //Total number of blocks
 if totBlocks<=$FFFF then //If it'll fit in the original BIOS block
  Write16b(totBlocks,offset+$13,buffer)
 else //Otherwise it'll need to go in the extended block - only for FAT16 & FAT32
  if(fat=diFAT16)or(fat=diFAT32)then Write32b(totBlocks,offset+$20,buffer);
 //Media descriptor byte
 WriteByte(mdb,offset+$15,buffer);
 //FAT Size
 if not master512 then
 begin
  //Calculation from Microsoft White Paper:
  //Microsoft Extensible Firmware Initiative FAT32 File System Specification
  //Version 1.03, 20001206. Page 21.
  RootDirSectors:=((Lrootsize*32)+(sectorSize-1))DIV sectorSize;
  TmpVal1:=size-(reserved+RootDirSectors);
  TmpVal2:=(256*allocSize)+LnumFATs; //Use 258 here to be closer to Apple spec
  if fat=diFAT32 then TmpVal2:=TmpVal2 div 2;
  FATsize:=(TmpVal1+(TmpVal2-1))div TmpVal2;
  FATsize:=FATsize div sectorSize;
 end
 else
  FATsize:=1;
 if fat<>diFAT32 then Write16b(FATsize,offset+$16,buffer)
 else Write32b(FATsize,offset+$24,buffer);
 //Extended BIOS block
 if not master512 then //The rest of this is not applicable to Master 512 images
 begin
  //Sectors per track
  Write16b($0020,offset+$18,buffer);
  //Number of heads
  Write16b($0010,offset+$1A,buffer);
  if fat<>diFAT32 then //FAT12 and 16
  begin
   //Extended Boot Signature
   WriteByte($29,offset+$26,buffer);
   //Volume Serial Number
   Write32b(VolumeSerialNumber,offset+$27,buffer);
   //Volume Label
   WriteString('NO NAME',offset+$2B,11,32,buffer);
   //File System Type
   if fat=diFAT12 then WriteString('FAT12',offset+$36,8,32,buffer);
   if fat=diFAT16 then WriteString('FAT16',offset+$36,8,32,buffer);
   bcloc:=$3E;
  end;
  if fat=diFAT32 then //FAT32
  begin
   //Root directory cluster $2C 4bytes = $0002
   Write32b(2,offset+$2C,buffer);
   //Location of FS info sector $30 2bytes = $0001
   Write16b(1,offset+$30,buffer);
   //Location of backup sector $32 2bytes = $0006
   Write16b(6,offset+$32,buffer);
   //Extended Boot Signature
   WriteByte($29,offset+$42,buffer);
   //Volume Serial Number
   Write32b(VolumeSerialNumber,offset+$43,buffer);
   //Volume Label
   WriteString('NO NAME',offset+$47,11,32,buffer);
   //File System Type
   WriteString('FAT32',offset+$52,8,32,buffer);
   bcloc:=$5A;
   //Write the FS Info block at $200
   Write32b($41615252,offset+$200,buffer); //Signature
   Write32b($61417272,offset+$3E4,buffer); //Signature
   TmpVal1:=totBlocks-(FATSize*LnumFATs)-reserved-1;
   Write32b(TmpVal1,offset+$3E8,buffer); //Free count
   Write32b(3,offset+$3EC,buffer); //Next free cluster
   Write32b($AA550000,offset+$3FC,buffer);
  end;
  //Boot code
  if bootable then
  begin
   //Write the boot code
   for index:=0 to Length(bootCode)-1 do
    WriteByte(bootCode[index],offset+bcloc+index,buffer);
   //And the text
   WriteString('Non-system disk'#$0D#$0A'Press any key to reboot'#$0D#$0A,
               offset+bcloc+Length(bootCode),0,0,buffer);
  end;
  //Boot block signature - indicates it is bootable
  Write16b($AA55,offset+$1FE,buffer);
  //Create the backup for FAT32
  if fat=diFAT32 then
   for index:=0 to $BFF do
    WriteByte(ReadByte(offset+index,buffer),offset+index+$C00,buffer);
  //Write the FAT(s)
  for index:=0 to LnumFATs-1 do
  begin
   //Calculate this FAT's location
   FATloc:=(reserved*sectorSize)+(FATsize*sectorSize*index);
   //Media descriptor byte, repeated on each FAT
   WriteByte(mdb,offset+FATloc,buffer);
   //Then the 0xFFFF which follows
   if fat<>diFAT32 then
    Write16b($FFFF,offset+FATloc+1,buffer)
   else
   begin
    Write24b($0FFFFF,offset+FATloc+1,buffer);
    Write32b($0FFFFFFF,offset+FATloc+4,buffer);
    Write32b($0FFFFFFF,offset+FATloc+8,buffer);
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Move a file/directory
-------------------------------------------------------------------------------}
function TDiscImage.MoveDOSFile(filename,directory: String): Integer;
var
 direntry : TDirEntry;
 sdir,
 sentry,
 ddir,
 dentry,
 ptr      : Cardinal;
begin
 Result:=-11;//Source file not found
 //Does the file exist?
 if FileExists(filename,sdir,sentry) then
 begin
  Result:=-6;//Destination directory not found
  //Take a copy
  direntry:=FDisc[sdir].Entries[sentry];
  //Does the destination directory exist?
  if(FileExists(directory,ddir,dentry))or(directory=dosrootname)then
  begin
   //Make sure it has been read in
   if not FDisc[FDisc[ddir].Entries[dentry].DirRef].BeenRead then
    ReadDirectory(directory);
   Result:=-10;//Can't move to the same directory
   //Destination directory reference
   ddir:=0;//Root
   if directory<>dosrootname then ddir:=FDisc[ddir].Entries[dentry].DirRef;
   if ddir<>sdir then //Can't move into the same directory
   begin
    Result:=-3; //File already exists in destination directory
    //Alter for the new parent
    direntry.Parent:=directory;
    //Does the filename already exist in the new location?
    if not FileExists(directory+Dir_Sep+direntry.Filename,ptr) then
    begin
     //Insert into the new directory
     Result:=InsertDOSEntry(ddir,direntry);
     //Now remove from the original directory
     RemoveDOSEntry(sdir,sentry);
    end;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Generate a DOS short name from a long name
-------------------------------------------------------------------------------}
function TDiscImage.DOSShortFilename(path,LFN: String;SFN :String=''): String;
var
 ext,n   : String;
 index,
 dir,
 entry   : Cardinal;
const
 illegal = ' +,;=[]';
begin
 Result:='';
 //Validate the path exists, and check for DOS FAT12, 16 or 32
 if(FileExists(path,dir,entry))and(GetMajorFormatNumber=diDOSPlus)then
 begin
  //DOS Plus - then just return the shortened, validated, filename
  if GetMinorFormatNumber=0 then
  begin
   Result:=ValidateDOSFilename(LFN);
   exit;
  end;
  //If this includes the path, then remove the path
  while Pos(dir_sep,LFN)>0 do
   LFN:=Copy(LFN,Pos(dir_sep,LFN)+1);
  //Now validate the filename
  LFN:=ValidateDOSFilename(LFN,True);
  //Any extension?
  ext:='';
  if Pos('.',LFN)>0 then
  begin
   //Find the '.'
   index:=Length(LFN);
   while(LFN[index]<>'.')and(index>2)do dec(index);
   //Then extract it
   ext:=Copy(LFN,index+1);
   if index>1 then LFN:=Copy(LFN,1,index-1) else LFN:='';
   //Make sure the extension is not more than 3 characters, and is uppercase
   ext:=UpperCase(LeftStr(ext,3));
  end;
  //Copy to the output and remove any illegal characters
  if Length(LFN)>0 then
   for index:=1 to Length(LFN) do
    if Pos(LFN[index],illegal)=0 then
     Result:=Result+UpperCase(LFN[index]);
  //Strip any leading periods
  if Length(Result)>0 then while Result[1]='.' do Result:=Copy(Result,2);
  //Still have a filename which is too long?
  if Length(Result)>8 then
  begin
   //Now we need to shorten it and add a '~n' to the end, that is unique
   //'n' can be 1 to 999999
   n:='1';
   LFN:=BuildDOSFilename(LeftStr(Result,7-Length(n))+'~'+n,ext);
   while(FileExists(path+dir_sep+LFN,dir,entry,True))
     and(StrToInt(n)<1000000)and(LFN<>SFN)do
   begin
    n:=IntToStr(StrToInt(n)+1);
    LFN:=BuildDOSFilename(LeftStr(Result,7-Length(n))+'~'+n,ext);
   end;
   //Is 'n' within range?
   if StrToInt(n)<1000000 then Result:=LeftStr(Result,7-Length(n))+'~'+n;
  end;
  Result:=BuildDOSFilename(Result,ext);
 end;
end;

{-------------------------------------------------------------------------------
Put a filename and extension together
-------------------------------------------------------------------------------}
function TDiscImage.BuildDOSFilename(f,e: String): String;
begin
 Result:=f;
 if Length(e)>0 then Result:=Result+'.'+e;
end;

{-------------------------------------------------------------------------------
Produce a report of the image's details
-------------------------------------------------------------------------------}
function TDiscImage.DOSReport(CSV: Boolean): TStringList;
var
 side: Integer;
begin
 Result:=TStringList.Create;
 side:=0;
 if GetMajorFormatNumber=diAcornADFS then
 begin
  if not CSV then Result.Add('');
  Result.Add('DOS Plus partition');
  if not CSV then Result.Add('------------------');
  side:=1;
 end;
 Result.Add('Sector Size: '+IntToStr(secsize)+' bytes');
 Result.Add('Sectors per Track: '+IntToStr(secspertrack));
 Result.Add('Root Address: 0x'+IntToHex(Fdosroot,8));
 Result.Add('Root Size: '+IntToStr(dosroot_size)+' bytes');
 Result.Add('Disc Size: '+IntToStr(disc_size[side])+' bytes');
 Result.Add('Free Space: '+IntToStr(free_space[side])+' bytes');
 Result.Add('Boot Map Location: 0x'+IntToHex(doshead,8));
 Result.Add('FAT Location: 0x'+IntToHex(dosmap,8));
 Result.Add('FAT Size: '+IntToStr(DOSFATSize*secsize)+' bytes');
 Result.Add('Number of FATs: '+IntToStr(NumFATs));
 Result.Add('Cluster Size: '+IntToStr(cluster_size)+' bytes');
 Result.Add('Allocation Unit: '+IntToStr(dosalloc)+' blocks');
 Result.Add('Number of Blocks: '+IntToStr(DOSblocks));
 Result.Add('Reserved Sectors: '+IntToStr(DOSResSecs));
 Result.Add('Disc Name: '+disc_name[side]);
end;
