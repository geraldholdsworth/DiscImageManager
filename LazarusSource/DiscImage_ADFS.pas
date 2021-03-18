//++++++++++++++++++ Acorn ADFS ++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies an ADFS disc and which type
-------------------------------------------------------------------------------}
function TDiscImage.ID_ADFS: Boolean;
var
 Check0,
 Check1,
 Check0a,
 Check1a  : Byte;
 ctr,
 dr_size,
 dr_ptr,
 zone     : Cardinal;
begin
 Result:=False;
 if FFormat=$FF then
 begin
  ResetVariables;
  //Is there actually any data?
  if GetDataLength>0 then
  begin
   //Check for Old Map
   Check0   :=ReadByte($0FF);
   Check1   :=ReadByte($1FF);
   Check0a  :=ByteCheckSum($0000,$100);
   Check1a  :=ByteCheckSum($0100,$100);
   //Do the checksums on both sectors
   if  (Check0a=Check0)
   and (Check1a=Check1) then
   begin
    //Checks are successful, now find out which type of disc: S/M/L/D
    Result:=True;
    //FFormat:=$1F; //Default to ADFS Hard drive
    FMap:=False;  //Set to old map
    FDirType:=0;  //Set to old directory
    ctr:=0;
    //Check where the root is.
    if (Read24b($6D6)=$000002) //Address of the root ($200 for old dir)
    and(ReadByte($200)=ReadByte($6FA)) then //Directory check bytes
     FDirType:=0; //old map, old directory - either S, M or L
    if (Read24b($BDA)=$000004) //Address of the root ($400 for new dir)
    and(ReadByte($400)=ReadByte($BFA)) then //Directory check bytes
    begin
     FDirType:=1; //So, old map, new directory must be ADFS D
     FFormat:=$13;
    end;
    disc_size:=Read24b($0FC)*$100;
    //The above checks will pass through if the first 512 bytes are all zeros,
    //meaning a, e.g., Commodore 64 image will be IDed as an ADFS Old Map.
    //So, we need to check the disc size is not zero also.
    if disc_size=0 then
    begin
     Result:=False;
     ResetVariables;
    end;
    if (disc_size>0) and (FFormat=$FF) then
    begin
     //Not a reliable way of determining disc shape. However, there are three
     //different sizes of the same format.
     case disc_size of
      163840: FFormat:=$10; // ADFS S
      327680: FFormat:=$11; // ADFS M
      655360: FFormat:=$12; // ADFS L
     end;
     if disc_size>819200 then FFormat:=$1F; //Hard drive
    end;
    if FFormat mod $10<3 then //ADFS S,M,L
    begin
     //Set the number of sectors per track - this is not held in the disc
     secspertrack:= 16;
     //Size of the sectors in bytes
     secsize     :=256;
    end;
    if FFormat mod $10=3 then //ADFS D
    begin
     //Set the number of sectors per track - this is not held in the disc
     secspertrack:=   5;
     //Size of the sectors in bytes
     secsize     :=1024;
    end;
    if FFormat mod $10=$F then //ADFS Hard drive
    begin
     secspertrack:= 16;
     secsize     :=256;
     //Make sure that we get a whole number of sectors on every track
     if disc_size mod (secspertrack*secsize)>0 then
      secspertrack:=Round(secspertrack
                   *((disc_size  /  (secspertrack*secsize))
                   - (disc_size div (secspertrack*secsize))));
    end;
   end;
   if not Result then
   begin
    FMap:=True;         //Assume New Map for now
    ctr:=0;
    dr_ptr:=$0000;
    repeat
     if ctr=0 then dr_ptr:=$0004;   //Point the disc record to $0004
     if ctr=1 then dr_ptr:=$0DC0;   //Point the disc record to $0DC0
     if ctr=2 then emuheader:=$0200;//Might have a header, added by an emulator
     //Then find the map
     dr_size   :=60; //Disc record size
     //Read some values from the disc record in the boot block
     //These are the minimum we require to find the map
     if emuheader+dr_ptr+$40<GetDataLength then
     begin
      secsize   :=1 shl ReadByte(dr_ptr+$00); //Sector size
      idlen     :=ReadByte(dr_ptr+$04);       //idlen
      bpmb      :=1 shl ReadByte(dr_ptr+$05); //Bits per map bit
      nzones    :=ReadByte(dr_ptr+$09)
                 +ReadByte(dr_ptr+$2A)*$100;  //nzones is 2 bytes, for E+ and F+
      zone_spare:=Read16b(dr_ptr+$0A);        //Zone spare bits
     end;
     //If there are more than 2 zones, we need the disc record size in bits
     if nzones>2 then
      zone:=dr_size*8
     else
      zone:=0;
     //Calculate the start of the map
     bootmap:=((nzones div 2)*(8*secsize-zone_spare)-zone)*bpmb;
     //If the bootmap is within the size of the disc, and there is at least
     //a single zone then continue
     if(emuheader+bootmap+nzones*secsize<GetDataLength)and(nzones>0)then
     begin
      Result:=True;
      //Check the checksums for each zone
      Check1:=$00;
      for zone:=0 to nzones-1 do
      begin
       //ZoneCheck checksum
       Check0:=ReadByte(bootmap+zone*secsize+$00);
       //CrossCheck checksum
       Check1:=Check1 XOR ReadByte(bootmap+zone*secsize+$03);
       //Check failed, reset format
       if Check0<>GeneralChecksum(bootmap+zone*secsize,
                                  secsize,secsize+4,$4,true) then
        Result:=False;
      end;
      //Cross zone check - should be $FF
      if Check1<>$FF then
       Result:=False;
     end;
     //Check the bootblock checksum
     if (ctr>0) and (Result) then
     begin
      Check0:=ReadByte($0C00+$1FF);
      if ByteChecksum($0C00,$200)<>Check0 then Result:=False;
     end;
     inc(ctr);
    until (Result) or (ctr=3);
    if Result then
    begin
     case ctr of
      1: FFormat:=$14; //ADFS E/E+
      2: FFormat:=$16; //ADFS F/F+
      3: FFormat:=$1F; //ADFS Hard Drive
     end;
     //Boot block checksum, if there is a partial disc record at $0DC0
     if dr_ptr=$DC0 then
     begin
      Check0 :=ByteChecksum($C00,$200);
      Check0a:=ReadByte($DFF);
      if Check0<>Check0a then FFormat:=$FF; //Checksums do not match
     end;
    end;
    //Check for type of directory, and change the format if necessary
    if FFormat<>$FF then
    begin
     FDirType:=1; //New Directory
     //Determine if it is a '+' format by reading the version flag
     if ReadByte(dr_ptr+$2C)>0 then
     begin
      if FFormat<>$1F then inc(FFormat);
      FDirType:=2;
     end;
    end;
   end;
  end;
  //Return a true or false
  Result:=FFormat shr 4=1;
 end;
end;

{-------------------------------------------------------------------------------
Read ADFS Directory
-------------------------------------------------------------------------------}
function TDiscImage.ReadADFSDir(dirname: String; sector: Cardinal): TDir;
var
 Entry              : TDirEntry;
 temp,
 StartName,EndName,
 dirtitle,pathname  : String;
 ptr,
 dircheck,numentrys,
 dirsize,
 entrys,nameheap,
 tail,NameLen,
 entrysize,offset,
 NameOff,amt        : Cardinal;
 addr               : TFragmentArray;
 StartSeq,EndSeq,
 dirchk,NewDirAtts  : Byte;
 validdir,validentry,
 endofentry         : Boolean;
const
 //Attributes
 OldAtts: array[0..9] of Char = ('R','W','L','D','E','r','w','e','P',' ');
 NewAtts: array[0..7] of Char = ('R','W','L','D','r','w',' ',' ');
begin
 RemoveControl(dirname);
 //Store complete path
 pathname:=dirname;
 //Update the progress indicator
 UpdateProgress('Reading '+pathname);
 //Store directory name
 if Pos(dir_sep,dirname)>0 then
 begin
  temp:=dirname;
  repeat
   temp:=Copy(temp,Pos(dir_sep,temp)+1,Length(temp))
  until Pos(dir_sep,temp)=0;
  Result.Directory:=temp;
 end
 else
  Result.Directory:=dirname;
 //Initialise some of the variables
 StartSeq        :=$00;
 EndSeq          :=$FF;
 numentrys       :=0;
 tail            :=$00;
 dirsize         :=$00;
 nameheap        :=$00;
 entrys          :=0;
 entrysize       :=$00;
 NewDirAtts      :=$00;
 dirchk          :=0;
 namesize        :=$00;
 dirtitle        :='';
 SetLength(addr,0);
 //Get the offset address
 if FMap then
 begin
  //New Map, so the sector will be an internal disc address
  if dirname=root_name then addr:=NewDiscAddrToOffset(0)
  else addr:=NewDiscAddrToOffset(sector);
  //But we need it as an offset into the data
  if Length(addr)>0 then
   sector:=addr[0].Offset;
 end
 else sector:=sector*$100; //Is Old Map, so offset is just the sector * $100
 //Read in the directory header
 case FDirType of
  0,1: //Old and New Directory
  begin
   StartSeq :=ReadByte(sector);        //Start Sequence Number to match with end
   StartName:=ReadString(sector+1,-4); //Hugo or Nick
   if FDirType=0 then //Old Directory
   begin
    numentrys:=47;                     //Number of entries per directory
    dirsize  :=1280;                   //Directory size in bytes
    tail     :=$35;                    //Size of directory tail
   end;
   if FDirType=1 then //New Directory
   begin
    numentrys:=77;                     //Number of entries per directory
    dirsize  :=2048;                   //Directory size in bytes
    tail     :=$29;                    //Size of directory tail
   end;
   entrys   :=$05;                     //Pointer to entries, from sector
   entrysize:=$1A;                     //Size of each entry
  end;
  2:   //Big Directory
  begin
   StartSeq :=ReadByte(sector);         //Start sequence number to match with end
   StartName:=ReadString(sector+$04,-4);//Should be SBPr
   NameLen  :=Read32b(sector+$08);     //Length of directory name
   dirsize  :=Read32b(sector+$0C);     //Directory size in bytes
   numentrys:=Read32b(sector+$10);     //Number of entries in this directory
   namesize :=Read32b(sector+$14);     //Size of the name heap in bytes
   dirname  :=ReadString(sector+$1C,-NameLen);//Directory name
   entrys   :=(($1C+NameLen+1+3)div 4)*4;         //Pointer to entries, from sector
   tail     :=$08;                                //Size of directory tail
   entrysize:=$1C;                                //Size of each entry
   nameheap :=entrys+numentrys*entrysize;         //Offset of name heap
  end;
 end;
 //Now we know the size of the directory, we can read in the tail
 tail:=dirsize-tail;
 //And mark it on the Free Space Map
 for amt:=sector to sector+dirsize do ADFSFillFreeSpaceMap(amt,$FD);
 //Not all of the tail is read in
 case FDirType of
  0:
  begin
   dirtitle:=ReadString(sector+tail+$0E,-19);//Title of the directory
   EndSeq  :=ReadByte(sector+tail+$2F);      //End sequence number to match with start
   EndName :=ReadString(sector+tail+$30,-4); //Hugo or Nick
   dirchk  :=ReadByte(sector+tail+$34);      //Directory Check Byte
  end;
  1:
  begin
   dirtitle:=ReadString(sector+tail+$06,-19);//Title of the directory
   EndSeq  :=ReadByte(sector+tail+$23);      //End sequence number to match with start
   EndName :=ReadString(sector+tail+$24,-4); //Hugo or Nick
   dirchk  :=ReadByte(sector+tail+$28);      //Directory Check Byte
  end;
  2:
  begin
   EndName :=ReadString(sector+tail+$00,-4); //Should be oven
   EndSeq  :=ReadByte(sector+tail+$04);      //End sequence number to match with start
   dirtitle:=dirname;                        //Does not have a directory title
   dirchk  :=ReadByte(sector+tail+$07);      //Directory Check Byte
  end;
 end;
 //Save the directory title
 Result.Title:=dirtitle;
 //Check for broken directory
 //This can result in having a valid directory structure, but a broken directory
 //ADFS normally refuses to list broken directories, but we will list them anyway,
 //just marking the directory as broken and return an error code
 Result.ErrorCode:=0;
 if (EndSeq<>StartSeq) then
  Result.ErrorCode:=Result.ErrorCode OR $01;
 if ((FDirType<2) and (StartName<>EndName)) then
  Result.ErrorCode:=Result.ErrorCode OR $02;
 if ((FDirType=2) and ((StartName<>'SBPr') or (EndName<>'oven'))) then
  Result.ErrorCode:=Result.ErrorCode OR $04;
 Result.Broken:=Result.ErrorCode<>$00;
 //Check for valid directory
 //We won't try and get the directory structure if it appears that it is invalid
 //Could just be that one of the names has got corrupt, but could be much worse
 validdir:=False;
 if((FDirType<2)and(StartName=EndName)and((StartName='Hugo')or(StartName='Nick')))
 or((FDirType=2)and(StartName='SBPr')and(EndName='oven'))then
  validdir:=True;
 //Load the entries
 if validdir then
 begin
  //Set up the array
  SetLength(Result.Entries,0);
  //Pointer to entry number - we'll use this later to find the end of the list
  ptr:=0;
  //Flag for a valid entry
  validentry:=True;
  while (ptr<numentrys) and (validentry) do
  begin
   //Offset to entry
   offset:=sector+entrys+ptr*entrysize;
   //Blank the entries
   ResetDirEntry(Entry);
   Entry.Parent:=pathname;
   //Read in the entries
   case FDirType of
    0,1: //Old and New Directory
     if ReadByte(offset)<>0 then //0 marks the end of the entries
     begin
      Entry.Filename :=ReadString(offset,-10,True);//Filename (including attributes for old)
      Entry.LoadAddr :=Read32b(offset+$0A);  //Load Address (can be timestamp)
      Entry.ExecAddr :=Read32b(offset+$0E);  //Execution Address (can be filetype)
      Entry.Length   :=Read32b(offset+$12);  //Length in bytes
      Entry.Sector   :=Read24b(offset+$16);  //How to find the file
      temp:='';
      //Old directories - attributes are in the filename's top bit
      if FDirType=0 then
      begin
       endofentry:=False;
       for amt:=0 to Length(Entry.Filename)-1 do
       begin
        if ord(Entry.Filename[amt+1])shr 7=1 then
         temp:=temp+OldAtts[amt];
        if ord(Entry.Filename[amt+1])AND$7F=$0D then endofentry:=True;
        //Clear the top bit
        if not endofentry then
         Entry.Filename[amt+1]:=chr(ord(Entry.Filename[amt+1]) AND $7F)
        else
         Entry.Filename[amt+1]:=' ';
       end;
       RemoveSpaces(Entry.Filename);
       //Reverse the attribute order to match actual ADFS
       for amt:=Length(temp) downto 1 do
        Entry.Attributes:=Entry.Attributes+temp[amt];//Attributes
      end;
      //New directories - attributes are separate, so filenames can have top bit set
      if FDirType=1 then
       NewDirAtts   :=ReadByte(offset+$19);  //Attributes will be disected with Big
     end
     else validentry:=False;
    2: //Big Directory
    begin
     Entry.LoadAddr :=Read32b(offset+$00);  //Load Address
     Entry.ExecAddr :=Read32b(offset+$04);  //Execution Address
     Entry.Length   :=Read32b(offset+$08);  //Length in bytes
     Entry.Sector   :=Read32b(offset+$0C);  //How to find file
     NewDirAtts     :=Read32b(offset+$10);  //Attributes (as New)
     NameLen        :=Read32b(offset+$14);  //Length of filename
     NameOff        :=Read32b(offset+$18);  //Offset into heap of filename
     Entry.Filename :=ReadString(sector+nameheap+NameOff,-NameLen); //Filename
    end;
   end;
   RemoveControl(Entry.Filename);
   //Attributes for New and Big
   if FDirType>0 then
   begin
    temp:='';
    for amt:=0 to 7 do
     if IsBitSet(NewDirAtts,amt) then temp:=temp+NewAtts[amt];
    //Reverse the attribute order to match actual ADFS
    for amt:=Length(temp) downto 1 do
     Entry.Attributes:=Entry.Attributes+temp[amt];//Attributes
   end;
   //If we have a valid entry then we can see if it is filetyped/datestamped
   //and add it to the list
   if validentry then
   begin
    //RISC OS - file may be datestamped and filetyped
    if (Entry.LoadAddr shr 20=$FFF) and (FFormat>$12) then
    begin
     //Get the 12 bit filetype
     temp:=IntToHex((Entry.LoadAddr AND $000FFF00)div $100,3);
     amt:=0;
     //Look it up in the table of RISC OS issued types
     repeat
      inc(amt);
     until (Integer(amt)=Length(FileTypes)) OR (temp=Copy(FileTypes[amt],1,3));
     //Found? Then assign to the Filetype property
     if temp=Copy(FileTypes[amt],1,3) then
      Entry.Filetype:=Copy(FileTypes[amt],4,Length(FileTypes[amt]))
     else
     //Otherwise just put the 12 bit filetype in
      Entry.Filetype:=temp;
     Entry.ShortFiletype:=temp;
     //Now sort the timestamp
     Entry.TimeStamp:=RISCOSToTimeDate(Entry.ExecAddr+
                                      (Entry.LoadAddr AND $FF)*$100000000);
    end;
    //Not a directory - default. Will be determined later
    Entry.DirRef:=-1;
    //Add to the result
    SetLength(Result.Entries,Length(Result.Entries)+1);
    Result.Entries[Length(Result.Entries)-1]:=Entry;
    //Move on to next
    inc(ptr);
   end;
  end;
  //Now we can run the directory check on DirCheckByte
  //But only for New and Big Directories, optional for old (ignored if zero)
  if ((FDirType=0) and (dirchk<>0)) or (FDirType>0) then
  begin
   //This value is the check byte.
   dircheck:=CalculateADFSDirCheck(sector);
   //Compare with what is stored
   if dirchk<>dircheck then
   begin
    //If different, just mark as broken directory
    Result.Broken:=True;
    Result.ErrorCode:=Result.ErrorCode OR $08;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Calculate the directory check byte
-------------------------------------------------------------------------------}
function TDiscImage.CalculateADFSDirCheck(sector:Cardinal): Byte;
var
 dircheck,
 amt,
 offset,
 tail,
 dirsize,
 EndOfChk,
 numentrys : Cardinal;
begin
 //Set up variables
 if FDirType=0 then  //Old Directory
 begin
  dirsize:=1280;
  tail:=dirsize-$35;
 end;
 if FDirType=1 then  //New Directory
 begin
  dirsize:=2048;
  tail:=dirsize-$29;
 end;
 if FDirType<2 then  //Old or New Directory
 begin
  //Count the number of entries
  numentrys:=0;
  while ReadByte(sector+$05+numentrys*$1A)<>0 do inc(numentrys);
  EndOfChk:=numentrys*$1A+$05;
 end;
 if FDirType=2 then  //Big Directory
 begin
  //Need to do some more calculation for the end of check figure
  dirsize:=Read32b(sector+$0C);
  tail:=dirsize-$08;
  numentrys:=Read32b(sector+$10);
  EndOfChk:=((($1C+Read32b(sector+$08)+1+3)div 4)*4)
            +(numentrys*$1C)
            +Read32b(sector+$14);
 end;
 //This has virtually the same loop repeated 5 times - but it is less code to
 //do it like this, than a single loop with if...then conditions to determine
 //the different iterations.
 dircheck:=0;
 amt:=0;
 //Stage 1: All the whole words at the start of the directory are accumulated
 while amt+3<EndOfChk do
 begin
  offset:=Read32b(sector+amt);
  dircheck:=offset XOR ROR13(dircheck);
  inc(amt,4);
 end;
 //Stage 2: The bytes (<4) at the start of the directory are accumulated
 //individually.
 while amt<EndOfChk do
 begin
  offset:=ReadByte(sector+amt);
  dircheck:=offset XOR ROR13(dircheck);
  inc(amt);
 end;
 //Stage 3: The first byte at the beginning of the directory tail is skipped.
 amt:=tail;
 //But not with Big Directories
 if FDirType<2 then inc(amt);
 //Stage 4: The whole words in the directory tail are accumulated, except the
 //very last word which is excluded as it contains the check byte.
 while amt+3<dirsize-4 do
 begin
  offset:=Read32b(sector+amt);
  dircheck:=offset XOR ROR13(dircheck);
  inc(amt,4);
 end;
 //Stage 4a: Big Directories also accumulate the final few bytes, but not the
 //final byte
 if FDirType=2 then
  while amt<dirsize-1 do
  begin
   offset:=ReadByte(sector+amt);
   dircheck:=offset XOR ROR13(dircheck);
   inc(amt);
  end;
 //Stage 5: The accumulated word has its four bytes exclusive ORd (EOR) together.
 Result  :=(dircheck AND $FF)
      XOR ((dircheck shr 24) AND $FF)
      XOR ((dircheck shr 16) AND $FF)
      XOR ((dircheck shr  8) AND $FF);
end;

{-------------------------------------------------------------------------------
Convert an ADFS New Map address to buffer offset address, with fragment lengths
-------------------------------------------------------------------------------}
function TDiscImage.NewDiscAddrToOffset(addr: Cardinal;offset:Boolean=True): TFragmentArray;
var
 fragid          : TFragmentArray;
 i,j,sector,id,
 allmap,len,off,
 zone,start,
 start_zone,
 zonecounter,
 id_per_zone     : Cardinal;
const
 dr_size = $40; //Size of disc record + header (zone 0)
 header  = 4;   //Size of zone header only (zones >0)
begin
 Result:=nil;
 SetLength(Result,0);
 if FMap then //Only works for new maps
 begin
  if addr=0 then //Root
  begin
   //We've been given the address of the root, but we know where this is so no
   //need to calculate it.
   SetLength(Result,1);
   Result[0].Offset:=root;//bootmap+(nzones*secsize*2);
   case FDirType of
    0: Result[0].Length:=$500;
    1: Result[0].Length:=$800;
    2: Result[0].Length:=root_size;
   end;
  end
  else
  begin
   //Set up an array
   SetLength(fragid,0);
   //Go through the allocation map, looking for the fragment
   //First we need to know how many ids per zone there are (max)
   id_per_zone:=((secsize*8)-zone_spare)div(idlen+1);
   //Then work out the start zone
   start_zone:=((addr DIV $100) mod $10000)div id_per_zone;
   //This is because the first fragment of an object does not necessarily
   //appear in zone order. Later fragments could be in earlier zones.
   for zonecounter:=0 to nzones-1 do
   begin
    //Account for which zone to start searching from
    zone:=(zonecounter+start_zone)mod nzones;
    //This is the start of where we take the offsets from
    start :=bootmap+dr_size;
    //Work out the end of this zone
    allmap:=(zone+1)*secsize*8-dr_size*8;
    //i is the bit counter - we need to move onto the next zone boundary
    i     :=zone*secsize*8;
    if zone>0 then dec(i,dr_size*8-header*8);
    repeat
     //Mark the offset
     off:=i;
     //Read in idlen number of bits
     id:=ReadBits(start,i,idlen);
     //and move the pointer on idlen bits
     inc(i,idlen);
     //Now find the end of the fragment entry
     j:=i-1;
     repeat
      inc(j);
     until(IsBitSet(ReadByte(start+(j div 8)),j mod 8))or(j>=allmap);
     //Make a note of the length
     if offset then
      len:=((j-i)+1+idlen)*bpmb
     else
      len:=(j-i)+1+idlen;
     //Move the pointer on, after the '1'
     i:=j;
     //Does it match the id we are looking for?
     if id=(addr div $100)mod$10000 then
     begin
      if offset then //Offset as image file offset
       off:=((off-(zone_spare*zone))*bpmb) mod disc_size
      else //Offset as number of bits from start of zone
       begin
        //Add the disc record (we are counting from the zone start
        inc(off,(dr_size*8));
        //Remove the other zones
        dec(off,(zone*secsize*8));
        //Remove the intial byte, as we need to point our offset from freelink
        dec(off,8);
       end;
      //Fragment ID found, so add it - there could be a few entries
      SetLength(fragid,Length(fragid)+1);
      fragid[Length(fragid)-1].Offset:=off;
      //Save the length
      fragid[Length(fragid)-1].Length:=len;
      //Save the zone
      fragid[Length(fragid)-1].Zone  :=zone;
     end;
     inc(i);
    until i>=allmap;
   end;
   //Now we need to set up the result array and convert from addresses to offsets
   SetLength(Result,Length(fragid));
   if Length(fragid)>0 then
    for i:=0 to Length(fragid)-1 do
    begin
     sector:=addr mod $100;
     //Sector needs to have 1 subtracted, if >=1
     if sector>=1 then dec(sector);
     //Then calculate the offset
     if offset then
      Result[i].Offset:=(fragid[i].Offset+(sector*secsize))
     else
      Result[i].Offset:=fragid[i].Offset;
     //Add the length of this fragment
     Result[i].Length:=fragid[i].Length;
     //Add the zone of this fragment
     Result[i].Zone  :=fragid[i].Zone;
    end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Calculate offset into image given the disc address (L only)
-------------------------------------------------------------------------------}
function TDiscImage.OldDiscAddrToOffset(disc_addr: Cardinal): Cardinal;
var
 tracks,
 track_size,
 track,
 side,
 oldheads,
 data_offset : Cardinal;
begin
 Result:=disc_addr;
 //ADFS L
 if FFormat=$12 then
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
  Result:= (track_size*side)+(track*track_size*oldheads)+data_offset;
 end;
end;

{-------------------------------------------------------------------------------
Calculate disc address given the offset into image (L only)
-------------------------------------------------------------------------------}
function TDiscImage.OffsetToOldDiscAddr(offset: Cardinal): Cardinal;
var
 tracks,
 track_size,
 track,
 side,
 oldheads,
 data_offset : Cardinal;
begin
 Result:=offset;
 //ADFS L
 if FFormat=$12 then
 begin
  //Number of tracks and heads
  tracks:=80;
  oldheads:=2;
  //Track Size;
  track_size:=secspertrack*secsize;
  //Track number
  track:=offset DIV (track_size*oldheads);
  //Which side
  side:=(offset MOD (track_size*oldheads))DIV track_size;
  //Offset into the sector for the data
  data_offset:=offset MOD track_size;
  //Final result
  Result:= (track*track_size)+(tracks*track_size*side)+data_offset;
 end;
end;

{-------------------------------------------------------------------------------
Calculate Boot Block or Old Map Free Space Checksum
-------------------------------------------------------------------------------}
function TDiscImage.ByteChecksum(offset,size: Cardinal): Byte;
var
 acc,
 pointer: Cardinal;
 carry : Byte;
begin
 //Reset the accumulator
 acc:=0;
 //Iterate through the block, ignoring the final byte (which contains the
 //checksum)
 for pointer:=size-2 downto 0 do
 begin
  //Add in the carry
  carry:=acc div $100;
  //and reset the carry
  acc:=acc and $FF;
  //Add each byte to the accumulator
  inc(acc,ReadByte(offset+pointer)+carry);
 end;
 //Return an 8 bit number
 Result:=acc and $FF;
end;

{-------------------------------------------------------------------------------
Read ADFS Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadADFSDisc: TDisc;
var
 d,ptr    : Cardinal;
 OldName0,
 OldName1 : String;
 addr: TFragmentArray;
begin
 //Initialise some variables
 root   :=$00; //Root address (set to zero so we can id the disc)
 Result:=nil;
 SetLength(Result,0);
 //Read in the header information (that hasn't already been read in during
 //the initial checks
 if FFormat<>$FF then //But only if the format is valid
 begin
  //ADFS Old Map
  if not FMap then
  begin
   //Set up boot option
   SetLength(bootoption,1);
   bootoption[0]:=ReadByte($1FD);
   //We already found the root when IDing it as ADFS, so now just confirm
   d:=2;
   root:=0;
   //Root size for old map old directory - assume for now
   root_size:=$500;
   repeat
    if (ReadString((d*$100)+1,-4)='Hugo')
    or (ReadString((d*$100)+1,-4)='Nick') then
     root:=d;
    inc(d);
   until (d=(disc_size div $100)-1) or (root>0);
   if root=0 then
    ResetVariables //Failed to find root, so reset the format
   else
   begin
    //Set the root size for old map new directory
    if FDirType=1 then root_size:=$800;
    //Get the two parts of the disc title
    OldName0 :=ReadString($0F7,-5);
    OldName1 :=ReadString($1F6,-5);
    //Start with a blank title
    disc_name:='          ';
    //Re-assemble the disc title
    if Length(OldName0)>0 then
     for d:=1 to Length(OldName0) do
      disc_name[(d*2)-1]  :=chr(ord(OldName0[d])AND$7F);
    //Both parts are interleaved
    if Length(OldName1)>0 then
     for d:=1 to Length(OldName1) do
      disc_name[ d*2   ]  :=chr(ord(OldName1[d])AND$7F);
    //Then remove all extraenous spaces
    RemoveSpaces(disc_name);
   end;
  end;
  //ADFS New Map
  if FMap then
  begin
   SetLength(bootoption,1);
   //Disc description starts at offset 4 and is 60 bytes long
   //Not all of these values will be used
   secsize     :=1 shl ReadByte(bootmap+$04);
   secspertrack:=ReadByte(bootmap+$05);
   heads       :=ReadByte(bootmap+$06);
   density     :=ReadByte(bootmap+$07);
   idlen       :=ReadByte(bootmap+$08);
   bpmb        :=1 shl ReadByte(bootmap+$09);
   skew        :=ReadByte(bootmap+$0A);
   bootoption[0]:=ReadByte(bootmap+$0B);
   lowsector   :=ReadByte(bootmap+$0C);
   nzones      :=ReadByte(bootmap+$0D);
   zone_spare  :=Read16b(bootmap+$0E);
   root        :=Read32b(bootmap+$10);
   disc_size   :=Read32b(bootmap+$14);
   disc_id     :=Read16b(bootmap+$18);
   disc_name   :=ReadString(bootmap+$1A,-10);
   disctype    :=Read32b(bootmap+$24);
   //Newer attributes for E+ and F+
   disc_size   :=disc_size+Read32b(bootmap+$28)*$100000000;
   share_size  :=1 shl ReadByte(bootmap+$2C);
   big_flag    :=ReadByte(bootmap+$2D);
   nzones      :=nzones+ReadByte(bootmap+$2E)*$100;
   format_vers :=Read32b(bootmap+$30);
   root_size   :=Read32b(bootmap+$34);
   //The root does not always follow the map
   addr:=NewDiscAddrToOffset(root);
   //So, find it - first reset it
   root:=0;
   //Look for a fragment of the correct length...only works on '+' formats
   for d:=0 to Length(addr)-1 do
    if addr[d].Length=root_size then root:=addr[d].Offset;
   //This failed to find it, so 'guess' - we'll assume it is after the bootmap
   if root=0 then
    for d:=Length(addr)-1 downto 0 do
     if addr[d].Offset>bootmap then root:=addr[d].Offset;
   //Failed to find, so resort to where we expect to find it
   if root=0 then root:=bootmap+(nzones*secsize*2);
   //Update the Format, now we know the disc size
   if disc_size>1638400 then FFormat:=$1F;
   //Make the disc title easier to work with
   RemoveSpaces(disc_name); //Remove spaces
   RemoveTopBit(disc_name); //Remove top-bit set characters
  end;
  if root>$00 then //If root is still $00, then we have failed to id the disc
  begin
   //Create an entry for the root
   SetLength(Result,1);
   //Blank the values
   ResetDir(Result[0]);
   //Calculate the Free Space Map
   ADFSFreeSpaceMap;
   //Read the root
   Result[0]:=ReadADFSDir(root_name,root);
   //Now iterate through the entries and find the sub-directories
   d:=0;
   repeat
    //If there are actually any entries
    if Length(Result[d].Entries)>0 then
    begin
     //Go through the entries
     for ptr:=0 to Length(Result[d].Entries)-1 do
      //And add them if they are valid
      if Result[d].Entries[ptr].Filename<>'' then
      begin
       //Attribute has a 'D', so drill down
       if Pos('D',Result[d].Entries[ptr].Attributes)>0 then
       begin
        //Once found, list their entries
        SetLength(Result,Length(Result)+1);
        //Read in the contents of the directory
        Result[Length(Result)-1]:=ReadADFSDir(Result[d].Entries[ptr].Parent
                                             +dir_sep
                                             +Result[d].Entries[ptr].Filename,
                                              Result[d].Entries[ptr].Sector);
        //Update the directory reference
        Result[d].Entries[ptr].DirRef:=Length(Result)-1;
       end;
      end;
    end;
    inc(d);
   //The length of disc will increase as more directories are found
   until d=Cardinal(Length(Result));
  end;
 end;
end;

{-------------------------------------------------------------------------------
Works out how much free space there is, and creates the free space map
-------------------------------------------------------------------------------}
procedure TDiscImage.ADFSFreeSpaceMap;
var
 d,f,p,
 ptr,c,
 finish,
 address          : Cardinal;
 freeend          : Byte;
 fsfragments      : TFragmentArray;
begin
 //Update progress
 UpdateProgress('Reading ADFS Free Space Map.');
 //Reset the free space counter
 free_space:=0;
 //Reset the array
 if (secspertrack>0) and (secsize>0) then //As long as these have been set
 begin
  //Number of sides
  SetLength(free_space_map,1);
  //Number of tracks
  SetLength(free_space_map[0],disc_size div (secspertrack*secsize));
  for c:=0 to Length(free_space_map[0])-1 do
  begin
   //Number of sectors per track
   if FMap then
    SetLength(free_space_map[0,c],(secspertrack*secsize)DIV bpmb)
   else
    SetLength(free_space_map[0,c],secspertrack{*(secsize div $100)});
   //Set them all to be uesd, for now.
   for d:=0 to Length(free_space_map[0,c])-1 do
    free_space_map[0,c,d]:=$FF;
  end;
  //Update progress
  UpdateProgress('Reading ADFS Free Space Map..');
 end;
 //Old Map (ADFS S,M,L, and D)
 if not FMap then
 begin
  //We'll add in the directories
  if Length(FDisc)>0 then
   for d:=0 to Length(FDisc)-1 do
    if Length(FDisc[d].Entries)>0 then
     for p:=0 to Length(FDisc[d].Entries)-1 do
      if FDisc[d].Entries[p].DirRef<>-1 then
       for f:=0 to FDisc[d].Entries[p].Length-1 do
        ADFSFillFreeSpaceMap(FDisc[d].Entries[p].Sector*$100+f,$FD);
  ptr:=(root*$100)+root_size; //Pointer to the first free space area
  //Set the system area on the FSM
  for address:=0 to ptr-1 do
   ADFSFillFreeSpaceMap(address,$FE);
  //Now the free space
  d:=0;//Counter into the map
  freeend:=ReadByte($1FE);//Number of entries
  //Continue for as many entries as there is
  while d<freeend do
  begin
   p:=Read24b(d)*$100;      //Pointer
   f:=Read24b($100+d)*$100; //Size
   //If the read pointer is between the last area, discard
   //Update the array
   if (secspertrack>0) and (secsize>0) then
    for address:=p to (p+f)-1 do
     ADFSFillFreeSpaceMap(address,$00);
   //Update the pointer to the end of the current area
   ptr:=p+f;
   //Add it to the total
   inc(free_space,f);
   //Move to the next entry
   inc(d,3);
   //Update progress
   UpdateProgress('Reading ADFS Free Space Map..'+AddChar('.','',d div 3));
  end;
 end;
 //New Map (ADFS E,E+,F,F+)
 if FMap then
 begin
  //Get the free fragments
  fsfragments:=ADFSGetFreeFragments;
  //If there are any
  if Length(fsfragments)>0 then
  begin
   for c:=0 to Length(fsfragments)-1 do
   begin
    address:=fsfragments[c].Offset;
    finish:=fsfragments[c].Offset+fsfragments[c].Length;
    //Now just go through and fill it with zeros
    while address<finish do
    begin
     //Mark it as empty
     ADFSFillFreeSpaceMap(address,$00);
     //Add to the free space counter
     if address<disc_size then inc(free_space,bpmb);
     //Advance the address
     inc(address,bpmb);
    end;
   end;
  end;
  //We'll add in the directories
  if Length(FDisc)>0 then
   for d:=0 to Length(FDisc)-1 do
    if Length(FDisc[d].Entries)>0 then
     for p:=0 to Length(FDisc[d].Entries)-1 do
      if FDisc[d].Entries[p].DirRef<>-1 then
      begin
       fsfragments:=NewDiscAddrToOffset(FDisc[d].Entries[p].Sector);
       if Length(fsfragments)>0 then
        for f:=0 to Length(fsfragments)-1 do
         for address:=0 to fsfragments[f].Length-1 do
          ADFSFillFreeSpaceMap(fsfragments[0].Offset+address,$FD);
      end;
  //Mark the system area
  for address:=bootmap to bootmap+(nzones*secsize*2) do //Two copies of the map
   ADFSFillFreeSpaceMap(address,$FE);
  //And the partial disc record/disc defect list
  if nzones>1 then
   for address:=$C00 to $DFF do ADFSFillFreeSpaceMap(address,$FE);
 end;
end;

{-------------------------------------------------------------------------------
Fill part of the FSM with a byte
-------------------------------------------------------------------------------}
procedure TDiscImage.ADFSFillFreeSpaceMap(address: Cardinal;usage: Byte);
var
 t,s: Cardinal;
begin
 if not FMap then
 begin
  //Track
  t:=address div (secspertrack*secsize);
  //Sector
  if t>0 then
   s:=(address mod (t*secspertrack*secsize))div secsize
  else
   s:=address div secsize;
 end;
 //New Map
 If FMap then
 begin
  //Track of where we are looking
  t:=address DIV (secspertrack*secsize);
  //bpmb chunk into this track
  if t>0 then
   s:=(address MOD (t*secspertrack*secsize))DIV bpmb
  else
   s:=address DIV bpmb;
 end;
 //Make sure we haven't overshot the end of the disc
 if t<Length(free_space_map[0]) then
  //Or the end of the current track
  if s<Length(free_space_map[0,t]) then
   //Set the chunk as system
   free_space_map[0,t,s]:=usage;
end;

{-------------------------------------------------------------------------------
Create ADFS blank image
-------------------------------------------------------------------------------}
function TDiscImage.FormatADFS(minor: Byte): TDisc;
var
 t,mapsize: Integer;
 check    : Byte;
 dirid,att: String;
begin
 //Blank everything
 ResetVariables;
 //Set the format
 FFormat:=$10+minor;
 //Set the filename
 imagefilename:='Untitled.'+FormatExt;
 //Setup the data area
 case minor of
  0 : SetDataLength( 160*1024);  //S (160KB)
  1 : SetDataLength( 320*1024);  //M (320KB)
  2 : SetDataLength( 640*1024);  //L (640KB)
  3 : SetDataLength( 800*1024);  //D (800KB)
  4 : SetDataLength( 800*1024);  //E (800KB)
  5 : SetDataLength( 800*1024);  //E+(800KB)
  6 : SetDataLength(1600*1024);  //F (1.6MB)
  7 : SetDataLength(1600*1024);  //F+(1.6MB)
 end;
 //Setup the variables
 if minor<4 then //Old maps (S, M, L and D)
 begin
  FMap:=False;
  if minor<3 then //ADFS S, M and L
  begin
   FDirType:=0;               //Old Directory
   secspertrack:=16;          //Sectors Per Track
   secsize:=256;              //Sector size
   root:=$200;                //Where the root is
   heads:=1;                  //Number of heads
   if minor=2 then heads:=2;
   root_size:=$500;           //Size of the root
  end;
  if minor=3 then //ADFS D
  begin
   FDirType:=1;               //New Directory
   secspertrack:=5;           //Sectors Per Track
   secsize:=1024;             //Sector size
   root:=$400;                //Where the root is
   heads:=2;                  //Number of heads
   root_size:=$800;           //Size of the root
  end;
  nzones:=1;                   //Number of zones (not required)
 end;
 if minor>3 then //New maps (E, E+, F, and F+)
 begin
  FMap:=True;
  secsize:=1024;
  heads:=2;
  //New Directory : E and F
  if (minor=4) OR (minor=6) then FDirType:=1;
  //Big Directory : E+ and F+
  if (minor=5) OR (minor=7) then FDirType:=2;
  //E and E+
  if (minor=4) OR (minor=5) then
  begin
   secspertrack:=5;
   nzones:=1;
  end;
  //F and F+
  if (minor=6) OR (minor=7) then
  begin
   secspertrack:=10;
   nzones:=4;
  end;
 end;
 //Fill with zeros
 for t:=0 to GetDataLength-1 do WriteByte(0,t);
 //Set the boot option
 SetLength(bootoption,1);
 bootoption[0]:=0;
 SetLength(free_space_map,1); //Free Space Map
 disc_size:=GetDataLength;    //Disc Size
 //Fill with zeros
 for t:=0 to disc_size-1 do WriteByte(0,t);
 //Write the map
 if not FMap then //Old Map
 begin
  //Old map FreeStart
  Write24b((root+root_size)div$100,$000);
  //Disc size
  Write24b(disc_size div$100,$0FC);
  //Checksum
  WriteByte(ByteCheckSum($0000,$100),$0FF);
  //Old map FreeLen
  Write24b((disc_size-(root+root_size))div$100,$100);
  //Disc ID
  Write24b($4077,$1FB); //Random 16 bit number
  //Old map FreeEnd
  WriteByte($03,$1FE);
  //Checksum
  WriteByte(ByteCheckSum($0100,$100),$1FF);
 end;
 if FMap then //New Map
 begin
  if nzones=1 then
  begin
   bootmap:=$0000;
   //Zone header
   Write16b($8218,bootmap+$01); //FreeLink
   WriteByte($FF, bootmap+$03); //CrossCheck
   //Part of the full disc record
   WriteByte($02,bootmap+$07); //Density
   WriteByte($07,bootmap+$09); //log2bpmb
   Write16b($0520,bootmap+$0E);//zone_spare
   if FDirType=1 then          //root
    Write32b($00000203,bootmap+$10)
   else
    Write32b($00000301,bootmap+$10);
   //Allocation map
   WriteByte($02,bootmap+$40);
   if FDirType=2 then
   begin
    WriteByte($80,bootmap+$41);
    WriteByte($03,bootmap+$42);
   end;
   WriteByte($80,bootmap+$43);
   WriteByte($80,bootmap+$35F);
  end;
  if nzones>1 then //Write the Boot Block (multizone only)
  begin
   //Defect List
   Write32b($20000000,$C00); //Terminate the list
   //Partial Disc Record
   WriteByte($0A,$DC0); //log2secsize
   WriteByte(secspertrack,$DC1);
   WriteByte($02,$DC2); //heads
   WriteByte($04,$DC3); //Density
   WriteByte($0F,$DC4); //idlen
   WriteByte($06,$DC5); //log2bpmb
   WriteByte($01,$DC6); //skew
   WriteByte(nzones,$DC9);
   Write16b($0640,$DCA);//zone_spare
   if FDirType=1 then   //root
    Write32b($00000209,$DCC)
   else
    Write32b($00033801,$DCC);
   Write32b(disc_size,$DD0);
   if FDirType=2 then Write32b($00000001,$DEC); //format version (+)
   WriteByte(ByteCheckSum($C00,$200),$DFF);  //Checksum
   bootmap:=$C6800; //Middle of the disc
   //Zone 0 header
   Write16b($8238,bootmap+1); //FreeLink
   //ZoneCheck
   //Part of the full disc record
   WriteByte($04,bootmap+$07); //Density
   WriteByte($06,bootmap+$09); //log2bpmb
   Write16b($0640,bootmap+$0E);//zone_spare
   if FDirType=1 then          //root
    Write32b($00000209,bootmap+$10)
   else
    Write32b($00033801,bootmap+$10);
   //Allocation map - zone 0
   WriteByte($02,bootmap+$40);
   WriteByte($80,bootmap+$47);
   WriteByte($80,bootmap+$33B); //End of free area
   //Allocation map - zones 1 to 3
   for t:=1 to nzones-1 do
   begin
    //Zone header
    Write16b($8018,($400*t)+bootmap+$01); //FreeLink
    if t=2 then
    begin
     Write16b($80B8,$C7001);
     WriteByte($02,$C7004); //This marks the location and length of the root
     if FDirType=2 then
     begin
      WriteByte($80,$C7013); //(for big dirs, the root is separate)
      Write16b($0338,$C7014);
     end;
     WriteByte($80,$C7017); //which will point towards $C8800
    end;
    if t=3 then Write16b($0180,$C7717); //Marks the end of the disc.
    check:=$00;
    if t=3 then check:=$FF; //All four cross checks XOR together need to be $FF
    WriteByte(check,($400*t)+bootmap+$03); //CrossCheck
    WriteByte($80,($400*t)+bootmap+$33B); //End of free area
   end;
   // $33B is used as the end of the free area because:
   //zone_spare = $640 div 8 = $C8 + zone_header = $CC
   //secsize = ($400-1) - $CC = $33B
  end;
  mapsize:=secsize*nzones;
  root_size:=$800;
  //Full Disc Record and allocation map - all formats
  WriteByte($0A,bootmap+$04); //log2secsize
  WriteByte(secspertrack,bootmap+$05);
  WriteByte($02,bootmap+$06); //heads
  WriteByte($0F,bootmap+$08); //idlen
  WriteByte($01,bootmap+$0A); //skew
  WriteByte(nzones,bootmap+$0D);
  Write32b(disc_size,bootmap+$14);
  Write16b($4077,bootmap+$18);//disc id
  if FDirType=2 then // '+' only attributes
  begin
   Write32b($00000001,bootmap+$30);//format version
   Write32b(root_size,bootmap+$34);//root size
   Write32b($20158318,bootmap+$24);//disctype
  end
  else // non '+' only attributes
   Write32b($20158C78,bootmap+$24); //disctype
  //Zone checks for all zones
  for t:=0 to nzones-1 do
   WriteByte(GeneralChecksum(bootmap+$00+(t*secsize),secsize,secsize+4,$4,true),
             bootmap+(t*secsize)+$00);
  //Next create a copy of everything
  for t:=0 to mapsize-1 do
   WriteByte(ReadByte(bootmap+t),bootmap+mapsize+t);
  root:=bootmap+mapsize+mapsize;
 end;
 //Now write the root
 dirid:='$';
 att:='DLR';
 CreateADFSDirectory(dirid,dirid,att);
 //Finalise all the variables by reading the data in again
 Result:=ReadADFSDisc;
end;

{-------------------------------------------------------------------------------
Update title on ADFS image
-------------------------------------------------------------------------------}
function TDiscImage.UpdateADFSDiscTitle(title: String): Boolean;
var
 t: Integer;
begin
 //Remove any extraenous spaces
 RemoveSpaces(title);
 //And update the internal variable
 disc_name:=title;
 //Make sure it is not overlength
 title:=LeftStr(title,10);
 //Pad with zeros if underlength
 if Length(title)<10 then
  for t:=Length(title) to 10 do
   title:=title+' ';
 //Now update the map
 if not FMap then //Old Map
 begin
  for t:=1 to 10 do //Still need to make sure only 10 characters are saved
  begin
   //Part 1 at $F7
   if t mod 2=1 then
    WriteByte(Ord(title[t]),$F7+(t DIV 2));
   //Part 2 at $1F6
   if t mod 2=0 then
    WriteByte(Ord(title[t]),$1F6+((t-1)DIV 2));
  end;
  //Checksum on first sector
  WriteByte(ByteCheckSum($0000,$100),$0FF);
  //Checksum on second sector
  WriteByte(ByteCheckSum($0100,$100),$1FF);
 end;
 if FMap then //New Map
 begin
  //Disc name is held at $16 of the disc record, after $4 bytes zone header
  for t:=1 to 10 do
   WriteByte(Ord(title[t]),bootmap+$16+$4+(t-1));
  //Checksum for zone 0
  WriteByte(GeneralChecksum(bootmap+$00,secsize,secsize+4,$4,true),bootmap);
 end;
 //Return a success
 Result:=True;
end;

{-------------------------------------------------------------------------------
Update boot option on ADFS image
-------------------------------------------------------------------------------}
function TDiscImage.UpdateADFSBootOption(option: Byte): Boolean;
begin
 //Make sure it is only 0, 1, 2 or 3
 option:=option MOD 4;
 //Set the internal variable
 bootoption[0]:=option;
 //Now update the map
 if not FMap then //Old Map
 begin
  //Boot option is at $1FD
  WriteByte(option,$1FD);
  //Checksum
  WriteByte(ByteCheckSum($0100,$100),$1FF);
 end;
 if FMap then //New Map
 begin
  //Boot option is at offset $07 of the disc record, after the $4 zone header
  WriteByte(option,bootmap+$07+$04);
  //Checksum for zone 0
  WriteByte(GeneralChecksum(bootmap+$00,secsize,secsize+4,$4,true),bootmap);
 end;
 //Return a success
 Result:=True;
end;

{-------------------------------------------------------------------------------
Retrieve all the free space fragments on ADFS New Map
-------------------------------------------------------------------------------}
function TDiscImage.ADFSGetFreeFragments(offset:Boolean=True): TFragmentArray;
var
 zone,
 zonecounter,
 startzone,
 freelink,
 i,j          : Cardinal;
 fragments    : TFragmentArray;
begin
 Result:=nil;
 fragments:=nil;
 startzone:=nzones div 2; //Where to start looking
 // Start at the start zone to find a hole big enough for the file to fit
 for zonecounter:=0 to nzones-1 do
 begin
  zone:=(zonecounter+startzone)mod nzones;
  //i is the bit counter...number of bits from the first freelink
  //Get the first freelink of the zone and set our counter
  i:=ReadBits(bootmap+(zone*secsize)+1,0,15);
  //The header freelink is always 15 bits or less
  freelink:=i;
  if i<>0 then //If the freelink is zero, then there are no free spaces
  begin
   repeat
    SetLength(fragments,Length(fragments)+1);
    if offset then
    begin
     //Mark the offset - number of bits from the start of the bootmap
     fragments[Length(fragments)-1].Offset:=i+8+(zone*secsize*8);
     //But we need it as an absolute address in order to write the data
     fragments[Length(fragments)-1].Offset:=
                    ((fragments[Length(fragments)-1].Offset-$200)
                   -(zone_spare*zone))*bpmb;
    end
    else //Unless the flag calls for number of bits since last free space
     fragments[Length(fragments)-1].Offset:=freelink;
    //Make a note of the zone
    fragments[Length(fragments)-1].Zone  :=zone;
    //Read in the next free link number of bits - this can be idlen bits
    freelink:=ReadBits(bootmap+(zone*secsize)+1+(i DIV 8),i mod 8,idlen);
    //Now find the end of the fragment length
    j:=(i+idlen)-1; //Start after the freelink
    repeat
     inc(j); //Length counter
    //Continue until we find a set bit
    //Or run out of zone (the 32 is the 4 byte zone header)
    until(IsBitSet(ReadByte(bootmap+(zone*secsize)+1+(j div 8)),j mod 8))
       or(j>=32+(secsize*8-zone_spare));
    //Make a note of the length (the 1 is for the set bit marking the end)
    if offset then
     fragments[Length(fragments)-1].Length:=((j-i)+1)*bpmb
    else
     fragments[Length(fragments)-1].Length:=(j-i)+1;
    //Move the pointer on, after the '1'
    inc(i,freelink);
   until (freelink=0)                   //Unless the next link is zero,
      or (i>=32+(secsize*8-zone_spare));//or we run out of zone
  end;
 end;
 Result:=fragments;
end;

{-------------------------------------------------------------------------------
Write a file to ADFS image
-------------------------------------------------------------------------------}
function TDiscImage.WriteADFSFile(var file_details:TDirEntry;var buffer:TDIByteArray;
                    directory:Boolean=False;extend:Boolean=True): Integer;
//Set directory to TRUE to ensure it doesn't get fragmented (New Map)
//Set extend to FALSE to ensure that the ExtendDirectory is run (Big Directory)
var
 dir          : Integer;
 timestamp    : Int64;
 l,ref,ptr,
 freeptr,
 safilelen,
 dest,fragid,
 zone,i,j,
 freelink     : Cardinal;
 freelen      : Byte;
 success,
 spacefound   : Boolean;
 fragments,
 fsfragments  : TFragmentArray;
begin
 //Start with a negative result
 Result:=-3;//File already exists
 success:=False;
 //Validate the proposed filename
 if not((file_details.Filename='$')and(FDirType=2))then
  file_details.Filename:=ValidateADFSFilename(file_details.Filename);
 //First make sure it doesn't exist already
 if(not FileExists(file_details.Parent+dirsep+file_details.Filename,ref))
 or((file_details.Filename='$')and(FDirType=2))then
  //Get the directory where we are adding it to, and make sure it exists
  if (FileExists(file_details.Parent,ref)) OR (file_details.Parent='$')
  or ((file_details.Filename='$')and(FDirType=2)) then
  begin
   if file_details.filename<>'$' then
   begin
    file_details.ImageAddress:=0;
    //Where we are inserting this into
    if file_details.Parent='$' then
     dir  :=0
    else
     dir  :=FDisc[ref div $10000].Entries[ref mod $10000].DirRef;
    //Big Dir - Verify directory is big enough or if it needs extending and moved.
    if(FDirType=2)and(extend)then //This will extend/contract the directory
     if not ExtendADFSBigDir(dir,Length(file_details.Filename),True) then
     begin
      Result:=-9; //Cannot extend
      exit;
     end;
    //Get the length of the file
    l:=Length(FDisc[dir].Entries);
    //Work out the "sector aligned file length"
    safilelen:=file_details.Length div secsize;
    if file_details.Length mod secsize>0 then inc(safilelen);
    safilelen:=safilelen*secsize;
   end else safilelen:=file_details.Length; //New length of the root
   Result:=-4;//Catalogue full
   //Make sure it will actually fit on the disc
   if free_space>=safilelen then
    //And make sure we can extend the catalogue
    if((FDirType=0)and(l<47)
    OR (FDirType=1)and(l<77)
    OR (FDirType=2)) then
    begin
     Result:=-7; //Map full
     //Set some flags
     spacefound:=False;
     success:=False;
     dest:=disc_size;
     //Find a big enough space
     if not FMap then //Old map
     begin
      ptr:=ReadByte($1FE); //Number of free space entries
      freeptr:=0;
      while(freeptr<ptr)
        and(Read24b($100+freeptr)*$100<safilelen)do
       inc(freeptr,3);
      if freeptr<ptr then //Space found
      begin
       //Fill in the details
       file_details.Sector:=Read24b($000+freeptr); //To return back
       SetLength(fragments,1); //Only the single fragment
       fragments[0].Offset:=file_details.Sector*$100; //Fragment details
       fragments[0].Length:=file_details.Length; //Fragment details
       spacefound:=True; //We found somewhere
      end;
     end;
     //New map
     if FMap then
     begin
      //Get the free space fragments
      fsfragments:=ADFSGetFreeFragments;
      if Length(fsfragments)>0 then
      begin
       //Go through them to find a big enough fragment
       i:=0;
       repeat
        if fsfragments[i].Length>=file_details.Length then
        begin
         //Found one, so set our flag
         spacefound:=True;
         //And add it to the fragment list (which will be one)
         SetLength(fragments,1);
         fragments[0]:=fsfragments[i];
        end;
        inc(i);
        //Continue for all fragments or we found somewhere
       until (i=Length(fsfragments)) or (spacefound);
       //None were big enough, so we'll need to fragment the file
       if(not spacefound)and(not directory)then //Only if not a directory
       begin
        //Go through the fragments again
        i:=0;
        //This time add up the lengths
        freelink:=0;
        repeat
         //Copy each fragment into our list
         SetLength(fragments,Length(fragments)+1);
         fragments[Length(fragments)-1]:=fsfragments[i];
         //Add the length to the total
         inc(freelink,fragments[Length(fragments)-1].Length);
         //If we have enough, set the flag
         if freelink>=file_details.Length then spacefound:=True;
         //Next free fragment
         inc(i);
        until (i=Length(fsfragments)) or (spacefound);
       end;
      end;
      if Length(fragments)>0 then
      begin
       // Now scan the zone again and find a unique ID
       zone:=fragments[0].Zone; //Zone of the first fragment
       // IDs start here for this zone
       fragid:=zone*(((secsize*8)-zone_spare)div(idlen+1));
       if zone=0 then j:=3 else j:=0; //Highest used ID in this zone
       //Check each fragment ID until we find one that doesn't exist
       while Length(NewDiscAddrToOffset((fragid+j)*$100))>0 do
        inc(j);
       inc(fragid,j); //We'll use this one
      end;
     end;
     //Write the file to the area
     if(spacefound)and(Length(fragments)>0)then
     begin
      Result:=-5;//Unknown error
      //Counter into the data
      ptr:=0;
      //Set to true for now, if one of the writes fails, it will remain at False
      success:=True;
      //Go through the fragments and write them
      for ref:=0 to Length(fragments)-1 do
      begin
       //Amount of data to write for this fragment
       if fragments[ref].Length>Length(buffer)-ptr then
        j:=Length(buffer)-ptr
       else
        j:=fragments[ref].Length;
       //Write the data, return a true or false, ANDed with past results
       success:=success AND WriteDiscData(fragments[ref].Offset,0,
                                          buffer,
                                          j,
                                          ptr);
       inc(ptr,fragments[ref].Length); //Next fragment of data
      end;
      //Set this marker to the start of the first offset
      dest:=fragments[0].Offset;
     end;
     //If it wrote OK, then continue
     if success then
     begin
      //Update the 'parent' address for the root
      if file_details.Filename='$' then
      begin
       //Into the directory header
       Write32b(fragid*$100,dest+$18);
       //Into the bootmap in zone 0
       Write32b(fragid*$100,bootmap+$10);
       //Into the disc record for multi-zone discs
       if nzones>1 then Write32b(fragid*$100,$DCC);
       //Update our root address
       root:=dest;
      end;
      //Update the checksum, if it is a directory
      if(Pos('D',file_details.Attributes)>0)or(file_details.filename='$')then
       if FDirType>0 then //New/Big Directory (Old Dir has zero for checksum)
        WriteByte(CalculateADFSDirCheck(dest),dest+(file_details.Length-1));
      //Now update the free space map
      if not FMap then //Old map
      begin
       ref:=safilelen div $100;
       if Read24b($100+freeptr)>ref then
       begin
        //Still enough space ahead, so just reduce the FreeLen
        Write24b(Read24b($100+freeptr)-ref,$100+freeptr);
        //And update the FreeStart
        Write24b(Read24b($000+freeptr)+ref,$000+freeptr);
       end
       else //Otherwise, move all the remaining pointers down one
       begin
        //Move to the next pointer
        inc(freeptr,3);
        //Move them all down one position
        while freeptr<ptr do
        begin
         Write24b(Read24b($000+freeptr),$000+freeptr-3);
         Write24b(Read24b($100+freeptr),$100+freeptr-3);
         inc(freeptr,3);
        end;
        //Then decrease the FreeEnd
        dec(ptr,3);
        //and write back
        Write24b(ptr,$1FE);
       end;
       //Update the checksums
       WriteByte(ByteCheckSum($0000,$100),$0FF);
       WriteByte(ByteCheckSum($0100,$100),$1FF);
      end;
      if FMap then //New map
      begin
       //Update the file details to indicate which fragment it is
       file_details.Sector:=fragid*$100; //take account of the sector offset
       //Recalculate the file length to be bpmb-aligned
       safilelen:=(file_details.Length div (bpmb*8))*(bpmb*8);
       if file_details.Length mod (bpmb*8)>0 then inc(safilelen,bpmb*8);
       //and at least bigger than bpmb*(idlen+1)
       if safilelen<(idlen+1)*bpmb then safilelen:=(idlen+1)*bpmb;
       //For each fragment:
       for i:=0 to Length(fragments)-1 do
       begin
        //Scan the zone, from the beginning, until we find a pointer to our fragment
        zone:=fragments[i].Zone; //Zone
        freeptr:=(1+zone*secsize)*8;//Pointer to the next freelink
        repeat
         //Next freelink
         if freeptr=8 then freelen:=15 else freelen:=idlen;
         freelink:=ReadBits(bootmap+(freeptr DIV 8),
                            freeptr MOD 8,freelen);
         //move the pointer on
         inc(freeptr,freelink);
         //we need it as an absolute address
         ref:=((freeptr-$200)-zone_spare*zone)*bpmb;
         //Continue until it points to our fragment
        until ref=fragments[i].Offset;
        //Make a note of the length of data we are laying down
        if fragments[i].Length<=safilelen then
         j:=fragments[i].Length
        else
         j:=safilelen;
        //And reduce the total accordingly
        dec(safilelen,j);
        //Length of fragment/file size in bits
        ref:=j DIV bpmb;
        //Get the freelink of where we are at
        if freeptr=8 then freelen:=15 else freelen:=idlen;
        ptr:=ReadBits(bootmap+(freeptr DIV 8),freeptr MOD 8,freelen);
        //If the data we are laying down is less than the space available
        if j<>fragments[i].Length then
        begin
         //Adjust the previous freelink to point to after this fragment
         if freeptr-freelink=8 then freelen:=15 else freelen:=idlen;
         WriteBits(freelink+ref,
                   bootmap+(freeptr-freelink)DIV 8,(freeptr-freelink)MOD 8,
                   freelen);
         //Reduce the freelink for this fragment by the above length and move afterwards
         //If it is not zero
         if freeptr+ref=8 then freelen:=15 else freelen:=idlen;
         if ptr<>0 then
          if ReadBits(bootmap+(freeptr+ref)DIV 8,(freeptr+ref)MOD 8,freelen)=0 then
           WriteBits(ptr-ref,bootmap+(freeptr+ref)DIV 8,(freeptr+ref)MOD 8,freelen);
        end
        else
        //If the data we are laying down fits exactly into the space
        begin
         //Take this pointer and add it to the previous pointer
         if freeptr-freelink=8 then freelen:=15 else freelen:=idlen;
         //Unless this pointer is zero, then the previous will need to be zero
         if ptr=0 then
          WriteBits(0,
                    bootmap+(freeptr-freelink)DIV 8,(freeptr-freelink)MOD 8,
                    freelen)
         else
          WriteBits(freelink+ptr,bootmap+(freeptr-freelink)DIV 8,
                   (freeptr-freelink)MOD 8,
                   freelen);
        end;
        //Put the new fragid in and terminate with a set bit for the length
        WriteBits(fragid,bootmap+freeptr DIV 8,freeptr MOD 8,idlen);
        dec(ref);//We're setting this bit
        WriteBits(1,bootmap+(freeptr+ref)DIV 8,(freeptr+ref)MOD 8,1);
       end;
       for zone:=0 to nzones-1 do
       begin
        //Ensure the top bit is set on the first link for each zone
        WriteByte(ReadByte(bootmap+2+zone*secsize)OR$80,bootmap+2+zone*secsize);
        //Zone checks
        WriteByte(
              GeneralChecksum(bootmap+$00+(zone*secsize),
                              secsize,
                              secsize+4,
                              $4,
                              true),
                  bootmap+(zone*secsize)+$00);
       end;
       //Make a copy
       for ptr:=0 to (nzones*secsize)-1 do
        WriteByte(ReadByte(bootmap+ptr),bootmap+ptr+nzones*secsize);
      end;
      //Now update the directory (local copy)
      if file_details.filename<>'$' then
      begin
       ref:=Length(FDisc[dir].Entries);
       ptr:=ExtendADFSCat(dir,file_details);
       fragid:=Length(FDisc[dir].Entries);
       //Not a directory
       FDisc[dir].Entries[ptr].DirRef:=-1;
       //Filetype and Timestamp for Arthur and RISC OS ADFS
       if (FDisc[dir].Entries[ptr].LoadAddr=0)
       and(FDisc[dir].Entries[ptr].ExecAddr=0)
       and(FDirType>0) then //New and Big directories
       begin
        FDisc[dir].Entries[ptr].LoadAddr:=$FFF00000;
        //Set the filetype, if not already set
        if FDisc[dir].Entries[ptr].ShortFileType<>'' then
        begin
         FDisc[dir].Entries[ptr].LoadAddr:=FDisc[dir].Entries[ptr].LoadAddr OR
            (StrToIntDef('$'+FDisc[dir].Entries[ptr].ShortFileType,0)<<8);
         for ref:=1 to Length(FileTypes) do
          if LeftStr(FileTypes[ref],3)=UpperCase(FDisc[dir].Entries[ptr].ShortFileType) then
           FDisc[dir].Entries[ptr].FileType:=Copy(FileTypes[ref],4);
        end;
        //Timestamp it, if not already done
        timestamp:=TimeDateToRISCOS(Now);
        FDisc[dir].Entries[ptr].TimeStamp:=Now;
        FDisc[dir].Entries[ptr].LoadAddr:=FDisc[dir].Entries[ptr].LoadAddr OR
            (timestamp DIV $100000000);
        FDisc[dir].Entries[ptr].ExecAddr:=timestamp MOD $100000000;
       end;
       //Is the file actually a directory?
       if Pos('D',file_details.Attributes)>0 then
       begin
        //Increase the number of directories by one
        SetLength(FDisc,Length(FDisc)+1);
        //Then assign DirRef
        FDisc[dir].Entries[ptr].DirRef:=Length(FDisc)-1;
        //Assign the directory properties
        FDisc[Length(FDisc)-1].Directory:=FDisc[dir].Entries[ptr].Filename;
        FDisc[Length(FDisc)-1].Title    :=FDisc[dir].Entries[ptr].Filename;
        FDisc[Length(FDisc)-1].Broken   :=False;
        SetLength(FDisc[Length(FDisc)-1].Entries,0);
       end;
       //And send the result back to the client
       Result:=ptr;
      end else Result:=0;
      //Update the catalogue
      UpdateADFSCat(file_details.Parent);
      //Update the free space map
      ADFSFreeSpaceMap;
     end
     else //Did not write OK
      if(FDirType=2)and(extend)then
       //Contract the directory, if needed
       if ExtendADFSBigDir(dir,0,False) then
       begin
        //Update the catalogue
        UpdateADFSCat(file_details.Parent);
        //Update the free space map
        ADFSFreeSpaceMap;
       end;
    end;
  end else Result:=-6; //Directory does not exist
end;

{-------------------------------------------------------------------------------
Create a directory
-------------------------------------------------------------------------------}
function TDiscImage.CreateADFSDirectory(var dirname,parent,attributes: String): Integer;
var
 dirid     : String;
 t         : Integer;
 c         : Byte;
 dir,
 entry,
 ref,
 dirtail,
 parentaddr: Cardinal;
 buffer    : TDIByteArray;
 fileentry : TDirEntry;
begin
 Result:=-3;//Directory already exists
 if (dirname='$') OR (parent='$') then //Creating the root
  parentaddr:=root DIV $100
 else
 begin
  FileExists(parent,ref);
  dir  :=ref DIV $10000;
  entry:=ref MOD $10000;
  parentaddr:=FDisc[dir].Entries[entry].Sector;
 end;
 if (dirname<>'$') then
  //Validate the name
  dirname:=ValidateADFSFilename(dirname);
 //Make sure it does not already exist
 if (not FileExists(parent+dirsep+dirname,ref)) OR (dirname='$') then
 begin
  Result:=-5;//Unknown error
  //Set as 'D' so it gets added as a directory
  if Pos('D',attributes)=0 then attributes:='D'+attributes;
  if FDirType=0 then //Old Directory
  begin
   dirtail:=$4CB;
   SetLength(buffer,$500);
   for t:=0 to Length(buffer)-1 do buffer[t]:=$00;
   //Directory identifier
   dirid:='Hugo';
   for t:=1 to 4 do
   begin
    buffer[t]            :=Ord(dirid[t]);
    buffer[dirtail+$2F+t]:=Ord(dirid[t]);
   end;
   //Directory Name
   for t:=1 to 10 do
   begin
    c:=0;
    if t-1<Length(dirname) then c:=Ord(dirname[t]);
    buffer[dirtail+t]:=c;
   end;
   //Directory Parent sector
   buffer[dirtail+$0B]:= parentaddr MOD $100;
   buffer[dirtail+$0C]:=(parentaddr DIV $100)MOD$100;
   buffer[dirtail+$0D]:=(parentaddr DIV $10000)MOD$100;
   buffer[dirtail+$0E]:=(parentaddr DIV $1000000)MOD$100;
   //Directory Title
   for t:=1 to 19 do
   begin
    c:=0;
    if t-1<Length(dirname) then c:=Ord(dirname[t]);
    buffer[dirtail+$0D+t]:=c;
   end;
  end;
  if FDirType=1 then //New Directory
  begin
   dirtail:=$7D7;
   SetLength(buffer,$800);
   for t:=0 to Length(buffer)-1 do buffer[t]:=$00;
   dirid:='Nick';
   //Directory identifier
   for t:=1 to 4 do
   begin
    buffer[t]            :=Ord(dirid[t]);
    buffer[dirtail+$23+t]:=Ord(dirid[t]);
   end;
   //Directory Name
   for t:=1 to 10 do
   begin
    c:=0;
    if t-1<Length(dirname) then c:=Ord(dirname[t]);
    buffer[dirtail+$18+t]:=c;
   end;
   //Directory Parent sector
   buffer[dirtail+$03]:= parentaddr MOD $100;
   buffer[dirtail+$04]:=(parentaddr DIV $100)MOD$100;
   buffer[dirtail+$05]:=(parentaddr DIV $10000)MOD$100;
   buffer[dirtail+$06]:=(parentaddr DIV $1000000)MOD$100;
   //Directory Title
   for t:=1 to 19 do
   begin
    c:=0;
    if t-1<Length(dirname) then c:=Ord(dirname[t]);
    buffer[dirtail+$05+t]:=c;
   end;
  end;
  if FDirType=2 then //Big Directory
  begin
   dirtail:=$7F8;
   SetLength(buffer,$800);
   for t:=0 to Length(buffer)-1 do buffer[t]:=$00;
   //Directory identifier - start
   dirid:='SBPr';
   for t:=1 to 4 do
    buffer[$03+t]:=Ord(dirid[t]);
   //Directory identifier - end
   dirid:='oven';
   for t:=1 to 4 do
    buffer[dirtail+t-1]:=Ord(dirid[t]);
   //BigDirNameLen
   buffer[$08]:= Length(dirname) mod $100;
   buffer[$09]:=(Length(dirname)DIV$100)mod $100;
   buffer[$0A]:=(Length(dirname)DIV$10000)mod $100;
   buffer[$0C]:=(Length(dirname)DIV$1000000)mod $100;
   //BigDirSize
   buffer[$0C]:=$00;
   buffer[$0D]:=$08;
   buffer[$0E]:=$00;
   buffer[$0F]:=$00;
   //BigDirParent
   buffer[$18]:=ReadByte(bootmap+$10);
   buffer[$19]:=ReadByte(bootmap+$11);
   buffer[$1A]:=ReadByte(bootmap+$12);
   buffer[$1B]:=ReadByte(bootmap+$13);
   //BigDirName
   for t:=1 to Length(dirname) do
    buffer[$1B+t]:=Ord(dirname[t]);
   buffer[$1B+Length(dirname)+1]:=$0D; //CR for end of directory name
  end;
  //Write the directory
  if dirname='$' then //Root - used when formatting an image
  begin
   for t:=0 to Length(buffer)-1 do
    WriteByte(buffer[t],root+t);
   //Directory Checkbyte
   WriteByte(CalculateADFSDirCheck(root),root+(root_size-1));
   Result:=0;
  end
  else //Other directories
  begin
   fileentry.Filename  :=dirname;
   fileentry.ExecAddr  :=$00000000;
   fileentry.LoadAddr  :=$00000000;
   fileentry.Length    :=Length(buffer);
   fileentry.Attributes:=attributes;
   fileentry.Parent    :=parent;
   //We can now deal with this like a normal file
   Result:=WriteADFSFile(fileentry,buffer,True);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update a directory catalogue
-------------------------------------------------------------------------------}
procedure TDiscImage.UpdateADFSCat(directory: String);
var
 dir,
 diraddr,
 ref,i,
 head,
 heapctr,
 tail      : Cardinal;
 c,a       : Byte;
 temp      : String;
 fragments : TFragmentArray;
const
 oldattr = 'RWLDErweP'+#00;
 newattr = 'RWLDrw';
begin
 if (FileExists(directory,ref)) or (directory='$') then
 begin
  //Get the directory reference and sector address
  if directory='$' then
  begin
   dir  :=0;
   diraddr:=root;
  end
  else
  begin
   dir  :=FDisc[ref div $10000].Entries[ref mod $10000].DirRef;
   diraddr:=FDisc[ref div $10000].Entries[ref mod $10000].Sector;
  end;
  //Make the sector address a disc address
  if not FMap then
   diraddr:=diraddr*$100 //Old map
  else //New map
  if directory<>'$' then //But not root, as this is given as direct
   begin
    fragments:=NewDiscAddrToOffset(diraddr);
    //Directories don't get fragmented
    diraddr:=fragments[0].Offset;
   end;
  //Update the directory title
  temp:=FDisc[dir].Title+#$0D;
  for i:=0 to 18 do
  begin
   //Padded with nulls
   c:=$00;
   if i<Length(temp) then c:=Ord(temp[i+1]);//AND$7F;
   //Write the byte - Old and New only
   if FDirType=0 then WriteByte(c AND$7F,diraddr+$4CB+$0E+i);
   if FDirType=1 then WriteByte(c,diraddr+$7D7+$06+i);
  end;
  //Clear the directory
  c:=0;
  if FDirType=0 then c:=47;
  if FDirType=1 then c:=77;
  if c>0 then //Old and New type only
   for i:=0 to c-1 do
    for ref:=0 to $19 do WriteByte($00,diraddr+$05+ref+i*$1A);
  if FDirType=2 then //Big type
  begin
   //Get the size of the header, with padding
   head:=Read32b(diraddr+$08)+$1C+1; //Size
   head:=((head+3)div 4)*4;//Pad to word boundary
   //Get the size of the directory, minus tail
   tail:=Read32b(diraddr+$0C)-8;
   //Now clear the directory entries
   for i:=head to tail-head do //Tail is 8 bytes
    WriteByte($00,diraddr+i);
  end;
  //Heap pointer for Big Directories
  heapctr:=0;
  if FDirType=2 then //Write the number of entries for big directory
   Write32b(Length(FDisc[dir].Entries),diraddr+$10);
  //Go through each entry and add it
  if Length(FDisc[dir].Entries)>0 then
   for ref:=0 to Length(FDisc[dir].Entries)-1 do
   begin
    //Filename - needs terminated with CR
    temp:=FDisc[dir].Entries[ref].Filename+#$0D;
    //Attributes (used by New and Big)
    a:=0;
    for i:=0 to 5 do
     if Pos(newattr[i+1],FDisc[dir].Entries[ref].Attributes)>0 then
      inc(a,1 shl i);
    if FDirType<2 then //Old and New only
    begin
     for i:=0 to 9 do
     begin
      //Padded with nulls
      c:=$00;
      if i<Length(temp) then c:=Ord(temp[i+1]);//AND$7F;
      //Add the attributes
      if FDirType=0 then //Old directory only
       if Pos(oldattr[i+1],FDisc[dir].Entries[ref].Attributes)>0 then
        c:=c+$80;
      //Write the byte
      WriteByte(c,diraddr+$05+i+ref*$1A);
     end;
     //Load Address
     Write32b(FDisc[dir].Entries[ref].LoadAddr,diraddr+$05+$0A+ref*$1A);
     //Execution Address
     Write32b(FDisc[dir].Entries[ref].ExecAddr,diraddr+$05+$0E+ref*$1A);
     //Length
     Write32b(FDisc[dir].Entries[ref].Length  ,diraddr+$05+$12+ref*$1A);
     //Sector
     Write24b(FDisc[dir].Entries[ref].Sector  ,diraddr+$05+$16+ref*$1A);
     if FDirType=0 then
      //This is what appears in brackets after the file - old directory
      WriteByte(00                             ,diraddr+$05+$19+ref*$1A)
     else //New directory - attributes
      WriteByte(a                              ,diraddr+$05+$19+ref*$1A);
    end;
    if FDirType=2 then //Big only
    begin
     //Load Address
     Write32b(FDisc[dir].Entries[ref].LoadAddr        ,diraddr+head+$00+ref*$1C);
     //Execution Address
     Write32b(FDisc[dir].Entries[ref].ExecAddr        ,diraddr+head+$04+ref*$1C);
     //Length
     Write32b(FDisc[dir].Entries[ref].Length          ,diraddr+head+$08+ref*$1C);
     //Sector
     Write24b(FDisc[dir].Entries[ref].Sector          ,diraddr+head+$0C+ref*$1C);
     //Tail copy of the sector
     Write32b(FDisc[dir].Entries[ref].Sector,
                            (diraddr+tail)-(Length(FDisc[dir].Entries)*4)+ref*4);
     //Attributes
     Write24b(a                                       ,diraddr+head+$10+ref*$1C);
     //Length of filename (not including CR)
     Write24b(Length(temp)-1                          ,diraddr+head+$14+ref*$1C);
     //Where to find the filename
     Write24b(heapctr                                 ,diraddr+head+$18+ref*$1C);
     //Write the filename
     for i:=0 to (((Length(temp)+3)div 4)*4)-1 do
     begin
      c:=0; //Padded with nulls to word boundary
      if i<Length(temp) then c:=Ord(temp[i+1]);//AND$7F;
      if c=32 then c:=c OR $80;
      //dir address+header+length of all entries+counter into heap+character
      WriteByte(c,diraddr+head+heapctr+i+Length(FDisc[dir].Entries)*$1C);
     end;
     //Move the heap counter on
     inc(heapctr,((Length(temp)+3)div 4)*4);
    end;
   end;
  If FDirType=2 then
   Write32b(heapctr,diraddr+$14); //BigDirNamesSize
  //Update the checksum
  if FDirType=0 then //Old - can be zero
   WriteByte($00,diraddr+$4FF);
  if FDirType=1 then //New
   WriteByte(CalculateADFSDirCheck(diraddr),diraddr+$7FF);
  if FDirType=2 then //Big
   WriteByte(CalculateADFSDirCheck(diraddr),diraddr+(Read32b(diraddr+$0C)-1));
 end;
end;

{-------------------------------------------------------------------------------
Update attributes on a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateADFSFileAttributes(filename,attributes: String): Boolean;
var
 ptr,
 dir,
 entry : Cardinal;
begin
 Result:=False;
 //Make sure that the file exists, but also to get the pointer
 if FileExists(filename,ptr) then
 begin
  //FileExists returns a pointer to the file
  entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=ptr div $10000;  //Top 16 bits - directory reference
  //Change the attributes on the local copy
  FDisc[dir].Entries[entry].Attributes:=attributes;
  //Then update the catalogue
  UpdateADFSCat(FDisc[dir].Entries[entry].Parent);
  //And return a success
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Validate a filename
-------------------------------------------------------------------------------}
function TDiscImage.ValidateADFSFilename(filename: String): String;
var
 i: Integer;
const
  illegalOld = '#* .:$&@';
  illegalBig = '$&%@\^:.#*"|';
begin
  for i:=1 to Length(filename) do
 begin
  //Remove top-bit set characters
  if FDirType=0 then filename[i]:=chr(ord(filename[i])AND$7F);
  //and remove control codes
  if ord(filename[i])<32 then
   filename[i]:=chr(ord(filename[i])+32);
 end;
 //Is it not too long
 if FDirType<2 then
  filename:=Copy(filename,1,10);
 //Remove any forbidden characters
 for i:=1 to Length(filename) do
  if (FDirType<2) AND (Pos(filename[i],illegalOld)>0)
  OR (FDirType=2) AND (Pos(filename[i],illegalBig)>0)then filename[i]:='_';
 Result:=filename;
end;

{-------------------------------------------------------------------------------
Retitle an ADFS directory
-------------------------------------------------------------------------------}
function TDiscImage.RetitleADFSDirectory(filename,newtitle: String): Boolean;
var
 ptr,
 entry,
 dir    : Cardinal;
 c,i    : Byte;
 temp   : String;
begin
 Result:=False;                           
 //ADFS Big Directories do not have titles
 if FDirType=2 then exit;
 //Check that the file exists
 if (FileExists(filename,ptr)) OR (filename='$') then
 begin
  if filename='$' then
  begin
   ptr:=root;
   FDisc[0].Title:=LeftStr(newtitle,19);
  end
  else
  begin
   //FileExists returns a pointer to the file
   entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
   dir  :=ptr div $10000;  //Top 16 bits - directory reference
   //Re-title the directory, limiting it to 19 characters
   FDisc[FDisc[dir].Entries[entry].DirRef].Title:=LeftStr(newtitle,19);
   //Now need to update the catalogue for this directory
   ptr:=FDisc[dir].Entries[entry].Sector;
  end;
  if not FMap then ptr:=ptr*$100;
  //Update the title
  temp:=newtitle+#$0D;
  for i:=0 to 18 do
  begin
   //Padded with null
   c:=0;
   if i<Length(temp) then c:=Ord(temp[i+1]);//AND$7F;
   //Write the byte
   if FDirType=0 then WriteByte(c AND$7F,ptr+$4CB+$0E+i);
   if FDirType=1 then WriteByte(c,ptr+$7D7+$06+i);
  end;
  //Calculate the new checksum for New Directory
  if FDirType=1 then
   WriteByte(CalculateADFSDirCheck(ptr),ptr+$7FF);
  //Update the catalogue
  if filename='$' then
   UpdateADFSCat('$')
  else
   UpdateADFSCat(FDisc[dir].Entries[entry].Parent);
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Rename an ADFS file/directory
-------------------------------------------------------------------------------}
function TDiscImage.RenameADFSFile(oldfilename: String;var newfilename: String):Integer;
var
 ptr,
 entry,
 dir    : Cardinal;
 frag   : TFragmentArray;
 i,
 space  : Integer;
 c      : Byte;
 temp   : String;
 swap   : TDirEntry;
 changed: Boolean;
begin
 Result:=-2; //File does not exist
 //Check that the new name meets the required ADFS filename specs
 newfilename:=ValidateADFSFilename(newfilename);
 //Check that the file exists
 if FileExists(oldfilename,ptr) then
 begin
  Result:=-3; //New name already exists
  //FileExists returns a pointer to the file
  entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=ptr div $10000;  //Top 16 bits - directory reference
  //Make sure the new filename does not already exist
  if not FileExists(FDisc[dir].Entries[entry].Parent+dirsep+newfilename,ptr) then
  begin
   Result:=-1;//Unknown error
   //Get the difference in lengths
   space:=Length(FDisc[dir].Entries[entry].Filename)-Length(newfilename);
   //Big Dir - Verify directory is big enough or if it needs extending and moved.
   if FDirType=2 then
    if not ExtendADFSBigDir(dir,space,False) then
    begin
     Result:=-9; //Cannot extend
     exit;
    end;
   //Are we renaming a directory?
   if FDisc[dir].Entries[entry].DirRef<>-1 then
   begin                                                             
    //If the title is the same, change it also
    if FDisc[FDisc[dir].Entries[entry].DirRef].Title=FDisc[dir].Entries[entry].Filename then
     FDisc[FDisc[dir].Entries[entry].DirRef].Title:=newfilename;
    //Now need to update the catalogue for this directory
    ptr:=FDisc[dir].Entries[entry].Sector;
    if not FMap then ptr:=ptr*$100
    else
    begin
     frag:=NewDiscAddrToOffset(ptr);
     ptr:=frag[0].Offset;
    end;
    //Update the title (Big Directories do not have a title)
    temp:=FDisc[FDisc[dir].Entries[entry].DirRef].Title+#$0D;
    for i:=0 to 18 do
    begin
     //Padded with nul
     c:=0;
     if i<Length(temp) then c:=Ord(temp[i+1]);//AND$7F;
     //Write the byte
     if FDirType=0 then WriteByte(c AND$7F,ptr+$4CB+$0E+i);
     if FDirType=1 then WriteByte(c,ptr+$7D7+$06+i);
    end;
    //Update the name
    temp:=newfilename+#$0D;
    for i:=0 to 9 do
    begin
     //Padded with nul
     c:=0;
     if i<Length(temp) then c:=Ord(temp[i+1]);//AND$7F;
     //Write the byte
     if FDirType=0 then WriteByte(c AND$7F,ptr+$4CB+$01+i);
     if FDirType=1 then WriteByte(c,ptr+$7D7+$19+i);
    end;
    //Calculate the new checksum for New Directory
    if FDirType=1 then
     WriteByte(CalculateADFSDirCheck(ptr),ptr+$7FF);
   end;
   //Change the entry
   FDisc[dir].Entries[entry].Filename:=newfilename;
   //Sort the entries into order
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
   //Update the catalogue
   UpdateADFSCat(FDisc[dir].Entries[entry].Parent);
   Result:=entry;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Consolodate an ADFS Old Map Free space map
-------------------------------------------------------------------------------}
procedure TDiscImage.ConsolodateADFSFreeSpaceMap;
var
 i,j,p,
 start,
 len       : Integer;
 fslinks   : TFragmentArray;
 FreeEnd,
 linklen   : Byte;
 FreeStart,
 FreeLen   : array[0..81] of Integer;
 changed   : Boolean;
begin
 if not FMap then //Old map only
 begin
  //Changed flag
  changed:=False;
  repeat
   //First, lets clear the array
   for i:=0 to Length(FreeLen)-1 do
   begin
    FreeStart[i]:=0;
    FreeLen  [i]:=0;
   end;
   //Get the number of free areas
   FreeEnd:=ReadByte($1FE);
   //Initialise some variables
   i:=3; //This is pointer into our free space map copy
   p:=1; //This is counter for number of entries in our map copy
   //Read the first entry
   FreeStart[0]:=Read24b($000);
   FreeLen[0]  :=Read24b($100);
   //Loop to read each successive entry
   while i<FreeEnd do
   begin
    //Next entry
    start:=Read24b($000+i);
    len  :=Read24b($100+i);
    //See if there are any contiguous areas
    for j:=0 to i div 3 do
    begin
     //The start of this will match the end of another
     if FreeStart[j]+FreeLen[j]=start then
     begin //If it does, then just increase the length and discard
      inc(FreeLen[j],len);
      start:=0;
      len  :=0;
     end;
     //This end will match the start of another
     if start+len=FreeStart[j] then
     begin //If it does, then just change the start, increase the length & discard
      FreeStart[j]:=start;
      inc(FreeLen[j],len);
      start:=0;
      len  :=0;
     end;
    end;
    //Otherwise, save a copy of it, and increase our counter
    if start+len>0 then
    begin
     FreeStart[p]:=start;
     FreeLen[p]:=len;
     inc(p);
    end;
    //Move to the next entry, if there is one
    inc(i,3);
   end;
   //Fill the free space with our copy, and clear any others
   for i:=0 to 81 do
   begin
    Write24b(FreeStart[i],$000+i*3);
    Write24b(FreeLen[i],$100+i*3);
   end;
   //And write the counter in
   WriteByte(p*3,$1FE);
   //Marks as changed
   changed:=True;
  until FreeEnd=p*3; //Go back and do it again, if things have changed
  if changed then
  begin
   //Run the checksums and we're done
   WriteByte(ByteCheckSum($0000,$100),$0FF);
   WriteByte(ByteCheckSum($0100,$100),$1FF);
  end;
 end;
 if FMap then //New Map
 begin
  //Flag to mark if anything changed
  changed:=False;
  //Get the list of free space links
  fslinks:=ADFSGetFreeFragments(False);
  p:=0; //Zone offset counter
  i:=0; //Freelink counter
  while i<Length(fslinks)-1 do
  begin
   //Zone's match?
   if fslinks[i].Zone=fslinks[i+1].Zone then
   begin
    //Does the current fragment's length equal the next fragment's offset?
    if fslinks[i].Length=fslinks[i+1].Offset then
    begin
     //Work out the disc address for where this fragment is
     start:=bootmap+1+(fslinks[i].Zone*secsize);
     //And what the offset should be
     len:=fslinks[i+1].Offset;
     //We need to point the length ahead of this
     if i<Length(fslinks)-2 then
     begin
      if fslinks[i+2].Zone=fslinks[i].Zone then inc(len,fslinks[i+2].Offset);
     //If these are the last two of the zone, then reset the new offset to zero
      if fslinks[i+2].Zone<>fslinks[i].Zone then len:=0;
     end;
     //Ditto
     if i=Length(fslinks)-2 then len:=0;
     //Write the new offset
     if p+fslinks[i].Offset=0 then linklen:=15 else linklen:=idlen;
     WriteBits(len,start+((p+fslinks[i].Offset)DIV 8),(p+fslinks[i].Offset)MOD 8,linklen);
     //Work out where the old fragment end pointer is and blank it
     WriteBits(0,start+(((p+fslinks[i].Offset+fslinks[i].Length)-1)div 8),
                 ((p+fslinks[i].Offset+fslinks[i].Length)-1)MOD 8,1);
     //And blank the old fragment start pointer
     WriteBits(0,start+((p+fslinks[i].Offset+fslinks[i].Length)div 8),
                 (p+fslinks[i].Offset+fslinks[i].Length)MOD 8,idlen);
     //Refresh the array
     fslinks:=ADFSGetFreeFragments(False);
     //Set our changed flag
     changed:=True;
     //And start again
     p:=0;
     i:=0;
    end else
    begin
     //Increase the zone offset counter and the counter into the array
     inc(p,fslinks[i].Offset);
     inc(i);
    end;
   end else
   begin
    //No zone match, so reset the zone offset counter and move onto the next fragment
    p:=0;
    inc(i);
   end;
  end;
  if changed then
  begin
   //Update the checksums
   for i:=0 to nzones-1 do
   begin
    //Ensure the top bit is set on the first link for each zone
    WriteByte(ReadByte(bootmap+2+i*secsize)OR$80,bootmap+2+i*secsize);
    //Zone checks
    WriteByte(
          GeneralChecksum(bootmap+$00+(i*secsize),
                          secsize,
                          secsize+4,
                          $4,
                          true),
              bootmap+(i*secsize)+$00);
   end;
   //Make a copy
   for p:=0 to (nzones*secsize)-1 do
    WriteByte(ReadByte(bootmap+p),bootmap+p+nzones*secsize);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Delete an ADFS file/directory
-------------------------------------------------------------------------------}
function TDiscImage.DeleteADFSFile(filename: String;
                    TreatAsFile:Boolean=False;extend:Boolean=True):Boolean;
var
 ptr,
 entry,
 dir,
 addr,
 len,
 fs,fl,i     : Cardinal;
 success,
 delfsm      : Boolean;
 FreeEnd,
 linklen     : Byte;
 fileparent  : String;
 fragments,
 fsfragments : TFragmentArray;
begin
 Result:=False;
 //Check that the file exists
 if(FileExists(filename,ptr))or((filename='$')and(FDirType=2))then
 begin
  //If we are deleting the root (usually only when extending/contracting)
  if(filename='$')and(FDirType=2)then
  begin
   entry:=$FFFF;
   dir  :=$FFFF;
  end
  else
  begin
   //FileExists returns a pointer to the file
   entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
   dir  :=ptr div $10000;  //Top 16 bits - directory reference
  end;
  success:=True;
  if filename<>'$' then
   //If directory, delete contents first
   if(FDisc[dir].Entries[entry].DirRef>0)and(not TreatAsFile)then
    //We'll do a bit of recursion to remove each entry one by one. If it
    //encounters a directory, that will get it's contents deleted, then itself.
    while (Length(FDisc[FDisc[dir].Entries[entry].DirRef].Entries)>0)
      and (success) do
      //If any fail for some reason, the whole thing fails
     success:=DeleteADFSFile(
             filename
            +dirsep
            +FDisc[FDisc[dir].Entries[entry].DirRef].Entries[0].Filename);
  //Only continue if we are successful
  if success then
  begin
   if filename<>'$' then
   begin
    //Make a note of the parent - these will become invalid soon
    fileparent:=FDisc[dir].Entries[entry].Parent;
    //Take a note of where it is on disc
    addr:=FDisc[dir].Entries[entry].Sector;
    //And how much space it took up
    len:=FDisc[dir].Entries[entry].Length;
    //Round up to the next whole sector
    i:=len DIV $100;
    if len MOD $100>0 then inc(i);
    len:=i;
    //Remove it from the internal array
    ReduceADFSCat(dir,entry);
    //Remove from the catalogue
    UpdateADFSCat(fileparent);
   end;
   if filename='$' then
    addr:=Read32b(bootmap+$0C+4); //ID of the root
   //Add to the free space map
   if not FMap then //Old map
   begin
    FreeEnd:=ReadByte($1FE); //FSM pointer
    //Go through each pointer and length and see if we can add to it
    for i:=0 to (FreeEnd div 3)-1 do
    begin
     fs:=Read24b($000+i*3); //FreeStart
     fl:=Read24b($100+i*3); //FreeLen
     if fs+fl=addr then //New space is immediately after this one
     begin
      //Add to FreeLen
      inc(fl,len);
      //and update
      Write24b(fl,$100+i*3);
      //Clear our pointers
      addr:=0;
      len:=0;
     end;
     if addr+len=fs then //New space is immediately before this one
     begin
      //Update FreeStart to new address
      Write24b(addr,$000+i*3);
      //Add to FreeLen
      inc(fl,len);
      //and update
      Write24b(fl,$100+i*3);
      //Clear our pointers
      addr:=0;
      len:=0;
     end;
    end;
    //Could not find an entry, so add a new one, if there is space
    if (addr+len>0) and (FreeEnd div 3<82) then
    begin
     Write24b(addr,$000+FreeEnd);
     Write24b(len ,$100+FreeEnd);
     inc(FreeEnd,3);
     WriteByte(FreeEnd,$1FE);
    end;
   end;
   if FMap then //New Map
   begin
    //Check to see if it is OK to delete the FSM fragment
    delfsm:=True;
    //Is it s shared object?
    if(addr mod $100>0)and(filename<>'$')then
    begin
     //Go through the entire catalogue looking for another with the same fragment
     if Length(FDisc)>0 then
      for dir:=0 to Length(FDisc)-1 do //Go through each directory
       if Length(FDisc[dir].Entries)>0 then
        for entry:=0 to Length(FDisc[dir].Entries)-1 do //And each file within
         if FDisc[dir].Entries[entry].Sector div $100=addr div $100 then
          delfsm:=False; //If we find another, then we shall not remove the fragment
    end;
    //Only continue if it is OK
    if delfsm then
    begin
     //Get all the fragments - we do this every time as we will be updating the
     //list each time
     fragments:=NewDiscAddrToOffset(addr,False);
     //Go through each one
     if Length(fragments)>0 then
      for i:=0 to Length(fragments)-1 do
      begin
       //Get all the free space fragments
       fsfragments:=ADFSGetFreeFragments(False);
       fs:=0;
       ptr:=fragments[i].Zone+1;//Set to a random value
       //Find the zone (the first fragment in the zone)
       if Length(fsfragments)>0 then
       begin
        while (fs<Length(fsfragments)-1)
           AND(fsfragments[fs].Zone<>fragments[i].Zone) do
         inc(fs);
        //We using ptr just in case fsfragments has no entries
        ptr:=fsfragments[fs].Zone;
       end;
       //There are no free fragments in this zone
       if(ptr<>fragments[i].Zone)or(Length(fsfragments)=0)then
       begin
        //So write the pointer to our fragment in the zone header
        //The zone header's freelink is always 15 bits long. The freelinks in the
        //map will be idlen bits long.
        WriteBits(fragments[i].Offset,(fragments[i].Zone*secsize)+1+bootmap,0,15);
        //And blank the fragment ID, leaving the stop bit, if it is there
        WriteBits($0,(fragments[i].Offset DIV 8)
                    +(fragments[i].Zone*secsize)+bootmap+1,
                     fragments[i].Offset MOD 8,idlen);
       end;
       //We do have some free fragments in this zone
       if Length(fsfragments)>0 then
       begin
        if fsfragments[fs].Zone=fragments[i].Zone then
        begin
         ptr:=fsfragments[fs].Offset;//Use as a counter
         while (fs<Length(fsfragments)-1)
           AND (fsfragments[fs].Zone=fragments[i].Zone)
           AND (ptr<fragments[i].Offset) do
         begin //We'll now find the first pointer past this fragment
          inc(fs);
          if fsfragments[fs].Zone=fragments[i].Zone then
           inc(ptr,fsfragments[fs].Offset);
         end;
         //We exit the loop with ptr more than the offset so we adjust the previous entry
         if ptr>fragments[i].Offset then
         begin
          //Write the pointer to our fragment
          WriteBits(fragments[i].Offset-(ptr-fsfragments[fs].Offset),
                                        ((ptr-fsfragments[fs].Offset)DIV 8)
                                        +(fsfragments[fs].Zone*secsize)+bootmap+1,
                                        (ptr-fsfragments[fs].Offset)MOD 8,
                                        15);
          //Now replace the fragment ID and add the distance to the next.
          dec(ptr,fragments[i].Offset);
          //Adjust number of bits to write
          linklen:=idlen; //In the map
          if fragments[i].Offset DIV 8=0 then linklen:=15; //In the header
          WriteBits(ptr,(fragments[i].Offset DIV 8)
                       +(fragments[i].Zone*secsize)+bootmap+1,
                       fragments[i].Offset MOD 8,
                       15);
         end
         else  //We exit the loop with ptr not as high, so we need to add an entry
         begin // and update the final fragment
          //Write the new pointer to our fragment
          //Adjust number of bits to write
          linklen:=idlen; //In the map
          if ptr DIV 8=0 then linklen:=15; //In the header
          WriteBits(fragments[i].Offset-ptr,(ptr DIV 8)
                                           +(fragments[i].Zone*secsize)+bootmap+1,
                                           ptr MOD 8,linklen);
          //Now write zero in place of our fragid
          WriteBits(0,(fragments[i].Offset DIV 8)
                     +(fragments[i].Zone*secsize)+bootmap+1,
                     fragments[i].Offset MOD 8,
                     idlen);
         end;
        end;
       end;
      end;
     //Update the checksums
     for i:=0 to nzones-1 do
     begin
      //Ensure the top bit is set on the first link for each zone
      WriteByte(ReadByte(bootmap+2+i*secsize)OR$80,bootmap+2+i*secsize);
      //Zone checks
      WriteByte(
            GeneralChecksum(bootmap+$00+(i*secsize),
                            secsize,
                            secsize+4,
                            $4,
                            true),
                bootmap+(i*secsize)+$00);
     end;
     //Make a copy
     for ptr:=0 to (nzones*secsize)-1 do
      WriteByte(ReadByte(bootmap+ptr),bootmap+ptr+nzones*secsize);
    end;
   end;
   //Big Dir - Verify if directory needs reduced.
   if(FDirType=2)and(extend)then ExtendADFSBigDir(dir,0,False);
   //Tidy up the free space map, as we may have missed something
   ConsolodateADFSFreeSpaceMap;
   //Update the free space map
   ADFSFreeSpaceMap;
   //Return a success
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path
-------------------------------------------------------------------------------}
function TDiscImage.ExtractADFSFile(filename: String;
                                             var buffer: TDIByteArray): Boolean;
var
 source        : Integer;
 entry,dir,
 frag,dest,
 fragptr,len,
 filelen       : Cardinal;
 fragments     : TFragmentArray;
begin
 Result   :=False;
 fragptr  :=0;
 fragments:=nil;
 source   :=0;
 if FileExists(filename,fragptr) then //Does the file actually exist?
 //Yes, so load it - there is nothing to stop a directory header being extracted
 //if passed in the filename parameter.
 begin
  //FileExists returns a pointer to the file
  entry:=fragptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=fragptr div $10000;  //Top 16 bits - directory reference
  //Make space to receive the file
  filelen:=FDisc[dir].Entries[entry].Length;
  SetLength(buffer,filelen);
  //Pointer into the fragment array
  frag   :=0;
  //Get the starting position
  if not FMap then //Old Map
   source:=FDisc[dir].Entries[entry].Sector*$100;
  if FMap then //New Map
  begin
   //Get the fragment offsets of the file
   fragments:=NewDiscAddrToOffset(FDisc[dir].Entries[entry].Sector);
   //No fragments have been returned - we have an error
   if Length(fragments)=0 then
   begin
    Result:=False;
    exit;
   end;
  end;
  dest  :=0;      //Length pointer/Destination pointer
  len   :=filelen;//Amount of data to read in
  repeat
   //Fragmented filing system, so need to work out source and length
   if FMap then
    if frag<Length(fragments) then
    begin
     source:=fragments[frag].Offset;           //Source of data
     len   :=fragments[frag].Length;           //Amount of data
    end;
   //Make sure we don't read too much
   if dest+len>filelen then
    len:=filelen-dest;
   //Read the data into the buffer
   ReadDiscData(source,len,FDisc[dir].Entries[entry].Side,buffer[dest]);
   //Move the size pointer on, by the amount read
   inc(dest,len);
   //Get the next block pointer
   inc(frag);
  until dest>=filelen; //Once we've reached the file length, we're done
 end;
 Result:=True;
end;

{-------------------------------------------------------------------------------
Moves a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.MoveADFSFile(filename,directory: String): Integer;
var
 direntry : TDirEntry;
 sdir,
 sentry,
 ddir,
 dentry,
 ptr      : Cardinal;
 sparent  : String;
begin
 Result:=-11;//Source file not found
 ptr:=0;
 if FileExists(filename,ptr) then
 begin
  Result:=-6;//Destination directory not found
  //FileExists returns a pointer to the file
  sentry:=ptr mod $10000;  //Bottom 16 bits - entry reference
  sdir  :=ptr div $10000;  //Top 16 bits - directory reference
  //Take a copy
  direntry:=FDisc[sdir].Entries[sentry];
  //Remember the original parent
  sparent:=direntry.Parent;
  if(FileExists(directory,ptr))or(directory='$')then
  begin
   Result:=-5;//Unknown error
   //Alter for the new parent
   direntry.Parent:=directory;
   //Destination directory reference
   ddir:=0;//Root
   if directory<>'$' then
   begin
    //FileExists returns a pointer to the file
    dentry:=ptr mod $10000;  //Bottom 16 bits - entry reference
    ddir  :=ptr div $10000;  //Top 16 bits - directory reference
    ddir:=FDisc[ddir].Entries[dentry].DirRef;
   end;
   //Extend the destination directory (Big Dir)
   if FDirType=2 then
    if not ExtendADFSBigDir(ddir,Length(direntry.Filename),True) then
    begin
     Result:=-9; //Cannot extend
     exit;
    end;
   //Insert into the new directory
   Result:=ExtendADFSCat(ddir,direntry);
   //And update
   UpdateADFSCat(directory);
   //Now remove from the original directory
   ReduceADFSCat(sdir,sentry);
   //And update the original parent
   UpdateADFSCat(sparent);
   //Contract the source directory (Big Dir)
   if FDirType=2 then ExtendADFSBigDir(sdir,0,False);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Extends, or reduces, an ADFS Big Directory by 'space' bytes
-------------------------------------------------------------------------------}
function TDiscImage.ExtendADFSBigDir(dir: Cardinal;space: Integer;add: Boolean):Boolean;
var
 parent,
 addr,
 d,e,
 dirsize,
 hdr,tail,
 heapsize : Cardinal;
 frag     : TFragmentArray;
begin
 Result:=False; //By default - fail to extend/contract
 //We won't take account of minus sizes, so zero it if it is
 if space<0 then space:=0;
 //Are we adding an extra entry?
 if add then inc(space,$1C+4); //Data after hdr ($1C) + before tail ($04)
 //Make sure it we are using big directories
 if FDirType=2 then
 begin
  //we need to find out the directory indirect address. Therefore we need to
  //know what the parent is.
  parent:=$FFFFFFFF; //Root, by default
  if Length(FDisc)>0 then
   for d:=0 to Length(FDisc)-1 do
    if Length(FDisc[d].Entries)>0 then
     for e:=0 to Length(FDisc[d].Entries)-1 do
      if FDisc[d].Entries[e].DirRef=dir then parent:=d*$10000+e;
  //Parent directory and entry will be in parent, if not found, will be zero (root)
  d:=parent DIV $10000; //Top 16 bits
  e:=parent MOD $10000; //Bottom 16 bits
  //We now know where, on the disc, the directory is
  if parent=$FFFFFFFF then //If it is the root, then just get the root address
   addr:=root
  else
  begin //Otherwise get the fragments, so we can get the address
   addr:=FDisc[d].Entries[e].Sector;
   frag:=NewDiscAddrToOffset(addr);
   if Length(frag)>1 then addr:=frag[1].Offset; //Usually root
   if Length(frag)=1 then addr:=frag[0].Offset; //Directories do not get fragmented
   if Length(frag)=0 then exit; //Could not find anything, so bye bye
  end;
  dirsize:=Read32b(addr+$0C);//Length of directory
  tail:=$08+Read32b(addr+$10)*$4;//Tail size
  hdr:=(($1C+Read32b(addr+$08)+$03)div 4)*4;//Size of header (padded to word boundary)
  heapsize:=Read32b(addr+$10)*$1C+Read32b(addr+$14);//Size of entries
  //Is the directory oversized? Only shrink if space is passed as zero
  if(dirsize-(tail+hdr+heapsize)>=2048)and(space=0)then //Contract the directory size
//   if parent<>$FFFFFFFF then //This function won't work on the root, currently
    ADFSBigDirSizeChange(d,e,False);
  //If we are contracting, the result might as well be true, whether it happened
  if space=0 then Result:=True; //or not.
  if space>0 then //Look to extend
//  begin
   //We'll add an extra 3 bytes to ensure there is enough space, including pad
   if dirsize-(tail+hdr+heapsize)>=space+3 then //to word boundary
    Result:=True //Doesn't need extending
   else //Directory needs to be extended (by 2048) and possibly moved.
//   begin
//    Result:=False; //Default
//    if parent<>$FFFFFFFF then //This function won't work on the root, currently
     Result:=ADFSBigDirSizeChange(d,e,True);
//   end;
//  end;
 end;
end;

{-------------------------------------------------------------------------------
Do the actual extending or contracting of a big directory
-------------------------------------------------------------------------------}
function TDiscImage.ADFSBigDirSizeChange(dir,entry:Cardinal;extend:Boolean):Boolean;
var
 buffer,
 buffer2    : TDIByteArray;
 dirsize,
 ptr,
 tail       : Cardinal;
 file_entry : TDirEntry;
 frags      : TFragmentArray;
 c          : Integer;
 ok         : Boolean;
begin
 //extend = True  : extend by 2048 bytes
 //extend = False : contract by 2048 bytes
 Result:=False;
 ok:=False;
 //Both extract and contract will need to treat the directory as a file
 if(dir<>$FFFF)and(entry<>$FFFF)then //Not the root
 begin
  file_entry:=FDisc[dir].Entries[entry];
  ok:=ExtractADFSFile(file_entry.Parent+dir_sep+file_entry.Filename,buffer);
 end
 else //Root
  ok:=ReadDiscData(root,Read32b(root+$0C),0,buffer);
 //Call ExtractADFSFile to extract to a temporary buffer
 if ok then
 begin
  //Take a copy
  buffer2:=buffer;
  //Update the header
  dirsize:=buffer[$0F]<<24
          +buffer[$0E]<<16
          +buffer[$0D]<< 8
          +buffer[$0C];
  if extend then inc(dirsize,2048) else dec(dirsize,2048);
  buffer[$0F]:=(dirsize DIV $1000000)MOD$100;
  buffer[$0E]:=(dirsize DIV $10000  )MOD$100;
  buffer[$0D]:=(dirsize DIV $100    )MOD$100;
  buffer[$0C]:= dirsize              MOD$100;
  //Move the tail (extend or contract buffer if necessary)
  if extend then
  begin
   SetLength(buffer,dirsize);//Extend before move
   //Clear the new area
   for ptr:=Length(buffer)-2048 to Length(buffer)-1 do buffer[ptr]:=0;
  end;
  //Work out the tail size (8 + num of entries * 4)
  tail:=8+(
        buffer[$10]
       +buffer[$11]<<8
       +buffer[$12]<<16
       +buffer[$13]<<24)*4;
  //Move the tail
  for ptr:=1 to tail do
   if extend then
   begin //Upwards (extending)
    buffer[dirsize-ptr]:=buffer[(dirsize-2048)-ptr];
    buffer[(dirsize-2048)-ptr]:=0;
   end
   else
   begin //Downwards (contracting)
    buffer[dirsize-ptr]:=buffer[(dirsize+2048)-ptr];
    buffer[(dirsize+2048)-ptr]:=0;
   end;
  //Contract after move
  if not extend then SetLength(buffer,dirsize);
  c:=-1;
  //Call DeleteADFSFile and treat the directory as a file, and don't extend/contract
  if(dir<>$FFFF)and(entry<>$FFFF)then
   ok:=DeleteADFSFile(file_entry.Parent+dir_sep+file_entry.Filename,True,False)
  else
   ok:=DeleteADFSFile('$',True,False); //Delete the root
  if ok then
  begin
   //The new length of the directory
   file_entry.Length:=Length(buffer);
   //Setup to write the root
   if(dir=$FFFF)and(entry=$FFFF)then
    file_entry.Filename:='$';
   //Call WriteADFSFile and treat the directory as a file (set extend to FALSE)
   c:=WriteADFSFile(file_entry,buffer,True,False);
   //Checksum for the root
   if file_entry.Filename='$' then
    WriteByte(CalculateADFSDirCheck(root),root+dirsize-1);
   //Success? Then return True
   if c>=0 then
    Result:=True
   else
    //If WriteADFSFile failed then revert to original and re-add, returning a FALSE
    c:=WriteADFSFile(file_entry,buffer2,True,False);
  end;
  //Success writing anything other than the root
  if(c>=0)AND(dir<>$FFFF)AND(entry<>$FFFF)then
  begin
   entry:=c;
   //As the directory is re-added, it's DirRef will be reassigned
   if FDisc[dir].Entries[entry].DirRef<>file_entry.DirRef then
   begin
    //So make a note of the new assignment
    c:=FDisc[dir].Entries[entry].DirRef;
    //Reset the old one
    FDisc[dir].Entries[entry].DirRef:=file_entry.DirRef;
    //Then remove it from the end of the array
    if c=Length(FDisc)-1 then SetLength(FDisc,c);
   end;
   //We need to know where it is now stored.
   frags:=NewDiscAddrToOffset(FDisc[dir].Entries[entry].Sector);
   ptr:=$0;
   c:=0;
   repeat
    if Length(frags)>c then
    begin
     //Read the fragment
     ptr:=frags[c].Offset;
     //Make sure it is the same length as the directory, otherwise reset ptr
     if Read32b(ptr+$0C)<>frags[c].Length then ptr:=0;
    end;
    //Increase the counter
    inc(c);
   until (c=Length(frags))or(ptr<>0);
   //Update the checksums
   if ptr<>$0 then
    WriteByte(CalculateADFSDirCheck(ptr),ptr+dirsize-1);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Inserts a directory entry 'direntry' into directory 'dir'
-------------------------------------------------------------------------------}
function TDiscImage.ExtendADFSCat(dir: Cardinal;direntry: TDirEntry): Cardinal;
var
 i : Cardinal;
begin
 //Extend the catalogue by 1
 SetLength(FDisc[dir].Entries,Length(FDisc[dir].Entries)+1);
 //Is this the first file?
 if Length(FDisc[dir].Entries)=1 then
  Result:=0
 else //No, find a place to insert the file
 begin
  Result:=0;
  while (Result<Length(FDisc[dir].Entries)-1)
  AND(UpperCase(direntry.Filename)>UpperCase(FDisc[dir].Entries[Result].Filename))do
   inc(Result);
  //Move the upper entries up
  if Result<Length(FDisc[dir].Entries)-1 then
   for i:=Length(FDisc[dir].Entries)-2 downto Result do
    FDisc[dir].Entries[i+1]:=FDisc[dir].Entries[i];
 end;
 //Then put it in
 FDisc[dir].Entries[Result]:=direntry;
end;

{-------------------------------------------------------------------------------
Removes directory entry index 'entry' from directory 'dir'
-------------------------------------------------------------------------------}
procedure TDiscImage.ReduceADFSCat(dir,entry: Cardinal);
var
 i: Integer;
begin
 //Now remove from the original directory
 if entry<Length(FDisc[dir].Entries)-1 then
  for i:=entry to Length(FDisc[dir].Entries)-2 do
   FDisc[dir].Entries[i]:=FDisc[dir].Entries[i+1];
 //Shorten the array by one
 SetLength(FDisc[dir].Entries,Length(FDisc[dir].Entries)-1);
end;

{-------------------------------------------------------------------------------
Attempts to fix any broken ADFS directories
-------------------------------------------------------------------------------}
function TDiscImage.FixBrokenADFSDirectories: Boolean;
var
 dir,
 entry     : Integer;
begin
 Result:=False;
 //Go through each directory and find any reported as broken
 if Length(FDisc)>0 then
 begin
  if FDisc[0].Broken then //Root directory broken
  begin
   //Fix the root
   FixADFSDirectory(-1,-1);
   //Mark as changed
   Result:=True;
  end;
  //Now check everything else
  for dir:=0 to Length(FDisc)-1 do
   if Length(FDisc[dir].Entries)>0 then
    for entry:=0 to Length(FDisc[dir].Entries)-1 do
     if FDisc[dir].Entries[entry].DirRef<>-1 then //Found a directory
      //Is it broken?
      if FDisc[FDisc[dir].Entries[entry].DirRef].Broken then
      begin
       FixADFSDirectory(dir,entry);
       Result:=True;//Set to true to indicate it has changed
      end;
 end;
 if Result then FDisc:=ReadADFSDisc; //Rescan the image
end;

{-------------------------------------------------------------------------------
Attempts to fix a broken ADFS directory
-------------------------------------------------------------------------------}
procedure TDiscImage.FixADFSDirectory(dir,entry: Integer);
var
 address,
 len       : Cardinal;
 dirref,i  : Integer;
 error,
 tail      : Byte;
 fragments : TFragmentArray;
 StartName,
 EndName   : String;
begin
 //Get the directory reference
 if(dir>=0)and(entry>=0) then
  dirref:=FDisc[dir].Entries[entry].DirRef //Sub directory
 else
  dirref:=0; //Root
 //What is the error?
 error:=FDisc[dirref].ErrorCode;
 //Where is the directory, and how big?
 address:=0;
 len:=0;
 //We need to resolve the actual disc offset and length
 if(dir>=0)and(entry>=0) then
 begin
  if FMap then //New Map
  begin
   //Get the fragments for the directory (should only be one)
   fragments:=NewDiscAddrToOffset(FDisc[dir].Entries[entry].Sector);
   i:=-1;
   if Length(fragments)>0 then i:=0;
   if Length(fragments)>1 then i:=1; //System areas have two
   if i>=0 then
   begin
    address:=fragments[i].Offset;
    len    :=fragments[i].Length;
   end;
  end;
  if not FMap then //Old Map
  begin
   address:=FDisc[dir].Entries[entry].Sector*$100;
   len    :=FDisc[dir].Entries[entry].Length;
  end;
 end
 else //Root
 begin
  address:=root;
  len    :=root_size;
 end;
 if FDirType=0 then tail:=$35;
 if FDirType=1 then tail:=$29;
 if FDirType=2 then tail:=$08;
 if (error AND $01=$01) then //StartSeq<>EndSeq
 begin
  //Quite simple - just pick up StartSeq and write it to EndSeq
  if FDirType=0 then WriteByte(ReadByte(address),address+(len-tail)+$2F);
  if FDirType=1 then WriteByte(ReadByte(address),address+(len-tail)+$23);
  if FDirType=2 then WriteByte(ReadByte(address),address+(len-tail)+$04);
 end;
 if (error AND $02=$02) then //StartName<>EndName (Old/New Dirs)
 begin
  //Almost as simple - just re-write what they should be
  if FDirType=0 then StartName:='Hugo';
  if FDirType=1 then StartName:='Nick';
  for i:=1 to 4 do
  begin
   WriteByte(Ord(StartName[i]),address+i);        //Header
   WriteByte(Ord(StartName[i]),address+(len-6)+i);//Tail
  end;
 end;
 if (error AND $04=$04) then //StartName<>'SBPr' or EndName<>'oven' (Big)
 begin
  //The same as previously, except start and end do not match
  StartName:='SBPr';
  EndName  :='oven';
  for i:=1 to 4 do
  begin
   WriteByte(Ord(StartName[i]),address+3+i);             //Header
   WriteByte(Ord(EndName[i])  ,address+(len-tail)+(i-1));//Tail
  end;
 end;
 //Bit 3 indicates invalid checksum - but we'll update anyway
 //The above changes could alter it
 if FDirType=0 then //Old - can be zero
  WriteByte($00,address+$4FF)
 else               //New
  WriteByte(CalculateADFSDirCheck(address),address+len-1);
 //Reset the flags
 FDisc[dirref].Broken:=False;
 FDisc[dirref].ErrorCode:=$00;
end;
