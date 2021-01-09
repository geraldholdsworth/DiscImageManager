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
  if Length(Fdata)>0 then
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
     if emuheader+dr_ptr+$40<Cardinal(Length(FData)) then
     begin
      secsize   :=1 shl ReadByte(dr_ptr+$00); //Sector size
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
     if(emuheader+bootmap+nzones*secsize<Cardinal(Length(Fdata)))and(nzones>0)then
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
  Result:=FFormat shr 4=1;
 end;
end;

{-------------------------------------------------------------------------------
Read ADFS Directory
-------------------------------------------------------------------------------}
function TDiscImage.ReadADFSDir(dirname: AnsiString; sector: Cardinal): TDir;
var
 Entry              : TDirEntry;
 temp,
 StartName,EndName,
 dirtitle,pathname  : AnsiString;
 ptr,
 dircheck,numentrys,
 dirsize,namesize,
 entrys,nameheap,
 tail,NameLen,
 entrysize,offset,
 NameOff,amt,
 EndOfChk           : Cardinal;
 addr               : TFragmentArray;
 StartSeq,EndSeq,
 dirchk,NewDirAtts  : Byte;
 validdir,validentry,
 endofentry         : Boolean;
const
 //Attributes
 OldAtts: array[0..9] of Char = ('R','W','L','D','E','r','w','e','P',' ');
 NewAtts: array[0..7] of Char = ('R','W','L','D','r','w',' ',' ');
 //RISC OS Filetypes (as at RISC OS 5.23)
 FileTypes: array[1..79] of AnsiString = (
 '004AIM'     ,'0E1Index'   ,'132ICO'     ,'190DSK'     ,'191PCWDisc' ,
 '194D20Disc' ,'195D2Disc'  ,'196D10Disc' ,'19BMyZ80'   ,'1A6AcornCPM',
 '5F4SparkScr','68EPackdDir','690Clear'   ,'691Degas'   ,'692IMG'     ,
 '693IFF'     ,'694MacPaint','695GIF'     ,'696Pineappl','697PCX'     ,
 '698QRT'     ,'699MTV'     ,'69ACadSoft' ,'69BIrlam'   ,'69CBMP'     ,
 '69EPBMPlus' ,'A91Zip'     ,'ABACPIO'    ,'ABFCabinet' ,'ADFPDF'     ,
 'AE9Alarms'  ,'AF1Music'   ,'AFFDrawFile','B60PNG'     ,'BBCBBC ROM' ,
 'BD9DiscP'   ,'BDADisc'    ,'BE8PhotoCD' ,'C46Tar'     ,'C85JPEG'    ,
 'DDCArchive' ,'DEADXF'     ,'F95Code'    ,'F9DDiscCD'  ,'F9EDiscDP'  ,
 'F9FDiscD'   ,'FAEResource','FB4DiscR'   ,'FB5NoDisc'  ,'FC3Patch'   ,
 'FC6PrntDefn','FC8DOSDisc' ,'FCASquash'  ,'FCCDevice'  ,'FCEFloppy'  ,
 'FCFCache'   ,'FD6TaskExec','FD7TaskObey','FDCSoftLink','FE4DOS'     ,
 'FE6UNIX Ex' ,'FEADesktop' ,'FEBObey'    ,'FECTemplate','FEDPalette' ,
 'FF0TIFF'    ,'FF2Config'  ,'FF4Printout','FF5PoScript','FF6Font'    ,
 'FF7BBC font','FF8Absolute','FF9Sprite'  ,'FFAModule'  ,'FFBBASIC'   ,
 'FFCUtility' ,'FFDData'    ,'FFECommand' ,'FFFText'                   );
begin
 RemoveControl(dirname);
 //Store complete path
 pathname:=dirname;
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
   dirtitle:=dirname;                                  //Does not have a directory title
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
      Entry.Sector   :=Read24b(offset+$16);   //How to find the file
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
     Entry.TimeStamp:=ConvertTimeDate(Entry.ExecAddr+
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
  //But only for New and Big Directories, optional for old
  if (FDirType>0) and (dirchk<>0) then //Ignore if the directory check byte is 0
  begin
   //Start of directory includes the nameheap for Big Directories
   if FDirType<2 then EndOfChk:=entrys+ptr*entrysize
   else EndOfChk:=nameheap+namesize;
   //This value is the check byte.
   dircheck:=CalculateADFSDirCheck(sector,EndOfChk,tail,dirsize);
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
function TDiscImage.CalculateADFSDirCheck(sector,EndOfChk,tail,
                                          dirsize:Cardinal): Byte;
var
 dircheck,
 amt,
 offset   : Cardinal;
begin
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
function TDiscImage.NewDiscAddrToOffset(addr: Cardinal): TFragmentArray;
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
 SetLength(Result,0);
 if FMap then //Only works for new maps
 begin
  if addr=0 then //Root
  begin
   //We've been given the address of the root, but we know where this is so no
   //need to calculate it.
   SetLength(Result,1);
   Result[0].Offset:=bootmap+(nzones*secsize*2);//addr;
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
     if id<>0 then
     begin
      //Now find the end of the fragment entry
      j:=i-1;
      repeat
       inc(j);
      until(IsBitSet(ReadByte(start+(j div 8)),j mod 8))or(j>=allmap);
      //Move the pointer on, after the '1'
      i:=j;
      //And remember it
      len:=(j+1)*bpmb;
      //Does it match the id we are looking for?
      if id=(addr div $100)mod$10000 then
      begin
       off:=(off-(zone_spare*zone))*bpmb;
       off:=off mod disc_size;
       //Fragment ID found, so add it - there could be a few entries
       SetLength(fragid,Length(fragid)+1);
       fragid[Length(fragid)-1].Offset:=off;
       //Save the length
       fragid[Length(fragid)-1].Length:=len-fragid[Length(fragid)-1].Offset;
      end;
     end else inc(i);
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
     Result[i].Offset:=(fragid[i].Offset+(sector*secsize));
     //Add the length of this fragment
     Result[i].Length:=fragid[i].Length;
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
begin
 //Reset the accumulator
 acc:=0;
 //Iterate through the block, ignoring the final byte (which contains the
 //checksum)
 for pointer:=size-2 downto 0 do
 begin
  //Add in the carry
  inc(acc,acc div $100);
  //and reset the carry
  acc:=acc and $FF;
  //Add each byte to the accumulator
  inc(acc,ReadByte(offset+pointer));
 end;
 //Add any carries and return an 8 bit number
 Result:=acc and $FF;
// Result:=((acc div $100)+(acc mod $100)) and $FF;
end;

{-------------------------------------------------------------------------------
Read ADFS Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadADFSDisc: TDisc;
var
 d,ptr    : Cardinal;
 OldName0,
 OldName1 : AnsiString;
 addr: TFragmentArray;
begin
 //Initialise some variables
 root   :=$00; //Root address (set to zero so we can id the disc)
 SetLength(Result,0);
// UpdateProgress('Reading ADFS catalogue');
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
 freelink,
 nextfreelink,
 address,len      : Cardinal;
 freeend          : Byte;
const
 dr_size = $40; //Size of disc record + header (zone 0)
begin
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
 end;
 //Old Map (ADFS S,M,L, and D)
 if not FMap then
 begin
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
  end;
 end;
 //New Map (ADFS E,E+,F,F+)
 if FMap then
 begin
  c:=0;//Counter for current zone
  while c<nzones do //Go through each zone
  begin
   //Get the start of the zone
   freelink:=bootmap+(c*secsize)+1;
   //Get the first freelink of the zone
   nextfreelink:=(Read16b(freelink)AND $7FFF)DIV 8;
   //We will continue while nextfreelink is not $0000
   while nextfreelink<>0 do
   begin
    //We're going to count the length, starting after the nextfreelink
    len:=1;
    repeat
     inc(len)
    until ReadByte(freelink+nextfreelink+len)<>0; //Until we find a bit set
    //Add another to count the first byte
    inc(len);
    //Calculate the disc address from where the free space starts
    address:=((freelink+nextfreelink)
            -(bootmap+dr_size)-(zone_spare DIV 8)*c)*8*bpmb;
    //And where it ends
    finish:=((freelink+nextfreelink+len)
           -(bootmap+dr_size)-(zone_spare DIV 8)*c)*8*bpmb;
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
    //Move the freelink pointer to where the nextfreelink was
    freelink:=freelink+nextfreelink;
    //And read what is there
    nextfreelink:=(Read16b(freelink)AND $7FFF)DIV 8;
   end;
   //Next zone
   inc(c);
  end;
  //Mark the system area
  for address:=bootmap to bootmap+(nzones*secsize*2) do //Two copies of the map
   ADFSFillFreeSpaceMap(address,$FE);
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
 dirid,att: AnsiString;
begin
 //Blank everything
 ResetVariables;
 //Set the format
 FFormat:=$10+minor;
 //Set the filename
 imagefilename:='Untitled.'+FormatExt;
 //Setup the data area
 case minor of
  0 : SetLength(FData,160*1024);  //S
  1 : SetLength(FData,320*1024);  //M
  2 : SetLength(FData,640*1024);  //L
  3 : SetLength(FData,800*1024);  //D
  4 : SetLength(FData,800*1024);  //E
  5 : SetLength(FData,800*1024);  //E+
  6 : SetLength(FData,1600*1024); //F
  7 : SetLength(FData,1600*1024); //F+
 end;
 //Fill with zeros
 for t:=0 to Length(FData)-1 do FData[t]:=0;
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
 //Set the boot option
 SetLength(bootoption,1);
 bootoption[0]:=0;
 SetLength(free_space_map,1); //Free Space Map
 disc_size:=Length(FData);    //Disc Size
 //Fill with zeros
 for t:=0 to disc_size-1 do FData[t]:=0;
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
   Write32b($00000203,bootmap+$10);//root
   //Allocation map
   WriteByte($02,bootmap+$40);
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
   Write32b($00000209,$DCC);//root
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
   Write32b($00000209,bootmap+$10);//root
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
function TDiscImage.UpdateADFSDiscTitle(title: AnsiString): Boolean;
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
Write a file to ADFS image
-------------------------------------------------------------------------------}
function TDiscImage.WriteADFSFile(var file_details: TDirEntry;
                              var buffer: TDIByteArray): Integer;
var
 dir      : Integer;
 l,
 ref,
 freeptr,
 ptr,
 filelen,
 safilelen: Cardinal;
 success  : Boolean;
begin
 //Start with a negative result
 Result:=-1;
 success:=False;
 //Validate the proposed filename
 file_details.Filename:=ValidateADFSFilename(file_details.Filename);
 //First make sure it doesn't exist already
 if not FileExists(file_details.Parent+dirsep+file_details.Filename,ref) then
  //Get the directory where we are adding it to, and make sure it exists
  if (FileExists(file_details.Parent,ref)) OR (file_details.Parent='$') then
  begin
   file_details.ImageAddress:=0;
   //Where we are inserting this into
   if file_details.Parent='$' then
    dir  :=0
   else
    dir  :=FDisc[ref div $10000].Entries[ref mod $10000].DirRef;
   l:=Length(FDisc[dir].Entries);
   //Get the file length
   filelen:=file_details.Length;
   //Work out the "sector aligned file length"
   safilelen:=filelen div secsize;
   if filelen mod secsize>0 then inc(safilelen);
   safilelen:=safilelen*secsize;
   //And make sure we can extend the catalogue
   if((FDirType=0)and(l<47)
   OR (FDirType=1)and(l<77)
   OR (FDirType=2)) then
   begin
    //First, update the map
    //Old map
    if not FMap then
    begin
     //Find a big enough space
     ptr:=ReadByte($1FE); //Number of free space entries
     freeptr:=0;
     while(freeptr<ptr)
       and(Read24b($100+freeptr)*$100<safilelen)do
      inc(freeptr,3);
     if freeptr<ptr then //Space found
     begin
      file_details.Sector:=Read24b($000+freeptr);
      //Write the file to the area
      success:=WriteDiscData(file_details.Sector*$100,0,buffer,filelen);
      if success then
      begin
       //Update the checksum, if it is a directory
       if Pos('D',file_details.Attributes)>0 then
       begin
        if FDirType=1 then //New Directory
         WriteByte(CalculateADFSDirCheck(file_details.Sector*$100,$05,$7D7,$800)
                                        ,file_details.Sector*$100+$7FF);
       end;
       //Update the free space map
       ref:=safilelen div $100;
       if safilelen mod $100>0 then inc(ref);
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
     end;
    end;
    //New map
    if FMap then
    begin
     //Get the file length
     //Find a big enough space
     //Write the file to the area
     //Update the free space map
     //Update the checksums
    end;
    //Now update the directory
    if success then
    begin
     //Extend the catalogue by 1
     SetLength(FDisc[dir].Entries,l+1);
     //Is this the first file?
     if Length(FDisc[dir].Entries)=1 then
      ptr:=0
     else //No, find a place to insert the file
     begin
      ptr:=0;
      while (ptr<Length(FDisc[dir].Entries)-1)
        AND (UpperCase(file_details.Filename)>UpperCase(FDisc[dir].Entries[ptr].Filename)) do
       inc(ptr);
      //Move the upper entries up
      if ptr<Length(FDisc[dir].Entries)-1 then
       for ref:=Length(FDisc[dir].Entries)-2 downto ptr do
        FDisc[dir].Entries[ref+1]:=FDisc[dir].Entries[ref];
     end;
     FDisc[dir].Entries[ptr]:=file_details;
     FDisc[dir].Entries[ptr].DirRef:=-1;
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
     //Update the catalogue
     UpdateADFSCat(file_details.Parent);
     //Update the free space map
     ADFSFreeSpaceMap;
    end;
   end;
  end;
end;

{-------------------------------------------------------------------------------
Create a directory
-------------------------------------------------------------------------------}
function TDiscImage.CreateADFSDirectory(var dirname,parent,attributes: AnsiString): Integer;
var
 dirid     : AnsiString;
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
 Result:=-1;
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
  end;
  //Write the directory
  if dirname='$' then //Root
  begin
   for t:=0 to Length(buffer)-1 do
    WriteByte(buffer[t],root+t);
   //Directory Checkbyte
   if FDirType=1 then //New Directory
    WriteByte(CalculateADFSDirCheck(root,
                                    $05,
                                    root_size-$29,
                                    root_size),
             root+(root_size-1));
   if FDirType=2 then //Big Directory
    WriteByte(CalculateADFSDirCheck(root,
                                    $1D,
                                    root_size-$08,
                                    root_size),
             root+(root_size-1));
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
   Result:=WriteADFSFile(fileentry,buffer);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update a directory catalogue
-------------------------------------------------------------------------------}
procedure TDiscImage.UpdateADFSCat(directory: AnsiString);
var
 dir,
 diraddr,
 ref,i     : Cardinal;
 c         : Byte;
 temp      : AnsiString;
const
 oldattr = 'RWLDErweP'+#00;
 newattr = 'RWLDrw';
begin
 if (FileExists(directory,ref)) or (directory='$') then
 begin
  if FDirType<2 then //Old and New Directory
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
   diraddr:=diraddr*$100;
   //Update the directory title
   temp:=FDisc[dir].Title+#$0D;
   for i:=0 to 18 do
   begin
    //Padded with nulls
    c:=$00;
    if i<Length(temp) then c:=Ord(temp[i+1])AND$7F;
    //Write the byte
    if FDirType=0 then WriteByte(c,diraddr+$4CB+$0E+i);
    if FDirType=1 then WriteByte(c,diraddr+$7D7+$06+i);
   end;
   //Clear the directory
   c:=0;
   if FDirType=0 then c:=47;
   if FDirType=1 then c:=77;
   if c>0 then
    for i:=0 to c-1 do
     for ref:=0 to $19 do WriteByte($00,diraddr+$05+ref+i*$1A);
   //Go through each entry and add it, overwriting what is there
   if Length(FDisc[dir].Entries)>0 then
    for ref:=0 to Length(FDisc[dir].Entries)-1 do
    begin
     //Filename - needs terminated with CR
     temp:=FDisc[dir].Entries[ref].Filename+#$0D;
     for i:=0 to 9 do
     begin
      //Padded with nulls
      c:=$00;
      if i<Length(temp) then c:=Ord(temp[i+1])AND$7F;
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
     begin
      c:=0;
      for i:=0 to 5 do
       if Pos(newattr[i+1],FDisc[dir].Entries[ref].Attributes)>0 then
        inc(c,1 shl i);
      WriteByte(c,diraddr+$05+$19+ref*$1A)
     end;
    end;
   //Update the checksum
   if FDirType=0 then //Old - can be zero
    WriteByte($00,diraddr+$4FF)
   else               //New
   begin
    //Start_of_entries + Number_of_entries * Entry_Size
    i:=$05+Length(FDisc[dir].entries)*$1A;
    WriteByte(CalculateADFSDirCheck(diraddr,i,$7D7,$800),diraddr+$7FF);
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update attributes on a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateADFSFileAttributes(filename,attributes: AnsiString): Boolean;
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
function TDiscImage.ValidateADFSFilename(filename: AnsiString): AnsiString;
var
 i: Integer;
const
  illegalOld = '#* .:$&@';
  illegalBig = '$&%@\^:.#*"|';
begin
  for i:=1 to Length(filename) do
 begin
  //Remove top-bit set characters
  filename[i]:=chr(ord(filename[i]) AND $7F);
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
function TDiscImage.RetitleADFSDirectory(filename,newtitle: AnsiString): Boolean;
var
 ptr,
 entry,
 dir    : Cardinal;
 c,i    : Byte;
 temp   : AnsiString;
begin
 Result:=False;
 if FMap then exit;
 //Check that the file exists
 if (FileExists(filename,ptr)) OR (filename='$') then
 begin
  if filename='$' then
  begin
   ptr:=root;
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
   //Padded with spaces
   c:=32;
   if i<Length(temp) then c:=Ord(temp[i+1])AND$7F;
   //Write the byte
   if FDirType=0 then WriteByte(c,ptr+$4CB+$0E+i);
   if FDirType=1 then WriteByte(c,ptr+$7D7+$06+i);
  end;
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
function TDiscImage.RenameADFSFile(oldfilename: AnsiString;var newfilename: AnsiString):Boolean;
var
 ptr,
 entry,
 dir    : Cardinal;
// frag   : TFragmentArray;
 i      : Integer;
 c      : Byte;
 temp   : AnsiString;
begin
 Result:=False;
 if FMap then exit;
 //Check that the new name meets the required DFS filename specs
 newfilename:=ValidateADFSFilename(newfilename);
 //Check that the file exists
 if FileExists(oldfilename,ptr) then
 begin
  //FileExists returns a pointer to the file
  entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=ptr div $10000;  //Top 16 bits - directory reference
  //Make sure the new filename does not already exist
  if not FileExists(FDisc[dir].Entries[entry].Parent+dirsep+newfilename,ptr) then
  begin
   //If this is a directory and the title is the same, change it also
   if FDisc[dir].Entries[entry].DirRef<>-1 then
   begin
    if FDisc[FDisc[dir].Entries[entry].DirRef].Title=FDisc[dir].Entries[entry].Filename then
     FDisc[FDisc[dir].Entries[entry].DirRef].Title:=newfilename;
    //Now need to update the catalogue for this directory
    ptr:=FDisc[dir].Entries[entry].Sector;
    if not FMap then ptr:=ptr*$100
{    else
    begin
     frag:=NewDiscAddrToOffset(ptr);
     ptr:=frag[0].Offset;
    end};
    //Update the title
    temp:=FDisc[FDisc[dir].Entries[entry].DirRef].Title+#$0D;
    for i:=0 to 18 do
    begin
     //Padded with spaces
     c:=32;
     if i<Length(temp) then c:=Ord(temp[i+1])AND$7F;
     //Write the byte
     if FDirType=0 then WriteByte(c,ptr+$4CB+$0E+i);
     if FDirType=1 then WriteByte(c,ptr+$7D7+$06+i);
    end;
    //Update the name
    temp:=newfilename+#$0D;
    for i:=0 to 9 do
    begin
     //Padded with spaces
     c:=32;
     if i<Length(temp) then c:=Ord(temp[i+1])AND$7F;
     //Write the byte
     if FDirType=0 then WriteByte(c,ptr+$4CB+$01+i);
     if FDirType=1 then WriteByte(c,ptr+$7D7+$19+i);
    end;
   end;
   //Change the entry
   FDisc[dir].Entries[entry].Filename:=newfilename;
   //Update the catalogue
   UpdateADFSCat(FDisc[dir].Entries[entry].Parent);
   Result:=True;
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
 FreeEnd   : Byte;
 FreeStart,
 FreeLen   : array[0..81] of Integer;
begin
 if not FMap then //Old map only
 begin
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
  until FreeEnd=p*3; //Go back and do it again, if things have changed
  //Run the checksums and we're done
  WriteByte(ByteCheckSum($0000,$100),$0FF);
  WriteByte(ByteCheckSum($0100,$100),$1FF);
 end;
end;

{-------------------------------------------------------------------------------
Delete an ADFS file/directory
-------------------------------------------------------------------------------}
function TDiscImage.DeleteADFSFile(filename: AnsiString):Boolean;
var
 ptr,
 entry,
 dir,
 addr,
 len,
 fs,fl,i    : Cardinal;
 success    : Boolean;
 FreeEnd    : Byte;
 fileparent : AnsiString;
begin
 Result:=False;
 if FMap then exit;
 //Check that the file exists
 if FileExists(filename,ptr) then
 begin
  //FileExists returns a pointer to the file
  entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=ptr div $10000;  //Top 16 bits - directory reference
  success:=True;
  //If directory, delete contents first
  if FDisc[dir].Entries[entry].DirRef>0 then
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
   fs:=Length(FDisc[dir].Entries);
   if entry<Length(FDisc[dir].Entries)-1 then
    for i:=entry to Length(FDisc[dir].Entries)-2 do
     FDisc[dir].Entries[i]:=FDisc[dir].Entries[i+1];
   //Shorten the array by one
   SetLength(FDisc[dir].Entries,Length(FDisc[dir].Entries)-1);
   fs:=Length(FDisc[dir].Entries);
   //Remove from the catalogue
   UpdateADFSCat(fileparent);
   //Add to the free space map
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
   //Tidy up the free space map, as we may have missed something
   ConsolodateADFSFreeSpaceMap;
   //Update the free space map
   ADFSFreeSpaceMap;
   //Return a success
   Result:=True;
  end;
 end;
end;