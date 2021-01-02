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
  UpdateProgress('Checking for ADFS');
  //Is there actually any data?
  if Length(Fdata)>0 then
  begin
   //Check for Old Map
   Check0   :=ReadByte(emuheader+$0FF);
   Check1   :=ReadByte(emuheader+$1FF);
   Check0a  :=ByteCheckSum(emuheader+$0000,$100);
   Check1a  :=ByteCheckSum(emuheader+$0100,$100);
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
    if (Read24b(emuheader+$BDA)=$000002) //Address of the root ($200 for old dir)
    and(ReadByte(emuheader+$200)=ReadByte(emuheader+$6FA)) then //Directory check bytes
     FDirType:=0; //old map, old directory - either S, M or L
    if (Read24b(emuheader+$BDA)=$000004) //Address of the root ($400 for new dir)
    and(ReadByte(emuheader+$400)=ReadByte(emuheader+$BFA)) then //Directory check bytes
    begin
     FDirType:=1; //So, old map, new directory must be ADFS D
     FFormat:=$13;
    end;
    disc_size:=Read24b(emuheader+$0FC)*$100;
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
    UpdateProgress('Checking for New Map ADFS...locating the disc record');
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
      secsize   :=1 shl ReadByte(emuheader+dr_ptr+$00); //Sector size
      bpmb      :=1 shl ReadByte(emuheader+dr_ptr+$05); //Bits per map bit
      nzones    :=ReadByte(emuheader+dr_ptr+$09)
                 +ReadByte(emuheader+dr_ptr+$2A)*$100;  //nzones is 2 bytes, for E+ and F+
      zone_spare:=Read16b(emuheader+dr_ptr+$0A);        //Zone spare bits
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
       Check0:=ReadByte(emuheader+bootmap+zone*secsize+$00);
       //CrossCheck checksum
       Check1:=Check1 XOR ReadByte(emuheader+bootmap+zone*secsize+$03);
       //Check failed, reset format
       if Check0<>GeneralChecksum(emuheader+bootmap+zone*secsize,
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
      Check0:=ReadByte(emuheader+$0C00+$1FF);
      if ByteChecksum(emuheader+$0C00,$200)<>Check0 then Result:=False;
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
      Check0 :=ByteChecksum(emuheader+$C00,$200);
      Check0a:=ReadByte(emuheader+$DFF);
      if Check0<>Check0a then FFormat:=$FF; //Checksums do not match
     end;
    end;
    //Check for type of directory, and change the format if necessary
    if FFormat<>$FF then
    begin
     FDirType:=1; //New Directory
     //Determine if it is a '+' format by reading the version flag
     if ReadByte(emuheader+dr_ptr+$2C)>0 then
     begin
      if FFormat<>$1F then inc(FFormat);
      FDirType:=2;
     end;
    end;
   end;
  end;
  Result:=FFormat<>$FF;
  if not Result then
   ResetVariables;
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
 UpdateProgress('Reading ADFS directory "'+dirname+'"');
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
   StartSeq :=ReadByte(OldDiscAddrToOffset(emuheader+sector));        //Start Sequence Number to match with end
   StartName:=ReadString(OldDiscAddrToOffset(emuheader+sector+1),-4); //Hugo or Nick
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
   StartSeq :=ReadByte(emuheader+sector);         //Start sequence number to match with end
   StartName:=ReadString(emuheader+sector+$04,-4);//Should be SBPr
   NameLen  :=Read32b(emuheader+sector+$08);     //Length of directory name
   dirsize  :=Read32b(emuheader+sector+$0C);     //Directory size in bytes
   numentrys:=Read32b(emuheader+sector+$10);     //Number of entries in this directory
   namesize :=Read32b(emuheader+sector+$14);     //Size of the name heap in bytes
   dirname  :=ReadString(emuheader+sector+$1C,-NameLen);//Directory name
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
   dirtitle:=ReadString(OldDiscAddrToOffset(emuheader+sector+tail+$0E),-19);//Title of the directory
   EndSeq  :=ReadByte(OldDiscAddrToOffset(emuheader+sector+tail+$2F));      //End sequence number to match with start
   EndName :=ReadString(OldDiscAddrToOffset(emuheader+sector+tail+$30),-4); //Hugo or Nick
   dirchk  :=ReadByte(OldDiscAddrToOffset(emuheader+sector+tail+$34));      //Directory Check Byte
  end;
  1:
  begin
   dirtitle:=ReadString(emuheader+sector+tail+$06,-19);//Title of the directory
   EndSeq  :=ReadByte(emuheader+sector+tail+$23);      //End sequence number to match with start
   EndName :=ReadString(emuheader+sector+tail+$24,-4); //Hugo or Nick
   dirchk  :=ReadByte(emuheader+sector+tail+$28);      //Directory Check Byte
  end;
  2:
  begin
   EndName :=ReadString(emuheader+sector+tail+$00,-4); //Should be oven
   EndSeq  :=ReadByte(emuheader+sector+tail+$04);      //End sequence number to match with start
   dirtitle:=dirname;                                  //Does not have a directory title
   dirchk  :=ReadByte(emuheader+sector+tail+$07);      //Directory Check Byte
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
     if ReadByte(OldDiscAddrToOffset(emuheader+offset))<>0 then //0 marks the end of the entries
     begin
      Entry.Filename :=ReadString(OldDiscAddrToOffset(emuheader+offset),-10,True);//Filename (including attributes for old)
      Entry.LoadAddr :=Read32b(OldDiscAddrToOffset(emuheader+offset+$0A));  //Load Address (can be timestamp)
      Entry.ExecAddr :=Read32b(OldDiscAddrToOffset(emuheader+offset+$0E));  //Execution Address (can be filetype)
      Entry.Length   :=Read32b(OldDiscAddrToOffset(emuheader+offset+$12));  //Length in bytes
      Entry.Sector   :=Read24b(OldDiscAddrToOffset(emuheader+offset+$16));   //How to find the file
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
       NewDirAtts   :=ReadByte(emuheader+offset+$19);  //Attributes will be disected with Big
     end
     else validentry:=False;
    2: //Big Directory
    begin
     Entry.LoadAddr :=Read32b(emuheader+offset+$00);  //Load Address
     Entry.ExecAddr :=Read32b(emuheader+offset+$04);  //Execution Address
     Entry.Length   :=Read32b(emuheader+offset+$08);  //Length in bytes
     Entry.Sector   :=Read32b(emuheader+offset+$0C);  //How to find file
     NewDirAtts     :=Read32b(emuheader+offset+$10);  //Attributes (as New)
     NameLen        :=Read32b(emuheader+offset+$14);  //Length of filename
     NameOff        :=Read32b(emuheader+offset+$18);  //Offset into heap of filename
     Entry.Filename :=ReadString(emuheader+sector+nameheap+NameOff,-NameLen); //Filename
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
  offset:=Read32b(emuheader+sector+amt);
  dircheck:=offset XOR ROR13(dircheck);
  inc(amt,4);
 end;
 //Stage 2: The bytes (<4) at the start of the directory are accumulated
 //individually.
 while amt<EndOfChk do
 begin
  offset:=ReadByte(emuheader+sector+amt);
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
  offset:=Read32b(emuheader+sector+amt);
  dircheck:=offset XOR ROR13(dircheck);
  inc(amt,4);
 end;
 //Stage 4a: Big Directories also accumulate the final few bytes, but not the
 //final byte
 if FDirType=2 then
  while amt<dirsize-1 do
  begin
   offset:=ReadByte(emuheader+sector+amt);
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
     id:=ReadBits(emuheader+start,i,idlen);
     //and move the pointer on idlen bits
     inc(i,idlen);
     if id<>0 then
     begin
      //Now find the end of the fragment entry
      j:=i-1;
      repeat
       inc(j);
      until(IsBitSet(ReadByte(emuheader+start+(j div 8)),j mod 8))or(j>=allmap);
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
 UpdateProgress('Reading ADFS catalogue');
 //Read in the header information (that hasn't already been read in during
 //the initial checks
 if FFormat<>$FF then //But only if the format is valid
 begin
  //ADFS Old Map
  if not FMap then
  begin
   //Set up boot option
   SetLength(bootoption,1);
   bootoption[0]:=ReadByte(OldDiscAddrToOffset(emuheader+$1FD));
   //We already found the root when IDing it as ADFS, so now just confirm
   d:=2;
   root:=0;
   //Root size for old map old directory - assume for now
   root_size:=$500;
   repeat
    if (ReadString(OldDiscAddrToOffset(emuheader+(d*$100)+1),-4)='Hugo')
    or (ReadString(OldDiscAddrToOffset(emuheader+(d*$100)+1),-4)='Nick') then
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
    OldName0 :=ReadString(OldDiscAddrToOffset(emuheader+$0F7),-5);
    OldName1 :=ReadString(OldDiscAddrToOffset(emuheader+$1F6),-5);
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
   secsize     :=1 shl ReadByte(emuheader+bootmap+$04);
   secspertrack:=ReadByte(emuheader+bootmap+$05);
   heads       :=ReadByte(emuheader+bootmap+$06);
   density     :=ReadByte(emuheader+bootmap+$07);
   idlen       :=ReadByte(emuheader+bootmap+$08);
   bpmb        :=1 shl ReadByte(emuheader+bootmap+$09);
   skew        :=ReadByte(emuheader+bootmap+$0A);
   bootoption[0]:=ReadByte(emuheader+bootmap+$0B);
   lowsector   :=ReadByte(emuheader+bootmap+$0C);
   nzones      :=ReadByte(emuheader+bootmap+$0D);
   zone_spare  :=Read16b(emuheader+bootmap+$0E);
   root        :=Read32b(emuheader+bootmap+$10);
   disc_size   :=Read32b(emuheader+bootmap+$14);
   disc_id     :=Read16b(emuheader+bootmap+$18);
   disc_name   :=ReadString(emuheader+bootmap+$1A,-10);
   disctype    :=Read32b(emuheader+bootmap+$24);
   //Newer attributes for E+ and F+
   disc_size   :=disc_size+Read32b(emuheader+bootmap+$28)*$100000000;
   share_size  :=1 shl ReadByte(emuheader+bootmap+$2C);
   big_flag    :=ReadByte(emuheader+bootmap+$2D);
   nzones      :=nzones+ReadByte(emuheader+bootmap+$2E)*$100;
   format_vers :=Read32b(emuheader+bootmap+$30);
   root_size   :=Read32b(emuheader+bootmap+$34);
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
  freeend:=ReadByte(OldDiscAddrToOffset(emuheader+$1FE));//Number of entries
  //Continue for as many entries as there is
  while d<freeend do
  begin
   p:=Read24b(OldDiscAddrToOffset(emuheader+d))*$100;      //Pointer
   f:=Read24b(OldDiscAddrToOffset(emuheader+$100+d))*$100; //Size
   //If the read pointer is between the last area, discard
   if p>=ptr then //Just being careful...shouldn't be required
   begin
    //Update the array
    if (secspertrack>0) and (secsize>0) then
     for address:=p to (p+f)-1 do
      ADFSFillFreeSpaceMap(address,$00);
    //Update the pointer to the end of the current area
    ptr:=p+f;
    //Add it to the total
    inc(free_space,f);
   end;
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
   nextfreelink:=(Read16b(emuheader+freelink)AND $7FFF)DIV 8;
   //We will continue while nextfreelink is not $0000
   while nextfreelink<>0 do
   begin
    //We're going to count the length, starting after the nextfreelink
    len:=1;
    repeat
     inc(len)
    until ReadByte(emuheader+freelink+nextfreelink+len)<>0; //Until we find a bit set
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
    nextfreelink:=(Read16b(emuheader+freelink)AND $7FFF)DIV 8;
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
 check: Byte;
 dirid: AnsiString;
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
  WriteByte(ByteCheckSum(emuheader+$0000,$100),$0FF);
  //Old map FreeLen
  Write24b((disc_size-(root+root_size))div$100,$100);
  //Disc ID
  Write24b($4077,$1FB); //Random 16 bit number
  //Old map FreeEnd
  WriteByte($03,$1FE);
  //Checksum
  WriteByte(ByteCheckSum(emuheader+$0100,$100),$1FF);
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
   //$33B is used as the end of the free area because:
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
 if FDirType=0 then //Old Directory
 begin
 //Directory identifier
  dirid:='Hugo';
  for t:=1 to 4 do
  begin
   WriteByte(Ord(dirid[t]),root+t);
   WriteByte(Ord(dirid[t]),$6FA+t);
  end;
  //Directory Name
  WriteByte(Ord('$'),$6CC);
  //Directory Parent
  Write24b($000002,$6D6);
  //Directory Title
  WriteByte(Ord('$'),$6D9);
 end;
 if FDirType=1 then //New Directory
 begin
  dirid:='Nick';
  //Directory identifier
  for t:=1 to 4 do
  begin
   WriteByte(Ord(dirid[t]),root+t);
   WriteByte(Ord(dirid[t]),root+(root_size-6)+t);
  end;
  //Directory Name
  WriteByte(Ord('$'),root+(root_size-$23));
  WriteByte($0D,root+(root_size-$22));
  //Directory Parent
  if not FMap then
   Write24b($000004,root+(root_size-$26))
  else
   Write24b($000209,root+(root_size-$26));
  //Directory Title
  WriteByte(Ord('$'),root+(root_size-$10));
  WriteByte($0D,root+(root_size-$F));
  //Directory Checkbyte
  WriteByte(CalculateADFSDirCheck(root,$05,root_size-$29,root_size),
            root+(root_size-1));
 end;
 if FDirType=2 then //Big Directory
 begin
  dirid:='SBPr';
  //Directory identifier - start
  for t:=1 to 4 do
   WriteByte(Ord(dirid[t]),root+t+$03);
  dirid:='oven';
  //Directory identifier - end
  for t:=1 to 4 do
   WriteByte(Ord(dirid[t]),root+(root_size-$09)+t);
  Write32b($00000001,root+$08); //BigDirNameLen
  Write32b(root_size,root+$0C); //BigDirSize
  Write32b(Read32b(bootmap+$10),root+$18);//BigDirParent
  WriteByte(Ord('$'),root+$1C); //BigDirName
  //Directory Checkbyte
  WriteByte(CalculateADFSDirCheck(root,$1D,root_size-$08,root_size),
            root+(root_size-1));
 end;                      //sector,EndOfChk,tail,dirsize
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
    WriteByte(Ord(title[t]),emuheader+$F7+(t DIV 2));
   //Part 2 at $1F6
   if t mod 2=0 then
    WriteByte(Ord(title[t]),emuheader+$1F6+((t-1)DIV 2));
  end;
  //Checksum on first sector
  WriteByte(ByteCheckSum(emuheader+$0000,$100),$0FF);
  //Checksum on second sector
  WriteByte(ByteCheckSum(emuheader+$0100,$100),$1FF);
 end;
 if FMap then //New Map
 begin
  //Disc name is held at $16 of the disc record, after $4 bytes zone header
  for t:=1 to 10 do
   WriteByte(Ord(title[t]),emuheader+bootmap+$16+$4+(t-1));
  //Checksum for zone 0
  WriteByte(GeneralChecksum(emuheader+bootmap+$00,secsize,secsize+4,$4,true),
             emuheader+bootmap);
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
  WriteByte(option,emuheader+$1FD);
  //Checksum
  WriteByte(ByteCheckSum(emuheader+$0100,$100),$1FF);
 end;
 if FMap then //New Map
 begin
  //Boot option is at offset $07 of the disc record, after the $4 zone header
  WriteByte(option,emuheader+bootmap+$07+$04);
  //Checksum for zone 0
  WriteByte(GeneralChecksum(emuheader+bootmap+$00,secsize,secsize+4,$4,true),
             emuheader+bootmap);
 end;
 //Return a success
 Result:=True;
end;
