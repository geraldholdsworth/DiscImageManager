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
 ctr,ds,
 dr_size,
 dr_ptr,
 zone     : Cardinal;
begin
 Result:=False;
 if FFormat=diInvalidImg then
 begin
  ResetVariables;
  //Interleaving, depending on the option
  Finterleave:=FForceInter;
  if Finterleave=0 then Finterleave:=2; //Auto, so pick INT for ADFS
  //Is there actually any data?
  if GetDataLength>0 then
  begin
   //Check for Old Map
   Check0   :=ReadByte($0FF);
   Check1   :=ReadByte($1FF);
   Check0a  :=ByteCheckSum($0000,$100,False);
   Check1a  :=ByteCheckSum($0100,$100,False);
   //Do the checksums on both sectors
   if  (Check0a=Check0)
   and (Check1a=Check1) then
   begin
    //Checks are successful, now find out which type of disc: S/M/L/D
    Result:=True;
    //FFormat:=$1F; //Default to ADFS Hard drive
    FMap:=False;  //Set to old map
    FDirType:=diADFSOldDir;  //Set to old directory
    //Check where the root is.
    if (Read24b($6D6)=$000002) //Address of the root ($200 for old dir)
    and(ReadByte($200)=ReadByte($6FA)) then //Directory check bytes
     FDirType:=diADFSOldDir; //old map, old directory - either S, M or L
    if (Read24b($BDA)=$000004) //Address of the root ($400 for new dir)
    and(ReadByte($400)=ReadByte($BFA)) then //Directory check bytes
    begin
     FDirType:=diADFSNewDir; //So, old map, new directory must be ADFS D
     FFormat:=diAcornADFS<<4+$03;
    end;
    disc_size[0]:=Read24b($0FC)*$100;
    //The above checks will pass through if the first 512 bytes are all zeros,
    //meaning a, e.g., Commodore 64 image will be IDed as an ADFS Old Map.
    //So, we need to check the disc size is not zero also.
    if disc_size[0]=0 then
    begin
     Result:=False;
     ResetVariables;
    end;
    if(disc_size[0]>0)and(FFormat=diInvalidImg)then
    begin
     //Not a reliable way of determining disc shape. However, there are three
     //different sizes of the same format.
     ctr:=0;
     while(FFormat=diInvalidImg)and(ctr<2)do
     begin
      //First check the size as recorded
      ds:=disc_size[0];
      //If that fails, we'll check the total data length
      if ctr=1 then ds:=GetDataLength;
      case ds of
       163840: FFormat:=diAcornADFS<<4+$00; // ADFS S
       327680: FFormat:=diAcornADFS<<4+$01; // ADFS M
       655360: FFormat:=diAcornADFS<<4+$02; // ADFS L
      end;
      //800K size was IDed earlier, but anything above will be HDD
      if(ds>819200)and(FFormat=diInvalidImg)then
       FFormat:=diAcornADFS<<4+$0F; //Hard drive
      //Next step
      inc(ctr);
      //Unless we have found a format
      if FFormat<>diInvalidImg then ctr:=2;
     end;
     //Do we have an ADFS L, but reported size is $000AA0?
     if(FFormat=diAcornADFS<<4+2)and(Read24b($0FC)=$000AA0)
     and(ReadString($6FB,-4)='Hugo')then
     begin
      FDOSPresent:=True;
      //Automatic interleave?
      if FForceInter=0 then
      begin
       //If this is not as expected, then change the interleave
       if(ReadByte($1000)<>$FF)or(Read16b($1001)<>$FFFF)then
       begin
        if FInterleave=2 then FInterleave:=1 else FInterleave:=2;
        //Still not as expected? Change back and clear the flag
        if(ReadByte($1000)<>$FF)or(Read16b($1001)<>$FFFF)then
        begin
         if FInterleave=2 then FInterleave:=1 else FInterleave:=2;
         FDOSPresent:=False;
        end;
       end;
      end;
      //If, at this stage, the flag is set, reset the disc size to 4K.
      if FDOSPresent then disc_size[0]:=$1000;
     end;
     //Do we still have a mystery size? We'll ignore 800K as this was set earlier
     if(FFormat=diInvalidImg)and(disc_size[0]<819200)and(GetDataLength<819200)then
      FFormat:=diAcornADFS<<4+$0E; //Mark it as an unknown shape
     //Check for AFS Level 3 partition
     //Value at $0F6 must be zero for normal ADFS, so if non zero will be AFS
     FAFSPresent:=ReadByte($0F6)<>0;
     //Check that the root is valid - could be AFS with no ADFS
     if FAFSPresent then //We'll just check the end name, and set to AFS L3
      if ReadString($6FB,-4)<>'Hugo' then FFormat:=diAcornFS<<4+2;
    end;
    if(GetMinorFormatNumber<3)or(FFormat=diAcornADFS<<4+$0E)then //ADFS S,M,L or unknown
    begin
     //Set the number of sectors per track - this is not held in the disc
     secspertrack:= 16;
     //Size of the sectors in bytes
     secsize     :=256;
    end;
    if GetMinorFormatNumber=3 then //ADFS D
    begin
     //Set the number of sectors per track - this is not held in the disc
     secspertrack:=   5;
     //Size of the sectors in bytes
     secsize     :=1024;
    end;
    if GetMinorFormatNumber=$F then //ADFS Hard drive
    begin
     secspertrack:= 16;
     secsize     :=256;
     //Make sure that we get a whole number of sectors on every track
     if disc_size[0]mod(secspertrack*secsize)>0 then
      secspertrack:=Round(secspertrack
                   *((disc_size[0]/  (secspertrack*secsize))
                   - (disc_size[0]div(secspertrack*secsize))));
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
      secsize   :=1<<ReadByte(dr_ptr+$00); //Sector size
      secspertrack:=ReadByte(dr_ptr+$01);  //Sectors per track
      heads     :=ReadByte(dr_ptr+$02);    //Number of heads
      idlen     :=ReadByte(dr_ptr+$04);       //idlen
      bpmb      :=1<<ReadByte(dr_ptr+$05); //Bits per map bit
      nzones    :=ReadByte(dr_ptr+$09)
                 +ReadByte(dr_ptr+$2A)*$100;  //nzones is 2 bytes, for E+ and F+
      zone_spare:=Read16b(dr_ptr+$0A);        //Zone spare bits
      rootfrag  :=Read32b(dr_ptr+$0C);        //Indirect address of root
      root_size :=Read32b(dr_ptr+$30);        //Size of root (big dir only)
      if root_size=0 then root_size:=$800;    //Not big map
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
      if ByteChecksum($0C00,$200,True)<>Check0 then Result:=False;
     end;
     inc(ctr);
    until (Result) or (ctr=3);
    if Result then
    begin
     case ctr of
      1: FFormat:=diAcornADFS<<4+$04; //ADFS E/E+
      2: FFormat:=diAcornADFS<<4+$06; //ADFS F/F+
      3: FFormat:=diAcornADFS<<4+$0F; //ADFS Hard Drive
     end;
     //Boot block checksum, if there is a partial disc record at $0DC0
     if dr_ptr=$DC0 then
     begin
      Check0 :=ByteChecksum($C00,$200,True);
      Check0a:=ReadByte($DFF);
      if Check0<>Check0a then FFormat:=diInvalidImg; //Checksums do not match
     end;
    end;
    //Check for type of directory, and change the format if necessary
    if FFormat<>diInvalidImg then
    begin
     FDirType:=diADFSNewDir; //New Directory
     //Determine if it is a '+' format by reading the version flag
     if ReadByte(dr_ptr+$2C)>0 then
     begin
      if FFormat<>diAcornADFS<<4+$0F then inc(FFormat);
      FDirType:=diADFSBigDir;
     end;
     //Root address for old map
     if FDirType=diADFSOldDir then
     begin
      root:=$200;
      root_size:=1280;
     end;
     if(FDirType=diADFSNewDir)and(not FMap)then
     begin
      root:=$400;
      root_size:=2048;
     end;
    end;
   end;
   //Check for DOS partition on ADFS Hard drives
   if(FFormat=diAcornADFS<<4+$F)and(not FAFSPresent)and(not FDOSPresent)
   and(FDirType=diADFSOldDir)and(FOpenDOSPart)then
   begin
    //Start at the root
    ctr:=root;
    ds:=GetDataLength;
    while(ctr<=ds)and(not FDOSPresent)do
    begin
     //Is there one here?
     IDDOSPartition(ctr);
     //Next sector
     inc(ctr,$100);
    end;
   end;
  end;
  //Return a true or false
  Result:=GetMajorFormatNumber=diAcornADFS;
  if Result then root_name:='$';
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
 dirbuffer          : TDIByteArray;
const
 //Attributes - not to be confused with what is returned from OSFILE
 //See the function GetAttributes in DiscImageUtils
 OldAtts: array[0..9] of Char = ('R','W','L','D','E','r','w','e','P',' ');
 NewAtts: array[0..5] of Char = ('R','W','L','D','r','w');
begin
 SetLength(dirbuffer,0);
 RemoveControl(dirname);
 //This is only here to stop the hints that Result isn't intialised
 Result.Directory:=dirname;
 //Reset the Result TDir to default values
 ResetDir(Result);
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
 //Reset the Partition flags
 Result.AFSPartition:=False;
 Result.DOSPartition:=False;
 //Set the sector
 Result.Sector:=sector;
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
 StartName       :='';
 EndName         :='';
 SetLength(addr,0);
 //Get the offset address
 if FMap then
 begin
  //New Map, so the sector will be an internal disc address
  if dirname=root_name then //root address
  begin
   if rootfrag=sector then addr:=NewDiscAddrToOffset(rootfrag)
   else
   begin
    SetLength(addr,1);
    addr[0].Offset:=sector;
    addr[0].Length:=root_size;
   end;
   Result.Sector:=rootfrag;
   dirsize:=addr[0].Length;
  end
  else                      //other object address
   addr:=NewDiscAddrToOffset(sector);
  //We need the total length of the big directory
  if Length(addr)>0 then
  begin
   if FDirType=diADFSOldDir then dirsize:=1280;
   if FDirType=diADFSNewDir then dirsize:=2048;
   if FDirType=diADFSBigDir then
    for amt:=0 to Length(addr)-1 do inc(dirsize,addr[amt].Length);
  end;
 end
 else
 begin
  //But we need it as an offset into the data, but set up as fragments
  SetLength(addr,1);
  //Is Old Map, so offset is just the sector * $100
  addr[0].Offset:=sector*$100;
  //Length - old and new type directories are fixed length
  if FDirType=diADFSOldDir then addr[0].Length:=1280;
  if FDirType=diADFSNewDir then addr[0].Length:=2048;
  //But big type directories the length varies - we worked this out above
  dirsize:=addr[0].Length;
 end;
 Result.Length:=dirsize;
 //Read the entire directory into a buffer
 if ExtractFragmentedData(addr,dirsize,dirbuffer) then
 begin
  sector:=0;
  //Read in the directory header
  case FDirType of
   diADFSOldDir,diADFSNewDir: //Old and New Directory
   begin
    StartSeq :=ReadByte(0,dirbuffer);        //Start Sequence Number to match with end
    StartName:=ReadString(1,-4,dirbuffer); //Hugo or Nick
    if FDirType=diADFSOldDir then //Old Directory
    begin
     numentrys:=47;                     //Number of entries per directory
     dirsize  :=1280;                   //Directory size in bytes
     tail     :=$35;                    //Size of directory tail
    end;
    if FDirType=diADFSNewDir then //New Directory
    begin
     numentrys:=77;                     //Number of entries per directory
     dirsize  :=2048;                   //Directory size in bytes
     tail     :=$29;                    //Size of directory tail
    end;
    entrys   :=$05;                     //Pointer to entries, from sector
    entrysize:=$1A;                     //Size of each entry
   end;
   diADFSBigDir:   //Big Directory
   begin
    StartSeq :=ReadByte(0,dirbuffer);         //Start sequence number to match with end
    StartName:=ReadString($04,-4,dirbuffer);//Should be SBPr
    NameLen  :=Read32b($08,dirbuffer);     //Length of directory name
    dirsize  :=Read32b($0C,dirbuffer);     //Directory size in bytes
    numentrys:=Read32b($10,dirbuffer);     //Number of entries in this directory
    namesize :=Read32b($14,dirbuffer);     //Size of the name heap in bytes
    dirname  :=ReadString($1C,-NameLen,dirbuffer);//Directory name
    entrys   :=(($1C+NameLen+1+3)div 4)*4;         //Pointer to entries, from sector
    tail     :=$08;                                //Size of directory tail
    entrysize:=$1C;                                //Size of each entry
    nameheap :=entrys+numentrys*entrysize;         //Offset of name heap
   end;
  end;
  //Now we know the size of the directory, we can read in the tail
  tail:=dirsize-tail;
  //And mark it on the Free Space Map
  for amt:=0 to dirsize do ADFSFillFreeSpaceMap(amt,$FD);
  //Not all of the tail is read in
  case FDirType of
   diADFSOldDir:
   begin
    dirtitle:=ReadString(tail+$0E,-19,dirbuffer);//Title of the directory
    EndSeq  :=ReadByte(tail+$2F,dirbuffer);      //End sequence number to match with start
    EndName :=ReadString(tail+$30,-4,dirbuffer); //Hugo or Nick
    dirchk  :=ReadByte(tail+$34,dirbuffer);      //Directory Check Byte
   end;
   diADFSNewDir:
   begin
    dirtitle:=ReadString(tail+$06,-19,dirbuffer);//Title of the directory
    EndSeq  :=ReadByte(tail+$23,dirbuffer);      //End sequence number to match with start
    EndName :=ReadString(tail+$24,-4,dirbuffer); //Hugo or Nick
    dirchk  :=ReadByte(tail+$28,dirbuffer);      //Directory Check Byte
   end;
   diADFSBigDir:
   begin
    EndName :=ReadString(tail+$00,-4,dirbuffer); //Should be oven
    EndSeq  :=ReadByte(tail+$04,dirbuffer);      //End sequence number to match with start
    dirtitle:=dirname;                        //Does not have a directory title
    dirchk  :=ReadByte(tail+$07,dirbuffer);      //Directory Check Byte
   end;
  end;
  //Save the directory title
  Result.Title:=dirtitle;
  //Check for broken directory
  //This can result in having a valid directory structure, but a broken directory
  //ADFS normally refuses to list broken directories, but we will list them anyway,
  //just marking the directory as broken and return an error code
  Result.ErrorCode:=0;
  //Start and End sequence numbers do not match
  if EndSeq<>StartSeq then
   Result.ErrorCode:=Result.ErrorCode OR $01;
  if FDirType<diADFSBigDir then
   if StartName<>EndName then//Start and End names do not match (Hugo or Nick)
    Result.ErrorCode:=Result.ErrorCode OR $02
   else //Start and End names are not valid for Old or New Directories
   if((StartName<>'Hugo')and(StartName<>'Nick'))
   or((EndName<>'Hugo')and(EndName<>'Nick'))then
    Result.ErrorCode:=Result.ErrorCode OR $40;
  //Start and End names are not valid for Big Directories
  if(FDirType=diADFSBigDir)and((StartName<>'SBPr') or (EndName<>'oven'))then
   Result.ErrorCode:=Result.ErrorCode OR $04;
  //Not sector aligned
  if sector mod secsize<>0 then
   Result.ErrorCode:=Result.ErrorCode OR $20;
  Result.Broken:=Result.ErrorCode<>$00;
  //Check for valid directory
  //We won't try and get the directory structure if it appears that it is invalid
  //Could just be that one of the names has got corrupt, but could be much worse
  validdir:=False;
  if((FDirType<diADFSBigDir)and(StartName=EndName)and((StartName='Hugo')or(StartName='Nick')))
  or((FDirType=diADFSBigDir)and(StartName='SBPr')and(EndName='oven'))then
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
    offset:=entrys+ptr*entrysize;
    //Blank the entries
    ResetDirEntry(Entry);
    //Sometimes the pathname has the root missing
    if pathname[1]=dir_sep then pathname:=root_name+pathname;
    Entry.Parent:=pathname;
    //Read in the entries
    case FDirType of
     diADFSOldDir,diADFSNewDir: //Old and New Directory
      if ReadByte(offset,dirbuffer)<>0 then //0 marks the end of the entries
      begin
       Entry.Filename :=ReadString(offset,-10,dirbuffer,True);//Filename (including attributes for old)
       Entry.LoadAddr :=Read32b(offset+$0A,dirbuffer);  //Load Address (can be timestamp)
       Entry.ExecAddr :=Read32b(offset+$0E,dirbuffer);  //Execution Address (can be filetype)
       Entry.Length   :=Read32b(offset+$12,dirbuffer);  //Length in bytes
       Entry.Sector   :=Read24b(offset+$16,dirbuffer);  //How to find the file
       temp:='';
       //Old directories - attributes are in the filename's top bit
       if FDirType=diADFSOldDir then
       begin
        endofentry:=False;
        if Length(Entry.Filename)>0 then
        begin
         for amt:=0 to 9 do
         begin
          //if ord(Entry.Filename[amt+1])>>7=1 then
          if ReadByte(offset+amt,dirbuffer)>>7=1 then
           temp:=temp+OldAtts[amt];
          if amt<Length(Entry.Filename) then
          begin
           if ord(Entry.Filename[amt+1])AND$7F=$0D then endofentry:=True;
           //Clear the top bit
           if not endofentry then
            Entry.Filename[amt+1]:=chr(ord(Entry.Filename[amt+1])AND$7F)
           else
            Entry.Filename[amt+1]:=' ';
          end;
         end;
         RemoveSpaces(Entry.Filename);
        end;
        //Reverse the attribute order to match actual ADFS
        if Length(temp)>0 then
         for amt:=Length(temp) downto 1 do
          Entry.Attributes:=Entry.Attributes+temp[amt];//Attributes
       end;
       //New directories - attributes are separate, so filenames can have top bit set
       if FDirType=diADFSNewDir then
        NewDirAtts   :=ReadByte(offset+$19,dirbuffer);  //Attributes will be disected with Big
      end
      else validentry:=False;
     diADFSBigDir: //Big Directory
     begin
      Entry.LoadAddr :=Read32b(offset+$00,dirbuffer);  //Load Address
      Entry.ExecAddr :=Read32b(offset+$04,dirbuffer);  //Execution Address
      Entry.Length   :=Read32b(offset+$08,dirbuffer);  //Length in bytes
      Entry.Sector   :=Read32b(offset+$0C,dirbuffer);  //How to find file
      NewDirAtts     :=Read32b(offset+$10,dirbuffer);  //Attributes (as New)
      NameLen        :=Read32b(offset+$14,dirbuffer);  //Length of filename
      NameOff        :=Read32b(offset+$18,dirbuffer);  //Offset into heap of filename
      Entry.Filename :=ReadString(nameheap+NameOff,-NameLen,dirbuffer); //Filename
     end;
    end;
    RemoveControl(Entry.Filename);
    //Attributes for New and Big
    if FDirType>diADFSOldDir then
    begin
     temp:='';
     for amt:=0 to 5 do
      if IsBitSet(NewDirAtts,amt) then temp:=temp+NewAtts[amt];
     //Reverse the attribute order to match actual ADFS
     if Length(temp)>1 then
      for amt:=Length(temp) downto 1 do
       Entry.Attributes:=Entry.Attributes+temp[amt];
     if Length(temp)=1 then Entry.Attributes:=temp;
    end;
    //If we have a valid entry then we can see if it is filetyped/datestamped
    //and add it to the list
    if validentry then
    begin
     //RISC OS - file may be datestamped and filetyped
     ADFSCalcFileDate(Entry);
     //Not a directory - default. Will be determined later
     Entry.DirRef:=-1;
     //Is this entry the DOS partition?
     if doshead=Entry.Sector*secsize then Entry.IsDOSPart:=True;
     //Add to the result
     SetLength(Result.Entries,Length(Result.Entries)+1);
     Result.Entries[Length(Result.Entries)-1]:=Entry;
     //Move on to next
     inc(ptr);
    end;
   end;
   //Now we can run the directory check on DirCheckByte
   //But only for New and Big Directories, optional for old (ignored if zero)
   if((FDirType=diADFSOldDir)and(dirchk<>0))or(FDirType>diADFSOldDir)then
   begin
    //This value is the check byte.
    dircheck:=CalculateADFSDirCheck(0,dirbuffer);
    //Compare with what is stored
    if dirchk<>dircheck then
    begin
     //If different, just mark as broken directory
     Result.Broken:=True;
     Result.ErrorCode:=Result.ErrorCode OR $08;
    end;
   end;
   Result.BeenRead:=True;
  end
  else
  begin //Could not be read in for some other reason
   if not Result.Broken then Result.ErrorCode:=Result.ErrorCode OR $10;
   Result.Broken:=True;
  end;
 end;
 if Result.Broken then inc(brokendircount);
end;

{-------------------------------------------------------------------------------
Convert a load and execution address to filetype and date/time
-------------------------------------------------------------------------------}
procedure TDiscImage.ADFSCalcFileDate(var Entry: TDirEntry);
var
 temp: String;
 rotd: Int64;
begin
 //Only valid for New and Big directories in ADFS or SparkFS
 if((GetMajorFormatNumber=diAcornADFS)and((FDirType=diADFSNewDir)or(FDirType=diADFSBigDir)))
 or(GetMajorFormatNumber=diSpark)then
  if Entry.LoadAddr>>20=$FFF then //Only if the top 12 bits are set
  begin
   //Get the 12 bit filetype
   temp:=IntToHex((Entry.LoadAddr AND$000FFF00)>>8,3);
   Entry.Filetype:=GetFiletypeFromNumber(StrToInt('$'+temp));
   Entry.ShortFiletype:=temp;
   //Now sort the timestamp
   rotd:=Entry.LoadAddr AND$FF; //Convert to 64 bit integer
   rotd:=(rotd<<32)OR Entry.ExecAddr; //Shift to the left and add the rest
   Entry.TimeStamp:=RISCOSToTimeDate(rotd);
  end;
end;

{-------------------------------------------------------------------------------
Calculate the directory check byte
-------------------------------------------------------------------------------}
function TDiscImage.CalculateADFSDirCheck(sector:Cardinal): Byte;
begin
 Result:=CalculateADFSDirCheck(sector,nil);
end;
function TDiscImage.CalculateADFSDirCheck(sector:Cardinal;buffer:TDIByteArray): Byte;
var
 dircheck,
 amt,
 offset,
 tail,
 dirsize,
 EndOfChk,
 numentrys : Cardinal;
begin
 EndOfChk:=0;
 tail    :=0;
 dirsize :=0;
 //Set up variables
 if FDirType=diADFSOldDir then  //Old Directory
 begin
  dirsize:=1280;
  tail:=dirsize-$35;
 end;
 if FDirType=diADFSNewDir then  //New Directory
 begin
  dirsize:=2048;
  tail:=dirsize-$29;
 end;
 if FDirType<diADFSBigDir then  //Old or New Directory
 begin
  //Count the number of entries
  numentrys:=0;
  while ReadByte(sector+$05+numentrys*$1A,buffer)<>0 do inc(numentrys);
  EndOfChk:=numentrys*$1A+$05;
 end;
 if FDirType=diADFSBigDir then  //Big Directory
 begin
  //Need to do some more calculation for the end of check figure
  dirsize:=Read32b(sector+$0C,buffer);
  tail:=dirsize-$08;
  numentrys:=Read32b(sector+$10,buffer);
  EndOfChk:=((($1C+Read32b(sector+$08,buffer)+1+3)div 4)*4)
            +(numentrys*$1C)
            +Read32b(sector+$14,buffer);
 end;
 //This has virtually the same loop repeated 5 times - but it is less code to
 //do it like this, than a single loop with if...then conditions to determine
 //the different iterations.
 dircheck:=0;
 amt:=0;
 //Stage 1: All the whole words at the start of the directory are accumulated
 while amt+3<EndOfChk do
 begin
  offset:=Read32b(sector+amt,buffer);
  dircheck:=offset XOR ROR13(dircheck);
  inc(amt,4);
 end;
 //Stage 2: The bytes (<4) at the start of the directory are accumulated
 //individually.
 while amt<EndOfChk do
 begin
  offset:=ReadByte(sector+amt,buffer);
  dircheck:=offset XOR ROR13(dircheck);
  inc(amt);
 end;
 //Stage 3: The first byte at the beginning of the directory tail is skipped.
 amt:=tail;
 //But not with Big Directories
 if FDirType<diADFSBigDir then inc(amt);
 //Stage 4: The whole words in the directory tail are accumulated, except the
 //very last word which is excluded as it contains the check byte.
 while amt+3<dirsize-4 do
 begin
  offset:=Read32b(sector+amt,buffer);
  dircheck:=offset XOR ROR13(dircheck);
  inc(amt,4);
 end;
 //Stage 4a: Big Directories also accumulate the final few bytes, but not the
 //final byte
 if FDirType=diADFSBigDir then
  while amt<dirsize-1 do
  begin
   offset:=ReadByte(sector+amt,buffer);
   dircheck:=offset XOR ROR13(dircheck);
   inc(amt);
  end;
 //Stage 5: The accumulated word has its four bytes exclusive ORd (EOR) together.
 Result  :=(dircheck     AND$FF)
      XOR ((dircheck>>24)AND$FF)
      XOR ((dircheck>>16)AND$FF)
      XOR ((dircheck>> 8)AND$FF);
end;

{-------------------------------------------------------------------------------
Convert an ADFS New Map address to buffer offset address, with fragment lengths
-------------------------------------------------------------------------------}
function TDiscImage.NewDiscAddrToOffset(addr: Cardinal;
                                           offset:Boolean=True): TFragmentArray;
var
 i,j,sector,id,
 allmap,len,off,
 zone,start,
 start_zone,
 zonecounter,
 fragid,
 id_per_zone     : Cardinal;
const
 dr_size = $40; //Size of disc record + header (zone 0)
 header  = 4;   //Size of zone header only (zones >0)
begin
 //Reset the result
 Result:=nil;
 SetLength(Result,0);
 if FMap then //Only works for new maps
 begin
  if(addr=0)or(addr=rootfrag)then //Root
  begin
   //We've been given the address of the root, but we know where this is so no
   //need to calculate it.
   SetLength(Result,1);
   Result[0].Offset:=bootmap+(nzones*secsize*2);
   case FDirType of
    diADFSOldDir: Result[0].Length:=$500;
    diADFSNewDir: Result[0].Length:=$800;
    diADFSBigDir: Result[0].Length:=root_size;
   end;
  end
  else
  begin
   //Extract the fragment ID part of the address
   fragid:=(addr div $100)mod(1<<idlen);
   //Calculate the sector offset
   sector:=addr mod $100;
   //Sector needs to have 1 subtracted, if >=1
   if sector>=1 then dec(sector);
   //Go through the allocation map, looking for the fragment
   //First we need to know how many ids per zone there are (max)
   id_per_zone:=((secsize*8)-zone_spare)div(idlen+1);
   //Then work out the start zone
   start_zone:=((addr DIV $100)mod(1<<idlen))div id_per_zone;
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
     if id=fragid then
     begin
      if offset then //Offset as image file offset
       off:=((off-(zone_spare*zone))*bpmb) mod disc_size[0]
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
      SetLength(Result,Length(Result)+1);
      Result[Length(Result)-1].Offset:=off;
      //Save the length
      Result[Length(Result)-1].Length:=len;
      //Save the zone
      Result[Length(Result)-1].Zone  :=zone;
     end;
     inc(i);
    until i>=allmap;
   end;
   //If offsets have been called for, then convert them
   if Length(Result)>0 then
    if offset then
     for i:=0 to Length(Result)-1 do
      Result[i].Offset:=(Result[i].Offset+(sector*secsize));
   //Root indirect address
   if(addr=rootfrag)and(Length(Result)>1)and(nzones>1)
   and(Result[0].Offset=sector*secsize)then
   begin
    for i:=1 to Length(Result) do
     Result[i-1]:=Result[i];
    SetLength(Result,Length(Result)-1);
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Calculate disc address given the offset into image (Interleave)
-------------------------------------------------------------------------------}
function TDiscImage.OffsetToOldDiscAddr(offset: Cardinal): Cardinal;
var
 track_size,
 track,
 side,
 data_offset : Cardinal;
const
 tracks   = 80;
 oldheads = 2;
begin
 Result:=offset;
 //ADFS L or AFS with 'INT' option
 if((FFormat=diAcornADFS<<4+$02)or(GetMajorFormatNumber=diAcornFS))
 and(Finterleave=2)then
 begin
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
function TDiscImage.ByteChecksum(offset,size: Cardinal;newmap: Boolean): Byte;
var
 buffer: TDIByteArray;
begin
 SetLength(buffer,0);
 Result:=ByteChecksum(offset,size,newmap,buffer);
end;
function TDiscImage.ByteChecksum(offset,size: Cardinal;newmap: Boolean;
                                                 var buffer:TDIByteArray): Byte;
var
 acc,
 pointer: Cardinal;
 carry : Byte;
begin
 //Reset the accumulator
 //This should be 255 for the FSM in Old Map, but zero for New Map boot block
 if newmap then acc:=0 //New
 else acc:=$FF;        //Old
 //We can't used the global FMap, as this may not have been defined yet
 //Iterate through the block, ignoring the final byte (which contains the
 //checksum)
 for pointer:=size-2 downto 0 do
 begin
  //Make a note of the carry
  carry:=acc div $100;
  //and ensure the accumulator is <=255
  acc:=acc and $FF;
  //Add each byte to the accumulator, along with the carry
  inc(acc,ReadByte(offset+pointer,buffer)+carry);
 end;
 //Return an 8 bit number
 Result:=acc and $FF;
end;

{-------------------------------------------------------------------------------
Read ADFS Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadADFSDisc: Boolean;
 function ReadTheADFSDisc: Boolean;
 type
   TVisit = record
     Sector : Cardinal;
     Name   : String;
   end;
 var
  d,ptr,i  : Cardinal;
  OldName0,
  OldName1 : String;
  addr     : TFragmentArray;
  visited  : array of TVisit;
 begin
  //Initialise some variables
  root   :=$00; //Root address (set to zero so we can id the disc)
  FDisc:=nil;
  Result:=False;
  SetLength(FDisc,0);
  brokendircount:=0;
  //Read in the header information (that hasn't already been read in during
  //the initial checks
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
    if(ReadString((d*$100)+1,-4)='Hugo')
    or(ReadString((d*$100)+1,-4)='Nick')then
     root:=d;
    inc(d);
   until(d=(disc_size[0]div$100)-1)or(root>0);
   if root=0 then //Failed to find root, so reset the format
   begin
    ResetVariables;
    //Now, let's see if it is an AFS
    if ID_AFS then exit else ResetVariables;
   end
   else
   begin
    //Set the root size for old map new directory
    if FDirType=diADFSNewDir then root_size:=$800;
    //Get the two parts of the disc title
    OldName0 :=ReadString($0F7,-5);
    OldName1 :=ReadString($1F6,-5);
    //Start with a blank title
    disc_name[0]:='          ';
    //Disc title
    if not FAFSPresent then //AFS partition present, so skip this
    begin
     //Re-assemble the disc title
     if Length(OldName0)>0 then
      for d:=1 to Length(OldName0) do
       disc_name[0][(d*2)-1]  :=chr(ord(OldName0[d])AND$7F);
     //Both parts are interleaved
     if Length(OldName1)>0 then
      for d:=1 to Length(OldName1) do
       disc_name[0][ d*2   ]  :=chr(ord(OldName1[d])AND$7F);
     //Then remove all extraenous spaces
     RemoveSpaces(disc_name[0]);
    end else disc_name[0]:='AFS L3';
   end;
  end;
  //ADFS New Map
  if FMap then
  begin
   SetLength(bootoption,1);
   //Disc description starts at offset 4 and is 60 bytes long
   //Not all of these values will be used
   secsize     :=1<<ReadByte(bootmap+$04);
   secspertrack:=ReadByte(bootmap+$05);
   heads       :=ReadByte(bootmap+$06);
   density     :=ReadByte(bootmap+$07);
   idlen       :=ReadByte(bootmap+$08);
   bpmb        :=1<<ReadByte(bootmap+$09);
   skew        :=ReadByte(bootmap+$0A);
   bootoption[0]:=ReadByte(bootmap+$0B);
   lowsector   :=ReadByte(bootmap+$0C);
   nzones      :=ReadByte(bootmap+$0D);
   zone_spare  :=Read16b(bootmap+$0E);
   rootfrag    :=Read32b(bootmap+$10);
   disc_size[0]:=Read32b(bootmap+$14);
   disc_id     :=Read16b(bootmap+$18);
   disc_name[0]:=ReadString(bootmap+$1A,-10);
   disctype    :=Read32b(bootmap+$24);
   //Newer attributes for E+ and F+
   disc_size[0]:=disc_size[0]+Read32b(bootmap+$28)*$100000000;
   share_size  :=254*secsize*(1<<ReadByte(bootmap+$2C));
   big_flag    :=ReadByte(bootmap+$2D);
   nzones      :=nzones+ReadByte(bootmap+$2E)*$100;
   format_vers :=Read32b(bootmap+$30);
   root_size   :=Read32b(bootmap+$34);
   if root_size=0 then root_size:=$800;
   //The root does not always follow the map
   addr:=NewDiscAddrToOffset(rootfrag);
   //So, find it - first reset it
   root:=0;
   if Length(addr)>0 then
   begin
    //The above method removes the initial system fragment, if present
    if Length(addr)>1 then
    begin
     i:=0;
     while i<=Length(addr)-1 do
     begin
      if addr[i].Length=nzones*secsize*2+root_size then
       root:=addr[i].Offset;
      inc(i);
     end;
    end
    else root:=addr[0].Offset;
    //This failed to find it, so 'guess' - we'll assume it is after the bootmap
    if root=0 then
     for d:=Length(addr)-1 downto 0 do
      if addr[d].Offset>bootmap then root:=addr[d].Offset;
   end;
   //Failed to find, so resort to where we expect to find it
   if root=0 then root:=bootmap+(nzones*secsize*2);
   //Update the Format, now we know the disc size
   if disc_size[0]>1638400 then FFormat:=diAcornADFS<<4+$0F;
   //Make the disc title easier to work with
   RemoveSpaces(disc_name[0]); //Remove spaces
   RemoveTopBit(disc_name[0]); //Remove top-bit set characters
  end;
  if root>$00 then //If root is still $00, then we have failed to id the disc
  begin
   //Create an entry for the root
   SetLength(FDisc,1);
   //Blank the values
   ResetDir(FDisc[0]);
   //Calculate the Free Space Map
   ADFSFreeSpaceMap;
   //Read the root
   FDisc[0]:=ReadADFSDir(root_name,root);
   //Now iterate through the entries and find the sub-directories
   d:=0;
   //Add the root as a visited directory
   SetLength(visited,1);
   visited[0].Sector:=root;
   visited[0].Name:=root_name;
   repeat
    //If there are actually any entries
    if Length(FDisc[d].Entries)>0 then
    begin
     //Go through the entries
     for ptr:=0 to Length(FDisc[d].Entries)-1 do
     begin
      //Make sure we haven't seen this before. If a directory references a higher
      //directory we will end up in an infinite loop.
      if Length(visited)>0 then
       for i:=0 to Length(visited)-1 do
        if(visited[i].Sector=FDisc[d].Entries[ptr].Sector)
        and(visited[i].Name=FDisc[d].Entries[ptr].Filename)then
         FDisc[d].Entries[ptr].Filename:='';//Blank off the filename so we can remove it later
      //And add them if they are valid
      if FDisc[d].Entries[ptr].Filename<>'' then
      begin
       //Attribute has a 'D', so drill down
       if Pos('D',FDisc[d].Entries[ptr].Attributes)>0 then
       begin
        //Once found, list their entries
        SetLength(FDisc,Length(FDisc)+1);
        //Read in the contents of the directory
        if FScanSubDirs then
         FDisc[Length(FDisc)-1]:=ReadADFSDir(GetParent(d)
                                              +dir_sep
                                              +FDisc[d].Entries[ptr].Filename,
                                               FDisc[d].Entries[ptr].Sector);
        FDisc[Length(FDisc)-1].Parent:=d;
        //Remember it
        SetLength(visited,Length(visited)+1);
        visited[Length(visited)-1].Sector:=FDisc[d].Entries[ptr].Sector;
        visited[Length(visited)-1].Name:=FDisc[d].Entries[ptr].Filename;
        //Update the directory reference
        FDisc[d].Entries[ptr].DirRef:=Length(FDisc)-1;
       end;
      end;
     end;
    end;
    inc(d);
   //The length of disc will increase as more directories are found
   until d=Cardinal(Length(FDisc));
  end;
  Result:=Length(FDisc)>0;
 end;
begin
 if GetMajorFormatNumber=diAcornADFS then //But only if the format is ADFS
 begin
  //Read in the disc
  Result:=ReadTheADFSDisc;
  //Is this an ADFS L with automatic interleave detection?
  if(FForceInter=0)and(FFormat=diAcornADFS<<4+2)then
  begin
   //Any broken directories?
   if brokendircount>0 then
   repeat
    //Try next setting
    inc(Finterleave);
    if Finterleave=4 then Finterleave:=1; //Wrap around
    //Read in the disc, this time with the new interleave setting
    Result:=ReadTheADFSDisc;
   until(brokendircount=0)or(Finterleave=2);//Until we get back to the start
  end;
 end;
 Result:=Result;
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
 if not Fupdating then //Only if we are not updating
 begin
  //Update progress
//  UpdateProgress('Reading ADFS Free Space Map.');
  //Reset the free space counter
  free_space[0]:=0;
  //Reset the array
  if (secspertrack>0) and (secsize>0) then //As long as these have been set
  begin
   //Number of sides
   SetLength(free_space_map,1);
   //Number of tracks
   SetLength(free_space_map[0],disc_size[0]div(secspertrack*secsize));
   if Length(free_space_map[0])>0 then
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
//   UpdateProgress('Reading ADFS Free Space Map..');
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
    inc(free_space[0],f);
    //Move to the next entry
    inc(d,3);
    //Update progress
//    UpdateProgress('Reading ADFS Free Space Map..'+AddChar('.','',d div 3));
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
      if address<disc_size[0] then inc(free_space[0],bpmb);
      //Advance the address
      inc(address,bpmb);
     end;
    end;
   end;
   //We'll add in the directories
{   if Length(FDisc)>0 then
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
       end;}
   //Mark the system area
   for address:=bootmap to bootmap+(nzones*secsize*2) do //Two copies of the map
    ADFSFillFreeSpaceMap(address,$FE);
   //And the partial disc record/disc defect list
   if nzones>1 then
    for address:=$C00 to $DFF do ADFSFillFreeSpaceMap(address,$FE);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Fill part of the FSM with a byte
-------------------------------------------------------------------------------}
procedure TDiscImage.ADFSFillFreeSpaceMap(address: Cardinal;usage: Byte);
var
 t,s: Cardinal;
begin
 t:=0;
 s:=0;
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
Create ADFS blank floppy image
-------------------------------------------------------------------------------}
function TDiscImage.FormatADFSFloppy(minor: Byte): Boolean;
var
 t     : Integer;
 dirid,
 att   : String;
begin
 //Blank everything
 ResetVariables;
 SetDataLength(0);
 root_name:='$';
 //Set the format
 FFormat:=diAcornADFS<<4+minor;
 //Interleave option
 if FForceInter=0 then Finterleave:=2 //Default for ADFS
 else Finterleave:=FForceInter;
 //Set the filename
 imagefilename:='Untitled.'+FormatExt;
 //Setup the data area
 case minor of
  0 : disc_size[0]:= 160*1024;  //S (160KB)
  1 : disc_size[0]:= 320*1024;  //M (320KB)
  2 : disc_size[0]:= 640*1024;  //L (640KB)
  3 : disc_size[0]:= 800*1024;  //D (800KB)
  4 : disc_size[0]:= 800*1024;  //E (800KB)
  5 : disc_size[0]:= 800*1024;  //E+(800KB)
  6 : disc_size[0]:=1600*1024;  //F (1.6MB)
  7 : disc_size[0]:=1600*1024;  //F+(1.6MB)
 end;
 SetDataLength(disc_size[0]);
 //Setup the variables
 if minor<4 then  //Old maps (S, M, L and D)
 begin
  FMap:=False;
  if minor<3 then //ADFS S, M and L
  begin
   FDirType:=diADFSOldDir;    //Old Directory
   secspertrack:=16;          //Sectors Per Track
   secsize:=256;              //Sector size
  end;
  if minor=3 then //ADFS D
   FDirType:=diADFSNewDir;    //New Directory
 end;
 if minor>3 then //New maps (E, E+, F, and F+)
 begin
  FMap:=True;
  secsize:=1024;
  heads:=2;
  idlen:=$F;
  skew:=1;
  root_size:=$800;
  //New Directory : E and F
  if (minor=4) OR (minor=6) then FDirType:=diADFSNewDir;
  //Big Directory : E+ and F+
  if (minor=5) OR (minor=7) then FDirType:=diADFSBigDir;
  //E and E+
  if (minor=4) OR (minor=5) then
  begin
   secspertrack:=5;
   nzones:=1;
   density:=2;
   bpmb:=1<<7;
   zone_spare:=$520;
   if FDirType=diADFSNewDir then
    rootfrag:=$00000203
   else
    rootfrag:=$00000301;
  end;
  //F and F+
  if (minor=6) OR (minor=7) then
  begin
   secspertrack:=10;
   nzones:=4;
   density:=4;
   bpmb:=1<<6;
   zone_spare:=$640;
   if FDirType=diADFSNewDir then   //root
    rootfrag:=$00000209
   else
    rootfrag:=$00033801;
  end;
  big_flag:=0;
  format_vers:=0;
  if FDirType=diADFSBigDir then format_vers:=1;
 end;
 //Fill with zeros
 for t:=0 to disc_size[0]-1 do WriteByte(0,t);
 //Set the boot option
 SetLength(bootoption,1);
 bootoption[0]:=0;
 SetLength(free_space_map,1); //Free Space Map
 //Write the map
 if not FMap then //Old Map
  FormatOldMapADFS(disctitle);
 if FMap then //New Map
  FormatNewMapADFS(disctitle,False);
 //Now write the root
 dirid:='$';
 att:='DLR';
 CreateADFSDirectory(dirid,dirid,att);
 //Finalise all the variables by reading the data in again
 Result:=ReadADFSDisc;
end;

{-------------------------------------------------------------------------------
Format an old map ADFS image
-------------------------------------------------------------------------------}
procedure TDiscImage.FormatOldMapADFS(disctitle: String);
var
 t: Byte;
begin
 if FDirType=diADFSOldDir then
 begin
  secspertrack:=16;          //Sectors Per Track
  secsize:=256;              //Sector size
  root:=$200;                //Where the root is
  heads:=1;                  //Number of heads
  if FFormat=diAcornADFS<<4+$02 then heads:=2;
  root_size:=$500;           //Size of the root
 end;
 if FDirType=diADFSNewDir then
 begin
  secspertrack:=5;           //Sectors Per Track
  secsize:=1024;             //Sector size
  root:=$400;                //Where the root is
  heads:=2;                  //Number of heads
  root_size:=$800;           //Size of the root
 end;
 nzones:=1;                   //Number of zones (not required for old map)
 rootfrag:=root div $100;
 //Old map FreeStart
 Write24b((root+root_size)div$100,$000);
 //Disc title
 for t:=0 to 9 do
 begin
  if t mod 2=0 then WriteByte(Ord(disctitle[t+1]),$0F7+(t div 2));
  if t mod 2=1 then WriteByte(Ord(disctitle[t+1]),$1F6+(t div 2));
 end;
 //Disc size
 Write24b(disc_size[0]div$100,$0FC);
 //Checksum
 WriteByte(ByteCheckSum($0000,$100,False),$0FF);
 //Old map FreeLen
 Write24b((disc_size[0]-(root+root_size))div$100,$100);
 //Disc ID
 Write24b($4077,$1FB); //Random 16 bit number
 //Old map FreeEnd
 WriteByte($03,$1FE);
 //Checksum
 WriteByte(ByteCheckSum($0100,$100,False),$1FF);
end;

{-------------------------------------------------------------------------------
Format a new map ADFS image
-------------------------------------------------------------------------------}
procedure TDiscImage.FormatNewMapADFS(disctitle: String; ide: Boolean);
var
 log2secsize,
 log2bpmb    : Byte;
 t           : Integer;
 frags       : TFragmentArray;
 eodoffset,
 filelen     : Cardinal;
begin
 log2secsize:=0;
 //Work out log2secsize
 while secsize<>1<<log2secsize do inc(log2secsize);
 log2bpmb   :=0;
 //Work out log2bpmb
 while bpmb<>1<<log2bpmb do inc(log2bpmb);
 if nzones>1 then //Write the Boot Block (multizone only)
 begin
  //Defect List
  Write32b($20000000,$C00); //Terminate the list
  Write32b($FFFFFFFF,$DAC); //Not sure - is never explained
  //ADFS Hardware Information
  if disc_size[0]>1638400 then //Hard drive only
  begin
   Write32b($00000000,$DB0); //SL
   if not ide then
   begin
    Write32b($0D0C200A,$DB4); //GPL2 GPL3 SH GPL1
    Write16b($03FF,$DB8);     //Low Current Cylinder
    Write16b($0080,$DBA);     //Pre Compensation Cylinder
   end
   else
   begin
    Write32b($00000000,$DB4);
    Write16b($0000,$DB8);
    WriteByte($00,$DBA);      //LBA Flag
    WriteByte($01,$DBB);      //Init Flag
   end;
   //Parking Cylinder
   t:=disc_size[0]div(secspertrack*heads*secsize);
   if FDirType=diADFSBigDir then
    Write32b(secspertrack*heads*(t-1),$DBC)
   else
    Write32b(secsize*secspertrack*heads*(t-1),$DBC);
  end;
  //Partial Disc Record
  WriteByte(log2secsize,$DC0); //log2secsize
  WriteByte(secspertrack,$DC1);
  WriteByte(heads,$DC2); //heads
  WriteByte(density,$DC3); //Density
  WriteByte(idlen,$DC4); //idlen
  WriteByte(log2bpmb,$DC5); //log2bpmb
  WriteByte(skew,$DC6); //skew
  WriteByte(lowsector,$DC8); //lowsector
  WriteByte(nzones,$DC9);
  Write16b(zone_spare,$DCA);//zone_spare
  Write32b(rootfrag,$DCC);
  Write32b(disc_size[0],$DD0);
  if FDirType=diADFSBigDir then
  begin
   Write32b($00000001,$DEC); //format version (+)
   Write32b(root_size,$DF0); //Root size (+)
  end;
  WriteByte(ByteCheckSum($C00,$200,True),$DFF);  //Checksum
  bootmap:=((nzones div 2)*(8*secsize-zone_spare)-480)*bpmb; //Middle of the disc
 end;
 if nzones=1 then bootmap:=0;
 //Write the zone headers (freelink - we'll do the checkbytes later)
 for t:=0 to nzones-1 do
 begin
  Write16b($8018,bootmap+1+(secsize*t)); //FreeLink
  //Write the terminating bit for the end of free space
  WriteBits(1,(bootmap+(secsize*t))+4+(((secsize*8-zone_spare)-1)div 8)
             ,(((secsize*8)-zone_spare)-1)mod 8,1);
 end;
 //Freelink zone 0
 Write16b($81F8,bootmap+1);
 //Zonecheck zone 0
 WriteByte($FF,bootmap+3);
 //Final zone, mark off the end of the disc
 eodoffset:=(disc_size[0]div bpmb)+(zone_spare*(nzones-1))+480+32; //In bits
 //The 480 is the disc record in bits, and the 32 is the last zone header in bits
 if eodoffset<secsize*nzones*8 then //Only if the end falls in the last zone
 begin
  //Terminating bit of the good area
  WriteBits(1,bootmap+((eodoffset-1)div 8),(eodoffset-1)mod 8,1);
  //Write the defect ID of 1
  WriteBits(1,bootmap+(eodoffset div 8),eodoffset mod 8,idlen);
 end;
 //Part of the full disc record
 WriteByte(log2secsize,bootmap+4+$00);    //log2secsize
 WriteByte(secspertrack,bootmap+4+$01);   //secspertrack
 WriteByte(heads,bootmap+4+$02);          //heads
 WriteByte(density,bootmap+4+$03);        //Density
 WriteByte(idlen,bootmap+4+$04);          //idlen
 WriteByte(log2bpmb,bootmap+4+$05);       //log2bpmb
 WriteByte(skew,bootmap+4+$06);           //skew
 WriteByte(lowsector,bootmap+4+$08);      //lowsector
 WriteByte(nzones mod $100,bootmap+4+$09);//nzones lsb
 Write16b(zone_spare,bootmap+4+$0A);      //zone_spare
 Write32b(rootfrag,bootmap+4+$0C);        //root
 Write32b(disc_size[0],bootmap+4+$10);       //disc_size
 Write16b($8DC5,bootmap+4+$14);           //disc_id
 for t:=0 to 9 do WriteByte(Ord(disctitle[t+1]),bootmap+4+$16+t); //Disc title
 if FDirType=diADFSBigDir then // '+' only attributes
 begin
  Write32b($20158318,bootmap+4+$20);      //disctype
  Write32b(disc_size[0]>>32,bootmap+4+$24);  //High word of disc size
  WriteByte(0,bootmap+4+$28);             //log2sharesize
  WriteByte(big_flag,bootmap+4+$29);      //big flag
  WriteByte(nzones>>8,bootmap+4+$2A);     //nzones msb
  Write32b(format_vers,bootmap+4+$2C);    //format version
  Write32b(root_size,bootmap+4+$30);      //root size
 end
 else // non '+' only attributes
  Write32b($20158C78,bootmap+4+$20);      //disctype
 //Create the fragments for the system areas
 if nzones>1 then
 begin
  SetLength(frags,2);
  frags[0].Offset:=0;
  frags[0].Length:=$1000;
  frags[0].Zone:=0;
  frags[1].Offset:=bootmap;
  frags[1].Length:=secsize*nzones*2;
  frags[1].Zone:=nzones div 2;
 end;
 if nzones=1 then
 begin
  SetLength(frags,1);
  frags[0].Offset:=0;
  frags[0].Length:=secsize*2;
  frags[0].Zone:=0;
 end;
 //Is root a part of this, then increase the second fragment
 if rootfrag>>8=2 then
  inc(frags[Length(frags)-1].Length,root_size);
 //Write the fragments
 filelen:=0;
 for t:=0 to Length(frags)-1 do inc(filelen,frags[t].Length);
 ADFSAllocateFreeSpace(filelen,$2,frags);
 //Create the fragments for the root
 if rootfrag>>8<>2 then
 begin
  SetLength(frags,1);
  frags[0].Offset:=bootmap+secsize*nzones*2;
  frags[0].Length:=root_size;
  frags[0].Zone:=nzones div 2;
  //Write the fragment
  ADFSAllocateFreeSpace(root_size,rootfrag>>8,frags);
 end;
 //Zone checks for all zones
 for t:=0 to nzones-1 do
  WriteByte(GeneralChecksum(bootmap+$00+(t*secsize),secsize,secsize+4,$4,true),
            bootmap+(t*secsize)+$00);
 //Next create a copy of everything
 for t:=0 to (nzones*secsize)-1 do
  WriteByte(ReadByte(bootmap+t),bootmap+(nzones*secsize)+t);
 root:=bootmap+nzones*secsize*2;
end;

{-------------------------------------------------------------------------------
Create ADFS blank hard disc image
-------------------------------------------------------------------------------}
function TDiscImage.FormatADFSHDD(harddrivesize: Cardinal; newmap: Boolean;
                                dirtype: Byte; ide,addheader: Boolean): Boolean;
var
 bigmap,
 ok           : Boolean;
 Lidlen,
 Lzone_spare,
 Lnzones,
 Llog2bpmb,
 Lroot,
 Llog2secsize,
 Llowsec      : Cardinal;
 t            : Byte;
 dirid,att    : String;
begin
 //Initialise the variables
 bigmap:=False;
 if dirtype=diADFSBigDir then bigmap:=True;
 Lidlen:=0;
 Lzone_spare:=0;
 Lnzones:=0;
 Llog2bpmb:=0;
 Lroot:=0;
 Llog2secsize:=9;
 Llowsec:=1;
 //Old or new map?
 dirtype:=dirtype mod 3;//Can only be 0, 1 or 2
 if dirtype=0 then newmap:=False; //Old directory only on old map
 if(not newmap)and(harddrivesize>512*1024*1024)then
  harddrivesize:=512*1024*1024; //512MB max on old map
 //Work out the parameters based on the drive size
 if newmap then //But only for new map
  ok:=ADFSGetHardDriveParams(harddrivesize,bigmap,ide,Lidlen,Lzone_spare,
                             Lnzones,Llog2bpmb,Lroot,Llog2secsize,Llowsec)
 else
  ok:=True; //old map is easier
 //Got some figures OK, so now do the formatting
 if ok then
 begin
  //Blank everything
  ResetVariables;
  SetDataLength(0);
  root_name:='$';
  //Set the format
  FFormat:=diAcornADFS<<4+$0F;
  //Set the map and directory
  FMap:=newmap;
  FDirType:=dirtype;
  //Set the filename
  imagefilename:='Untitled.'+FormatExt;
  //Setup the data area
  if addheader then emuheader:=$200 else emuheader:=0;
  SetDataLength(harddrivesize+emuheader);
  //Fill with zeros
  for t:=0 to GetDataLength-1 do WriteByte(0,t);
  //Set the boot option
  SetLength(bootoption,1);
  bootoption[0]:=0;
  SetLength(free_space_map,1); //Free Space Map
  disc_size[0]:=harddrivesize;    //Disc Size
  //Set up old map
  if not FMap then FormatOldMapADFS(disctitle);
  //Set up new map
  if FMap then
  begin
   idlen:=Lidlen;
   zone_spare:=Lzone_spare;
   nzones:=Lnzones;
   bpmb:=1<<Llog2bpmb;
   rootfrag:=Lroot;
   secsize:=1<<Llog2secsize;
   if ide then
   begin
    heads:=16;
    secspertrack:=63;
   end
   else
   begin
    heads:=4;
    secspertrack:=32;
   end;
   density:=0;
   skew:=0;
   lowsector:=Llowsec;
   root_size:=$800;
   if FDirType=2 then
   begin
    format_vers:=1;
    big_flag:=0;
    if disc_size[0]>512*1024*1024 then big_flag:=1;
   end;
   FormatNewMapADFS(disctitle,ide);
  end;
  //Now write the root
  dirid:=root_name;
  att:='DLR';
  CreateADFSDirectory(dirid,dirid,att);
  //Finalise all the variables by reading the data in again
  Result:=ReadADFSDisc;
 end;
end;

{-------------------------------------------------------------------------------
Update title on ADFS image
-------------------------------------------------------------------------------}
function TDiscImage.UpdateADFSDiscTitle(title: String): Boolean;
var
 t: Integer;
begin
 //Is this an ADFS/AFS Hybrid?
 if FAFSPresent then
 begin
  Result:=UpdateAFSDiscTitle(title);
  exit;
 end;
 //Remove any extraenous spaces
 RemoveSpaces(title);
 //Make sure it is not overlength
 title:=LeftStr(title,10);
 //And update the internal variable
 disc_name[0]:=title;
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
  WriteByte(ByteCheckSum($0000,$100,False),$0FF);
  //Checksum on second sector
  WriteByte(ByteCheckSum($0100,$100,False),$1FF);
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
  WriteByte(ByteCheckSum($0100,$100,False),$1FF);
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
function TDiscImage.ADFSGetFreeFragments(offset:Boolean=True;
                                          whichzone:Integer=-1): TFragmentArray;
var
 zonecounter  : Integer;
 zone,
 startoffset,
 startzone,
 freelink,
 i,j          : Cardinal;
 fragments    : TFragmentArray;
 zonecheck    : array of Boolean;
begin
 Result:=nil;
 fragments:=nil;
 startzone:=nzones div 2; //Where to start looking
 SetLength(zonecheck,nzones); //So we can check each zone
 for i:=0 to nzones-1 do zonecheck[i]:=False;
 // Start at the start zone to find a hole big enough for the file to fit
 zonecounter:=startzone;
 startoffset:=0;
 while startoffset<=Ceil(nzones/2) do
 begin
  zone:=zonecounter;
  //i is the bit counter...number of bits from the first freelink
  //Get the first freelink of the zone and set our counter
  i:=ReadBits(bootmap+(zone*secsize)+1,0,15);
  //The header freelink is always 15 bits or less
  freelink:=i;
  if i<>0 then //If the freelink is zero, then there are no free spaces
  begin
   repeat
    if(whichzone=-1)or(whichzone=zone)then
    begin
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
    end;
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
    if(whichzone=-1)or(whichzone=zone)then
     if offset then
      fragments[Length(fragments)-1].Length:=((j-i)+1)*bpmb
     else
      fragments[Length(fragments)-1].Length:=(j-i)+1;
    //Move the pointer on, after the '1'
    inc(i,freelink);
   until (freelink=0)                   //Unless the next link is zero,
      or (i>=32+(secsize*8-zone_spare));//or we run out of zone
  end;
  //Set this zone to having been checked
  zonecheck[zonecounter]:=True;
  //Work out the next one, centralising around the root
  while(startoffset<=Ceil(nzones/2))and(zonecheck[zonecounter])do
  begin
   //Have we checked below?
   if zonecounter=startzone-startoffset then
    //Then check above
    if startzone+startoffset<Length(zonecheck) then
     zonecounter:=startzone+startoffset;
   //Has it been checked already?
   if zonecheck[zonecounter] then
   begin
    //Move to next
    inc(startoffset);
    zonecounter:=startzone-startoffset;
    //A couple of checks to make sure we're not out of bounds
    if(zonecounter<0)and(startzone+startoffset<Length(zonecheck))then
     zonecounter:=startzone+startoffset;
    if(zonecounter<0)or(zonecounter>=Length(zonecheck))then
     startoffset:=nzones; //This will finish off the loop
   end;
  end;
 end;
 Result:=fragments;
end;

{-------------------------------------------------------------------------------
Write a file to ADFS image
-------------------------------------------------------------------------------}
function TDiscImage.WriteADFSFile(var file_details: TDirEntry;
                        var buffer: TDIByteArray;extend: Boolean=True): Integer;
//Set extend to FALSE to ensure that the ExtendDirectory is run (Big Directory)
var
 timestamp    : Int64;
 dir,entry,
 l,ptr,
 freeptr,
 safilelen,
 dest,fragid  : Cardinal;
 success,
 spacefound   : Boolean;
 fragments    : TFragmentArray;
 sharedbyte   : Byte;
 //Set the shared byte
 procedure SetSharedByte(entrylength: Cardinal;setinitial: Boolean);
 begin
  //Get the sharing offset
  if setinitial then
   sharedbyte:=ADFSSectorAlignLength(entrylength,False)div secsize;
  //If these are equal, we need to move on 1
  if ADFSSectorAlignLength(entrylength,False)=entrylength then inc(sharedbyte);
 end;
 //Check for shared fragment
 procedure CheckForShared(start: Integer);
 var
  entry2  : Cardinal;
  currfile: String;
 begin
  //Don't check the current file, as this will be the same (obvs)
  if start>=0 then currfile:=FDisc[dir].Entries[start].Filename
  else currfile:='';
  //Iterate through the other files in the directory
  if Length(FDisc[dir].Entries)>0 then
   for entry2:=0 to Length(FDisc[dir].Entries)-1 do
    if (FDisc[dir].Entries[entry2].Sector>>8=fragid)
    and(FDisc[dir].Entries[entry2].Sector mod$100>=sharedbyte)
    and(FDisc[dir].Entries[entry2].Filename<>currfile)then
    begin
     //Work out the shared byte
     sharedbyte:=(ADFSSectorAlignLength(FDisc[dir].Entries[entry2].Length
                                                      ,False)div secsize)
                +(FDisc[dir].Entries[entry2].Sector mod $100);
     //As before, if the lengths are equal, move on 1
     SetSharedByte(FDisc[dir].Entries[entry2].Length,False);
    end;
   //No space sharing with siblings, so reset the flag
   if safilelen>fragments[0].Length-sharedbyte*secsize then
   begin
    sharedbyte:=$00;
    SetLength(fragments,0);
   end;
 end;
//Main procedure
begin
 //Is this on the AFS partition?
 if FAFSPresent then
  if LeftStr(file_details.Parent,Length(afsrootname))=afsrootname then
  begin
   Result:=WriteAFSFile(file_details,buffer);
   exit;
  end;
 //Is this on the DOS partition?
 if FDOSPresent then
  if LeftStr(file_details.Parent,Length(dosrootname))=dosrootname then
  begin
   Result:=WriteDOSFile(file_details,buffer);
   exit;
  end;
 l      :=0;
 dir    :=0;
 freeptr:=0;
 //Start with a negative result
 Result:=-3;//File already exists
 success:=False;
 //Validate the proposed filename
 if not((file_details.Filename=root_name)and(FDirType=diADFSBigDir))then
  file_details.Filename:=ValidateADFSFilename(file_details.Filename);
 //First make sure it doesn't exist already
 if(not FileExists(file_details.Parent+dir_sep+file_details.Filename,dir,entry))
 or((file_details.Filename=root_name)and(FDirType=diADFSBigDir))then
  //Get the directory where we are adding it to, and make sure it exists
  if(FileExists(file_details.Parent,dir,entry))
  OR(file_details.Parent=root_name)then
  begin
   if file_details.filename<>root_name then
   begin
    //Where we are inserting this into
    if file_details.Parent=root_name then
     dir  :=0
    else
     dir  :=FDisc[dir].Entries[entry].DirRef;
    //Has it been read in?
    if not FDisc[dir].BeenRead then ReadDirectory(file_details.Parent);
    //Big Dir - Verify directory is big enough or if it needs extending and moved.
    if(FDirType=diADFSBigDir)and(extend)then //This will extend/contract the directory
     if not ExtendADFSBigDir(dir,Length(file_details.Filename),True) then
     begin
      Result:=-9; //Cannot extend
      exit;
     end;
    //Get the length of the file
    l:=Length(FDisc[dir].Entries);
    //Work out the "sector aligned file length"
    safilelen:=ADFSSectorAlignLength(file_details.Length,False);
   end else safilelen:=file_details.Length; //New length of the root
   Result:=-4;//Catalogue full
   //Make sure it will actually fit on the disc
   if free_space[0]>=safilelen then
    //And make sure we can extend the catalogue
    if((FDirType=diADFSOldDir)and(l<47)
    OR (FDirType=diADFSNewDir)and(l<77)
    OR (FDirType=diADFSBigDir))then
    begin
     Result:=-7; //Map full
     //Set some flags
     spacefound:=False;
     success:=False;
     dest:=disc_size[0];
     sharedbyte:=$00; //Clear this to indicate we're not reusing
     //Find a big enough space
     if (FMap)
     and(Pos('D',file_details.Attributes)=0)
     and(file_details.filename<>root_name)
     and(file_details.Length<((idlen+1)*bpmb)-secsize)then
     begin
      //Looks like we can share this - is there any already shared we can reuse
      //First check the parent - if this is '1' then see if there's any space
      if (FDisc[dir].Sector mod $100=$01)
      and(FDisc[dir].Directory<>root_name)then //Don't share with the root though
      begin
       //Get the fragments for this object
       fragments:=NewDiscAddrToOffset(FDisc[dir].Sector);
       //If there is not just the one, then skip
       if Length(fragments)=1 then
       begin
        //Get the fragment ID
        fragid:=FDisc[dir].Sector>>8;
        //And the sharing offset
        SetSharedByte(FDisc[dir].Length,True);
        //And check the children for the same ID
        CheckForShared(-1);
       end;
      end;
      //No space, so go through the children and do the same checks
      if(sharedbyte=$00)and(Length(FDisc[dir].Entries)>0)then
       for entry:=0 to Length(FDisc[dir].Entries)-1 do
        if(FDisc[dir].Entries[entry].Sector mod$100=$01)
        and(entry<Length(FDisc[dir].Entries)-1)then
        begin
         //Get the fragments for this object
         fragments:=NewDiscAddrToOffset(FDisc[dir].Entries[entry].Sector);
         if Length(fragments)=1 then
         begin
          //Get the fragment ID
          fragid:=FDisc[dir].Entries[entry].Sector>>8;
          //And the sharing offset
          SetSharedByte(FDisc[dir].Entries[entry].Length,True);
          CheckForShared(entry);
         end;
        end;
     end;
     //Nothing we can reuse, so look for a new one
     if sharedbyte=$00 then
     begin
      fragid:=0; //The function returns the fragment ID, or free pointer, with this
      fragments:=ADFSFindFreeSpace(file_details.Length,fragid);
     end;
     //Set the flag, if space has been found
     spacefound:=Length(fragments)>0;
     //Need to set up some variables for old map
     if spacefound then
      if not FMap then
      begin
       file_details.Sector:=fragments[0].Offset div $100;
       freeptr:=fragid;
      end;
     //Write the file to the area
     if(spacefound)and(Length(fragments)>0)then
     begin
      Result:=-5;//Unknown error
      //Compensate for shared fragment
      if(Length(fragments)=1)and(sharedbyte>$01)then
       inc(fragments[0].Offset,(sharedbyte-1)*secsize);
      //Write the data back to the image
      success:=WriteFragmentedData(fragments,buffer);
      //Set this marker to the start of the first offset
      dest:=fragments[0].Offset;
     end;
     //If it wrote OK, then continue
     if success then
     begin
      //Update the checksum, if it is a directory
      if(Pos('D',file_details.Attributes)>0)then
       if FDirType>diADFSOldDir then //New/Big Directory (Old Dir has zero for checksum)
        WriteByte(CalculateADFSDirCheck(dest),dest+(file_details.Length-1));
      //Now update the free space map
      if not FMap then //Old map
       ADFSAllocateFreeSpace(file_details.Length,freeptr);
      if FMap then     //New map
      begin
       //Can this fragment be shared, if not already?
       if (sharedbyte=$00)
       and(Length(fragments)=1)
       and(file_details.Length<(idlen+1)*bpmb)
       and(fragments[0].Length-file_details.Length>secsize)then sharedbyte:=$01;
       //Unless it's Big Map and is a directory (not shared because they grow)
       if(FDirType=diADFSBigDir)
       and(Pos('D',file_details.Attributes)>0)
       and(file_details.filename<>root_name)then sharedbyte:=$00;
       //Make note of the fragment ID
       file_details.Sector:=fragid<<8+sharedbyte;
       //Only need to allocate the free space if we're not reusing
       if sharedbyte<$02 then
        ADFSAllocateFreeSpace(file_details.Length,fragid,fragments);
      end;
      //Now update the directory (local copy)
      if file_details.filename<>root_name then
      begin
       //Convert load/exec address into filetype and datestamp, if necessary
       ADFSCalcFileDate(file_details);
       //Now we add the entry into the directory catalogue
       ptr:=ExtendADFSCat(dir,file_details);
       //Not a directory
       FDisc[dir].Entries[ptr].DirRef:=-1;
       //Filetype and Timestamp for Arthur and RISC OS ADFS
       if (FDisc[dir].Entries[ptr].LoadAddr=0)
       and(FDisc[dir].Entries[ptr].ExecAddr=0)
       and(FDirType>diADFSOldDir)then //New and Big directories
       begin
        FDisc[dir].Entries[ptr].LoadAddr:=$FFF00000;
        //Set the filetype, if not already set
        if FDisc[dir].Entries[ptr].ShortFileType<>'' then
        begin
         FDisc[dir].Entries[ptr].LoadAddr:=FDisc[dir].Entries[ptr].LoadAddr OR
            (StrToIntDef('$'+FDisc[dir].Entries[ptr].ShortFileType,0)<<8);
         FDisc[dir].Entries[ptr].FileType:=
          GetFiletypeFromNumber(StrToIntDef('$'+FDisc[dir].Entries[ptr].ShortFileType,0));
        end;
        //Timestamp it, if not already done
        timestamp:=TimeDateToRISCOS(Now);
        FDisc[dir].Entries[ptr].TimeStamp:=Now;
        FDisc[dir].Entries[ptr].LoadAddr:=FDisc[dir].Entries[ptr].LoadAddr OR
            (timestamp DIV $100000000);
        FDisc[dir].Entries[ptr].ExecAddr:=timestamp MOD $100000000;
       end;
       //File imported from DFS, expand the tube address, if needed
       if (FDisc[dir].Entries[ptr].LoadAddr>>16=$00FF)
       and(FDisc[dir].Entries[ptr].ExecAddr>>16=$00FF)then
       begin
        FDisc[dir].Entries[ptr].LoadAddr:=
                                (FDisc[dir].Entries[ptr].LoadAddr AND $0000FFFF)
                                OR$FFFF0000;
        FDisc[dir].Entries[ptr].ExecAddr:=
                                (FDisc[dir].Entries[ptr].ExecAddr AND $0000FFFF)
                                OR$FFFF0000;
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
        FDisc[Length(FDisc)-1].Parent   :=dir;
        FDisc[Length(FDisc)-1].Sector   :=FDisc[dir].Entries[ptr].Sector;
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
      if(FDirType=diADFSBigDir)and(extend)then
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
Find some free space
-------------------------------------------------------------------------------}
function TDiscImage.ADFSFindFreeSpace(filelen: Cardinal;
                                          var fragid: Cardinal): TFragmentArray;
var
 ptr,
 freeptr,
 safilelen,
 i,j,
 freelink,
 zone,
 idperzone  : Cardinal;
 spacefound : Boolean;
 fsfragments: TFragmentArray;
begin
 Result:=nil;
 spacefound:=False;
 //Work out the "sector aligned file length"
 safilelen:=ADFSSectorAlignLength(filelen,False);//Ceil(filelen/secsize)*secsize;
 //Find some free space
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
   SetLength(Result,1); //Only the single fragment
   Result[0].Offset:=Read24b($000+freeptr)*$100; //Fragment details
   Result[0].Length:=filelen; //Fragment details
   fragid:=freeptr; //Return the free pointer
  end else fragid:=0; //Reset freeptr
 end;
 //New map
 if FMap then
 begin
  idperzone:=((secsize*8)-zone_spare)div(idlen+1); //Max IDs per zone
  //Get the free space fragments
  fsfragments:=ADFSGetFreeFragments;
  if Length(fsfragments)>0 then
  begin
   //Go through them to find a big enough fragment
   i:=0;
   repeat
    if(fsfragments[i].Length>=safilelen)
    and(fsfragments[i].Length>=(idlen+1)*bpmb)//Smallest possible length
    and(fsfragments[i].Offset+filelen<disc_size[0])then
    begin
     //Found one, so set our flag
     spacefound:=True;
     //And add it to the fragment list (which will be one)
     SetLength(Result,1);
     Result[0]:=fsfragments[i];
     Result[0].Length:=filelen;
    end;
    inc(i);
    //Continue for all fragments or we found somewhere
   until (i=Length(fsfragments)) or (spacefound);
   //None were big enough, so we'll need to fragment the file
   if not spacefound then
   begin
    //Go through the fragments again
    i:=0;
    //This time add up the lengths
    freelink:=0;
    repeat
     //Copy each fragment into our list
     SetLength(Result,Length(Result)+1);
     Result[Length(Result)-1]:=fsfragments[i];
     //Reduce the length of the final fragment, if over
     if freelink+fsfragments[i].Length>=filelen then
      Result[Length(Result)-1].Length:=filelen-freelink;
     //Add the length to the total
     inc(freelink,Result[Length(Result)-1].Length);
     //If we have enough, set the flag
     if freelink>=filelen then spacefound:=True;
     //Next free fragment
     inc(i);
    until (i=Length(fsfragments)) or (spacefound);
   end;
  end;
  //Nothing found, so blank the fragment ID to let the caller know
  if Length(Result)=0 then fragid:=0;
  //Align the lengths
  if Length(Result)>0 then
   for i:=0 to Length(Result)-1 do
    Result[i].Length:=ADFSSectorAlignLength(Result[i].Length);
  //Assign an ID
  if(Length(Result)>0)and(fragid=0)then
  begin
   i:=0;
   while(fragid=0)and(i<Length(Result))do
   begin
    // Now scan the zone again and find a unique ID
    zone:=Result[i].Zone; //Zone of the first fragment
    // IDs start here for this zone
    fragid:=zone*idperzone;
    j:=0; //Highest used ID in this zone
    //Check each fragment ID until we find one that doesn't exist
    while(Length(NewDiscAddrToOffset((fragid+j)*$100))>0)and(j<idperzone)do
     inc(j);
    //Is it valid?
    if(j<idperzone)and(Length(NewDiscAddrToOffset((fragid+j)*$100))=0)then
    begin
     inc(fragid,j); //We'll use this one
     if fragid<3 then fragid:=3; //Can't be 0,1 or 2
    end
    else fragid:=0; //Otherwise return a blank
    if fragid>=1<<idlen then fragid:=0; //Fragment ID is outside ID length
    if fragid=0 then inc(i); //Move onto the next fragment
   end;
   if fragid=0 then SetLength(Result,0); //None found, so return a nil result
  end;
 end;
end;

{-------------------------------------------------------------------------------
Sector, BPMB and idlen align a file length
-------------------------------------------------------------------------------}
function TDiscImage.ADFSSectorAlignLength(filelen: Cardinal;
                                             bpmbalign: Boolean=True): Cardinal;
begin
 Result:=filelen;
 if bpmbalign then
 begin
  //Align to BPMB
  Result:=Ceil(filelen/bpmb)*bpmb;
  //and at least bigger than bpmb*(idlen+1).
  if Result<(idlen+1)*bpmb then Result:=(idlen+1)*bpmb;
 end;
 //Finally, sector align it
 Result:=Ceil(Result/secsize)*secsize;
end;

{-------------------------------------------------------------------------------
Allocate the space in the free space map (old map)
-------------------------------------------------------------------------------}
function TDiscImage.ADFSAllocateFreeSpace(filelen,freeptr: Cardinal): Boolean;
var
 ref,ptr,
 safilelen : Cardinal;
begin
 Result:=False;
 if not FMap then //Old map
 begin
  //Work out the "sector aligned file length"
  safilelen:=ADFSSectorAlignLength(filelen,False);//Ceil(filelen/secsize)*secsize;
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
   ptr:=ReadByte($1FE); //Number of free space entries
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
   WriteByte(ptr,$1FE);
  end;
  //Update the checksums
  WriteByte(ByteCheckSum($0000,$100,False),$0FF);
  WriteByte(ByteCheckSum($0100,$100,False),$1FF);
  //Consolidate the free space map
  ConsolidateADFSFreeSpaceMap;
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Allocate the space in the free space map (new map)
-------------------------------------------------------------------------------}
function TDiscImage.ADFSAllocateFreeSpace(filelen,fragid: Cardinal;
                                            fragments: TFragmentArray): Boolean;
var
 ref,
 safilelen,
 freeptr,
 freelink,
 zone,
 zoneptr,
 ptr,i,j   : Cardinal;
 zonesize  : QWord;
 freelen   : Byte;
 //freelen is set to 15 bits if it is the first entry in the zone.
 procedure SetFreeLen(pos: Cardinal);
 begin
  if pos=8 then freelen:=15 else freelen:=idlen;
 end;
begin
 Result:=False;
 if FMap then //New map
 begin
  Result:=True;
  //Recalculate the file length to be bpmb-aligned
  safilelen:=ADFSSectorAlignLength(filelen);
  //Zone size, so we don't accidentally go over the end
  zonesize:=((secsize*8)-zone_spare)+$20; //Take account of the 4-byte zone header
  //For each fragment:
  for i:=0 to Length(fragments)-1 do
  begin
   //Scan the zone, from the beginning, until we find a pointer to our fragment
   zone:=fragments[i].Zone; //Zone
   zoneptr:=zone*secsize;   //Pointer to the start of the zone
   freeptr:=(1+zoneptr)*8;  //Pointer to the next freelink, start at the beginning
   repeat
    //Next freelink
    SetFreeLen(freeptr-(zoneptr*8));
    freelink:=ReadBits(bootmap+(freeptr DIV 8),freeptr MOD 8,freelen);
    //move the pointer on
    inc(freeptr,freelink);
    //we need it as an absolute address
    ref:=((freeptr-$200)-zone_spare*zone)*bpmb; // $200 is 64 bytes in bits
    //Continue until it points to our fragment
   until ref=fragments[i].Offset;
   //Make a note of the length of data we are laying down for this fragment
   if fragments[i].Length<=safilelen then
    j:=fragments[i].Length
   else
    j:=safilelen;
   //And reduce the total accordingly
   dec(safilelen,j);
   //Length of fragment/file size in bits
   ref:=j DIV bpmb;
   //Smallest object size has to be bigger than idlen
   if ref<idlen then ref:=idlen+1;
   //Get the freelink of where we are at
   SetFreeLen(freeptr-zoneptr*8);
   ptr:=ReadBits(bootmap+(freeptr DIV 8),freeptr MOD 8,freelen);
   //If the data we are laying down is less than the space available
   if j<>fragments[i].Length then
   begin
    //Adjust the previous freelink to point to after this fragment
    SetFreeLen((freeptr-zoneptr*8)-freelink);
    if (((freeptr-freelink)-zoneptr*8)+freelen<=zonesize)
    and(Result)then
     WriteBits(freelink+ref,
               bootmap+(freeptr-freelink)DIV 8,(freeptr-freelink)MOD 8,
               freelen)
    else Result:=False;
    //Reduce the freelink for this fragment by the above length and move afterwards
    //If it is not zero
    SetFreeLen((freeptr-zoneptr*8)+ref);
    if ptr<>0 then
     if ReadBits(bootmap+(freeptr+ref)DIV 8,(freeptr+ref)MOD 8,freelen)=0 then
      if (((freeptr+ref)-zoneptr*8)+freelen<=zonesize)and(Result)then
       WriteBits(ptr-ref,bootmap+(freeptr+ref)DIV 8,(freeptr+ref)MOD 8,freelen)
      else Result:=False;
   end
   else
   //If the data we are laying down fits exactly into the space
   begin
    //Take this pointer and add it to the previous pointer
    SetFreeLen((freeptr-zoneptr*8)-freelink);
    //Unless this pointer is zero, then the previous will need to be zero
    if ptr<>0 then
    begin
     if (((freeptr-freelink)-zoneptr*8)+freelen<=zonesize)and(Result)then
      WriteBits(0,
                bootmap+(freeptr-freelink)DIV 8,(freeptr-freelink)MOD 8,
                freelen)
     else Result:=False;
    end
    else
    begin
     if (((freeptr-freelink)-zoneptr*8)+freelen<zonesize)and(Result)then
      WriteBits(freelink+ptr+ref,bootmap+(freeptr-freelink)DIV 8,
                (freeptr-freelink)MOD 8,freelen)
     else Result:=False;
    end;
   end;
   //Put the new fragid in and terminate with a set bit for the length
   if((freeptr-zoneptr*8)+idlen<=zonesize)and(Result)then
    WriteBits(fragid,bootmap+freeptr DIV 8,freeptr MOD 8,idlen)
   else Result:=False;
   dec(ref);//We're setting this bit
   if(((freeptr+ref)-zoneptr*8)+1<=zonesize)and(Result)then
    WriteBits(1,bootmap+(freeptr+ref)DIV 8,(freeptr+ref)MOD 8,1)
   else Result:=False;
  end;
  if Result then
   for zone:=0 to nzones-1 do
   begin
    //Ensure the top bit is set on the first link for each zone
    WriteByte(ReadByte(bootmap+2+zoneptr)OR$80,bootmap+2+zoneptr);
    //Zone checks
    WriteByte(
          GeneralChecksum(bootmap+$00+zoneptr,
                          secsize,
                          secsize+4,
                          $4,
                          true),
              bootmap+zoneptr+$00);
   end;
  //Make a copy, if successful
  if Result then
   for ptr:=0 to (nzones*secsize)-1 do
    WriteByte(ReadByte(bootmap+ptr),bootmap+ptr+nzones*secsize)
  else //Otherwise, restore the original
   for ptr:=0 to (nzones*secsize)-1 do
    WriteByte(ReadByte(bootmap+ptr+nzones*secsize),bootmap+ptr);;
 end;
end;

{-------------------------------------------------------------------------------
Write data to fragments
-------------------------------------------------------------------------------}
function TDiscImage.WriteFragmentedData(fragments: TFragmentArray;
                                             var buffer: TDIByteArray): Boolean;
var
 ptr,
 ref,j : Cardinal;
begin
 Result:=False;
 if Length(fragments)>0 then
 begin
  //Counter into the data
  ptr:=0;
  //Set to true for now, if one of the writes fails, it will remain at False
  Result:=True;
  //Go through the fragments and write them
  for ref:=0 to Length(fragments)-1 do
  begin
   //Amount of data to write for this fragment
   if fragments[ref].Length>Length(buffer)-ptr then
    j:=Length(buffer)-ptr
   else
    j:=fragments[ref].Length;
   //Write the data, return a true or false, ANDed with past results
   Result:=Result AND WriteDiscData(fragments[ref].Offset,0,
                                      buffer,
                                      j,
                                      ptr);
   inc(ptr,fragments[ref].Length); //Next fragment of data
  end;
 end;
end;

{-------------------------------------------------------------------------------
Create a directory
-------------------------------------------------------------------------------}
function TDiscImage.CreateADFSDirectory(var dirname,parent,
                                                   attributes: String): Integer;
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
 //Is this on the AFS partition?
 if FAFSPresent then
  if LeftStr(parent,Length(afsrootname))=afsrootname then
  begin
   Result:=CreateAFSDirectory(dirname,parent,attributes);
   exit;
  end;
 //Is this on the DOS partition?
 if FDOSPresent then
  if LeftStr(parent,Length(dosrootname))=dosrootname then
  begin
   Result:=CreateDOSDirectory(dirname,parent,attributes);
   exit;
  end;
 SetLength(buffer,0);
 Result:=-3;//Directory already exists
 if(dirname=root_name)OR(parent=root_name)then //Creating the root
  parentaddr:=rootfrag
 else
 begin
  FileExists(parent,dir,entry);
  parentaddr:=FDisc[dir].Entries[entry].Sector;
  //Has it been read in?
  if not FDisc[FDisc[dir].Entries[entry].DirRef].BeenRead then ReadDirectory(parent);
 end;
 if dirname<>root_name then
  //Validate the name
  dirname:=ValidateADFSFilename(dirname);
 //Make sure it does not already exist
 if(not FileExists(parent+dir_sep+dirname,ref))OR(dirname=root_name)then
 begin
  Result:=-5;//Unknown error
  //Set as 'D' so it gets added as a directory
  if Pos('D',attributes)=0 then attributes:='D'+attributes;
  if FDirType=diADFSOldDir then //Old Directory
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
  if FDirType=diADFSNewDir then //New Directory
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
  if FDirType=diADFSBigDir then //Big Directory
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
  if dirname=root_name then //Root - used when formatting an image
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
procedure TDiscImage.UpdateADFSCat(directory: String;newname: String='');
var
 dir,
 diraddr,
 ref,i,
 head,
 heapctr,
 tail,
 dirlen    : Cardinal;
 c,a       : Byte;
 temp      : String;
 fragments : TFragmentArray;
 dirbuffer : TDIByteArray;
const
 oldattr = 'RWLDErweP'+#00;
 newattr = 'RWLDrw';
begin
 //Is this on the AFS partition?
 if FAFSPresent then
  if directory=afsrootname then
  begin
   UpdateAFSDirectory(directory);
   exit;
  end;
 //Is this on the DOS partition?
 if FDOSPresent then
  if directory=dosrootname then
  begin
   UpdateDOSDirectory(directory);
   exit;
  end;
 if(FileExists(directory,ref))or(directory=root_name)then
 begin
  //Get the directory reference and sector address
  if directory=root_name then
  begin
   dir  :=0;
   if FMap then diraddr:=rootfrag
   else diraddr:=root;
  end
  else
  begin
   dir  :=FDisc[ref div $10000].Entries[ref mod $10000].DirRef;
   diraddr:=FDisc[ref div $10000].Entries[ref mod $10000].Sector;
   //Has it been read in?
   if not FDisc[dir].BeenRead then ReadDirectory(directory);
  end;
  //Make the sector address a disc address
  if not FMap then
  begin //Old map
   //Create a fragment array
   SetLength(fragments,1);
   fragments[0].Offset:=diraddr*$100; //Once the above line is removed, add the *$100 here
   if FDirType=diADFSOldDir then fragments[0].Length:=$500;
   if FDirType=diADFSNewDir then fragments[0].Length:=$800;
   dirlen:=fragments[0].Length;
  end
  else //New map
  begin
   //Get the fragments of the directory
   fragments:=NewDiscAddrToOffset(diraddr);
   if Length(fragments)=0 then exit;
   dirlen:=0;
   for i:=0 to Length(fragments)-1 do inc(dirlen,fragments[i].Length);
  end;
  //Read in and cache the directory
  if ExtractFragmentedData(fragments,dirlen,dirbuffer) then
  begin
   diraddr:=0; //This, and all following references to it, can be removed once working
   //Update the directory title
   temp:=FDisc[dir].Title+#$0D;
   for i:=0 to 18 do
   begin
    //Padded with nulls
    c:=$00;
    if i<Length(temp) then c:=Ord(temp[i+1]);
    //Write the byte - Old and New only
    if FDirType=diADFSOldDir then WriteByte(c AND$7F,$4CB+$0E+i,dirbuffer);
    if FDirType=diADFSNewDir then WriteByte(c       ,$7D7+$06+i,dirbuffer);
   end;
   //Update the directory name
   if newname<>'' then
   begin
    newname:=newname+#$0D;
    for i:=0 to 9 do
    begin
     //Padded with nulls
     c:=$00;
     if i<Length(newname) then c:=Ord(newname[i+1]);
     //Write the byte - Old and New only
     if FDirType=diADFSOldDir then WriteByte(c AND$7F,$4CB+$01+i,dirbuffer);
     if FDirType=diADFSNewDir then WriteByte(c       ,$7D7+$19+i,dirbuffer);
    end;
   end;
   //Clear the directory
   c:=0;
   if FDirType=diADFSOldDir then c:=47;
   if FDirType=diADFSNewDir then c:=77;
   if c>0 then //Old and New type only
    for i:=0 to c-1 do
     for ref:=0 to $19 do WriteByte($00,$05+ref+i*$1A,dirbuffer);
   if FDirType=diADFSBigDir then //Big type
   begin
    //Get the size of the header, with padding
    head:=Read32b($08,dirbuffer)+$1C+1; //Size
    head:=((head+3)div 4)*4;//Pad to word boundary
    //Get the size of the directory, minus tail
    tail:=Read32b($0C,dirbuffer)-8;
    //Now clear the directory entries
    for i:=head to tail-head do //Tail is 8 bytes
     WriteByte($00,i,dirbuffer);
   end;
   //Heap pointer for Big Directories
   heapctr:=0;
   if FDirType=diADFSBigDir then //Write the number of entries for big directory
    Write32b(Length(FDisc[dir].Entries),$10,dirbuffer);
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
       inc(a,1<<i);
     if FDirType<diADFSBigDir then //Old and New only
     begin
      for i:=0 to 9 do
      begin
       //Padded with nulls
       c:=$00;
       if i<Length(temp) then c:=Ord(temp[i+1]);
       //Add the attributes
       if FDirType=diADFSOldDir then //Old directory only
        if Pos(oldattr[i+1],FDisc[dir].Entries[ref].Attributes)>0 then
         c:=c+$80;
       //Write the byte
       WriteByte(c,$05+i+ref*$1A,dirbuffer);
      end;
      //Load Address
      Write32b(FDisc[dir].Entries[ref].LoadAddr,$05+$0A+ref*$1A,dirbuffer);
      //Execution Address
      Write32b(FDisc[dir].Entries[ref].ExecAddr,$05+$0E+ref*$1A,dirbuffer);
      //Length
      Write32b(FDisc[dir].Entries[ref].Length  ,$05+$12+ref*$1A,dirbuffer);
      //Sector
      Write24b(FDisc[dir].Entries[ref].Sector  ,$05+$16+ref*$1A,dirbuffer);
      if FDirType=diADFSOldDir then
       //This is what appears in brackets after the file - old directory
       WriteByte(00                             ,$05+$19+ref*$1A,dirbuffer)
      else //New directory - attributes
       WriteByte(a                              ,$05+$19+ref*$1A,dirbuffer);
     end;
     if FDirType=diADFSBigDir then //Big only
     begin
      //Load Address
      Write32b(FDisc[dir].Entries[ref].LoadAddr        ,head+$00+ref*$1C,dirbuffer);
      //Execution Address
      Write32b(FDisc[dir].Entries[ref].ExecAddr        ,head+$04+ref*$1C,dirbuffer);
      //Length
      Write32b(FDisc[dir].Entries[ref].Length          ,head+$08+ref*$1C,dirbuffer);
      //Sector
      Write24b(FDisc[dir].Entries[ref].Sector          ,head+$0C+ref*$1C,dirbuffer);
      //Tail copy of the sector
      Write32b(FDisc[dir].Entries[ref].Sector,
                             tail-(Length(FDisc[dir].Entries)*4)+ref*4,dirbuffer);
      //Attributes
      Write24b(a                                       ,head+$10+ref*$1C,dirbuffer);
      //Length of filename (not including CR)
      Write24b(Length(temp)-1                          ,head+$14+ref*$1C,dirbuffer);
      //Where to find the filename
      Write24b(heapctr                                 ,head+$18+ref*$1C,dirbuffer);
      //Write the filename
      for i:=0 to (((Length(temp)+3)div 4)*4)-1 do
      begin
       c:=0; //Padded with nulls to word boundary
       if i<Length(temp) then c:=Ord(temp[i+1]);
       if c=32 then c:=c OR $80;
       //dir address+header+length of all entries+counter into heap+character
       WriteByte(c,head+heapctr+i+Length(FDisc[dir].Entries)*$1C,dirbuffer);
      end;
      //Move the heap counter on
      inc(heapctr,((Length(temp)+3)div 4)*4);
     end;
    end;
   If FDirType=diADFSBigDir then
    Write32b(heapctr,$14,dirbuffer); //BigDirNamesSize
   //Update the checksum
   if FDirType=diADFSOldDir then //Old - can be zero
    WriteByte($00,$4FF,dirbuffer);
   if FDirType=diADFSNewDir then //New
    WriteByte(CalculateADFSDirCheck($0,dirbuffer),$7FF,dirbuffer);
   if FDirType=diADFSBigDir then //Big
    WriteByte(CalculateADFSDirCheck($0,dirbuffer),Read32b($0C,dirbuffer)-1,dirbuffer);
   //Write the directory back out to the image
   WriteFragmentedData(fragments,dirbuffer);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update attributes on a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateADFSFileAttributes(filename,attributes: String): Boolean;
var
// ptr,
 dir,
 entry : Cardinal;
begin
 //Is this on the AFS partition?
 if FAFSPresent then
  if LeftStr(filename,Length(afsrootname))=afsrootname then
  begin
   Result:=UpdateAFSAttributes(filename,attributes);
   exit;
  end;
 //Is this on the DOS partition?
 if FDOSPresent then
  if LeftStr(filename,Length(dosrootname))=dosrootname then
  begin
   Result:=UpdateDOSAttributes(filename,attributes);
   exit;
  end;
 Result:=False;
 //Make sure that the file exists, but also to get the pointer
 if FileExists(filename,dir,entry) then
 begin
  //Change the attributes on the local copy
  FDisc[dir].Entries[entry].Attributes:=attributes;
  //Then update the catalogue
  UpdateADFSCat(GetParent(dir));
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
  if FDirType=diADFSOldDir then filename[i]:=chr(ord(filename[i])AND$7F);
  //and remove control codes
  if ord(filename[i])<32 then
   filename[i]:=chr(ord(filename[i])+32);
 end;
 //Is it not too long
 if FDirType<diADFSBigDir then
  filename:=Copy(filename,1,10);
 //Remove any forbidden characters
 for i:=1 to Length(filename) do
  if((FDirType<diADFSBigDir)AND(Pos(filename[i],illegalOld)>0))
  OR((FDirType=diADFSBigDir)AND(Pos(filename[i],illegalBig)>0))then
   filename[i]:='_';
 Result:=filename;
end;

{-------------------------------------------------------------------------------
Retitle an ADFS directory
-------------------------------------------------------------------------------}
function TDiscImage.RetitleADFSDirectory(filename,newtitle: String): Boolean;
var
 entry,
 dir    : Cardinal;
begin
 Result:=False;                           
 //ADFS Big Directories do not have titles
 if FDirType=diADFSBigDir then exit;
 //Check that the file exists, or is the root
 if(FileExists(filename,dir,entry))OR(filename=root_name)then
 begin
  if filename=root_name then
  begin
   //Re-title the directory, limiting it to 19 characters
   FDisc[0].Title:=LeftStr(newtitle,19);
   //Update the catalogue, which will update the title
   UpdateADFSCat(root_name);
  end
  else
  begin
   //Has it been read in?
   if not FDisc[FDisc[dir].Entries[entry].DirRef].BeenRead then
    ReadDirectory(filename);
   //Re-title the directory, limiting it to 19 characters
   FDisc[FDisc[dir].Entries[entry].DirRef].Title:=LeftStr(newtitle,19);
   //Update the catalogue, which will update the title
   UpdateADFSCat(GetParent(dir)+dir_sep
                +FDisc[dir].Entries[entry].Filename);
  end;
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
 space  : Integer;
 swap   : TDirEntry;
 changed: Boolean;
begin
 //Is this on the AFS partition
 if FAFSPresent then
  if LeftStr(oldfilename,Length(afsrootname))=afsrootname then
  begin
   Result:=RenameAFSFile(oldfilename,newfilename);
   exit;
  end;
 //Is this on the DOS partition
 if FDOSPresent then
  if LeftStr(oldfilename,Length(dosrootname))=dosrootname then
  begin
   Result:=RenameDOSFile(oldfilename,newfilename);
   exit;
  end;
 Result:=-2; //File does not exist
 //Check that the new name meets the required ADFS filename specs
 newfilename:=ValidateADFSFilename(newfilename);
 //Check that the file exists
 if FileExists(oldfilename,dir,entry) then
 begin
  Result:=-3; //New name already exists
  //Make sure the new filename does not already exist
  if(not FileExists(GetParent(dir)+dir_sep+newfilename,ptr))
  // or the user is just changing case
  or(LowerCase(GetParent(dir)+dir_sep+newfilename)=LowerCase(oldfilename))then
  begin
   Result:=-1;//Unknown error
   //Get the difference in lengths
   space:=Length(FDisc[dir].Entries[entry].Filename)-Length(newfilename);
   //Big Dir - Verify directory is big enough or if it needs extending and moved.
   if FDirType=diADFSBigDir then
    if not ExtendADFSBigDir(dir,space,False) then
    begin
     Result:=-4; //Cannot extend
     exit;
    end;
   //Are we renaming a directory?
   if FDisc[dir].Entries[entry].DirRef<>-1 then
   begin                                                             
    //If the title is the same, change it also
    if FDisc[FDisc[dir].Entries[entry].DirRef].Title=FDisc[dir].Entries[entry].Filename then
     FDisc[FDisc[dir].Entries[entry].DirRef].Title:=newfilename;
    //This will update the catalogue for that directory, updating the title too
    UpdateADFSCat(GetParent(dir)+dir_sep
                 +FDisc[dir].Entries[entry].Filename,newfilename);
    FDisc[FDisc[dir].Entries[entry].DirRef].Directory:=newfilename;
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
   UpdateADFSCat(GetParent(dir));
   Result:=entry;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Consolidate an ADFS Free space map
-------------------------------------------------------------------------------}
procedure TDiscImage.ConsolidateADFSFreeSpaceMap;
var
 ptr,ref,
 pos     : Cardinal;
 i,p,len,
 start   : Integer;
 fslinks : TFragmentArray;
 linklen : Byte;
 changed : Boolean;
begin
 if not FMap then //Old map only
 begin //Bug here - deleting multiple files does not update the FSM
  SetLength(fslinks,0);
  //Just make sure the current FS Map is in order, not fragmented and FreeEnd is correct
  //Total number of entries
  ptr:=ReadByte($1FE);
  //Start at the beginning
  ref:=0;
  //Add each one
  while ref<ptr do
  begin
   //FreeStart
   start:=Read24b($000+ref);
   //FreeLen
   len  :=Read24b($100+ref);
   if len>0 then //Only if the length isn't zero
   begin
    //Add a new entry, in order using insertion sort
    SetLength(fslinks,Length(fslinks)+1);
    pos:=Length(fslinks)-1;
    if pos>0 then
     for i:=pos-1 downto 0 do
      if fslinks[i].Offset>start then
      begin
       fslinks[i+1].Offset:=fslinks[i].Offset;
       fslinks[i+1].Length:=fslinks[i].Length;
       pos:=i;
      end;
    fslinks[pos].Offset:=start;
    fslinks[pos].Length:=len;
   end;
   inc(ref,3);
  end;
  //Concatenate entries
  if Length(fslinks)>2 then
  begin
   i:=0;
   while i<Length(fslinks)-2 do
   begin
    //if concurrent
    if fslinks[i].Offset+fslinks[i].Length=fslinks[i+1].Offset then
    begin
     //Add them together
     inc(fslinks[i+1].Length,fslinks[i].Length);
     fslinks[i+1].Offset:=fslinks[i].Offset;
     //And remove the next entry
     if i<Length(fslinks)-2 then
     begin
      //Iterate through the above entries
      for ref:=i to Length(fslinks)-2 do
      begin
       //Move each one down by one
       fslinks[ref].Offset:=fslinks[ref+1].Offset;
       fslinks[ref].Length:=fslinks[ref+1].Length;
      end;
      //And decrease the length
      SetLength(fslinks,Length(fslinks)-1);
     end;
    end;
    inc(i);
   end;
  end;
  ptr:=Length(fslinks)*3;
  //Write back FreeEnd
  WriteByte(ptr,$1FE);
  //And all the entries
  for i:=0 to 81 do
  if i<Length(fslinks) then
  begin
   Write24b(fslinks[i].Offset,$000+(i*3));
   Write24b(fslinks[i].Length,$100+(i*3));
  end
  else
  begin //Blank off the extra entries
   Write24b(0,$000+(i*3));
   Write24b(0,$100+(i*3));
  end;
  //Update the checksums
  WriteByte(ByteCheckSum($0000,$100,False),$0FF);
  WriteByte(ByteCheckSum($0100,$100,False),$1FF);
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
Consolidate fragments in an ADFS New Map Free space map
-------------------------------------------------------------------------------}
procedure TDiscImage.ConsolidateADFSFragments(fragid: Cardinal);
var
 fragments : TFragmentArray;
 frag,i,
 dirlen    : Cardinal;
begin
 //Only for new map
 if FMap then
 begin
  //Get the array of fragments (as offsets from the start of the bootmap)
  //This array is only used to keep track of the fragments.
  fragments:=NewDiscAddrToOffset(fragid,False);
  //Only need to continue if there is more than 1 fragment
  if Length(fragments)>1 then
  begin
   //Work out the total length
   dirlen:=0;
   for i:=0 to Length(fragments)-1 do inc(dirlen,fragments[i].Length);
   //Start off with the first one
   frag:=0;
   //Continue until the last (the number of fragments may reduce)
   while frag<Length(fragments)-1 do
   begin
    //Is the next one adjacent to this one?
    if (fragments[frag].Offset+fragments[frag].Length=fragments[frag+1].Offset)
    and(fragments[frag].Zone=fragments[frag+1].Zone)then
    begin
     //Blank off the stop bit of the first, and ID of the second
     WriteBits($0,((fragments[frag+1].Offset-1) DIV 8)
                 +(fragments[frag+1].Zone*secsize)+bootmap+1,
                   (fragments[frag+1].Offset-1) MOD 8,idlen);
     //Increase the size of this one
     inc(fragments[frag].Length,fragments[frag+1].Length);
     //And remove the next one
     if frag+1<Length(fragments)-2 then //If it is not the last
      for i:=frag+1 to Length(fragments)-2 do
       fragments[i]:=fragments[i+1];
     //And drop the last one off the list
     SetLength(fragments,Length(fragments)-1);
    end;
    //Next one
    inc(frag);
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Delete an ADFS file/directory
-------------------------------------------------------------------------------}
function TDiscImage.DeleteADFSFile(filename: String;
                    TreatAsFile:Boolean=False;extend:Boolean=True):Boolean;
var
 dirref,
 entry,
 dir,
 addr,
 len,i     : Cardinal;
 success   : Boolean;
 fileparent: String;
begin 
 UpdateProgress('Deleting '+filename);
 Result:=False;
 //Is this an AFS file?
 if FAFSPresent then
  if LeftStr(filename,Length(afsrootname))=afsrootname then
  begin
   Result:=DeleteAFSFile(filename);
   exit;
  end;
 //Is this a DOS file?
 if FDOSPresent then
  if LeftStr(filename,Length(dosrootname))=dosrootname then
  begin
   Result:=DeleteDOSFile(filename);
   exit;
  end;
 //Check that the file exists
 if(FileExists(filename,dir,entry))or((filename=root_name)and(FDirType=diADFSBigDir))then
 begin
  //If we are deleting the root (usually only when extending/contracting)
  if(filename=root_name)and(FDirType=diADFSBigDir)then
  begin
   entry:=$FFFF;
   dir  :=$FFFF;
  end;
  success:=True;
  if filename<>root_name then
  begin
   //Make a note of the parent - these will become invalid soon
   fileparent:=FDisc[dir].Entries[entry].Parent;//GetParent(dir);
   //Take a note of where it is on disc
   addr:=FDisc[dir].Entries[entry].Sector;
   //And how much space it took up
   len:=FDisc[dir].Entries[entry].Length;
   //Is this file the currently open DOS Partition?
   if FDisc[dir].Entries[entry].isDOSPart then exit(False); //Then fail
   //If directory, delete contents first
   if(FDisc[dir].Entries[entry].DirRef>0)and(not TreatAsFile)then
   begin
    dirref:=FDisc[dir].Entries[entry].DirRef;
    FDisc[dirref].Deleted:=True;
    //Has it been read in?
    if not FDisc[dirref].BeenRead then
     ReadDirectory(filename);
    //We'll do a bit of recursion to remove each entry one by one. If it
    //encounters a directory, that will get it's contents deleted, then itself.
    if entry<Length(FDisc[dir].Entries) then
    while(Length(FDisc[dirref].Entries)>0)and(success)do
     //If any fail for some reason, the whole thing fails
     success:=DeleteADFSFile(filename+dir_sep+FDisc[dirref].Entries[0].Filename);
    //Remove the directory from the internal array
    if dirref<Length(FDisc)-2 then
     for i:=dirref to Length(FDisc)-2 do
      FDisc[i]:=FDisc[i+1];
    SetLength(FDisc,Length(FDisc)-1);
    //Update all the directory references
    UpdateDirRef(dirref);
   end;
  end;
  //Only continue if we are successful
  if success then
  begin
   if filename<>root_name then
   begin
    //Round up to the next whole sector
    i:=len DIV$100;
    if len MOD$100>0 then inc(i);
    len:=i;
    //Remove it from the internal array
    ReduceADFSCat(dir,entry);
    //Remove from the catalogue
    UpdateADFSCat(fileparent);
   end;
   if filename=root_name then
    addr:=rootfrag;//Read32b(bootmap+$0C+4); //ID of the root
   //Add to the free space map
   if not FMap then ADFSDeAllocateFreeSpace(addr,len); //Old map
   if     FMap then ADFSDeAllocateFreeSpace(addr);     //New Map
   //Big Dir - Verify if directory needs reduced.
   if(FDirType=diADFSBigDir)and(extend)then ExtendADFSBigDir(dir,0,False);
   //Tidy up the free space map, as we may have missed something
   ConsolidateADFSFreeSpaceMap;
   //Update the free space map
   ADFSFreeSpaceMap;
   //Return a success
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
De-allocates old map free space
-------------------------------------------------------------------------------}
procedure TDiscImage.ADFSDeAllocateFreeSpace(addr,len: Cardinal);
var
 FreeEnd : Byte;
 fs,fl,i : Cardinal;
begin
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
  if(addr+len>0)and(FreeEnd div 3<82)then
  begin
   Write24b(addr,$000+FreeEnd);
   Write24b(len ,$100+FreeEnd);
   inc(FreeEnd,3);
   WriteByte(FreeEnd,$1FE);
  end;
 end;
end;

{-------------------------------------------------------------------------------
De-allocates new map fragments
-------------------------------------------------------------------------------}
procedure TDiscImage.ADFSDeAllocateFreeSpace(addr: Cardinal);
function GetLinkLen(Loffset: Cardinal): Byte;
begin
 //Adjust number of bits to write
 Result:=idlen; //In the map
 if Loffset DIV 8=0 then Result:=15; //In the header
end;
var
 linklen    : Byte;
 fragments,
 fsfragments: TFragmentArray;
 fs,i,ptr,
 dir,entry  : Cardinal;
 delfsm     : Boolean;
 lastzone   : Integer;
begin
 if FMap then //New Map
 begin
  //Check to see if it is OK to delete the FSM fragment
  delfsm:=True;
  //Is it a shared object?
  if addr mod$100>0 then
   //Go through the entire catalogue looking for another with the same fragment
   if Length(FDisc)>0 then
    for dir:=0 to Length(FDisc)-1 do //Go through each directory
     if Length(FDisc[dir].Entries)>0 then
      for entry:=0 to Length(FDisc[dir].Entries)-1 do //And each file within
       if(FDisc[dir].Entries[entry].Sector div $100=addr div$100)
       and(FDisc[dir].Entries[entry].Sector<>addr)then
        delfsm:=False; //If we find another, then we shall not remove the fragment
  //Only continue if it is OK
  if delfsm then
  begin
   //Get all the fragments - we do this every time as we will be updating the
   //list each time
   fragments:=NewDiscAddrToOffset(addr,False);
   lastzone:=-1;
  if Length(fragments)=0 then//ERROR - WE SHOULD HAVE FRAGMENTS ****************
   addr:=addr; //***************************************************************
   //Go through each one
   if Length(fragments)>0 then
    for i:=0 to Length(fragments)-1 do
    begin
     //Get all the free space fragments from the zone we are looking at
     if fragments[i].Zone<>lastzone then
      fsfragments:=ADFSGetFreeFragments(False,fragments[i].Zone);
     lastzone:=fragments[i].Zone;
     //There are no free fragments in this zone
     if Length(fsfragments)=0 then
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
      fs:=0;
      ptr:=fsfragments[fs].Offset;//Use as a counter
      while(fs<Length(fsfragments)-1)AND(ptr<fragments[i].Offset)do
      begin //We'll now find the first pointer past this fragment
       inc(fs);
       inc(ptr,fsfragments[fs].Offset);
      end;
      //We exit the loop with ptr more than the offset so we adjust the previous entry
      if ptr>fragments[i].Offset then
      begin
       linklen:=GetLinkLen(ptr-fsfragments[fs].Offset);
       //Write the pointer to our fragment
       WriteBits(fragments[i].Offset-(ptr-fsfragments[fs].Offset),
                                     ((ptr-fsfragments[fs].Offset)DIV 8)
                                     +(fsfragments[fs].Zone*secsize)+bootmap+1,
                                     (ptr-fsfragments[fs].Offset)MOD 8,
                                     linklen);
       //Now replace the fragment ID and add the distance to the next.
       dec(ptr,fragments[i].Offset);
       linklen:=GetLinkLen(fragments[i].Offset);
       WriteBits(ptr,(fragments[i].Offset DIV 8)
                    +(fragments[i].Zone*secsize)+bootmap+1,
                    (fragments[i].Offset) MOD 8,
                    linklen);
      end
      else  //We exit the loop with ptr not as high, so we need to add an entry
      begin // and update the final fragment
       //Write the new pointer to our fragment
       linklen:=GetLinkLen(ptr);
       WriteBits(fragments[i].Offset-ptr,(ptr DIV 8)
                                        +(fragments[i].Zone*secsize)+bootmap+1,
                                        ptr MOD 8,linklen);
       linklen:=GetLinkLen(fragments[i].Offset);
       //Now write zero in place of our fragid
       WriteBits(0,(fragments[i].Offset DIV 8)
                  +(fragments[i].Zone*secsize)+bootmap+1,
                  fragments[i].Offset MOD 8,
                  linklen);
      end;
     end;
    end;
   //Update the checksums
   for i:=0 to nzones-1 do
   begin
    //Ensure the top bit is set on the first link for each zone
    WriteByte(ReadByte(bootmap+2+i*secsize)OR$80,bootmap+2+i*secsize);
    //Zone checks
    WriteByte(GeneralChecksum(bootmap+$00+(i*secsize),
                              secsize,secsize+4,$4,true),
              bootmap+(i*secsize)+$00);
   end;
   //Make a copy
   for ptr:=0 to (nzones*secsize)-1 do
    WriteByte(ReadByte(bootmap+ptr),bootmap+ptr+nzones*secsize);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path
-------------------------------------------------------------------------------}
function TDiscImage.ExtractADFSFile(filename: String;
                                             var buffer: TDIByteArray): Boolean;
var
 entry,dir,
 filelen       : Cardinal;
 fragments     : TFragmentArray;
begin
 Result   :=False;
 fragments:=nil;
 //Is this on an AFS partition?
 if FAFSPresent then
  if LeftStr(filename,Length(afsrootname))=afsrootname then
  begin
   Result:=ExtractAFSFile(filename,buffer);
   exit;
  end;
 //Is this on a DOS Partition?
 if FDOSPresent then
  if LeftStr(filename,Length(dosrootname))=dosrootname then
  begin
   Result:=ExtractDOSFile(filename,buffer);
   exit;
  end;
 //No, so look on ADFS
 if FileExists(filename,dir,entry) then //Does the file actually exist?
 //Yes, so load it - there is nothing to stop a directory header being extracted
 //if passed in the filename parameter.
 begin
  filelen:=FDisc[dir].Entries[entry].Length;
  //Get the starting position
  if not FMap then //Old Map
  begin
   SetLength(fragments,1);
   fragments[0].Offset:=FDisc[dir].Entries[entry].Sector*$100;
   fragments[0].Length:=filelen;
  end;
  if FMap then //New Map
   //Get the fragment offsets of the file
   fragments:=NewDiscAddrToOffset(FDisc[dir].Entries[entry].Sector);
  Result:=ExtractFragmentedData(fragments,filelen,buffer);
 end;
end;

{-------------------------------------------------------------------------------
Extracts data given fragments (New Map)
-------------------------------------------------------------------------------}
function TDiscImage.ExtractFragmentedData(fragments: TFragmentArray;
                            filelen: Cardinal;var buffer: TDIByteArray):Boolean;
var
 dest,
 len,
 source,
 frag   : Cardinal;
begin
 Result:=False;
 //Pointer into the fragment array
 frag   :=0;
 //No fragments have been given, or the file length is zero - we have an error
 if(Length(fragments)>0)and(filelen>0)then
 begin
  SetLength(buffer,filelen);
  dest  :=0;      //Length pointer/Destination pointer
  len   :=filelen;//Amount of data to read in
  repeat
   //We need to work out source and length
   if frag<Length(fragments) then
   begin
    source:=fragments[frag].Offset;           //Source of data
    len   :=fragments[frag].Length;           //Amount of data
   end;
   //Make sure we don't read too much
   if dest+len>filelen then
    len:=filelen-dest;
   //Read the data into the buffer
   ReadDiscData(source,len,0,dest,buffer);
   //Move the size pointer on, by the amount read
   inc(dest,len);
   //Get the next block pointer
   inc(frag);
  until dest>=filelen; //Once we've reached the file length, we're done
  Result:=True;
 end;
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
 if FileExists(filename,sdir,sentry) then
 begin
  Result:=-6;//Destination directory not found
  //Take a copy
  direntry:=FDisc[sdir].Entries[sentry];
  //Remember the original parent
  sparent:=GetParent(sdir);
  if(FileExists(directory,ddir,dentry))or(directory=root_name)then
  begin
   Result:=-10;//Can't move to the same directory
   //Destination directory reference
   if directory=root_name then ddir:=0//Root
   else ddir:=FDisc[ddir].Entries[dentry].DirRef;
   if ddir>=Length(FDisc) then exit(-12);
   if ddir<>sdir then //Can't move into the same directory
   begin
    //Has it been read in?
    if not FDisc[ddir].BeenRead then ReadDirectory(directory);
    Result:=-3; //File already exists in destination directory
    //Alter for the new parent
    direntry.Parent:=directory;
    //Does the filename already exist in the new location?
    if not FileExists(directory+Dir_Sep+direntry.Filename,ptr) then
    begin
     Result:=-5;//Unknown error
     //Extend the destination directory (Big Dir)
     if FDirType=diADFSBigDir then
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
     if FDirType=diADFSBigDir then ExtendADFSBigDir(sdir,0,False);
    end;
   end;
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
 //ADFS does fragment big directories, so the code below needs to reflect this
 Result:=False; //By default - fail to extend/contract
 //We won't take account of minus sizes, so zero it if it is
 if space<0 then space:=0;
 //Are we adding an extra entry?
 if add then inc(space,$1C+4); //Data after hdr ($1C) + before tail ($04)
 //Make sure it we are using big directories
 if FDirType=diADFSBigDir then
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
   if Length(frag)=0 then exit; //Could not find anything, so bye bye
   addr:=frag[0].Offset; //Only interested in the header
  end;
  dirsize:=Read32b(addr+$0C);                   //Length of directory
  //Tail size: number of entries * 4 (as the objects' indirect addresses are
  //stored at the end) + 8 (for the size of the tail)
  tail   :=$08+Read32b(addr+$10)*$4;
  //Header size: Length of name (padded to word boundary) + 1C (size of header)
  hdr    :=$1C+Ceil(Read32b(addr+$08)/4)*4;
  //Heap size: number of entries * 1C (size of an entry) + size of heap
  heapsize:=Read32b(addr+$10)*$1C+Read32b(addr+$14);
  //Is the directory oversized? Only shrink if space is passed as zero
  if(dirsize-(tail+hdr+heapsize)>=2048)and(space=0)then //Contract the directory size
    ADFSBigDirSizeChange(d,e,False);
  //If we are contracting, the result might as well be true, whether it happened
  if space=0 then Result:=True; //or not.
  if space>0 then //Look to extend
   //We'll add an extra 3 bytes to ensure there is enough space, including pad
   if dirsize-(tail+hdr+heapsize)>=space+3 then //to word boundary
    Result:=True //Doesn't need extending
   else //Directory needs to be extended (by 2048)
    Result:=ADFSBigDirSizeChange(d,e,True);
 end;
end;

{-------------------------------------------------------------------------------
Do the actual extending or contracting of a big directory
-------------------------------------------------------------------------------}
function TDiscImage.ADFSBigDirSizeChange(dir,entry:Cardinal;extend:Boolean):Boolean;
var
 dircache    : TDIByteArray;
 fragments,
 newfragments: TFragmentArray;
 dirsize,
 fragsize,
 fragid,tail,
 i,ctr       : Cardinal;
 copyADFS    : Boolean;
begin
 //extend = True  : extend by 2048 bytes
 //extend = False : contract by 2048 bytes
 Result:=False;
 fragsize:=0;
 copyADFS:=True; //Copy the behaviour of ADFS
 //Get the directory address, and then the fragments
 if(dir<>$FFFF)and(entry<>$FFFF)then //Not the root
  fragid:=FDisc[dir].Entries[entry].Sector
 else //Root
  fragid:=rootfrag;
 //Get the directory size from the header
 fragments:=NewDiscAddrToOffset(fragid);
 SetLength(newfragments,0);
 //Work out the size
 if Length(fragments)>0 then
 begin
  //Get the reported size of the directory, from the header
  dirsize:=Read32b(fragments[0].Offset+$C);
  //Then work out how much space the fragments are giving us.
  {When a directory is contracted in ADFS, the original fragment(s) are left to
  allow future expansion.}
  for i:=0 to Length(fragments)-1 do
   inc(fragsize,fragments[i].Length);
  //Load in the directory into a cache
  if ExtractFragmentedData(fragments,dirsize,dircache) then
  begin
   //Work out the tail size (8 + num of entries * 4)
   tail:=8+Read32b($10,dircache)*4;
   //Extending
   if extend then
   begin
    //Try and find space for 2048 bytes, using the existing fragment ID
    if dirsize+2048>fragsize then //But only if we need to
     newfragments:=ADFSFindFreeSpace(2048,fragid);
    //If it returns at least 1 fragment, then we can continue
    if(Length(newfragments)>0)or(dirsize+2048<=fragsize)then
    begin
     //Increase the length by 2048 bytes
     inc(dirsize,2048);
     SetLength(dircache,dirsize);
     //Update the header
     Write32b(dirsize,$0C,dircache);
     //Clear the new area
     for i:=dirsize-2048 to dirsize-1 do dircache[i]:=0;
     //Move the tail to the end and blank the middle
     for i:=1 to tail do
     begin
      dircache[dirsize-i]       :=dircache[(dirsize-2048)-i];
      dircache[(dirsize-2048)-i]:=0;
     end;
     //If we needed to find more fragments
     if Length(newfragments)>0 then
     begin
      //Allocate the space
      ADFSAllocateFreeSpace(2048,fragid>>8,newfragments);
      //Re-read the fragments (thereby joining the two, in order)
      fragments:=NewDiscAddrToOffset(fragid);
     end;
    end;
   end;
   if not extend then
   begin
    //Move the tail in by 2048 bytes
    for i:=1 to tail do
    begin
     dircache[ dirsize-i]      :=dircache[(dirsize+2048)-i];
     dircache[(dirsize+2048)-i]:=0;
    end;
    //Decrease the length by 2048 bytes
    dec(dirsize,2048);
    SetLength(dircache,dirsize);
    //Update the header
    Write32b(dirsize,$0C,dircache);
    //De-allocate the space - this will de-allocate all fragments for this ID
    if not copyADFS then
    begin
     //ADFS does not reduce the fragments when contracting
     ADFSDeAllocateFreeSpace(fragid);
     ConsolidateADFSFreeSpaceMap;
    end;
    //Reduce the fragments by 2048 bytes, from the end
    ctr:=2048;
    i:=Length(fragments);
    repeat
     //Reduce the fragment counter, to a minimum of zero
     if i>0 then dec(i);
     //If the last fragment is less than what we are reducing by
     if fragments[i].Length<ctr then
     begin
      //Decrease our counter by the length
      dec(ctr,fragments[i].Length);
      //And remove the fragment
      SetLength(fragments,Length(fragments)-1);
     end
     else //Otherwise just remove what is remaining from the length of the fragment
     begin
      dec(fragments[i].Length,ctr);
      //Has it got to zero? Then remove the fragment
      if fragments[i].Length=0 then SetLength(fragments,Length(fragments)-1);
      //And reduce the counter to zero
      ctr:=0;
     end;
    until(ctr=0)or(i=0);
    //Re-allocate the reduced fragments
    if not copyADFS then
     ADFSAllocateFreeSpace(dirsize,fragid>>8,fragments);
   end;
   //Update the checksum
   dircache[dirsize-1]:=CalculateADFSDirCheck(0,dircache);
   //Write the directory cache back
   Result:=WriteFragmentedData(fragments,dircache);
   //Consolidate the fragments
   if extend then ConsolidateADFSFragments(fragid);
   //And update the entry
   if(dir<>$FFFF)and(entry<>$FFFF)then //As long as it isn't the root
    FDisc[dir].Entries[entry].Length:=dirsize
   else //But, for the root, we need to update the disc record
    Write32b(dirsize,bootmap+$30+4);
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
 if GetMajorFormatNumber=diAcornADFS then //Can't fix it if it isn't ADFS
 begin
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
        Result:=FixADFSDirectory(dir,entry)or Result;//Set to true to indicate it has changed
  end;
  ReadADFSDisc; //Rescan the image
 end;
end;

{-------------------------------------------------------------------------------
Attempts to fix a broken ADFS directory
-------------------------------------------------------------------------------}
function TDiscImage.FixADFSDirectory(dir,entry: Integer): Boolean;
var
 len       : Cardinal;
 dirref,i  : Integer;
 error,
 tail      : Byte;
 fragments : TFragmentArray;
 StartName,
 EndName   : String;
 dircache  : TDIByteArray;
begin
 Result:=False;
 //Get the directory reference
 if(dir>=0)and(entry>=0) then
  dirref:=FDisc[dir].Entries[entry].DirRef //Sub directory
 else
  dirref:=0; //Root
 //What is the error?
 error:=FDisc[dirref].ErrorCode;
 if error>0 then //Only act if there is an error
 begin
  //Where is the directory, and how big?
  len:=0;
  //We need to resolve the actual disc offset and length
  if(dir>=0)and(entry>=0) then
  begin
   if FMap then //New Map
   begin
    //Get the fragments for the directory (should only be one)
    fragments:=NewDiscAddrToOffset(FDisc[dir].Entries[entry].Sector);
    len:=0;
    //Work out the total length
    if Length(fragments)>0 then
     for i:=0 to Length(fragments)-1 do inc(len,fragments[i].Length);
   end;
   if not FMap then //Old Map
   begin
    SetLength(fragments,1);
    fragments[0].Offset:=FDisc[dir].Entries[entry].Sector*$100;
    len:=FDisc[dir].Entries[entry].Length;
    fragments[0].Length:=len;
   end;
  end
  else //Root
  begin
   //As above
   fragments:=NewDiscAddrToOffset(rootfrag);
   len:=0;
   //Work out the total length
   if Length(fragments)>0 then
    for i:=0 to Length(fragments)-1 do inc(len,fragments[i].Length);
  end;
  if Length(fragments)>0 then
  begin
   //Retrieve the directory into a cache
   if ExtractFragmentedData(fragments,len,dircache) then
   begin
    //Tail length
    if FDirType=diADFSOldDir then tail:=$35;
    if FDirType=diADFSNewDir then tail:=$29;
    if FDirType=diADFSBigDir then tail:=$08;
    //First basic check to see if the directory structure is where it should be
    Result:=True; //This can happen for interleaved images
    if(FDirType=diADFSOldDir)and(Length(FDisc[dirref].Entries)=0)
    and(ReadString(1,-4,dircache)<>'Hugo')
    and(ReadString((len-6),-4,dircache)<>'Hugo')
    and(error AND $02<>$02)then //Only if this is not the reason why it is broken
     Result:=False; //We will assume that if neither are Hugo, then the dir is somewhere else
    if Result then //So we will only fix if we can
    begin
     //Start the fixes
     if (error AND $01=$01) then //StartSeq<>EndSeq
     begin
      //Quite simple - just pick up StartSeq and write it to EndSeq
      if FDirType=diADFSOldDir then WriteByte(ReadByte(0,dircache),(len-tail)+$2F,dircache);
      if FDirType=diADFSNewDir then WriteByte(ReadByte(0,dircache),(len-tail)+$23,dircache);
      if FDirType=diADFSBigDir then WriteByte(ReadByte(0,dircache),(len-tail)+$04,dircache);
     end;
     if (error AND $02=$02) then //StartName<>EndName (Old/New Dirs)
     begin
      //Almost as simple - just re-write what they should be
      if FDirType=diADFSOldDir then StartName:='Hugo';
      if FDirType=diADFSNewDir then StartName:='Nick';
      for i:=1 to 4 do
      begin
       WriteByte(Ord(StartName[i]),i,dircache);        //Header
       WriteByte(Ord(StartName[i]),(len-6)+i,dircache);//Tail
      end;
     end;
     if (error AND $04=$04) then //StartName<>'SBPr' or EndName<>'oven' (Big)
     begin
      //The same as previously, except start and end do not match
      StartName:='SBPr';
      EndName  :='oven';
      for i:=1 to 4 do
      begin
       WriteByte(Ord(StartName[i]),3+i,dircache);             //Header
       WriteByte(Ord(EndName[i])  ,(len-tail)+(i-1),dircache);//Tail
      end;
     end;
     //Bit 3 indicates invalid checksum - but we'll update anyway
     //The above changes could alter it
     if FDirType=diADFSOldDir then //Old - can be zero
      WriteByte($00,$4FF,dircache)
     else               //New
      WriteByte(CalculateADFSDirCheck(0,dircache),len-1,dircache);
     //Write the directory back
     if WriteFragmentedData(fragments,dircache) then
     begin
      //Reset the flags
      FDisc[dirref].Broken:=False;
      FDisc[dirref].ErrorCode:=$00;
     end;
    end;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Calculates the parameters for a new map hard drive
-------------------------------------------------------------------------------}
function TDiscImage.ADFSGetHardDriveParams(Ldiscsize:Cardinal;bigmap,ide:Boolean;
              var Lidlen,Lzone_spare,Lnzones,Llog2bpmb,Lroot,
                  Llog2secsize,Llowsec: Cardinal):Boolean;
 //Adapted from the RISC OS RamFS ARM code procedure InitDiscRec in RamFS50
var
 r0,r1,r2,r3,r4,r6,r7,r8,r9,r10,r11,
 lr,minidlen: Integer;
label //Don't like using labels and GOTO, but it was easier to adapt from the ARM code
 {FT01,FT02,}FT10,FT20,FT30,FT35,FT40,FT45,FT50,FT60,FT70,FT80,FT90;
const
 maxidlen=21;			//Maximum possible
 minlog2bpmb=8;                 //RamFS starts at 7. I've found better results with 8
 maxlog2bpmb=12;
 minzonespare=32;
 maxzonespare=128;              //RamFS limit is 64. The upper limit can be $FFFF
 minzones=1;
 maxzones=127;			//RamFS limit is 127. The upper limit can be $FFFF
 zone0bits=8*60;
 bigdirminsize=2048;
 newdirsize=$500;
 //Llog2secsize=9;                //Min is 8, max is 12. HForm fixes this at 9.
begin
 if ide then //IDE format
 begin
  Llog2secsize:=9;
  Llowsec:=1;
 end
 else  //ST506 format
 begin
  Llog2secsize:=8;
  Llowsec:=0;
 end;
 //heads should be 16, and secspertrack should be 63
 Result:=False;
  minidlen:=Llog2secsize+3;     //idlen MUST be at least log2secsize+3
  r0:=minlog2bpmb;		//Initialise log2bpmb
 FT10:
  r4:=Ldiscsize>>r0;		//Map bits for disc
  r1:=minzonespare;		//Initialise zone_spare
 FT20:
  r6:=(8<<Llog2secsize)-r1;	//Bits in a zone Minus sparebits
  r2:=minzones;			//Minimum of one zone
  r7:=r6-zone0bits;		//Minus bits in zone 0
 FT30:
  IF r7>r4 THEN GOTO FT35;	//Do we have enough allocation bits yet? then accept
  inc(r7,r6);			//More map bits
  inc(r2,1);			//and another zone
  IF r2<=maxzones THEN GOTO FT30;//Still OK?
  GOTO FT80;			//Here when too many zones, try a higher log2bpmb
 FT35:
				 //Now we have to choose idlen. We want idlen to be
				 //the smallest it can be for the disc.
  r3:=minidlen;			//Minimum value of idlen
 FT40:
  r8:=r6 DIV (r3+1);		//ids per zone
  r9:=1<<r3;			//work out 1<<idlen
  lr:=r8*r2;			//total ids needed
  IF lr>r9 THEN GOTO FT60;	//idlen too small?
				 //We're nearly there. Now to work out if the last zone
				 //can be handled correctly.
  lr:=r7-r4;
  IF lr=0 THEN GOTO FT50;
  IF lr<r3 THEN GOTO FT60;	//Must be at least idlen+1 bits
				 //Check also that we're not too close to the start of the zone
  lr:=r7-r6;			//Get the start of the zone
  lr:=r4-lr;			//lr = bits available in last zone
  IF lr<r3 THEN GOTO FT60;
				 //If the last zone is the map zone (ie nzones<=2), check it's
				 //big enough to hold 2 copies of he map+the root directory
  IF r2>2 THEN GOTO FT50;
  r10:=r2*(2<<Llog2secsize);	//r10 = 2*map size (in disc bytes)
  r11:=(1<<r0)-1;		//r11 = LFAU-1 (in disc bytes), for rounding up
  IF not bigmap THEN
  begin
   inc(r10,newdirsize);		//Short filename: add dir size to map
   GOTO FT45;
  end;
				 //Long filename case - root is separate object in map zone
  r9:=(r11+bigdirminsize)>>r0;   //r9=directory size (in map bits)
  IF r9<=r3 THEN r9:=r3+1;	//Ensure at least idlen+1
  dec(lr,r9);
  IF lr<0 THEN GOTO FT60;
 FT45:
  inc(r10,r11);
  r10:=r10>>r0;			//r10=map (+dir) size (in map bits)
  IF r10<=r3 THEN r10:=r3+1;	//Ensure at least idlen+1
  IF lr<r10 THEN GOTO FT60;
 FT50:				//We've found a result - fill in the disc record
  Lidlen:=r3;
  Lzone_spare:=r1;
  Lnzones:=r2;
  Llog2bpmb:=r0;
  Result:=True; //Mark as result found
{  IF not bigmap THEN GOTO FT01; //Do we have long filenames?
			        //The root dir's ID is the first available ID in the middle
				//zone of the map
  r2:=r2>>1;			//zones/2
  IF r2<>0 THEN lr:=r2*r8	// * ids per zone
           ELSE lr:=3;		//If zones/2=0 then only one zone so ID is 3
  lr:=(lr<<8)OR 1;		//Construct full indirect disc address with sharing offset of 1
  GOTO FT02;}
 //FT01:				//not long filenames. root dir is &2nn where nn is ((zones<<1)+1)
  lr:=(r2<<1)+$201;
 //FT02:
  Lroot:=lr;
  GOTO FT90;
 FT60:                           //Increase idlen
  inc(r3);
  IF r3<=maxidlen THEN GOTO FT40;
 FT70:                           //Increase zone_spare
  inc(r1);
  IF r1<=maxzonespare THEN GOTO FT20;
 FT80:                           //Increase log2bpmb
  inc(r0);
  IF r0<=maxlog2bpmb THEN GOTO FT10;
 FT90:
end;

{-------------------------------------------------------------------------------
Update a file's load or execution address
-------------------------------------------------------------------------------}
function TDiscImage.UpdateADFSFileAddr(filename:String;newaddr:Cardinal;
                                                           load:Boolean):Boolean;
var
 dir,
 entry: Cardinal;
begin
 Result:=False;
 //Ensure the file actually exists
 if FileExists(filename,dir,entry) then
 begin
  //Are they valid?
  if dir<Length(FDisc)then
   if entry<Length(FDisc[dir].Entries)then
   begin
    //Update our entry
    if load then FDisc[dir].Entries[entry].LoadAddr:=newaddr AND$FFFFFFFF
            else FDisc[dir].Entries[entry].ExecAddr:=newaddr AND$FFFFFFFF;
    //And update the catalogue for the parent directory
    UpdateADFSCat(GetParent(dir));
    //Return a positive result
    Result:=True;
   end;
 end;
end;

{-------------------------------------------------------------------------------
Update a file's filetype
-------------------------------------------------------------------------------}
function TDiscImage.UpdateADFSFileType(filename:String;newtype:String):Boolean;
var
 ptr,
 dir,
 entry: Cardinal;
 newft: Integer;
begin
 Result:=False;
 ptr:=0;
 //Ensure the file actually exists
 if FileExists(filename,dir,entry) then
 begin
  //Are they valid?
  if dir<Length(FDisc)then
   if entry<Length(FDisc[dir].Entries)then
   begin
    //Hex number?
    if IntToHex(StrToIntDef('$'+newtype,0),3)<>UpperCase(newtype) then
     newft:=GetFileTypeFromName(newtype) //No, so translate
    else
     newft:=StrToInt('$'+newtype);       //Yes, just convert
    //Valid filetype?
    if newft>=0 then
    begin
     //Calculate the new load address
     ptr:=FDisc[dir].Entries[entry].LoadAddr;
     //Set the top 12 bits to indicate filetyped
     ptr:=ptr OR$FFF00000;
     //Clear the filetype area, preserving the rest
     ptr:=ptr AND$FFF000FF;
     //Set the filetype
     ptr:=ptr OR(newft<<8);
     //Now just make the call to update the load address
     if UpdateADFSFileAddr(filename,ptr,True) then
     begin
      //And update the local copy of the filetypes
      FDisc[dir].Entries[entry].ShortFileType:=IntToHex(newft,3);
      FDisc[dir].Entries[entry].Filetype:=GetFileTypeFromNumber(newft);
      //And set a positive result
      Result:=True;
     end;
    end;
   end;
 end;
end;

{-------------------------------------------------------------------------------
Update a file's timestamp
-------------------------------------------------------------------------------}
function TDiscImage.UpdateADFSTimeStamp(filename:String;newtimedate:TDateTime):Boolean;
var
 ptr,
 dir,
 entry: Cardinal;
 rotd : Int64;
begin
 //Is this on an AFS partition?
 if FAFSPresent then
  if LeftStr(filename,Length(afsrootname))=afsrootname then
  begin
   Result:=UpdateAFSTimeStamp(filename,newtimedate);
   exit;
  end;
 //Is this on a DOS Partition?
 if FDOSPresent then
  if LeftStr(filename,Length(dosrootname))=dosrootname then
  begin
   Result:=UpdateDOSTimeStamp(filename,newtimedate);
   exit;
  end;
 Result:=False;
 ptr:=0;
 //Ensure the file actually exists
 if FileExists(filename,dir,entry) then
 begin
  //Are they valid?
  if dir<Length(FDisc)then
   if entry<Length(FDisc[dir].Entries)then
   begin
    //Convert to RISC OS time format
    rotd:=TimeDatetoRISCOS(newtimedate);//RISC OS TimeDate is 40 bits long
    //Calculate the new load address
    ptr:=FDisc[dir].Entries[entry].LoadAddr;
    //Set the top 12 bits to indicate filetyped, and preserve the rest
    ptr:=ptr OR$FFF00000;
    //Clear the bottom 8 bits
    ptr:=ptr AND$FFFFFF00;
    //Set the bottom 8 bits with the new time
    ptr:=ptr OR(rotd>>32);
    //Update the load address
    if UpdateADFSFileAddr(filename,ptr,True) then
    begin
     //Now calculate the new exec address
     ptr:=rotd AND $FFFFFFFF;
     //Update the exec address
     if UpdateADFSFileAddr(filename,ptr,False) then
     begin
      FDisc[dir].Entries[entry].TimeStamp:=newtimedate;
      Result:=True;
     end;
    end;
   end;
 end;
end;

{-------------------------------------------------------------------------------
Extract an ADFS, AFS or DOS partition
-------------------------------------------------------------------------------}
function TDiscImage.ExtractADFSPartition(side: Cardinal): TDIByteArray;
var
 index,
 diff       : Integer;
 start,
 new1,
 new2       : Cardinal;
 JesMapList : Array of Cardinal;
 JesMap     : String;
 buffer     : TDIByteArray;
begin
 Result:=nil;
 if(FAFSPresent)or(FDOSPresent)then //Make sure it is a hybrid
 begin
  //Side 0 : must be ADFS
  if side=0 then
  begin
   //Extracting the ADFS part is simply just blanking the AFS partition, and the
   //ADFS disc title (so that $0F6 and $1F6 do not point to anything).
   new1:=GetDataLength;
   SetLength(Result,new1);
   //Copy the data across - this will de-interleave it
   UpdateProgress('Extracting Partition');
   for index:=0 to new1-1 do Result[index]:=ReadByte(index);
   UpdateProgress('Extending ADFS Partition');
   //Blank off the addresses
   if FAFSPresent then //AFS Only
   begin
    Write24b(0,$0F6,Result);
    Write24b(0,$1F6,Result);
   end;
   //Add a free space entry
   index:=ReadByte($1FE,Result);
   if index<$F3 then
   begin
    //Start
    start:=0;
    if FAFSPresent then start:=Read24b($0FC,Result);
    if FDOSPresent then start:=disc_size[0]>>8;
    Write24b(start,$000+index,Result);
    //Length
    Write24b((Length(Result)>>8)-start,$100+index,Result);
    //Pointer
    inc(index,3);
    WriteByte(index,$1FE,Result);
   end;
   //Expand the image size
   Write24b(Length(Result)>>8,$0FC,Result);
   //Blank off the other partition
   for index:=disc_size[0] to new1-1 do Result[index]:=$00;
   //Update the checksums
   WriteByte(ByteCheckSum($0000,$100,False,Result),$0FF,Result);
   WriteByte(ByteCheckSum($0100,$100,False,Result),$1FF,Result);
   //Change interleave on 640KB images
   if new1=$A0000 then
   begin
    //Set up the temporary store
    SetLength(buffer,$A0000);
    //Copy the data across - direct read from one array to a interleave write in the other
    for index:=0 to $9FFFF do WriteByte(Result[index],index,buffer);
    //Write over the result with the new temporary store
    for index:=0 to $9FFFF do Result[index]:=buffer[index];
   end;
  end;
  //Side 1 : Partition is AFS
  if(side=1)and(FAFSPresent)then
  begin
   //Setup the buffer to accomodate the AFS partition and the ADFS dummy header
   SetLength(Result,disc_size[1]+$700);//Header + Root = $200+$500
   //Copy the ADFS dummy header across to the top of the buffer
   UpdateProgress('Copying the ADFS header');
   for index:=0 to $6FF do Result[index]:=ReadByte(index);
   //Copy the AFS partition to just after this
   UpdateProgress('Extracting the Acorn File Server partition');
   SetLength(JesMapList,0); //We'll look for objects while we're at it
   JesMap:='';
   for index:=0 to disc_size[1]-1 do
   begin
    //Copy across, byte by byte
    Result[$700+index]:=ReadByte(disc_size[0]+index);
    //Make a note of the character
    JesMap:=JesMap+chr(Result[index]);
    //Make sure it is no longer than 6 characters
    if Length(JesMap)>6 then JesMap:=RightStr(JesMap,6);
    //Have we found an object?
    if JesMap='JesMap' then
    begin
     //Make a note of the location
     SetLength(JesMapList,Length(JesMapList)+1);
     JesMapList[Length(JesMapList)-1]:=index-5;
    end;
   end;
   //Adjust the disc addresses in the ADFS dummy header
   UpdateProgress('Updating the header');
   diff:=disc_size[0]-$700; //Difference between the original and the AFS
   new1:=(afshead-diff)>>8;
   new2:=(afshead2-diff)>>8;
   Result[$0F6]:= new1 AND $FF;
   Result[$0F7]:=(new1 AND $FF00)>>8;
   Result[$0F8]:=(new1 AND $FF0000)>>16;
   Result[$1F6]:= new2 AND $FF;
   Result[$1F7]:=(new2 AND $FF00)>>8;
   Result[$1F8]:=(new2 AND $FF0000)>>16;
   //Adjust the root SIN in the AFS header
   start:=afsroot-(diff>>8);
   Result[(new1<<8)+$1F]:= start AND $FF;
   Result[(new1<<8)+$20]:=(start AND $FF00)>>8;
   Result[(new2<<8)+$1F]:= start AND $FF;
   Result[(new2<<8)+$20]:=(start AND $FF00)>>8;
   //Adjust all the allocated sectors in all the objects from positions $0A-$0E to $FA-$FE
   //We also need to update each directory entry to point to where the object is now.
   UpdateProgress('Updating object addresses');
   if Length(JesMapList)>0 then
    for index:=0 to Length(JesMapList)-1 do
    begin
     //First we'll update the chain of pointers
     new1:=$0A;
     start:=$FFFFFF;
     while(start<>0)and(new1<$FF)do //We repeat until the end or we find a zero
     begin
      //Grab the current pointer
      start:=Result[JesMapList[index]+new1  ]
            +Result[JesMapList[index]+new1+1]<<8
            +Result[JesMapList[index]+new1+2]<<16;
      //If it isn't zero
      if start<>0 then
      begin
       //Then adjust
       start:=start-(diff>>8);
       //And save it back
       Result[JesMapList[index]+new1  ]:= start AND $FF;
       Result[JesMapList[index]+new1+1]:=(start AND $FF00)>>8;
       Result[JesMapList[index]+new1+2]:=(start AND $FF0000)>>16;
      end;
      //Next pointer
      inc(new1,5);
     end;
     //Is this JesMap a directory? Need to adjust all the object pointers
    end;
   //Blank out the ADFS Free Space Map
   for index:=0 to $F5 do
   begin
    Result[index]:=$00;
    Result[index+$100]:=$00;
   end;
   Result[$1FE]:=$00;
   //Update the disc size
   start:=disc_size[1]>>8;
   Result[$0FC]:= start AND $FF;
   Result[$0FD]:=(start AND $FF00)>>8;
   Result[$0FE]:=(start AND $FF0000)>>16;
   //Update the checksums
   WriteByte(ByteCheckSum($0000,$100,False,Result),$0FF,Result);
   WriteByte(ByteCheckSum($0100,$100,False,Result),$1FF,Result);
   //Blank off the last 32 bytes of the 'root'
   for index:=0 to 31 do Result[$6E0+index]:=$00;
   //Write 'Hug' to the last 3 bytes
   Result[$6FD]:=$48;
   Result[$6FE]:=$75;
   Result[$6FF]:=$67;
  end;
{  //Extracting the AFS part - as ADFS, just blank off the ADFS part, making sure
  //that the ADFS root is unreadable.
  //This is the old method - quick and easy.
  if(side=1)and(FAFSPresent)then
  begin
   //Copy the data across
   UpdateProgress('Extracting Partition');
   SetLength(Result,GetDataLength);
   for index:=0 to GetDataLength-1 do Result[index]:=ReadByte(index);
   //Blank the ADFS section (we'll use the WriteByte method to take account of interleave)
   for index:=$200 to (Read24b($FC,Result)<<8)-1 do WriteByte($00,index,Result);
   //Blank the ADFS free space map lengths
   for index:=$100 to $1F5 do Result[index]:=$00;
   //Blank the ADFS free space map starts
   for index:=$001 to $0F5 do Result[index]:=$00;
   Result[$000]:=$20;
   //And the rest of the header
   for index:=$1F9 to $1FE do Result[index]:=$00;
   for index:=$0F9 to $0FE do Result[index]:=$00;
   Result[$0FC]:=$20;
   //Update the checksums
   WriteByte(ByteCheckSum($0000,$100,Result),$0FF,Result);
   WriteByte(ByteCheckSum($0100,$100,Result),$1FF,Result);
  end;}
  //Extracting the DOS part - this is literally extracting the DOS Plus part
  if(side=1)and(FDOSPresent)then
  begin
   //Copy the data across
   UpdateProgress('Extracting Partition');
   //Do we have a DOS header? If not, then make room for it
   if doshead=dosmap then start:=$1000 else start:=$0;
   //Allocate some space
   SetLength(Result,disc_size[1]+start);
   //If no header, then install one
   if doshead=dosmap then //Usually 640KB ADFS hybrid
    WriteDOSHeader(0,disc_size[1]+start,diMaster512,False,Result);
   //Write the DOS partition
   for index:=0 to disc_size[1]-1 do Result[start+index]:=ReadByte(index+doshead);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Get the maximum size for a partition on an ADFS 8 bit image
-------------------------------------------------------------------------------}
function TDiscImage.GetADFSMaxLength(lastentry:Boolean): Cardinal;
var
 index,
 last  : Integer;
 fsst,
 fsed,
 fsptr : Cardinal;
begin
 Result:=0;
 //Only for adding AFS partition to 8 bit ADFS
 if(GetMajorFormatNumber=diAcornADFS)and(not FMap)and(FDirType=diADFSOldDir)then
 begin
  //Is there enough space? Must be contiguous at the end
  fsptr:=ReadByte($1FE); //Pointer to next free space entry
  if fsptr>0 then //There is some free space
  begin
   fsst:=0;
   fsed:=0;
   index:=0;
   last:=0;
   //Find the final entry (which might not necessarily be the last)
   while index<fsptr do
   begin
    if Read24b(index)>fsst then
    begin
     fsst:=Read24b(index);     //Start
     fsed:=Read24b($100+index);//Length
     last:=index;
    end;
    inc(index,3);
   end;
   if not lastentry then Result:=fsed*secsize
   else Result:=last;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Produce a report of the image's details
-------------------------------------------------------------------------------}
function TDiscImage.ADFSReport(CSV: Boolean): TStringList;
var
 temp   : String;
 fsmoff,
 fsm    : TFragmentArray;
 Index,
 Zone   : Integer;
begin
 Result:=TStringList.Create;
 if FMap then
 begin
  temp:='New Map';
  if FDirType=diADFSNewDir then temp:=temp+' New Directory';
  if FDirType=diADFSBigDir then temp:=temp+' Big Directory';
  Result.Add(temp);
  if not CSV then Result.Add('');
  Result.Add('Disc Record');
  if not CSV then Result.Add('-----------');
  Result.Add('Sector Size: '+IntToStr(secsize)+' bytes');
  Result.Add('Sectors per Track: '+IntToStr(secspertrack));
  Result.Add('Heads: '+IntToStr(heads));
  temp:=IntToStr(density);
  case density of
   0: temp:='Hard Drive';
   1: temp:='Single';
   2: temp:='Double';
   3: temp:='Double+';
   4: temp:='Quad';
   8: temp:='Octal';
  end;
  Result.Add('Density: '+temp);
  Result.Add('ID Length: '+IntToStr(idlen)+' bits');
  Result.Add('Bits per Map Byte (LFAU): '+IntToStr(bpmb)+' bytes');
  Result.Add('Skew: '+IntToStr(skew));
  temp:=IntToStr(bootoption[0]);
  case bootoption[0] of
   0: temp:='None';
   1: temp:='Load';
   2: temp:='Run';
   3: temp:='Exec';
  end;
  Result.Add('Boot Option: '+temp);
  Result.Add('Low Sector: '+IntToStr(lowsector));
  Result.Add('Number of Zones: '+IntToStr(nzones));
  Result.Add('Zone Spare Bits: '+IntToStr(zone_spare)+' bits');
  Result.Add('Root Indirect Address: 0x'+IntToHex(rootfrag,8));
  Result.Add('Disc Size: '+IntToStr(disc_size[0])+' bytes');
  Result.Add('Disc ID: 0x'+IntToHex(disc_id,4));
  Result.Add('Disc Name: '+disc_name[0]);
  Result.Add('Disc Type: 0x'+IntToHex(disctype,4));
  if FDirType=diADFSBigDir then
  begin
   Result.Add('Share Size: 0x'+IntToHex(share_size,4));
   Result.Add('Big Flag: '+IntToStr(big_flag));
   Result.Add('Format Version: '+IntToStr(format_vers));
   Result.Add('Root Size: '+IntToStr(root_size)+' bytes');
  end;
  if not CSV then Result.Add('');
  Result.Add('Root Address: 0x'+IntToHex(root,8));
 end
 else
 begin
  temp:='Old Map';
  if FDirType=diADFSNewDir then temp:=temp+' New Directory';
  if FDirType=diADFSOldDir then temp:=temp+' Old Directory';
  Result.Add(temp);
  Result.Add('Disc Size: '+IntToStr(disc_size[0])+' bytes');
  Result.Add('Disc Name: '+disc_name[0]);
  Result.Add('Root Address: 0x'+IntToHex(root<<8,8));
 end;
 Result.Add('Boot Map Location: 0x'+IntToHex(bootmap,8));
 Result.Add('Free Space: '+IntToStr(free_space[0])+' bytes');
 Result.Add('Cylinders: '+IntToStr(Length(free_space_map[0])));
 Result.Add('Broken Directory Count: '+IntToStr(brokendircount));
 if not CSV then Result.Add('');
 Result.Add('Free Space Map');
 if not CSV then Result.Add('==============');
 temp     :='Disc Address';
 if CSV then temp:=temp+',' else temp:=temp+'  ';
 temp:=temp+'Length';
 if FMap then
 begin
  if CSV then temp:=temp+',' else temp:=temp+'        ';
  temp:=temp+'Map Address';
  if CSV then temp:=temp+',' else temp:=temp+'   ';
  temp:=temp+'Length';
  if CSV then temp:=temp+',' else temp:=temp+'        ';
  temp:=temp+'Zone';
  Result.Add(temp);
  if not CSV then Result.Add('--------------------------------------------------------------');
  fsmoff:=ADFSGetFreeFragments(False);
  fsm:=ADFSGetFreeFragments;
  for Zone:=0 to nzones-1 do
   for Index:=0 to Length(fsmoff)-1 do
    if fsm[Index].Zone=Zone then
    begin
     temp     :='0x'+IntToHex(fsm[Index].Offset,10);
     if CSV then temp:=temp+',' else temp:=temp+'  ';
     temp:=temp+'0x'+IntToHex(fsm[Index].Length,10);
     if CSV then temp:=temp+',' else temp:=temp+'  ';
     temp:=temp+'0x'+IntToHex(fsmoff[Index].Offset,10);
     if CSV then temp:=temp+',' else temp:=temp+'  ';
     temp:=temp+'0x'+IntToHex(fsmoff[Index].Length,10);
     if CSV then temp:=temp+',' else temp:=temp+'  ';
     temp:=temp+'0x'+IntToHex(fsmoff[Index].Zone,4);
     Result.Add(temp);
    end;
 end
 else
 begin
  Result.Add(temp);
  if not CSV then Result.Add('--------------------------');
  Index:=ReadByte($1FE); //Number of free space entries
  Zone:=0;
  while Zone<Index do
  begin
   //Fill in the details
   temp     :='0x'+IntToHex(Read24b($000+Zone)*$100,10);
   if CSV then temp:=temp+',' else temp:=temp+'  ';
   temp:=temp+'0x'+IntToHex(Read24b($100+Zone)*$100,10);
   Result.Add(temp);
   inc(Zone,3);
  end;
 end;
end;
