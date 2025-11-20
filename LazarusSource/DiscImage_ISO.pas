//++++++++++++++++++ ISO +++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies an ISO
-------------------------------------------------------------------------------}
function TDiscImage.ID_ISO: Boolean;
var
 vdst  : Cardinal=0;
 pvd   : Cardinal=0;
 Ltype : Byte=0;
 index : Integer=0;
begin
 Result:=False;
 if FFormat=diInvalidImg then
  if GetDataLength>$8800 then //Allow for 32KB OS area at the head
  begin
   ISOFSM;
   //Find the Volume Descriptors
   index:=$10*$800;       //We don't know the block size yet, so assume 0x800
   vdst :=$00;            //Set Terminator
   SetLength(ISOVolDes,0);//Volume Descriptors
   while(index<GetDataLength-6)
     and(index<$100*$800) //We'll continue for 0x90 blocks only
     and((Length(ISOVolDes)=0)or(vdst=$00))do
   begin
    //Look for 'CD001'
    if ReadString(index+1,-5)='CD001'then
    begin
     free_space_map[0,(index div $800)div 32,(index div $800)mod 32]:=diFSMSystem;
     Ltype:=ReadByte(index);
     //Primary and supplimentary Volume Descriptors
     if((Ltype=$01)and(index=$8000)and(pvd=$00))//Primary (only at sector $10)
     or (Ltype=$02)                             //Supplimentary
     or (Ltype=$00)                             //Boot Record
     or (Ltype=$03)then                         //Volume Partition Descriptor
     begin
      SetLength(ISOVolDes,Length(ISOVolDes)+1);
      ISOVolDes[Length(ISOVolDes)-1].VDType:=Ltype;
      ISOVolDes[Length(ISOVolDes)-1].Offset:=index;
      if Ltype=$01 then pvd:=index;             //Make a note of the primary
     end;
     //Volume Descripter Set Terminator
     if Ltype=$FF then vdst:=index;
    end;
    inc(index,$800);
   end;
   Result:=(Length(ISOVolDes)>0)and(vdst<>$00)and(pvd<>$00);
   if Result then FFormat:=diISO<<4 else SetLength(free_space_map,0);
  end;
end;

{-------------------------------------------------------------------------------
Setup the free space map
-------------------------------------------------------------------------------}
procedure TDiscImage.ISOFSM;
var
 c : Integer=0;
 d : Integer=0;
begin
 //Create the free space map
 SetLength(free_space_map,1);
 //Number of tracks (32 blocks)
 SetLength(free_space_map[0],GetDataLength div(32*$800));
 if Length(free_space_map[0])>0 then
 begin
  for c:=0 to Length(free_space_map[0])-1 do
  begin
   SetLength(free_space_map[0,c],32);
   for d:=0 to Length(free_space_map[0,c])-1 do free_space_map[0,c,d]:=diFSMBlank;
  end;
  //Mark out the system area
  for c:=0 to 31 do free_space_map[0,0,c]:=diFSMSystem;
 end;
end;

{-------------------------------------------------------------------------------
Reads the ISO Image into memory
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadISOImage;
var
 Lvdnum  : Cardinal=0;
 pth2use : Cardinal=0;
 index   : Cardinal=0;
begin
 if FFormat>>4=diISO then
 begin
  //Read in all the volume descriptors
  ISOReadVolumeDescriptors;
  //Get the most appropriate volume descriptor and path table to use
  ISOGetVDAndPathToUse(Lvdnum,pth2use);
  //Set the variables
  secsize      :=ISOVolDes[Lvdnum].BlckSize;
  root         :=ISOVolDes[Lvdnum].RootOffset;
  root_size    :=ISOVolDes[Lvdnum].RootLength;
  root_name    :='D:';
  dir_sep      :='/';
  disc_size[0] :=GetDataLength;
  disc_name[0] :=ISOVolDes[Lvdnum].VolumeID;
  FISOFormat   :=diISO;
  //Calculate the free space left
  free_space[0]:=disc_size[0]                  //Total size
                -root_size                     //Size of the root
                -(10*secsize)                 //The initial 32KB
                -(Length(ISOVolDes)*secsize); //All the Volume Descriptors
  //Make allowances for all the path tables
  for index:=0 to Length(ISOVolDes)-1 do
   dec(free_space[0],Length(ISOVolDes[index].PathTbl)*secsize);
  //Adjust the format
  inc(FFormat,ISOVolDes[Lvdnum].Joilet);
  //Mark out the system areas in the FSM
  for index:=0 to 31 do free_space_map[0,0,index]:=diFSMSystem; //Initial 32K
  //Read in the appropriate path table
  ISOReadPathTable(Lvdnum,pth2use);
  //And finally read in the directories
  ISOReadDirectories(Lvdnum);
  dir_sep:='/';
 end;
end;

{-------------------------------------------------------------------------------
Gets the DateTime Stamp from the Volume Descriptor
-------------------------------------------------------------------------------}
function TDiscImage.ISOGetPVDDateTime(offset: Cardinal): TDateTime;
begin
 //Try to encode the date time
 //We're ignoring the timezone field as this usually gets ignored anyway
 if not TryEncodeDateTime(StrToIntDef(chr(ReadByte(offset   ))//Year
                                     +chr(ReadByte(offset+ 1))
                                     +chr(ReadByte(offset+ 2))
                                     +chr(ReadByte(offset+ 3)),1900)
                         ,StrToIntDef(chr(ReadByte(offset+ 4))//Month
                                     +chr(ReadByte(offset+ 5)),01)
                         ,StrToIntDef(chr(ReadByte(offset+ 6))//Day
                                     +chr(ReadByte(offset+ 7)),01)
                         ,StrToIntDef(chr(ReadByte(offset+ 8))//Hour
                                     +chr(ReadByte(offset+ 9)),00)
                         ,StrToIntDef(chr(ReadByte(offset+10))//Minute
                                     +chr(ReadByte(offset+11)),00)
                         ,StrToIntDef(chr(ReadByte(offset+12))//Second
                                     +chr(ReadByte(offset+13)),00)
                         ,StrToIntDef(chr(ReadByte(offset+14))//1/100ths second
                                     +chr(ReadByte(offset+15))+'0',000)
                         ,Result) then
  //Default return value, in case we don't get a valid date time
  Result:=EncodeDateTime(1900,01,01,00,00,00,000);
end;

{-------------------------------------------------------------------------------
Reads the volume descriptors
-------------------------------------------------------------------------------}
procedure TDiscImage.ISOReadVolumeDescriptors;
var
 vdnum : Integer=0;
 c     : Integer=0;
 procedure FillFSMPathTable(path: Cardinal);
 var
  c: Integer=0;
 begin
  if path<>0 then
   for c:=0 to (ISOVolDes[vdnum].PathSize div $800)-1 do
    free_space_map[0,((path div $800)+c)div 32
                    ,((path div $800)+c)mod 32]:=diFSMSystem;
 end;
begin
 if Length(ISOVolDes)>0 then
  for vdnum:=0 to Length(ISOVolDes)-1 do
  begin
   //System ID at $08
   ISOVolDes[vdnum].SystemID:=ReadString(ISOVolDes[vdnum].Offset+$08,-32);
   if(ISOVolDes[vdnum].VDType=$01)or(ISOVolDes[vdnum].VDType=$02)then
   begin
    //Volume ID at $28
    ISOVolDes[vdnum].VolumeID:=ReadString(ISOVolDes[vdnum].Offset+$28,-32);
    //Logical blocks at $50
    ISOVolDes[vdnum].Size:=Read32b(ISOVolDes[vdnum].Offset+$50);
    //Joilet spec at $58:
    if (ReadByte(ISOVolDes[vdnum].Offset+$58)=$25)
    and(ReadByte(ISOVolDes[vdnum].Offset+$59)=$2F)
    and(ReadByte(ISOVolDes[vdnum].Offset+$5A)>>4=$4)then
     ISOVolDes[vdnum].Joilet:=((ReadByte(ISOVolDes[vdnum].Offset+$5A)AND$F)div 2)+1;
    //Volume size at $78
    ISOVolDes[vdnum].NumDiscs:=Read16b(ISOVolDes[vdnum].Offset+$78);
    //Volume number at $7C
    ISOVolDes[vdnum].DiscNum:=Read16b(ISOVolDes[vdnum].Offset+$7C);
    //Block size at $80
    ISOVolDes[vdnum].BlckSize:=Read16b(ISOVolDes[vdnum].Offset+$80);
    //Path size at $84
    ISOVolDes[vdnum].PathSize:=Read32b(ISOVolDes[vdnum].Offset+$84);
    //Path table at $8C
    ISOVolDes[vdnum].PathTbl[0]:=Read32b(ISOVolDes[vdnum].Offset+$8C);
    FillFSMPathTable(ISOVolDes[vdnum].PathTbl[0]);
    //Optional path table at $90
    ISOVolDes[vdnum].oPathTbl[0]:=Read32b(ISOVolDes[vdnum].Offset+$90);
    FillFSMPathTable(ISOVolDes[vdnum].oPathTbl[0]);
    //M-Path table at $94
    ISOVolDes[vdnum].PathTbl[1]:=Read32b(ISOVolDes[vdnum].Offset+$94,True);
    FillFSMPathTable(ISOVolDes[vdnum].PathTbl[1]);
    //Optional m-path table at $98
    ISOVolDes[vdnum].oPathTbl[1]:=Read32b(ISOVolDes[vdnum].Offset+$98,True);
    FillFSMPathTable(ISOVolDes[vdnum].oPathTbl[1]);
    //Root directory entry at $9C
    ISOVolDes[vdnum].RootOffset:=Read32b(ISOVolDes[vdnum].Offset+$9C+$2);
    ISOVolDes[vdnum].RootLength:=Read32b(ISOVolDes[vdnum].Offset+$9C+$A);
    for c:=0 to (ISOVolDes[vdnum].RootLength div $800)-1 do
     free_space_map[0,((ISOVolDes[vdnum].RootOffset div $800)+c)div 32
                     ,((ISOVolDes[vdnum].RootOffset div $800)+c)mod 32]:=diFSMDir;
    //Volume Set Identifier at $BE
    ISOVolDes[vdnum].VolSetID:=ReadString(ISOVolDes[vdnum].Offset+$BE,-128);
    //Publisher Identifier at $13E
    ISOVolDes[vdnum].PublisID:=ReadString(ISOVolDes[vdnum].Offset+$13E,-128);
    //Data Preparer Identifier at $1BE
    ISOVolDes[vdnum].DataPrID:=ReadString(ISOVolDes[vdnum].Offset+$1BE,-128);
    //Application Identifier at $23E
    ISOVolDes[vdnum].AppID:=ReadString(ISOVolDes[vdnum].Offset+$23E,-128);
    //Copyright File Identifier at $2BE
    ISOVolDes[vdnum].CopyID:=ReadString(ISOVolDes[vdnum].Offset+$2BE,-37);
    //Abstract File Identifier at $2E3
    ISOVolDes[vdnum].AbstID:=ReadString(ISOVolDes[vdnum].Offset+$2E3,-37);
    //Bibliographic File Identifier at $308
    ISOVolDes[vdnum].BibliID:=ReadString(ISOVolDes[vdnum].Offset+$308,-37);
    //Volume Creation date at $32D
    ISOVolDes[vdnum].DateCre:=ISOGetPVDDateTime(ISOVolDes[vdnum].Offset+$32D);
    //Volume Modification date at $33E
    ISOVolDes[vdnum].DateMod:=ISOGetPVDDateTime(ISOVolDes[vdnum].Offset+$33E);
    //Volume Expiration date at $34F
    ISOVolDes[vdnum].DateExp:=ISOGetPVDDateTime(ISOVolDes[vdnum].Offset+$34F);
    //Volume Use after date at $360
    ISOVolDes[vdnum].DateUse:=ISOGetPVDDateTime(ISOVolDes[vdnum].Offset+$360);
   end;
  end;
end;

{-------------------------------------------------------------------------------
Works out the best volume descriptor to use
-------------------------------------------------------------------------------}
procedure TDiscImage.ISOGetVDAndPathToUse(var ISOVD2Use: Cardinal;var ISOpath2use: Cardinal);
var
 index : Cardinal=0;
begin
 if Length(ISOVolDes)>0 then
 begin
  //Find the Joilet volume descriptor, or just default to the primary
  ISOVD2Use:=0;
  for index:=0 to Length(ISOVolDes)-1 do
   if ISOVolDes[index].Joilet>0 then ISOVD2Use:=index;
  //Now get the directories from the path table for the chosen volume descriptor
  ISOpath2use:=0; //Path table to use (0 or 1)
  while(ISOVolDes[ISOVD2Use].PathTbl[ISOpath2use]=0)and(ISOpath2use<2)do
   inc(ISOpath2use);
 end
 else
 begin
  ISOVD2Use  :=0;
  ISOpath2use:=0;
 end;
end;

{-------------------------------------------------------------------------------
Gets the DateTime Stamp from the directory entry
-------------------------------------------------------------------------------}
function TDiscImage.ISOGetDirDateTime(offset: Cardinal): TDateTime;
begin
 //Try to encode the date time
 //We're ignoring the timezone field as this usually gets ignored anyway
 if not TryEncodeDateTime(ReadByte(offset  )+1900//Year
                         ,ReadByte(offset+1)     //Month
                         ,ReadByte(offset+2)     //Day
                         ,ReadByte(offset+3)     //Hour
                         ,ReadByte(offset+4)     //Minute
                         ,ReadByte(offset+5)     //Second
                         ,000                    //Millisecond
                         ,Result) then
  //Default return value, in case we don't get a valid date time
  Result:=EncodeDateTime(1900,01,01,00,00,00,000);
end;

{-------------------------------------------------------------------------------
Converts the ISO attributes into a string
-------------------------------------------------------------------------------}
function TDiscImage.ISOAttributes(attr: Cardinal; attributes: String;
                                                    pad: Boolean=False): String;
var
 I: Integer=0;
 b: Cardinal=0;
begin
 Result:='';
 for I:=1 to Length(attributes) do
 begin
  b:=1<<(I-1);
  if(attr AND b)=b then Result:=Result+attributes[I];
 end;
 //Pad it?
 if pad then Result:=LeftStr(Result+StringOfChar(' ',Length(attributes))
                            ,Length(attributes));
end;

{-------------------------------------------------------------------------------
Gets the full path name of the directory reference
-------------------------------------------------------------------------------}
function TDiscImage.ISOGetFullPath(dir: Integer): String;
begin
 Result:=FDisc[dir].Directory;
 while(FDisc[dir].Parent<>-1)and(FDisc[dir].Parent<>dir)do
 begin
  dir:=FDisc[dir].Parent;
  Result:=FDisc[dir].Directory+dir_sep+Result;
 end;
end;

{-------------------------------------------------------------------------------
Updates the directory entry name
-------------------------------------------------------------------------------}
procedure TDiscImage.ISOChangeDirName(dir,entry: Cardinal);
begin
 if FDisc[dir].Entries[entry].DirRef>0 then
  FDisc[FDisc[dir].Entries[entry].DirRef].Directory:=
                                             FDisc[dir].Entries[entry].Filename;
end;

{-------------------------------------------------------------------------------
Reads the full path table
-------------------------------------------------------------------------------}
procedure TDiscImage.ISOReadPathTable(VDNum: Cardinal; pth2use: Cardinal=0);
var
 ptr   : Cardinal=0;
 offset: Cardinal=0;
 index : Cardinal=0;
 nument: Cardinal=0;
 len   : Cardinal=0;
 dir   : Cardinal=0;
begin
 if pth2use<2 then
 begin
  //Update the progress indicator
  UpdateProgress('Reading path table');
  //Pointer into the path table
  ptr   :=0;
  //Offset into the data
  offset:=ISOVolDes[VDNum].PathTbl[pth2use]*secsize;
  while ptr<ISOVolDes[VDNum].PathSize do
  begin
   //Entry number
   index:=Length(FDisc);
   //Make room
   SetLength(FDisc,index+1);
   //Intialise the entry
   ResetDir(FDisc[index]);
   //Length of directory name at offset $00
   len:=ReadByte(offset+ptr);
   //Block (where the directory data is) at offset $02
   FDisc[index].Sector:=Read32b(offset+ptr+2,pth2use=1);
   //Parent at offset $06
   FDisc[index].Parent:=Read16b(offset+ptr+6,pth2use=1)-1;
   //Name at offset $08
   if len>0 then FDisc[index].Directory:=ReadString(offset+ptr+8,-len);
   //Root
   if index=0 then
   begin
    //Length of root is in the volume descriptor
    FDisc[index].Length   :=root_size;
    //Give the root a default name
    FDisc[index].Directory:=root_name;
   end;
   //Not the root, and not a blank directory entry - create an entry in the parent
   if (FDisc[index].Parent<>index)
   and(FDisc[index].Parent<>-1)
   and(FDisc[index].Directory<>'')then
   begin
    //Take a note of the directory reference for the parent
    dir   :=FDisc[index].Parent;
    //Take a note of the last entry reference
    nument:=Length(FDisc[dir].Entries);
    //Make room
    SetLength(FDisc[dir].Entries,nument+1);
    //Initialise the entry
    ResetDirEntry(FDisc[dir].Entries[nument]);
    //Add the entry
    FDisc[dir].Entries[nument].Parent  :=ISOGetFullPath(FDisc[index].Parent);
    FDisc[dir].Entries[nument].Sector  :=FDisc[index].Sector;
    FDisc[dir].Entries[nument].Filename:=FDisc[index].Directory;
    FDisc[dir].Entries[nument].DirRef  :=index;
   end;
   //Blank directory name? Then delete the reference
   if FDisc[index].Directory='' then SetLength(FDisc,Length(FDisc)-1);
   //Move to the next
   inc(ptr,8+len+(len mod 2));
  end;
 end;
end;

{-------------------------------------------------------------------------------
Reads the directories
-------------------------------------------------------------------------------}
procedure TDiscImage.ISOReadDirectories(VDNum: Cardinal);
var
 index  : Integer=0;
 TagPtr : Cardinal=0;
 ptr    : Cardinal=0;
 len    : Byte=0;
 entlen : Byte=0;
 taglen : Byte=0;
 flags  : Byte=0;
 nument : Cardinal=0;
 line   : String='';
 offset : Cardinal=0;
 roattr : Cardinal=0;
 foffset: Cardinal=0;
 c      : Integer=0;
 //Add a new, blank, entry into the Entries list
 function NewEntry(entryname: String;dir: Integer): Cardinal;
 begin
  Result:=Length(FDisc[dir].Entries);
  SetLength(FDisc[dir].Entries,Result+1);
  //Do an insertion sort to keep them in alphabetical order
  while(Result>0)and(entryname<FDisc[dir].Entries[Result-1].Filename)do
  begin
   FDisc[dir].Entries[Result]:=FDisc[dir].Entries[Result-1];
   dec(Result);
  end;
  //Fill in the blanks
  FDisc[dir].Entries[Result].Parent:=ISOGetFullPath(dir);
  FDisc[dir].Entries[Result].DirRef:=-1; //Not a directory
 end;
begin
 //Find all the directory entries
 for index:=0 to Length(FDisc)-1 do
 begin
  //Must be the root, so the parent will be -1
  if FDisc[index].Parent=index then FDisc[index].Parent:=-1;
  //Update the progress indicator
  UpdateProgress('Reading '+FDisc[index].Directory);
  FDisc[index].BeenRead:=True;
  offset:=FDisc[index].Sector*secsize;
  ptr   :=0;
  while ptr<FDisc[index].Length do
  begin
   //Entry size at $00
   entlen:=ReadByte(offset+ptr);
   if entlen<>$0 then
   begin
    //Flags at $19
    flags:=ReadByte(offset+ptr+$19);
    //Length of filename at $20
    len:=ReadByte(offset+ptr+$20);
    //Filename at $21 (padded if even length, none if odd length)
    line:=ReadString(offset+ptr+$21,-len);
    if line<>'' then
    begin
     //Sometimes, filename is terminated with ';<file ID>'
     if line[Length(line)-1]=';' then SetLength(line,Length(line)-2);
     //Is a directory, so find the entry
     if(flags AND $2)=2 then
     begin
      nument:=0;
      //Search for the filename
      while(nument<Length(FDisc[index].Entries))
        and(FDisc[index].Entries[nument].Filename<>line)do inc(nument);
      //Not found? Search for the sector
      if nument>=Length(FDisc[index].Entries) then
      begin
       nument :=0;
       foffset:=Read32b(offset+ptr+$02);
       while(nument<Length(FDisc[index].Entries))
         and(FDisc[index].Entries[nument].Sector<>foffset)do inc(nument);
      end;
      //Still not found? Just create a new entry
      if nument>=Length(FDisc[index].Entries) then nument:=NewEntry(line,index);
     end
     //Not a directory, so add an entry
     else nument:=NewEntry(line,index);
     //Length of data at $0A
     FDisc[index].Entries[nument].Length:=Read32b(offset+ptr+$0A);
     if FDisc[index].Entries[nument].DirRef<>-1 then
      FDisc[FDisc[index].Entries[nument].DirRef].Length:=
                                   FDisc[index].Entries[nument].Length;
     //Adjust the free space counter (whole blocks)
     dec(free_space[0],((FDisc[index].Entries[nument].Length-1) div secsize)*secsize);
     //DateTime Stamp at $12 (this may get overwritten if RISC OS fields are present)
     FDisc[index].Entries[nument].TimeStamp:=
                                            ISOGetDirDateTime(offset+ptr+$12);
     //File Flags at $19 (already read above)
     FDisc[index].Entries[nument].Attributes:=ISOAttributes(flags,'HDA',True);
     //Only change the next entries if not a directory
     if(flags AND $2)=0 then
     begin
      //Location of data at $02
      FDisc[index].Entries[nument].Sector:=Read32b(offset+ptr+$02);
      //Filename at $21 (padded if even length, none if odd length)
      FDisc[index].Entries[nument].Filename:=line; //Already read above
     end;
     //Free Space Map
     for c:=0 to ((FDisc[index].Entries[nument].Length-1)div$800)-1 do
      if flags AND $2=$2 then
       free_space_map[0,(FDisc[index].Entries[nument].Sector+c)div 32
                       ,(FDisc[index].Entries[nument].Sector+c)mod 32]:=diFSMDir
      else
       free_space_map[0,(FDisc[index].Entries[nument].Sector+c)div 32
                       ,(FDisc[index].Entries[nument].Sector+c)mod 32]:=diFSMUsed;
     //Attributes in the OS area ($21+filename length)
     inc(len,(len+1)mod 2);//if len mod 2=0 then inc(len);
     //System Use Entry?
     if len+$21<entlen then
     begin
      //RISC OS
      if ReadString(offset+ptr+$21+len,-10)='ARCHIMEDES' then
      begin
       //Set the sub format
       FISOFormat                             :=diAcornADFS;
       //Read the load and execution addresses
       FDisc[index].Entries[nument].LoadAddr  :=Read32b(offset+ptr+$2B+len);
       FDisc[index].Entries[nument].ExecAddr  :=Read32b(offset+ptr+$2F+len);
       //Calculate the time stamp and filetype, if any
       ADFSCalcFileDate(FDisc[index].Entries[nument]);
       //And the ADFS specific attributes
       roattr                                 :=Read32b(offset+ptr+$33+len);
       FDisc[index].Entries[nument].Attributes:=
                                          FDisc[index].Entries[nument].Attributes
                                         +ISOAttributes(roattr,'RWELrwel');
       //Bit 8 of the RISC OS attributes indicate filename should start with a '!'
       if(roattr AND $100)=$100 then
       begin
        FDisc[index].Entries[nument].Filename[1]:='!';
        //If a directory, we'll need to change the directory name too
        ISOChangeDirName(index,nument);
       end;
      end
      else //See what other tags have been recorded
      begin
       TagPtr:=ptr; //We'll need to make a note of the current pointer
       taglen:=len; //And length
       while taglen+$21+2<=entlen do //Continue until we hit the entry length
       begin
        case ReadString(offset+TagPtr+$21+len,-2) of
         'AS': //Commodore Amiga
          begin
           FISOFormat                             :=diAmiga;
           //Have the attributes been recorded?
           if ReadByte(offset+TagPtr+$21+len+4)AND 1=1 then
            FDisc[index].Entries[nument].Attributes:=
                                        FDisc[index].Entries[nument].Attributes+
                             AmigaIntToStrAttr(Read32b(offset+TagPtr+$21+len+5));
          end;
         'PX': //POSIX attributes - used by Amiga DOS
          begin
           FISOFormat                             :=diAmiga;
           FDisc[index].Entries[nument].Attributes:=
                                          FDisc[index].Entries[nument].Attributes
                  +ISOAttributes(Read32b(offset+TagPtr+$21+len+4),'xia ewr EWR');
          end;
         'NM':; //Alternate Name
         {NM Format : 'Alternate Name'
         NM<length><0x01><flags><alternate name>
         <flags>: bit 0 - CONTINUE (name continues in next NM)
                  bit 1 - CURRENT (refers to current dir)
                  bit 2 - PARENT (refers to parent of current dir)
                  bit 5 - HOST (use host name)}
        end;
        //Move onto the next tag, or end of entry.
        inc(taglen,ReadByte(offset+TagPtr+$21+len+2));
        inc(TagPtr,ReadByte(offset+TagPtr+$21+len+2));
       end;
      end;
     end;
     //Is there a '.' as the last character of the filename?
     if RightStr(FDisc[index].Entries[nument].Filename,1)='.' then
     begin
      SetLength(FDisc[index].Entries[nument].Filename
               ,Length(FDisc[index].Entries[nument].Filename)-1);
      //If a directory, we'll need to change the directory name too
      ISOChangeDirName(index,nument);
     end;
    end;
   end;
   //Next entry
   inc(ptr,entlen);
   while(ReadByte(offset+ptr)=0)and(ptr<FDisc[index].Length)do inc(ptr);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Extracts the given file to buffer, returning a True on success
-------------------------------------------------------------------------------}
function TDiscImage.ExtractISOFile(filename: String; var buffer: TDIByteArray): Boolean;
var
 entry: Cardinal=0;
 dir  : Cardinal=0;
begin
 Result:=False;
 if FileExists(filename,dir,entry) then
 begin
  SetLength(buffer,FDisc[dir].Entries[entry].Length);
  Result:=ReadDiscData(FDisc[dir].Entries[entry].Sector*secsize
                      ,FDisc[dir].Entries[entry].Length
                      ,0
                      ,0
                      ,buffer);
 end;
end;
