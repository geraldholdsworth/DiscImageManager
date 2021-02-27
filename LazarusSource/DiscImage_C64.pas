//++++++++++++++++++ Commodore +++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a Commodore 1541/1571/1581 disc and which type
-------------------------------------------------------------------------------}
function TDiscImage.ID_CDR: Boolean;
var
 BAM,
 hdr,
 i   : Cardinal;
 ctr : Byte;
begin
 Result:=False;
 if FFormat=$FF then
 begin
  ResetVariables;
  //Is there actually any data?
  if GetDataLength>0 then
  begin
   //IDing a 1541/1571
   ctr:=0;
   //BAM is at track 18 sector 0
   BAM:=ConvertDxxTS(0,18,0); //Get the BAM address - track 18 sector 0
   //BAM offset 0x02 should be 0x41 or 0x00
   if (ReadByte(BAM+$02)=$41)
   or (ReadByte(BAM+$02)=$00) then
    inc(ctr);
   //BAM offset 0xA0, 0xA1, 0xA4, and 0xA7-0xAA should be 0xA0
   if  (ReadByte(BAM+$A0)=$A0)
   and (ReadByte(BAM+$A1)=$A0)
   and (ReadByte(BAM+$A4)=$A0) then
    inc(ctr,3);
   for i:=$A7 to $AA do
    if ReadByte(BAM+i)=$A0 then
     inc(ctr);
   //BAM offset 0xA5 should be 0x32 and 0xA6 should be 0x41 ("2A")
   if  (ReadByte(BAM+$A5)=$32)
   and (ReadByte(BAM+$A6)=$41) then
    inc(ctr,2);
   //Succesful checks
   //BAM offset 0x03 will be 0x00 for 1541 and 0x80 for 1571
   if (ctr=10) and (ReadByte(BAM+$03)=$00) then FFormat:=$20; //Single sided : 1541
   if (ctr=10) and (ReadByte(BAM+$03)=$80) then FFormat:=$21; //Double sided : 1571
   //BAM is also at track 53 sector 0, for a double sided disc
   //IDing a 1581
   if FFormat=$FF then //Don't need to ID a 1581 if we already have a 1541/1571
   begin
    ctr:=0;
    //header is at track 40 sector 0
    hdr:=ConvertDxxTS(2,40,0);
    //header offset 0x02 should be 0x44
    if ReadByte(hdr+$02)=$44 then inc(ctr);
    //header offset 0x03 should be 0x00
    if ReadByte(hdr+$03)=$00 then inc(ctr);
    //header offset 0x14, 0x15, 0x18, 0x1B, 0x1C should be 0xA0
    if  (Read16b(hdr+$14)=$A0A0)
    and (ReadByte(hdr+$18)=$A0) and (Read16b(hdr+$1B)=$A0A0) then
     inc(ctr,5);
    //header offset 0x19 should 0x33 and 0x1A should be 0x44 ("3D")
    if  Read16b(hdr+$19)=$4433 then
     inc(ctr,2);
    //BAM, side 0, is at track 40 sector 1
    BAM:=ConvertDxxTS(2,40,1);
    //BAM offset 0x00, 0x01 should be 0x28 & 2
    if Read16b(BAM+$00)=$0228 then
     inc(ctr,2);
    //BAM offset 0x02 should be 0x44
    if ReadByte(BAM+$02)=$44 then
     inc(ctr);
    //BAM offset 0x04 & 0x05 should be the same is header offset 0x16 & 0x17
    if  (ReadByte(BAM+$04)=ReadByte(hdr+$16))
    and (ReadByte(BAM+$05)=ReadByte(hdr+$17)) then
     inc(ctr,2);
    //BAM, side 2, is at track 40 sector 2
    BAM:=ConvertDxxTS(2,40,2);
    //as above, except
    //BAM offset 0x00, 0x01 should be 0 & 0xFF
    if Read16b(BAM+$00)=$FF00 then
     inc(ctr,2);
    //Successful checks
    if ctr=16 then FFormat:=$22; //1581
   end;
   FDSD  :=(FFormat>$20)and(FFormat<$2F); //Set/reset the DoubleSided flag
   Result:=FFormat shr 4=2;               //Return TRUE if succesful ID
   If Result then FMap:=False;            //and reset the NewMap flag
  end;
 end;
end;

{-------------------------------------------------------------------------------
Converts a track and sector address into a file offset address (Commodore)
-------------------------------------------------------------------------------}
function TDiscImage.ConvertDxxTS(format,track,sector: Integer): Integer;
var
 x,c: Integer;
begin
 Result:=0;
 c:=0;
 //1541 has only 36 tracks
 if (format=0) AND (track>40) then track:=-1;
 //So if it is 36-40, compensate
 if (format=0) AND (track>35) then
 begin
  c:=track-35;
  track:=35;
 end;
 //1571 has only 70 tracks
 if (format=1) AND (track>70) then track:=-1;
 case format of
  0,1: //1541 & 1571
   if track<CDRhightrack[0] then
   begin
    //Start at the end
    x:=7;
    while track>CDRhightrack[x] do
    begin
     //Increase the tally by the number of sectors
     inc(Result,(CDRhightrack[x]-CDRhightrack[x+1])*CDRnumsects[x]);
     //Move to next entry
     dec(x);
    end;
    //Then add on the number of tracks * sectors
    inc(Result,(track+c-CDRhightrack[x+1])*CDRnumsects[x]);
   end;
  2: Result:=(track-1)*40; //1581
 end;
 //Add on the sectors
 inc(Result,sector);
 //Multiply by the bytes per sector
 Result:=Result*$100;
 //If the track is invalid, return an invalid number
 if track=-1 then Result:=$FFFFF;
end;

{-------------------------------------------------------------------------------
Read Commodore Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadCDRDisc: TDisc;
var
 ptr,t,s,amt,
 file_chain,
 file_ptr,p,
 ch,c,f,dirTr :Integer;
 temp         : String;
begin
 Result:=nil;
 SetLength(Result,1);
 ResetDir(Result[0]);
 //Get the format
 f:=FFormat AND $F; //'f' is the format - 0: D64, 1: D71, 2: D81
 dirTr:=18; //D64 and D71 disc info is on track 18, sector 0
 if f=2 then dirTr:=40; //D81 disc info is on track 40, sector 0
 //Read the Header
 ptr:=ConvertDxxTS(f,dirTr,0); //Get the offset address of the header
 //Get the disc title
 temp:='';
 if f=2 then c:=$04 else c:=$90; //Location of disc title
 for ch:=0 to 15 do
 begin
  p:=ReadByte(ptr+c+ch);
  if (p>32) and (p<>$A0) then temp:=temp+chr(p AND $7F);
 end;
 RemoveControl(temp);
 disc_name:=temp;
 //Size of the disc
 if f=2 then
  disc_size:=ConvertDxxTS(f,80,40)
 else
 begin
  disc_size:=ConvertDxxTS(f,35,17);
  if FDSD then disc_size:=disc_size*2;
 end;
 //Get the location of the directory
 t:=ReadByte(ptr+0);
 s:=ReadByte(ptr+1);
 //Calculate the free space and map
 CDRFreeSpaceMap;
 //Calculate where the first directory is
 ptr:=ConvertDxxTS(f,t,s);
 amt:=0;
 //Set the root directory name
 Result[0].Directory:=root_name;
 repeat
  //Track/Sector for next link or 00/FF for end
  t:=ReadByte(ptr);
  s:=ReadByte(ptr+1);
  for c:=0 to 7 do
   if ReadByte(ptr+(c*$20)+2)>$00 then
   begin
    SetLength(Result[0].Entries,amt+1);
    ResetDirEntry(Result[0].Entries[amt]);
    Result[0].Entries[amt].Parent:=root_name;
    //First track/sector of Fdata
    Result[0].Entries[amt].Track :=ReadByte(ptr+(c*$20)+3);
    Result[0].Entries[amt].Sector:=ReadByte(ptr+(c*$20)+4);
    //Filetype
    Result[0].Entries[amt].ShortFiletype:=
                        LeftStr(CDRFileTypes[ReadByte(ptr+(c*$20)+2) AND $0F],3);
    Result[0].Entries[amt].Filetype:=
                        Copy(CDRFileTypes[ReadByte(ptr+(c*$20)+2) AND $0F],4);
    //Attributes
    if (ReadByte(ptr+(c*$20)+2) AND $40)=$40 then //Locked
     Result[0].Entries[amt].Attributes:=Result[0].Entries[amt].Attributes+'L';
    if (ReadByte(ptr+(c*$20)+2) AND $80)=$80 then // Closed
     Result[0].Entries[amt].Attributes:=Result[0].Entries[amt].Attributes+'C';
    //Length of file - in sectors
    Result[0].Entries[amt].Length:=Read16b(ptr+(c*$20)+$1E);
    //now follow the chain to find the exact file length}
    file_ptr:=ConvertDxxTS(f,
      Result[0].Entries[amt].Track,Result[0].Entries[amt].Sector); //first sector
    //Now read the rest of the chain
    for file_chain:=1 to Result[0].Entries[amt].Length-1 do
     file_ptr:=ConvertDxxTS(f,ReadByte(file_ptr),ReadByte(file_ptr+1));
    //and get the partial usage of final sector
    if ReadByte(file_ptr)=$00 then
     Result[0].Entries[amt].Length:=
                 ((Result[0].Entries[amt].Length-1)*254)+ReadByte(file_ptr+1)-1;
    //Filename
    temp:='';
    for ch:=0 to 15 do
    begin
     p:=ReadByte(ptr+(c*$20)+5+ch);
     if (p>32) and (p<>$A0) then temp:=temp+chr(p AND $7F);
    end;
    Result[0].Entries[amt].Filename:=temp;
    //Not a directory - not used by D64/D71/D81
    Result[0].Entries[amt].DirRef:=-1;
    inc(amt);
   end;
  //If not end of directory, go to next block
  if (t<>$00) and (s<>$FF) then ptr:=ConvertDxxTS(f,t,s);
 until (t=$00) and (s=$FF);
end;

{-------------------------------------------------------------------------------
Create a new, blank, disc
-------------------------------------------------------------------------------}
function TDiscImage.FormatCDR(minor: Byte): TDisc;
var
 t,i    : Integer;
begin
 //Blank everything
 ResetVariables;
 //Set the format
 FFormat:=$20+minor;
 //Set the filename
 imagefilename:='Untitled.'+FormatExt;
 //Setup the data area
 case minor of
  0 : SetDataLength(175531);  //1541
  1 : SetDataLength(351062);  //1571
  2 : SetDataLength(822400);  //1581
 end;
 //Fill with zeros
 for t:=0 to GetDataLength-1 do WriteByte(0,t);
 if minor<2 then //1541 and 1571
 begin
  //Location of root
  WriteByte($12,$16500);
  WriteByte($01,$16501);
  //Disc DOS version byte
  WriteByte($41,$16502);
  //Sides
  WriteByte($80*minor,$16503);
  //BAM Entries
  i:=Length(CDRhightrack)-2;
  for t:=1 to 35 do
  begin
   if t=CDRhightrack[i] then dec(i);
   if t<>18 then
   begin
    //Sectors free
    WriteByte(CDRnumsects[i],$16500+(t*4));
    //Free areas
    WriteByte($FF,$16501+(t*4));
   end;
   if t=18 then //Track 18 - BAM location
   begin
    //Sectors free
    WriteByte(CDRnumsects[i]-2,$16500+(t*4));
    //Free areas
    WriteByte($FC,$16501+(t*4));
   end;
   WriteByte($FF,$16502+(t*4));
   WriteByte((1 shl (CDRnumsects[i]-16))-1,$16503+(t*4));
  end;
  //Disc Name
  for t:=0 to 15 do WriteByte($A0,$16590+t);
  //Reserved
  Write16b($A0A0,$165A0);
  //Disc ID
  Write16b($3030,$165A2);
  //Reserved
  WriteByte($A0,$165A4);
  //DOS Type '2A'
  Write16b($4132,$165A5);
  //Reserved
  Write32b($A0A0A0A0,$165A7);
  //First directory entry
  WriteByte($FF,$16601);
  FDSD:=False;
  if minor=1 then //1571
  begin
   FDSD:=True;
   //BAM Entries
   i:=Length(CDRhightrack)-2;
   while CDRhightrack[i]<>36 do dec(i);
   for t:=36 to 70 do
   begin
    if t=CDRhightrack[i] then dec(i);
    if t<>53 then
    begin
     //Sectors free
     WriteByte(CDRnumsects[i],$165DD+(t-36));
     //Free areas
     WriteByte($FF,$41000+((t-36)*3));
     WriteByte($FF,$41001+((t-36)*3));
     WriteByte((1 shl (CDRnumsects[i]-16))-1,$41002+((t-36)*3));
    end;
   end;
  end;
 end;
 if minor=2 then //1581
 begin
  //Write the header
  WriteByte($28,$61800); //Track for first directory entry
  WriteByte($03,$61801); //Sector for first directory entry
  WriteByte($44,$61802); //Disc DOS Version number
  //Disc name
  for t:=0 to 15 do WriteByte($A0,$61804+t);
  Write16b($A0A0,$61814); //Reserved
  Write16b($2020,$61816); //Disc ID
  WriteByte($A0,$61818); //Reserved
  WriteByte($33,$61819); //DOS version
  WriteByte($44,$6181A); //Disc version
  Write16b($A0A0,$6181B); //Reserved
  //BAM side 1
  WriteByte($28,$61900); //Track for next BAM
  WriteByte($02,$61901); //Sector for next BAM
  WriteByte($44,$61902); //Version number
  WriteByte($BB,$61903); //1s complement of version number
  Write16b($2020,$61904);//Disc ID bytes
  WriteByte($C0,$61906); //I/O Byte
  //BAM entries, tracks 1 to 39
  for t:=0 to 38 do
  begin
   WriteByte($28,$61910+(t*6)); //Number of free sectors on track
   Write32b($FFFFFFFF,$61911+(t*6)); //Free sectors
   WriteByte($FF,$61915+(t*6)); //Free sectors
  end;
  //BAM entries, track 40
  Write32b($FFFFF024,$619FA);
  Write16b($FFFF,$619FE);
  //BAM side 2
  WriteByte($00,$61A00); //Track for next BAM
  WriteByte($FF,$61A01); //Sector for next BAM
  WriteByte($44,$61A02); //Version number
  WriteByte($BB,$61A03); //1s complement of version number
  Write16b($2020,$61A04);//Disc ID bytes
  WriteByte($C0,$61A06); //I/O Byte
  //BAM entries, tracks 41 to 80
  for t:=0 to 39 do
  begin
   WriteByte($28,$61A10+(t*6)); //Number of free sectors on track
   Write32b($FFFFFFFF,$61A11+(t*6)); //Free sectors
   WriteByte($FF,$61A15+(t*6)); //Free sectors
  end;
  //First directory entry
  WriteByte($FF,$61B01);
 end;
 Result:=ReadCDRDisc;
end;

{-------------------------------------------------------------------------------
Calculate the free space map
-------------------------------------------------------------------------------}
procedure TDiscImage.CDRFreeSpaceMap;
var
 c,ch,f,sec,x,
 dirTr,dirTr1  : Byte;
 ptr,ptr1,s    : Cardinal;
begin
 //Get the format
 f:=FFormat AND $F; //'f' is the format - 0: D64, 1: D71, 2: D81
 dirTr:=18; //D64 and D71 disc info is on track 18, sector 0
 if f=2 then dirTr:=40; //D81 disc info is on track 40, sector 0
 //BAM for side 1 (D71 only)
 dirTr1:=dirTr;
 if f=1 then dirTr1:=53;
 //Read the Header
 ptr :=ConvertDxxTS(f,dirTr ,0); //Get the offset address of the header
 ptr1:=ConvertDxxTS(f,dirTr1,0); //and the BAM for side 1 (D71)
 //Set up the variables
 free_space:=0;
 SetLength(free_space_map,1);
 if f=0 then SetLength(free_space_map[0],35); //35 tracks for D64
 if f=1 then SetLength(free_space_map[0],70); //70 tracks for D71
 if f=2 then SetLength(free_space_map[0],80); //80 tracks for D81
 sec:=Length(CDRhightrack)-2;
 for c:=0 to Length(free_space_map[0])-1 do
 begin
  //D64 and D71 have differing number of sectors per track
  if f<2 then
  begin
   if c+1=CDRhightrack[sec] then dec(sec);
   SetLength(free_space_map[0,c],CDRnumsects[sec]);
  end //But 1581 have 40 sectors per track on every track
  else SetLength(free_space_map[0,c],40);
  //Set it as used (or system)
  for ch:=0 to Length(free_space_map[0,c])-1 do
   if ((c=dirTr-1)  AND (f<2))
   OR ((c=dirTr1-1) AND (f=1))
   OR ((c=dirTr-1)  AND (f=2)) then
    free_space_map[0,c,ch]:=$FE  //Specify as system
   else
    free_space_map[0,c,ch]:=$FF; //Specify as files
 end;
 //Calculate the free space (D64/D71)
 if f<2 then
 begin
  //Free space, side 0
  for c:=1 to 35 do //35 tracks
  begin
   //First byte is number of free sectors
   if c<>dirTr then //But we'll assume that the directory track is used
    inc(free_space,ReadByte(ptr+c*4)*$100);
   for ch:=0 to 23 do
   begin
    //Next 4 are the free sectors - 1 bit per sector
    s:=ReadByte(ptr+(1+(ch DIV 8))+(c*4));
    x:=1 shl(ch MOD 8);
    if (s AND x=x) AND (ch<Length(free_space_map[0,c-1])) then
     free_space_map[0,c-1,ch]:=$00;
   end;
  end;
  //Free space, side 1 (D71 - D64 will be zeros anyway)
  if f=1 then
   for c:=0 to 34 do //another 35 tracks
   begin 
    //First byte is number of free sectors
    if c+36<>dirTr1 then //But we'll assume that the directory track is used
     inc(free_space,ReadByte(ptr+$DD+c)*$100);
    for ch:=0 to 23 do
    begin
     //Next 4 are the free sectors - 1 bit per sector
     s:=ReadByte(ptr1+(ch DIV 8)+(c*3));
     x:=1 shl(ch MOD 8);
     if (s AND 1 shl ch=1 shl ch) AND (ch<Length(free_space_map[0,c+35])) then
      free_space_map[0,c+35,ch]:=$00;
    end;
   end;
 end;
 //Calculate the free space (D81)
 if f=2 then
  for ch:=1 to 2 do //Sector (0 is header, 1 is BAM side 0, 2 is BAM side 1)
  begin
   //Get the offset address for the sector
   ptr:=ConvertDxxTS(f,dirTr,ch);
   for c:=0 to 39 do //40 tracks
   begin
    //First byte is number of free sectors
    if c<>dirTr then //But we'll assume that the directory track is used
     inc(free_space,ReadByte(ptr+$10+c*6)*$100);
    for sec:=0 to 39 do //40 sectors per track
    begin
     //Next 5 are the free sectors - 1 bit per sector
     s:=ReadByte(ptr+($11+(sec DIV 8))+(c*6));
     x:=1 shl(sec MOD 8);
     if(s AND x=x) then
      free_space_map[0,c+(ch-1)*40,sec]:=$00;
    end;
   end;
  end;
end;

{-------------------------------------------------------------------------------
Set or clear the BAM (i.e. mark as used or not used)
-------------------------------------------------------------------------------}
procedure TDiscImage.CDRSetClearBAM(track,sector: Byte;used: Boolean);
var
 f,i,j,
 fr    : Byte;
 ptr,
 ptr1  : Cardinal;
begin
 f:=FFormat mod $10;
 //Pointer to BAM number of free sectors:
 //ptr will be the number of free sectors
 //ptr1 will be the allocation bits for the track
 if (f=0) or ((f=1)and(track<37))then
 begin //1541 or 1571 side 0
  ptr:=ConvertDxxTS(f,18,0)+track*4;
  ptr1:=ptr+1;
 end;
 if (f=1)and(track>36) then
 begin //1571 side 1
  ptr:=ConvertDxxTS(f,18,0)+$DD+track;
  ptr1:=ConvertDxxTS(f,53,0)+(track-37)*4;
 end;
 if (f=2)and(track<41) then
 begin //1581 side 0
  ptr:=ConvertDxxTS(f,40,1)+(track-1)*6;
  ptr1:=ptr+1;
 end;
 if (f=2)and(track>40) then
 begin //1581 side 1
  ptr:=ConvertDxxTS(f,40,2)+(track-41)*6;
  ptr1:=ptr+1;
 end;
 //Which bit needs to be clear?
 i:=ReadByte(ptr1+(sector DIV 8));     //Read the byte
 if used then
  i:=i AND($FF XOR(1 shl(sector MOD 8))) //Clear that bit
 else
  i:=i OR(1 shl(sector MOD 8)); //Set that bit
 WriteByte(i,ptr1+(sector DIV 8));     //Write it back
 //Number of bytes used to store the BAM
 if f<2 then j:=3 else j:=5;
 //Count the number of free sectors
 fr:=0;
 for i:=0 to j-1 do
  for f:=0 to 7 do
   if IsBitSet(ReadByte(ptr1+i),f) then inc(fr);
 //And store the result
 WriteByte(fr,ptr);
end;

{-------------------------------------------------------------------------------
Update the disc title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateCDRDiscTitle(title: String): Boolean;
var
 ptr: Cardinal;
 i: Byte;
begin
 disc_name:=title;
 //Get the location of the disc title, less one
 if FFormat mod $10<2 then ptr:=ConvertDxxTS(FFormat mod $10,18,0)+$8F;
 if FFormat mod $10=2 then ptr:=ConvertDxxTS(FFormat mod $10,40,0)+$03;
 //Fill the 16 bytes
 for i:=1 to 16 do
 begin
  //If shorter than 16 characters, pad with 0xA0
  if i>Length(title) then WriteByte($A0,ptr+i)
  //Otherwise write the character
  else WriteByte(ord(title[i]),ptr+i);
 end;
 //Return a succesful result
 Result:=True;
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path
-------------------------------------------------------------------------------}
function TDiscImage.ExtractCDRFile(filename: String;
                                             var buffer: TDIByteArray): Boolean;
var
 source        : Integer;
 entry,dir,
 dest,
 fragptr,len,
 filelen       : Cardinal;
begin
 Result:=False;
 if FileExists(filename,fragptr) then //Does the file actually exist?
 //Yes, so load it - there is nothing to stop a directory header being extracted
 //if passed in the filename parameter.
 begin
  //FileExists returns a pointer to the file
  entry:=fragptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=fragptr div $10000;  //Top 16 bits - directory reference
  //Make space to receive the file
  filelen:=FDisc[dir].Entries[entry].Length;
  if filelen>0 then //Make sure there is something to read
  begin
   SetLength(buffer,filelen);
   //Get the starting position
   fragptr:=ConvertDxxTS(FFormat AND $F,
                         FDisc[dir].Entries[entry].Track,
                         FDisc[dir].Entries[entry].Sector);
   dest  :=0;      //Length pointer/Destination pointer
   repeat
    //Fragmented filing systems, so need to work out source and length
    source:=fragptr+2;                        //Source of data
    len   :=254;                              //Amount of data
    //Make sure we don't read too much
    if dest+len>filelen then
     len:=filelen-dest;
    //Read the data into the buffer
    ReadDiscData(source,len,FDisc[dir].Entries[entry].Side,buffer[dest]);
    //Move the size pointer on, by the amount read
    inc(dest,len);
    //Get the next block pointer
    fragptr:=ConvertDxxTS(FFormat AND $F,
                          ReadByte(fragptr),
                          ReadByte(fragptr+1));
   until dest>=filelen; //Once we've reached the file length, we're done
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Write a file to Commodore image
-------------------------------------------------------------------------------}
function TDiscImage.WriteCDRFile(file_details: TDirEntry;
                             var buffer: TDIByteArray): Integer;
var
 count,
 ptr       : Cardinal;
 success   : Boolean;
 f,frag,i,
 track,
 sector    : Byte;
 block     : TDIByteArray;
 fragments : TFragmentArray;
begin
 Result:=-8;//Nothing to write
 count:=file_details.Length;
 if count>0 then //Make sure that there is something to write
 begin
  f:=FFormat MOD $10; //Minor format (sub format)
  //Overwrite the parent
  file_details.Parent:=root_name;
  //Check that the filename is valid
  file_details.Filename:=ValidateDFSFilename(file_details.Filename);
  Result:=-4;//Catalogue full
  //Is there enough free directory entries for another file?
  if ((f<2) and (Length(FDisc[0].Entries)<144))
  or ((f=2) and (Length(FDisc[0].Entries)<296)) then
  begin
   Result:=-3;//File already exists
   //Make sure the file does not already exist
   if not(FileExists(file_details.Parent+dir_sep+file_details.Filename,ptr))then
   begin
    Result:=-2;//Disc full
    //How many fragments to split the file into
    frag:=count div 254;
    if count mod 254>0 then inc(frag);
    //Split the file
    SetLength(fragments,frag);
    for i:=0 to frag-1 do
     fragments[i].Length:=254;
    if count mod 254>0 then fragments[frag-1].Length:=count mod 254;
    //Where to put them - fragments are tended to be put around the root(s).
    //So we search backwards, then forwards, then backwards, etc.
    if count<free_space then //Will it actually fit?
    begin
     track:=18; //Value of this is unimportant, but needs to be set to something
     sector:=0; //Sector to start looking
     i:=0;
     while (CDRFindNextTrack(track,sector)) AND (i<frag) do
     begin
      //Make a note and move on
      fragments[i].Offset:=track*$100+sector;
      //Mark as used
      free_space_map[0,track-1,sector]:=$FF;
      inc(i);
     end;
     //Now we have our locations, write the data
     //Set the length of our block
     SetLength(block,$100);
     //Set our succeed flag
     success:=True;
     if Length(fragments)>1 then //Only if we have more than one fragment
      for frag:=0 to Length(fragments)-2 do
      begin
       //Put the next t/s link into the block
       block[0]:=fragments[frag+1].Offset DIV $100; //Track
       block[1]:=fragments[frag+1].Offset MOD $100; //Sector
       //And copy the data across
       for i:=0 to 253 do
        block[i+2]:=buffer[(frag*254)+i];
       //Now write it onto the disc
       success:=success AND WriteDiscData(ConvertDxxTS(f,
                                                       fragments[frag].Offset DIV $100,
                                                       fragments[frag].Offset MOD $100)
                                         ,0,block,$100);
      end;
     frag:=Length(fragments)-1;//frag should already be set to this
     //Set up the final block
     SetLength(block,fragments[frag].Length+2);
     //Put the length details in
     block[0]:=$00;//Last entry
     block[1]:=fragments[frag].Length;//Size of this entry
     //As before, copy the data across
     for i:=0 to fragments[frag].Length-1 do
      block[i+2]:=buffer[(frag*254)+i];
     //Now write it onto the disc
     success:=success AND WriteDiscData(ConvertDxxTS(f,
                                                      fragments[frag].Offset DIV $100,
                                                      fragments[frag].Offset MOD $100)
                                        ,0,block,fragments[frag].Length);
     Result:=-5;//Unknown error
     if success then
     begin
      //Update the BAM
      for frag:=0 to Length(fragments)-1 do
      begin
       track :=fragments[frag].Offset DIV$100;
       sector:=fragments[frag].Offset MOD$100;
       CDRSetClearBAM(track,sector,True);
      end;
      //Update the directory entries
      SetLength(FDisc[0].Entries,Length(FDisc[0].Entries)+1);
      i:=Length(FDisc[0].Entries)-1;
      //Copy the details across
      FDisc[0].Entries[i]:=file_details;
      //Update the first t/s link
      FDisc[0].Entries[i].Track        :=fragments[0].Offset DIV$100;
      FDisc[0].Entries[i].Sector       :=fragments[0].Offset MOD$100;
      //Make sure it has some attributes
      if FDisc[0].Entries[i].Attributes='' then
       FDisc[0].Entries[i].Attributes   :='C';
      //Filetype - has one been supplied?
      FDisc[0].Entries[i].Filetype:='';//We're going to overwrite this anyway
      if FDisc[0].Entries[i].ShortFileType<>'' then
      //Try and match it up with the known ones
       for frag:=0 to Length(CDRFileTypes)-1 do
        if LeftStr(CDRFileTypes[frag],3)=UpperCase(FDisc[0].Entries[i].ShortFileType) then
        begin
         FDisc[0].Entries[i].ShortFileType:=LeftStr(CDRFileTypes[frag],3);
         FDisc[0].Entries[i].Filetype     :=Copy(CDRFileTypes[frag],4);
        end;
      //Nothing was supplied, or nothing got matched, so revert to default (PRG)
      if FDisc[0].Entries[i].FileType='' then
      begin
       FDisc[0].Entries[i].ShortFileType:=LeftStr(CDRFileTypes[2],3);
       FDisc[0].Entries[i].Filetype     :=Copy(CDRFileTypes[2],4);
      end;
      //Update the catalogue
      UpdateCDRCat;
      //Update the free space map
      CDRFreeSpaceMap;
      //Return the index to the calling function
      Result:=i;
     end;
    end;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update the catalogue
-------------------------------------------------------------------------------}
procedure TDiscImage.UpdateCDRCat;
var
 track,
 sector,
 dirsector,
 maxsector,
 i,j,k,
 attr      : Byte;
 c,
 ptr       : Cardinal;
 sectors   : TDIByteArray;
 temp      : String;
begin
 //Track and sector number for the directory
 track:=18;     //Track where the system is
 sector:=1;     //Sector where the first directory is
 maxsector:=19; //Maximum sectors on this track
 //1581 track and sector number
 if FFormat mod $10=2 then
 begin
  track:=40;
  sector:=3;
  maxsector:=40;
 end;
 SetLength(sectors,maxsector+1); //Pointer to directories
 for i:=0 to maxsector do sectors[i]:=$FF;
 //Clear the FSM for the system track
 for i:=sector to maxsector do
 begin
  free_space_map[0,track-1,i]:=$00;
  CDRSetClearBAM(track,i,False);
 end;
 //Iterate through the entries
 k:=0;
 if Length(FDisc[0].Entries)>0 then k:=Length(FDisc[0].Entries)-1;
 for i:=0 to k do
 begin
  //Pointer to this directory entry
  //Directories are all on the same track, interleaved by 3 sectors (1,4,7,etc)
  if i MOD 8=0 then
  begin
   dirsector:=(((i DIV 8)*3)MOD(maxsector-1))+sector;
   //Make sure we haven't used it already
   while free_space_map[0,track-1,dirsector]<>$00 do
   begin
    inc(dirsector); //so move to the next
    dirsector:=dirsector MOD maxsector;//Make sure we don't go over the end
   end;
   //Claim this sector
   free_space_map[0,track-1,dirsector]:=$FE;
   sectors[i DIV 8]:=dirsector;
  end;
  ptr:=ConvertDxxTS(FFormat mod $10,track,dirsector)+(i MOD 8)*$20;
  //t/s link to next directory - they are all 00/00 except for the first
  WriteByte($00,ptr  ); //Track
  WriteByte($00,ptr+1); //Sector
  //File type and attributes
  attr:=0;
  if Length(FDisc[0].Entries)>0 then
  begin
   for j:=0 to Length(CDRFileTypes)-1 do
    if LeftStr(CDRFileTypes[j],3)=FDisc[0].Entries[i].ShortFileType then
     attr:=j;
   if Pos('L',FDisc[0].Entries[i].Attributes)>0 then attr:=attr OR $40;
   if Pos('C',FDisc[0].Entries[i].Attributes)>0 then attr:=attr OR $80;
  end;
  WriteByte(attr,ptr+2);
  //t/s link to first fragment
  if Length(FDisc[0].Entries)>0 then
  begin
   WriteByte(FDisc[0].Entries[i].Track,ptr+3);
   WriteByte(FDisc[0].Entries[i].Sector,ptr+4);
  end
  else
  begin
   WriteByte($00,ptr+3);
   WriteByte($00,ptr+4);
  end;
  //Filename
  temp:='';
  if Length(FDisc[0].Entries)>0 then temp:=FDisc[0].Entries[i].Filename;
  for j:=1 to 16 do
  begin
   if Length(FDisc[0].Entries)>0 then c:=$A0 else c:=$00;
   if j<=Length(temp) then c:=ord(temp[j]);
   WriteByte(c,ptr+4+j);
  end;
  //t/s location of side-sector block (REL files) - currently not supported
  WriteByte($00,ptr+$15); //Track
  WriteByte($00,ptr+$16); //Sector
  //REL file length record - currently not supported
  WriteByte($00,ptr+$17);
  //Unused bytes
  for j:=0 to 5 do
   WriteByte($00,ptr+$18+j);
  //Approx file size in sectors
  c:=0;
  if Length(FDisc[0].Entries)>0 then
  begin
   c:=FDisc[0].Entries[i].Length DIV 254;
   if FDisc[0].Entries[i].Length MOD 254>0 then inc(c);
  end;
  Write16b(c,ptr+$1E);
 end;
 //Update the t/s links on the first entry of each sector used
 j:=0;
 if Length(FDisc[0].Entries)>0 then j:=(Length(FDisc[0].Entries)-1)DIV$8;
 for i:=0 to j do
 begin
  //Pointer to this directory
  ptr:=ConvertDxxTS(FFormat mod $10,track,sectors[i]);
  if i=j then WriteByte($00,ptr) else WriteByte(track,ptr);
  WriteByte(sectors[i+1],ptr+1);
  //Update the BAM for the system track
  CDRSetClearBAM(track,sectors[i],True);
 end;
end;

{-------------------------------------------------------------------------------
Find the next free sector (converted directly from the 1541 code)
-------------------------------------------------------------------------------}
function TDiscImage.CDRFindNextSector(var track,sector: Byte): Boolean;
var
 BAM_free_blocks,s: Byte;
begin
 //Reset the counter
 BAM_free_blocks:=0;
 //Count up the free blocks/sectors in this track
 for s:=0 to Length(free_space_map[0,track-1])-1 do
  if free_space_map[0,track-1,s]=$00 then inc(BAM_free_blocks);
 //If we have some free sectors, find them
 if BAM_free_blocks>0 then
 begin
  //Following the same algorithm as the 1541 - interlace them by 10 sectors
  inc(sector,10);
  //If we have gone over the maximum number of sectors for the track,
  if sector>=Length(free_space_map[0,track-1]) then
  begin
   //Reduce by the maximum number of sectors
   dec(sector,Length(free_space_map[0,track-1]));
   //And if not zero, another one
   if sector>0 then dec(sector);
  end;
  //If it is not free, move onto the next, until we find a free sector
  while (free_space_map[0,track-1,sector]<>$00)
    and (sector<Length(free_space_map[0,track-1])-1) do
   inc(sector);
 end;
 //Return a false result if there are no free sectors in this track
 if (sector>=Length(free_space_map[0,track-1])) or (BAM_free_blocks=0) then
  Result:=False
 else //Otherwise return a positive
  Result:=True;
 //Both results will update track and sector
end;

{-------------------------------------------------------------------------------
Find the next free track
-------------------------------------------------------------------------------}
function TDiscImage.CDRFindNextTrack(var track,sector: Byte): Boolean;
var
 starttrack,
 counter,
 hightrack,
 lowtrack,
 cycles,i    : Byte;
 freesecs    : Boolean;
begin
 //Number of sides - also, number of times to repeat main loop
 if FFormat mod $10=1 then cycles:=2 else cycles:=1;
 //Counter for distance from root
 counter:=1;
 repeat
  //Counter for number of sides looked at
  i:=0;
  //We'll continue for as many sides there are
  while i<cycles do
  begin
   //Set up the root, lowest track number and highest track number
   if FFormat mod $10<2 then //1541 and 1571
    if i=0 then              //1541 and 1571 side 0
     begin
      starttrack:=18;
      lowtrack:=0;
      hightrack:=36;
     end
     else                    //1571 side 1
     begin
      starttrack:=53;
      lowtrack:=36;
      hightrack:=70;
     end
   else                      //1581
   begin
    starttrack:=40;
    lowtrack:=0;
    hightrack:=80;
   end;
   //We'll start off looking below the root
   if starttrack-counter>=lowtrack then
    track:=starttrack-counter
   else //Unless we have reached the front end, so look after the root
    track:=starttrack+counter;
   //Get a check on number of free sectors
   freesecs:=CDRFindNextSector(track,sector);
   //If none found, do it again bu the opposite way round
   if not freesecs then
   begin //If we previously looked before the root, look after
    if track=starttrack-counter then
     track:=starttrack+counter
    else //And vice-versa
     if starttrack-counter>=lowtrack then
      track:=starttrack-counter;
    //Another check for number of free sectors
    freesecs:=CDRFindNextSector(track,sector);
   end;
   //If we still have none, move our side counter on
   if not freesecs then inc(i) else i:=cycles; //Or finish it
  end;
  //Move our track counter on
  inc(counter);
  //Until we have either some free sectors, or run out of tracks
 until (freesecs) OR (track>=hightrack);
 //Return the result of free sectors
 Result:=freesecs;
end;

{-------------------------------------------------------------------------------
Rename a file
-------------------------------------------------------------------------------}
function TDiscImage.RenameCDRFile(oldfilename: String;var newfilename: String):Integer;
var
 ptr,entry,dir: Cardinal;
begin
 Result:=-2; //File does not exist
 //Check that the file exists
 if FileExists(oldfilename,ptr) then
 begin                            
  Result:=-3;//Destination already exists
  //FileExists returns a pointer to the file
  entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=ptr div $10000;  //Top 16 bits - directory reference
  //Make sure the new filename does not already exist
  if not FileExists(FDisc[dir].Entries[entry].Parent+dirsep+newfilename,ptr) then
  begin
   //Change the entry
   FDisc[dir].Entries[entry].Filename:=newfilename;
   //Update the catalogue
   UpdateCDRCat;
   Result:=entry;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Delete a file
-------------------------------------------------------------------------------}
function TDiscImage.DeleteCDRFile(filename: String):Boolean;
var
 ptr,entry,dir,i: Cardinal;
 track,sector   : Byte;
begin
 Result:=False;
 //Check that the file exists
 if FileExists(filename,ptr) then
 begin
  //FileExists returns a pointer to the file
  entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=ptr div $10000;  //Top 16 bits - directory reference
  //Remove all entries from the BAM
  track:=FDisc[dir].Entries[entry].Track;
  sector:=FDisc[dir].Entries[entry].Sector;
  while track<>$00 do
  begin
   CDRSetClearBAM(track,sector,False);
   ptr:=ConvertDxxTS(FFormat MOD$10,track,sector);
   track:=ReadByte(ptr);
   sector:=ReadByte(ptr+1);
   for i:=0 to $FF do WriteByte($00,ptr+i);// Delete the data
  end;
  //Remove the filename from the entries by moving the entries below up one
  for i:=entry+1 to Length(FDisc[dir].Entries)-1 do
   FDisc[dir].Entries[i-1]:=FDisc[dir].Entries[i];
  //Reduce the number of entries by 1
  SetLength(FDisc[dir].Entries,Length(FDisc[dir].Entries)-1);
  //Update the catalogue
  UpdateCDRCat;
  //Update the free space
  CDRFreeSpaceMap;
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Update a file's attribute or filetype
-------------------------------------------------------------------------------}
function TDiscImage.UpdateCDRFileAttributes(filename,attributes: String):Boolean;
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
  UpdateCDRCat;
  //And return a success
  Result:=True;
 end;
end;
