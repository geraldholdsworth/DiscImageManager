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
  if Length(Fdata)>0 then
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
 temp         : AnsiString;
const
 //Commodore 64 Filetypes
 FileTypes   : array[0.. 5] of AnsiString = (
 'DELDeleted' ,'SEQSequence','PRGProgram' ,'USRUser File','RELRelative',
 'CBMCBM'     );
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
                           Copy(FileTypes[ReadByte(ptr+(c*$20)+2) AND $0F],1,3);
    Result[0].Entries[amt].Filetype:=
                           Copy(FileTypes[ReadByte(ptr+(c*$20)+2) AND $0F],4);
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
  0 : SetLength(FData,175531);  //1541
  1 : SetLength(FData,351062);  //1571
  2 : SetLength(FData,822400);  //1581
 end;
 //Fill with zeros
 for t:=0 to Length(FData)-1 do FData[t]:=0;
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
   if ((c=dirTr-1) AND (ch<2) AND (f<2))
   OR ((c=dirTr1-1)AND (ch=0) AND (f=1))
   OR ((c=dirTr-1) AND (ch<4) AND (f=2)) then
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
Update the disc title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateCDRDiscTitle(title: AnsiString): Boolean;
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
function TDiscImage.ExtractCDRFile(filename: AnsiString;
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
 end;
 Result:=True;
end;

{-------------------------------------------------------------------------------
Write a file to Commodore image
-------------------------------------------------------------------------------}
function TDiscImage.WriteCDRFile(file_details: TDirEntry;
                             var buffer: TDIByteArray): Integer;
var
 count,
 ptr       : Cardinal;
 s:AnsiString;
 f,frag,i,
 track,
 sector    : Byte;
 block     : TDIByteArray;
 fragments : TFragmentArray;
begin
 Result:=-1;
 count:=file_details.Length;
 f:=FFormat MOD $10; //Minor format (sub format)
 //Overwrite the parent
 file_details.Parent:=root_name;
 //Check that the filename is valid
 file_details.Filename:=ValidateDFSFilename(file_details.Filename);
 //Make sure the file does not already exist
 if not(FileExists(file_details.Parent+dir_sep+file_details.Filename,ptr))then
 begin
  //How many fragments to split the file into
  frag:=count div 254;
  if count mod 254>0 then inc(frag);
  //Split the file
  SetLength(fragments,frag);
  for i:=0 to frag-1 do
   fragments[i].Length:=254;
  if count mod 254>0 then fragments[frag-1].Length:=count mod 254;
  //Where to put them - fragments are tended to be put around the root.
  //So we search backwards, then forwards, then backwards, etc.
  if count<free_space then //Will it actually fit?
  begin
   track:=18; //Value of this is unimportant
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
  end;
  s:='';
  for i:=0 to frag-1 do
   s:=s+IntToHex(fragments[i].Offset,4)+'>';
 end;
  Result:=-1;//Not needed - just using as a stop point
end;

{-------------------------------------------------------------------------------
Find the next free sector (converted directly from the 1541 code)
-------------------------------------------------------------------------------}
function TDiscImage.CDRFindNextSector(var track,sector: Byte): Boolean;
var
 BAM_free_blocks,s: Byte;
begin
 BAM_free_blocks:=0;
 for s:=0 to Length(free_space_map[0,track-1])-1 do
  if free_space_map[0,track-1,s]=$00 then inc(BAM_free_blocks);
 if free_space_map[0,track-1,sector]<>$00 then
  if BAM_free_blocks>0 then
  begin
   inc(sector,10);
   if sector>=Length(free_space_map[0,track-1]) then
   begin
    dec(sector,Length(free_space_map[0,track-1]));
    if sector>0 then dec(sector);
   end;
   while (free_space_map[0,track-1,sector]<>$00) and (sector<Length(free_space_map[0,track-1])-1) do
    inc(sector);
  end;
 if (sector>=Length(free_space_map[0,track-1])) or (BAM_free_blocks=0) then
  Result:=False
 else
  Result:=True;
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
 if FFormat mod $10=1 then cycles:=2 else cycles:=1;
 i:=0;
 while i<cycles do
 begin
  if FFormat mod $10<2 then
   if i=0 then
    begin
     starttrack:=18;
     lowtrack:=0;
     hightrack:=36;
    end
    else
    begin
     starttrack:=53;
     lowtrack:=36;
     hightrack:=70;
    end
  else
  begin
   starttrack:=40;
   lowtrack:=0;
   hightrack:=80;
  end;
  counter:=1;
  repeat
   track:=starttrack-counter;
   if track<=lowtrack then
    track:=starttrack+counter;
   inc(counter);
   freesecs:=CDRFindNextSector(track,sector);
  until (freesecs) OR (track>=hightrack);
  if not freesecs then inc(i) else i:=cycles;
 end;
 Result:=freesecs;
end;

function TDiscImage.RenameCDRFile(oldfilename: AnsiString;var newfilename: AnsiString):Boolean;
begin
 Result:=False;
end;
function TDiscImage.DeleteCDRFile(filename: AnsiString):Boolean;
begin
 Result:=False;
end; 
function TDiscImage.UpdateCDRFileAttributes(filename,attributes: AnsiString):Boolean;
begin
 Result:=False;
end;
