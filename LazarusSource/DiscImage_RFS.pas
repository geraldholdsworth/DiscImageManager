//++++++++++++++++++ Acorn RFS +++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a RFS
-------------------------------------------------------------------------------}
function TDiscImage.ID_RFS: Boolean;
var
 i    : Integer;
 copy : Cardinal;
 found: Boolean;
begin
 Result:=False;
 if FFormat=diInvalidImg then
  //Is there actually any data?
  if GetDataLength>0 then
  begin
   Result:=True; //Assume a postive ID for now
   //Check the signature
   for i:=0 to Length(RFSsig)-1 do
    if ReadByte(i)<>Ord(RFSsig[i+1])then Result:=False;
   //Check the rest of the header
   if(ReadByte($6)=$82)and(Result)then
   begin
    //Check the pointer to the code is within the data
    copy:=Read16b($4);
    if(copy<$8000)or(copy-$8000>GetDataLength)then Result:=False;
    //Get the (C) pointer
    copy:=ReadByte($7);
    //Does it point to '(C)'?
    if copy<GetDataLength-Length(RFScrt) then
    begin
     if ReadByte(copy)=$00 then
     begin
      for i:=1 to Length(RFScrt) do
       if ReadByte(copy+i)<>Ord(RFScrt[i]) then Result:=False;
     end else Result:=False;
    end else Result:=False;
    //Find the first valid block
    if Result then
    begin
     //We'll start at the header code
     i:=Read16b($4)-$8000;
     found:=False;
     while(i<GetDataLength)and(not found)do
     begin
      inc(i);
      found:=ValidRFSHeader(i);
     end;
     //Was not a valid block, so no +ve ID...or rememeber for later
     if not found then Result:=False else root:=i;
    end;
   end;
   //Set the internal format
   If Result then
   begin
    FFormat:=diAcornRFS<<4;
    //Set the disc size to the length of the uncompressed data
    disc_size[0]:=GetDataLength;
   end;
  end;
end;

{-------------------------------------------------------------------------------
Is this a valid ROM FS or CFS header?
-------------------------------------------------------------------------------}
function TDiscImage.ValidRFSHeader(ptr: Cardinal;cfs: Boolean=False): Boolean;
var
 j    : Cardinal;
begin
 Result:=False;
 //Look for a $2A '*'
 if ReadByte(ptr)=$2A then
 begin
  //Valid filename?
  j:=0;
  Result:=True;
  while(ReadByte(ptr+j)<>$00)and(ptr+j<GetDataLength)and(Result)do
{  begin
   if(ReadByte(ptr+j)<32)or(ReadByte(ptr+j)>126)then Result:=False;}
   inc(j);
//  end;
  //Check the block status bit 6 is set and block length is zero
//  if(IsBitSet(ReadByte($D+ptr+j),6))and(Read16b($B+ptr+j)>0)then Result:=False;
  //Or the block status bit 6 is clear and block length is not zero
//  if(not IsBitSet(ReadByte($D+ptr+j),6))and(Read16b($B+ptr+j)=0)then Result:=False;
  //Is the EOF marker valid? (Only for ROM FS)
  if not cfs then
   if(Result)
   and((Read16b($E+ptr+j)<ptr+$8000)
     or(Read16b($E+ptr+j)>GetDataLength+$8000))then Result:=False;
  //Is the Header CRC valid?
  if Result then if Read16b($12+ptr+j)<>GetCRC16(ptr+1,j+$11)then Result:=False;
 end;
end;

{-------------------------------------------------------------------------------
Read in and decode the file
-------------------------------------------------------------------------------}
function TDiscImage.ReadRFSImage: Boolean;
var
 i        : Integer;
 filenum,
 pos,
 nextfile,
 blocklen,
 blocknum,
 lastblock: Cardinal;
 temp     : String;
 blockst  : Byte;
 firstblck: Boolean;
 procedure AddData;
 begin
  if blocklen>0 then
  begin
   //Add the block length to the total length
   inc(FDisc[0].Entries[filenum].Length,blocklen);
   //Move to after the data
   inc(i,blocklen);
   //Next block
   inc(pos,i+2);
  end else inc(pos,i);//For zero length files
 end;
begin
 FDisc:=nil;
 //Set up the TDisc structure for return
 SetLength(FDisc,1);
 ResetDir(FDisc[0]);
 //Set the root directory name
 root_name:='ROM';
 FDisc[0].Directory:=root_name;
 FDisc[0].BeenRead:=True;
 //Set the filename
 imagefilename:='Untitled.'+FormatExt;
 //Free space
 free_space[0]:=16384-disc_size[0];
 //We found the first valid block in the ID process
 pos:=root;
 //Keep track of which file we are on
 filenum:=0;
 //The last block's status byte
 blockst:=$00;
 //Keep track of the last block's details
 lastblock:=0;
 firstblck:=False;
 //Where the next file is located
 nextfile:=disc_size[0];
 //Loop through until we run out of bytes
 while(pos<disc_size[0])or(ReadByte(pos)=$2B)do
 begin
  //Was the last data block seen the last block of the file?
  if IsBitSet(blockst,7) then
  begin
   inc(filenum);
   blockst:=0;
  end;
  //Check for a valid header
  if ValidRFSHeader(pos) then
  begin
   //Read in the filename
   temp:=ReadString(pos+1,$00,False); //Need to add in control codes
   i:=Length(temp)+1; //To keep the counter right
   //'i' becomes our pointer now
   inc(i);
   //Sometimes a file has no filename, so give it one
   if temp='' then temp:='?';
   //Create a new entry in our array, if need be
   if filenum>=Length(FDisc[0].Entries) then
   begin
    //Now create the entry for this file
    SetLength(FDisc[0].Entries,filenum+1);
    ResetDirEntry(FDisc[0].Entries[filenum]);
    FDisc[0].Entries[filenum].Length  :=0;      //Length counter
    FDisc[0].Entries[filenum].Filename:=FilenameToASCII(temp);//Filename
    FDisc[0].Entries[filenum].Sector  :=pos;  //Where to find it (first block)
    FDisc[0].Entries[filenum].Parent  :=FDisc[0].Directory;
    FDisc[0].Entries[filenum].DirRef  :=-1;
    //First block?
    firstblck:=True;
    //Read in the load address
    FDisc[0].Entries[filenum].LoadAddr:=Read32b(pos+i);
    //Read in the execution address
    FDisc[0].Entries[filenum].ExecAddr:=Read32b(pos+i+4);
   end;
   //Pointer to the next file
   nextfile:=Read16b($E+pos+i)-$8000;
   //Read in the block number
   blocknum:=Read16b(pos+i+8);
   //Is it a new block, or copy protection?
   if(blocknum>0)and(firstblck)and(Length(FDisc[0].Entries)>1)then
    if (lastblock=blocknum-1)
    and(FDisc[0].Entries[filenum-1].Filename=FDisc[0].Entries[filenum].Filename)then
    begin
     SetLength(FDisc[0].Entries,Length(FDisc[0].Entries)-1);
     dec(filenum);
     firstblck:=False;
    end;
   lastblock:=blocknum;
   //Get the length of this block
   blocklen:=Read16B(pos+i+10);
   //Get the block status
   blockst:=ReadByte(pos+i+12);
   if IsBitSet(blockst,0) then FDisc[0].Entries[filenum].Attributes:='L'
   else FDisc[0].Entries[filenum].Attributes:='';
   //Move our chunk pointer onto the data
   inc(i,19);//Points to the data
   //Add the data and move on
   AddData;
  end
  else
  begin
   //Repeat of previous block
   if ReadByte(pos)=$23 then
   begin
    //Only the single byte here
    i:=1;
    //Add the data and move on
    AddData;
   end
   else pos:=nextfile; //Not a valid header, or repeater, so move to the next file
  end;
 end;
 Result:=Length(FDisc)>0;
end;

{-------------------------------------------------------------------------------
Re-adjust the offsets in the header 6502 code
-------------------------------------------------------------------------------}
function TDiscImage.AdjustRFSOffsets(base: Cardinal): Boolean;
var
 invsno,
 invert,
 claim,
 claiml : Word;
 sig    : String;
 i      : Byte;
begin
 Result:=False;
 //Is it one we've created? Get the signature string
 sig:='';
 for i:=0 to $F do sig:=sig+chr(ReadByte(base+$51+i));
 //If it isn't, we can't relocate the code
 if sig='DiscImageManager' then
 begin
  WriteByte((root+$8000)AND$FF,$12+base); //Data address low
  WriteByte((root+$8000)DIV$100,$16+base);//Data address High
  invsno:=$8079+(base-$2F);
  invert:=$807B+(base-$2F);
  claim :=$804F+(base-$2F);
  claiml:=$8065+(base-$2F);
  Write16b(invsno,base+$0B);
  Write16b(invert,base+$1C);
  Write16b(invsno,base+$2B);
  Write16b(claim ,base+$3D);
  Write16b(invsno,base+$40);
  Write16b(claiml,base+$48);
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Create a new, empty, ROM FS image
-------------------------------------------------------------------------------}
function TDiscImage.FormatRFS: Boolean;
begin
 Result:=FormatRFS('','','',$01);
end;
function TDiscImage.FormatRFS(title: String): Boolean;
begin
 Result:=FormatRFS(title,'','',$01);
end;
function TDiscImage.FormatRFS(title,copyright: String): Boolean;
begin
 Result:=FormatRFS(title,copyright,'',$01);
end;
function TDiscImage.FormatRFS(title,copyright,version: String): Boolean;
begin
 Result:=FormatRFS(title,copyright,version,$01);
end;
function TDiscImage.FormatRFS(title,copyright,version: String;binvers: Byte): Boolean;
var
 i      : Integer;
 ptr    : Cardinal;
const
 BlkFile: array[0..$1D] of Byte=(
                $2A,$2A,$44,$49,$4D,$2D,$52,$4F,$4D,$2A,$00,$00,$00,$00,$00,$00,
                $00,$00,$00,$00,$00,$00,$00,$C0,$DE,$80,$00,$00,$2E,$9A);
begin
 Result:=False;
 FDisc:=nil;
 //Blank everything
 ResetVariables;
 //Set up the TDisc structure for return
 SetLength(FDisc,1);
 ResetDir(FDisc[0]);
 //Set the root directory name
 root_name:='ROM';
 FDisc[0].Directory:=root_name;
 FDisc[0].BeenRead:=True;
 //Set the format
 FFormat:=diAcornRFS<<4;
 //Set the filename
 imagefilename:='Untitled.'+FormatExt;
 //Set up the arrays
 SetLength(disc_size,1);
 SetLength(free_space,1);
 //Setup the data area (16K)
 SetDataLength(16384);
 //Write the signature
 for i:=0 to Length(RFSsig)-1 do WriteByte(Ord(RFSsig[i+1]),i);
 //ROM Type
 WriteByte($82,6);
 //Version number
 WriteByte(binvers,8);
 //Title string
 if title='' then title:=rfstitle;
 WriteString(title,$9,0,0);
 ptr:=Length(title)+9;
 WriteByte(0,ptr);
 inc(ptr);
 //Version String
 if version='' then version:='1.00';
 WriteString(version,ptr,0,0);
 inc(ptr,Length(version));
 WriteByte(0,ptr);
 //Pointer to (C)
 WriteByte(ptr,7);
 //(C) string
 if copyright='' then copyright:=rfscopyright;
 if(copyright[1]<>'(')
 or(copyright[2]<>'C')
 or(copyright[3]<>')')then copyright:='(C)'+copyright;
 WriteString(copyright,ptr+1,0,0);
 inc(ptr,Length(copyright)+1);
 WriteByte(0,ptr);
 inc(ptr);
 //Pointer to code header
 Write16b($8000+ptr,4);
 //Code header
 for i:=Low(ROMHDR) to High(ROMHDR) do
  WriteByte(ROMHDR[i],ptr+(i-Low(ROMHDR)));
 //Update the root address
 root:=ptr+Length(ROMHDR);
 //Adjust the offsets
 if AdjustRFSOffsets(ptr) then
 begin
  inc(ptr,Length(ROMHDR));
  //Write blank file
  for i:=0 to Length(BlkFile)-1 do WriteByte(BlkFile[i],root+i);
  //Update the EOF
  WriteByte(ptr+Length(BlkFile)+$8000,ptr+$18);
  //And the Header CRC
  Write16b(GetCRC16(root+1,Length(BlkFile)-3),root+Length(BlkFile)-2);
  inc(ptr,Length(BlkFile));
  //No more files
  WriteByte($2B,ptr);
  disc_size[0]:=ptr+1;
  SetDataLength(disc_size[0]);
  free_space[0]:=16384-disc_size[0];
  //Read it back in again
  Result:=ReadRFSImage;
 end;
end;

{-------------------------------------------------------------------------------
Extracts a file from ROM FS
-------------------------------------------------------------------------------}
function TDiscImage.ExtractRFSFile(entry: Integer;
                                               var buffer:TDIByteArray):Boolean;
var
 i,fn : Integer;
 ptr,
 dat,
 pos  : Cardinal;
 crc,
 len  : Word;
begin
 //Default return result
 Result   :=False;
 if entry<Length(FDisc[0].Entries) then
 begin
  //Set up the receiving buffer
  SetLength(buffer,FDisc[0].Entries[entry].Length);
  //Only if not a zero length file
  if FDisc[0].Entries[entry].Length>0 then
  begin
   //Pointer into this buffer
   ptr:=0;
   //Filename length
   fn:=Length(FDisc[0].Entries[entry].Filename)+1;
   //Pointer to the file
   pos:=FDisc[0].Entries[entry].Sector;
   //Lets build the data
   while ptr<FDisc[0].Entries[entry].Length do
   begin
    //Are we looking at a full header?
    if ReadByte(pos)=$2A then
    begin
     len:=Read16b(pos+fn+$B); //Block length
     dat:=pos+fn+$14; //Data position
    end;
    //Or are we looking at a repeat header?
    if ReadByte(pos)=$23 then dat:=pos+1; //Data position
    //Read in the Data CRC
    crc:=Read16b(dat+len);
    //Grab this part of the data, but only if it is valid
    if crc=GetCRC16(dat,len) then
     for i:=0 to len-1 do buffer[ptr+i]:=ReadByte(dat+i)
    else for i:=0 to len-1 do buffer[ptr+i]:=$00; //Otherwise fill with zeros
    //Move along
    inc(ptr,len);
    //Reposition our file pointer
    pos:=dat+len+2;
   end;
  end;
  //Return a +ve result
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Writes a new file to a ROM FS
-------------------------------------------------------------------------------}
function TDiscImage.WriteRFSFile(var file_details: TDirEntry;
                          var buffer: TDIByteArray;insert: Integer=-1): Integer;
var
 j,fn    : Integer;
 filelen,
 fileptr,
 len,
 ptr     : Cardinal;
 blocknum: Word;
 blockst : Byte;
begin
 Result:=-5; //Unknown error
 //Make sure the filename is not beyond max length
 file_details.Filename:=LeftStr(file_details.Filename,10);
 //Work out the total file length, including header and block headers
 filelen:=Length(file_details.Filename)+$14+1; //Header length, zero length file
 if Length(buffer)>0 then
 begin
  //Single block
  if Length(buffer)<=256 then inc(filelen,2); //Just the Data CRC
  //More than 1 block - we'll need two headers
  if Length(buffer)>256 then filelen:=filelen*2+4; //With Data CRC
  //More than 2 blocks - we'll need the block repeaters
  if Length(buffer)>512 then inc(filelen,(((Length(buffer)-512)div 256)+1)*3);
 end;
 //Add the actual file data
 inc(filelen,Length(buffer));
 //Is there something to add to, and enough space?
 if(Length(FDisc)=1)and(free_space[0]>=filelen)then
 begin
  //Increase the entries
  SetLength(FDisc[0].Entries,Length(FDisc[0].Entries)+1);
  //Are we inserting this file, or adding to the end?
  if insert>=0 then
  begin
   for j:=Length(FDisc[0].Entries)-1 downto insert+1 do
   begin
    FDisc[0].Entries[j]:=FDisc[0].Entries[j-1];
    inc(FDisc[0].Entries[j].Sector,filelen);
   end;
   Result:=insert;
  end
  else Result:=Length(FDisc[0].Entries)-1; //Return the new pointer
  //Update the entry
  ResetDirEntry(FDisc[0].Entries[Result]);
  FDisc[0].Entries[Result]:=file_details; //Copy the entry across
  //Override some of the settings
  FDisc[0].Entries[Result].Filename:=FilenameToASCII(file_details.Filename);//Filename
  FDisc[0].Entries[Result].Sector  :=0;  //Where to find it (first block)
  FDisc[0].Entries[Result].Parent  :=FDisc[0].Directory;//Parent
  FDisc[0].Entries[Result].DirRef  :=-1;//Not a directory
  //Update the disc size and reduce the free space
  inc(disc_size[0],filelen);
  free_space[0]:=16384-disc_size[0];
  //Increase the data area
  SetDataLength(disc_size[0]);
  //Add the file to the data
  ptr:=root;
  //Where to put this file
  if Result>0 then
  begin
   fileptr:=FDisc[0].Entries[Result-1].Sector
           +Length(FDisc[0].Entries[Result-1].Filename)+1;
   ptr:=Read32b(fileptr+$E)-$8000;
  end;
  if Result=0 then ptr:=root+filelen;
  //If we are inserting, then we need to move the data & re-adjust the pointers
  if insert>=0 then
  begin
   //Move the data
   for j:=disc_size[0]-1 downto FDisc[0].Entries[insert].Sector+filelen do
    WriteByte(ReadByte(j-filelen),j);
   //Re-adjust the pointers
   RFSReAdjustPointers(FDisc[0].Entries[insert].Sector+filelen,filelen);
  end;
  FDisc[0].Entries[Result].Sector:=ptr; //Update the pointer
  fn:=Length(FDisc[0].Entries[Result].Filename)+1;//Filename length
  //Where are we in the file?
  fileptr:=0;
  //Block counter
  blocknum:=0;
  while fileptr<Length(buffer) do
  begin
   //We need to know the length of this data block
   if fileptr+$100>Length(buffer) then len:=Length(buffer)-fileptr
   else len:=$100;
   //Now the data block header
   //Are we first/last or intermediate block?
   if(fileptr<$100)or(fileptr>=Length(buffer)-len)then
   begin
    //Sync byte
    WriteByte($2A,ptr);
    inc(ptr);
    //Filename
    WriteString(FDisc[0].Entries[Result].Filename,ptr,0,0);
    WriteByte(0,ptr+fn-1);
    inc(ptr,fn);
    //Load Address
    Write32b(FDisc[0].Entries[Result].LoadAddr,ptr);
    //Exec Address
    Write32b(FDisc[0].Entries[Result].ExecAddr,ptr+4);
    //Block number
    Write16b(blocknum,ptr+8);
    //Length of this block
    Write16b(len,ptr+10);
    //Block status
    blockst:=$00;
    if fileptr+len>=Length(buffer) then
     blockst:=blockst OR $80; //Final block
    if Pos('L',FDisc[0].Entries[Result].Attributes)>0 then
     blockst:=blockst OR $01; //Locked
    WriteByte(blockst,ptr+12);
    //Next file pointer
    Write32b(FDisc[0].Entries[Result].Sector+$8000+filelen,ptr+13);
    //Header CRC-16
    Write16b(GetCRC16(ptr-fn,fn+17),ptr+17);
    inc(ptr,19);
   end
   else
   begin
    WriteByte($23,ptr);
    inc(ptr);
   end;
   inc(blocknum);
   //Data
   for j:=0 to len-1 do WriteByte(buffer[fileptr+j],ptr+j);
   //Data CRC-16
   Write16b(GetCRC16(ptr,len),ptr+len);
   //Move data pointer on
   inc(ptr,2+len);
   //Move file pointer on
   inc(fileptr,len);
  end;
  WriteByte($2B,disc_size[0]-1);
 end;
end;

{-------------------------------------------------------------------------------
Deletes a file from ROM FS
-------------------------------------------------------------------------------}
function TDiscImage.DeleteRFSFile(entry: Cardinal): Boolean;
var
 filepos,
 eofpos,
 diff,i  : Cardinal;
 Lfile   : String;
begin
 Result:=False;
 //We won't leave the image with no files
 if Length(FDisc[0].Entries)>1 then
 begin
  //Locate the file
  filepos:=FDisc[0].Entries[entry].Sector;
  //Get the pointer to the next file
  Lfile:=ReadString(filepos+1,0);//Need to get the filename first
  eofpos:=Read32b(filepos+Length(Lfile)+$F)-$8000; //Where the next file is
  diff:=eofpos-filepos; //And the amount we need to adjust addresses by
  //Move the later files down
  for i:=eofpos to disc_size[0]-1 do WriteByte(ReadByte(i),i-diff);
  //Reduce the data length
  SetDataLength(disc_size[0]-diff);
  disc_size[0]:=GetDataLength;
  free_space[0]:=16384-disc_size[0];
  //Re-adjust the EOF pointers in all block headers
  RFSReAdjustPointers(filepos,diff);
  //Remove from the internal array
  if entry<Length(FDisc[0].Entries)-1 then
   for i:=entry to Length(FDisc[0].Entries)-2 do
   begin
    FDisc[0].Entries[i]:=FDisc[0].Entries[i+1];
    dec(FDisc[0].Entries[i].Sector,diff); //Adjust where to find it
   end;
  SetLength(FDisc[0].Entries,Length(FDisc[0].Entries)-1);
  //All good, then let's go home.
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Readjust the pointers in RFS blocks
-------------------------------------------------------------------------------}
procedure TDiscImage.RFSReAdjustPointers(filepos: Cardinal;diff: Integer);
var
 len  : Word;
 Lfile: String;
 H    : Byte;
begin
 if diff=0 then exit; //If no change, then not worth doing
 //Block length (for repeaters, we'll default to 256 bytes)
 len:=$100;
 //Keep going until we hit the End of ROM marker, or run out of image
 while(ReadByte(filepos)<>$2B)and(filepos<disc_size[0])do
 begin
  //Read the first byte
  H:=ReadByte(filepos);
  //Full block header
  if H=$2A then
  begin
   //Get the filename
   Lfile:=ReadString(filepos+1,0);
   //Update the address
   Write32b(Read32b(filepos+Length(Lfile)+$F)-diff,filepos+Length(Lfile)+$F);
   //Update the CRC
   Write16b(GetCRC16(filepos+1,Length(Lfile)+$12),filepos+Length(Lfile)+$13);
   //Get the block length
   len:=Read16b(filepos+Length(Lfile)+$C);
   //Move along
   inc(filepos,len+Length(Lfile)+$17);
  end;
  //Block repeater
  if H=$23 then inc(filepos,len+3);
  //Not recognised, so jump out of the loop and procedure
  if(H<>$23)and(H<>$2A)and(H<>$2B)then filepos:=disc_size[0];
 end;
end;

{-------------------------------------------------------------------------------
Move an RFS file
-------------------------------------------------------------------------------}
function TDiscImage.MoveRFSFile(entry: Cardinal;dest: Integer): Integer;
begin
 //NOTE: dest is where after to insert the file (-1 is at the top)
 Result:=-5; //Unknown error
 //Extract the original file into a buffer, and make a note of the file details
 //Delete the file
 //Adjust the destination to account for the deleted file
 //Write the file into the appropriate position
end;
