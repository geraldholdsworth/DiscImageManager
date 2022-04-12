//++++++++++++++++++ Acorn CFS +++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a UEF
-------------------------------------------------------------------------------}
function TDiscImage.ID_CFS: Boolean;
var
 i : Integer;
const
 uefstring = 'UEF File!';
begin
 Result:=False;
 if FFormat=diInvalidImg then
  //Is there actually any data?
  if GetDataLength>0 then
  begin
   //Test to make sure it is a UEF file
   Result:=True;
   for i:=1 to Length(uefstring) do
    if ReadByte(i-1)<>Ord(uefstring[i])then Result:=False;
   //Set the internal format
   If Result then
   begin
    FFormat:=diAcornUEF<<4;
    //Set the disc size to the length of the uncompressed data
    disc_size[0]:=GetDataLength;
   end;
  end;
end;

{-------------------------------------------------------------------------------
Read in and decode the file
-------------------------------------------------------------------------------}
function TDiscImage.ReadUEFFile: TDisc;
var
 i,j      : Integer;
 filenum,
// baud,
 pos,
 ptr,
 chunkid,
 chunklen,
 blocklen,
 blocknum,
 lastblock,
 headcrc,
 datacrc  : Cardinal;
 temp     : String;
// tone     : Real;
 blockst  : Byte;
 firstblck,
 crcok    : Boolean;
 dummy    : TDIByteArray;
begin
 Result:=nil;
 SetLength(dummy,0);
 //Set up the TDisc structure for return
 Result:=FormatCFS;
{ SetLength(Result,1);
 ResetDir(Result[0]);
 //Set the root directory name
 root_name:='tape';
 Result[0].Directory:=root_name;}
// baud:=1200;
 //Starting position is after the magic string
 pos:=$0C;
 //Keep track of which file we are on
 filenum:=0;
 //The last block's status byte
 blockst:=$00;
 //CRC Checks
 crcok:=True;
 //Keep track of the last block's details
 lastblock:=0;
 firstblck:=False;
 //Loop through until we run out of bytes
 while pos<disc_size[0] do
 begin
  //Read in the chunk ID
  chunkid :=Read16b(pos);
  //And the chunk length
  chunklen:=Read32b(pos+2);
  //Was the last data block seen the last block of the file?
  if IsBitSet(blockst,7) then
  begin
   inc(filenum);
   blockst:=0;
  end;
  //Move on after the header
  inc(pos,6);
  //Decode the chunk
  case chunkid of
{   $0000 : //Origin Information +++++++++++++++++++++++++++++++++++++++++++++++
    temp:=ReadString(pos,$00);
   $0005 : //Target Machine Type ++++++++++++++++++++++++++++++++++++++++++++++
    temp:='Target Machine is '+CFSTargetMachine(ReadByte(pos));}
   $0100 : //Implicit Start/Stop Bit Tape Data Block ++++++++++++++++++++++++++
    //Check for sync byte
    if ReadByte(pos)=$2A then // $2A is the sync byte
    begin
     //Read in the filename
     temp:=ReadString(pos+1,$00,False); //Need to add in control codes
     i:=Length(temp)+1; //To keep the counter right
     //'i' becomes our pointer now
     inc(i);
     //Sometimes a file has no filename, so give it one
     if temp='' then temp:='?';
     //Create a new entry in our array, if need be
     if filenum>=Length(Result[0].Entries) then
     begin
      //If the last file failed CRC checks on any block, clear the data
      if (not crcok) and (Length(CFSFiles)>0) then
       SetLength(CFSFiles[filenum],0);
      //Now create the entry for this file
      SetLength(Result[0].Entries,filenum+1);
      ResetDirEntry(Result[0].Entries[filenum]);
      Result[0].Entries[filenum].Length  :=0;      //Length counter
      Result[0].Entries[filenum].Filename:=FilenameToASCII(temp);//Filename
      Result[0].Entries[filenum].Sector  :=pos-6;  //Where to find it (first block)
      Result[0].Entries[filenum].Parent  :=Result[0].Directory;
      Result[0].Entries[filenum].DirRef  :=-1;
      SetLength(CFSFiles,filenum+1);
      firstblck:=True;
      //Read in the load address
      Result[0].Entries[filenum].LoadAddr:=Read32b(pos+i);
      //Read in the execution address
      Result[0].Entries[filenum].ExecAddr:=Read32b(pos+i+4);
      //CRC Checks
      crcok:=True;
     end;
     //Read in the block number
     blocknum:=Read16b(pos+i+8);
     //Is it a new block, or copy protection?
     if(blocknum>0)and(firstblck)and(Length(Result[0].Entries)>1)then
      if (lastblock=blocknum-1)
      and(Result[0].Entries[filenum-1].Filename=Result[0].Entries[filenum].Filename)
       {and(files[filenum-1].LoadAddr=files[filenum].LoadAddr)
       and(files[filenum-1].ExecAddr=files[filenum].ExecAddr)}then
      begin
       SetLength(Result[0].Entries,Length(Result[0].Entries)-1);
       dec(filenum);
       firstblck:=False;
      end;
     lastblock:=blocknum;
     //Take a note of where we are in the file's data, as we build it up
     ptr:=Result[0].Entries[filenum].Length;
     //Get the length of this block
     blocklen:=Read16B(pos+i+10);
     //And add it to the total length
     inc(Result[0].Entries[filenum].Length,blocklen);
     //Get the block status
     blockst:=ReadByte(pos+i+12);
     if IsBitSet(blockst,0) then
      Result[0].Entries[filenum].Attributes:='L'
     else
      Result[0].Entries[filenum].Attributes:='';
     //Get the CRC16 value for the header
     headcrc:=Read16b(pos+i+17);
     //Check it is valid
     if headcrc<>GetCRC16(pos+1,i+16,dummy) then crcok:=False;
     //Move our chunk pointer onto the data
     inc(i,19);//Points to the data
     //Increase the file's data length to match the total length, so far
     SetLength(CFSFiles[filenum],Result[0].Entries[filenum].Length);
     //And copy in the data in this block
     for j:=0 to blocklen-1 do CFSFiles[filenum][ptr+j]:=ReadByte(pos+i+j);
     //Move to after the data
     inc(i,blocklen);
     //So we can read in the data's CRC
     datacrc:=Read16b(pos+i);
     //Check it is valid
     if datacrc<>GetCRC16(ptr,blocklen,CFSFiles[filenum]) then crcok:=False;
    end;
{   $0110 : //High Tone ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //Work out the length of the tone
    tone:=Read16b(pos)*(1/(baud*2))*8;
   $0112 : //Baudwise Gap +++++++++++++++++++++++++++++++++++++++++++++++++++++
    //Work out the length of the gap
    tone:=Read16b(pos)*(1/(baud*2))*8;}
  end;
  //Move our offset pointer to the next chunk
  inc(pos,chunklen);
 end;
end;

{-------------------------------------------------------------------------------
Convert the block status byte into a human readable string
-------------------------------------------------------------------------------}
function TDiscImage.CFSBlockStatus(status: Byte): String;
begin
 Result:='';
 if status AND $01=$01 then Result:=Result+'Locked ';
 if status AND $40=$40 then Result:=Result+'Zero length ';
 if status AND $80=$80 then Result:=Result+'Final block';
end;

{-------------------------------------------------------------------------------
Convert the target machine byte into a human readable string
-------------------------------------------------------------------------------}
function TDiscImage.CFSTargetMachine(machine: Byte): String;
begin
 Result:='not specified ('+IntToHex(machine,2)+')';
 case machine and 7 of
  0: Result:='BBC Model A';
  1: Result:='Acorn Electron';
  2: Result:='BBC Model B';
  3: Result:='BBC Master';
  4: Result:='Acorn Atom';
 end;
end;

{-------------------------------------------------------------------------------
Extracts a file from the UEF
-------------------------------------------------------------------------------}
function TDiscImage.ExtractCFSFile(entry: Integer;var buffer:TDIByteArray):Boolean;
var
 i: Integer;
begin
 //As UEFs can have many files with the same name, we need to use the direct
 //access into the array - entry is the index of FDisc{x].Entries
 Result:=Length(CFSFiles[entry])>0; //Return a false result if no data
 //If the CRC check failed, there will be no data
 if Result then
 begin
  //Set the buffer
  SetLength(buffer,Length(CFSFiles[entry]));
  //Copy the data across
  for i:=0 to Length(CFSFiles[entry])-1 do buffer[i]:=CFSFiles[entry][i];
 end;
end;

{-------------------------------------------------------------------------------
Rebuilds and saves a UEF file
-------------------------------------------------------------------------------}
procedure TDiscImage.WriteUEFFile(filename: String;uncompress: Boolean=False);
var
 entry,
 ptr,
 fileptr,
 len     : Cardinal;
 buffer  : TDIByteArray;
 temp    : String;
 i,j     : Integer;
 blockst,
 blocknum: Byte;
 dummy   : TDIByteArray;
 F       : TGZFileStream;
 Func    : TFileStream;
begin
 SetLength(dummy,0);
 //Only continue if there are any entries
 if Length(FDisc[0].Entries)>0 then
 begin
  //Clear the data area
  SetDataLength(12);
  //Header
  temp:='UEF File!'+#00+#05+#00;
  for i:=1 to Length(temp) do WriteByte(Ord(temp[i]),i-1);
  //First chunk - 'made by' chunk
  temp:='Disc Image Manager'+#00;
  SetDataLength(12+Length(temp)+6);
  Write16b($0000,$0C);
  Write32b(Length(temp),$0E);
  for i:=1 to Length(temp) do WriteByte(Ord(temp[i]),$11+i);
  //Files
  for entry:=0 to Length(FDisc[0].Entries)-1 do
  begin
   //Set up our file pointer
   ptr:=GetDataLength;
   //Get the file - Only write the file if there is something to write
   if ExtractCFSFile(entry,buffer) then
   begin
    //Write the leading tone, single byte data block and another tone to start
    //Leading tone, 5 seconds
    SetDataLength(ptr+8);
    Write16b($0110,ptr);
    Write32b(2,ptr+2);
    Write16b($05DC,ptr+6);
    ptr:=GetDataLength;
    //Single byte data block
    SetDataLength(ptr+7);
    Write16b($0100,ptr);
    Write32b(1,ptr+2);
    WriteByte($DC,ptr+6);
    ptr:=GetDataLength;
    //Second leading tone, 5 seconds
    SetDataLength(ptr+8);
    Write16b($0110,ptr);
    Write32b(2,ptr+2);
    Write16b($05DC,ptr+6);
    ptr:=GetDataLength;
    //Where are we in the file?
    fileptr:=0;
    //Block counter
    blocknum:=0;
    while fileptr<Length(CFSFiles[entry]) do
    begin
     //Data block
     SetDataLength(ptr+6);
     Write16b($100,ptr);
     //We need to know the length of this block
     if fileptr+$100>Length(CFSFiles[entry]) then
      len:=Length(CFSFiles[entry])-fileptr
     else
      len:=$100;
     //And the length of the filename
     temp:=FDisc[0].Entries[entry].Filename+#00;
     //Then write the length of the chunk
     Write32b(len+22+Length(temp),ptr+2);
     ptr:=GetDataLength;
     //Now the data block header
     SetDataLength(GetDataLength+1);
     WriteByte($2A,ptr); //Sync byte
     //Filename
     SetDataLength(ptr+Length(temp));
     for i:=1 to Length(temp) do WriteByte(Ord(temp[i]),ptr+i);
     inc(ptr,Length(temp)+1);
     //Rest of the header
     SetDataLength(ptr+19);
     //Load Address
     Write32b(FDisc[0].Entries[entry].LoadAddr,ptr);
     //Exec Address
     Write32b(FDisc[0].Entries[entry].ExecAddr,ptr+4);
     //Block number
     Write16b(blocknum,ptr+8);
     inc(blocknum);
     //Length of this block
     Write16b(len,ptr+10);
     //Block status
     blockst:=$00;
     if fileptr+len>=Length(CFSFiles[entry]) then
      blockst:=blockst OR $80; //Final block
     if Pos('L',FDisc[0].Entries[entry].Attributes)>0 then
      blockst:=blockst OR $01; //Locked
     WriteByte(blockst,ptr+12);
     //Unused bytes
     Write32b($00,ptr+13);
     //Header CRC-16
     Write16b(GetCRC16(ptr-Length(temp),Length(temp)+17,dummy),ptr+17);
     //Data
     SetDataLength(GetDataLength+len+3);
     for j:=0 to len do WriteByte(CFSFiles[entry][fileptr+j],ptr+19+j);
     //Data CRC-16
     Write16b(GetCRC16(ptr+19,len,dummy),ptr+19+len);
     //Move data pointer on
     inc(ptr,21+len);
     //Write the tone chunk
     SetDataLength(GetDataLength+8);
     Write16b($110,ptr);
     Write32b(2,ptr+2);
     if fileptr+len>=Length(CFSFiles[entry]) then
      Write16b($07D0,ptr+6)  //Final block, so longer tone
     else
      Write16b($0258,ptr+6); //Short tone as not at the end
     //Move file pointer on
     inc(fileptr,len);
     //Move main pointer on
     inc(ptr,8);
    end;
    //Write a silence gap
    if entry<Length(FDisc[0].Entries)-1 then
    begin
     SetDataLength(ptr+8);
     Write16b($112,ptr);
     Write32b(2,ptr+2);
     Write16b($07D0,ptr+6);
     inc(ptr,8);
    end;
   end;
  end;
  //Finally, write the data out to the file, compressed
  if not uncompress then
  begin
   try
    F:=TGZFileStream.Create(filename,gzOpenWrite);
    F.Seek(0,0);
    F.Write(Fdata[0],Length(Fdata)-1);
    F.Free;
   finally
   end;
  end;
  //Or, write the data out to the file, uncompressed
  if uncompress then
  begin
   try
    Func:=TFileStream.Create(filename,fmCreate OR fmShareDenyNone);
    Func.Write(Fdata[0],Length(Fdata)-1);
    Func.Free;
   finally
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Create a new, empty, UEF file for CFS
-------------------------------------------------------------------------------}
function TDiscImage.FormatCFS:TDisc;
begin
 Result:=nil;
 //Set up the TDisc structure for return
 SetLength(Result,1);
 ResetDir(Result[0]);
 //Set the root directory name
 root_name:='tape';
 Result[0].Directory:=root_name;
 Result[0].BeenRead:=True;
 //Set the format
 FFormat:=diAcornUEF<<4;
 //Set the filename
 imagefilename:='Untitled.'+FormatExt;
end;

{-------------------------------------------------------------------------------
Delete a file from CFS
-------------------------------------------------------------------------------}
function TDiscImage.DeleteCFSFile(entry: Cardinal): Boolean;
var
 i: Integer;
begin
 //Start with a negative result
 Result:=False;
 //Make sure we have a tree
 if Length(FDisc)=1 then
  if entry<Length(FDisc[0].Entries) then
  begin
   //Entry is not the last one
   if entry<Length(FDisc[0].Entries)-1 then
   begin
    //Move them all down by one
    for i:=entry+1 to Length(FDisc[0].Entries)-1 do
    begin
     FDisc[0].Entries[i-1]:=FDisc[0].Entries[i];
     CFSFiles[i-1]:=CFSFiles[i];
    end;
   end;
   //Reduce the length by one
   SetLength(FDisc[0].Entries,Length(FDisc[0].Entries)-1);
   //And the data files
   SetLength(CFSFiles,Length(CFSFiles)-1);
   //And signal a success
   Result:=True;
  end;
end;

{-------------------------------------------------------------------------------
Updates whether a CFS file is locked or not
-------------------------------------------------------------------------------}
function TDiscImage.UpdateCFSAttributes(entry: Cardinal;attributes: String): Boolean;
begin
 //Start with a negative result
 Result:=False;
 if Length(FDisc)=1 then //Make sure we have something
  if entry<Length(FDisc[0].Entries) then //And we're not overshooting
  begin
   //Then simply update the attributes
   FDisc[0].Entries[entry].Attributes:=attributes;
   //And return a positive result
   Result:=True;
  end;
end;

{-------------------------------------------------------------------------------
Moves a CFS file (reorder) to after dest
-------------------------------------------------------------------------------}
function TDiscImage.MoveCFSFile(entry: Cardinal;dest: Integer): Integer;
var
 file_details: TDirEntry;
 buffer      : TDIByteArray;
 i           : Integer;
begin
 Result:=-5; //Unknown error
 if dest<-1 then dest:=-1; //A destination of -1 is at the top
 if Length(FDisc)=1 then
  if (entry<Length(FDisc[0].Entries))
  and(dest <Length(FDisc[0].Entries))
  and(entry<>dest)then
  begin
   Result:=-1; //Could not load file
   //Extract the data for the file being moved
   if ExtractCFSFile(entry,buffer) then
   begin
    Result:=-5; //Unknown error
    //And the file details
    file_details:=FDisc[0].Entries[entry];
    if dest>=0 then
    begin
     //Are we moving down?
     if entry>dest then
     begin
      for i:=entry downto dest+2 do
      begin
       FDisc[0].Entries[i]:=FDisc[0].Entries[i-1];
       CFSFiles[i]:=CFSFiles[i-1];
      end;
      inc(dest);
     end;
     //Are we moving up?
     if dest>entry then
      for i:=entry+1 to dest do
      begin
       FDisc[0].Entries[i-1]:=FDisc[0].Entries[i];
       CFSFiles[i-1]:=CFSFiles[i];
      end;
    end;
    //Is the destination -1? This means insert at the front
    if(dest=-1)and(entry>0)then
    begin
     for i:=entry-1 downto 0 do
     begin
      FDisc[0].Entries[i+1]:=FDisc[0].Entries[i];
      CFSFiles[i+1]:=CFSFiles[i];
     end;
     dest:=0; //Where we are moving to
    end;
    //Then insert it after the one specified
    FDisc[0].Entries[dest]:=file_details;
    CFSFiles[dest]:=buffer;
    Result:=dest;
   end;
  end;
end;

{-------------------------------------------------------------------------------
Copies a CFS file to after dest
-------------------------------------------------------------------------------}
function TDiscImage.CopyCFSFile(entry: Cardinal;dest: Integer): Integer;
var
 file_details: TDirEntry;
 buffer      : TDIByteArray;
 i           : Integer;
begin
 Result:=-5; //Unknown
 if dest<-1 then dest:=-1; //A destination of -1 is at the top
 if Length(FDisc)=1 then
  if (entry<Length(FDisc[0].Entries))
  and(dest <Length(FDisc[0].Entries))then
  begin
   Result:=-1; //Could not load file
   //Extract the data for the file being copied
   if ExtractCFSFile(entry,buffer) then
   begin
    Result:=-5; //Unknown error
    //And the file details
    file_details:=FDisc[0].Entries[entry];
    //Increase the list length
    SetLength(FDisc[0].Entries,Length(FDisc[0].Entries)+1);
    SetLength(CFSFiles,Length(CFSFiles)+1);
    if dest+1<Length(FDisc[0].Entries)-1 then
     for i:=Length(FDisc[0].Entries)-2 downto dest+1 do
     begin
      //Move them all up by one
      FDisc[0].Entries[i+1]:=FDisc[0].Entries[i];
      CFSFiles[i+1]:=CFSFiles[i];
     end;
    //Then insert it after the one specified
    FDisc[0].Entries[dest+1]:=file_details;
    CFSFiles[dest+1]:=buffer;
    Result:=dest+1;
   end;
  end;
end;

{-------------------------------------------------------------------------------
Writes a new CFS file to a UEF
-------------------------------------------------------------------------------}
function TDiscImage.WriteCFSFile(var file_details: TDirEntry;var buffer: TDIByteArray): Integer;
var
 i: Integer;
begin
 Result:=-5; //Unknown error
 if(Length(FDisc)=1)and(Length(buffer)>0)then //Make sure there is something
 begin
  //Make sure the filename is not beyond max length
  file_details.Filename:=LeftStr(file_details.Filename,10);
  //Increase the entries
  SetLength(FDisc[0].Entries,Length(FDisc[0].Entries)+1);
  //and increase the data array
  SetLength(CFSFiles,Length(CFSFiles)+1);
  //Return the new pointer
  Result:=Length(FDisc[0].Entries)-1;
  //Update the entry
  ResetDirEntry(FDisc[0].Entries[Result]);
  FDisc[0].Entries[Result]:=file_details; //Copy the entry across
  //Override some of the settings
  FDisc[0].Entries[Result].Filename:=FilenameToASCII(file_details.Filename);//Filename
  FDisc[0].Entries[Result].Sector  :=0;  //Where to find it (first block)
  FDisc[0].Entries[Result].Parent  :=FDisc[0].Directory;//Parent
  FDisc[0].Entries[Result].DirRef  :=-1;//Not a directory
  //Copy from the buffer into the data array
  SetLength(CFSFiles[Result],Length(buffer));
  for i:=0 to Length(buffer)-1 do CFSFiles[Result][i]:=buffer[i];
  inc(disc_size[0],Length(buffer));
 end;
 if Length(buffer)=0 then Result:=-8; //Nothing to write
end;

{-------------------------------------------------------------------------------
Renames a file in CFS
-------------------------------------------------------------------------------}
function TDiscImage.RenameCFSFile(entry: Cardinal;newfilename: String): Integer;
begin
 Result:=-1; //Failed to rename
 if Length(FDisc)=1 then //Check to make sure we have something
  if entry<Length(FDisc[0].Entries) then //And to make sure we're not overshooting
  begin
   //Simply just rename it
   FDisc[0].Entries[entry].Filename:=newfilename;
   //And return the entry number
   Result:=entry;
  end;
end;

{-------------------------------------------------------------------------------
Update a file's load or execution address
-------------------------------------------------------------------------------}
function TDiscImage.UpdateCFSFileAddr(entry,newaddr:Cardinal;load:Boolean):Boolean;
begin
 Result:=False;
 if Length(FDisc)=1 then //Check to make sure we have something
  if entry<Length(FDisc[0].Entries) then //And to make sure we're not overshooting
  begin
   //Simply just adjust it
   if load then FDisc[0].Entries[entry].LoadAddr:=newaddr AND$FFFFFFFF
           else FDisc[0].Entries[entry].ExecAddr:=newaddr AND$FFFFFFFF;
   //And return a positive result
   Result:=True;
  end;
end;
