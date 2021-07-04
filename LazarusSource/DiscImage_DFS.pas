//++++++++++++++++++ Acorn DFS +++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a DFS disc and which type
-------------------------------------------------------------------------------}
function TDiscImage.ID_DFS: Boolean;
var
 c,i    : Byte;
 t0,t1  : Integer;
 chk,dbl: Boolean;
 sec    :Cardinal;
begin
 if FFormat=diInvalidImg then
 begin
  ResetVariables;
  //Is there actually any data?
  if GetDataLength>0 then
  begin
   chk:=True;
   //Offset 0x0001 should have 9 bytes >31
   c:=0;
   for i:=0 to 8 do
    if(ReadByte($0001+i)>31)or(ReadByte($0001+i)=0)then inc(c);
   if c<>9 then chk:=False;
   //Offset 0x0100 should have 4 bytes >31
   c:=0;
   for i:=0 to 3 do
    if(ReadByte($0100+i)>31)or(ReadByte($0100+i)=0)then inc(c);
   if c<>4 then chk:=False;
   //Offset 0x0105 should have bits 0,1 and 2 clear (i.e. divisible by 8)
   if(ReadByte($0105)AND$7)<>0 then chk:=False;
   //Offset 0x0106 should have bits 2,3,6 and 7 clear
   if(ReadByte($0106)AND$CC)<>0 then chk:=False;
   //Above checks have passed
   if chk then
   begin
    dbl:=True; //Double sided flag
    //Check the entire first two sectors - if they are all zero assume ssd
    c:=0;
    for i:=0 to $FE do if ReadByte($0A00+i)=0 then inc(c);
    if(c=$FF)and(ReadByte($0AFF)=0)then dbl:=False;
    if dbl then
    begin
     for i:=0 to $FE do if ReadByte($B00+i)=0 then inc(c);
     if(c=$FF)and(ReadByte($0BFF)=0)then dbl:=False;
    end;
    //Offset 0x0A01 should have 9 bytes >31
    c:=0;
    for i:=0 to 8 do
     if(ReadByte($0A01+i)>31)or(ReadByte($0A01+i)=0)then inc(c);
    if c<>9 then dbl:=False;
    //Offset 0x0B00 should have 4 bytes >31
    c:=0;
    for i:=0 to 3 do
     if(ReadByte($0B00+i)>31)or(ReadByte($0B00+i)=0)then inc(c);
    if c<>4 then dbl:=False;
    //Offset 0x0B05 should have bits 0,1 and 2 clear
    if(ReadByte($0B05)AND$7)<>0 then dbl:=False;
    //Offset 0x0B06 should have bits 2,3,6 and 7 clear
    if(ReadByte($0B06)AND$CC)<>0 then dbl:=False;
    //Number of sectors, side 0
    t0:=ReadByte($0107)+((ReadByte($0106)AND$3)<<8);
    //DS tests passed, get the number of sectors, side 1
    if dbl then
     t1:=ReadByte($0B07)+((ReadByte($0B06)AND$3)<<8)
    else
     t1:=t0;
    //Not a double sided
    if t1=0 then
    begin
     //So mark as so
     dbl:=False;
     //This needs to be set to something other that 0, otherwise it'll fail to
     //ID as a DFS. Actually, DFS does accept zero length disc sizes, but
     //everything we have checked so far is for zeros.
     t1:=t0;
    end;
    FDSD:=dbl;
    if FDSD then
     FFormat:=diAcornDFS<<4+1
    else
     FFormat:=diAcornDFS<<4;
    //Number of sectors should be >0
    if((t0=0)or(t1=0))and(not FDFSzerosecs)then
    begin
     FFormat:=diInvalidImg;
     chk:=False;
    end;
    //Now we check the files. If the sector addresses are outside the disc, we fail
    if(chk)and(ReadByte($105)>>3>0)then //If there are any entries
    begin
     //Side 0
     if t0=0 then t0:=$320; //Assume 200K disc
     for i:=0 to (ReadByte($105)>>3)-1 do
     begin
      //Get the start sector
      sec:=(ReadByte($108+7+i*8)+((ReadByte($108+6+i*8)AND$3)<<8))<<8;
      //And add the length to it
      inc(sec,Read16b($108+4+i*8)+((ReadByte($108+6+i*8)AND$30)<<12));
      //If the end of the file is over the end of the disc, fail it
      if sec>t0<<8 then chk:=False;
     end;
     //Side 2
     if dbl then
     begin
      if t1=0 then t1:=$320; //Assume 200K disc
      for i:=0 to (ReadByte($B05)>>3)-1 do
      begin
       //Get the start sector
       sec:=(ReadByte($B08+7+i*8)+((ReadByte($B08+6+i*8)AND$3)<<8))<<8;
       //And add the length to it
       inc(sec,Read16b($B08+4+i*8)+((ReadByte($B08+6+i*8)AND$30)<<12));
       //If the end of the file is over the end of the disc, fail it
       if sec>t1<<8 then chk:=False;
      end;
     end;
     //If checks have failed, then reset the format
     if not chk then FFormat:=diInvalidImg;
    end;
   end;
   //Test for Watford DFS - we'll only test one side.
   if chk then
   begin
    //Offset 0x0200 should have 8 bytes of 0xAA
    c:=0;
    for i:=0 to 7 do
     if ReadByte($0200+i)=$AA then inc(c);
    //Offset 0x0300 should have 4 bytes of 0x00
    for i:=0 to 3 do
     if ReadByte($0300+i)=$00 then inc(c);
    //Disc size should match also
    if(c=12)and(Read16b($306)=Read16b($106))then
     if FFormat>>4=diAcornDFS then
      inc(FFormat,2);
   end;
  end;
 end;
 Result:=FFormat>>4=diAcornDFS;
end;

{-------------------------------------------------------------------------------
Converts a sector and side address into file offset address
-------------------------------------------------------------------------------}
function TDiscImage.ConvertDFSSector(address,side: Integer): Integer;
var
 sector,
 offset: Integer;
begin
 //For an single sided disc, this is just the address
 if not FDSD then
  Result:=address
 //Otherwise, needs a bit of jiggery pokery, as the sides are interleaved
 else
 begin
  sector:=address DIV $100; //Sectors are $100 in size, and we need to know the sector
  offset:=address MOD $100; //Offset within the sector
  //Annoyingly, it is the tracks which are interleaved, not the sectors.
  //On Acorn DFS discs, there are 10 sectors per track
  Result:=(((sector MOD 10)+(20*(sector DIV 10))+(10*side))*$100)+offset;
 end;
 //MMB
 if FFormat>>4=diMMFS then
 begin
  if(side<0)or(side>511)then side:=0;
  Result:=Result+side*$32000+$2000;
 end;
end;

{-------------------------------------------------------------------------------
Read Acorn DFS Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadDFSDisc(mmbdisc:Integer=-1): TDisc;
var
 s,t,c,f,
 locked,
 ptr,amt,
 diroff    : Integer;
 temp      : String;
begin
 Result:=nil;
 //Determine how many sides
 if (FFormat AND $1)=1 then //Double sided image
 begin
  SetLength(Result,2);
  SetLength(bootoption,2);
  SetLength(disc_size,2);
  SetLength(disc_name,2);
 end
 else                       //Single sided image
 begin
  SetLength(Result,1);
  SetLength(bootoption,1);
  SetLength(free_space,1);
  SetLength(disc_name,1);
 end;
 //Used by MMB. For DFS, this should be 0
 if(mmbdisc<0)or(mmbdisc>511)then mmbdisc:=0;
 s:=mmbdisc;
 repeat
  ResetDir(Result[s-mmbdisc]);
  //Number of entries on disc side
  t:=ReadByte(ConvertDFSSector($105,s)) div 8;
  if(FFormat mod$10>$1)and(FFormat mod$10<$4)then //Extra files on Watford DFS
   inc(t,ReadByte(ConvertDFSSector($305,s))div 8);
  SetLength(Result[s-mmbdisc].Entries,t);
  //Directory name - as DFS only has $, this will be the drive number + '$'
  Result[s-mmbdisc].Directory:=':'+IntToStr(s*2)+dir_sep+root_name;
  Result[s-mmbdisc].Partition:=s;
  //Get the disc title(s)
  Result[s-mmbdisc].Title:=ReadString(ConvertDFSSector($000,s),-8)
                          +ReadString(ConvertDFSSector($100,s),-4);
  RemoveSpaces(Result[s-mmbdisc].Title);
  RemoveControl(Result[s-mmbdisc].Title);
  disc_name[s]:=Result[s-mmbdisc].Title;
  //Boot Option
  if FFormat>>4=diAcornDFS then
   bootoption[s]:=(ReadByte(ConvertDFSSector($106,s))AND$30)>>4;
  //Disc Size
  disc_size[s]:=(ReadByte(ConvertDFSSector($107,s))
              +((ReadByte(ConvertDFSSector($106,s))AND$03)<<8))*$100;
  //Zero length disc size?
  if disc_size[s]=0 then disc_size[s]:=$32000;//Default size of 200K
  //Read the catalogue
  for f:=1 to t do
  begin
   //Reset the variables
   ResetDirEntry(Result[s-mmbdisc].Entries[f-1]);
   //Is it a Watford, and are we in the Watford area?
   diroff:=$000;
   ptr:=f;
   if(FFormat mod$10>$01)and(FFormat mod$10<$04)then
    if (f>31) then
    begin
     diroff:=$200;
     ptr:=f-31;
    end;
   //Read in the filename
   temp:='';
   for c:=0 to 6 do
   begin
    amt:=ReadByte(ConvertDFSSector(diroff+($08*ptr)+c,s))AND$7F;
    if amt>32 then temp:=temp+chr(amt);
   end;
   Result[s-mmbdisc].Entries[f-1].Filename:=temp;
   //Get the directory character
   temp:=chr(ReadByte(ConvertDFSSector(diroff+($08*ptr)+7,s))AND$7F);
   if temp=' 'then temp:=root_name; //Acorn Atom DOS root is ' '
   //If the directory is not root, add it to the filename
   if temp<>root_name then
    Result[s-mmbdisc].Entries[f-1].Filename:=temp+dir_sep
                                      +Result[s-mmbdisc].Entries[f-1].Filename;
   //Make up a parent directory pathname so this can be found
   Result[s-mmbdisc].Entries[f-1].Parent:=':'+IntToStr(s*2)+dir_sep+root_name;
   //Is it locked? This is actually the top bit of the final filename character
   locked:=(ReadByte(ConvertDFSSector(diroff+($08*ptr)+7,s))AND$80)>>7;
   if locked=1 then
    Result[s-mmbdisc].Entries[f-1].Attributes:='L'
   else
    Result[s-mmbdisc].Entries[f-1].Attributes:='';
   //Load address - need to multiply bits 16/17 by $55 to expand it to 8 bits
   Result[s-mmbdisc].Entries[f-1].LoadAddr:=
      (((ReadByte(ConvertDFSSector(diroff+$106+($08*ptr),s))AND$0C)<<14)*$55)
        +Read16b( ConvertDFSSector(diroff+$100+($08*ptr),s));
   //Execution address - need to multiply bits 16/17 by $55 to expand it to 8 bits
   Result[s-mmbdisc].Entries[f-1].ExecAddr:=
      (((ReadByte(ConvertDFSSector(diroff+$106+($08*ptr),s))AND$C0)<<10)*$55)
      +  Read16b( ConvertDFSSector(diroff+$102+($08*ptr),s));
   //Length
   Result[s-mmbdisc].Entries[f-1].Length:=
      (((ReadByte(ConvertDFSSector(diroff+$106+($08*ptr),s))AND$30)<<12))
      +  Read16b( ConvertDFSSector(diroff+$104+($08*ptr),s));
   //Sector of start of data
   Result[s-mmbdisc].Entries[f-1].Sector:=
       ((ReadByte(ConvertDFSSector(diroff+$106+($08*ptr),s))AND$03)<<8)
        +ReadByte(ConvertDFSSector(diroff+$107+($08*ptr),s));
   //Which side it is on
   Result[s-mmbdisc].Entries[f-1].Side:=s;
   //Not a directory - not used in DFS
   Result[s-mmbdisc].Entries[f-1].DirRef:=-1;
  end;
  //Next side
  if(FFormat AND $1=1)then inc(s) else s:=2+mmbdisc;
 until s=2+mmbdisc;
 //Free Space Map (not MMB)
 if FFormat>>4=diAcornDFS then DFSFreeSpaceMap(Result);
end;

{-------------------------------------------------------------------------------
Update the DFS Free Space Map, and update the free space counter
-------------------------------------------------------------------------------}
procedure TDiscImage.DFSFreeSpaceMap(LDisc: TDisc);
var
 f,s,c,e,fs: Cardinal;
begin
 //Set up the arrays
 if (FFormat AND $1)=1 then //Double sided image
 begin
  SetLength(free_space_map,2);
  SetLength(free_space,2);
 end
 else                       //Single sided image
 begin
  SetLength(free_space_map,1);
  SetLength(free_space,1);
 end;
 for s:=0 to Length(free_space_map)-1 do
 begin
  //Directory size
  free_space[s]:=$200;
  if FFormat mod $10>1 then inc(free_space[s],$200); //Watford DFS
  //Free Space Map
  SetLength(free_space_map[s],disc_size[s]DIV$A00); //Number of tracks
  for f:=0 to Length(free_space_map[s])-1 do
  begin
   SetLength(free_space_map[s,f],10); //Number of sectors per track
   for c:=0 to 9 do
    free_space_map[s,f,c]:=$00;
  end;
  //First two sectors are used
  free_space_map[s,0,0]:=$FE;
  free_space_map[s,0,1]:=$FE;
  if FFormat mod $10>1 then //Watford DFS
  begin
   free_space_map[s,0,2]:=$FE;
   free_space_map[s,0,3]:=$FE;
  end;
  if Length(LDisc[s].Entries)>0 then
   for e:=0 to Length(LDisc[s].Entries)-1 do
   begin
    inc(free_space[s],(LDisc[s].Entries[e].Length div $100)*$100);
    if LDisc[s].Entries[e].Length mod $100>0 then inc(free_space[s],$100);
    //Add it to the free space map
    c:=LDisc[s].Entries[e].Length div $100;
    if LDisc[s].Entries[e].Length mod $100>0 then inc(c);
    if c>0 then //Take care of zero length files
     for fs:=0 to c-1 do
      if(LDisc[s].Entries[e].Sector+fs)div 10<Length(free_space_map[s])then
       if(LDisc[s].Entries[e].Sector+fs) mod 10<Length(free_space_map[s,(LDisc[s].Entries[e].Sector+fs) div 10]) then
        free_space_map[s,(LDisc[s].Entries[e].Sector+fs) div 10,
                         (LDisc[s].Entries[e].Sector+fs) mod 10]:=$FF;
   end;
  free_space[s]:=disc_size[s]-free_space[s];
 end;
end;

{-------------------------------------------------------------------------------
Write Acorn DFS File
-------------------------------------------------------------------------------}
function TDiscImage.WriteDFSFile(file_details: TDirEntry;var buffer: TDIByteArray): Integer;
var
 f      : Byte;
 i,l,
 pos,
 count,
 newlen,
 filen,
 size1,
 size2  : Integer;
 ptr    : Cardinal;
 success: Boolean;
begin
 Result:=-3; //File already exists
 count:=file_details.Length;
 f:=FFormat MOD $10; //Minor format (sub format)
 //Ensure that Side is not beyond the array
 file_details.Side:=file_details.Side MOD 2;
 if file_details.Side>Length(FDisc)-1 then file_details.Side:=0;
 //Overwrite the parent
 file_details.Parent:=':'+IntToStr(file_details.Side*2)+dir_sep+root_name;
 //Check that the filename is valid
 file_details.Filename:=ValidateDFSFilename(file_details.Filename);
 //Make sure the file does not already exist
 if not(FileExists(file_details.Parent+dir_sep+file_details.Filename,ptr))then
 begin
  Result:=-4;//Catalogue full
  //Can the catalogue be extended?
  l:=Length(FDisc[file_details.Side].Entries);
  if ((l<31) and (f<2))         // Max 31 entries for Acorn DFS
  or ((l<62) and (f>1)) then    // and 62 entries for Watford DFS
  begin
   //Extend the catalogue by 1
   SetLength(FDisc[file_details.Side].Entries,l+1);
   Inc(l);
   filen:=0; //File 0 means no space, so add at the beginning
   size2:=count div $100;//Size, up to the next boundary, of the file being inserted
   if count mod $100>0 then inc(size2);
   if(l>1)then //Not the first entry?
   begin
    //Find if there is space inside the catalogue to insert the file
    for i:=l-2 downto 1 do
    begin
     //Size, up to the next boundary, of the existing file
     size1:=FDisc[file_details.Side].Entries[i].Length div $100;
     if FDisc[file_details.Side].Entries[i].Length mod $100>0 then inc(size1);
     //Check to see if it will fit above this file
     if FDisc[file_details.Side].Entries[i].Sector+size1+size2
                    <=FDisc[file_details.Side].Entries[i-1].Sector then filen:=i;
    end;
    //Move everything above this down by 1
    for i:=l-1 downto filen+1 do
     FDisc[file_details.Side].Entries[i]:=FDisc[file_details.Side].Entries[i-1];
    //Find the next available sector, from the previous entry in the catalogue
    pos:=FDisc[file_details.Side].Entries[filen+1].Length div $100;
    if FDisc[file_details.Side].Entries[filen+1].Length mod $100>0 then inc(pos);
    pos:=pos+FDisc[file_details.Side].Entries[filen+1].Sector;
   end
   else
   begin //First sector for the data, if first entry
    if f<2 then pos:=2; //Acorn DFS is sector 2
    if f>1 then pos:=4; //Watford DFS is sector 4
   end;
   //Add the entry at the insert point
   FDisc[file_details.Side].Entries[filen]:=file_details;
   //and update the entry we're writing to point to this sector
   FDisc[file_details.Side].Entries[filen].Sector:=pos;
   //Extend the image size, if necessary (image size != data size)
   newlen:=size2*$100;
   if ConvertDFSSector(pos*$100+newlen,file_details.Side)>GetDataLength then
    SetDataLength(ConvertDFSSector(pos*$100+newlen,file_details.Side));
   //Then write the actual data
   success:=WriteDiscData(pos*$100,file_details.Side,buffer,count);
   //Update the catalogue, if successful
   if success then
   begin
    //Update the catalogue
    UpdateDFSCat(file_details.Side);
    //Update the free space
    DFSFreeSpaceMap(FDisc);
    //Pointer to where it was inserted
    Result:=filen;
   end
   //or revert back if not
   else
   begin
    Result:=-5; //Unknown error
    if l>1 then
     for i:=filen to l-2 do
      FDisc[file_details.Side].Entries[i]:=FDisc[file_details.Side].Entries[i+1];
    SetLength(FDisc[file_details.Side].Entries,l-1);
   end;
   //The data written will get overwritten anyway if failed.
  end;
 end;
end;

{-------------------------------------------------------------------------------
Validate a filename
-------------------------------------------------------------------------------}
function TDiscImage.ValidateDFSFilename(filename: String): String;
var
 i: Integer;
const
  illegal = '#*:';
begin
  for i:=1 to Length(filename) do
 begin
  //Remove top-bit set characters
  filename[i]:=chr(ord(filename[i]) AND $7F);
  //and remove control codes
  if ord(filename[i])<32 then
   filename[i]:=chr(ord(filename[i])+32);
 end;
 //Ensure that the root has not been included
 if  (filename[1]=root_name)
 and (filename[2]=dir_sep) then
  filename:=Copy(filename,3,Length(filename));
 //Is it not too long, including any directory specifier?
 if (filename[2]=dir_sep) then
  filename:=Copy(filename,1,9)
 else
  filename:=Copy(filename,1,7);
 //Remove any forbidden characters
 for i:=1 to Length(filename) do
  if Pos(filename[i],illegal)>0 then filename[i]:='_';
 Result:=filename;
end;

{-------------------------------------------------------------------------------
Update the catalogue
-------------------------------------------------------------------------------}
procedure TDiscImage.UpdateDFSCat(side: Integer);
var
 i,s,c : Integer;
 fn,dn : String;
 t,f   : Byte;
begin
 f:=FFormat mod $10;//Subformat
 //Update the number of catalogue entries
 c:=Length(FDisc[side].Entries);
 if c<32 then
  WriteByte(c*8,ConvertDFSSector($105,side));
 if f>1 then //Extra files on Watford DFS
  if c>31 then
   begin
    WriteByte( 31*8,   ConvertDFSSector($105,side));
    WriteByte((c-31)*8,ConvertDFSSector($305,side));
   end;
 //Update the entries
 for i:=0 to Length(FDisc[side].Entries)-1 do
 begin
  //Catalogue sector
  s:=$000; //Acorn DFS
  c:=i;
  if (f>1) and (i>30) then s:=$200; //Watford DFS
  if s=$200 then c:=i-31;
  //Filename
  fn:=FDisc[side].Entries[i].Filename;
  //Directory specifier
  dn:=root_name; //Default will be root
  //Is there a directory specifier in the filename?
  if fn[2]=dir_sep then
  begin
   //Yes update the specifier
   dn:=fn[1];
   //and shorten the filename
   fn:=Copy(fn,3,Length(fn));
  end;
  //Now write the filename into the image
  for t:=0 to 6 do
   if t<Length(fn) then
    WriteByte(ord(fn[t+1]),ConvertDFSSector(s+t+$08*(c+1),side))
   else //Pad with spaces
    WriteByte($20,         ConvertDFSSector(s+t+$08*(c+1),side));
  //Directory specifier
  t:=Ord(dn[1]);
  //Attribute
  if Pos('L',FDisc[side].Entries[i].Attributes)>0 then
   t:=t OR $80;
  //Write the directory specifier and attribute together
  WriteByte(t,               ConvertDFSSector(s+7+$08*(c+1),side));
  //Load address
  Write16b(FDisc[side].Entries[i].LoadAddr and $FFFF,
                             ConvertDFSSector(s+$100+$08*(c+1),side));
  //Execution address
  Write16b(FDisc[side].Entries[i].ExecAddr and $FFFF,
                             ConvertDFSSector(s+$102+$08*(c+1),side));
  //Length
  Write16b(FDisc[side].Entries[i].Length   and $FFFF,
                             ConvertDFSSector(s+$104+$08*(c+1),side));
  //Start Sector
  WriteByte(FDisc[side].Entries[i].Sector  and $FF,
                             ConvertDFSSector(s+$107+$08*(c+1),side));
  //Extra bits for Load,Execution,Length and Start Sector
  t:=((Integer(FDisc[side].Entries[i].Sector)  and   $300)>> 8) //bits 0,1
   OR((Integer(FDisc[side].Entries[i].LoadAddr)and $30000)>>14) //bits 2,3
   OR((Integer(FDisc[side].Entries[i].Length  )and $30000)>>12) //bits 4,5
   OR((Integer(FDisc[side].Entries[i].ExecAddr)and $30000)>>10);//bits 6,7
  WriteByte(t,               ConvertDFSSector(s+$106+$08*(c+1),side));
 end;
end;

{-------------------------------------------------------------------------------
Rename Acorn DFS File
-------------------------------------------------------------------------------}
function TDiscImage.RenameDFSFile(oldfilename: String;var newfilename: String):Integer;
var
 ptr,entry,dir: Cardinal;
begin
 Result:=-2;//File does not exist
 //Check that the new name meets the required DFS filename specs
 newfilename:=ValidateDFSFilename(newfilename);
 //Check that the file exists
 if FileExists(oldfilename,ptr) then
 begin                                    
  Result:=-3;//Destination already exists
  //FileExists returns a pointer to the file
  entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=ptr div $10000;  //Top 16 bits - directory reference
  //Make sure the new filename does not already exist
  if(not FileExists(FDisc[dir].Entries[entry].Parent+dirsep+newfilename,ptr))
  // or the user is just changing case
  or(LowerCase(FDisc[dir].Entries[entry].Parent+dirsep+newfilename)=LowerCase(oldfilename))then
  begin
   //Change the entry
   FDisc[dir].Entries[entry].Filename:=newfilename;
   //Update the catalogue
   UpdateDFSCat(dir);
   Result:=entry;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Delete Acorn DFS File
-------------------------------------------------------------------------------}
function TDiscImage.DeleteDFSFile(filename: String):Boolean;
var
 ptr,entry,dir: Cardinal;
 i: Integer;
begin
 Result:=False;
 //Check that the file exists
 if FileExists(filename,ptr) then
 begin
  //FileExists returns a pointer to the file
  entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=ptr div $10000;  //Top 16 bits - directory reference
  //Remove the filename from the entries by moving the entries below up one
  for i:=entry+1 to Length(FDisc[dir].Entries)-1 do
   FDisc[dir].Entries[i-1]:=FDisc[dir].Entries[i];
  //Reduce the number of entries by 1
  SetLength(FDisc[dir].Entries,Length(FDisc[dir].Entries)-1);
  //Update the catalogue
  UpdateDFSCat(dir);
  //Update the free space
  DFSFreeSpaceMap(FDisc);
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Update DFS File attributes
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDFSFileAttributes(filename,attributes: String): Boolean;
var
 ptr,
 entry,
 dir   : Cardinal;
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
  UpdateDFSCat(dir);
  //And return a success
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Create DFS blank image
-------------------------------------------------------------------------------}
function TDiscImage.FormatDFS(minor,tracks: Byte): TDisc;
var
 s: Byte;
 t: Integer;
 side_size: Cardinal;
begin
 Result:=nil;
 //Blank everything
 ResetVariables;
 //Set the format
 FFormat:=diAcornDFS<<4+minor;
 //Set the filename
 imagefilename:='Untitled.'+FormatExt;
 //How many sides?
 if (FFormat AND $1)=1 then //Double sided image
 begin
  SetLength(Result,2);
  FDSD:=True;
  SetLength(bootoption,2);
  SetLength(disc_size,2);
  disc_size[1]:=0;
  SetLength(free_space,2);
  free_space[1]:=0;
  SetLength(disc_name,2);
 end
 else                       //Single sided image
 begin
  SetLength(Result,1);
  FDSD:=False;
  SetLength(bootoption,1);
  SetLength(disc_size,1);
  SetLength(free_space,1);
  SetLength(disc_name,1);
 end;
 //Setup the data area
 SetDataLength($200*(minor+1)); // $200 for the header, per side. $400 for Watford
 //Fill with zeros
 for t:=0 to GetDataLength-1 do WriteByte(0,t);
 s:=0;
 repeat
  //Reset the array
  ResetDir(Result[s]);
  //Number of entries on disc side
  SetLength(Result[s].Entries,0);
  //Directory name - as DFS only has $, this will be the drive number + '$'
  Result[s].Directory:=':'+IntToStr(s*2)+dir_sep+root_name;
  //Get the disc title(s)
  Result[s].Title:=disctitle;
  disc_name[s]:=Result[s].Title;
  //Disc Size
  side_size:=0;
  if tracks=0 then side_size:=$190; //40T
  if tracks=1 then side_size:=$320; //80T
  //Initialise the disc
  WriteByte(side_size div $100,ConvertDFSSector($106,s));
  WriteByte(side_size mod $100,ConvertDFSSector($107,s));
  inc(disc_size[s],side_size*$100);
  //Disc Title
  UpdateDFSDiscTitle(disctitle,tracks);
  //Directory size
  inc(free_space[s],$200);
  //Next side
  if (FFormat AND $1=1) then inc(s) else s:=2;
 until s=2;
 //Update the free space
 DFSFreeSpaceMap(Result);
end;

{-------------------------------------------------------------------------------
Set the DFS disc title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDFSDiscTitle(title: String;side: Byte): Boolean;
var
 a  : Cardinal;
 b,c: Byte;
begin
 Result:=False;
 if Length(FDisc)>0 then
 begin
  //Set the DFS root directory title
  FDisc[side].Title:=title;
  //Set the disc_name for both sides
  if Length(FDisc)>1 then
   disc_name[1]:=FDisc[1].Title;
  disc_name[0]:=FDisc[0].Title;
 end;
 //Update the data
 for c:=0 to 11 do
 begin
  a:=$000;               //First 8 characters
  if c>7 then a:=$100-8; //Last 4 characters
  b:=32;                 //Pad with spaces
  if c<Length(title) then b:=Ord(title[c+1])AND$7F; //Chr, no top bit set
  if b<32 then b:=32;    //Ensure no control characters
  WriteByte(b,ConvertDFSSector(a+c,side)); //Write it
 end;
 Result:=True;
end;

{-------------------------------------------------------------------------------
Set the DFS boot option
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDFSBootOption(option,side: Byte): Boolean;
var
 b: Byte;
begin
 bootoption[side]:=option AND $3;
 b:=ReadByte(ConvertDFSSector($106,side));
 b:=(b AND $CF) OR ((option AND $3)shl 4);
 WriteByte(b,ConvertDFSSector($106,side));
 Result:=True;
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path
-------------------------------------------------------------------------------}
function TDiscImage.ExtractDFSFile(filename: String;
                                             var buffer: TDIByteArray): Boolean;
var
 source,side   : Integer;
 entry,dir,
 fragptr,
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
  //Work out where it is coming from
  source:=FDisc[dir].Entries[entry].Sector*$100;
  side:=FDisc[dir].Entries[entry].Side;
  //Read the data into the buffer
  if filelen>0 then
   ReadDiscData(source,filelen,side,buffer[0]);
  //Return positive
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Update a file's load or execution address
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDFSFileAddr(filename:String;newaddr:Cardinal;load:Boolean):Boolean;
var
 ptr,
 dir,
 entry: Cardinal;
begin
 Result:=False;
 ptr:=0;
 //Ensure the file actually exists
 if FileExists(filename,ptr) then
 begin
  //Extract the references
  dir  :=ptr DIV $10000;
  entry:=ptr MOD $10000;
  //Are they valid?
  if dir<Length(FDisc)then
   if entry<Length(FDisc[dir].Entries)then
   begin
    //Update our entry
    if load then FDisc[dir].Entries[entry].LoadAddr:=newaddr AND$FFFFFF
            else FDisc[dir].Entries[entry].ExecAddr:=newaddr AND$FFFFFF;
    //And update the catalogue for the parent directory
    UpdateDFSCat(FDisc[dir].Entries[entry].Side);
    //Return a positive result
    Result:=True;
   end;
 end;
end;
