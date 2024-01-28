//++++++++++++++++++ Acorn DFS +++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a DFS disc and which type
-------------------------------------------------------------------------------}
function TDiscImage.ID_DFS: Boolean;
var
 c,i    : Byte;
 t0,t1  : Integer;
 chk    : Boolean;
 sec    : Cardinal;
 temp   : String;
begin
 if FFormat=diInvalidImg then
 begin
  ResetVariables;
  //Is there actually any data?
  if(GetDataLength>0)and(GetDataLength<=400*1024)then
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
    FDSD:=True; //Double sided flag
    if not FDFSZeroSecs then
    begin
     //Check the entire first two sectors - if they are all zero assume ssd
     c:=0;
     for i:=0 to $FE do if ReadByte($0A00+i)=0 then inc(c);
     if(c=$FF)and(ReadByte($0AFF)=0)then FDSD:=False;
     if FDSD then
     begin
      for i:=0 to $FE do if ReadByte($B00+i)=0 then inc(c);
      if(c=$FF)and(ReadByte($0BFF)=0)then FDSD:=False;
     end;
    end;
    //Offset 0x0A01 should have 9 bytes >31
    c:=0;
    for i:=0 to 8 do
     if(ReadByte($0A01+i)>31)or(ReadByte($0A01+i)=0)then inc(c);
    if c<>9 then FDSD:=False;
    //Offset 0x0B00 should have 4 bytes >31
    c:=0;
    for i:=0 to 3 do
     if(ReadByte($0B00+i)>31)or(ReadByte($0B00+i)=0)then inc(c);
    if c<>4 then FDSD:=False;
    //Offset 0x0B05 should have bits 0,1 and 2 clear
    if(ReadByte($0B05)AND$7)<>0 then FDSD:=False;
    //Offset 0x0B06 should have bits 2,3,6 and 7 clear
    if(ReadByte($0B06)AND$CC)<>0 then FDSD:=False;
    //Number of sectors, side 0
    t0:=ReadByte($0107)+((ReadByte($0106)AND$3)<<8);
    //DS tests passed, get the number of sectors, side 1
    if FDSD then
     t1:=ReadByte($0B07)+((ReadByte($0B06)AND$3)<<8)
    else
     t1:=t0;
    //Not a double sided
    if(t1=0)and(not FDFSzerosecs)then
    begin
     //So mark as so
     FDSD:=False;
     //This needs to be set to something other that 0, otherwise it'll fail to
     //ID as a DFS. Actually, DFS does accept zero length disc sizes, but
     //everything we have checked so far is for zeros.
     t1:=t0;
    end;
    //Zero number of sectors, and we're allowing these, so let's look at the extension
    if(t1=0)and(FDFSzerosecs)then //If it is '.SSD' then it is single sided
     if UpperCase(RightStr(FFilename,4))='.SSD' then FDSD:=False;
    //Set the initial format
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
    //We'll also check for blank filenames too
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
      if(sec>t0<<8)and(not FDFSBeyondEdge)then chk:=False;
      if(sec>t0<<8)and(FDFSBeyondEdge)    then t0:=((sec+$FF)>>8); //Or fix it
      //Check for blank filenames - no longer required
      temp:=ReadString($008+(i*8),-7);
      RemoveTopBit(temp); //Attributes are in the top bit
      RemoveSpaces(temp); //Remove extraneous spaces
      if(temp='')and(not FDFSAllowBlank)then chk:=False;
      //Fixes the sector number, if needed
      WriteByte(t0 AND$FF,$107);
      WriteByte((t0>>8AND$3)OR(ReadByte($106)AND$FC),$106);
     end;
     //Side 2
     if(FDSD)and(ReadByte($B05)>>3>0)then
     begin
      if t1=0 then t1:=$320; //Assume 200K disc
      for i:=0 to (ReadByte($B05)>>3)-1 do
      begin
       //Get the start sector
       sec:=(ReadByte($B08+7+i*8)+((ReadByte($B08+6+i*8)AND$3)<<8))<<8;
       //And add the length to it
       inc(sec,Read16b($B08+4+i*8)+((ReadByte($B08+6+i*8)AND$30)<<12));
       //If the end of the file is over the end of the disc, fail it as a double
       if(sec>t1<<8)and(not FDFSBeyondEdge)then FDSD:=False;
       if(sec>t1<<8)and(FDFSBeyondEdge)    then sec:=(t1+$FF)>>8; //Or fix it
       //Check for blank filenames
       if not FDFSAllowBlank then
       begin
        temp:=ReadString($A08+(i*8),-7);
        RemoveTopBit(temp); //Attributes are in the top bit
        RemoveSpaces(temp); //Remove extraneous spaces
        if temp='' then FDSD:=False;
       end;
      end;
      WriteByte(t1 AND$FF,$A07); //Temporary fix
      WriteByte((t1>>8AND$3)OR(ReadByte($A06)AND$FC),$A06); //Temporary fix
     end;
     //If checks have failed, then reset the format
     if not chk then FFormat:=diInvalidImg;
    end;
   end;
   //Test for Watford DFS
   if chk then
   begin
    t0:=0;//Side 1 - by default Acorn
    t1:=0;//Side 2 - by default Acorn
    //First we check side 1
    //Offset 0x0200 should have 8 bytes of 0xAA
    c:=0;
    for i:=0 to 7 do
     if ReadByte($0200+i)=$AA then inc(c);
    //Offset 0x0300 should have 4 bytes of 0x00
    for i:=0 to 3 do
     if ReadByte($0300+i)=$00 then inc(c);
    if c=12 then
     if GetMajorFormatNumber=diAcornDFS then
      t0:=1;//Set side 1 to Watford
    //Now we check side 2
    if FDSD then
    begin
     //Offset 0x0C00 should have 8 bytes of 0xAA
     c:=0;
     for i:=0 to 7 do
      if ReadByte($0C00+i)=$AA then inc(c);
     //Offset 0x0D00 should have 4 bytes of 0x00
     for i:=0 to 3 do
      if ReadByte($0D00+i)=$00 then inc(c);
     if c=12 then
      if GetMajorFormatNumber=diAcornDFS then
       t1:=1;//Set side 1 to Watford
    end;
    //Determine the format
    if not FDSD then //Single sided
    begin
     if t0=0 then FFormat:=diAcornDFS<<4+0; //Acorn SSD
     if t0=1 then FFormat:=diAcornDFS<<4+2; //Watford SSD
    end
    else            //Double sided
    begin
     if t0=0 then
     begin
      if t1=0 then FFormat:=diAcornDFS<<4+1; //Acorn DSD
      if t1=1 then FFormat:=diAcornDFS<<4+5; //Acorn/Watford DSD
     end;
     if t0=1 then
     begin
      if t1=0 then FFormat:=diAcornDFS<<4+7; //Watford/Acorn DSD
      if t1=1 then FFormat:=diAcornDFS<<4+3; //Watford DSD
     end;
    end;
    //Set the Double Sided flag
    //FDSD:=dbl;
   end;
  end;
 end;
 Result:=GetMajorFormatNumber=diAcornDFS;
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
 if GetMajorFormatNumber=diMMFS then
 begin
  if(side<0)or(side>511)then side:=0;
  Result:=Result+side*$32000+$2000;
 end;
end;

{-------------------------------------------------------------------------------
Read Acorn DFS Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadDFSDisc(mmbdisc:Integer=-1): Boolean;
var
 s,t,f,
 locked,
 ptr,
 diroff    : Integer;
 temp      : String;
begin
 Result:=False;
 FDisc:=nil;
 //Determine how many sides
 if FDSD then //Double sided image
 begin
  SetLength(FDisc,2);
  SetLength(bootoption,2);
  SetLength(disc_size,2);
  SetLength(disc_name,2);
  //SetLength(FPartitions,2);
 end
 else                       //Single sided image
 begin
  SetLength(FDisc,1);
  SetLength(bootoption,1);
  SetLength(free_space,1);
  SetLength(disc_name,1);
  //SetLength(FPartitions,1);
 end;
 //Used by MMB. For DFS, this should be 0
 if(mmbdisc<0)or(mmbdisc>511)then mmbdisc:=0;
 s:=mmbdisc;
 repeat
  ResetDir(FDisc[s-mmbdisc]);
  //Number of entries on disc side
  t:=ReadByte(ConvertDFSSector($105,s)) div 8;
  if(GetMinorFormatNumber>$1)and(GetMinorFormatNumber<$4)then //Extra files on Watford DFS
   inc(t,ReadByte(ConvertDFSSector($305,s))div 8);
  SetLength(FDisc[s-mmbdisc].Entries,t);
  //Directory name - as DFS only has $, this will be the drive number + '$'
  FDisc[s-mmbdisc].Directory:=':'+IntToStr(s*2)+dir_sep+root_name;
  FDisc[s-mmbdisc].Partition:=s;
  FDisc[s-mmbdisc].BeenRead :=True;
  //Get the disc title(s)
  FDisc[s-mmbdisc].Title:=ReadString(ConvertDFSSector($000,s),-8)
                          +ReadString(ConvertDFSSector($100,s),-4);
  RemoveSpaces(FDisc[s-mmbdisc].Title);
  RemoveControl(FDisc[s-mmbdisc].Title);
  disc_name[s]:=FDisc[s-mmbdisc].Title;
  //Boot Option
  if GetMajorFormatNumber=diAcornDFS then
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
   ResetDirEntry(FDisc[s-mmbdisc].Entries[f-1]);
   //Is it a Watford, and are we in the Watford area?
   diroff:=$000;
   ptr:=f;
   if(GetMinorFormatNumber=2)or(GetMinorFormatNumber=3)then
    if (f>31) then
    begin
     diroff:=$200;
     ptr:=f-31;
    end;
   //Read in the filename
   temp:=ReadString(ConvertDFSSector(diroff+($08*ptr),s),-7);
   RemoveTopBit(temp); //Attributes are in the top bit
   RemoveSpaces(temp); //Remove extraneous spaces
   FDisc[s-mmbdisc].Entries[f-1].Filename:=temp;
   //Get the directory character
   temp:=chr(ReadByte(ConvertDFSSector(diroff+($08*ptr)+7,s))AND$7F);
   if temp=' 'then temp:=root_name; //Acorn Atom DOS root is ' '
   //If the directory is not root, add it to the filename
   if temp<>root_name then
    FDisc[s-mmbdisc].Entries[f-1].Filename:=temp+dir_sep
                                      +FDisc[s-mmbdisc].Entries[f-1].Filename;
   //Make up a parent directory pathname so this can be found
   FDisc[s-mmbdisc].Entries[f-1].Parent:=':'+IntToStr(s*2)+dir_sep+root_name;
   //Is it locked? This is actually the top bit of the final filename character
   locked:=(ReadByte(ConvertDFSSector(diroff+($08*ptr)+7,s))AND$80)>>7;
   if locked=1 then
    FDisc[s-mmbdisc].Entries[f-1].Attributes:='L'
   else
    FDisc[s-mmbdisc].Entries[f-1].Attributes:='';
   //Load address - need to multiply bits 16/17 by $55 to expand it to 8 bits
   FDisc[s-mmbdisc].Entries[f-1].LoadAddr:=
      (((ReadByte(ConvertDFSSector(diroff+$106+($08*ptr),s))AND$0C)<<14)*$55)
        +Read16b( ConvertDFSSector(diroff+$100+($08*ptr),s));
   //Execution address - need to multiply bits 16/17 by $55 to expand it to 8 bits
   FDisc[s-mmbdisc].Entries[f-1].ExecAddr:=
      (((ReadByte(ConvertDFSSector(diroff+$106+($08*ptr),s))AND$C0)<<10)*$55)
      +  Read16b( ConvertDFSSector(diroff+$102+($08*ptr),s));
   //Length
   FDisc[s-mmbdisc].Entries[f-1].Length:=
      (((ReadByte(ConvertDFSSector(diroff+$106+($08*ptr),s))AND$30)<<12))
      +  Read16b( ConvertDFSSector(diroff+$104+($08*ptr),s));
   //Sector of start of data
   FDisc[s-mmbdisc].Entries[f-1].Sector:=
       ((ReadByte(ConvertDFSSector(diroff+$106+($08*ptr),s))AND$03)<<8)
        +ReadByte(ConvertDFSSector(diroff+$107+($08*ptr),s));
   //Which side it is on
   FDisc[s-mmbdisc].Entries[f-1].Side:=s;
   //Not a directory - not used in DFS
   FDisc[s-mmbdisc].Entries[f-1].DirRef:=-1;
  end;
  //Next side
  if(FFormat AND $1=1)then inc(s) else s:=2+mmbdisc;
  {FPartitions[s-mmbdisc].Directories:=Result;
  FPartitions[s-mmbdisc].DirSep:=dir_sep;
  FPartitions[s-mmbdisc].Format:=diAcornDFS;
  FPartitions[s-mmbdisc].Title:=disc_name[s];
  FPartitions[s-mmbdisc].RootName:=root_name;}
 until s=2+mmbdisc;
 //Free Space Map (not MMB)
 if GetMajorFormatNumber=diAcornDFS then DFSFreeSpaceMap;
 Result:=Length(FDisc)>0;
end;

{-------------------------------------------------------------------------------
Update the DFS Free Space Map, and update the free space counter
-------------------------------------------------------------------------------}
procedure TDiscImage.DFSFreeSpaceMap;
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
  if IsWatford(s) then inc(free_space[s],$200); //Watford DFS
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
  if IsWatford(s) then //Watford DFS
  begin
   free_space_map[s,0,2]:=$FE;
   free_space_map[s,0,3]:=$FE;
  end;
  if Length(FDisc[s].Entries)>0 then
   for e:=0 to Length(FDisc[s].Entries)-1 do
   begin
    inc(free_space[s],(FDisc[s].Entries[e].Length div $100)*$100);
    if FDisc[s].Entries[e].Length mod $100>0 then inc(free_space[s],$100);
    //Add it to the free space map
    c:=FDisc[s].Entries[e].Length div $100;
    if FDisc[s].Entries[e].Length mod $100>0 then inc(c);
    if c>0 then //Take care of zero length files
     for fs:=0 to c-1 do
      if(FDisc[s].Entries[e].Sector+fs)div 10<Length(free_space_map[s])then
       if(FDisc[s].Entries[e].Sector+fs)mod 10<Length(free_space_map[s,
                                         (FDisc[s].Entries[e].Sector+fs)div 10])
                                                                            then
        free_space_map[s,(FDisc[s].Entries[e].Sector+fs) div 10,
                         (FDisc[s].Entries[e].Sector+fs) mod 10]:=$FF;
   end;
  free_space[s]:=disc_size[s]-free_space[s];
 end;
end;

{-------------------------------------------------------------------------------
Is it a Watford side?
-------------------------------------------------------------------------------}
function TDiscImage.IsWatford(s: Integer): Boolean;
begin
 Result:=False;
 //This side a Watford DFS?
 if (GetMinorFormatNumber=2)
 or (GetMinorFormatNumber=3)
 or((GetMinorFormatNumber=5)and(s=1))
 or((GetMinorFormatNumber=7)and(s=0))then Result:=True;
end;

{-------------------------------------------------------------------------------
Write Acorn DFS File
-------------------------------------------------------------------------------}
function TDiscImage.WriteDFSFile(var file_details: TDirEntry;var buffer: TDIByteArray): Integer;
var
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
  if((l<31)and(not IsWatford(file_details.Side))) // Max 31 entries for Acorn DFS
  or((l<62)and(IsWatford(file_details.Side)))then // and 62 entries for Watford DFS
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
    if not IsWatford(file_details.Side) then pos:=2; //Acorn DFS is sector 2
    if IsWatford(file_details.Side) then pos:=4; //Watford DFS is sector 4
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
    DFSFreeSpaceMap;
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
 t     : Byte;
begin
 //Update the number of catalogue entries
 c:=Length(FDisc[side].Entries);
 if c<32 then
  WriteByte(c*8,ConvertDFSSector($105,side));
 if IsWatford(side) then //Extra files on Watford DFS
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
  if(IsWatford(side))and(i>30)then s:=$200; //Watford DFS
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
  WriteString(fn,ConvertDFSSector(s+$08*(c+1),side),7,32);
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
  if(not FileExists(GetParent(dir)+dir_sep+newfilename,ptr))
  // or the user is just changing case
  or(LowerCase(GetParent(dir)+dir_sep+newfilename)=LowerCase(oldfilename))then
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
  DFSFreeSpaceMap;
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Update DFS File attributes
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDFSFileAttributes(filename,attributes: String): Boolean;
var
 entry,
 dir   : Cardinal;
begin
 Result:=False;
 //Make sure that the file exists, but also to get the pointer
 if FileExists(filename,dir,entry) then
 begin
  if FDisc[dir].Entries[entry].DirRef<>-1 then attributes:=attributes+'D'
  else attributes:=ReplaceStr(attributes,'D','');
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
function TDiscImage.FormatDFS(minor,tracks: Byte): Boolean;
var
 s: Byte;
 t: Integer;
 side_size: Cardinal;
begin
 FDisc:=nil;
 //Blank everything
 ResetVariables;
 //Set the format
 FFormat:=diAcornDFS<<4+minor;
 //Set the filename
 imagefilename:='Untitled.'+FormatExt;
 //How many sides?
 if (FFormat AND $1)=1 then //Double sided image
 begin
  SetLength(FDisc,2);
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
  SetLength(FDisc,1);
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
  ResetDir(FDisc[s]);
  //Number of entries on disc side
  SetLength(FDisc[s].Entries,0);
  //Directory name - as DFS only has $, this will be the drive number + '$'
  FDisc[s].Directory:=':'+IntToStr(s*2)+dir_sep+root_name;
  //Get the disc title(s)
  FDisc[s].Title:=disctitle;
  disc_name[s]:=FDisc[s].Title;
  FDisc[s].BeenRead:=True;
  //Disc Size
  side_size:=0;
  if tracks=0 then side_size:=$190; //40T
  if tracks=1 then side_size:=$320; //80T
  //Initialise the disc
  WriteByte(side_size div $100,ConvertDFSSector($106,s));
  WriteByte(side_size mod $100,ConvertDFSSector($107,s));
  inc(disc_size[s],side_size*$100);
  //Increase the data length, if needed
  if FDSD then
   SetDataLength(disc_size[0]+disc_size[1]);
  //Disc Title
  UpdateDFSDiscTitle(disctitle,s);
  //Watford ID
  if minor>1 then
  begin
   for t:=0 to 7 do WriteByte($AA,ConvertDFSSector($200+t,s));
   WriteByte(side_size div $100,ConvertDFSSector($306,s));
   WriteByte(side_size mod $100,ConvertDFSSector($307,s));
  end;
  //Directory size
  inc(free_space[s],$200);
  //Next side
  if(FFormat AND$1=1)then inc(s)else s:=2;
 until s=2;
 //Update the free space
 DFSFreeSpaceMap;
 Result:=Length(FDisc)>0;
end;

{-------------------------------------------------------------------------------
Set the DFS disc title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDFSDiscTitle(title: String;side: Byte): Boolean;
var
 a  : Cardinal;
 b,c: Byte;
const
 pad = 0;
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
  b:=pad;                //Pad with spaces
  if c<Length(title) then b:=Ord(title[c+1]);//AND$7F; //Chr, no top bit set
  if b<32 then b:=pad;   //Ensure no control characters
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
 filelen       : Cardinal;
begin
 Result:=False;
 if FileExists(filename,dir,entry) then //Does the file actually exist?
 //Yes, so load it - there is nothing to stop a directory header being extracted
 //if passed in the filename parameter.
 begin
  //Make space to receive the file
  filelen:=FDisc[dir].Entries[entry].Length;
  SetLength(buffer,filelen);
  //Work out where it is coming from
  source:=FDisc[dir].Entries[entry].Sector*$100;
  side:=FDisc[dir].Entries[entry].Side;
  //Read the data into the buffer
  if filelen>0 then Result:=ReadDiscData(source,filelen,side,0,buffer);
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

{-------------------------------------------------------------------------------
Extract a DFS side
-------------------------------------------------------------------------------}
function TDiscImage.ExtractDFSPartition(side: Cardinal): TDIByteArray;
var
 sidesize,
 address  : Cardinal;
begin
 Result:=nil;
 if side<2 then
 begin
  //How big is the side?
  sidesize:=(ReadByte(ConvertDFSSector($107,side))
           +(ReadByte(ConvertDFSSector($106,side))AND$3)*$100)*$100;
  if sidesize=0 then sidesize:=$32000; //Assume 200K
  //Setup the array
  SetLength(Result,sidesize);
  //Copy the data across
  for address:=$0 to sidesize-1 do
   Result[address]:=ReadByte(ConvertDFSSector(address,side));
 end;
end;

{-------------------------------------------------------------------------------
Add a blank side to a single sided image
-------------------------------------------------------------------------------}
function TDiscImage.AddDFSBlankSide(tracks: Byte): Boolean;
var
 Lbuffer    : TDIByteArray;
 side_size,t: Cardinal;
begin
 Result:=False;
 if not FDSD then
 begin
  //Create an blank single sided image
  //Setup the data area
  SetLength(Lbuffer,$200*(GetMinorFormatNumber+1)); // $200 for the header. $400 for Watford
  //Fill with zeros
  for t:=0 to Length(Lbuffer)-1 do Lbuffer[t]:=0;
  side_size:=0;
  if tracks=40 then side_size:=$190; //40T
  if tracks=80 then side_size:=$320; //80T
  SetLength(Lbuffer,side_size<<8);
  //Initialise the disc
  Lbuffer[$106]:=side_size div $100;
  Lbuffer[$107]:=side_size mod $100;
  //Watford ID
  if GetMinorFormatNumber>1 then
  begin
   for t:=0 to 7 do Lbuffer[$200+t]:=$AA;
   Lbuffer[$306]:=side_size div $100;
   Lbuffer[$307]:=side_size mod $100;
  end;
  //Add the data to the image
  Result:=AddDFSSide(Lbuffer);
 end;
end;

{-------------------------------------------------------------------------------
Add a side to a single sided image
-------------------------------------------------------------------------------}
function TDiscImage.AddDFSSide(var buffer: TDIByteArray): Boolean;
var
 FdataCopy,
 NewData    : TDIByteArray;
 addr,
 side0size,
 side2size,
 totsize    : Cardinal;
 oldfilename: String;
begin
 Result:=False;
 if not FDSD then
 begin
  //Take a copy of the class main data area
  FdataCopy:=Fdata;
  //Set the double sided flag, so that the convert sector function works
  FDSD:=True;
  //Set the new buffer to a size big enough to receive
  side0size:=(ReadByte(ConvertDFSSector($107,0))
            +(ReadByte(ConvertDFSSector($106,0))AND$3)*$100)*$100;
  if side0size=0 then side0size:=$32000; //Assume 200K
  side2size:=(ReadByte(ConvertDFSSector($107,0),buffer)
            +(ReadByte(ConvertDFSSector($106,0),buffer)AND$3)*$100)*$100;
  if side2size=0 then side2size:=$32000; //Assume 200K
  //Work out the total size
  if side0size>=side2size then totsize:=2*side0size else totsize:=2*side2size;
  SetLength(NewData,totsize);
  //Merge the data with the current data, but into another buffer
  //Step 1 - copy the current data into the new buffer, interleaving it
  for addr:=0 to side0size-1 do
   WriteByte(ReadByte(ConvertDFSSector(addr,0))
            ,ConvertDFSSector(addr,0),NewData);//Does range checking
  //Step 2 - copy the new data into the new buffer, interleaving it
  for addr:=0 to side2size-1 do
   WriteByte(ReadByte(ConvertDFSSector(addr,0),buffer)
            ,ConvertDFSSector(addr,1),NewData);//Does range checking
  //Move this buffer into the class main data area
  Fdata:=NewData;
  oldfilename:=imagefilename;
  //Re-ID the image to confirm
  if IDImage then //If success return a TRUE
   Result:=True
  else //If failure, restore the class main data area and return a FALSE
  begin
   Fdata:=FdataCopy;
   //Re-ID the image
   IDImage; //This will return a true result anyway
  end;
  //Re-read the image
  ReadImage;
  imagefilename:=oldfilename;
 end;
end;
function TDiscImage.AddDFSSide(filename: String): Boolean;
var
 NewImage: TDiscImage;
 F       : TFileStream;
 buffer  : TDIByteArray;
begin
 Result:=False;
 buffer:=nil;
 //Only read the file in if it actually exists (or rather, Windows can find it)
 if SysUtils.FileExists(filename) then
 begin
  //Only for Acorn DFS
  if(GetMajorFormatNumber=diAcornDFS)and(not FDSD)then //Single sided images only
  begin
   //Pre-load the proposed image
   NewImage:=TDiscImage.Create;
   NewImage.LoadFromFile(filename,False);
   //And make sure it is a DFS SS image
   if (NewImage.MajorFormatNumber=diAcornDFS)
   and(not NewImage.DoubleSided)then
   begin
    //Load the file in
    try
     F:=TFileStream.Create(filename,fmOpenRead or fmShareDenyNone);
     SetLength(buffer,F.Size);
     F.Read(buffer[0],F.Size);
    except
    end;
    F.Free;
    //Continue if there is any data
    if Length(buffer)>0 then Result:=AddDFSSide(buffer);
   end;
   NewImage.Free;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Move a file
-------------------------------------------------------------------------------}
function TDiscImage.MoveDFSFile(filename,directory: String): Integer;
var
 oldfn: String;
begin
 oldfn:=filename;
 //Moving and copying are the same, essentially
 Result:=CopyFile(filename,directory);
 //We just need to delete the original once copied
 if Result>-1 then DeleteFile(oldfn);
end;

{-------------------------------------------------------------------------------
Produce a report of the image's details
-------------------------------------------------------------------------------}
function TDiscImage.DFSReport(CSV: Boolean): TStringList;
var
 temp: String;
 side: Integer;
begin
 Result:=TStringList.Create;
 if FDSD then Result.Add('Double Sided') else Result.Add('Single Sided');
 side:=0;
 while side<Length(disc_size) do
 begin
  if not CSV then Result.Add('');
  Result.Add('Side '+IntToStr(side));
  if not CSV then Result.Add('------');
  Result.Add('Disc Size: '+IntToStr(disc_size[side])+' bytes');
  Result.Add('Free Space: '+IntToStr(free_space[side])+' bytes');
  temp:=IntToStr(bootoption[side]);
  case bootoption[side] of
   0: temp:='None';
   1: temp:='Load';
   2: temp:='Run';
   3: temp:='Exec';
  end;
  Result.Add('Boot Option: '+temp);
  Result.Add('Disc Name: '+disc_name[side]);
  Result.Add('Tracks: '+IntToStr(Length(free_space_map[side])));
  inc(side)
 end;
end;
