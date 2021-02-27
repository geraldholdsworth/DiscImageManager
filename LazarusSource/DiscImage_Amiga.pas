//++++++++++++++++++ Commodore Amiga +++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a Commodore Amiga disc
-------------------------------------------------------------------------------}
function TDiscImage.ID_Amiga: Boolean;
var
 Checksum1,
 Checksum2 : Cardinal;
 ctr       : Integer;
 temp      : String;
const
 DiscIDs   : array[0..3] of String = ('DOS','PFS','KICK','KICKSUP');
begin
 Result:=False;
 if FFormat=$FF then
 begin
  ResetVariables;
  //Only continue if there is data
  if GetDataLength>0 then
  begin
   //Find the disc ID
   ctr:=-1;
   repeat
    inc(ctr);
    temp:=ReadString(0,-Length(DiscIDs[ctr]));
   until (temp=DiscIDs[ctr]) or (ctr=High(DiscIDs));
   if temp=DiscIDs[ctr] then
   begin
    if Read32b($8,True)<>$370 then FBootBlock:=False; //No boot block
    //Read the boot block checksum
    if FBootBlock then
    begin
     Checksum1:=Read32b($4,True);
     //And calculate what it should be
     Checksum2:=AmigaBootChecksum($0);
    end
    else
    begin
     //No bootblock, so set both checksums to be the same
     Checksum1:=0;
     Checksum2:=0;
    end;
    //And make sure they match
    if Checksum1=Checksum2 then
    begin
     //Default directory type
     FDirType :=$00;
     //Get more details from the boot block, if there is one
     if FBootBlock then
     begin
      FMap    :=IsBitSet(ReadByte($03),0);   //AmigaDOS OFS/FFS
      FDirType:=(ReadByte($03) AND $4)shr 2; //AmigaDOS DIRC
     end;
     inc(FDirType,$10);
     secsize  :=$200;                        //Sector size
     FFormat  :=$4F;                         //Amiga format (hard disc)
     density  :=0;                           //Hard disc
     //Find the root
     root:=$002; //Start search at sector 2
     repeat
      //Make sure the checksums are not equal
      Checksum1:=$00;
      Checksum2:=$FF;
      //Find the primary and secondary types for a root block
      if root*secsize+secsize<GetDataLength then
       if  (Read32b(root*secsize     ,True)=2)
       and (Read32b(root*secsize+$1FC,True)=1) then
       begin
        //Rootblock Checksum
        Checksum1:=Read32b(root*secsize+$14,True);
        Checksum2:=AmigaChecksum(root*secsize);
       end
       else
        //Check next sector
        inc(root);
     until (Checksum1=Checksum2)
        or (root*secsize+secsize>=GetDataLength);
     //Update the format. Anything else is a hard drive (already set)
     if (Checksum1=Checksum2) and (root=$370) then
     begin
      FFormat  :=$40;                         //Amiga format (DD)
      density  :=2;                           //Double Density
     end;
     if (Checksum1=Checksum2) and (root=$6E0) then
     begin
      FFormat  :=$41;                         //Amiga format (HD)
      density  :=4;                           //High Density
     end;
     //Set the disc size
     disc_size:=root*secsize*2;
     //Set the directory separator
     dir_sep:='/';
     //and the root name
     root_name:='root';
     //More checks to ensure we have the root
     if (Checksum1<>Checksum2)
     or (Read32b(root*secsize+$000,True)<>$02)
     or (Read32b(root*secsize+$00C,True)<>$48) then
     //these are, of course, only valid for a floppy image
      ResetVariables;
    end;
   end;
   Result:=FFormat shr 4=4;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Read Commodore Amiga Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadAmigaDisc: TDisc;
var
 d,ptr,
 sectors,
 maxetry : Integer;
 fsm,
 fsmptr  : Cardinal;
 b,c     : Byte;
begin
 Result:=nil;
 //Initialise some variables
 SetLength(Result,0);
 if FFormat<>$FF then
 begin
  //Total number of sectors will be double where the root is
  sectors   :=root*2;
  //Disc size
  disc_size :=Cardinal(sectors)*secsize;
  //Disc name
  disc_name :=ReadString(root*secsize+$1B1,-(root*secsize+$1B0));
  //Work out the free space
  free_space:=secsize*2; //Allow for the boot block, even if there isn't one
  dec(sectors,2);        //The first two sectors will still be allocated for one
  //Free Space Map pointer - starts at the root block
  fsmptr    :=root*secsize+$13C;
  //Maximum number of entries in the bitmap block
  maxetry   :=25;
  while sectors>0 do
  begin
   ptr       :=0;
   repeat
    //Get the next bitmap block
    fsm:=Read32b(fsmptr+Cardinal(ptr),True)*secsize;
    //Iterate through the bitmap block
    if fsm>0 then
    begin
     for d:=4 to secsize do
     begin
      //Get the next map byte
      b:=ReadByte(fsm+Cardinal(d));
      //Go through each bit in this byte
      for c:=0 to 7 do
      begin
       //If a bit is set, that sector is free
       if (IsBitSet(b,c)) and (sectors>0) then
        inc(free_space,secsize);
       //Take account of each sector
       dec(sectors);
      end;
     end;
    end;
    //Next bitmap block pointer
    inc(ptr,4);
   until (fsm=0) or (ptr>=maxetry*4) or (sectors<=0);
   //Get next Free Space Map pointer, if it is extended.
   fsmptr:=Read32b(fsmptr+Cardinal(ptr))*secsize;
   //Calculate the maximum number of entries
   maxetry:=(secsize-4) div 4;
   //If free space map pointer is 0, we have run out of bitmap blocks, so zero
   //the sector count
   if fsmptr=0 then sectors:=0;
  end;
  //Create an entry for the root
  SetLength(Result,1);
  //Blank the values
  ResetDir(Result[0]);
  //We'll start by reading the root
  Result[0]:=ReadAmigaDir(root_name,root);
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
      //Attribute has a 'F', so drill down
      if Pos('F',Result[d].Entries[ptr].Attributes)>0 then
      begin
       //Once found, list their entries
       SetLength(Result,Length(Result)+1);
       //Read in the contents of the directory
       Result[Length(Result)-1]:=ReadAmigaDir(Result[d].Entries[ptr].Parent+dir_sep
                                         +Result[d].Entries[ptr].Filename,
                                          Result[d].Entries[ptr].Sector);
       //Update the directory reference
       Result[d].Entries[ptr].DirRef:=Length(Result)-1;
      end;
     end;
   end;
   inc(d);
  //The length of disc will increase as more directories are found
  until d>=Length(Result);
 end;
end;

{-------------------------------------------------------------------------------
Read Commodore Amiga Directory
-------------------------------------------------------------------------------}
function TDiscImage.ReadAmigaDir(dirname: String; offset: Cardinal): TDir;
var
 address,
 i,att,a,
 link,ent: Cardinal;
 Entry   : TDirEntry;
const
 attributes: array[0..31] of Char =
  ('D','E','W','R','A','P','S','H',
   'd','e','w','r','d','e','w','r',
   ' ',' ',' ',' ',' ',' ',' ',' ',
   ' ',' ',' ',' ',' ',' ',' ',' ');
begin
 //Initialise the return variable (this just stops the compiler from warning)
 Result.Directory:='';
 ResetDir(Result);
 //If the checksum checks out, read in the contents
 if Read32b(offset*secsize+$14,True)=AmigaChecksum(offset*secsize) then
 begin
  //Directory Name
  if offset=root then
   Result.Directory:=dirname
  else
   Result.Directory:=ReadString(offset*secsize+$1B1,-ReadByte(offset*secsize+$1B0));
  //Go through the hash table and find the entries.
  ent:=Read32b(offset*secsize+$0C,True); //Size of hash table
  if ent=0 then ent:=(secsize div 4)-56; //if 0, then it should be BSIZE/4 - 56
  for i:=0 to ent-1 do
  begin
   //Get the sector of the next entry
   link:=Read32b(offset*secsize+$18+i*4,True);
   //Is entry found (0 if no entry)
//   if link<>0 then
   while link<>0 do
   begin
    ResetDirEntry(Entry);
    //Work out the offset absolute address
    address:=link*secsize;
    //Make sure checksum is valid
    if Read32b(address+$14,True)=AmigaChecksum(address) then
    begin
     //Read in the details
     Entry.Sector  :=Read32b(address+$10,True); //Sector of the first data block
     Entry.Filename:=ReadString(address+$1B1,-ReadByte(address+$1B0));
     Entry.Length  :=Read32b(address+$144,True);
     Entry.Parent  :=dirname;
     if Read32b(address+$1FC,True)=2 then //This is a directory
     begin
      //We'll use 'F' for directory, as 'D' is used for something else
      Entry.Attributes:='F';
      Entry.Filetype  :='Directory';
      Entry.Length    :=secsize;
      Entry.Sector    :=link; //Sector will therefore point to the header
     end;
     //Attributes
     att           :=Read32b(address+$140,True);
     for a:=0 to 31 do
      if not IsBitSet(att,a) then
       Entry.Attributes:=Entry.Attributes+attributes[a];
     RemoveSpaces(Entry.Attributes);
     //Not a directory - default. Will be determined later
     Entry.DirRef:=-1;
     //Add to the result
     SetLength(Result.Entries,Length(Result.Entries)+1);
     Result.Entries[Length(Result.Entries)-1]:=Entry;
     //Read the next in the link chain, if any (0 will exit the loop)
     link:=Read32b(address+$1F0,True);
    end
    //If checksum is invalid, mark the link as 0, which will exit the loop
    else link:=0;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Calculate Amiga Boot Block checksum
-------------------------------------------------------------------------------}
function TDiscImage.AmigaBootChecksum(offset: Cardinal): Cardinal;
begin
 Result:=not GeneralChecksum(offset,$400,$004,0,True);
end;

{-------------------------------------------------------------------------------
Calculate Amiga regular checksum
-------------------------------------------------------------------------------}
function TDiscImage.AmigaChecksum(offset: Cardinal): Cardinal;
begin
 Result:=-GeneralChecksum(offset,$200,$014,0,False);
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path
-------------------------------------------------------------------------------}
function TDiscImage.ExtractAmigaFile(filename: String;
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
  fragptr:=Cardinal(FDisc[dir].Entries[entry].Sector);
  dest  :=0;      //Length pointer/Destination pointer
  repeat
   //Fragmented filing system, so need to work out source and length
   source:=Integer(fragptr*secsize)+$18;     //Source of data
   len   :=Read32b(fragptr*secsize+$C,True);//Amount of data
   //Make sure we don't read too much
   if dest+len>filelen then
    len:=filelen-dest;
   //Read the data into the buffer
   ReadDiscData(source,len,FDisc[dir].Entries[entry].Side,buffer[dest]);
   //Move the size pointer on, by the amount read
   inc(dest,len);
   //Get the next block pointer
   fragptr:=Read32b(fragptr*secsize+$10,True);
  until dest>=filelen; //Once we've reached the file length, we're done
 end;
 Result:=True;
end;

{-------------------------------------------------------------------------------
Write a file to Amiga image
-------------------------------------------------------------------------------}
function TDiscImage.WriteAmigaFile(var file_details: TDirEntry;
                             var buffer: TDIByteArray): Integer;
begin
 Result:=-1;
end;

{-------------------------------------------------------------------------------
Create a directory on an Amiga image
-------------------------------------------------------------------------------}
function TDiscImage.CreateAmigaDirectory(var dirname,parent,attributes: String): Integer;
begin
 Result:=-1;
end;

{-------------------------------------------------------------------------------
Retitle an Amiga directory
-------------------------------------------------------------------------------}
function TDiscImage.RetitleAmigaDirectory(filename,newtitle: String): Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Create a new Amiga image
-------------------------------------------------------------------------------}
function TDiscImage.FormatAmiga(minor: Byte): TDisc;
begin
 //
end;

{-------------------------------------------------------------------------------
Rename a file
-------------------------------------------------------------------------------}
function TDiscImage.RenameAmigaFile(oldfilename: String;var newfilename: String):Integer;
begin
 Result:=-6; //Unsupported in this format
end;

{-------------------------------------------------------------------------------
Delete a file
-------------------------------------------------------------------------------}
function TDiscImage.DeleteAmigaFile(filename: String):Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Update a file's attributes
-------------------------------------------------------------------------------}
function TDiscImage.UpdateAmigaFileAttributes(filename,attributes: String):Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Update the disc title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateAmigaDiscTitle(title: String): Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Moves a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.MoveAmigaFile(filename,directory: String): Integer;
begin
 Result:=-1;
end;
