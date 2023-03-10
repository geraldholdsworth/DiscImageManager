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
 look4root : Boolean;
const
 DiscIDs   : array[0..3] of String = ('DOS','PFS','KICK','KICKSUP');
begin
 Result:=False;
 if FFormat=diInvalidImg then
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
    //Default directory type
    FDirType :=$00;
    //Get more details from the boot block disc ID
    FMap    :=IsBitSet(ReadByte($03),0);   //AmigaDOS OFS/FFS
    FDirType:=(ReadByte($03) AND $4)<<2; //AmigaDOS DIRC
    //Look at the checksum
    if not FMap then //OFS should have a checksum
    begin
     Checksum1:=Read32b($4,True);
     //And calculate what it should be (only if non-zero)
     if Checksum1<>0 then
      Checksum2:=AmigaBootChecksum($0)
     else
      Checksum2:=0;
    end
    else
    begin
     //FFS won't, so set both checksums to be the same
     Checksum1:=0;
     Checksum2:=0;
    end;
    if Checksum1=Checksum2 then
    begin
     inc(FDirType,$10);
     secsize  :=$200;                        //Sector size
     //Set up for a hard disc for now
     FFormat  :=diAmiga<<4+$F;               //Amiga format (hard disc)
     density  :=0;                           //Hard disc
     //Find the root - this will actually be halfway through the disc
     root:=(GetDataLength div secsize)div 2;
     look4root:=False;//We're not looking at the moment
     repeat
      if look4root then inc(root);//Next sector, if we are looking
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
       end;
      //If we haven't found the root in the middle
      if(Checksum1<>Checksum2)and(not look4root) then
      begin
       //Start at the beginning and work through
       root:=1;
       look4root:=True;
      end;
      //Carry on until we either find the root, or we reach the end of the code
     until (Checksum1=Checksum2)
        or (root*secsize+secsize>=GetDataLength);
     //Update the format. Anything else is a hard drive (already set)
     if Checksum1=Checksum2 then
     begin
      if root=$370 then
      begin
       FFormat  :=diAmiga<<4;                  //Amiga format (DD)
       density  :=2;                           //Double Density
      end;
      if root=$6E0 then
      begin
       FFormat  :=diAmiga<<4+1;                //Amiga format (HD)
       density  :=4;                           //High Density
      end;
     end;
     //Set the disc size
     disc_size[0]:=root*secsize*2;
     //Set the directory separator
     dir_sep:='/';
     //and the root name
     root_name:='DF0:';
     //More checks to ensure we have the root
     if (Checksum1<>Checksum2)
     or (Read32b(root*secsize+$000,True)<>$02)
     or (Read32b(root*secsize+$00C,True)<>$48) then
     //these are, of course, only valid for a floppy image
      ResetVariables;
    end;
   end;
   Result:=GetMajorFormatNumber=diAmiga;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Read Commodore Amiga Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadAmigaDisc: Boolean;
var
 d,ptr,
 sectors : Integer;
begin
 FDisc:=nil;
 //Initialise some variables
 SetLength(FDisc,0);
 if FFormat<>diInvalidImg then
 begin
  //Total number of sectors will be double where the root is
  sectors   :=root*2;
  //Disc size
  disc_size[0]:=Cardinal(sectors)*secsize;
  //Disc name
  disc_name[0]:=ReadString(root*secsize+$1B1,-(root*secsize+$1B0));
  //Create an entry for the root
  SetLength(FDisc,1);
  //Blank the values
  ResetDir(FDisc[0]);
  //We'll start by reading the root
  FDisc[0]:=ReadAmigaDir(root_name,root);
  //Now iterate through the entries and find the sub-directories
  d:=0;
  repeat
   //If there are actually any entries
   if Length(FDisc[d].Entries)>0 then
   begin
    //Go through the entries
    for ptr:=0 to Length(FDisc[d].Entries)-1 do
     //And add them if they are valid
     if FDisc[d].Entries[ptr].Filename<>'' then
     begin
      //Attribute has a 'F', so drill down
      if Pos('F',FDisc[d].Entries[ptr].Attributes)>0 then
      begin
       //Once found, list their entries
       SetLength(FDisc,Length(FDisc)+1);
       //Read in the contents of the directory
       if FScanSubDirs then
        FDisc[Length(FDisc)-1]:=ReadAmigaDir(GetParent(d)+dir_sep
                                          +FDisc[d].Entries[ptr].Filename,
                                           FDisc[d].Entries[ptr].Sector);
       FDisc[Length(FDisc)-1].Parent:=d;
       //Update the directory reference
       FDisc[d].Entries[ptr].DirRef:=Length(FDisc)-1;
      end;
     end;
   end;
   inc(d);
  //The length of disc will increase as more directories are found
  until d>=Length(FDisc);
  //Get the free space map
  ReadAmigaFSM;
 end;
 Result:=Length(FDisc)>0;
end;

{-------------------------------------------------------------------------------
Read Commodore Amiga Directory
-------------------------------------------------------------------------------}
function TDiscImage.ReadAmigaDir(dirname: String; offset: Cardinal): TDir;
var
 address,
 i,
 link,ent: Cardinal;
 Entry   : TDirEntry;
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
  Result.Sector:=offset;
  Result.BeenRead:=True;
  //Go through the hash table and find the entries.
  ent:=Read32b(offset*secsize+$0C,True); //Size of hash table
  if ent=0 then ent:=(secsize div 4)-56; //if 0, then it should be BSIZE/4 - 56
  for i:=0 to ent-1 do
  begin
   //Get the sector of the next entry
   link:=Read32b(offset*secsize+$18+i*4,True);
   //Is entry found (0 if no entry)
   while link<>0 do
   begin
    ResetDirEntry(Entry);
    //Work out the offset absolute address
    address:=link*secsize;
    //Make sure checksum is valid
    if Read32b(address+$14,True)=AmigaChecksum(address) then
    begin
     //Read in the details
     Entry.Sector  :=link;//Read32b(address+$10,True); //Sector of the first data block
     Entry.Filename:=ReadString(address+$1B1,-ReadByte(address+$1B0));
     Entry.Length  :=Read32b(address+$144,True);
     Entry.Parent  :=dirname;
     Entry.TimeStamp:=FromAmigaTime(Read32b(address+$1A4,True),
                                    Read32b(address+$1A8,True),
                                    Read32b(address+$1AC,True));
     //Attributes
     Entry.Attributes:=AmigaIntToStrAttr(Read32b(address+$140,True));
     if Read32b(address+$1FC,True)=2 then //This is a directory
     begin
      //We'll use 'F' for directory, as 'D' is used for something else
      Entry.Attributes:=Entry.Attributes+'F';
      Entry.Filetype  :='Directory';
      Entry.Length    :=secsize;
      //Entry.Sector    :=link; //Sector will therefore point to the header
     end;
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
  Result.BeenRead:=True;
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
 entry,dir : Cardinal;
begin
 Result:=False;
 SetLength(buffer,0);
 if FileExists(filename,dir,entry) then //Does the file actually exist?
  if FDisc[dir].Entries[entry].Length>0 then //Is there anything to extract?
   Result:=ExtractAmigaData(FDisc[dir].Entries[entry].Sector,
                            FDisc[dir].Entries[entry].Length,
                            buffer);
end;

{-------------------------------------------------------------------------------
Read data following the hash table links
-------------------------------------------------------------------------------}
function TDiscImage.ExtractAmigaData(sector,filelen: Cardinal;
                                             var buffer: TDIByteArray): Boolean;
var
 source,
 dest,
 fragptr,
 len     : Cardinal;
 links   : TFragmentArray;
begin
 //Default return result
 Result:=False;
 //Set the buffer to the required length
 SetLength(buffer,filelen);
 //Get the links for the file
 links:=GetAmigaChain(sector);
 //If there are any, then continue
 if Length(links)>0 then
 begin
  dest:=0;//Length pointer/Destination pointer
  for fragptr:=0 to Length(links)-1 do
  begin
   len:=links[fragptr].Length;
   source:=links[fragptr].Offset*secsize;
   //Make sure we don't read too much
   if filelen<>0 then if dest+len>filelen then len:=filelen-dest;
   //Increase the space required
   if dest+len>Length(buffer) then SetLength(buffer,dest+len);
   if not FMap then inc(source,$18);//Move to where the data is
   //Read the data into the buffer
   ReadDiscData(source,len,0,dest,buffer);
   //Move the size pointer on, by the amount read
   inc(dest,len);
  end;
 end;
 Result:=True;
end;

{-------------------------------------------------------------------------------
Write a file to Amiga image
-------------------------------------------------------------------------------}
function TDiscImage.WriteAmigaFile(var file_details: TDirEntry;
                                             var buffer: TDIByteArray): Integer;
var
 index,
 fragptr,
 dir,
 entry,
 paraddr,
 days,
 mins,
 ticks,
 filelen : Cardinal;
 header  : TDIByteArray;
 frag    : TFragmentArray;
 hdrblks,
 datablks: array of Cardinal;
procedure WriteBlockToDisc(address: Cardinal);
var index: Cardinal;
begin
 //Write to the disc
 for index:=0 to Length(header)-1 do
  WriteByte(header[index],address+index);
 //Update the checksum
 Write32b(AmigaChecksum(address),address+$14,True);
end;
begin
 ValidateAmigaFile(file_details.Filename);
 Result:=-6; //Destination directory does not exist
 //Ensure that the file does not alredy exist
 if not FileExists(file_details.Parent+dir_sep+file_details.Filename,dir,entry) then
 begin
  Result:=-3; //File already exists
  //Ensure that the parent exists
  if FileExists(file_details.Parent,dir,entry) then
  begin
   Result:=-2; //Image full
   //Get the parent address
   if file_details.Parent=root_name then
   begin
    paraddr:=root;
    dir:=0;
   end
   else paraddr:=FDisc[dir].Entries[entry].Sector;
   //Has it been read in?
   if not FDisc[dir].BeenRead then ReadDirectory(file_details.Parent);
   //Work out the total number of space, including headers
   filelen:=Length(buffer)+secsize; //At least one header
   //OFS has 24 byte data headers for each block of data
   if not FMap then inc(filelen,Ceil(Length(buffer)/(secsize-24))*24);
   //One OFS file header has enough space for 72 pointers * $1E8 = $8940 bytes
   if not FMap then inc(filelen,(Length(buffer)div$8940)*secsize);
   //One FFS file header has enough space for 72 pointers * $200 = $9000 bytes
   if FMap then inc(filelen,(Length(buffer)div$9000)*secsize);
   //Ensure it is an exact multiple of sector size
   filelen:=Ceil(filelen/secsize)*secsize;
   //Find space
   frag:=AmigaFindFreeSpace(filelen);
   //If any was found, the go ahead and create the file header
   if Length(frag)>0 then
   begin
    //Initialise our block addresses
    SetLength(hdrblks,0);
    SetLength(datablks,0);
    //Split the fragments into header blocks and data blocks
    for fragptr:=0 to Length(frag)-1 do
    begin
     if fragptr mod 73=0 then //File header or extended block
     begin
      SetLength(hdrblks,Length(hdrblks)+1);
      hdrblks[Length(hdrblks)-1]:=frag[fragptr].Offset;
     end;
     if fragptr mod 73<>0 then //Data block
     begin
      SetLength(datablks,Length(datablks)+1);
      datablks[Length(datablks)-1]:=frag[fragptr].Offset;
     end;
    end;
    //Now go through each and prepare the headers
    SetLength(header,secsize);//Prepare the header
    //Header blocks
    for fragptr:=0 to Length(hdrblks)-1 do
    begin
     //Clear the buffer area
     for index:=0 to Length(header)-1 do header[index]:=0;
     //Prepare block for writing
                                       //Common ++++++++++++++++++++++++++++++++
     Write32b(hdrblks[fragptr],4,header,True);         //Self pointer
     if Length(datablks)<=72*(fragptr+1) then          //Number of data block pointers here
      Write32b(Length(datablks)-72*fragptr,8,header,True)
     else
      Write32b(72,8,header,True);
     for index:=0 to 71 do                             //Data pointers
      if index+(72*fragptr)<Length(datablks) then
       Write32b(datablks[index+(72*fragptr)],$134-index*4,header,True);
     if fragptr+1<Length(hdrblks) then
      Write32b(hdrblks[fragptr+1],$1F8,header,True);   //Next extended data block
     Write32b($FFFFFFFD,$1FC,header,True);             //Secondary Type
                                       //File header +++++++++++++++++++++++++++
     if fragptr=0 then
     begin
      Write32b(2,0,header,True);                       //Primary Type
      Write32b(datablks[0],$10,header,True);           //First data block pointer
      Write32b(AmigaStrToIntAttr(file_details.Attributes),
               $140,header,True);                      //Attributes
      Write32b(Length(buffer),$144,header,True);       //File length
      ToAmigaTime(Now,days,mins,ticks);
      Write32b(days,$1A4,header,True);                 //Last access date - days
      Write32b(mins,$1A8,header,True);                 //Last access time - mins
      Write32b(ticks,$1AC,header,True);                //Last access time - ticks
      WriteByte(Length(file_details.Filename),
                $1B0,header);                          //Filename length
      WriteString(file_details.Filename,
                $1B1,30,0,header);                     //Filename
      Write32b(paraddr,$1F4,header,True);              //Parent address
     end;
                                       //Extended block header +++++++++++++++++
     if fragptr<>0 then
     begin
      Write32b(16,0,header,True);                      //Primary Type
      Write32b(hdrblks[0],$1F4,header,True);           //File header block
     end;
     //Now write the block to disc
     WriteBlockToDisc(hdrblks[fragptr]*secsize);
    end;
    //Data blocks 
    for fragptr:=0 to Length(datablks)-1 do
    begin
     //Clear the buffer area
     for index:=0 to Length(header)-1 do header[index]:=0;
     //Prepare block for writing
                                       //Data block (OFS) ++++++++++++++++++++++
     if not FMap then
     begin
      Write32b(8,0,header,True);                       //Primary Type
      Write32b(hdrblks[0],4,header,True);              //Pointer to file header
      Write32b(fragptr+1,8,header,True);               //File data block number
      if Length(buffer)-(fragptr*$1E8)<$1E8 then       //Data length
       Write32b(Length(buffer)-(fragptr*$1E8),$c,header,True)
      else
       Write32b($1E8,$c,header,True);
      if fragptr+1<Length(datablks)then
       Write32b(datablks[fragptr+1],$10,header,True);     //Next data block
      for index:=0 to $1E7 do                          //Data
       if index+fragptr*$1E8<Length(buffer) then
        WriteByte(buffer[index+fragptr*$1E8],$18+index,header);
     end;
                                       //Data block (FFS) ++++++++++++++++++++++
     if FMap then
      for index:=0 to $1FF do                          //Data
       if index+fragptr*$200<Length(buffer) then
        WriteByte(buffer[index+fragptr*$200],index,header);
     //Now write the block to disc
     WriteBlockToDisc(datablks[fragptr]*secsize);
    end;
    //Blocks now written, add it to the parent
    AmigaAddtoChain(file_details.Filename,paraddr,frag[0].Offset);
    //Update our local copy
    Result:=Length(FDisc[dir].Entries);
    SetLength(FDisc[dir].Entries,Length(FDisc[dir].Entries)+1);
    //Update our local FSM
    for index:=0 to Length(frag)-1 do
     AmigaFillFreeSpaceMap(frag[index].Offset*secsize,$FF);
    //Write the fields
    FDisc[dir].Entries[Result]:=file_details;
    FDisc[dir].Entries[Result].Sector:=frag[0].Offset;
    FDisc[dir].Entries[Result].Timestamp:=Now;
    FDisc[dir].Entries[Result].DirRef:=-1;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Create a directory on an Amiga image
-------------------------------------------------------------------------------}
function TDiscImage.CreateAmigaDirectory(var dirname,parent,attributes: String): Integer;
var
 buffer          : TDIByteArray;
 index,
 days,mins,ticks,
 dir,entry,
 paraddr         : Cardinal;
 frag            : TFragmentArray;
begin
 ValidateAmigaFile(dirname);
 Result:=-6; //Destination directory does not exist
 //Ensure that the directory does not alredy exist
 if not FileExists(parent+dir_sep+dirname,dir,entry) then
 begin
  Result:=-3; //Directory already exists
  //Ensure that the parent exists
  if FileExists(parent,dir,entry) then
  begin
   Result:=-2; //Image full
   //Get the parent address
   if parent=root_name then
   begin
    paraddr:=root;
    dir:=0;
   end
   else
   begin
    paraddr:=FDisc[dir].Entries[entry].Sector;
    //Has it been read in?
    if not FDisc[FDisc[dir].Entries[entry].DirRef].BeenRead then ReadDirectory(parent);
   end;
   //Find space
   frag:=AmigaFindFreeSpace(secsize);
   //If any was found, the go ahead and create the directory
   if Length(frag)>0 then
   begin
    //Prepare the header
    SetLength(buffer,secsize);
    //Clear it
    for index:=0 to Length(buffer)-1 do buffer[index]:=0;
    //Write the data
    Write32b(2,0,buffer,True);             //Primary Type
    Write32b(2,$1FC,buffer,True);          //Secondary Type
    Write32b(frag[0].Offset,4,buffer,True);//Self pointer
    Write32b(AmigaStrToIntAttr(attributes),$140,buffer,True);//Attributes
    ToAmigaTime(Now,days,mins,ticks);
    Write32b(days,$1A4,buffer,True);       //Last access date - days
    Write32b(mins,$1A8,buffer,True);       //Last access time - mins
    Write32b(ticks,$1AC,buffer,True);      //Last access time - ticks
    WriteByte(Length(dirname),$1B0,buffer);//Directory name length
    WriteString(dirname,$1B1,30,0,buffer); //Directory name
    Write32b(paraddr,$1F4,buffer,True);    //Parent address
    //Write to the disc
    for index:=0 to Length(buffer)-1 do
     WriteByte(buffer[index],frag[0].Offset*secsize+index);
    //Update the checksum
    Write32b(AmigaChecksum(frag[0].Offset*secsize),frag[0].Offset*secsize+$14,True);
    //Add to the parent directory
    AmigaAddToChain(dirname,paraddr,frag[0].Offset);
    //Update our local copy
    Result:=Length(FDisc[dir].Entries);
    SetLength(FDisc[dir].Entries,Length(FDisc[dir].Entries)+1);
    //Update our local FSM
    AmigaFillFreeSpaceMap(frag[0].Offset*secsize,$FF);
    //Write the fields
    FDisc[dir].Entries[Result].Sector:=frag[0].Offset;
    FDisc[dir].Entries[Result].Parent:=parent;
    FDisc[dir].Entries[Result].Filename:=dirname;
    FDisc[dir].Entries[Result].Attributes:='F'+attributes;//'F' is directory
    FDisc[dir].Entries[Result].Timestamp:=Now;
    FDisc[dir].Entries[Result].Length:=secsize;
    FDisc[dir].Entries[Result].Filetype:='Directory';
    FDisc[dir].Entries[Result].DirRef:=Length(FDisc); //Reference to this directory
    SetLength(FDisc,Length(FDisc)+1);//Make room
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Create a new Amiga image - Floppy
-------------------------------------------------------------------------------}
function TDiscImage.FormatAmigaFDD(minor: Byte): Boolean;
begin
 //Blank everything
 ResetVariables;
 SetDataLength(0);
 //Set the format
 //FFormat:=diAmiga<<4+minor;
 //Start with blank result
 FDisc:=nil;
 SetLength(FDisc,0);
 //Format the drive
 case minor of
  0: FormatAmiga(880*1024);
  1: FormatAmiga(1760*1024);
 end;
 //Read it back in to set the rest up
 if ID_Amiga then Result:=ReadAmigaDisc;
 //Set the filename
 imagefilename:='Untitled.'+FormatExt;
end;

{-------------------------------------------------------------------------------
Create a new Amiga image - Hard Disc
-------------------------------------------------------------------------------}
function TDiscImage.FormatAmigaHDD(harddrivesize: Cardinal): Boolean;
begin
 //Blank everything
 ResetVariables;
 SetDataLength(0);
 //Set the format
 //FFormat:=diAmiga<<4+$F;
 //Start with blank result
 FDisc:=nil;
 SetLength(FDisc,0);
 //Format the drive
 FormatAmiga(harddrivesize);
 //Read it back in to set the rest up
 if ID_Amiga then Result:=ReadAmigaDisc;
 //Set the filename
 imagefilename:='Untitled.'+FormatExt;
end;

{-------------------------------------------------------------------------------
Create a new Amiga image - generic
-------------------------------------------------------------------------------}
procedure TDiscImage.FormatAmiga(size: Cardinal);
var
 index,
 bmpsize,
 fsmblock,
 days,
 mins,
 ticks    : Cardinal;
 fsm      : TDIByteArray;
 fsmlist,
 extlist  : TFragmentArray;
begin
 ResetVariables;
 secsize:=$200;
 //Round the size down to the nearest block size
 size:=(size div secsize)*secsize;
 //Then create it
 SetDataLength(size);
 //Now blank it
 UpdateProgress('Formatting...');
 for index:=0 to size-1 do WriteByte(0,index);
 //Set up the data areas
 UpdateProgress('Initialising...');
 //Bootblock
 WriteString('DOS',0,3,0);
 //For sizes > 20MB (i.e. Hard Drives) we'll use FFS
 if size>=20*1024*1024 then WriteByte(1,3);
 //Rootblock
 root:=(size div secsize)div 2;
 Write32b($2,root*secsize,True);            //Primary Type
 Write32b($1,root*secsize+$1FC,True);       //Secondar Type
 Write32b($48,root*secsize+$C,True);        //Hash Table Size
 Write32b($FFFFFFFF,root*secsize+$138,True);//Valid bitmap (-1)
 ToAmigaTime(Now,days,mins,ticks);
 Write32b(days,root*secsize+$1A4,True);     //Last access date
 Write32b(days,root*secsize+$1D8,True);     //Last access date
 Write32b(days,root*secsize+$1E4,True);     //Creation date
 Write32b(mins,root*secsize+$1A8,True);     //Last access time
 Write32b(mins,root*secsize+$1DC,True);     //Last access time
 Write32b(mins,root*secsize+$1E8,True);     //Creation time
 Write32b(ticks,root*secsize+$1AC,True);    //Last access time
 Write32b(ticks,root*secsize+$1E0,True);    //Last access time
 Write32b(ticks,root*secsize+$1EC,True);    //Creation time
 WriteByte(Length(amigadisctitle),root*secsize+$1B0);//Length of disc name
 WriteString(amigadisctitle,root*secsize+$1B1,30,0); //Disc name
 //Bitmap block
 bmpsize:=Ceil(((size-secsize*2)div secsize)/8);
 //We'll create it all in a temporary store first
 SetLength(fsm,bmpsize);
 for index:=0 to bmpsize-1 do fsm[index]:=$FF;
 //We have more than required
 if bmpsize*8>(size-secsize*2)div secsize then
  for index:=((size-secsize*2)div secsize)+1 to bmpsize*8 do
   AmigaAllocateFSMBlock(index+2,True,fsm);
 //Mark out the used blocks (i.e. Root)
 AmigaAllocateFSMBlock(root,True,fsm);
 //Write the FSM to disc
 SetLength(fsmlist,Ceil(bmpsize/$1FC)); //We'll create our pointer list
 if Length(fsmlist)>25 then //And our bitmap extensions list
  SetLength(extlist,Ceil((Length(fsmlist)-25)/127));
 if Length(fsmlist)>0 then //Make sure something is there
 begin
  //First entry is after the root
  for index:=1 to Length(fsmlist) do
  begin
   if index<26 then fsmblock:=root+index          //Less than 25 blocks
   else fsmblock:=root+index+1+((index-26)div 127)*127;//Make way for the ext blocks
   //Allocate the space
   AmigaAllocateFSMBlock(fsmblock,True,fsm);
   //Write the markers to the root block and ext blocks
   if index-1<25 then Write32b(fsmblock,root*secsize+$13C+((index-1)*4),True)
   else Write32b(fsmblock,(root+26+((index-26)div 127)*127)*secsize+((index-26)*4),True);
   //Make a note
   fsmlist[index-1].Offset:=fsmblock;
  end;
  //Write out the pointers to the bitmap extension blocks
  //(last word of each ext block points to the next, or 0 for last)
  if Length(extlist)>0 then
  begin
   //Work out the locations and allocate the free space
   for index:=0 to Length(extlist)-1 do
   begin
    extlist[index].Offset:=root+26+index*127;
    AmigaAllocateFSMBlock(extlist[index].Offset,True,fsm);
   end;
   //Go through again and write the pointers
   for index:=0 to Length(extlist)-1 do
    //First will be in the rootblock
    if index=0 then Write32b(extlist[index].Offset,root*secsize+$1A0,True)
    else //Subsequent at the end of each ext block
     if index+1<Length(extlist)-1 then //Unless it is the last
      Write32b(extlist[index+1].Offset,extlist[index].Offset*secsize+$1FC,True);
  end;
  AmigaWriteBitmap(fsmlist,fsm);
 end;
 //Root checksum
 Write32b(AmigaChecksum(root*secsize),(root*secsize)+$14,True);
end;

{-------------------------------------------------------------------------------
Rename a file
-------------------------------------------------------------------------------}
function TDiscImage.RenameAmigaFile(oldfilename: String;var newfilename: String):Integer;
var
 dirname  : String;
 dir,ref,
 entry,
 sector   : Cardinal;
begin
 Result:=-2; //File does not exist
 //Validate the new filename
 ValidateAmigaFile(newfilename);
 //Ensure that the file exists
 if FileExists(oldfilename,dir,entry) then
 begin
  Result:=-3; //New name already exists
  //Get the parent directory path
  dirname:=GetParent(dir);
  //Make sure it does not already exist
  if not FileExists(dirname+dir_sep+newfilename,ref) then
  begin
   Result:=-1;
   sector:=FDisc[dir].Entries[entry].Sector;
   //Remove the old filename from the chain
   if AmigaRemoveFromChain(Copy(oldfilename,Length(dirname)+2),
                           FDisc[dir].Sector,sector) then
   begin
    //Add the new filename to the chain
    AmigaAddToChain(newfilename,FDisc[dir].Sector,sector);
    //Update the filename in the entry
    WriteByte(Length(newfilename),sector*secsize+$1B1);
    WriteString(newfilename,sector*secsize+$1B1,30,0);
    //And update the checksum
    Write32b(AmigaChecksum(sector*secsize),sector*secsize+$14,True);
    //Update the local copy and return the reference
    FDisc[dir].Entries[entry].Filename:=newfilename;
    Result:=entry;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Delete a file
-------------------------------------------------------------------------------}
function TDiscImage.DeleteAmigaFile(filename: String):Boolean;
var
 dirref   : Integer;
 dir,
 entry,
 hashval  : Cardinal;
 success  : Boolean;
 fsmfrags,
 links    : TFragmentArray;
 fsm      : TDIByteArray;
begin
 Result:=False;
 if filename=root_name then exit(False);
 //Does the file exist?
 if FileExists(filename,dir,entry) then
 begin
  dirref:=-1;
  //Is this a directory?
  if FDisc[dir].Entries[entry].DirRef<>-1 then
  begin
   dirref:=FDisc[dir].Entries[entry].DirRef;
   FDisc[dirref].Deleted:=True;
   //Has it been read in?
   if not FDisc[dirref].BeenRead then ReadDirectory(filename);
   success:=True;
   //Recusively delete the contents.
   while(Length(FDisc[dirref].Entries)>0)and(success)do
    success:=DeleteAmigaFile(filename+dir_sep+FDisc[dirref].Entries[0].Filename);
  end;
  //Remove the entry from the chain
  if AmigaRemoveFromChain(Copy(filename,Length(GetParent(dir))+2),
                          FDisc[dir].Sector,
                          FDisc[dir].Entries[entry].Sector) then
  begin
   //Now we can remove this and all related sectors from the FSM
   fsmfrags:=AmigaReadBitmap(fsm);//Get the FSM
   links:=GetAmigaChain(FDisc[dir].Entries[entry].Sector);  //Get the chain
   if Length(links)>0 then        //Go through the chain and mark each one as free
    for hashval:=0 to Length(links)-1 do
    begin
     AmigaAllocateFSMBlock(links[hashval].Offset,False,fsm); //Mark as free
     AmigaFillFreeSpaceMap(links[hashval].Offset*secsize,$00);//Local copy
    end;
   AmigaWriteBitmap(fsmfrags,fsm);//Put the FSM back
   //Update our local copy
   if Length(FDisc[dir].Entries)>1 then
    if entry<Length(FDisc[dir].Entries)-1 then //Move the other entries down
     for hashval:=entry to Length(FDisc[dir].Entries)-2 do
      FDisc[dir].Entries[hashval]:=FDisc[dir].Entries[hashval+1];
   //And lose the last entry
   SetLength(FDisc[dir].Entries,Length(FDisc[dir].Entries)-1);
   if dirref>=-1 then UpdateDirRef(dirref);
   //Set as positive result
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update a file's attributes
-------------------------------------------------------------------------------}
function TDiscImage.UpdateAmigaFileAttributes(filename,attributes: String):Boolean;
var
 dir,
 entry: Cardinal;
begin
 Result:=False;
 //Make sure the file actually exists
 if FileExists(filename,dir,entry) then
 begin
  //Write the new attribute word
  Write32b(AmigaStrToIntAttr(attributes),
           FDisc[dir].Entries[entry].Sector*secsize+$140,True);
  //Update the checksum
  Write32b(AmigaChecksum(FDisc[dir].Entries[entry].Sector*secsize),
           FDisc[dir].Entries[entry].Sector*secsize+$14,True);
  //Update our local copy
  FDisc[dir].Entries[entry].Attributes:=attributes;
  //Return a positive result
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Update the disc title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateAmigaDiscTitle(title: String): Boolean;
begin
 Result:=True;
 //Ensure that the title is valid
 ValidateAmigaFile(title);
 //Write the length
 WriteByte(Length(title),root*secsize+$1B0);
 //And now write the title
 WriteString(title,root*secsize+$1B1,30,0);
 //And update the checksum
 Write32b(AmigaChecksum(root*secsize),root*secsize+$14,True);
 //Update the local copy
 disc_name[0]:=title;
end;

{-------------------------------------------------------------------------------
Moves a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.MoveAmigaFile(filename,directory: String): Integer;
var
 sdir,sentry,
 ddir,dentry,
 index       : Cardinal;
 file_details: TDirEntry;
begin
 Result:=-6; //Destination does not exist
 if FileExists(directory,ddir,dentry) then
 begin
  if directory=root_name then ddir:=0
  else ddir:=FDisc[ddir].Entries[dentry].DirRef;
  if ddir>=Length(FDisc) then exit(-12);
  Result:=-11; //Source does not exist
  if FileExists(filename,sdir,sentry) then
  begin
   if sdir=ddir then exit(-10);//Cannot move to the same directory 
   //Has it been read in?
   if not FDisc[ddir].BeenRead then ReadDirectory(directory);
   //Remove from the source
   if AmigaRemoveFromChain(Copy(filename,Length(GetParent(sdir))+2),
                           FDisc[sdir].Sector,
                           FDisc[sdir].Entries[sentry].Sector) then
   begin
    //And then add to the destination
    AmigaAddToChain(Copy(filename,Length(GetParent(sdir))+2),
                    FDisc[ddir].Sector,
                    FDisc[sdir].Entries[sentry].Sector);
    //Take a note of this
    file_details:=FDisc[sdir].Entries[sentry];
    //Update the local copies
    if Length(FDisc[sdir].Entries)>1 then //First remove from the source
     if sentry<Length(FDisc[sdir].Entries)-1 then //Move the other entries down
      for index:=sentry to Length(FDisc[sdir].Entries)-2 do
       FDisc[sdir].Entries[index]:=FDisc[sdir].Entries[index+1];
    //And lose the last entry
    SetLength(FDisc[sdir].Entries,Length(FDisc[sdir].Entries)-1);
    //And then add to the destination
    Result:=Length(FDisc[ddir].Entries);
    SetLength(FDisc[ddir].Entries,Length(FDisc[ddir].Entries)+1);
    //Update the parent
    file_details.Parent:=directory;
    if file_details.DirRef<>-1 then FDisc[file_details.DirRef].Parent:=ddir;
    //Write to the array
    FDisc[ddir].Entries[Result]:=file_details;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Reads the Free Space Map
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadAmigaFSM;
var
 buffer   : TDIByteArray;
 fsmlist  : TFragmentArray;
 hashptr,
 fragptr,
 discaddr,
 c,d      : Cardinal;
 bit      : Byte;
begin
 //UpdateProgress('Reading Free Space Map');
 //Set up the variables
 free_space[0]:=0;
 secspertrack:=22;//Not used anywhere else
 SetLength(free_space_map,1);
 SetLength(free_space_map[0],disc_size[0]div(secsize*secspertrack));
 for c:=0 to Length(free_space_map[0])-1 do
 begin
  //Number of sectors per track
  SetLength(free_space_map[0,c],secspertrack);
  //Set them all to be used, for now.
  for d:=0 to Length(free_space_map[0,c])-1 do free_space_map[0,c,d]:=$FF;
 end;
 //Set the first two sectors as system
 free_space_map[0,0,0]:=$FE;
 free_space_map[0,0,1]:=$FE;
 AmigaFillFreeSpaceMap(disc_size[0]-1,$00);
 //Read in the Free Space Map
 fsmlist:=AmigaReadBitmap(buffer);
 //Did we get anything? Mark off the systems areas on our copy
 if Length(fsmlist)>0 then
  for c:=0 to Length(fsmlist) do
   AmigaFillFreeSpaceMap(fsmlist[c].Offset*secsize,$FE);
 //Mark out the rest of our copy of the FSM
 if Length(buffer)>0 then
 begin
  inc(free_space[0],secsize*2);//Take account of the boot block
  //So, start at the beginning
  hashptr:=0;
  //And get each 32 bit word
  while hashptr<Length(buffer) do
  begin
   //Read it in
   fragptr:=Read32b(hashptr,buffer,True);
   //Go through each bit
   for bit:=0 to 31 do
    if IsBitSet(fragptr,bit) then
    begin
     //Calculate where on the disc this will be
     discaddr:=secsize*(2+(hashptr*8)+bit);
     inc(free_space[0],secsize);//Set, so is free
     AmigaFillFreeSpaceMap(discaddr,$00);
    end;
   inc(hashptr,4);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Fill part of the FSM with a byte
-------------------------------------------------------------------------------}
procedure TDiscImage.AmigaFillFreeSpaceMap(address: Cardinal;usage: Byte);
var
 t,s: Cardinal;
begin
 t:=0;
 s:=0;
 //Track
 t:=address div (secspertrack*secsize);
 //Sector
 if t>0 then
  s:=(address mod (t*secspertrack*secsize))div secsize
 else
  s:=address div secsize;
 //Make sure we haven't overshot the end of the disc
 if t<Length(free_space_map[0]) then
  //Or the end of the current track
  if s<Length(free_space_map[0,t]) then
   //Set the chunk as system
   free_space_map[0,t,s]:=usage;
end;

{-------------------------------------------------------------------------------
Convert Delphi time to Amiga time
-------------------------------------------------------------------------------}
procedure TDiscImage.ToAmigaTime(time: TDateTime;var days,mins,ticks: Cardinal);
var
 hour,
 min,
 sec,
 ms   : Word;
begin
 days:=DaysBetween(time,EncodeDate(1978,1,1));
 DecodeTime(time,hour,min,sec,ms);
 mins:=hour*60+min;
 ticks:=sec*50+(ms div 20);
end;

{-------------------------------------------------------------------------------
Convert Amiga time to Delphi time
-------------------------------------------------------------------------------}
function TDiscImage.FromAmigaTime(days, mins, ticks: Cardinal): TDateTime;
begin
 Result:=EncodeDate(1978,1,1)+days
        +EncodeTime(mins div 60,mins mod 60,ticks div 50,ticks mod 50);
end;

{-------------------------------------------------------------------------------
Convert an attribute number to a string
-------------------------------------------------------------------------------}
function TDiscImage.AmigaIntToStrAttr(attr: Cardinal): String;
var
 a: Byte;
const
 attributes: array[0..31] of Char =
  ('D','E','W','R','A','P','S','H',
   'd','e','w','r','l','x','i','a',
   ' ',' ',' ',' ',' ',' ',' ',' ',
   ' ',' ',' ',' ',' ',' ',' ',' ');
begin
 Result:='';
 for a:=0 to 31 do
  if IsBitSet(attr,a) then
   Result:=Result+attributes[a];
 RemoveSpaces(Result);
end;

{-------------------------------------------------------------------------------
Convert an attribute string to a number
-------------------------------------------------------------------------------}
function TDiscImage.AmigaStrToIntAttr(attr: String): Cardinal;
var
 a: Byte;
const
 attributes: array[0..31] of Char =
  ('D','E','W','R','A','P','S','H',
   'd','e','w','r','l','x','i','a',
   ' ',' ',' ',' ',' ',' ',' ',' ',
   ' ',' ',' ',' ',' ',' ',' ',' ');
begin
 Result:=0;
 for a:=0 to 31 do
  if Pos(attributes[a],attr)>0 then
   Result:=Result+1<<a;
end;

{-------------------------------------------------------------------------------
Calculate the hash value for a filename
-------------------------------------------------------------------------------}
function TDiscImage.AmigaCalculateHashValue(filename: String): Cardinal;
var
 index: Byte;
begin
 Result:=Length(filename);
 filename:=UpperCase(filename);
 for index:=1 to Length(filename) do
 begin
  Result:=Result*13;
  Result:=Result+Ord(filename[index]);
  Result:=Result AND $7FF;
  Result:=Result;
 end;
 Result:=Result mod((secsize div 4)-56);
 Result:=Result;
end;

{-------------------------------------------------------------------------------
Mark the appropriate bit in the supplied FSM
-------------------------------------------------------------------------------}
procedure TDiscImage.AmigaAllocateFSMBlock(addr: Cardinal;used: Boolean;
                                                         var fsm: TDIByteArray);
var
 bmpoffset: Cardinal;
 bit,use  : Byte;
begin
 if used then use:=0 else use:=1;
 //Get the offsets
 bmpoffset:=GetAmigaFSMOffset(addr,bit);
 //Set/clear the bit
 WriteBits(use,bmpoffset,bit,1,fsm);
end;

{-------------------------------------------------------------------------------
Calculate the byte and bit offsets given a sector address
-------------------------------------------------------------------------------}
function TDiscImage.GetAmigaFSMOffset(addr: Cardinal;var bit: Byte): Cardinal;
begin
 //Work out the bit offset
 bit:=(addr-2)mod 32;
 bit:=24-((bit div 8)*8)+(bit mod 8);
 //And the byte offset
 Result:=((addr-2)div 32)*4;
end;

{-------------------------------------------------------------------------------
Read an FSM from the disc to the supplied buffer
-------------------------------------------------------------------------------}
function TDiscImage.AmigaReadBitmap(var fsm: TDIByteArray): TFragmentArray;
var
 hashptr,
 fragptr : Cardinal;
begin
 Result:=nil;
 //Initialise the arrays
 SetLength(fsm,0);
 SetLength(Result,0);
 //Start at where the root pointer is pointing to
 hashptr:=(root*secsize)+$13C;
 //Dummy to make sure the loop fires
 fragptr:=$FFFFFFFF;
 while fragptr<>0 do
 begin
  //Read in the pointer to the next fragment
  fragptr:=Read32b(hashptr,True);
  //If it is not zero
  if fragptr<>0 then
  begin
   //Add to the fragment array
   SetLength(Result,Length(Result)+1);
   Result[Length(Result)-1].Offset:=fragptr;
   //Increase our buffer length
   SetLength(fsm,Length(fsm)+(secsize-4));
   //Read in the data
   ReadDiscData((fragptr*secsize)+4,secsize-4,0,Length(fsm)-(secsize-4),fsm);
   //Move onto the next hash pointer
   inc(hashptr,4);
   //If we reach the end of the root table, move onto the extended block
   if hashptr=(root*secsize)+$1A0 then hashptr:=Read32b(hashptr,True)*secsize;
  end;
 end;
 //Adjust the buffer length to match the disc size
 if Length(fsm)>Ceil((disc_size[0]div secsize)/32)*4 then
  SetLength(fsm,Ceil((disc_size[0]div secsize)/32)*4);
end;

{-------------------------------------------------------------------------------
Write a supplied FSM to the disc
-------------------------------------------------------------------------------}
procedure TDiscImage.AmigaWriteBitmap(fsmlist: TFragmentArray;var fsm: TDIByteArray);
var
 index: Cardinal;
begin
 for index:=0 to Length(fsm)-1 do
  WriteByte(fsm[index],fsmlist[index div$1FC].Offset*secsize+4+(index mod$1FC));
 //Sort out the checksums
 for index:=0 to Length(fsmlist)-1 do
  Write32b(AmigaChecksum(fsmlist[index].Offset*secsize),fsmlist[index].Offset*secsize,True);
end;

{-------------------------------------------------------------------------------
Find and allocate some space for data
-------------------------------------------------------------------------------}
function TDiscImage.AmigaFindFreeSpace(filelen: Cardinal): TFragmentArray;
var
 fsm    : TDIByteArray;
 fsmlist: TFragmentArray;
 ptr,
 count,
 offset,
 test   : Cardinal;
 bit    : Byte;
 direct : Boolean;
begin
 Result:=nil;
 //Initialise the return variable
 SetLength(Result,0);
 //Is there actually enough space?
 if free_space[0]>=filelen then
 begin
  //Get the FSM
  fsmlist:=AmigaReadBitmap(fsm);
  //Find enough blocks for the data
  count:=0;
  ptr:=root-1;
  direct:=False; //Going down
  while count<filelen do
  begin
   //Get the byte and bit offset into the FSM
   //offset:=GetAmigaFSMOffset(ptr,bit);
   offset:=((ptr-2)div 32)*4;
   bit:=(ptr-2)mod 32;
   //Is the bit set?
   test:=1<<bit;
   if Read32b(offset,fsm,True)AND test=test then
   begin
    //Add to the fragment array
    SetLength(Result,Length(Result)+1);
    Result[Length(Result)-1].Offset:=ptr;
    //Add to the counter
    inc(count,secsize);
    //Alloate each block
    AmigaAllocateFSMBlock(ptr,True,fsm);
   end;
   //Next block
   if not direct then dec(ptr) else inc(ptr);
   //Hit the start, then change direction
   if ptr=2 then
   begin
    ptr:=root+1;
    direct:=True;
   end;
  end;
  //Write the FSM back
  AmigaWriteBitmap(fsmlist,fsm);
 end;
end;

{-------------------------------------------------------------------------------
Date/Time stamp a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateAmigaTimeStamp(filename: String;newtimedate: TDateTime): Boolean;
var
 dir,
 entry,
 days,
 mins,
 ticks : Cardinal;
begin
 Result:=False;
 //Make sure the file actually exists
 if FileExists(filename,dir,entry) then
 begin
  //Convert the date/time into Amiga
  ToAmigaTime(newtimedate,days,mins,ticks);
  //Write the Date and Time
  Write32b(days ,FDisc[dir].Entries[entry].Sector*secsize+$1A4,True);
  Write32b(mins ,FDisc[dir].Entries[entry].Sector*secsize+$1A8,True);
  Write32b(ticks,FDisc[dir].Entries[entry].Sector*secsize+$1AC,True);
  //Update the checksum
  Write32b(AmigaChecksum(FDisc[dir].Entries[entry].Sector*secsize),
           FDisc[dir].Entries[entry].Sector*secsize+$14,True);
  //Update our local copy
  FDisc[dir].Entries[entry].TimeStamp:=newtimedate;
  //Return a positive result
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Get the chain links for an object
-------------------------------------------------------------------------------}
function TDiscImage.GetAmigaChain(sector: Cardinal): TFragmentArray;
var
 hashptr,
 len,
 source   : Cardinal;
begin
 //Set up the variables
 Result:=nil;
 SetLength(Result,0);
 hashptr:=$134;
 source:=$FFFFFFFF; //Make sure this is not zero
 while(source<>0)and(sector<>0)do //Continue until the termination block
 begin
  //Confirm the checksum
  if Read32b(sector*secsize+$14,True)=AmigaChecksum(sector*secsize) then
  begin
   //Get the source and length of the next set of data
   if FMap then //FFS
   begin
    source:=Read32b(sector*secsize+hashptr,True)*secsize;//Source of data
    len:=secsize;//Amount of data
   end
   else //OFS
   begin//We can do some sanity checks here, to confirm this is the right block
    source:=Read32b(sector*secsize+hashptr,True)*secsize; //Read the pointer
    if source<>0 then //Not zero, then valid
     //Confirm the checksum
     if Read32b(source+$14,True)=AmigaChecksum(source) then
     begin
      len:=Read32b(source+$C,True); //Get the length
      inc(source,$18); //Data is here
     end else exit; //Checksums don't match, so exit
   end;
   if len=0 then exit;//If the length returns zero, then quit
   if(len>0)and(source<>0)then
   begin
    //Add a new entry
    SetLength(Result,Length(Result)+1);
    //And make a note of this link
    Result[Length(Result)-1].Offset:=source div secsize;
    Result[Length(Result)-1].Length:=len;
    //Get the next block pointer
    dec(hashptr,4);
    if hashptr=$14 then //End of hash table, need to get the next link
    begin
     //Next extended file pointer table
     sector:=Read32b(sector*secsize+$1F8,True);
     hashptr:=$134;
    end;
   end;
  end else exit; //Checksum doesn't match, so exit
 end;
end;

{-------------------------------------------------------------------------------
Add an entry to a hash chain
-------------------------------------------------------------------------------}
procedure TDiscImage.AmigaAddToChain(filename: String;paraddr,sector: Cardinal);
var
 hashval,
 fragptr,
 index   : Cardinal;
begin
 //Blocks now written, add it to the parent
 hashval:=AmigaCalculateHashValue(filename);//Calculate the hash value
 //Add to the parent directory
 fragptr:=$18+hashval*4;
 //Read the current value
 index:=Read32b(paraddr*secsize+fragptr,True);
 //If not zero, follow the chain until it is zero
 while index<>0 do
 begin
  paraddr:=index;
  fragptr:=$1F0;
  index:=Read32b(paraddr*secsize+fragptr,True);
 end;
 //Now we have an address at the end of the chain.
 Write32b(sector,paraddr*secsize+fragptr,True);
 //Update the checksum
 Write32b(AmigaChecksum(paraddr*secsize),paraddr*secsize+$14,True);
end;

{-------------------------------------------------------------------------------
Remove an entry from a hash chain
-------------------------------------------------------------------------------}
function TDiscImage.AmigaRemoveFromChain(filename: String;paraddr,sector: Cardinal):Boolean;
var
 hashval,
 link    : Cardinal;
begin
 Result:=False;
 //Calulate the hash value
 hashval:=AmigaCalculateHashValue(filename)*4+$18;
 link:=Read32b(paraddr*secsize+hashval,True);//Start at the parent hash table
 while(link<>sector)and(link<>0)do
 begin
  paraddr:=link;
  hashval:=$1F0;
  link:=Read32b(paraddr*secsize+hashval,True);//Now we check the chain link
 end;
 //Have we found our entry?
 if link<>0 then
 begin
  //Copy the chain link from our file to this, thereby bypassing our header
  Write32b(Read32b(sector*secsize+$1F0,True),paraddr*secsize+hashval,True);
  //Don't forget the checksum
  Write32b(AmigaChecksum(paraddr*secsize),paraddr*secsize+$14,True);
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Validate an Amiga filename
-------------------------------------------------------------------------------}
procedure TDiscImage.ValidateAmigaFile(var filename: String);
var
 index: Integer;
const
 illegal = '/:';
begin
 //Make sure it is no longer than 30 characters long
 filename:=LeftStr(filename,30);
 if Length(filename)>0 then //Change illegal characters
  for index:=1 to Length(filename) do
   if Pos(filename[index],illegal)>0 then filename[index]:='_';
 //If nothing was supplied, then supply something
 if Length(filename)=0 then filename:='Unnamed';
end;

{-------------------------------------------------------------------------------
Produce a report of the image's details
-------------------------------------------------------------------------------}
function TDiscImage.AmigaReport(CSV: Boolean): TStringList;
var
 temp: String;
begin
 Result:=TStringList.Create;
 if FMap then temp:='Fast File System' else temp:='Original File System';
 if FDirType=diAmigaDir   then temp:=temp+' AmigaDOS Directory';
 if FDirType=diAmigaCache then temp:=temp+' AmigaDOS Directory Cache';
 Result.Add(temp);
 Result.Add('Sector Size: '+IntToStr(secsize)+' bytes');
 temp:=IntToStr(density);
 case density of
  0: temp:='Hard Drive';
  1: temp:='Single';
  2: temp:='Double';
  4: temp:='Quad';
  8: temp:='Octal';
 end;
 Result.Add('Density: '+temp);
 Result.Add('Root Address: 0x'+IntToHex(root,8));
 Result.Add('Disc Size: '+IntToStr(disc_size[0])+' bytes');
 Result.Add('Free Space: '+IntToStr(free_space[0])+' bytes');
 Result.Add('Disc Name: '+disc_name[0]);
end;
