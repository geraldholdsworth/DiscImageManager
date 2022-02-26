//++++++++++++++++++ Published Methods +++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Create the instance
-------------------------------------------------------------------------------}
constructor TDiscImage.Create;
begin
 inherited;
 //This just sets all the global and public variables to zero, or blank.
 ResetVariables;
 SetDataLength(0);
// SetLength(Fpartitions,0);
 //ADFS Interleaving option
 FForceInter:=0;
 //Deal with Spark archives as a filing system (i.e. in this class)
 FSparkAsFS:=True;
 //Allow DFS images which report number of sectors as zero
 FDFSzerosecs:=False;
 //Allow files to go beyond the edge of the disc
 FDFSBeyondEdge:=False;
 //Allow blank filenames in DFS
 FDFSAllowBlank:=False;
end;
constructor TDiscImage.Create(Clone: TDiscImage);
var
 index: Cardinal;
begin
 inherited Create;
 //Reset the variables to default
 ResetVariables;
 SetDataLength(Length(Clone.RAWData));
 //Copy the raw data across
 for index:=0 to Length(Fdata)-1 do
  Fdata[index]:=Clone.RAWData[index];
 //ADFS Interleaving option
 FForceInter  :=Clone.InterleaveMethod;
 //Deal with Spark archives as a filing system (i.e. in this class)
 FSparkAsFS   :=Clone.SparkAsFS;
 //Allow DFS images which report number of sectors as zero
 FDFSzerosecs :=Clone.AllowDFSZeroSectors;
 //Filename
 FFilename    :=Clone.Filename;
 //Just read the data in
 If IDImage then ReadImage;
 //And set the filename
 imagefilename:=FFilename;
end;

{-------------------------------------------------------------------------------
Free the instance
-------------------------------------------------------------------------------}
destructor TDiscImage.Destroy;
begin
 Close;
 inherited;
end;

{-------------------------------------------------------------------------------
Calculate a CRC-32 for a file
-------------------------------------------------------------------------------}
function TDiscImage.GetFileCrc(filename: String;entry:Cardinal=0): String;
var
 buffer: TDIByteArray;
begin
 Result:='error';
 if ExtractFile(filename,buffer,entry) then Result:=GetCRC(buffer);
end;

{-------------------------------------------------------------------------------
Attempt to fix directories (entry point)
-------------------------------------------------------------------------------}
function TDiscImage.FixDirectories: Boolean;
begin
 Result:=False;
 //Only for ADFS
 if FFormat>>4=diAcornADFS then Result:=FixBrokenADFSDirectories;
end;

{-------------------------------------------------------------------------------
Construct a save filter
-------------------------------------------------------------------------------}
function TDiscImage.SaveFilter(var FilterIndex: Integer;thisformat: Integer=-1):String;
var
 currentformat,
 queryformat   : Byte;
 ext           : String;
 index         : Integer;
begin
 Result:='';
 //Save the current format
 currentformat:=FFormat;
 index:=0;
 for queryformat:=$00 to $8F do
 begin
  //Set the format
  FFormat:=queryformat;
  //Get the current extension
  ext:=FormatToExt;
  //If it is empty, skip
  if ext<>'' then
  begin
   //If the current string is not empty, add a delimiter
   if Result<>'' then Result:=Result+'|';
   //Add the format string and extension
   ext:=FormatToString+'|*.'+ext;
   Result:=Result+ext;
   //Next entry
   inc(index);
   //Set the filter index, if we are at the current format
   if(currentformat=queryformat)and(thisformat=-1)then FilterIndex:=index;
   if(thisformat=queryformat)and(thisformat<>-1)then FilterIndex:=index;
  end;
 end;
 //Restore the current format
 FFormat:=currentformat;
end;

{-------------------------------------------------------------------------------
Load an image from a file, unGZIPping it, if necessary
-------------------------------------------------------------------------------}
function TDiscImage.LoadFromFile(filename: String;readdisc: Boolean=True): Boolean;
begin
 Result:=False;
 //Only read the file in if it actually exists (or rather, Windows can find it)
 if SysUtils.FileExists(filename) then
 begin
  //Make a note of the file being read in
  FFilename:=filename;
  //Free up the Spark instance, if used
  if FFormat>>4=diSpark then SparkFile.Free;
  //Blank off the variables
  ResetVariables;
  //Read the file in, uncompressing if need be
  FData:=Inflate(filename);
  //ID the image
  if(IDImage)and(readdisc)then ReadImage;
  //Return a true or false, depending on if FFormat is set.
  Result:=FFormat<>diInvalidImg;
  //Set the image filename, if a valid image
  If Result then imagefilename:=filename;
 end;
end;

{-------------------------------------------------------------------------------
ID an image
-------------------------------------------------------------------------------}
function TDiscImage.IDImage: Boolean;
begin
 //Reset the variables
 ResetVariables;
 //This check is done in the ID functions anyway, but we'll do it here also
 if GetDataLength>0 then
 begin
  //ID the type of image, from the data contents
  //These need to be ID-ed in a certain order as one type can look like another
  if not ID_ADFS     then //Acorn ADFS
  if not ID_AFS      then //AFS
  if not ID_CDR      then //Commodore
  if not ID_Amiga    then //Amiga
  if not ID_DFS      then //Acorn DFS
  if not ID_Sinclair then //Sinclair/Amstrad
  if not ID_CFS      then //Acorn CFS
  if not ID_MMB      then //MMFS
  if not ID_Spark    then //Spark archive
  if not ID_DOSPlus  then //DOS plus
   ResetVariables;        //Reset everything
  //Just by the ID process:
  //ADFS 'F' can get mistaken for Commodore
  //Commodore and Amiga can be mistaken for DFS
 end;
 //Return a true or false result
 Result:=FFormat<>diInvalidImg;
end;

{-------------------------------------------------------------------------------
Read the disc in, depending on the format
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadImage;
var
 d: Cardinal;
begin
 case FFormat>>4 of
  diAcornDFS : FDisc:=ReadDFSDisc;     //Acorn DFS
  diAcornADFS:
   begin
    FDisc:=ReadADFSDisc;               //Acorn ADFS
    ReadAFSPartition;//Read in the AFS partition, if one is present
    ReadDOSPartition;//Read in the DOS Plus partition, if one is present
   end;
  diAcornFS  : ReadAFSPartition;       //Acorn File Server
  diCommodore: FDisc:=ReadCDRDisc;     //Commodore
  diSinclair : FDisc:=ReadSinclairDisc;//Sinclair/Amstrad
  diAmiga    : FDisc:=ReadAmigaDisc;   //Amiga
  diAcornUEF : FDisc:=ReadUEFFile;     //Acorn CFS
  diMMFS     : FDisc:=ReadMMBDisc;     //MMFS
  diSpark    : FDisc:=ReadSparkArchive;//Spark archive
  diDOSPlus  : ReadDOSPartition;       //DOS Plus
 end;
 //Find the maximum directory entries
 if FFormat>>4<>diSpark then //Not Spark, as this already provides this info
 begin
  FMaxDirEnt:=0;
  if Length(FDisc)>0 then
   for d:=0 to Length(FDisc)-1 do
    if Length(FDisc[d].Entries)>FMaxDirEnt then
     FMaxDirEnt:=Length(FDisc[d].Entries);
 end
 else FMaxDirEnt:=SparkFile.MaxDirEnt;
end;

{-------------------------------------------------------------------------------
Saves an image to a file
-------------------------------------------------------------------------------}
procedure TDiscImage.SaveToFile(filename: String;uncompress: Boolean=False);
var
 FDiscDrive: TFileStream;
 ext: String;
begin
 //Validate the filename
 ext:=ExtractFileExt(filename); //First extract the extension
 if ext='' then //If it hasn't been given an extension, then give it the default
 begin
  filename:=LeftStr(filename,Length(filename)-Length(ext));
  filename:=filename+'.'+FormatToExt;
 end;
 if FFormat>>4<>diAcornUEF then //Not CFS
 begin
  //Create the stream
  try
   FDiscDrive:=TFileStream.Create(filename,fmCreate OR fmShareDenyNone);
   //Move to the beginning of the stream
   FDiscDrive.Position:=0;
   //Read the image into the data buffer
   FDiscDrive.Write(Fdata[0],Length(Fdata));
   //Close the stream
   FDiscDrive.Free;
   //Change the image's filename
   imagefilename:=filename;
  except
   //Could not create
  end;
 end;
 if FFormat>>4=diAcornUEF then WriteUEFFile(filename,uncompress); //CFS
end;

{-------------------------------------------------------------------------------
Closes an image file
-------------------------------------------------------------------------------}
procedure TDiscImage.Close;
begin
 ResetVariables;
end;

{-------------------------------------------------------------------------------
Create and format a new disc image
-------------------------------------------------------------------------------}
function TDiscImage.FormatFDD(major,minor,tracks: Byte): Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major MOD $10;
 minor :=minor MOD $10;
 tracks:=tracks MOD 2;
 case major of
  diAcornDFS://Create DFS
   begin
    FDisc:=FormatDFS(minor,tracks);
    Result:=Length(FDisc)>0;
   end;
  diAcornADFS://Create ADFS
   begin
    FDisc:=FormatADFSFloppy(minor);
    Result:=Length(FDisc)>0;
   end;
  diCommodore://Create Commodore 64/128
   begin
    FDisc:=FormatCDR(minor);
    Result:=Length(FDisc)>0;
   end;
  diSinclair://Create Sinclair/Amstrad
   begin
    FDisc:=FormatSpectrum(minor);
    Result:=Length(FDisc)>0;
   end;
  diAmiga://Create AmigaDOS
   begin
    FDisc:=FormatAmiga(minor);
    Result:=Length(FDisc)>0;
   end;
  diAcornUEF://Create CFS
   begin
    FDisc:=FormatCFS;
    Result:=Length(FDisc)>0;
   end;
  diDOSPlus://Create DOS or DOS Plus
   begin
    FDisc:=FormatDOS(minor);
    Result:=Length(FDisc)>0;
   end;
 end;
end;

{-------------------------------------------------------------------------------
Create and format a new hard disc image
-------------------------------------------------------------------------------}
function TDiscimage.FormatHDD(major:Byte;harddrivesize:Cardinal;newmap:Boolean;
                                                          dirtype:Byte):Boolean;
begin
 Result:=False;
 case major of
  diAcornADFS: //Create ADFS
   begin
    FDisc:=FormatADFSHDD(harddrivesize,newmap,dirtype);
    Result:=Length(FDisc)>0;
   end;
  diAcornFS: Result:=FormatAFS(harddrivesize,dirtype);//Create Acorn FS
 end;
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path (CFS, entry is entry number)
-------------------------------------------------------------------------------}
function TDiscImage.ExtractFile(filename:String;var buffer:TDIByteArray;
                                entry:Cardinal=0): Boolean;
begin
 //Start with a false result
 Result:=False;
 case FFormat>>4 of
  diAcornDFS :Result:=ExtractDFSFile(filename,buffer);     //Extract DFS
  diAcornADFS:Result:=ExtractADFSFile(filename,buffer);    //Extract ADFS
  diCommodore:Result:=ExtractCDRFile(filename,buffer);     //Extract Commodore 64/128
  diSinclair :Result:=ExtractSpectrumFile(filename,buffer);//Extract Sinclair/Amstrad
  diAmiga    :Result:=ExtractAmigaFile(filename,buffer);   //Extract AmigaDOS
  diAcornUEF :Result:=ExtractCFSFile(entry,buffer);        //Extract CFS
  diSpark    :Result:=ExtractSparkFile(filename,buffer);   //Extract Spark
  diAcornFS  :Result:=ExtractAFSFile(filename,buffer);     //Extract Acorn FileStore
  diDOSPlus  :Result:=ExtractDOSFile(filename,buffer);     //Extract DOS Plus
 end;
end;

{-------------------------------------------------------------------------------
Save a file into the disc image, from buffer
-------------------------------------------------------------------------------}
function TDiscImage.WriteFile(var file_details: TDirEntry; var buffer: TDIByteArray): Integer;
var
 count : Integer;
begin
 //Start with a false result
 Result:=-2; //Error - disc full
 //Get the length of data to be written
 count:=file_details.Length;
 //Only write a file if there is actually any data to be written
 if(count>0)and(Length(free_space)>0)then
 begin
  file_details.Side:=file_details.Side mod Length(free_space);
  //Can only write a file that will fit on the disc, or CFS
  if(count<=free_space[file_details.Side])or(FFormat>>4=diAcornUEF)then
   case FFormat>>4 of
    diAcornDFS :Result:=WriteDFSFile(file_details,buffer);     //Write DFS
    diAcornADFS:Result:=WriteADFSFile(file_details,buffer);    //Write ADFS
    diCommodore:Result:=WriteCDRFile(file_details,buffer);     //Write Commodore 64/128
    diSinclair :Result:=WriteSpectrumFile(file_details,buffer);//Write Sinclair/Amstrad
    diAmiga    :Result:=WriteAmigaFile(file_details,buffer);   //Write AmigaDOS
    diAcornUEF :Result:=WriteCFSFile(file_details,buffer);     //Write CFS
    diAcornFS  :Result:=WriteAFSFile(file_details,buffer);     //Write Acorn FS
    diDOSPlus  :Result:=WriteDOSFile(file_details,buffer);     //Write DOS Plus
   end;
 end
 else Result:=-8; //Error - zero length file
end;

{-------------------------------------------------------------------------------
Create a directory
-------------------------------------------------------------------------------}
function TDiscImage.CreateDirectory(var filename,parent,attributes: String): Integer;
begin
 //Start with a false result
 Result:=-1;
 case FFormat>>4 of
  diAcornDFS : exit;//Can't create directories on DFS
  diAcornADFS:      //Create directory on ADFS
    Result:=CreateADFSDirectory(filename,parent,attributes);
  diCommodore: exit;//Can't create directories on Commodore
  diSinclair : exit;//Can't create directories on Sinclair/Amstrad
  diAmiga    :      //Create directory on AmigaDOS
    Result:=CreateAmigaDirectory(filename,parent,attributes);
  diAcornUEF : exit;//Can't create directories on CFS
  diAcornFS  :      //Create directory on Acorn FS
    Result:=CreateAFSDirectory(filename,parent,attributes);
  diDOSPlus  :      //Create directory on DOS Plus
    Result:=CreateDOSDirectory(filename,parent,attributes);
 end;
end;

{-------------------------------------------------------------------------------
Retitle a directory
-------------------------------------------------------------------------------}
function TDiscImage.RetitleDirectory(var filename,newtitle: String): Boolean;
begin
 //Start with a false result
 Result:=False;
 case FFormat>>4 of
  diAcornDFS :      //DFS doesn't have directories
  begin
   //Update the disc title instead
   if(filename[1]=':')and(filename[3]='.')and(filename[4]='$')then
    Result:=UpdateDiscTitle(newtitle,StrToIntDef(filename[2],0));
  end;
  diAcornADFS:      //Retitle ADFS directory
    Result:=RetitleADFSDirectory(filename,newtitle);
  diCommodore: exit;//Commodore doesn't have directories
  diSinclair : exit;//Sinclair/Amstrad doesn't have directories
  diAmiga    :      //Retitle AmigaDOS directory
    Result:=RetitleAmigaDirectory(filename,newtitle);
  diAcornUEF : exit;//CFS doesn't have directories
  diAcornFS  : exit;//Can't retitle AFS directories
  diDOSPlus  : exit;//Can't retitle DOS directories
 end;
end;

{-------------------------------------------------------------------------------
Does a file exist?
-------------------------------------------------------------------------------}
function TDiscImage.FileExists(filename: String;var Ref: Cardinal): Boolean;
var
 dir,entry: Cardinal;
begin
 dir:=$FFFF;
 entry:=$FFFF;
 Result:=FileExists(filename,dir,entry);
 Ref:=dir*$10000+entry;
end;
function TDiscImage.FileExists(filename: String;var dir,entry: Cardinal): Boolean;
var
 Path   : array of String;
 i,j,l,
 ptr,
 level  : Integer;
 test,
 test2  : String;
begin
 //This will not work with CFS as you can have multiple files with the same name
 //in the same 'directory'. It will just find the first occurance.
 Result:=False;
 //For the root, we'll just return a default value
 if filename=root_name then
 begin
  dir:=$FFFF;
  entry:=$FFFF;
  Result:=True;
  exit;
 end;
 //AFS or DOS Root
 if(filename=afsrootname)or(filename=dosrootname)then
  if Length(FDisc)>0 then
  begin
   i:=0;
   //Just look for the root
   while(i<Length(FDisc))and(FDisc[i].Directory<>filename)do inc(i);
   if FDisc[i].Directory=filename then
   begin
    dir:=i;
    entry:=$FFFF;
    Result:=True;
    exit;
   end;
  end;
 //Not going to search if there is no tree to search in
 if(Length(FDisc)>0)and(filename<>'')then//or if there is nothing being searched for
 begin
  SetLength(Path,0);
  //Explode the pathname into an array, without the '.'
  if(FFormat>>4<>diAcornDFS)and(FFormat>>4<>diCommodore)then //Not DFS or Commodore
   Path:=filename.Split(dir_sep);
  if FFormat>>4=diAcornDFS then //With DFS, we need the initial root name, including the '.'
  begin
   //So should only be 2 entries
   SetLength(Path,2);
   //But supplied filename may not contain the root
   if Pos(root_name+dir_sep,filename)>0 then
   begin
    //Root name
    Path[0]:=Copy(filename,0,Pos(root_name+dir_sep,filename));
    //And filename
    Path[1]:=Copy(filename,Pos(root_name+dir_sep,filename)
                          +Length(root_name)+Length(dir_sep),Length(filename));
   end
   else
   begin
    //If it doesn't, we will assume the root of side 0
    Path[0]:=FDisc[0].Directory;
    //And make the second entry the filename, after the '.', if it has one
    if Pos(dir_sep,filename)>0 then
     Path[1]:=Copy(filename,Pos(dir_sep,filename)+Length(dir_sep))
    else
     Path[1]:=filename;
   end;
  end;
  if FFormat>>4=diCommodore then //Commodore is similar to DFS
  begin
   //So should only be 2 entries
   SetLength(Path,2);
   //But supplied filename may not contain the root
   if Pos(root_name+dir_sep,filename)>0 then
   begin
    //Root name
    Path[0]:=Copy(filename,0,Pos(root_name+dir_sep,filename));
    //And filename
    Path[1]:=Copy(filename,Pos(root_name+dir_sep,filename)
                          +Length(root_name)+Length(dir_sep),Length(filename));
   end
   else
   begin
    Path[0]:=root_name;
    Path[1]:=filename;
   end;
  end;
  //If there is a path, then follow it
  if Length(Path)>0 then
  begin
   //Position into the Path array (i.e. directory level)
   level:=0;
   //Counters/Pointers
   i:=0;
   j:=-1;
   ptr:=-1;
   test:=UpperCase(AddTopBit(Path[level]));
   test2:=UpperCase(AddTopBit(FDisc[i].Directory));
   //Haven't matched the root, so need to look for it
   if test<>test2 then
    //So we'll continue until we find it
    while(i<Length(FDisc))and(test2<>test)do
    begin
     inc(i);
     test2:=UpperCase(AddTopBit(FDisc[i].Directory));
    end;
   //Using UpperCase makes it a case insensitive search
   if test=test2 then
    //Have we found the initial directory (usually '$')
    repeat
     //Let's move to the next directory level
     inc(level);
     //And search the entries
     j:=-1;
     repeat
      inc(j);
      test:='';
      if level<Length(Path) then
       test2:=UpperCase(AddTopBit(Path[level]))
      else
       test2:='not found';
      l:=j;
      //Just to make sure we don't search past the end of the arrays
      if (i>=0) and (i<Length(FDisc)) then
      begin
       if j<Length(FDisc[i].Entries) then
        test:=UpperCase(AddTopBit(FDisc[i].Entries[j].Filename));
       l:=Length(FDisc[i].Entries)-1;
      end;
     until (test2=test) or (j>=l);
     //So, we found the entry
     if test2=test then
     begin
      //Make a note
      ptr:=i;
      //Is it a directory? Then move down to the next level
      if (i>=0) and (i<Length(FDisc)) then
       if j<Length(FDisc[i].Entries) then
       begin
        test2:=UpperCase(AddTopBit(FDisc[i].Entries[j].Filename));
        i:=FDisc[i].Entries[j].DirRef;
        //Unless we are looking for a directory
        if level=Length(Path)-1 then
          if UpperCase(AddTopBit(Path[level]))=test2 then
           i:=-1;
       end;
     end
     else j:=-1;
    until (i=-1) or (j=-1); //End if it is not a directory, or is not found
  end;
   //Found, so return TRUE, with the references
  if j<>-1 then
  begin
   Result:=True;
   dir:=ptr;
   entry:=j;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Direct access to disc data
-------------------------------------------------------------------------------}
function TDiscImage.ReadDiscData(addr,count,side,offset: Cardinal;
                                             var buffer: TDIByteArray): Boolean;
var
 i   : Cardinal;
begin
 Result:=False;
 if count>0 then //Make sure there is something to read
 begin
  //Simply copy from source to destination
  for i:=0 to count-1 do
  begin
   if offset+i<Length(buffer) then //Bit of range checking
   begin
    if FFormat>>4<>diAcornDFS then //All but DFS
     buffer[offset+i]:=ReadByte(addr+i);
    if FFormat>>4=diAcornDFS then  //DFS only
     buffer[offset+i]:=ReadByte(ConvertDFSSector(addr+i,side));
   end;
  end;
  //Return a success if we didn't go out of range
  Result:=offset+count<=Length(buffer);
 end;
end;

{-------------------------------------------------------------------------------
Direct access writing to disc
-------------------------------------------------------------------------------}
function TDiscImage.WriteDiscData(addr,side: Cardinal;var buffer: TDIByteArray;
                                    count: Cardinal;start: Cardinal=0): Boolean;
var
 i   : Cardinal;
begin
 Result:=False;
 if(count=0)or(Length(disc_size)=0)then exit;
 //Make sure the numbers fit
 if start+count<=Length(buffer) then
 begin
  //Sometimes the image file is smaller than the actual disc size
  if GetDataLength<disc_size[side]then SetDataLength(disc_size[side]);
  if FFormat>>4<>diAcornDFS then //not DFS
  begin
   //Ensure that the entire block will fit into the available space
   Result:=(addr+count)<=GetDataLength;
   //Simply copy from source to destination
   if Result then
    for i:=0 to count-1 do
     WriteByte(buffer[start+i],addr+i);
  end
  else //DFS
  begin
   //Ensure that the entire block will fit into the available space
   Result:=ConvertDFSSector(addr+count,side)<=GetDataLength;
   //Simply copy from source to destination
   if Result then
    for i:=0 to count-1 do
     WriteByte(buffer[start+i],ConvertDFSSector(addr+i,side));
  end;
 end;
end;

{-------------------------------------------------------------------------------
Searches for a file, and returns the result in a TSearchResults
-------------------------------------------------------------------------------}
function TDiscImage.FileSearch(search: TDirEntry): TSearchResults;
//Comparison functions...saves a lot of if...then statements
 function CompStr(S1,S2: String): Byte; //Compares Strings
 begin
  Result:=0;
  if S1<>'' then
   if CompareString(S2,S1,False) then Result:=1;
 end;
 function CompCar(S1,S2: Cardinal): Byte; //Compares Cardinals/Integers
 begin
  Result:=0;
  if(S1=S2)and(S1<>0)then Result:=1;
 end;
 function CompTSt(S1,S2: TDateTime): Byte; //Compares TDateTime
 begin
  Result:=0;
  if(S1=S2)and(S1<>0) then Result:=1;
 end;
//Function proper starts here
var
 dir,
 entry  : Integer;
 found,
 target : Byte;
 Ldirsep : Char;
begin
 Result:=nil;
 //Reset the search results array to empty
 SetLength(Result,0);
 //Has the complete path been included in the Filename?
 if(Pos(GetDirSep(0),search.Filename)>0)
 or(Pos(GetDirSep(1),search.Filename)>0)then
 begin
  if Pos(GetDirSep(0),search.Filename)>0 then Ldirsep:=GetDirSep(0)
  else if Pos(GetDirSep(1),search.Filename)>0 then Ldirsep:=GetDirSep(1);
  //Split filename into parent and filename
  if FFormat<>diAcornDFS then //Not DFS
  begin
   target:=Length(search.Filename);
   //Look for the last directory separator
   while(search.Filename[target]<>Ldirsep)and(target>1)do
    dec(target);
   //And split into parent and filename
   search.Parent:=LeftStr(search.Filename,target-1);
   search.Filename:=Copy(search.Filename,target+1);
  end;
  if FFormat=diAcornDFS then //DFS only
  begin
   //Is there a drive specifier, with root? :0.$ or :2.$
   if(search.Filename[1]=':')and(Copy(search.Filename,3,2)='.$')then
   begin
    search.Parent  :=LeftStr(search.Filename,4);
    search.Filename:=Copy(search.Filename,6);
   end;
   //Is there just a root?
   if Copy(search.Filename,1,2)='$.' then
   begin
    search.Parent  :='$.';
    search.Filename:=Copy(search.Filename,3);
   end;
   //After these, is the filename left blank?
   if search.Filename='' then
   begin
    search.Filename:=search.Parent;
    search.Parent  :='';
   end;
  end;
 end;
 //Work out what the search target is
 target:=0;
 //The search works by heading for a target. If the specified target is met
 //(i.e. all given fields match), then the search is a success.
 with search do
 begin
  if Parent       <>'' then inc(target);
  if Filename     <>'' then inc(target);
  if Attributes   <>'' then inc(target);
  if Filetype     <>'' then inc(target);
  if ShortFiletype<>'' then inc(target);
  if LoadAddr  <>$0000 then inc(target);
  if ExecAddr  <>$0000 then inc(target);
  if Length    <>$0000 then inc(target);
  if Side      <>$0000 then inc(target);
  if Track     <>$0000 then inc(target);
  if Sector    <>$0000 then inc(target);
  if DirRef    <>$0000 then inc(target);
  if TimeStamp <>0     then inc(target);
 end;
 //Only seach if there is something to search in
 if Length(FDisc)>0 then
  for dir:=0 to Length(FDisc)-1 do
   //Only search directories which have children
   if Length(FDisc[dir].Entries)>0 then
    for entry:=0 to Length(FDisc[dir].Entries)-1 do
    begin
     //Found counter
     found:=0;
     //Search parameters
     with FDisc[dir].Entries[entry] do
     begin
      inc(found,CompStr(search.Parent,       Parent));
      inc(found,CompStr(search.Filename,     Filename));
      inc(found,CompStr(search.Attributes,   Attributes));
      inc(found,CompStr(search.Filetype,     Filetype));
      inc(found,CompStr(search.ShortFiletype,ShortFiletype));
      inc(found,CompCar(search.LoadAddr,     LoadAddr));
      inc(found,CompCar(search.ExecAddr,     ExecAddr));
      inc(found,CompCar(search.Length,       Length));
      inc(found,CompCar(search.Side,         Side));
      inc(found,CompCar(search.Track,        Track));
      inc(found,CompCar(search.Sector,       Sector));
      inc(found,CompCar(search.DirRef,       DirRef));
      inc(found,CompTSt(search.TimeStamp,    TimeStamp));
     end;
     //Have we hit the target?
     //found is the number of matches found, and target is what we're aiming for
     if found=target then
     begin
      //Yes, so add this to the results
      SetLength(Result,Length(Result)+1);
      Result[Length(Result)-1]:=FDisc[dir].Entries[entry];
      //User will still need to call FileExists to get the dir and entry references
     end;
    end;
end;

{-------------------------------------------------------------------------------
Rename a file - oldfilename is full path, newfilename has no path
-------------------------------------------------------------------------------}
function TDiscImage.RenameFile(oldfilename: String;var newfilename: String;
                                    entry:Cardinal=0): Integer;
begin
 Result:=-1;//Failed to rename
 case FFormat>>4 of
  diAcornDFS : Result:=RenameDFSFile(oldfilename,newfilename);     //Rename DFS
  diAcornADFS: Result:=RenameADFSFile(oldfilename,newfilename);    //Rename ADFS
  diCommodore: Result:=RenameCDRFile(oldfilename,newfilename);     //Rename Commodore 64/128
  diSinclair : Result:=RenameSpectrumFile(oldfilename,newfilename);//Rename Sinclair/Amstrad
  diAmiga    : Result:=RenameAmigaFile(oldfilename,newfilename);   //Rename AmigaDOS
  diAcornUEF : Result:=RenameCFSFile(entry,newfilename);           //Rename CFS
  diAcornFS  : Result:=RenameAFSFile(oldfilename,newfilename);     //Rename AFS
  diDOSPlus  : Result:=RenameDOSFile(oldfilename,newfilename);     //Rename DOS Plus
 end;
end;

{-------------------------------------------------------------------------------
Deletes a file (given full pathname)
-------------------------------------------------------------------------------}
function TDiscImage.DeleteFile(filename: String;entry: Cardinal=0): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : Result:=DeleteDFSFile(filename);     //Delete DFS
  diAcornADFS: Result:=DeleteADFSFile(filename);    //Delete ADFS
  diCommodore: Result:=DeleteCDRFile(filename);     //Delete Commodore 64/128
  diSinclair : Result:=DeleteSinclairFile(filename);//Delete Sinclair/Amstrad
  diAmiga    : Result:=DeleteAmigaFile(filename);   //Delete AmigaDOS
  diAcornUEF : Result:=DeleteCFSFile(entry);        //Delete CFS
  diAcornFS  : Result:=DeleteAFSFile(filename);     //Delete Acorn FS
  diDOSPlus  : Result:=DeleteDOSFile(filename);     //Delete DOS Plus
 end;
end;

{-------------------------------------------------------------------------------
Moves a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.MoveFile(filename,directory: String): Integer;
begin
 Result:=-12;
 case FFormat>>4 of
  diAcornDFS : Result:=MoveDFSFile(filename,directory);  //Move on DFS
  diAcornADFS: Result:=MoveADFSFile(filename,directory); //Move ADFS File
  diAmiga    : Result:=MoveAmigaFile(filename,directory);//Move Amiga File
  diAcornFS  : Result:=MoveAFSFile(filename,directory);  //Move AFS File
  diDOSPlus  : Result:=MoveDOSFile(filename,directory);  //Move DOS File
 end;
end;
function TDiscImage.MoveFile(source: Cardinal;dest: Integer): Integer;
begin
 Result:=-12;
 //Can only move files on DFS (between drives), ADFS, Amiga, AFS and CFS
 if FFormat>>4=diAcornUEF then //Move on CFS
  Result:=MoveCFSFile(source,dest);
end;

{-------------------------------------------------------------------------------
Copies a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.CopyFile(filename,directory: String): Integer;
begin
 Result:=CopyFile(filename,directory,filename);
end;
function TDiscImage.CopyFile(filename,directory,newfilename: String): Integer;
var
 buffer      : TDIByteArray;
 ptr,
 entry,
 dir,
 d,e         : Cardinal;
 tempfn,
 newparent   : String;
 file_details: TDirEntry;
begin
 ptr:=0;
 SetLength(buffer,0);
 //Need to extract the filename from the full path...and ensure the file exists
 Result:=-11; //Could not find file
 if FileExists(filename,dir,entry) then
 begin
  Result:=1; //Could not load file
  //Ensure the new location does not already have such a file
  file_details:=FDisc[dir].Entries[entry]; //Take a copy first
  file_details.Filename:=newfilename;      //The new filename
  //Are we copying a directory?
  if Fdisc[dir].Entries[entry].DirRef=-1 then //No, then continue
  begin
   //First, get the file into memory
   if ExtractFile(filename,buffer) then
   begin
    Result:=-5;//Unknown error
    //Is there a file of the same name at the new location?
    if FileExists(directory+dir_sep+file_details.Filename,ptr) then
     //Delete the old one
     DeleteFile(directory+dir_sep+file_details.Filename);
    //Set up the filedetails
    file_details.Parent:=directory;
    if FFormat>>4=diAcornDFS then //DFS
     if(directory[1]=':')and(directory[2]='2') then
      file_details.Side:=1
     else
      file_details.Side:=0;
    //Then write it back to the image
    Result:=WriteFile(file_details,buffer);
   end;
  end;
  if FDisc[dir].Entries[entry].DirRef>=0 then //Copying directory
  begin
   Result:=-10; //Can't move or copy to the same directory
   //Is there already a file/directory of that name at the destination?
   if not FileExists(directory+dir_sep+newfilename,ptr) then
   begin
    //First, create a new directory at the destination
    Result:=CreateDirectory(newfilename,directory,FDisc[dir].Entries[entry].Attributes);
    //if successful, then copy all the files
    if Result>=0 then
    begin
     //Then iterate through each entry and copy them using recursion
     d:=FDisc[dir].Entries[entry].DirRef; //Get the directory reference
     //Work out the new parent path for the files
     newparent:=directory+dir_sep+newfilename;
     //Copy the files
     if Length(FDisc[d].Entries)>0 then
      for e:=0 to Length(FDisc[d].Entries)-1 do
      begin
       tempfn:=GetParent(d)+dir_sep+FDisc[d].Entries[e].Filename;
       CopyFile(tempfn,newparent);
      end;
    end;
   end;
  end;
 end;
end;
function TDiscImage.CopyFile(source: Cardinal;dest: Integer): Integer;
begin
 Result:=-12;
 //Can only copy files on DFS (between drives), ADFS, Amiga, AFS and CFS
 if FFormat>>4=diAcornUEF then //Copy on CFS
  Result:=CopyCFSFile(source,dest);
end;

{-------------------------------------------------------------------------------
Set the attributes for a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateAttributes(filename,attributes: String;entry:Cardinal=0):Boolean;
begin
 Result:=False;
 if(filename<>root_name)and(filename<>afsrootname)then
  case FFormat>>4 of
   diAcornDFS : Result:=UpdateDFSFileAttributes(filename,attributes);     //Update DFS attributes
   diAcornADFS: Result:=UpdateADFSFileAttributes(filename,attributes);    //Update ADFS attributes
   diCommodore: Result:=UpdateCDRFileAttributes(filename,attributes);     //Update Commodore 64/128 attributes
   diSinclair : Result:=UpdateSinclairFileAttributes(filename,attributes);//Update Sinclair/Amstrad attributes
   diAmiga    : Result:=UpdateAmigaFileAttributes(filename,attributes);   //Update AmigaDOS attributes
   diAcornUEF : Result:=UpdateCFSAttributes(entry,attributes);            //Update CFS attributes
   diAcornFS  : Result:=UpdateAFSAttributes(filename,attributes);         //Update AFS attributes
   diDOSPlus  : Result:=UpdateDOSAttributes(filename,attributes);         //Update DOS Plus attributes
  end;
end;

{-------------------------------------------------------------------------------
Set the disc title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDiscTitle(title: String;side: Byte): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : Result:=UpdateDFSDiscTitle(title,side);//Title DFS Disc
  diAcornADFS:
   begin
    if(FDOSPresent)and(side=1)then
     Result:=UpdateDOSDiscTitle(title)                //Title DOS Plus on ADFS hybrid disc
    else
     Result:=UpdateADFSDiscTitle(title);              //Title ADFS Disc
   end;
  diCommodore: Result:=UpdateCDRDiscTitle(title);     //Title Commodore 64/128 Disc
  diSinclair : Result:=UpdateSinclairDiscTitle(title);//Title Sinclair/Amstrad Disc
  diAmiga    : Result:=UpdateAmigaDiscTitle(title);   //Title AmigaDOS Disc
  diAcornUEF : Result:=False;                         //Can't retitle CFS
  diAcornFS  : Result:=UpdateAFSDiscTitle(title);     //Title AFS Disc
  diDOSPlus  : Result:=UpdateDOSDiscTitle(title);     //Title DOS Plus Disc
 end;
end;

{-------------------------------------------------------------------------------
Set the boot option
-------------------------------------------------------------------------------}
function TDiscImage.UpdateBootOption(option,side: Byte): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : Result:=UpdateDFSBootOption(option,side);//Update DFS Boot
  diAcornADFS: Result:=UpdateADFSBootOption(option);    //Update ADFS Boot
  diCommodore: exit;//Update Commodore 64/128 Boot ++++++++++++++++++++++++++++++++++++++
  diSinclair : exit;//Update Sinclair/Amstrad Boot ++++++++++++++++++++++++++++++++++++++
  diAmiga    : exit;//Update AmigaDOS Boot ++++++++++++++++++++++++++++++++++++++++++++++
  diAcornUEF : exit;//Can't update CFS boot option
  diAcornFS  : exit;//Can't update AFS boot option
  diDOSPlus  : exit;//Can't update DOS boot option
 end;
end;

{-------------------------------------------------------------------------------
Change the load address
-------------------------------------------------------------------------------}
function TDiscImage.UpdateLoadAddr(filename:String;newaddr:Cardinal;entry:Cardinal=0): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : Result:=UpdateDFSFileAddr(filename,newaddr,True); //Update DFS Load Address
  diAcornADFS: Result:=UpdateADFSFileAddr(filename,newaddr,True);//Update ADFS Load Address
  diCommodore: exit;//Update Commodore 64/128 Load Address
  diSinclair : exit;//Update Sinclair/Amstrad Load Address
  diAmiga    : exit;//Update AmigaDOS Load Address
  diAcornUEF : Result:=UpdateCFSFileAddr(entry,newaddr,True);    //Update CFS Load Address
  diAcornFS  : Result:=UpdateAFSFileAddr(filename,newaddr,True); //Update AFS Load Address
  diDOSPlus  : exit;//No Load address on DOS Plus
 end;
end;

{-------------------------------------------------------------------------------
Change the execution address
-------------------------------------------------------------------------------}
function TDiscImage.UpdateExecAddr(filename:String;newaddr:Cardinal;entry:Cardinal=0): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : Result:=UpdateDFSFileAddr(filename,newaddr,False); //Update DFS Execution Address
  diAcornADFS: Result:=UpdateADFSFileAddr(filename,newaddr,False);//Update ADFS Execution Address
  diCommodore: exit;//Update Commodore 64/128 Execution Address
  diSinclair : exit;//Update Sinclair/Amstrad Execution Address
  diAmiga    : exit;//Update AmigaDOS Execution Address
  diAcornUEF : Result:=UpdateCFSFileAddr(entry,newaddr,False);    //Update CFS Execution Address
  diAcornFS  : Result:=UpdateAFSFileAddr(filename,newaddr,False); //Update AFS Execution Address
  diDOSPlus  : exit;//No Execution address on DOS Plus
 end;
end;

{-------------------------------------------------------------------------------
Timestamp the file
-------------------------------------------------------------------------------}
function TDiscImage.TimeStampFile(filename:String;newtimedate:TDateTime):Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : exit;//Update DFS Timestamp
  diAcornADFS: Result:=UpdateADFSTimeStamp(filename,newtimedate);//Update ADFS Timestamp
  diCommodore: exit;//Update Commodore 64/128 Timestamp
  diSinclair : exit;//Update Sinclair/Amstrad Timestamp
  diAmiga    : exit;//Update AmigaDOS Timestamp
  diAcornUEF : exit;//Update CFS Timestamp
  diAcornFS  : Result:=UpdateAFSTimeStamp(filename,newtimedate);//Update AFS Timestamp
  diDOSPlus  : Result:=UpdateDOSTimeStamp(filename,newtimedate);//Update DOS Timestamp
 end;
end;

{-------------------------------------------------------------------------------
Change the file's filetype
-------------------------------------------------------------------------------}
function TDiscImage.ChangeFileType(filename,newtype: String): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : exit;//Update DFS Filetype
  diAcornADFS: Result:=UpdateADFSFileType(filename,newtype);//Update ADFS Filetype
  diCommodore: exit;//Update Commodore 64/128 Filetype
  diSinclair : exit;//Update Sinclair/Amstrad Filetype
  diAmiga    : exit;//Update AmigaDOS Filetype
  diAcornUEF : exit;//Update CFS Filetype
  diAcornFS  : exit;//Update AFS Filetype
  diDOSPlus  : exit;//Update DOS Filetype - done through renaming
 end;
end;

{-------------------------------------------------------------------------------
Convert a filetype number to a string
-------------------------------------------------------------------------------}
function TDiscImage.GetFileTypeFromNumber(filetype: Integer): String;
var
 i: Integer;
begin
 filetype:=filetype AND$FFF;
 Result:='';
 i:=1;
 repeat
  if StrToInt('$'+LeftStr(ROFileTypes[i],3))=filetype then
   Result:=Copy(ROFileTypes[i],4);
  inc(i);
 until(i>Length(ROFileTypes))or(Result<>'');
 if Result='' then Result:=IntToHex(filetype,3);
end;

{-------------------------------------------------------------------------------
Convert a filetype name into a number
-------------------------------------------------------------------------------}
function TDiscImage.GetFileTypeFromName(filetype: String): Integer;
var
 i: Integer;
begin
 Result:=-1;
 i:=1;
 repeat
  if UpperCase(filetype)=UpperCase(Copy(ROFileTypes[i],4))then
   Result:=StrToInt('$'+LeftStr(ROFileTypes[i],3));
  inc(i);
 until(i>Length(ROFileTypes))or(Result<>-1);
end;

{-------------------------------------------------------------------------------
Updating is commencing
-------------------------------------------------------------------------------}
procedure TDiscImage.BeginUpdate;
begin
 Fupdating:=True;
end;

{-------------------------------------------------------------------------------
Updating is ending
-------------------------------------------------------------------------------}
procedure TDiscImage.EndUpdate;
begin
 Fupdating:=False;
 case FFormat>>4 of
  diAcornDFS : exit;//Update DFS
  diAcornADFS: ADFSFreeSpaceMap;//Update ADFS
  diCommodore: exit;//Update Commodore 64/128
  diSinclair : exit;//Update Sinclair/Amstrad
  diAmiga    : exit;//Update AmigaDOS
  diAcornUEF : exit;//Update CFS
  diDOSPlus  : exit;//Update DOS Plus
 end;
end;

{-------------------------------------------------------------------------------
Makes sure that a copied/moved filename does not clash with one already there
-------------------------------------------------------------------------------}
function TDiscImage.ValidateFilename(parent:String;var filename:String): Boolean;
var
 ptr  : Cardinal;
 len,
 ctr  : Byte;
 newfn: String;
begin
 //Default return result
 Result:=False;
 len:=0; //Maximum file length
 //Work out the max file length for the system
 case FFormat>>4 of
  diAcornDFS : len:=7;
  diAcornADFS:
   case FDirType of
    0,1: len:=10;
    2  : len:=255;
   end;
  diCommodore: len:=16;
  diSinclair : exit;
  diAmiga    : len:=30;
  diAcornUEF : len:=10;
  diMMFS     : len:=7;
  diAcornFS  : len:=10;
  diDOSPlus  : len:=12; //Filename+'.'+extension
 end;
 if len=0 then exit; //Unsupported
 //Extract the filename
 while Pos(dir_sep,filename)>0 do
  filename:=Copy(filename,Pos(dir_sep,filename)+1);
 //CFS files can have multiple files with the same name
 if FFormat>>4=diAcornUEF then
 begin
  Result:=True;
  exit;
 end;
 //Validate it
 if FileExists(parent+dir_sep+filename,ptr) then
 begin
  newfn:=filename;
  ctr:=0;
  repeat
   inc(ctr);
   while Length(newfn+IntToStr(ctr))>len do
    newfn:=LeftStr(newfn,Length(newfn)-1);
  until(not FileExists(parent+dir_sep+newfn+IntToStr(ctr),ptr))or(ctr=0);
  if ctr>0 then
  begin
   filename:=newfn+IntToStr(ctr);
   Result:=True;
  end;
 end else Result:=True;
end;

{-------------------------------------------------------------------------------
Returns the disc size for a partition
-------------------------------------------------------------------------------}
function TDiscImage.DiscSize(partition: Cardinal):Int64;
begin
 Result:=0;
 if partition<Length(disc_size) then Result:=disc_size[partition];
end;

{-------------------------------------------------------------------------------
Returns the free space for a partition
-------------------------------------------------------------------------------}
function TDiscImage.FreeSpace(partition: Cardinal):Int64;
begin
 Result:=0;
 if partition<Length(free_space) then Result:=free_space[partition];
end;

{-------------------------------------------------------------------------------
Returns the disc name for a partition
-------------------------------------------------------------------------------}
function TDiscImage.Title(partition: Cardinal):String;
begin
 Result:='';
 if partition<Length(disc_name) then Result:=disc_name[partition];
end;

{-------------------------------------------------------------------------------
Create a password file for AFS systems
-------------------------------------------------------------------------------}
function TDiscImage.CreatePasswordFile(Accounts: TUserAccounts): Integer;
begin
 Result:=-5;
 if(FFormat>>4=diAcornFS)
 or((FFormat>>4=diAcornADFS)and(Fafspresent)) then
  Result:=CreateAFSPassword(Accounts);
end;

{-------------------------------------------------------------------------------
Read the password file for AFS systems
-------------------------------------------------------------------------------}
function TDiscImage.ReadPasswordFile: TUserAccounts;
begin
 Result:=nil;
 if(FFormat>>4=diAcornFS)
 or((FFormat>>4=diAcornADFS)and(Fafspresent)) then
  Result:=ReadAFSPassword;
end;

{-------------------------------------------------------------------------------
Returns the complete path for the parent
-------------------------------------------------------------------------------}
function TDiscImage.GetParent(dir: Integer): String;
var
 Ldirsep: Char;
begin
 Result:='';
 if(dir>=0)and(dir<Length(FDisc))then Ldirsep:=GetDirSep(FDisc[dir].Partition);
 if dir<Length(FDisc)then
  while dir<>-1 do
  begin
   Result:=FDisc[dir].Directory+Ldirsep+Result;
   dir:=FDisc[dir].Parent;
  end;
 if Result<>'' then
 begin
  while Result[Length(Result)]=Ldirsep do
   Result:=LeftStr(Result,Length(Result)-1);
 end;
end;

{-------------------------------------------------------------------------------
Extracts or deletes a partition/side
-------------------------------------------------------------------------------}
function TDiscImage.SeparatePartition(side: Cardinal;filename: String=''): Boolean;
var
 buffer    : TDIByteArray;
 FDiscDrive: TFileStream;
 ext       : String;
 oldformat : Word;
begin
 //If filename is empty, delete the current partition.
 //If filename is not empty, save the current partition
 Result:=False;
 buffer:=nil;
 //Blank filename, so extract the other partition
 if filename='' then
 begin
  if side=0 then side:=1
  else
  if side=1 then side:=0;
 end;
 //Extract the partition/side into a buffer.
 case FFormat>>4 of
  diAcornDFS : buffer:=ExtractDFSPartition(side);  //DFS
  diAcornADFS: buffer:=ExtractADFSPartition(side); //ADFS/AFS or DOS
 end;
 //Ensure there is something to deal with
 if buffer<>nil then
 begin
  //Filename is not blank, so save the data
  if filename<>'' then
  begin
   //Validate the filename
   ext:=ExtractFileExt(filename); //First extract the extension
   if ext='' then //If it hasn't been given an extension, then give it the default
   begin
    filename:=LeftStr(filename,Length(filename)-Length(ext));
    //Remember the current format
    oldformat:=FFormat;
    //If we have an ADFS/AFS hybrid, and are saving the AFS partition
    if(FFormat>>4=diAcornADFS)and(side<>0)and(FAFSPresent)then
     FFormat:=diAcornFS<<4;
    //If we have an ADFS/DOS hybrid, and are saving the DOS partition
    if(FFormat>>4=diAcornADFS)and(side<>0)and(FDOSPresent)then
     FFormat:=diDOSPlus<<4;
    filename:=filename+'.'+FormatToExt;
    //Change back
    FFormat:=oldformat;
   end;
   //Create the stream
   try
    FDiscDrive:=TFileStream.Create(filename,fmCreate OR fmShareDenyNone);
    //Move to the beginning of the stream
    FDiscDrive.Position:=0;
    //Read the image into the data buffer
    FDiscDrive.Write(buffer[0],Length(buffer));
    //Close the stream
    FDiscDrive.Free;
   except
    //Could not create
   end;
  end;
  //Filename is blank, so replace the data
  if filename='' then
  begin
   //Remember the old filename
   filename:=imagefilename;
   //Move the buffer into the main data store
   Fdata:=buffer;
   //ID it
   if IDImage then
   begin
    //Restore the filename
    imagefilename:=filename;
    //Re-read
    ReadImage;
   end;
  end;
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Get the maximum size for a partition on an ADFS 8 bit image
-------------------------------------------------------------------------------}
function TDiscImage.GetMaxLength: Cardinal;
begin
 Result:=0;
 //Only 8 bit ADFS
 if(FFormat>>4=diAcornADFS)and(not FMap)and(FDirType=diADFSOldDir)then
  Result:=GetADFSMaxLength(False);
end;

{-------------------------------------------------------------------------------
Adds a partition to an existing image
-------------------------------------------------------------------------------}
function TDiscImage.AddPartition(size: Cardinal;format: Byte): Boolean;
begin
 Result:=False;
 //Only for adding AFS or DOS Plus partition to 8 bit ADFS
 if(FFormat>>4=diAcornADFS)and(not FMap)and(FDirType=diADFSOldDir)then
  case format of
   0: Result:=AddAFSPartition(size); //Add AFS partition
   1: Result:=AddDOSPartition(size); //Add DOS Plus partition
  end;
end;
function TDiscImage.AddPartition(filename: String): Boolean;
begin
 Result:=False;
 //Only for Acorn DFS, given a filename
 if(FFormat>>4=diAcornDFS)and(not FDSD)then //Single sided images only
  Result:=AddDFSSide(filename);
end;

{-------------------------------------------------------------------------------
Change the Interleave Method
-------------------------------------------------------------------------------}
function TDiscImage.ChangeInterleaveMethod(NewMethod: Byte): Boolean;
var
 buffer: TDIByteArray;
 index : Cardinal;
begin
 Result:=False;
 //Are we actually changing, and is it within range?
 if(NewMethod<>Finterleave)and(NewMethod>0)and(NewMethod<4)then
  //Only works with ADFS L and Acorn FS, or ADFS with non-auto interleave
  if(FFormat=diAcornADFS<<4+2)
  or(FFormat=diAcornADFS<<4+$E)
  or((FFormat>>4=diAcornADFS)and(FForceInter>0))
  or(FFormat>>4=diAcornFS) then
  begin
   //Set up the buffer
   SetLength(buffer,GetDataLength);
   //Copy the data across, converting as we go to sequential
   for index:=0 to Length(buffer) do
    buffer[index]:=ReadByte(index);
   //Now copy back forcing our new method
   Finterleave:=NewMethod;
   for index:=0 to Length(buffer) do
    WriteByte(buffer[index],index);
   Result:=True;
  end;
end;

{-------------------------------------------------------------------------------
Return the directory separator for the specified partition
-------------------------------------------------------------------------------}
function TDiscImage.GetDirSep(partition: Byte): Char;
begin
 Result:=dir_sep;
 if partition=1 then
 begin
  if FDOSPresent then Result:='\';
 end;
end;
