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
 FForceInter   :=0;
 //Deal with Spark archives as a filing system (i.e. in this class)
 FSparkAsFS    :=True;
 //Allow DFS images which report number of sectors as zero
 FDFSzerosecs  :=False;
 //Allow files to go beyond the edge of the disc
 FDFSBeyondEdge:=False;
 //Allow blank filenames in DFS
 FDFSAllowBlank:=False;
 //Scan sub directories in ADFS, Amiga, DOS, Spark
 FScanSubDirs  :=True;
 //Use short filenames in DOS even if long filenames exist
 FDOSUseSFN    :=False;
 //Open DOS Partitions on ADFS
 FOpenDOSPart  :=True;
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
 //Allow files to go beyond the edge of the disc
 FDFSBeyondEdge:=Clone.DFSBeyondEdge;
 //Allow blank filenames in DFS
 FDFSAllowBlank:=Clone.DFSAllowBlanks;
 //Scan sub directories in ADFS, Amiga, DOS, Spark
 FScanSubDirs  :=Clone.ScanSubDirs;
 //Use short filenames in DOS even if long filenames exist
 FDOSUseSFN    :=Clone.FDOSUseSFN;
 //Open DOS Partitions on ADFS
 FOpenDOSPart  :=Clone.OpenDOSPartitions;
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
 if GetMajorFormatNumber=diAcornADFS then Result:=FixBrokenADFSDirectories;
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
 for queryformat:=$00 to $AF do
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
  if GetMajorFormatNumber=diSpark then SparkFile.Free;
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
  if not ID_DOSPlus  then //DOS plus
  if not ID_Sinclair then //Sinclair/Amstrad
  if not ID_CFS      then //Acorn CFS
  if not ID_MMB      then //MMFS
  if not ID_Spark    then //Spark archive
  if not ID_DFS      then //Acorn DFS
   ResetVariables;        //Reset everything
  //Just by the ID process:
  //ADFS 'F' can get mistaken for Commodore
  //Commodore, Amiga and blank DOS can be mistaken for DFS
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
 case GetMajorFormatNumber of
  diAcornDFS : ReadDFSDisc;     //Acorn DFS
  diAcornADFS:
   begin
    ReadADFSDisc;               //Acorn ADFS
    ReadAFSPartition;//Read in the AFS partition, if one is present
    ReadDOSPartition;//Read in the DOS Plus partition, if one is present
   end;
  diAcornFS  : ReadAFSPartition;       //Acorn File Server
  diCommodore: ReadCDRDisc;            //Commodore
  diSinclair : ReadSinclairDisc;       //Sinclair/Amstrad
  diAmiga    : ReadAmigaDisc;          //Amiga
  diAcornUEF : ReadUEFFile;            //Acorn CFS
  diMMFS     : ReadMMBDisc;            //MMFS
  diSpark    : ReadSparkArchive;       //Spark archive
  diDOSPlus  : ReadDOSPartition;       //DOS Plus
 end;
 //Find the maximum directory entries
 if GetMajorFormatNumber<>diSpark then //Not Spark, as this already provides this info
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
function TDiscImage.SaveToFile(filename: String;uncompress: Boolean=False): Boolean;
var
 FDiscDrive: TFileStream;
 ext: String;
begin
 Result:=False;
 //Validate the filename
 ext:=ExtractFileExt(filename); //First extract the extension
 if ext='' then //If it hasn't been given an extension, then give it the default
 begin
  filename:=LeftStr(filename,Length(filename)-Length(ext));
  filename:=filename+'.'+FormatToExt;
 end;
 if GetMajorFormatNumber<>diAcornUEF then //Not CFS
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
   Result:=True;
  except
   //Could not create
   Result:=False;
  end;
 end;
 if GetMajorFormatNumber=diAcornUEF then WriteUEFFile(filename,uncompress); //CFS
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
function TDiscImage.FormatFDD(major:Word;minor:Byte=0;tracks: Byte=0;filename: String=''): Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major AND $FFF;
 minor :=minor AND $F;
 if major<>diDOSPlus then
  tracks:=tracks MOD 2;
 case major of
  diAcornDFS : Result:=FormatDFS(minor,tracks); //Create DFS
  diAcornADFS: Result:=FormatADFSFloppy(minor); //Create ADFS
  diCommodore: Result:=FormatCDR(minor);        //Create Commodore 64/128
  diSinclair : Result:=FormatSpectrum(minor);   //Create Sinclair/Amstrad
  diAmiga    : Result:=FormatAmigaFDD(minor);   //Create AmigaDOS
  diAcornUEF : Result:=FormatCFS;               //Create CFS
  diSpark    : Result:=FormatSpark(filename);   //Create Spark
  diDOSPlus  :                                  //Create DOS or DOS Plus
   begin
    case minor of
     0: Result:=FormatDOS( 640*1024,diFAT12);   // 640KB ADFS/DOS Plus
     1: Result:=FormatDOS( 800*1024,diFAT12);   // 800KB DOS Plus
     2: Result:=FormatDOS( 360*1024,diFAT12);   // 360KB DOS
     3: Result:=FormatDOS( 720*1024,diFAT12);   // 720KB DOS
     4: Result:=FormatDOS(1440*1024,diFAT12);   //1.44MB DOS
     5: Result:=FormatDOS(2880*1024,diFAT12);   //2.88MB DOS
    end;
   end;
 end;
end;

{-------------------------------------------------------------------------------
Create and format a new hard disc image
-------------------------------------------------------------------------------}
function TDiscimage.FormatHDD(major: Word;harddrivesize: Cardinal;
                 ide,newmap: Boolean;dirtype: Byte;addheader: Boolean): Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major AND $FFF;
 case major of
  diAcornADFS: Result:=FormatADFSHDD(harddrivesize,
                                     newmap,
                                     dirtype,
                                     ide,
                                     addheader);        //Create ADFS
  diAcornFS  : Result:=FormatAFS(harddrivesize,dirtype);//Create Acorn FS
  diDOSPlus  : Result:=FormatDOS(harddrivesize,dirtype);//Create DOS HDD
  diAmiga    : Result:=FormatAmigaHDD(harddrivesize);   //Create Amiga HDD
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
 case GetMajorFormatNumber of
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
function TDiscImage.ExtractFile(filename:String;Stream:TStream;
                                                     entry:Cardinal=0): Boolean;
var
 Lbuffer: TDIByteArray;
begin
 //Start with a false result
 Result:=False;
 SetLength(Lbuffer,0);
 //Get the file into our buffer
 if Stream<>nil then
  if ExtractFile(filename,Lbuffer,entry) then
  begin
   //Positive Result
   Result:=True;
   //Now copy into the stream (which should already exist, and be in position)
   Stream.Write(Lbuffer[0],Length(Lbuffer));
  end;
end;

{-------------------------------------------------------------------------------
Save a file into the disc image, from buffer
-------------------------------------------------------------------------------}
function TDiscImage.WriteFile(var file_details: TDirEntry;
                      var buffer: TDIByteArray;ShowFSM: Boolean=False): Integer;
var
 count : Integer;
begin
 //Start with a false result
 Result:=-2; //Error - disc full
 //Get the length of data to be written
 count:=file_details.Length;
 //Only write a file if there is actually any data to be written
 if(count>0)and((Length(free_space)>0)or(GetMajorFormatNumber=diSpark))then
 begin
  if GetMajorFormatNumber<>diSpark then
   file_details.Side:=file_details.Side mod Length(free_space);
  //Can only write a file that will fit on the disc, or CFS
  if(count<=free_space[file_details.Side])
  or(GetMajorFormatNumber=diAcornUEF)
  or(GetMajorFormatNumber=diSpark)then
   case GetMajorFormatNumber of
    diAcornDFS :Result:=WriteDFSFile(file_details,buffer);     //Write DFS
    diAcornADFS:Result:=WriteADFSFile(file_details,buffer);    //Write ADFS
    diCommodore:Result:=WriteCDRFile(file_details,buffer);     //Write Commodore 64/128
    diSinclair :Result:=WriteSpectrumFile(file_details,buffer);//Write Sinclair/Amstrad
    diAmiga    :Result:=WriteAmigaFile(file_details,buffer);   //Write AmigaDOS
    diAcornUEF :Result:=WriteCFSFile(file_details,buffer);     //Write CFS
    diSpark    :Result:=WriteSparkFile(file_details,buffer);   //Write !Spark
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
 case GetMajorFormatNumber of
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
  diSpark    :      //Create directory on !SparkFS
    Result:=CreateSparkDirectory(filename,parent,attributes);
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
 case GetMajorFormatNumber of
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
  diAmiga    : exit;//AmigaDOS does not have directory titles
  diAcornUEF : exit;//CFS doesn't have directories
  diAcornFS  : exit;//Can't retitle AFS directories
  diDOSPlus  : exit;//Can't retitle DOS directories
 end;
end;

{-------------------------------------------------------------------------------
Does a file exist?
-------------------------------------------------------------------------------}
function TDiscImage.FileExists(filename: String;var Ref: Cardinal;sfn: Boolean=False): Boolean;
var
 dir,entry: Cardinal;
begin
 dir:=$FFFF;
 entry:=$FFFF;
 Result:=FileExists(filename,dir,entry);
 Ref:=dir*$10000+entry;
end;
function TDiscImage.FileExists(filename: String;var dir,entry: Cardinal;sfn: Boolean=False): Boolean;
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
   if Length(FDisc)>1 then
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
   end else
   begin
    dir:=0;
    entry:=$FFFF;
    Result:=True;
    exit;
   end;
 //Not going to search if there is no tree to search in
 if(Length(FDisc)>0)and(filename<>'')then//or if there is nothing being searched for
 begin
  SetLength(Path,0);
  //Explode the pathname into an array, without the '.'
  if(GetMajorFormatNumber<>diAcornDFS)and(GetMajorFormatNumber<>diCommodore)then //Not DFS or Commodore
  begin
   if(GetMajorFormatNumber=diAcornADFS)and(FDOSPresent)
   and(LeftStr(filename,Length(dosrootname))=dosrootname)then//Is this on a DOS Partition of an ADFS?
    Path:=filename.Split('\')
   else
    Path:=filename.Split(dir_sep);
  end;
  if GetMajorFormatNumber=diAcornDFS then //With DFS, we need the initial root name, including the '.'
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
  if GetMajorFormatNumber=diCommodore then //Commodore is similar to DFS
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
        if sfn then test:=UpperCase(AddTopBit(FDisc[i].Entries[j].ShortFilename))
        else test:=UpperCase(AddTopBit(FDisc[i].Entries[j].Filename));
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
        if sfn then test2:=UpperCase(AddTopBit(FDisc[i].Entries[j].ShortFilename))
        else test2:=UpperCase(AddTopBit(FDisc[i].Entries[j].Filename));
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
 i      : Cardinal;
begin
 Result:=False;
 if count>0 then //Make sure there is something to read
 begin
  //Return a success if we didn't go out of range
  Result:=offset+count<=Length(buffer);
  //Simply copy from source to destination
  for i:=0 to count-1 do
  begin
   if offset+i<Length(buffer) then //Bit of range checking
   begin
    if GetMajorFormatNumber<>diAcornDFS then //All but DFS
     if addr+i<GetDataLength then        //If the data is within bounds
      buffer[offset+i]:=ReadByte(addr+i) //Read it
     else Result:=False;                 //Otherwise return a fail
    if GetMajorFormatNumber=diAcornDFS then  //DFS only
     if ConvertDFSSector(addr+i,side)<GetDataLength then
      buffer[offset+i]:=ReadByte(ConvertDFSSector(addr+i,side))
     else Result:=False;                 //Fail if outside the image
   end;
  end;
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
  if GetMajorFormatNumber<>diAcornDFS then //not DFS
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
 case GetMajorFormatNumber of
  diAcornDFS : Result:=RenameDFSFile(oldfilename,newfilename);     //Rename DFS
  diAcornADFS: Result:=RenameADFSFile(oldfilename,newfilename);    //Rename ADFS
  diCommodore: Result:=RenameCDRFile(oldfilename,newfilename);     //Rename Commodore 64/128
  diSinclair : Result:=RenameSpectrumFile(oldfilename,newfilename);//Rename Sinclair/Amstrad
  diAmiga    : Result:=RenameAmigaFile(oldfilename,newfilename);   //Rename AmigaDOS
  diAcornUEF : Result:=RenameCFSFile(entry,newfilename);           //Rename CFS
  diAcornFS  : Result:=RenameAFSFile(oldfilename,newfilename);     //Rename AFS
  diSpark    : Result:=RenameSparkFile(oldfilename,newfilename);   //Rename Spark
  diDOSPlus  : Result:=RenameDOSFile(oldfilename,newfilename);     //Rename DOS Plus
 end;
end;

{-------------------------------------------------------------------------------
Deletes a file (given full pathname)
-------------------------------------------------------------------------------}
function TDiscImage.DeleteFile(filename: String;entry: Cardinal=0): Boolean;
begin
 Result:=False;
 case GetMajorFormatNumber of
  diAcornDFS : Result:=DeleteDFSFile(filename);     //Delete DFS
  diAcornADFS: Result:=DeleteADFSFile(filename);    //Delete ADFS
  diCommodore: Result:=DeleteCDRFile(filename);     //Delete Commodore 64/128
  diSinclair : Result:=DeleteSinclairFile(filename);//Delete Sinclair/Amstrad
  diAmiga    : Result:=DeleteAmigaFile(filename);   //Delete AmigaDOS
  diAcornUEF : Result:=DeleteCFSFile(entry);        //Delete CFS
  diAcornFS  : Result:=DeleteAFSFile(filename);     //Delete Acorn FS
  diSpark    : Result:=DeleteSparkFile(filename);   //Delete SparkFS
  diDOSPlus  : Result:=DeleteDOSFile(filename);     //Delete DOS Plus
 end;
end;

{-------------------------------------------------------------------------------
Moves a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.MoveFile(filename,directory: String): Integer;
begin
 Result:=-12;
 case GetMajorFormatNumber of
  diAcornDFS : Result:=MoveDFSFile(filename,directory);  //Move on DFS
  diAcornADFS: Result:=MoveADFSFile(filename,directory); //Move ADFS File
  diAmiga    : Result:=MoveAmigaFile(filename,directory);//Move Amiga File
  diAcornFS  : Result:=MoveAFSFile(filename,directory);  //Move AFS File
  diSpark    : Result:=MoveSparkFile(filename,directory);//Move Spark File
  diDOSPlus  : Result:=MoveDOSFile(filename,directory);  //Move DOS File
 end;
end;
function TDiscImage.MoveFile(source: Cardinal;dest: Integer): Integer;
begin
 Result:=-12;
 //Can only move files on DFS (between drives), ADFS, Amiga, AFS and CFS
 if GetMajorFormatNumber=diAcornUEF then //Move on CFS
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
    if GetMajorFormatNumber=diAcornDFS then //DFS
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
 if GetMajorFormatNumber=diAcornUEF then //Copy on CFS
  Result:=CopyCFSFile(source,dest);
end;

{-------------------------------------------------------------------------------
Set the attributes for a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateAttributes(filename,attributes: String;entry:Cardinal=0):Boolean;
begin
 Result:=False;
 if(filename<>root_name)and(filename<>afsrootname)then
  case GetMajorFormatNumber of
   diAcornDFS : Result:=UpdateDFSFileAttributes(filename,attributes);     //Update DFS attributes
   diAcornADFS: Result:=UpdateADFSFileAttributes(filename,attributes);    //Update ADFS attributes
   diCommodore: Result:=UpdateCDRFileAttributes(filename,attributes);     //Update Commodore 64/128 attributes
   diSinclair : Result:=UpdateSinclairFileAttributes(filename,attributes);//Update Sinclair/Amstrad attributes
   diAmiga    : Result:=UpdateAmigaFileAttributes(filename,attributes);   //Update AmigaDOS attributes
   diAcornUEF : Result:=UpdateCFSAttributes(entry,attributes);            //Update CFS attributes
   diAcornFS  : Result:=UpdateAFSAttributes(filename,attributes);         //Update AFS attributes
   diSpark    : Result:=UpdateSparkAttributes(filename,attributes);       //Update Spark attributes
   diDOSPlus  : Result:=UpdateDOSAttributes(filename,attributes);         //Update DOS Plus attributes
  end;
end;

{-------------------------------------------------------------------------------
Set the disc title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDiscTitle(title: String;side: Byte): Boolean;
begin
 Result:=False;
 case GetMajorFormatNumber of
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
 case GetMajorFormatNumber of
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
 case GetMajorFormatNumber of
  diAcornDFS : Result:=UpdateDFSFileAddr(filename,newaddr,True); //Update DFS Load Address
  diAcornADFS: Result:=UpdateADFSFileAddr(filename,newaddr,True);//Update ADFS Load Address
  diCommodore: exit;//Update Commodore 64/128 Load Address
  diSinclair : exit;//Update Sinclair/Amstrad Load Address
  diAmiga    : exit;//Update AmigaDOS Load Address
  diAcornUEF : Result:=UpdateCFSFileAddr(entry,newaddr,True);    //Update CFS Load Address
  diAcornFS  : Result:=UpdateAFSFileAddr(filename,newaddr,True); //Update AFS Load Address
  diSpark    : Result:=UpdateSparkFileAddr(filename,newaddr,True);//Update Spark Load Address
  diDOSPlus  : exit;//No Load address on DOS Plus
 end;
end;

{-------------------------------------------------------------------------------
Change the execution address
-------------------------------------------------------------------------------}
function TDiscImage.UpdateExecAddr(filename:String;newaddr:Cardinal;entry:Cardinal=0): Boolean;
begin
 Result:=False;
 case GetMajorFormatNumber of
  diAcornDFS : Result:=UpdateDFSFileAddr(filename,newaddr,False); //Update DFS Execution Address
  diAcornADFS: Result:=UpdateADFSFileAddr(filename,newaddr,False);//Update ADFS Execution Address
  diCommodore: exit;//Update Commodore 64/128 Execution Address
  diSinclair : exit;//Update Sinclair/Amstrad Execution Address
  diAmiga    : exit;//Update AmigaDOS Execution Address
  diAcornUEF : Result:=UpdateCFSFileAddr(entry,newaddr,False);    //Update CFS Execution Address
  diAcornFS  : Result:=UpdateAFSFileAddr(filename,newaddr,False); //Update AFS Execution Address
  diSpark    : Result:=UpdateSparkFileAddr(filename,newaddr,False);//Update Spark Load Address
  diDOSPlus  : exit;//No Execution address on DOS Plus
 end;
end;

{-------------------------------------------------------------------------------
Timestamp the file
-------------------------------------------------------------------------------}
function TDiscImage.TimeStampFile(filename:String;newtimedate:TDateTime):Boolean;
begin
 Result:=False;
 case GetMajorFormatNumber of
  diAcornDFS : exit;//Update DFS Timestamp
  diAcornADFS: Result:=UpdateADFSTimeStamp(filename,newtimedate);//Update ADFS Timestamp
  diCommodore: exit;//Update Commodore 64/128 Timestamp
  diSinclair : exit;//Update Sinclair/Amstrad Timestamp
  diAmiga    : Result:=UpdateAmigaTimeStamp(filename,newtimedate);//Update AmigaDOS Timestamp
  diAcornUEF : exit;//Update CFS Timestamp
  diAcornFS  : Result:=UpdateAFSTimeStamp(filename,newtimedate);//Update AFS Timestamp
  diSpark    : Result:=UpdateSparkTimeStamp(filename,newtimedate);//Update Spark Timestamp
  diDOSPlus  : Result:=UpdateDOSTimeStamp(filename,newtimedate);//Update DOS Timestamp
 end;
end;

{-------------------------------------------------------------------------------
Change the file's filetype
-------------------------------------------------------------------------------}
function TDiscImage.ChangeFileType(filename,newtype: String): Boolean;
begin
 Result:=False;
 case GetMajorFormatNumber of
  diAcornDFS : exit;//Update DFS Filetype
  diAcornADFS: Result:=UpdateADFSFileType(filename,newtype);//Update ADFS Filetype
  diCommodore: exit;//Update Commodore 64/128 Filetype
  diSinclair : exit;//Update Sinclair/Amstrad Filetype
  diAmiga    : exit;//Update AmigaDOS Filetype
  diAcornUEF : exit;//Update CFS Filetype
  diAcornFS  : exit;//Update AFS Filetype
  diSpark    : Result:=UpdateSparkFileType(filename,newtype);
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
 case GetMajorFormatNumber of
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
 ptr,
 part : Cardinal;
 len,
 ctr  : Byte;
 newfn: String;
begin
 //Which partition, and hence, which file system?
 if(FFormat=diAcornADFS)and((AFSPresent)or(DOSPresent))then
 begin
  if(AFSPresent)and(Copy(parent,1,Length(afsrootname))=afsrootname)then part:=1;
  if(DOSPresent)and(Copy(parent,1,Length(dosrootname))=dosrootname)then part:=1;
 end;
 //Default return result
 Result:=False;
 len:=0; //Maximum file length
 //Work out the max file length for the system
 case GetMajorFormatNumber of
  diAcornDFS : len:=7;
  diAcornADFS:
  begin
   case FDirType of
    diADFSOldDir,
    diADFSNewDir : len:=10;
    diADFSBigDir : len:=255;
   end;
   //Is it on the second partition?
   if(AFSPresent)and(Copy(parent,1,Length(afsrootname))=afsrootname)then len:=10;
   if(DOSPresent)and(Copy(parent,1,Length(dosrootname))=dosrootname)then len:=12;
  end;
  diCommodore: len:=16;
  diSinclair : exit;
  diAmiga    : len:=30;
  diAcornUEF : len:=10;
  diMMFS     : len:=7;
  diAcornFS  : len:=10;
  diDOSPlus  :
  begin
   if GetMinorFormatNumber=0 then len:=12 //Filename+'.'+extension - Master 512 DOS Plus
   else len:=255; //FAT12, 16, and 32
  end;
 end;
 if len=0 then exit; //Unsupported
 //Extract the filename
 while Pos(GetDirSep(part),filename)>0 do
  filename:=Copy(filename,Pos(GetDirSep(part),filename)+1);
 //CFS files can have multiple files with the same name
 if GetMajorFormatNumber=diAcornUEF then
 begin
  Result:=True;
  exit;
 end;
 //Validate it
 if FileExists(parent+GetDirSep(part)+filename,ptr) then
 begin
  newfn:=filename;
  ctr:=0;
  repeat
   inc(ctr);
   while Length(newfn+IntToStr(ctr))>len do
    newfn:=LeftStr(newfn,Length(newfn)-1);
  until(not FileExists(parent+GetDirSep(part)+newfn+IntToStr(ctr),ptr))or(ctr=0);
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
function TDiscImage.DiscSize(partition: QWord):QWord;
begin
 Result:=0;
 if partition<Length(disc_size) then Result:=disc_size[partition];
end;

{-------------------------------------------------------------------------------
Returns the free space for a partition
-------------------------------------------------------------------------------}
function TDiscImage.FreeSpace(partition: QWord):QWord;
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
 if(GetMajorFormatNumber=diAcornFS)
 or((GetMajorFormatNumber=diAcornADFS)and(Fafspresent)) then
  Result:=CreateAFSPassword(Accounts);
end;

{-------------------------------------------------------------------------------
Read the password file for AFS systems
-------------------------------------------------------------------------------}
function TDiscImage.ReadPasswordFile: TUserAccounts;
begin
 Result:=nil;
 if(GetMajorFormatNumber=diAcornFS)
 or((GetMajorFormatNumber=diAcornADFS)and(Fafspresent)) then
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
 case GetMajorFormatNumber of
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
    if(GetMajorFormatNumber=diAcornADFS)and(side<>0)and(FAFSPresent)then
     FFormat:=diAcornFS<<4;
    //If we have an ADFS/DOS hybrid, and are saving the DOS partition
    if(GetMajorFormatNumber=diAcornADFS)and(side<>0)and(FDOSPresent)then
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
 if(GetMajorFormatNumber=diAcornADFS)and(not FMap)and(FDirType=diADFSOldDir)then
  Result:=GetADFSMaxLength(False);
end;

{-------------------------------------------------------------------------------
Adds a partition to an existing image
-------------------------------------------------------------------------------}
function TDiscImage.AddPartition(size: Cardinal;format: Byte): Boolean;
begin
 Result:=False;
 //Only for adding AFS or DOS Plus partition to 8 bit ADFS
 if(GetMajorFormatNumber=diAcornADFS)and(not FMap)and(FDirType=diADFSOldDir)then
  case format of
   0: Result:=AddAFSPartition(size); //Add AFS partition
   1: Result:=AddDOSPartition(size); //Add DOS Plus partition
  end;
end;
function TDiscImage.AddPartition(tracks: Byte): Boolean;
begin
 Result:=False;
 if(tracks<>40)and(tracks<>80)then exit; //40 or 80 not specified, so quit
 //Only for Acorn DFS, given a number of tracks
 if(GetMajorFormatNumber=diAcornDFS)and(not FDSD)then //Single sided images only
  Result:=AddDFSBlankSide(tracks);
end;
function TDiscImage.AddPartition(filename: String): Boolean;
begin
 Result:=False;
 //Only for Acorn DFS, given a filename
 if(GetMajorFormatNumber=diAcornDFS)and(not FDSD)then //Single sided images only
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
  or((GetMajorFormatNumber=diAcornADFS)and(FForceInter>0))
  or(GetMajorFormatNumber=diAcornFS) then
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

{-------------------------------------------------------------------------------
Read a directory, given the directory name
-------------------------------------------------------------------------------}
function TDiscImage.ReadDirectory(dirname: String): Integer;
var
 dir,
 entry,
 sector,
 len,f  : Cardinal;
 NewDir : TDir;
begin
 RemoveControl(dirname);
 //This is only here to stop the hints that variables aren't intialised
 Result:=-1;
 NewDir.Directory:=dirname;
 dir:=0;
 entry:=0;
 sector:=0;
 len:=0;
 //Reset the Result TDir to default values
 ResetDir(NewDir);
 //Is it a valid directory?
 if FileExists(dirname,dir,entry) then //Does it exist? (and grab the references)
  if dir<Length(FDisc) then
   if entry<Length(FDisc[dir].Entries) then
    if FDisc[dir].Entries[entry].DirRef>-1 then //Valid directory
     if not FDisc[FDisc[dir].Entries[entry].DirRef].BeenRead then //Hasn't already been read?
     begin
      sector:=FDisc[dir].Entries[entry].Sector;
      //Divert to the appropriate function
      case GetMajorFormatNumber of
       diAcornADFS: NewDir:=ReadADFSDir(dirname,sector);
       diAcornFS  : NewDir:=ReadAFSDirectory(dirname,sector);
       diAmiga    : NewDir:=ReadAmigaDir(dirname,sector);
       diDOSPlus  : NewDir:=ReadDOSDirectory(dirname,sector,len);
      end;
      //Did it return something?
      if NewDir.Directory<>'' then
      begin
       //Return an index (the previously saved directory reference
       Result:=FDisc[dir].Entries[entry].DirRef;
       //Then add it to the list
       FDisc[Result]:=NewDir;
       FDisc[Result].Parent:=dir;
       FDisc[Result].BeenRead:=True;
       //Now go through the entries and see if there are any sub dirs
       if Length(FDisc[Result].Entries)>0 then //Make sure we have some entries
        for f:=0 to Length(FDisc[Result].Entries)-1 do
         if(Pos('D',FDisc[Result].Entries[f].Attributes)>0) //Found a directory
         or(Pos('F',FDisc[Result].Entries[f].Attributes)>0)then
         //D is for ADFS, AFS and DOS Plus, F is for Amiga
         begin
          SetLength(FDisc,Length(FDisc)+1); //Make space for it
          FDisc[Result].Entries[f].DirRef:=Length(FDisc)-1; //And set the directory reference
         end;
      end;
     end;
end;

{-------------------------------------------------------------------------------
Produce a report of the image's details
-------------------------------------------------------------------------------}
function TDiscImage.ImageReport(CSV: Boolean): TStringList;
var
 temp,
 uline : String;
 side  : Integer;
 report: TStringList;
begin
 Result:=TStringList.Create;
 if FFormat<>diInvalidImg then
 begin
  //Header
  temp:='Container format: '+FormatToString;
  Result.Add(temp);
  if not(CSV) then
  begin
   uline:='';
   uline:=AddChar('=',uline,Length(temp));
   Result.Add(uline);
   Result.Add('');
  end;
  //Main Report, depending on format
  if GetMajorFormatNumber=diAcornDFS  then report:=DFSReport(CSV); //DFS
  if GetMajorFormatNumber=diAcornADFS then report:=ADFSReport(CSV);//ADFS
  if(GetMajorFormatNumber=diAcornFS)
  or((GetMajorFormatNumber=diAcornADFS)
  and(FAFSPresent))                   then report:=AFSReport(CSV);//AFS
  if(GetMajorFormatNumber=diDOSPlus)
  or((GetMajorFormatNumber=diAcornADFS)
  and(FDOSPresent))                   then report:=DOSReport(CSV);//DOS
  if GetMajorFormatNumber=diCommodore then report:=CDRReport(CSV);//C64
  if GetMajorFormatNumber=diAmiga     then report:=AmigaReport(CSV);//Amiga
  if GetMajorFormatNumber=diSinclair  then report:=TStringList.Create;//Not written yet
  if GetMajorFormatNumber=diAcornUEF  then report:=TStringList.Create;//Nothing to report
  if GetMajorFormatNumber=diSpark     then report:=TStringList.Create;//Nothing to report
  //Incorporate the report
  if report.Count>0 then
   for side:=0 to report.Count-1 do Result.Add(report[side]);
  report.Free;
  //Re-jig the output if CSV has been specified
  if(CSV)and(Result.Count>0)then
   for side:=0 to Result.Count-1 do
    Result[side]:='"'+StringReplace(Result[side],': ','","',[rfReplaceAll])+'"';
 end;
end;

//++++++++++++++++++ TSpark Published Methods ++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Create the instance
-------------------------------------------------------------------------------}
constructor TSpark.Create(filename: String;blank: Boolean=false);
var
 F: TFileStream;
begin
 //Set the filename
 ZipFilename:=filename;
 //Create a blank file
 if blank then
 begin
  F:=TFileStream.Create(ZipFilename,fmCreate or fmShareDenyNone);
  F.Free;
 end;
 //Create a stream
 F:=TFileStream.Create(ZipFilename,fmOpenRead or fmShareDenyNone);
 //Read it using the overloaded constructor
 Create(F);
 //And free it up
 F.Free;
end;
constructor TSpark.Create(Stream: TStream);
begin
 inherited Create;
 //Initialise the variables
 Fbuffer    :=nil;
 Fversion   :='1.05';
 FTimeOut   :=30;
 FIsSpark   :=False;
 FIsPack    :=False;
 //Read the zip file into memory from the stream
 SetLength(Fbuffer,Stream.Size);
 Stream.Position:=0;
 Stream.Read(Fbuffer[0],Stream.Size);
 if Length(Fbuffer)<5 then exit;
 //Check it is a !Spark file (or just a zip file)
 if (Fbuffer[0]=$50)
 and(Fbuffer[1]=$4B)
 and(Fbuffer[2]=$03)
 and(Fbuffer[3]=$04)then FIsSpark:=True;
 //Is it a !PackDir file
 if (Fbuffer[0]=$50)
 and(Fbuffer[1]=$41)
 and(Fbuffer[2]=$43)
 and(Fbuffer[3]=$4B)
 and(Fbuffer[4]=$00)then FIsPack:=True;
 //It is neither
 if(not FIsSpark)and(not FIsPack)then SetLength(Fbuffer,0);//Clear the buffer
 //The extract the file details - the function will select the appropriate algorithm
 FFileList:=ExtractFiles;
end;

{-------------------------------------------------------------------------------
Destroy (free) the instance
-------------------------------------------------------------------------------}
destructor TSpark.Destroy;
begin
 inherited;
end;

{-------------------------------------------------------------------------------
Write a file to the archive
-------------------------------------------------------------------------------}
procedure TSpark.WriteFile(var filetozip: TFileEntry;var buffer: TDIByteArray);
var
 tempname : String;
 tempfile : TFileStream;
 zipfile  : TZipper;
 EoCL,
 CL,ptr,
 dataptr  : Cardinal;
 fnL,exL  : Word;
 index,
 adjust   : Integer;
const newExL = $18;
begin
 if FIsPack then exit;
 if filetozip.Directory then CreateDirectory(filetozip.ArchiveName)
 else
 begin
  //Zipper will only zip up existing files, so we'll need to save the data to a
  //temporary file first, then zip that.
  tempname:=GetTempFileName;//Get a temporary name
  tempfile:=TFileStream.Create(tempname,fmCreate);//Create the file
  tempfile.Position:=0;
  tempfile.Write(buffer[0],Length(buffer)); //Write the file data to it
  tempfile.Free; //Close the file
  //Now we can open the zipfile
  zipfile:=TZipper.Create;
  try
   zipfile.Filename:=ZipFilename; //Set the zipfile name
   zipfile.Entries.AddFileEntry(tempname,filetozip.ArchiveName); //Add the file
   zipfile.ZipAllFiles; //Then zip all the files
  finally
   zipfile.Free; //And close it
  end;
  //Finally, delete the temporary file
  DeleteFile(tempname);
  //Then we will need to load the zip file in and change the values and add to
  //the library at the end.
  tempfile:=TFileStream.Create(ZipFilename,fmOpenRead or fmShareDenyNone);
  SetLength(Fbuffer,tempfile.Size);
  tempfile.Position:=0;
  tempfile.Read(Fbuffer[0],tempfile.Size);
  tempfile.Free;
  CL:=0;
  EoCL:=FindEoCL(CL); //Get the end of central library marker
  if EoCL<>0 then
  begin
   //Find the entry for this file
   if FindEntry(filetozip.ArchiveName,false,ptr,dataptr) then
   begin
    //Get the variable length fields lengths
    fnL:=Fbuffer[ptr+$1C]+Fbuffer[ptr+$1D]<<8;
    exL:=Fbuffer[ptr+$1E]+Fbuffer[ptr+$1F]<<8;
    //We need to add up to 24 bytes, so everything from the next entry needs to shift along
    adjust:=0;
    if exL<newExL then adjust:=newExL-exL;
    if adjust>0 then
    begin
     SetLength(Fbuffer,Length(Fbuffer)+adjust);
     for index:=Length(Fbuffer)-1 downto ptr+$2E+fnL+exL+adjust do
      Fbuffer[index]:=Fbuffer[index-adjust];
    end;
    //Update the OS type to RISC OS
    Fbuffer[ptr+$05]:=13;
    //Update the extra field length
    Fbuffer[ptr+$1E]:=(exL+adjust)mod$100;
    Fbuffer[ptr+$1F]:=(exL+adjust)>>8;
    //Fill in the extra field
    Fbuffer[ptr+$2E+fnL]:=$41;//A
    Fbuffer[ptr+$2F+fnL]:=$43;//C
    //Length of extra -4
    Fbuffer[ptr+$30+fnL]:=((exL+adjust)-4)mod$100;
    Fbuffer[ptr+$31+fnL]:=((exL+adjust)-4)>>8;
    Fbuffer[ptr+$32+fnL]:=$41;//A
    Fbuffer[ptr+$33+fnL]:=$52;//R
    Fbuffer[ptr+$34+fnL]:=$43;//C
    Fbuffer[ptr+$35+fnL]:=$30;//0
    //Load address
    Fbuffer[ptr+$36+fnL]:=filetozip.LoadAddr mod $100;
    Fbuffer[ptr+$37+fnL]:=(filetozip.LoadAddr>>8)mod $100;
    Fbuffer[ptr+$38+fnL]:=(filetozip.LoadAddr>>16)mod $100;
    Fbuffer[ptr+$39+fnL]:=(filetozip.LoadAddr>>24)mod $100;
    //Exec address
    Fbuffer[ptr+$3A+fnL]:=filetozip.ExecAddr mod $100;
    Fbuffer[ptr+$3B+fnL]:=(filetozip.ExecAddr>>8)mod $100;
    Fbuffer[ptr+$3C+fnL]:=(filetozip.ExecAddr>>16)mod $100;
    Fbuffer[ptr+$3D+fnL]:=(filetozip.ExecAddr>>24)mod $100;
    //Attributes
    Fbuffer[ptr+$3E+fnL]:=filetozip.Attributes;
    //Blank off the rest
    if adjust>0 then
    begin
     for index:=17 to adjust-1 do Fbuffer[ptr+fnL+index+$2E]:=0;
     //Now we need to replicate this extra field into the main header
     SetLength(Fbuffer,Length(Fbuffer)+adjust);//First, extend again
     for index:=Length(Fbuffer)-1 downto dataptr+$1E+fnL+exL+adjust do
      Fbuffer[index]:=Fbuffer[index-adjust];
    end;
    //Now just replicate from above
    inc(ptr,adjust);//Don't forget this has moved too
    for index:=0 to newExL-1 do
     Fbuffer[dataptr+$1E+fnL+index]:=Fbuffer[ptr+$2E+fnL+index];
    //And update the field in the main header
    Fbuffer[dataptr+$1C]:=(exL+adjust)mod$100;
    Fbuffer[dataptr+$1D]:=(exL+adjust)>>8;
    //Get the compressed size of the file
    filetozip.Length:=Length(buffer);
    filetozip.Size:=Fbuffer[dataptr+$12]
                   +Fbuffer[dataptr+$13]<<8
                   +Fbuffer[dataptr+$14]<<16
                   +Fbuffer[dataptr+$15]<<24;
    //And the data offset
    filetozip.DataOffset:=dataptr;
    //Update the End of Central Library
    inc(EoCL,adjust*2);
    //Update Offset of CL
    inc(CL,adjust);
    UpdateCL(CL,EoCL); //Write both fields
    //Save it back again
    SaveData;
    //And now add it to the overall list
    SetLength(FFileList,Length(FFileList)+1);
    FFileList[Length(FFileList)-1]:=filetozip;
    //Set the spark flag
    FIsSpark:=True;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Create an empty directory
-------------------------------------------------------------------------------}
procedure TSpark.CreateDirectory(path: String);
var
 index,
 ctr      : Integer;
 buffer   : TDIByteArray;
 EoCL,CL,
 offset   : Cardinal;
 datetime : QWord;
 year,
 month,
 day,
 hour,
 minute,
 second,
 ms       : Word;
 filetozip: TFileEntry;
const
 headersig: array[0..3] of Byte = ($50,$4B,$03,$04);
 clsig    : array[0..3] of Byte = ($50,$4B,$01,$02);
 eoclsig  : array[0..3] of Byte = ($50,$4B,$05,$06);
begin
 if FIsPack then exit;
 //Make sure that the path is not empty
 if path<>'' then
 begin
  //Ensure it ends with a directory specifier
  if path[Length(path)]<>'/' then path:=path+'/';
  //And does not start with the root
  if Length(path)>2 then if path[1]='$' then path:=Copy(path,3);
  //Set up the buffer for the header entry
  SetLength(buffer,Length(path)+$36);
  for index:=0 to 3 do buffer[index]:=headersig[index];
  //Version
  buffer[4]:=$0A;
  //Blank the rest
  for index:=5 to $1D do buffer[index]:=$00;
  //Modification time $0A/$0B
  DecodeDateTime(Now,year,month,day,hour,minute,second,ms);
  datetime:=(hour<<$B)OR(minute<<5)OR(second div 2);
  buffer[$0A]:=datetime mod $100;
  buffer[$0B]:=datetime>>8;
  //Modification date $0C/$0D
  datetime:=((year-1980)<<9)OR(month<<5)OR day;
  buffer[$0C]:=datetime mod $100;
  buffer[$0D]:=datetime>>8;
  //Filename length
  buffer[$1A]:=Length(path)mod$100;
  buffer[$1B]:=Length(path)>>8;
  //Extra length
  buffer[$1C]:=$18;
  //Filename
  for index:=1 to Length(path) do buffer[$1D+index]:=Ord(path[index]);
  //Fill in the extra field
  buffer[$1E+Length(path)]:=$41;//A
  buffer[$1F+Length(path)]:=$43;//C
  //Length of extra -4
  buffer[$20+Length(path)]:=$14;
  buffer[$21+Length(path)]:=$00;
  buffer[$22+Length(path)]:=$41;//A
  buffer[$23+Length(path)]:=$52;//R
  buffer[$24+Length(path)]:=$43;//C
  buffer[$25+Length(path)]:=$30;//0
  //Load address
  datetime:=((Floor(Now)-2)*8640000)+Floor((Now-Floor(Now))*8640000);
  buffer[$26+Length(path)]:=(datetime>>32)mod$100;//last byte of the time/date
  buffer[$27+Length(path)]:=$FD;
  buffer[$28+Length(path)]:=$FF;
  buffer[$29+Length(path)]:=$FF;
  //Exec address
  buffer[$2A+Length(path)]:= datetime mod $100; //RISC OS time/date
  buffer[$2B+Length(path)]:=(datetime>>8)mod $100;
  buffer[$2C+Length(path)]:=(datetime>>16)mod $100;
  buffer[$2D+Length(path)]:=(datetime>>24)mod $100;
  //Attributes
  buffer[$2E+Length(path)]:=$0F;
  //Blank off the rest
  for index:=0 to 7 do buffer[$2F+Length(path)+index]:=$00;
  //Find the end of the list
  CL:=0;
  EoCL:=FindEoCL(CL);
  //Increase the archive size
  SetLength(Fbuffer,Length(Fbuffer)+Length(buffer));
  //Move the data up
  if EoCL>0 then
   for index:=Length(Fbuffer)-1 downto CL do
    Fbuffer[index]:=Fbuffer[index-Length(buffer)];
  //There is no EoCL, so we need to create one
  if EoCL=0 then
  begin
   EoCL:=Length(Fbuffer);
   SetLength(Fbuffer,Length(Fbuffer)+$16);
   //Write the signature
   for index:=0 to 3 do Fbuffer[EoCL+index]:=eoclsig[index];
   //Reset the EoCL back to 0, for now
   EoCL:=0;
  end;
  //Then insert where the CL was
  for index:=0 to Length(buffer)-1 do Fbuffer[CL+index]:=buffer[index];
  //Remember where we put it
  offset:=CL;
  //Adjust the CL and EoCL
  inc(EoCL,Length(buffer));
  inc(CL,Length(buffer));
  //Update the CL location
  UpdateCL(CL,EoCL);
  //Now add the entry to the central database
  SetLength(buffer,Length(buffer)+$10);
  //Now move data around - start at the end so we don't overwrite anything
  for index:=Length(buffer)-1 downto $2E do buffer[index]:=buffer[index-$10];
  //Middle part
  for index:=$1F downto $06 do buffer[index]:=buffer[index-2];
  //CL signature
  for index:=0 to 3 do buffer[index]:=clsig[index];
  //Version
  buffer[$04]:=$14;
  //OS
  buffer[$05]:=$0D;
  //Zero out the non-required fields
  for index:=$20 to $29 do buffer[index]:=$00;
  //Offset of main entry
  buffer[$2A]:=offset mod$100;
  buffer[$2B]:=(offset>>8)mod$100;
  buffer[$2C]:=(offset>>16)mod$100;
  buffer[$2D]:=(offset>>24)mod$100;
  //Now to insert this into the Central Library
  //Increase the archive size
  SetLength(Fbuffer,Length(Fbuffer)+Length(buffer));
  //Move the data up
  for index:=Length(Fbuffer)-1 downto EoCL do
   Fbuffer[index]:=Fbuffer[index-Length(buffer)];
  //Then insert where the EoCL was
  for index:=0 to Length(buffer)-1 do Fbuffer[EoCL+index]:=buffer[index];
  //And update again
  inc(EoCL,Length(buffer));
  UpdateCL(CL,EoCL);
  //Count the number of entries
  ctr:=0;
  for index:=CL to EoCL do
   if (Fbuffer[index]=$50)
   and(Fbuffer[index+1]=$4B)
   and(Fbuffer[index+2]=$01)
   and(Fbuffer[index+3]=$02)then inc(ctr);
  //Number of entries
  Fbuffer[EoCL+$8]:=ctr mod$100;
  Fbuffer[EoCL+$9]:=(ctr>>8)mod$100;
  Fbuffer[EoCL+$A]:=ctr mod $100;
  Fbuffer[EoCL+$B]:=(ctr>>8)mod$100;
  //Save it back again
  SaveData;
  //Populate the entry
  filetozip.ArchiveName:=path;
  filetozip.Attributes:=$0F;
  filetozip.LoadAddr:=((datetime>>32)mod$100)+$FFFFFD00;
  filetozip.ExecAddr:=datetime and $FFFFFFFF;
  filetozip.DataOffset:=offset;
  filetozip.Directory:=True;
  filetozip.Size:=0;
  filetozip.Length:=0;
  filetozip.NumEntries:=0;
  //Sort the filename and path out for RISC OS
  RISCOSFilename(path,True,filetozip.Filename,filetozip.Parent);
  //And now add it to the overall list
  SetLength(FFileList,Length(FFileList)+1);
  FFileList[Length(FFileList)-1]:=filetozip;
  //Set the spark flag
  FIsSpark:=True;
 end;
end;

{-------------------------------------------------------------------------------
Rename, or move, a file or directory (published)
-------------------------------------------------------------------------------}
function TSpark.RenameFile(oldpath, newpath: String): Boolean;
begin
 Result:=False;
 //We do this to eliminate any false positives. Some will still get through
 if(oldpath<>'')and(newpath<>'')and(oldpath<>newpath)and(Length(Fbuffer)>0)then
 begin
  //Fire off the function
  Result:=RenameTheFile(oldpath,newpath);
  //A directory will return a false result, whatever
  if oldpath[Length(oldpath)]='/' then Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Delete a file/directory (published)
-------------------------------------------------------------------------------}
function TSpark.DeleteFile(filename: String):Boolean;
begin
 Result:=False;
 //We do this to eliminate any false positives. Some will still get through
 if(filename<>'')and(Length(Fbuffer)>0)then
 begin
  //Fire off the function
  Result:=DeleteTheFile(filename);
  //A directory will return a false result, whatever
  if filename[Length(filename)]='/' then Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Update the load and execution address
-------------------------------------------------------------------------------}
function TSpark.UpdateLoadExecAddress(path: String;load, exec: Cardinal): Boolean;
var
 ptr,
 dataptr : Cardinal;
 fnL     : Word;
 index   : Integer;
begin
 Result:=False;
 if FIsPack then exit;
 //No blanks
 if path<>'' then
 begin
  //Remove the root, if present
  if Length(path)>2 then if path[1]='$' then path:=Copy(path,3);
  //Set up our variables
  ptr:=0;
  dataptr:=0;
  fnL:=Length(path);
  //Find the entry
  if FindEntry(path,False,ptr,dataptr) then
  begin
   //Update the entries (CL)
   Fbuffer[ptr+fnL+$36]:=load mod$100;
   Fbuffer[ptr+fnL+$37]:=(load>>8)mod$100;
   Fbuffer[ptr+fnL+$38]:=(load>>16)mod$100;
   Fbuffer[ptr+fnL+$39]:=(load>>24)mod$100;
   Fbuffer[ptr+fnL+$3A]:=exec mod$100;
   Fbuffer[ptr+fnL+$3B]:=(exec>>8)mod$100;
   Fbuffer[ptr+fnL+$3C]:=(exec>>16)mod$100;
   Fbuffer[ptr+fnL+$3D]:=(exec>>24)mod$100;
   //Update the entries (header)
   Fbuffer[dataptr+fnL+$26]:=load mod$100;
   Fbuffer[dataptr+fnL+$27]:=(load>>8)mod$100;
   Fbuffer[dataptr+fnL+$28]:=(load>>16)mod$100;
   Fbuffer[dataptr+fnL+$29]:=(load>>24)mod$100;
   Fbuffer[dataptr+fnL+$2A]:=exec mod$100;
   Fbuffer[dataptr+fnL+$2B]:=(exec>>8)mod$100;
   Fbuffer[dataptr+fnL+$2C]:=(exec>>16)mod$100;
   Fbuffer[dataptr+fnL+$2D]:=(exec>>24)mod$100;
   //Save the data
   SaveData;
   //Update the entry in the list of files
   if Length(FFileList)>0 then
    for index:=0 to Length(FFileList)-1 do
     if FFileList[index].ArchiveName=path then
     begin
      FFileList[index].LoadAddr:=load;
      FFileList[index].ExecAddr:=exec;
     end;
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update a file's attributes
-------------------------------------------------------------------------------}
function TSpark.UpdateAttributes(path: String; attributes: Word): Boolean;
var
 ptr,
 dataptr : Cardinal;
 fnL     : Word;
 index   : Integer;
begin
 Result:=False;
 if FIsPack then exit;
 //No blanks
 if path<>'' then
 begin
  //Remove the root if present
  if Length(path)>2 then if path[1]='$' then path:=Copy(path,3);
  //Set up our variables
  ptr:=0;
  dataptr:=0;
  fnL:=Length(path);
  //Find the entry
  if FindEntry(path,False,ptr,dataptr) then
  begin
   //Update the entries (CL)
   Fbuffer[ptr+fnL+$3E]:=attributes mod$100;
   //Update the entries (header)
   Fbuffer[dataptr+fnL+$2E]:=attributes mod$100;
   //Save the data
   SaveData;
   //Update the entry in the list of files
   if Length(FFileList)>0 then
    for index:=0 to Length(FFileList)-1 do
     if FFileList[index].ArchiveName=path then
      FFileList[index].Attributes:=attributes;
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Split a path into parent and filename for RISC OS
-------------------------------------------------------------------------------}
procedure TSpark.RISCOSFilename(path: String;addroot: Boolean;
                                       var filename: String;var parent: String);
var
 index: Integer;
begin
 filename:='';
 if addroot then parent:='$' else parent:='';
 //Remove any trailing '/'
 if path[Length(path)]='/' then path:=LeftStr(path,Length(path)-1);
 //Swap the '/' for '.' and vice versa
 SwapDirSep(path);
 //Is there a directory separator?
 if Pos('.',path)=0 then //No
  filename:=path
 else
 begin
  //Yes, find the last one
  index:=Length(path);
  while(path[index]<>'.')and(index>1)do dec(index);
  filename:=Copy(path,index+1);
  if addroot then parent:=parent+'.';
  parent:=parent+Copy(path,1,index-1);
 end;
end;

{-------------------------------------------------------------------------------
Swap directory separators
-------------------------------------------------------------------------------}
procedure TSpark.SwapDirSep(var path: String);
var
 index: Integer;
begin
 for index:=1 to Length(path) do
  if path[index]='/' then path[index]:='.'
  else if path[index]='.' then path[index]:='/';
end;

{-------------------------------------------------------------------------------
Convert an attribute byte to a string (ADFS compatible)
-------------------------------------------------------------------------------}
function TSpark.ConvertAttribute(attr: Byte): String;
var
 cnt : Byte;
 temp: String;
begin
 Result:='';
 //Collate the attributes
 for cnt:=0 to 7 do
  if attr AND(1<<cnt)=1<<cnt then
   Result:=Result+NewAtts[cnt];
 //Reverse the attribute order to match actual ADFS
 temp:='';
 if Length(Result)>0 then
  for cnt:=Length(Result) downto 1 do
   temp:=temp+Result[cnt];
 Result:=temp;
end;

{-------------------------------------------------------------------------------
Convert an attribute string to a byte (ADFS compatible)
-------------------------------------------------------------------------------}
function TSpark.ConvertAttribute(attr: String): Byte;
var
 cnt : Byte;
begin
 //Convert the attributes from a string to a byte
 Result:=0;
 for cnt:=0 to Length(NewAtts)-1 do
  if Pos(NewAtts[cnt],attr)>0 then
   Result:=Result OR 1<<cnt;
end;
