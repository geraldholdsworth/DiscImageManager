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
 FForceInter          :=0;
 //Deal with Spark archives as a filing system (i.e. in this class)
 FSparkAsFS           :=True;
 //Allow DFS images which report number of sectors as zero
 FDFSzerosecs         :=False;
 //Allow files to go beyond the edge of the disc
 FDFSBeyondEdge       :=False;
 //Allow blank filenames in DFS
 FDFSAllowBlank       :=False;
 //Scan sub directories in ADFS, Amiga, DOS, Spark
 FScanSubDirs         :=True;
 //Use short filenames in DOS even if long filenames exist
 FDOSUseSFN           :=False;
 //Open DOS Partitions on ADFS
 FOpenDOSPart         :=True;
 //Add implied attributes for DFS/CFS/RFS
 FAddImpliedAttributes:=True;
 //Set the titles
 Fdisctitle           :=defdisctitle;
 Fafsdisctitle        :=defafsdisctitle;
 Famigadisctitle      :=defamigadisctitle;
 Frfstitle            :=defrfstitle;
 Frfscopyright        :=defrfscopyright;
end;
constructor TDiscImage.Create(Clone: TDiscImage);
var
 index: Cardinal=0;
begin
 inherited Create;
 //Reset the variables to default
 ResetVariables;
 SetDataLength(Length(Clone.RAWData));
 //Copy the raw data across
 for index:=0 to Length(Fdata)-1 do Fdata[index]:=Clone.RAWData[index];
 //ADFS Interleaving option
 FForceInter          :=Clone.InterleaveMethod;
 //Deal with Spark archives as a filing system (i.e. in this class)
 FSparkAsFS           :=Clone.SparkAsFS;
 //Allow DFS images which report number of sectors as zero
 FDFSzerosecs         :=Clone.AllowDFSZeroSectors;
 //Allow files to go beyond the edge of the disc
 FDFSBeyondEdge       :=Clone.DFSBeyondEdge;
 //Allow blank filenames in DFS
 FDFSAllowBlank       :=Clone.DFSAllowBlanks;
 //Scan sub directories in ADFS, Amiga, DOS, Spark
 FScanSubDirs         :=Clone.ScanSubDirs;
 //Use short filenames in DOS even if long filenames exist
 FDOSUseSFN           :=Clone.FDOSUseSFN;
 //Open DOS Partitions on ADFS
 FOpenDOSPart         :=Clone.OpenDOSPartitions;
 //Add implied attributes for DFS/CFS/RFS
 FAddImpliedAttributes:=Clone.AddImpliedAttributes;
 //Set the titles
 Fdisctitle           :=Clone.DefaultDiscTitle;
 Fafsdisctitle        :=Clone.DefaultAFSDiscTitle;
 Famigadisctitle      :=Clone.DefaultAmigaDiscTitle;
 Frfstitle            :=Clone.DefaultRFSTitle;
 Frfscopyright        :=Clone.DefaultRFSCopyright;
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
 if FDSKImage<>nil then FDSKImage.Free;
 Close;
 inherited;
end;

{-------------------------------------------------------------------------------
Calculate a CRC-32 for a file
-------------------------------------------------------------------------------}
function TDiscImage.GetFileCrc(filename: String;entry:Cardinal=0): String;
var
 buffer: TDIByteArray=nil;
begin
 Result:='error';
 if ExtractFile(filename,buffer,entry) then Result:=GetCRC(buffer);
end;

{-------------------------------------------------------------------------------
Calculate a MD5 for a file
-------------------------------------------------------------------------------}
function TDiscImage.GetFileMD5(filename: String;entry:Cardinal=0): String;
var
 buffer: TDIByteArray=nil;
begin
 Result:='error';
 if ExtractFile(filename,buffer,entry) then Result:=GetMD5(buffer);
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
 currentformat : Byte=0;
 queryformat   : Byte=0;
 ext           : String='';
 index         : Integer=0;
begin
 Result:='';
 //Save the current format
 currentformat:=FFormat;
 for queryformat:=$00 to $BF do
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
  if not ID_RFS      then //Acorn RFS
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
 d: Cardinal=0;
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
  diAcornRFS : ReadRFSImage;           //Acorn ROM FS
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
 FDiscDrive: TFileStream=nil;
 dscfile   : String='';
 ext       : String='';
 lsecsize  : Cardinal=0;
 Lheads    : Byte=0;
 Lcyl      : Word=0;
 procedure LWriteByte(b: Byte);
 begin
  FDiscDrive.WriteByte(b);
 end;
 procedure LWrite16b(b: Word);
 begin                                
  FDiscDrive.WriteByte((b>>8)MOD$100);
  FDiscDrive.WriteByte(b     MOD$100);
 end;
 procedure LWrite24b(b: Cardinal);
 begin                                 
  FDiscDrive.WriteByte((b>>16)MOD$100);
  FDiscDrive.WriteByte((b>>8) MOD$100);
  FDiscDrive.WriteByte(b      MOD$100);
 end;
 procedure LWrite32b(b: Cardinal);
 begin                                 
  FDiscDrive.WriteByte((b>>24)MOD$100);
  FDiscDrive.WriteByte((b>>16)MOD$100);
  FDiscDrive.WriteByte((b>>8) MOD$100);
  FDiscDrive.WriteByte(b      MOD$100);
 end;
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
   //Now create the *.dsc file, if it was successful
   if(Result)and(FcreateDSC)
   and(GetMajorFormatNumber=diAcornADFS)
   and(GetMinorFormatNumber=$F)then
   begin
    //Create the filename
    dscfile:=LeftStr(filename,Length(filename)-Length(ExtractFileExt(filename)))
            +'.dsc';
    //If there is one already, then don't create a new one
    if not SysUtils.FileExists(dscfile) then
    begin
     try
      FDiscDrive:=TFileStream.Create(dscfile,fmCreate OR fmShareDenyNone);
      FDiscDrive.Position:=0;
      //Offset 00, length 3 - reserved
      LWrite24b($000000);
      //Offset 03, length 1 - Block length descriptor
      LWriteByte($08); //Should this be 80 or 08???
      //Offset 04, length 1 - Density code (00 = MFM)
      LWriteByte($00);
      //Offset 05, length 4 - reserved
      LWrite32b($00000000);
      //Offset 09, length 3 - Sector size (usually 0x000100)
      if secsize<>0 then Lsecsize:=secsize else Lsecsize:=$100;
      LWrite24b(Lsecsize);
      //Offset 0C, length 1 - 01 = soft sectors
      LWriteByte($01);
      //Offset 0D, length 2 - Number of cylinders/tracks
      Lcyl:=Length(free_space_map[0]);
      LWrite16b(Lcyl);
      //Offset 0F, length 1 - Number of heads
      Lheads:=(Length(Fdata)div Lsecsize)div Lcyl;
      LWriteByte(Lheads);
      //Offset 10, length 2 - Reduced Write Current Cylinder
      LWrite16b($0080);
      //Offset 12, length 2 - Write Pre-compensation Cylinder
      LWrite16b($0080);
      //Offset 14, length 1 - landing zone
      LWriteByte($00);
      //Offset 15, length 1 - Seek time code
      LWriteByte($01);
     finally
      FDiscDrive.Free;
     end;
    end;
   end;
  except
   //Could not create
   Result:=False;
  end;
 end;
 if GetMajorFormatNumber=diAcornUEF then
  WriteUEFFile(filename,uncompress); //CFS
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
function TDiscImage.FormatFDD(major: Word;minor: Byte;tracks: Byte=0): Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major AND $FFF;
 minor :=minor AND $F;
 if major<>diDOSPlus then tracks:=tracks MOD 2;
 case major of
  diAcornDFS : Result:=FormatDFS(minor,tracks); //Create DFS
  diAcornADFS: Result:=FormatADFSFloppy(minor); //Create ADFS
  diCommodore: Result:=FormatCDR(minor);        //Create Commodore 64/128
  diSinclair : Result:=FormatSpectrum(minor);   //Create Sinclair/Amstrad
  diAmiga    : Result:=FormatAmigaFDD(minor);   //Create AmigaDOS
  diAcornUEF : Result:=FormatCFS;               //Create Acorn CFS
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
 //We've failed to create something, so reset everything
 if not Result then ResetVariables;
end;
function TDiscImage.FormatFDD(major: Word; filename: String): Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major AND $FFF;
 if major=diSpark then Result:=FormatSpark(filename);   //Create Spark
 //We've failed to create something, so reset everything
 if not Result then ResetVariables;
end;
function TDiscImage.FormatFDD(major: Word): Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major AND $FFF;
 if major=diAcornUEF then Result:=FormatCFS;               //Create CFS
 //We've failed to create something, so reset everything
 if not Result then ResetVariables;
end;
function TDiscImage.FormatFDD(major: Word;title: String;version: String;
                                      copyright: String;binvers: Byte): Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major AND $FFF;
 if major=diAcornRFS then Result:=FormatRFS(title,copyright,version,binvers);//Create ROM FS
 //We've failed to create something, so reset everything
 if not Result then ResetVariables;
end;

{-------------------------------------------------------------------------------
Create and format a new hard disc image
-------------------------------------------------------------------------------}
function TDiscimage.FormatHDD(major: Word;harddrivesize: Cardinal):Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major AND $FFF;
 if major=diAmiga then Result:=FormatAmigaHDD(harddrivesize); //Create Amiga HDD
 //We've failed to create something, so reset everything
 if not Result then ResetVariables;
end;
function TDiscimage.FormatHDD(major: Word;harddrivesize: Cardinal;
                                                        dirtype: Byte): Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major AND $FFF;
 case major of
  diAcornFS  : Result:=FormatAFS(harddrivesize,dirtype);//Create Acorn FS
  diDOSPlus  : Result:=FormatDOS(harddrivesize,dirtype);//Create DOS HDD
 end;
 //We've failed to create something, so reset everything
 if not Result then ResetVariables;
end;
function TDiscimage.FormatHDD(major: Word;harddrivesize: Cardinal;
                 ide,newmap: Boolean;dirtype: Byte;addheader: Boolean): Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major AND $FFF;
 case major of //First three are just repeats of above, so we'll call the above
  diAcornFS  : Result:=FormatHDD(major,harddrivesize,dirtype);//Create Acorn FS
  diDOSPlus  : Result:=FormatHDD(major,harddrivesize,dirtype);//Create DOS HDD
  diAmiga    : Result:=FormatHDD(major,harddrivesize);        //Create Amiga HDD
  diAcornADFS: Result:=FormatADFSHDD(harddrivesize,
                                     newmap,
                                     dirtype,
                                     ide,
                                     addheader);        //Create ADFS
 end;
 //We've failed to create something, so reset everything
 if not Result then ResetVariables;
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
  diAcornRFS :Result:=ExtractRFSFile(entry,buffer);        //Extract ROM FS
  diSpark    :Result:=ExtractSparkFile(filename,buffer);   //Extract Spark
  diAcornFS  :Result:=ExtractAFSFile(filename,buffer);     //Extract Acorn FileStore
  diDOSPlus  :Result:=ExtractDOSFile(filename,buffer);     //Extract DOS Plus
 end;
end;
function TDiscImage.ExtractFile(filename:String;Stream:TStream;
                                                     entry:Cardinal=0): Boolean;
var
 Lbuffer: TDIByteArray=nil;
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
 count : Integer=0;
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
    diAcornRFS :Result:=WriteRFSFile(file_details,buffer);     //Write ROM FS
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
 dir  : Cardinal=0;
 entry: Cardinal=0;
begin
 dir:=$FFFF;
 entry:=$FFFF;
 Result:=FileExists(filename,dir,entry,sfn);
 Ref:=dir*$10000+entry;
end;
function TDiscImage.FileExists(filename: String;var dir,entry: Integer;sfn: Boolean=False): Boolean;
var
 Ldir  :Cardinal=0;
 Lentry:Cardinal=0;
begin
 Result:=FileExists(filename,Ldir,Lentry,sfn);
 if dir=$FFFF then dir:=-1 else dir:=Ldir;
 if entry=$FFFF then entry:=-1 else entry:=Lentry;
end;
function TDiscImage.FileExists(filename: String;var dir,entry: Cardinal;sfn: Boolean=False): Boolean;
var
 Path   : array of String=nil;
 i      : Integer=0;
 j      : Integer=-1;
 l      : Integer=0;
 ptr    : Integer=-1;
 level  : Integer=0;
 test   : String='';
 test2  : String='';
begin
 //This will not work with CFS as you can have multiple files with the same name
 //in the same 'directory'. It will just find the first occurance.
 Result:=False;
 dir:=$FFFF;
 entry:=$FFFF;
 //Blank filename, so quit
 if Length(filename)=0 then exit;
 //Ends in a DOS directory separator
 if((filename[Length(filename)]='\')
 or (filename[Length(filename)]='/'))
 and(GetMajorFormatNumber=diDOSPlus)then //But only for DOS images
  filename:=LeftStr(filename,Length(filename)-1);
 //Blank filename, so quit
 if Length(filename)=0 then exit;
 //For the root, we'll just return a default value
 if filename=root_name then
 begin
  dir:=$FFFF;
  entry:=$FFFF;
  Result:=True;
  exit;
 end;
 //AFS or DOS Root
 if(UpperCase(filename)=UpperCase(afsrootname))
 or(UpperCase(filename)=UpperCase(dosrootname))then
  if Length(FDisc)>0 then
   if Length(FDisc)>1 then
   begin
    i:=0;
    //Just look for the root
    while(i<Length(FDisc))
      and(UpperCase(FDisc[i].Directory)<>UpperCase(filename))do inc(i);
    if UpperCase(FDisc[i].Directory)=UpperCase(filename) then
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
 i      : Cardinal=0;
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
 i   : Cardinal=0;
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
function TDiscImage.FileSearch(search: TDirEntry;AddTo: TSearchResults=nil): TSearchResults;
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
 dir    : Integer=0;
 entry  : Integer=0;
 Index  : Integer=0;
 found  : Byte=0;
 target : Byte=0;
 Ldirsep: Char='.';
 Exclude: Boolean=False;
 nodups : Boolean=False;
begin
 Result:=AddTo; //We're adding to a previous search
 Ldirsep:='.';
 //Reset the search results array to empty
 //SetLength(Result,0);
 //Are we removing entries?
 if search.Filename[1]='|' then
 begin
  Exclude:=True;
  search.Filename:=Copy(search.Filename,2);
 end;
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
      //Yes, are we excluding or including?
      if Exclude then
      begin //Remove from the results
       for Index:=0 to Length(Result)-1 do
        if (Result[Index].Parent=FDisc[dir].Entries[entry].Parent)
        and(Result[Index].Filename=FDisc[dir].Entries[entry].Filename)then
         ResetDirEntry(Result[Index]);
      end
      else
      begin //Add to the results
       //But only if they're not there already
       nodups:=False;
       for Index:=0 to Length(Result)-1 do
        if (Result[Index].Parent=FDisc[dir].Entries[entry].Parent)
        and(Result[Index].Filename=FDisc[dir].Entries[entry].Filename)then
         nodups:=True;
       if not nodups then
       begin
        SetLength(Result,Length(Result)+1);
        Result[Length(Result)-1]:=FDisc[dir].Entries[entry];
       end;
      end;
      //User will still need to call FileExists to get the dir and entry references
     end;
    end;
 //Now remove any blank entries
 if Length(Result)>0 then
 begin
  Index:=0;
  while Index<Length(Result) do
  begin
   if(Result[Index].Parent='')and(Result[Index].Filename='')then
   begin
    if Index<Length(Result)-2 then
     for dir:=Index to Length(Result)-2 do Result[dir]:=Result[dir+1];
    SetLength(Result,Length(Result)-1);
    dec(Index);
   end;
   inc(Index);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Rename a file - oldfilename is full path, newfilename has no path
-------------------------------------------------------------------------------}
function TDiscImage.RenameFile(oldfilename: String;var newfilename: String): Integer;
begin
 Result:=-1;//Failed to rename
 case GetMajorFormatNumber of
  diAcornDFS : Result:=RenameDFSFile(oldfilename,newfilename);     //Rename DFS
  diAcornADFS: Result:=RenameADFSFile(oldfilename,newfilename);    //Rename ADFS
  diCommodore: Result:=RenameCDRFile(oldfilename,newfilename);     //Rename Commodore 64/128
  diSinclair : Result:=RenameSpectrumFile(oldfilename,newfilename);//Rename Sinclair/Amstrad
  diAmiga    : Result:=RenameAmigaFile(oldfilename,newfilename);   //Rename AmigaDOS
  diAcornFS  : Result:=RenameAFSFile(oldfilename,newfilename);     //Rename AFS
  diSpark    : Result:=RenameSparkFile(oldfilename,newfilename);   //Rename Spark
  diDOSPlus  : Result:=RenameDOSFile(oldfilename,newfilename);     //Rename DOS Plus
 end;
end;
function TDiscImage.RenameFile(entry:Cardinal;var newfilename: String): Integer;
begin
 Result:=-1;//Failed to rename
 case GetMajorFormatNumber of
  diAcornUEF : Result:=RenameCFSFile(entry,newfilename);           //Rename CFS
  diAcornRFS : Result:=RenameRFSFile(entry,newfilename);           //Rename RFS
 end;
end;

{-------------------------------------------------------------------------------
Deletes a file (given full pathname)
-------------------------------------------------------------------------------}
function TDiscImage.DeleteFile(filename: String): Boolean;
begin
 Result:=False;
 case GetMajorFormatNumber of
  diAcornDFS : Result:=DeleteDFSFile(filename);     //Delete DFS
  diAcornADFS: Result:=DeleteADFSFile(filename);    //Delete ADFS
  diCommodore: Result:=DeleteCDRFile(filename);     //Delete Commodore 64/128
  diSinclair : Result:=DeleteSinclairFile(filename);//Delete Sinclair/Amstrad
  diAmiga    : Result:=DeleteAmigaFile(filename);   //Delete AmigaDOS
  diAcornFS  : Result:=DeleteAFSFile(filename);     //Delete Acorn FS
  diSpark    : Result:=DeleteSparkFile(filename);   //Delete SparkFS
  diDOSPlus  : Result:=DeleteDOSFile(filename);     //Delete DOS Plus
 end;
end;
function TDiscImage.DeleteFile(entry: Cardinal): Boolean;
begin
 Result:=False;
 case GetMajorFormatNumber of
  diAcornUEF : Result:=DeleteCFSFile(entry);        //Delete CFS
  diAcornRFS : Result:=DeleteRFSFile(entry);        //Delete ROM FS
 end;
end;

{-------------------------------------------------------------------------------
Moves a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.MoveFile(filename,directory: String): Integer;
begin
 Result:=-12;
 case GetMajorFormatNumber of
  diAcornDFS : Result:=MoveDFSFile(filename,directory);  //Move DFS File
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
 //Make sure that the source and dest given are valid and not identical
 if(source<>dest)and(dest>=-1)and(dest<Length(FDisc[0].Entries))
 and(source<Length(FDisc[0].Entries))then
  case GetMajorFormatNumber of
   diAcornUEF: Result:=MoveCFSFile(source,dest);         //Move CFS File
   diAcornRFS: Result:=MoveRFSFile(source,dest);         //Move ROM FS File
  end;
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
 buffer      : TDIByteArray=nil;
 ptr         : Cardinal=0;
 entry       : Cardinal=0;
 dir         : Cardinal=0;
 d           : Cardinal=0;
 e           : Cardinal=0;
 tempfn      : String='';
 newparent   : String='';
 file_details: TDirEntry=();
begin
 ResetDirEntry(file_details);
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
 //Make sure that the source and dest given are valid and not identical
 if(source<>dest)and(dest>=-1)and(dest<Length(FDisc[0].Entries))
 and(source<Length(FDisc[0].Entries))then
  case GetMajorFormatNumber of
   diAcornUEF: Result:=CopyCFSFile(source,dest);         //Copy CFS File
   diAcornRFS: Result:=CopyRFSFile(source,dest);         //Copy ROM FS File
  end;
end;

{-------------------------------------------------------------------------------
Set the attributes for a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateAttributes(filename,attributes: String;entry:Cardinal=0):Boolean;
begin
 Result:=False;
 if(filename<>root_name)and(filename<>afsrootname)then
 begin
  //Validate the attributes
  ValidateAttributes(attributes);
  //Set the attributes
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
end;

{-------------------------------------------------------------------------------
Validate the attributes
-------------------------------------------------------------------------------}
procedure TDiscImage.ValidateAttributes(var attributes: String);
var
 i   : Integer=0;
 new : String='';
 attr: String='';
begin
 //Get the valid attributes for the current format
 case GetMajorFormatNumber of
  diAcornDFS : attr:='L';
  diAcornADFS: if Fdirtype=diADFSOldDir then
                attr:=ADFSOldAttributes else
                attr:=ADFSNewAttributes;
  diCommodore: attr:='LC';
  diSinclair : ;
  diAmiga    : attr:=AmigaAttributes;
  diAcornUEF : attr:='L';
  diAcornFS  : attr:='WRLwr';
  diSpark    : attr:=ADFSNewAttributes;
  diDOSPlus  : attr:='HRSA';
 end;
 //Remove any invalid characters
 if(Length(attributes)>0)and(Length(attr)>0)then
 begin
  new:='';
  for i:=1 to Length(attributes) do
   if Pos(attributes[i],attr)>0 then
    new:=new+attributes[i];
  attributes:=new;
 end;
 //Remove any repeated characters
 if Length(attributes)>1 then
 begin
  new:=attributes[1];
  for i:=2 to Length(attributes) do
   if Pos(attributes[i],new)=0 then new:=new+attributes[i];
  attributes:=new;
 end;
end;

{-------------------------------------------------------------------------------
Set the disc title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDiscTitle(NewTitle: String;side: Byte): Boolean;
begin
 Result:=False;
 case GetMajorFormatNumber of
  diAcornDFS : Result:=UpdateDFSDiscTitle(NewTitle,side);//Title DFS Disc
  diAcornADFS:
   begin
    if(FDOSPresent)and(side=1)then
     Result:=UpdateDOSDiscTitle(NewTitle)                //Title DOS Plus on ADFS hybrid disc
    else
     Result:=UpdateADFSDiscTitle(NewTitle);              //Title ADFS Disc
   end;
  diCommodore: Result:=UpdateCDRDiscTitle(NewTitle);     //Title Commodore 64/128 Disc
  diSinclair : Result:=UpdateSinclairDiscTitle(NewTitle);//Title Sinclair/Amstrad Disc
  diAmiga    : Result:=UpdateAmigaDiscTitle(NewTitle);   //Title AmigaDOS Disc
  diAcornUEF : exit;                                     //Can't retitle CFS
  diAcornFS  : Result:=UpdateAFSDiscTitle(NewTitle);     //Title AFS Disc
  diDOSPlus  : Result:=UpdateDOSDiscTitle(NewTitle);     //Title DOS Plus Disc
  diAcornRFS : Result:=UpdateRFSTitle(NewTitle);         //Title ROM FS
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
Change the Version String
-------------------------------------------------------------------------------}
function TDiscImage.UpdateVersionString(version: String): Boolean;
begin
 if GetMajorFormatNumber=diAcornRFS then
  Result:=UpdateRFSVersion(version)
 else
  Result:=False;
end;

{-------------------------------------------------------------------------------
Change the copyright string
-------------------------------------------------------------------------------}
function TDiscImage.UpdateCopyright(copyright: String): Boolean;
begin
 if GetMajorFormatNumber=diAcornRFS then
  Result:=UpdateRFSCopyright(copyright)
 else
  Result:=False;
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
function TDiscImage.GetFileType(filetype: Integer): String;
var
 i: Integer=1;
begin
 filetype:=filetype AND$FFF;
 Result:='';
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
function TDiscImage.GetFileType(filetype: String): Integer;
var
 i: Integer=1;
begin
 Result:=-1;
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
 ptr  : Cardinal=0;
 part : Cardinal=0;
 len  : Byte=0;
 ctr  : Byte=0;
 newfn: String='';
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
 Ldirsep: Char='.';
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
 buffer    : TDIByteArray=nil;
 FDiscDrive: TFileStream=nil;
 ext       : String='';
 oldformat : Word=0;
begin
 //If filename is empty, delete the current partition.
 //If filename is not empty, save the current partition
 Result:=False;
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
 if(GetMajorFormatNumber=diAcornDFS)and(not GetDoubleSided)then //Single sided images only
  Result:=AddDFSBlankSide(tracks);
end;
function TDiscImage.AddPartition(filename: String): Boolean;
begin
 Result:=False;
 //Only for Acorn DFS, given a filename
 if(GetMajorFormatNumber=diAcornDFS)and(not GetDoubleSided)then //Single sided images only
  Result:=AddDFSSide(filename);
end;

{-------------------------------------------------------------------------------
Change the Interleave Method
-------------------------------------------------------------------------------}
function TDiscImage.ChangeInterleaveMethod(NewMethod: Byte): Boolean;
var
 buffer: TDIByteArray=nil;
 index : Cardinal=0;
begin
 Result:=False;
 //Are we actually changing, and is it within range?
 if (NewMethod<>Finterleave)
 and(NewMethod>=Low(Fints)+1)
 and(NewMethod<=High(Fints)+1)then
  //Only works with ADFS L and Acorn FS, or ADFS with non-auto interleave
  if(FFormat=diAcornADFS<<4+2)
  or(FFormat=diAcornADFS<<4+$E)
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
 dir    : Cardinal=0;
 entry  : Cardinal=0;
 sector : Cardinal=0;
 len    : Cardinal=0;
 f      : Cardinal=0;
 NewDir : TDir;
begin
 RemoveControl(dirname);
 //This is only here to stop the hints that variables aren't intialised
 Result:=-1;
 NewDir.Directory:=dirname;
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
 temp  : String='';
 uline : String='';
 side  : Integer=0;
 report: TStringList=nil;
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
  if GetMajorFormatNumber=diCommodore then report:=CDRReport{(CSV)};//C64
  if GetMajorFormatNumber=diAmiga     then report:=AmigaReport{(CSV)};//Amiga
  if GetMajorFormatNumber=diSinclair  then report:=TStringList.Create;//Not written yet
  if GetMajorFormatNumber=diAcornUEF  then report:=TStringList.Create;//Nothing to report
  if GetMajorFormatNumber=diAcornRFS  then report:=TStringList.Create;//Nothing to report
  if GetMajorFormatNumber=diSpark     then report:=TStringList.Create;//Nothing to report
  //Incorporate the report
  if report.Count>0 then
   for side:=0 to report.Count-1 do Result.Add(report[side]);
  report.Free;
  //Re-jig the output if CSV has been specified
  if(CSV)and(Result.Count>0)then
   for side:=0 to Result.Count-1 do
    if Result[side][1]<>'"' then
     Result[side]:='"'
                  +StringReplace(Result[side],': ','","',[rfReplaceAll])
                  +'"';
 end;
end;

{-------------------------------------------------------------------------------
Convert an interleave into a string
-------------------------------------------------------------------------------}
function TDiscImage.GetInterleaveString(Inter: Byte): String;
begin
 Result:='unknown';
 if(FFormat=diAcornADFS<<4+2)
 or(FFormat=diAcornADFS<<4+$E)
 or(GetMajorFormatNumber=diAcornFS)then
  if Inter<=High(FInts)then Result:=FInts[Inter];
end;

{-------------------------------------------------------------------------------
Get the total number of interleave types
-------------------------------------------------------------------------------}
function TDiscImage.GetNumberOfInterleaves: Byte;
begin
 Result:=Length(FInts);
end;

{-------------------------------------------------------------------------------
Create a Windows filename
-------------------------------------------------------------------------------}
function TDiscImage.GetWindowsFilename(dir,entry: Integer): String;
var
 extsep: Char='.';
begin
 Result:='';
 if(dir>=0)and(dir<Length(FDisc))then
  if(entry>=0)and(entry<=Length(FDisc[dir].Entries))then
  begin
   extsep:=#0;
   //Get the filename
   Result:=FDisc[dir].Entries[entry].Filename;
   //Convert BBC chars to PC
   BBCtoWin(Result);
   //Replace any non-valid characters
   ValidateWinFilename(Result);
   //Add the filetype to the end, if any (but not directories)
   if (FDisc[dir].Entries[entry].ShortFileType<>'')
   and(FDisc[dir].Entries[entry].DirRef=-1) then
   begin
    if(GetMajorFormatNumber=diAcornDFS)
    or(GetMajorFormatNumber=diAcornADFS)
    or(GetMajorFormatNumber=diAcornFS)then extsep:=','; //DFS, ADFS and AFS
    if(GetMajorFormatNumber=diCommodore)                //Commodore
    or(GetMajorFormatNumber=diAmiga)then extsep:='.';   //Amiga
    Result:=Result+extsep+FDisc[dir].Entries[entry].ShortFileType;
   end;
  end;
end;

{-------------------------------------------------------------------------------
Create an inf file
-------------------------------------------------------------------------------}
procedure TDiscImage.CreateINFFile(dir,entry: Integer; path: String;filename: String='');
var
 F              : TFileStream=nil;
 Ltitle         : String='';
 inffile        : String='';
 Limagefilename : String='';
 windowsfilename: String='';
 attributes     : Byte=0;
 hexlen         : Byte=0;
 t              : Integer=0;
const
 adfsattr = 'RWELrwel';
begin
 if(dir>=0)and(dir<Length(FDisc))then
  if(entry>=0)and(entry<Length(FDisc[dir].Entries))then
  begin
   //Length of the hex numbers
   hexlen:=8;
   //6 for DFS (after discussion on Stardot forum)
   if GetMajorFormatNumber=diAcornDFS then hexlen:=6;
   Limagefilename:=FDisc[dir].Entries[entry].Filename;
   if filename='' then //If no filename has been supplied, generate one
    windowsfilename:=GetWindowsFilename(dir,entry)
   else                //Otherwise just use the supplied name
    windowsfilename:=filename;
   //Add the root, if DFS and no directory specifier
   if(GetMajorFormatNumber=diAcornDFS)and(Limagefilename[2]<>'.')then
    Limagefilename:=RightStr(GetParent(dir),1)+'.'+Limagefilename;
   //Put quotes round the filename if it contains a space
   if Pos(' ',Limagefilename)>0 then Limagefilename:='"'+Limagefilename+'"';
   //Create the string
   inffile:=PadRight(LeftStr(Limagefilename,12),12)+' '
           +IntToHex(FDisc[dir].Entries[entry].LoadAddr,hexlen)+' '
           +IntToHex(FDisc[dir].Entries[entry].ExecAddr,hexlen)+' '
           +IntToHex(FDisc[dir].Entries[entry].Length,hexlen);
   //Create the attributes
   attributes:=$00;
   if(GetMajorFormatNumber=diAcornDFS)
   or(GetMajorFormatNumber=diAcornUEF)
   or(GetMajorFormatNumber=diAcornRFS)then //DFS, CFS and RFS
   begin
    if FDisc[dir].Entries[entry].Attributes='L' then attributes:=$08;
    if FAddImpliedAttributes then
     inc(attributes,$01+$02); //Add the 'RW' attributes as implied attributes
   end;
   if(GetMajorFormatNumber=diAcornADFS)
   or(GetMajorFormatNumber=diAcornFS)then //ADFS and AFS
    for t:=0 to 7 do
     if Pos(adfsattr[t+1],FDisc[dir].Entries[entry].Attributes)>0 then
      inc(attributes,1<<t);
   inffile:=inffile+' '+IntToHex(attributes,2);
   //Timestamp word (for AFS compatibility)
   if FDisc[dir].Entries[entry].TimeStamp<>0 then
    inffile:=inffile+' '
            +IntToHex(DateTimeToAFS(FDisc[dir].Entries[entry].TimeStamp),4);
   //CRC
   inffile:=inffile+' CRC32='+GetFileCRC(GetParent(dir)+
                              GetDirSep(FDisc[dir].Partition)+
                              FDisc[dir].Entries[entry].Filename,
                              entry);
   //Timestamp (for RISC OS and DOS compatibility)
   if FDisc[dir].Entries[entry].TimeStamp<>0 then
    inffile:=inffile+' DATETIME='+FormatDateTime('yyyymmddhhnnss',
                                     FDisc[dir].Entries[entry].TimeStamp);
   //Directory?
   if FDisc[dir].Entries[entry].DirRef<>-1 then
   begin
    Ltitle:=FDisc[FDisc[dir].Entries[entry].DirRef].Title;
    if Pos(' ',Ltitle)>0 then Ltitle:='"'+Ltitle+'"';
    inffile:=inffile+' DIRTITLE='+PadRight(LeftStr(Ltitle,21),21);
   end;
   //Create the inf file
   try
    F:=TFileStream.Create(path+windowsfilename+'.inf',fmCreate OR fmShareDenyNone);
    F.Position:=0;
    F.Write(inffile[1],Length(inffile));
    F.Free;
   except
    //Could not create
   end;
  end;
end;

{-------------------------------------------------------------------------------
Create an inf file for a root
-------------------------------------------------------------------------------}
procedure TDiscImage.CreateRootInf(filename: String; dir: Integer);
var
 F        : TFileStream=nil;
 Ltitle   : String='';
 dirtitle : String='';
 inffile  : String='';
begin
 Ltitle:=Title(FDisc[dir].Partition);
 if Pos(' ',Ltitle)>0 then Ltitle:='"'+Ltitle+'"';
 dirtitle:=FDisc[dir].Title;
 if Pos(' ',dirtitle)>0 then dirtitle:='"'+dirtitle+'"';
 inffile:=PadRight(LeftStr(FDisc[dir].Directory,12),12)
         +' OPT='+IntToHex(BootOpt[FDisc[dir].Partition],2)
         +' TITLE='+PadRight(LeftStr(Ltitle,12),12)
         +' DIRTITLE='+PadRight(LeftStr(dirtitle,21),21);
 //Create the inf file
 try
  F:=TFileStream.Create(filename+'.inf',fmCreate OR fmShareDenyNone);
  F.Position:=0;
  F.Write(inffile[1],Length(inffile));
  F.Free;
 except
  //Could not create
 end;
end;

//++++++++++++++++++ TSpark Published Methods ++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Create the instance
-------------------------------------------------------------------------------}
constructor TSpark.Create(filename: String;blank: Boolean=false);
var
 F        : TFileStream=nil;
 outbuffer: array of Byte;
begin
 //Set the filename
 ZipFilename:=filename;
 //Create a blank file
 if blank then
 begin
  SetLength(outbuffer,22);
  WriteSignature(ZIPEoCL,outbuffer);
  F:=TFileStream.Create(ZipFilename,fmCreate or fmShareDenyNone);
  F.Position:=0;
  F.Write(outbuffer[0],Length(outbuffer));
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
 Fversion   :='1.06';
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
 function CreateTempZip: String;
 var
  tempname : String='';
  tempfile : TFileStream=nil;
  zipfile  : TZipper=nil;
  EoCL     : Integer=-1;
  CL       : Integer=-1;
  ptr      : Cardinal=0;
  dataptr  : Cardinal=0;
  fnL      : Word=0;
  exL      : Word=0;
  index    : Integer=0;
  adjust   : Integer=0;
  Lbuffer  : TDIByteArray=nil;
 const newExL = $18;
 begin
  Result:=ExtractFilePath(ZIPFileName)+'__DIM__TempArchive.zip';//Get a temporary filename for the ZIP
  //Zipper will only zip up existing files, so we'll need to save the data to a
  //temporary file first, then zip that.
  tempname:=ExtractFilePath(ZIPFileName)+'__DIM__TempFile.dat';//Get a temporary name
  tempfile:=TFileStream.Create(tempname,fmCreate);//Create the file
  tempfile.Position:=0;
  tempfile.Write(buffer[0],Length(buffer)); //Write the file data to it
  tempfile.Free; //Close the file
  //Now we can open the zipfile
  zipfile:=TZipper.Create;
  try
   zipfile.Filename:=Result; //Set the zipfile name
   zipfile.Entries.AddFileEntry(tempname,filetozip.ArchiveName); //Add the file
   zipfile.ZipAllFiles; //Then zip all the files
  finally
   zipfile.Free; //And close it
  end;
  //Finally, delete the temporary file
  SysUtils.DeleteFile(tempname);
  //Then we will need to load the zip file in and change the values and add to
  //the library at the end.
  tempfile:=TFileStream.Create(Result,fmOpenRead or fmShareDenyNone);
  SetLength(Lbuffer,tempfile.Size);
  tempfile.Position:=0;
  tempfile.Read(Lbuffer[0],tempfile.Size);
  tempfile.Free;
  CL:=FindCL(EoCL,Lbuffer); //Get the end of central library marker
  if(EoCL<>-1)and(CL<>-1)then
  begin
   //Find the entry for this file
   if FindEntry(filetozip.ArchiveName,false,ptr,dataptr,LBuffer) then
    if ptr+$1F<Length(Lbuffer) then
    begin
     //Get the variable length fields lengths
     fnL:=Lbuffer[ptr+$1C]+Lbuffer[ptr+$1D]<<8;
     exL:=Lbuffer[ptr+$1E]+Lbuffer[ptr+$1F]<<8;
     //We need to add up to 24 bytes, so everything from the next entry needs to shift along
     adjust:=0;
     if exL<newExL then adjust:=newExL-exL;
     if adjust>0 then
     begin
      SetLength(Lbuffer,Length(Lbuffer)+adjust);
      for index:=Length(Lbuffer)-1 downto ptr+$2E+fnL+exL+adjust do
       Lbuffer[index]:=Lbuffer[index-adjust];
     end;
     if ptr+$3E+fnL<Length(Lbuffer) then
     begin
      //Update the OS type to RISC OS
      Lbuffer[ptr+$05]:=13;
      //Update the extra field length
      Lbuffer[ptr+$1E]:=(exL+adjust)mod$100;
      Lbuffer[ptr+$1F]:=(exL+adjust)>>8;
      //Fill in the extra field
      Lbuffer[ptr+$2E+fnL]:=$41;//A
      Lbuffer[ptr+$2F+fnL]:=$43;//C
      //Length of extra -4
      Lbuffer[ptr+$30+fnL]:=((exL+adjust)-4)mod$100;
      Lbuffer[ptr+$31+fnL]:=((exL+adjust)-4)>>8;
      Lbuffer[ptr+$32+fnL]:=$41;//A
      Lbuffer[ptr+$33+fnL]:=$52;//R
      Lbuffer[ptr+$34+fnL]:=$43;//C
      Lbuffer[ptr+$35+fnL]:=$30;//0
      //Load address
      Lbuffer[ptr+$36+fnL]:=filetozip.LoadAddr mod $100;
      Lbuffer[ptr+$37+fnL]:=(filetozip.LoadAddr>>8)mod $100;
      Lbuffer[ptr+$38+fnL]:=(filetozip.LoadAddr>>16)mod $100;
      Lbuffer[ptr+$39+fnL]:=(filetozip.LoadAddr>>24)mod $100;
      //Exec address
      Lbuffer[ptr+$3A+fnL]:=filetozip.ExecAddr mod $100;
      Lbuffer[ptr+$3B+fnL]:=(filetozip.ExecAddr>>8)mod $100;
      Lbuffer[ptr+$3C+fnL]:=(filetozip.ExecAddr>>16)mod $100;
      Lbuffer[ptr+$3D+fnL]:=(filetozip.ExecAddr>>24)mod $100;
      //Attributes
      Lbuffer[ptr+$3E+fnL]:=filetozip.Attributes;
      //Blank off the rest
      if adjust>0 then
      begin
       for index:=17 to adjust-1 do Lbuffer[ptr+fnL+index+$2E]:=0;
       //Now we need to replicate this extra field into the main header
       SetLength(Lbuffer,Length(Lbuffer)+adjust);//First, extend again
       for index:=Length(Lbuffer)-1 downto dataptr+$1E+fnL+exL+adjust do
        Lbuffer[index]:=Lbuffer[index-adjust];
      end;
      //Now just replicate from above
      inc(ptr,adjust);//Don't forget this has moved too
      for index:=0 to newExL-1 do
       Lbuffer[dataptr+$1E+fnL+index]:=Lbuffer[ptr+$2E+fnL+index];
      //And update the field in the main header
      Lbuffer[dataptr+$1C]:=(exL+adjust)mod$100;
      Lbuffer[dataptr+$1D]:=(exL+adjust)>>8;
      //Get the compressed size of the file
      filetozip.Length:=Length(buffer);
      filetozip.Size:=Lbuffer[dataptr+$12]
                     +Lbuffer[dataptr+$13]<<8
                     +Lbuffer[dataptr+$14]<<16
                     +Lbuffer[dataptr+$15]<<24;
      //And the data offset
      filetozip.DataOffset:=dataptr;
      //Update the End of Central Library
      inc(EoCL,adjust*2);
      //Update Offset of CL
      inc(CL,adjust);
      UpdateCL(CL,EoCL,Lbuffer); //Write both fields
      //Save it back again
      tempfile:=TFileStream.Create(Result,fmCreate or fmShareDenyNone);
      tempfile.Position:=0;
      tempfile.Write(Lbuffer[0],Length(Lbuffer));
      tempfile.Free;
     end;
    end;
  end;
 end;
var
 tempzipfile: String='';
 Lfile      : TFileStream=nil;
begin
 if FIsPack then exit;
 if filetozip.Directory then CreateDirectory(filetozip.ArchiveName)
 else
 begin
  //First, ZIP up the file - this creates a new file
  tempzipfile:=CreateTempZip;
  //Combine the existing and new files
  CombineZIP([ZIPFilename,tempzipfile],ZIPFilename);
  //Now delete the temporary ZIP file
  SysUtils.DeleteFile(tempzipfile);
  //Read the file into the buffer
  Lfile:=TFileStream.Create(ZIPFilename,fmOpenRead OR fmShareDenyNone);
  Lfile.Position:=0;
  SetLength(Fbuffer,Lfile.Size);
  Lfile.Read(Fbuffer[0],Lfile.Size);
  Lfile.Free;
  //And now add it to the overall list
  SetLength(FFileList,Length(FFileList)+1);
  FFileList[Length(FFileList)-1]:=filetozip;
  //Set the spark flag
  FIsSpark:=True;
 end;
end;

{-------------------------------------------------------------------------------
Create an empty directory
-------------------------------------------------------------------------------}
procedure TSpark.CreateDirectory(path: String);
var
 index     : Integer=0;
 ctr       : Integer=0;
 databuffer: TDIByteArray=nil;
 clbuffer  : TDIByteArray=nil;
 EoCL      : Integer=-1;
 CL        : Integer=-1;
 offset    : Cardinal=0;
 datetime  : QWord=0;
 year      : Word=0;
 month     : Word=0;
 day       : Word=0;
 hour      : Word=0;
 minute    : Word=0;
 second    : Word=0;
 ms        : Word=0;
 filetozip : TFileEntry=();
begin
 //In this procedure, I've assigned each value to the array individually, even
 //though I could use a loop to do the same thing. Just because it is easier to
 //read and maintain.
 ResetFileEntry(filetozip);
 if FIsPack then exit;
 //Make sure that the path is not empty
 if path<>'' then
 begin
  //Ensure it ends with a directory specifier
  if path[Length(path)]<>'/' then path:=path+'/';
  //And does not start with the root
  if Length(path)>2 then if path[1]='$' then path:=Copy(path,3);
  //Set up the buffer for the header entry
  SetLength(databuffer,Length(path)+$36); // $36=$1E header + $18 extra
  WriteSignature(ZIPHeader,databuffer);
  //Version $04/$05
  databuffer[$04]:=$0A;
  databuffer[$05]:=$00;
  //Flags $06/$07
  databuffer[$06]:=$00;
  databuffer[$07]:=$00;
  //Compression $08/$09
  databuffer[$08]:=$00;
  databuffer[$09]:=$00;
  //Modification time $0A/$0B
  DecodeDateTime(Now,year,month,day,hour,minute,second,ms);
  datetime:=(hour<<$B)OR(minute<<5)OR(second div 2);
  databuffer[$0A]:=datetime mod $100;
  databuffer[$0B]:=datetime>>8;
  //Modification date $0C/$0D
  datetime:=((year-1980)<<9)OR(month<<5)OR day;
  databuffer[$0C]:=datetime mod $100;
  databuffer[$0D]:=datetime>>8;
  //CRC32 $0E/$0F/$10/$11 - just blank as there isn't any data
  databuffer[$0E]:=$00;
  databuffer[$0F]:=$00;
  databuffer[$10]:=$00;
  databuffer[$11]:=$00;
  //Compressed size $12/$13/$14/$15 - just blank as there isn't any data
  databuffer[$12]:=$00;
  databuffer[$13]:=$00;
  databuffer[$14]:=$00;
  databuffer[$15]:=$00;
  //Uncompressed $16/$17/$18/$19 - just blank as there isn't any data
  databuffer[$16]:=$00;
  databuffer[$17]:=$00;
  databuffer[$18]:=$00;
  databuffer[$19]:=$00;
  //Filename length $1A/$1B
  databuffer[$1A]:=Length(path)mod$100;
  databuffer[$1B]:=Length(path)>>8;
  //Extra length $1C/$1D - 24 bytes
  databuffer[$1C]:=$18;
  databuffer[$1D]:=$00;
  //Filename from $1E
  for index:=1 to Length(path) do databuffer[$1D+index]:=Ord(path[index]);
  //Fill in the extra fields
  //Two byte identifier 'AC'
  databuffer[$1E+Length(path)]:=$41;//A
  databuffer[$1F+Length(path)]:=$43;//C
  //Length of extra field
  databuffer[$20+Length(path)]:=$14; //20 bytes (24 minus the identifier and length)
  databuffer[$21+Length(path)]:=$00;
  //!SparkFS signature, followed by RISC OS stuff
  databuffer[$22+Length(path)]:=$41;//A
  databuffer[$23+Length(path)]:=$52;//R
  databuffer[$24+Length(path)]:=$43;//C
  databuffer[$25+Length(path)]:=$30;//0
  //Load address
  datetime:=((Floor(Now)-2)*8640000)+Floor((Now-Floor(Now))*8640000);
  databuffer[$26+Length(path)]:=(datetime>>32)mod$100;//last byte of the time/date
  databuffer[$27+Length(path)]:=$FD;
  databuffer[$28+Length(path)]:=$FF;
  databuffer[$29+Length(path)]:=$FF;
  //Exec address
  databuffer[$2A+Length(path)]:= datetime mod $100; //RISC OS time/date
  databuffer[$2B+Length(path)]:=(datetime>>8)mod $100;
  databuffer[$2C+Length(path)]:=(datetime>>16)mod $100;
  databuffer[$2D+Length(path)]:=(datetime>>24)mod $100;
  //Attributes
  databuffer[$2E+Length(path)]:=$0F;
  //Blank off the rest
  for index:=0 to 6 do databuffer[$2F+Length(path)+index]:=$00;
  //Find the end of the list
  CL:=FindCL(EoCL,Fbuffer);
  //Increase the archive size
  SetLength(Fbuffer,Length(Fbuffer)+Length(databuffer));
  //Move the data up
  if(EoCL<>-1)and(CL<>-1)then
   for index:=Length(Fbuffer)-1 downto CL do
    Fbuffer[index]:=Fbuffer[index-Length(databuffer)];
  //There is no EoCL, so we need to create one
  if EoCL=-1 then
  begin
   EoCL:=Length(Fbuffer);
   SetLength(Fbuffer,Length(Fbuffer)+$16);
   //Write the signature
   WriteSignature(ZIPEoCL,Fbuffer,EoCL);
   //Reset the EoCL back to 0, for now
   EoCL:=0;
  end;
  if CL=-1 then CL:=0;
  //Then insert where the CL was
  for index:=0 to Length(databuffer)-1 do Fbuffer[CL+index]:=databuffer[index];
  //Remember where we put it
  offset:=CL;
  //Adjust the CL and EoCL
  inc(EoCL,Length(databuffer));
  inc(CL,Length(databuffer));
  //Update the CL location
  UpdateCL(CL,EoCL,Fbuffer);
  //Now add the entry to the central database - this is largely the same as above
  SetLength(clbuffer,Length(databuffer)+$10);//But the CL is 16 bytes bigger
  //CL signature $00-$03
  WriteSignature(ZIPCL,clbuffer);
  //Version made by: ZIP Spec version $04
  clbuffer[$04]:=$14;
  //OS made by $05
  clbuffer[$05]:=$0D; //RISC OS
  //Version needed $06/$07
  clbuffer[$06]:=databuffer[$04];
  clbuffer[$07]:=databuffer[$05];
  //Flags $08/$09
  clbuffer[$08]:=databuffer[$06];
  clbuffer[$09]:=databuffer[$07];
  //Compression $0A/$0B
  clbuffer[$0A]:=databuffer[$08];
  clbuffer[$0B]:=databuffer[$09];
  //Modification time $0C/$0D
  clbuffer[$0C]:=databuffer[$0A];
  clbuffer[$0D]:=databuffer[$0B];
  //Modification date $0E/$0F
  clbuffer[$0E]:=databuffer[$0C];
  clbuffer[$0F]:=databuffer[$0D];
  //CRC-32 $10-$13
  clbuffer[$10]:=databuffer[$0E];
  clbuffer[$11]:=databuffer[$0F];
  clbuffer[$12]:=databuffer[$10];
  clbuffer[$13]:=databuffer[$11];
  //Compressed Size $14-$17
  clbuffer[$14]:=databuffer[$12];
  clbuffer[$15]:=databuffer[$13];
  clbuffer[$16]:=databuffer[$14];
  clbuffer[$17]:=databuffer[$15];
  //Uncompressed size $18-$1B
  clbuffer[$18]:=databuffer[$16];
  clbuffer[$19]:=databuffer[$17];
  clbuffer[$1A]:=databuffer[$18];
  clbuffer[$1B]:=databuffer[$19];
  //Filename length $1C/$1D
  clbuffer[$1C]:=databuffer[$1A];
  clbuffer[$1D]:=databuffer[$1B];
  //Extra fields length $1E/$1F
  clbuffer[$1E]:=databuffer[$1C];
  clbuffer[$1F]:=databuffer[$1D];
  //File comment length $20/$21
  clbuffer[$20]:=$00;
  clbuffer[$21]:=$00;
  //Disk # start $22/$23       
  clbuffer[$22]:=$00;
  clbuffer[$23]:=$00;
  //Internal address $24/$25
  clbuffer[$24]:=$00;
  clbuffer[$25]:=$00;
  //External address $26-$29
  clbuffer[$26]:=$00;
  clbuffer[$27]:=$00;
  clbuffer[$28]:=$00;
  clbuffer[$29]:=$00;
  //Offset of local header $2A-$2D
  databuffer[$2A]:= offset     mod$100;
  databuffer[$2B]:=(offset>> 8)mod$100;
  databuffer[$2C]:=(offset>>16)mod$100;
  databuffer[$2D]:=(offset>>24)mod$100;
  //Filename from $2E
  for index:=1 to Length(path) do clbuffer[$2D+index]:=Ord(path[index]);
  //Extra fields
  for index:=0 to $17 do
   clbuffer[$2E+Length(path)+index]:=databuffer[$1E+Length(path)+index];
  //Now to insert this into the Central Library
  //Increase the archive size
  SetLength(Fbuffer,Length(Fbuffer)+Length(clbuffer));
  //Move the data up
  for index:=Length(Fbuffer)-1 downto EoCL do
   Fbuffer[index]:=Fbuffer[index-Length(clbuffer)];
  //Then insert where the EoCL was
  for index:=0 to Length(clbuffer)-1 do Fbuffer[EoCL+index]:=clbuffer[index];
  //And update again
  inc(EoCL,Length(clbuffer));
  UpdateCL(CL,EoCL,Fbuffer);
  //Count the number of entries
  ctr:=0;
  for index:=CL to EoCL-3 do
   if (Fbuffer[index  ]=$50)
   and(Fbuffer[index+1]=$4B)
   and(Fbuffer[index+2]=$01)
   and(Fbuffer[index+3]=$02)then inc(ctr);
  //Number of entries
  if EoCL+$B<Length(Fbuffer) then
  begin
   Fbuffer[EoCL+$8]:=ctr mod$100;
   Fbuffer[EoCL+$9]:=(ctr>>8)mod$100;
   Fbuffer[EoCL+$A]:=ctr mod $100;
   Fbuffer[EoCL+$B]:=(ctr>>8)mod$100;
  end;
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
 ptr     : Cardinal=0;
 dataptr : Cardinal=0;
 fnL     : Word=0;
 index   : Integer=0;
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
  if FindEntry(path,False,ptr,dataptr,FBuffer) then
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
 ptr     : Cardinal=0;
 dataptr : Cardinal=0;
 fnL     : Word=0;
 index   : Integer=0;
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
  if FindEntry(path,False,ptr,dataptr,FBuffer) then
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
 index: Integer=0;
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
 index: Integer=0;
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
 cnt : Byte=0;
 temp: String='';
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
 cnt : Byte=0;
begin
 //Convert the attributes from a string to a byte
 Result:=0;
 for cnt:=0 to Length(NewAtts)-1 do
  if Pos(NewAtts[cnt],attr)>0 then
   Result:=Result OR 1<<cnt;
end;

{-------------------------------------------------------------------------------
Validate the file in memory
-------------------------------------------------------------------------------}
function TSpark.Validate: Boolean;
var
 CL      : Integer=-1;
 EoCL    : Integer=-1;
 temp    : Cardinal=0;
 numfiles: Integer=0;
 index   : Integer=0;
 ptr     : Cardinal=0;
 data    : Cardinal=0;
 size    : Cardinal=0;
 usize   : Cardinal=0;
 //Check the length of the extra field
 function CheckExtraField(addr,len: Cardinal): Boolean;
 var
  total: Cardinal=0;
 begin
  Result:=False;
  while(total<len)and(addr<Length(Fbuffer)-4)do
  begin
   //Extra field has pairs of 2 byte tag + 2 byte length
   inc(total,4+Fbuffer[addr+2]+Fbuffer[addr+3]<<8);
   inc(addr,4+Fbuffer[addr+2]+Fbuffer[addr+3]<<8);
  end;
  Result:=total=len;
 end;
begin
 Result:=False;
 //Minimum size of a ZIP file is 22 bytes
 if Length(Fbuffer)>=22 then
 begin
  //Find the central library
  CL:=FindCL(EoCL,Fbuffer);
  if(CL<>-1)and(EoCL<>-1)then
  begin
   Result:=True;
   //Size of Central Library
   temp:=Fbuffer[EoCL+$C]
        +Fbuffer[EoCL+$D]<<8
        +Fbuffer[EoCL+$E]<<16
        +Fbuffer[EoCL+$F]<<24;
   //Test to see if it matches the references already found
   Result:=(temp=EoCL-CL)AND(Result);
   //Total number of files
   numfiles:=Fbuffer[EoCL+$A]
            +Fbuffer[EoCL+$B]<<8;
   ptr:=CL;
   index:=0;
   while(index<numfiles)and(ptr<EoCL)do
   begin
    //Check the signature
    Result:=(Fbuffer[ptr  ]=$50)
         and(Fbuffer[ptr+1]=$4B)
         and(Fbuffer[ptr+2]=$01)
         and(Fbuffer[ptr+3]=$02)
         AND(Result);
    //Check the extra field
    temp:=Fbuffer[ptr+$1E]+Fbuffer[ptr+$1F]<<8;
    if temp>0 then
     Result:=(CheckExtraField(ptr+$2E+Fbuffer[ptr+$1C]+Fbuffer[ptr+$1D]<<8,temp))
         AND(Result);
    //Compressed size
    size:=Fbuffer[ptr+$14]
         +Fbuffer[ptr+$15]<<8
         +Fbuffer[ptr+$16]<<16
         +Fbuffer[ptr+$17]<<24;
    //Uncompressed size
    usize:=Fbuffer[ptr+$18]
          +Fbuffer[ptr+$19]<<8
          +Fbuffer[ptr+$1A]<<16
          +Fbuffer[ptr+$1B]<<24;
    //Data pointer
    data:=Fbuffer[ptr+$2A]
         +Fbuffer[ptr+$2B]<<8
         +Fbuffer[ptr+$2C]<<16
         +Fbuffer[ptr+$2D]<<24;
    //Check the local file entry
    Result:=(Fbuffer[data]=$50)
         and(Fbuffer[data+1]=$4B)
         and(Fbuffer[data+2]=$03)
         and(Fbuffer[data+3]=$04)
         AND(Result);
    //Check compressed size
    Result:=(Fbuffer[data+$12]
            +Fbuffer[data+$13]<<8
            +Fbuffer[data+$14]<<16
            +Fbuffer[data+$15]<<24=size)
         AND(Result);
    //Check uncompressed size
    Result:=(Fbuffer[data+$16]
            +Fbuffer[data+$17]<<8
            +Fbuffer[data+$18]<<16
            +Fbuffer[data+$19]<<24=usize)
         AND(Result);
    //Check the extra field
    temp:=Fbuffer[data+$1C]+Fbuffer[data+$1D]<<8;
    if temp>0 then
     Result:=(CheckExtraField(data+$1E+Fbuffer[data+$1A]+Fbuffer[data+$1B]<<8,temp))
          AND(Result);
    //Next entry - here we work out where it should be
    temp:=ptr+$2E
             +Fbuffer[ptr+$1C]+Fbuffer[ptr+$1D]<<8
             +Fbuffer[ptr+$1E]+Fbuffer[ptr+$1F]<<8
             +Fbuffer[ptr+$20]+Fbuffer[ptr+$21]<<8;
    //Now we find the next marker
    repeat
     inc(ptr);
    until(ptr=EoCL)or((Fbuffer[ptr  ]=$50)
                   and(Fbuffer[ptr+1]=$4B)
                   and(Fbuffer[ptr+2]=$01)
                   and(Fbuffer[ptr+3]=$02));
    //And check if they match
    Result:=(ptr=temp)AND(Result);
    //Next file
    inc(index);
   end;
   //Check we got all the files
   Result:=(index=numfiles)AND(Result);
  end;
 end;
end;
