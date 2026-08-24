{-------------------------------------------------------------------------------
Parse commands sent through via the console
-------------------------------------------------------------------------------}
procedure TMainForm.ParseCommand(var Command: TStringArray);
type
  searchresult = Record
    Filename: String;
    Directory: Boolean;
    Parent: String;
  end;
var
 error        : Integer=0;
 Lcurrdir     : Integer=0;
 opt          : Integer=0;
 Index        : Integer=0;
 ptr          : Integer=0;
 catopt       : Byte=0;
 Lparent      : String='';
 temp         : String='';
 format       : String='';
 from         : String='';
 dir          : Cardinal=0;
 entry        : Cardinal=0;
 harddrivesize: Cardinal=0;
 dirtype      : Byte=0;
 known        : Boolean=False;
 ok           : Boolean=False;
 newmap       : Boolean=False;
 searchlist   : TSearchRec;
 Files        : TSearchResults;
 OSFiles      : array of searchresult;
 filedetails  : TDirEntry=();
 filelist     : TStringList;
 newImage     : TDiscImage;
 {$IFDEF DIMCONSOLE}
 HexDumpForm  : THexDumpForm;
 {$ENDIF}
const
 DiscFormats = //Accepted format strings
 'DFSS80  DFSS40  DFSD80  DFSD40  WDFSS40 WDFSS40 WDFSD80 WDFSD40 ADFSS   ADFSM   '+
 'ADFSL   ADFSD   ADFSE   ADFSE+  ADFSF   ADFSF+  C1541   C1571   C1581   AMIGADD '+
 'AMIGAHD CFS     DOS+640 DOS+800 DOS360  DOS720  DOS1440 DOS2880 ';
 DiscNumber : array[1..28] of Integer = //Accepted format numbers
 ($001   ,$000   ,$011   ,$010   ,$021   ,$020   ,$031   ,$030   ,$100   ,$110,
  $120   ,$130   ,$140   ,$150   ,$160   ,$170   ,$200   ,$210   ,$220   ,$400,
  $410   ,$500   ,$A00   ,$A01   ,$A02   ,$A03   ,$A04   ,$A05);
 Options : array[0..3] of String = ('none','load','run','exec'); //Boot options
 Inter   : array[0..3] of String = ('auto','seq', 'int','mux' ); //Interleave
 //Configuration settings (registry)
 Configs : array of array[0..2] of String = (
 ('AddImpliedAttributes' ,'B','Add Implied Attributes for DFS/CFS/RFS'),
 ('ADFS_L_Interleave'    ,'I','0=Automatic; 1=Sequential; 2=Interleave; 3=Multiplex'),
 ('Append_Filetype'      ,'B','Append filetype to exported files'),
// ('ConsoleWidth'         ,'I','Console width in characters'),
 ('Create_DSC'           ,'B','Create *.dsc file with hard drives'),
 ('CreateINF'            ,'B','Create a *.inf file when extracting'),
 ('CSVAddress'           ,'B','Include the disc address in CSV file'),
 ('CSVAttributes'        ,'B','Include the file attributes in CSV file'),
 ('CSVCRC32'             ,'B','Include the CRC-32 in CSV file'),
 ('CSVExecAddr'          ,'B','Include the execution address in CSV file'),
 ('CSVFilename'          ,'B','Include the filename in CSV file'),
 ('CSVIncDir'            ,'B','Include directories in CSV file'),
 ('CSVIncFilename'       ,'B','Include image filename in CSV file'),
 ('CSVIncReport'         ,'B','Include image report in CSV file'),
 ('CSVLength'            ,'B','Include the file length in CSV file'),
 ('CSVLoadAddr'          ,'B','include the load address in CSV file'),
 ('CSVMD5'               ,'B','Include the MD5 in CSV file'),
 ('CSVParent'            ,'B','Include the parent in CSV file'),
 ('Debug_Mode'           ,'B','Is debug mode on?'),
 ('DefaultADFSOptions'   ,'I','Which ADFS format for new image dialogue'),
 ('DefaultAFSCreatePWord','B','Whether to create password file for new AFS'),
 ('DefaultAFSImageSize'  ,'I','Default AFS image size'),
 ('DefaultAFSOptions'    ,'I','Which Acorn FS format for new image dialogue'),
 ('DefaultAmigaOptions'  ,'I','Which Amiga format for new image dialogue'),
 ('DefaultC64Options'    ,'I','Which Commodore 64 format for new image dialogue'),
 ('DefaultDFSOptions'    ,'I','Which DFS format for new image dialogue'),
 ('DefaultDFSTOptions'   ,'I','Which DFS track setting for new image dialogue'),
 ('DefaultDOSOptions'    ,'I','Which DOS format for new image dialogue'),
 ('DefaultROMFSBinVers'  ,'I','Default binary version number for new ROM FS'),
 ('DefaultROMFSCopy'     ,'S','Default copyright string to use for new ROM FS'),
 ('DefaultROMFSTitle'    ,'S','Default title to use for new ROM FS'),
 ('DefaultROMFSVersion'  ,'S','Default version to use for new ROM FS'),
 ('DefaultSpecOptions'   ,'I','Which Spectrum format for new image dialogue'),
 ('DefaultSystemOptions' ,'I','Which system for new image dialogue'),
 ('DFS_Allow_Blanks'     ,'B','Allow blank filenames in DFS'),
 ('DFS_Beyond_Edge'      ,'B','Check for files going over the DFS disc edge'),
 ('DFS_Zero_Sectors'     ,'B','Allow DFS images with zero sectors'),
 ('Hide_CDR_DEL'         ,'B','Hide DEL files in Commodore images'),
 ('Open_DOS'             ,'B','Automatically open DOS partitions in ADFS'),
 ('Scan_SubDirs'         ,'B','Automatically scan sub-directories'),
 ('Spark_Is_FS'          ,'B','Treat Spark archives as file system'),
 ('Texture'              ,'I','Which texture background to use'),
 ('UEF_Compress'         ,'B','Compress UEF images when saving'),
 ('View_Options'         ,'I','Displays which menus are visible'),
 ('WindowStyle'          ,'I','Native or RISC OS styling'));
 {$INCLUDE 'DIMHelp.pas'}
 //Validate a filename, building a complete path if required
 function ValidFile(thisfile: String): Boolean;
 begin
  //Build a complete path to the file, if required
  if Image.FileExists(thisfile,dir,entry) then
   temp:=thisfile
  else
   temp:=Image.GetParent(Fcurrdir)
        +Image.DirSep(Image.Disc[Fcurrdir].Partition)
        +thisfile;
  //Does it exist?
  Result:=Image.FileExists(temp,dir,entry);
 end;
 //Report the free space
 procedure ReportFreeSpace;
 var
  free : QWord=0;
  used : QWord=0;
  total: QWord=0;
 begin
  free:=Image.FreeSpace(Image.Disc[Fcurrdir].Partition);
  total:=Image.DiscSize(Image.Disc[Fcurrdir].Partition);
  used:=total-free;
  Write(cmdBold+IntToStr(free)+cmdNormal+' bytes free. ');
  Write(cmdBold+IntToStr(used)+cmdNormal+' bytes used. ');
  WriteLn(cmdBold+IntToStr(total)+cmdNormal+' bytes total.');
 end;
 //Check for modified image
 function Confirm: Boolean;
 var
  Lconfirm: String='';
 begin
  Result:=True;
  if HasChanged then
  begin
   Result:=False;
   WriteLn('Image has been modified.');
   Write('Are you sure you want to continue? (yes/no): ');
   ConsoleApp.ReadInput(Lconfirm);
   if Length(Lconfirm)>0 then if LowerCase(Lconfirm[1])='y' then Result:=True;
  end;
 end;
 //Get the image size
 function GetDriveSize(GivenSize: String): Cardinal;
 begin
  //Default in Kilobytes
  Result:=StrToIntDef(GivenSize,0);
  //Has it been specified in Megabytes?
  if UpperCase(RightStr(GivenSize,1))='M' then
   Result:=StrToIntDef(LeftStr(GivenSize,Length(GivenSize)-1),0)*1024;
 end;
 //Wildcard filename search
 function GetListOfFiles(Lfilesearch: String;LImage: TDiscImage;LFiles: TSearchResults=nil): TSearchResults;
 begin
  ResetDirEntry(filedetails);
  //Select the file
  filedetails.Filename:=Lfilesearch;
  filedetails.Parent:=Image.GetParent(Fcurrdir);
  //First we look for the files - this will allow wildcarding
  Result:=LImage.FileSearch(filedetails,LFiles);
 end;
 function GetListOfFiles(Lfilesearch: String; LFiles: TSearchResults=nil): TSearchResults;
 begin
  Result:=GetListOfFiles(Lfilesearch,Image,LFiles);
 end;
 //Build the filename
 function BuildFilename(Lfile: TDirEntry): String;
 begin
  Result:='';
  if Lfile.Parent<>'' then
   Result:=Lfile.Parent
        +Image.DirSep(Image.Disc[Fcurrdir].Partition);
  Result:=Result+Lfile.Filename;
 end;
//Main procedure definition starts here
begin
 ResetDirEntry(filedetails);
 if Length(Command)=0 then exit;
 //Convert the command to lower case
 Command[0]:=LowerCase(Command[0]);
 //'ls' command is the same as 'cat os'
 if Command[0]='ls' then
 begin
  SetLength(Command,2);
  Command[0]:='cat';
  Command[1]:='os';
 end;
 //Error number
 error:=0;
 //Parse the command
 case Command[0] of
  //Change the access rights of a file +++++++++++++++++++++++++++++++++++++++++
  'access':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then
    begin
     //No attributes given? Then pass none
     if Length(Command)<3 then
     begin
      SetLength(Command,3);
      Command[2]:='';
     end;
     Files:=nil;
     Files:=GetListOfFiles(Command[1]);
     if Length(Files)>0 then
      for Index:=0 to Length(Files)-1 do
      begin
       temp:=BuildFilename(Files[Index]);
       Write('Changing attributes for '+temp+' ');
       if Image.UpdateAttributes(temp,Command[2])then
       begin
        error:=-1;
        HasChanged:=True;
       end else error:=3;
      end
     else WriteLn(cmdRed+'No files found.'+cmdNormal)
    end
    else error:=2
   else error:=1;
  //Add files ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'add','find','from':
   begin
    newImage:=TDiscImage.Create;
    ok   :=True;
    from :='';
    //Parse the from command first
    if Command[0]='from' then
     if Length(Command)>1 then //Should be a file specified
     begin
      from:=Command[1]; //Get the filename
      //Now move everything down 2 places so that 'add' and 'find' will work
      if Length(Command)>2 then
      begin
       for Index:=2 to Length(Command)-1 do
        Command[Index-2]:=Command[Index];
       SetLength(Command,Length(Command)-2);
      end
      else
      begin
       error:=2;
       ok:=False;
      end;
     end
     else
     begin
      error:=2;
      ok:=False;
     end;
    //Make sure the donor image exists and is valid
    if(ok)and(from<>'')then
     if FileExists(from) then
     begin
      WriteLn('Reading image.');
      if newImage.LoadFromFile(from) then
      begin
       WriteLn(cmdBold+newImage.FormatString+cmdNormal+' image read OK.');
       ok:=True;
      end
      else WriteLn(cmdRed+'Image not read.'+cmdNormal);
     end
     else
     begin
      ok:=False;
      WriteLn(cmdRed+'Image file "'+from+'" not found.'+cmdNormal);
     end;
    //Now parse the 'add' or 'find' commands
    if ok then
     //Ensure that the only command after 'from' is either 'add' or 'find'
     if(Command[0]='add')or(Command[0]='find')then
      //Make sure we have a valid image for 'add', or the command is 'find'
      if((Image.FormatNumber<>diInvalidImg)and(Command[0]='add'))
      or (Command[0]='find')then
       if Length(Command)>1 then //Are there any files given?
       begin
        SetLength(OSFiles,0);
        for Index:=1 to Length(Command)-1 do //Just add a file
        begin
         ok:=True; //Add to list
         if Command[Index][1]='|' then //Remove from list
         begin
          ok:=False;
          Command[Index]:=Copy(Command[Index],2);
         end;
         //Collate a list of files
         if from='' then //From the host OS
         begin
          //Can contain a wild card
          FindFirst(Command[Index],faDirectory,searchlist);
          //First thing we do is collate a list of files/directories
          repeat
           //These are previous and top directories, and nothing found
           if (searchlist.Name<>'.')
           and(searchlist.Name<>'..')
           and(searchlist.Name<>'')then
           begin
            //New entry
            if ok then
            begin
             ptr:=Length(OSFiles);
             SetLength(OSFiles,ptr+1);
             //Make a note of the filename
             OSFiles[ptr].Filename:=ExtractFilePath(Command[Index])+searchlist.Name;
             //And whether it is a directory or not
             if(searchlist.Attr AND faDirectory)=faDirectory then
              OSFiles[ptr].Directory:=True
             else
              OSFiles[ptr].Directory:=False;
            end
            else //Remove an entry
            begin
             temp:=ExtractFilePath(Command[Index])+searchlist.Name;
             for ptr:=0 to Length(OSFiles)-1 do
              if (OSFiles[ptr].Filename=temp)
              and(OSFiles[ptr].Directory=((searchlist.Attr AND faDirectory)=faDirectory))then
               OSFiles[ptr].Filename:='';
            end;
           end;
           //Next entry
          until FindNext(searchlist)<>0;
          //All done, then close the search
          FindClose(searchlist);
         end
         else //From the supplied image
         begin
          Files:=GetListOfFiles(Command[Index],newImage);
          for filedetails in Files do
           if ok then //Add the file
           begin
            ptr:=Length(OSFiles);
            SetLength(OSFiles,ptr+1);
            //We'll need to put in the entire path so we can find it later
            OSFiles[ptr].Filename :=filedetails.Filename;
            OSFiles[ptr].Parent   :=filedetails.Parent;
            OSFiles[ptr].Directory:=filedetails.DirRef<>-1;
           end
           else //Remove the file
           begin
            for ptr:=0 to Length(OSFiles)-1 do
             if (OSFiles[ptr].Filename =filedetails.Filename)
             and(OSFiles[ptr].Parent   =filedetails.Parent)
             and(OSFiles[ptr].Directory=(filedetails.DirRef<>-1))then
              OSFiles[ptr].Filename:='';
           end;
         end;
        end;
        //Now remove blank entries
        ptr:=0;
        while ptr<Length(OSFiles) do
        begin
         if OSFiles[ptr].Filename='' then
         begin
          if ptr<Length(OSFiles)-2 then
           for Index:=ptr to Length(OSFiles)-2 do OSFiles[Index]:=OSFiles[Index+1];
          SetLength(OSFiles,Length(OSFiles)-1);//Use Delete method
          dec(ptr);
         end;
         inc(ptr);
        end;
        //Report the number of entries found
        WriteLn(IntToStr(Length(OSFiles))+' entries found.');
        //Now we add/list them
        ok:=True;
        for ptr:=0 to Length(OSFiles)-1 do
        begin
         //Add directory
         if OSFiles[ptr].Directory then
         begin
          if Command[0]='add' then
          begin
           Write('Adding directory: '''+OSFiles[ptr].Filename+'''. ');
           if from='' then
            ok:=(ok)AND(AddDirectoryToImage(OSFiles[ptr].Filename))
           else
            ok:=(ok)AND(AddDirectoryToImage(OSFiles[ptr].Filename,newImage,OSFiles[ptr].Parent));
          end //Or list the directory
          else WriteLn(cmdBlue+'Directory'+cmdNormal+': '''
                      +OSFiles[ptr].Filename+'''.');
         end
         else //Add a single file
         begin
          if Command[0]='add' then
          begin
           Write('Adding file: '''+OSFiles[ptr].Filename+'''. ');
           if from='' then //Add from host OS
            ok:=(ok)AND(AddFileToImage(OSFiles[ptr].Filename)>=0)
           else //Add from supplied image
           begin
            if newImage.FileExists(OSFiles[ptr].Parent+newImage.DirSep+OSFiles[ptr].Filename,dir,entry) then
             ok:=(ok)AND(ImportFile(newImage,dir,entry)=0);
            //else ok:=False;
           end;
          end //Or list the file
          else WriteLn(cmdBlue+'File'+cmdNormal+': '''
                      +OSFiles[ptr].Filename+'''.');
         end;
         //Write was a success
         if(Command[0]='add')and(ok)then
         begin
          HasChanged:=True;
          error:=-1;
         end;
         //Write was a failure
         if(Command[0]='add')and(not ok)then error:=3;
        end;
       end
       else error:=2//Nothing has been passed
      else error:=1//No image
     else WriteLn(cmdRed+'Unknown command after "from"'+cmdNormal);
    newImage.Free;
   end;
  //Display a catalogue of the current directory +++++++++++++++++++++++++++++++
  'cat':
   begin
    //Determine the parameter
    catopt:=0;                                       //Current directory
    if Length(Command)>1 then
    begin
     if(LowerCase(Command[1])='all') then catopt:=1; //Entire image
     if(LowerCase(Command[1])='dir') then catopt:=2; //List the directories (inc roots)
     if(LowerCase(Command[1])='root')then catopt:=3; //List the roots only
     if(LowerCase(Command[1])='os')  then catopt:=4; //Current host OS directory
    end;
    //Act on the option
    case catopt of
     0,1,2://Current dir, entire image, directories and roots
      if Image.FormatNumber<>diInvalidImg then
      begin
       //Default option - just catalogue the current directory
       opt:=Fcurrdir;
       ptr:=Fcurrdir;
       //Entire image, directories and roots
       if catopt>0 then
       begin
        opt:=0;
        ptr:=Length(Image.Disc)-1;
       end;
       for Lcurrdir:=opt to ptr do
       begin
        //List the catalogue
        if catopt<2 then
        begin
         WriteLn(cmdBlue+StringOfChar('-',80)+cmdNormal);
         WriteLn(cmdBold+'Catalogue listing for directory '
                 +Image.GetParent(Lcurrdir));
         Write(PadRight(Image.Disc[Lcurrdir].Title,40));
         WriteLn('Option: '+IntToStr(Image.BootOpt[Image.Disc[Lcurrdir].Partition])
                +' ('
                +UpperCase(Options[Image.BootOpt[Image.Disc[Lcurrdir].Partition]])
                +')');
         Write(PadRight('Number of entries: '
                       +IntToStr(Length(Image.Disc[Lcurrdir].Entries)),40));
         if (Image.Disc[Lcurrdir].Broken)
         and(Image.MajorFormatNumber=diAcornADFS)then
          WriteLn(cmdRed
                 +'Broken (0x'
                 +IntToHex(Image.Disc[Lcurrdir].ErrorCode,2)+')')
         else WriteLn();
         WriteLn(cmdNormal);
         if Length(Image.Disc[Lcurrdir].Entries)>0 then
          for Index:=0 to Length(Image.Disc[Lcurrdir].Entries)-1 do
          begin
           //Filename
           Write(PadRight(Image.Disc[Lcurrdir].Entries[Index].Filename,10));
           //Attributes
           Write(' ('+Image.Disc[Lcurrdir].Entries[Index].Attributes+')');
           //Files
           if Image.Disc[Lcurrdir].Entries[Index].DirRef=-1 then
           begin
            //Filetype - ADFS, Spark only
            if  (Image.Disc[Lcurrdir].Entries[Index].FileType<>'')
            and((Image.MajorFormatNumber=diAcornADFS)
            or  (Image.MajorFormatNumber=diSpark))then
             Write(' '+Image.Disc[Lcurrdir].Entries[Index].FileType);
            //Timestamp - ADFS, Spark, FileStore, Amiga and DOS only
            if  (Image.Disc[Lcurrdir].Entries[Index].TimeStamp>0)
            and((Image.MajorFormatNumber=diAcornADFS)
            or  (Image.MajorFormatNumber=diSpark)
            or  (Image.MajorFormatNumber=diAcornFS)
            or  (Image.MajorFormatNumber=diAmiga)
            or  (Image.MajorFormatNumber=diDOSPlus))then
             Write(' '+FormatDateTime(TimeDateFormat,
                                   Image.Disc[Lcurrdir].Entries[Index].TimeStamp));
            if(Image.Disc[Lcurrdir].Entries[Index].TimeStamp=0)
            or(Image.MajorFormatNumber=diAcornFS)then
            begin
             //Load address
             Write(' '+IntToHex(Image.Disc[Lcurrdir].Entries[Index].LoadAddr,8));
             //Execution address
             Write(' '+IntToHex(Image.Disc[Lcurrdir].Entries[Index].ExecAddr,8));
            end;
            //Length
            Write(' '+ConvertToKMG(Image.Disc[Lcurrdir].Entries[Index].Length)+
                  ' ('+IntToHex(Image.Disc[Lcurrdir].Entries[Index].Length,8)+')');
           end
           else
            if (Image.MajorFormatNumber=diAcornADFS)
            and(Image.Disc[Image.Disc[Lcurrdir].Entries[Index].DirRef].Broken)then
             Write(cmdRed+' Broken'+cmdNormal);
           //New line
           WriteLn();
          end;
        end;
        //List only the directories or roots
        if catopt>1 then
        begin
         //Roots have no parent, so will be '-1'
         Write(cmdBold);
         if Image.Disc[Lcurrdir].Parent=-1 then Write('Root: ')
                                           else Write('Directory: ');
         WriteLn(cmdNormal+Image.GetParent(Lcurrdir));
        end;
       end;
      end;
    3: //Just the roots
     if Image.FormatNumber<>diInvalidImg then
      for Index:=0 to Length(Image.Partitions)-1 do
       WriteLn(cmdBold+'Root: '+cmdNormal+Image.Partitions[Index].RootName);
    4: //Display a catalogue of the host directory
     begin
      WriteLn(cmdBold+cmdCyan+GetCurrentDir+cmdNormal);
      WriteLn(cmdBlue+StringOfChar('-',80)+cmdNormal);
      // if we have found a file...
      If FindFirst('*',faAnyFile and faDirectory,searchlist)=0 then
      begin
       repeat
        // we do stuff with the file entry we found
        with searchlist do
        begin
         //Enhance the text for directories and hidden entries
         If(Attr and faDirectory)= faDirectory then Write(cmdBold);
         If((Attr and faHidden)  = faHidden)
         or(Name[1]='.')                       then Write(cmdGreen);
         //Print the details, padding with spaces and spread across lines if longer than 38
         temp:=Name;
         while Length(temp)>38 do
         begin
          WriteLn(LeftStr(temp,38));
          temp:=RightStr(temp,Length(temp)-38);
         end;
         Write(LeftStr(temp+StringOfChar(' ',38),38),' ');
         Write(Size:13,' ');
         Write(FormatDateTime('hh:mm:ss dd"/"mm"/"yyyy',TimeStamp));
         //Print the attributes, including directory or hidden
         Write(' (');
         If(Attr and faDirectory)= faDirectory then Write('D') else Write('-');
         If(Attr and faReadOnly) = faReadOnly  then Write('R') else Write('-');
         If((Attr and faHidden)  = faHidden)
         or(Name[1]='.')                       then Write('H') else Write('-');
         If(Attr and faSysFile)  = faSysFile   then Write('S') else Write('-');
         If(Attr and faArchive)  = faArchive   then Write('A') else Write('-');
         //End the line and return to normal text
         WriteLn(')'+cmdNormal);
        end;
       until FindNext(searchlist)<>0;
      end;
      // we are done with file list
      FindClose(searchlist);
     end;
    end;
    //No image loaded
    if(catopt<4)and(Image.FormatNumber=diInvalidImg)then error:=1;
   end;
  //Change the host directory ++++++++++++++++++++++++++++++++++++++++++++++++++
  'chdir':
   if Length(Command)>1 then
    if SetCurrentDir(Command[1]) then error:=-1
    else error:=3
   else WriteLn(cmdBold+cmdCyan+GetCurrentDir+cmdNormal);
  //Defrag +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'compact','defrag':
   if Image.FormatNumber<>diInvalidImg then //Image inserted?
   begin
    //Get the drive/partition specification, default to 0 if none specified
    if Length(Command)>1 then ptr:=StrToIntDef(Command[1],-1)
    else ptr:=Image.Disc[Fcurrdir].Partition;
    //Count number of sides/partitions
    dir:=0;
    for Index:=0 to Length(Image.Disc)-1 do
     if Image.Disc[Index].Parent=-1 then inc(dir);
    //Is it valid?
    if(ptr>=0)and(ptr<dir)then
    begin
     if Command[0]='compact' then temp:='Compacting' else temp:='Defragging';
     WriteLn(cmdBold+cmdBlue+temp+' drive/partition '+IntToStr(ptr)+cmdNormal);
     Defrag(ptr);
    end
    else WriteLn(cmdRed+'Invalid drive or partition specification'+cmdNormal);
   end
   else error:=1;
  //Set a configuration option, display available options or current settings ++
  'config','status':
   if(Command[0]='config')and(Length(Command)>2)then
   begin
    ok:=False;
    for Index:=0 to Length(Configs)-1 do
     if UpperCase(Command[1])=UpperCase(Configs[Index,0]) then
     begin
      ok:=True;
      case Configs[Index,1] of
       'B' : if LowerCase(Command[2])='true' then
              DIMReg.SetRegValB(Configs[Index,0],True)
             else
              DIMReg.SetRegValB(Configs[Index,0],False);
       'I' :
        begin
         dir:=0;
         if LowerCase(LeftStr(Command[2],2))='0x' then
          dir:=StrToIntDef('$'+Copy(Command[2],3),0);
         if(Command[2][1]='$')or(Command[2][1]='&')then
          dir:=StrToIntDef('$'+Copy(Command[2],2),0);
         if dir=0 then dir:=StrToIntDef(Command[2],0);
         DIMReg.SetRegValI(Configs[Index,0],dir);
        end;
       'S' : DIMReg.SetRegValS(Configs[Index,0],Command[2]);
      end;
     end;
    if ok then
    begin
     WriteLn('Configuration option set.');
     //Update the image with the latest settings
     Image.InterleaveMethod    :=DIMReg.GetRegValI('ADFS_L_Interleave',0);
     Image.SparkAsFS           :=DIMReg.GetRegValB('Spark_Is_FS',True);
     Image.AddImpliedAttributes:=DIMReg.GetRegValB('AddImpliedAttributes',True);
     Image.AllowDFSZeroSectors :=DIMReg.GetRegValB('DFS_Zero_Sectors',False);
     Image.DFSBeyondEdge       :=DIMReg.GetRegValB('DFS_Beyond_Edge',False);
     Image.DFSAllowBlanks      :=DIMReg.GetRegValB('DFS_Allow_Blanks',False);
     Image.ScanSubDirs         :=DIMReg.GetRegValB('Scan_SubDirs',True);
     Image.OpenDOSPartitions   :=DIMReg.GetRegValB('Open_DOS',True);
     Image.CreateDSC           :=DIMReg.GetRegValB('Create_DSC',False);
     Image.AppendFiletype      :=DIMReg.GetRegValB('Append_Filetype',True);
    end
    else WriteLn(cmdRed+'Invalid configuration option.'+cmdNormal);
   end else
   //Not enough parameters, so list the config options or current settings
   begin
    Write(cmdBold+cmdBlue);
    if Command[0]='config' then Write('Valid configuration options')
    else Write('Current configuration settings');
    WriteLn(cmdNormal);
    WriteLn('Not all configurations are used by the console.');
    //Get the longest string
    ptr:=1;
    for Index:=0 to Length(Configs)-1 do
     if Length(Configs[Index,0])>ptr then ptr:=Length(Configs[Index,0]);
    //Display the current configs, or current settings
    for Index:=0 to Length(Configs)-1 do
    begin
     Write(cmdRed+cmdBold+PadRight(Configs[Index,0],ptr)+cmdNormal+': ');
     if Command[0]='config' then //Available settings
     begin
      Write(cmdRed);
      case Configs[Index,1] of
       'B': Write('True|False');
       'I': Write('<Integer>');
       'S': Write('<String>');
      end;
      WriteLn(cmdNormal);
      if Configs[Index,2]<>'' then
       WriteLn(StringOfChar(' ',ptr+2)+Configs[Index,2]);
     end
     else //Current settings
     begin
      if DIMReg.DoesKeyExist(Configs[Index,0]) then
       case Configs[Index,1] of
        'B' : WriteLn(DIMReg.GetRegValB(Configs[Index,0]));
        'I' : WriteLn('0x'+IntToHex(DIMReg.GetRegValI(Configs[Index,0]),4));
        'S' : WriteLn(DIMReg.GetRegValS(Configs[Index,0]));
       end
      else WriteLn(cmdRed+'Not set'+cmdNormal);
     end;
    end;
   end;
  //Creates a directory ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'create':
   if Image.FormatNumber<>diInvalidImg then
   begin
    //Default directory name, if none given
    temp:='NewDir';
    //See if there was a directory name given
    if Length(Command)>1 then temp:=Command[1];
    Write('Create new directory '''+temp+''' ');
    //Get the parent and set the attributes
    Lparent:=Image.GetParent(Fcurrdir);
    format:='DLR';
    //Create the directory
    if Image.CreateDirectory(temp,Lparent,format)>=0 then
    begin
     error:=-1;
     HasChanged:=True;
    end
    else error:=3
   end
   else error:=1;//No image
  //Delete a specified file or directory +++++++++++++++++++++++++++++++++++++++
  'delete':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then //Are there any files given?
     for Index:=1 to Length(Command)-1 do
     begin
      //Try in the local directory
      temp:=Image.GetParent(Fcurrdir)
           +Image.DirSep(Image.Disc[Fcurrdir].Partition)
           +Command[Index];
      ok:=Image.FileExists(temp,dir,entry);
      //Nothing, so try fully qualified path
      if not ok then
      begin
       temp:=Command[Index];
       ok:=Image.FileExists(temp,dir,entry);
      end;
      //Have we found something?
      if ok then
      begin
       //Perform the deletion
       if (Image.MajorFormatNumber<>diAcornUEF)
       and(Image.MajorFormatNumber<>diAcornRFS)then
        ok:=Image.DeleteFile(temp)
       else
        ok:=Image.DeleteFile(entry);
       //Report findings
       if ok then
       begin
        WriteLn(cmdGreen+''''+Command[Index]+''' deleted.'+cmdNormal);
        HasChanged:=True;
       end
       else WriteLn(cmdRed+'Could not delete '''+Command[Index]+'''.'+cmdNormal);
      end
      else WriteLn(cmdRed+''''+Command[Index]+''' not found.'+cmdNormal);
     end
    else error:=2//Nothing has been passed
   else error:=1;//No image
  //Change directory +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'dir': //Currently does not deal with multi-partitions where the root names are the same
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then
    begin
     temp:=Command[1];
     //Parent ?
     if temp[1]='^' then
      if Image.Disc[Fcurrdir].Parent>=0 then
       temp:=Image.GetParent(Image.Disc[Fcurrdir].Parent)+Copy(temp,2)
      else
       temp:=Image.GetParent(Image.Partitions[Image.Disc[Fcurrdir].Partition].RootRef)+Copy(temp,2);
     //Are there more parent specifiers?
     Lparent:=Image.DirSep(Image.Disc[Fcurrdir].Partition)+'^';
     while Pos(Lparent,temp)>1 do
     begin
      ptr:=Pos(Lparent,temp)-1;
      while(ptr>1)
        and(temp[ptr]<>Image.DirSep(Image.Disc[Fcurrdir].Partition))do
       dec(ptr);
      if ptr>1 then
       temp:=LeftStr(temp,ptr-1)+Copy(temp,Pos(Lparent,temp)+Length(Lparent));
      if ptr=1 then
       temp:=LeftStr(temp,ptr)+Copy(temp,Pos(Lparent,temp)+Length(Lparent));
     end;
     //Found, so make sure that dir and entry are within bounds
     if ValidFile(temp) then
     begin
      //Must be a root - find the correct partition
      if dir>=Length(Image.Disc) then
       for Index:=0 to Length(Image.Partitions)-1 do
        if Image.Partitions[Image.Disc[Index].Partition].RootName=temp then
        begin
         Fcurrdir:=Image.Partitions[Image.Disc[Index].Partition].RootRef;
         ok:=True;
        end;
      //Sub directory
      if dir<Length(Image.Disc) then
       if entry<Length(Image.Disc[dir].Entries) then
        if Image.Disc[dir].Entries[entry].DirRef>=0 then
        begin
         Fcurrdir:=Image.Disc[dir].Entries[entry].DirRef;
         ok:=True;
        end
        else WriteLn(cmdRed+''''+temp+''' is a file.'+cmdNormal);
      //Must be a root - so select the root on the current partition
      if entry>Length(Image.Disc) then
      begin
       if dir<Length(Image.Disc) then
        Fcurrdir:=dir
       else
        Fcurrdir:=Image.Partitions[Image.Disc[Fcurrdir].Partition].RootRef;
       ok:=True;
      end;
     end;
     //Are we on DFS and we have a drive specifier?
     if Image.MajorFormatNumber=diAcornDFS then
     begin
      opt:=0;//Default drive 0
      if Length(temp)>1 then
       if temp[1]=':' then opt:=StrToIntDef(temp[2],0);
      if(Image.DoubleSided)and(opt=2)then
       opt:=Length(Image.Disc)-1; //Only select if double sided
      //We'll ignore anything after the drive specifier
      Fcurrdir:=opt;
      ok:=True;
     end;
     //Report back to the user
     if ok then
      WriteLn(cmdGreen
             +'Directory '''+Image.GetParent(Fcurrdir)+''' selected.'
             +cmdNormal)
     else WriteLn(cmdRed+''''+temp+''' does not exist.'+cmdNormal);
    end
    else error:=2//Nothing has been passed
   else error:=1;//No image
  //Changes the directory title ++++++++++++++++++++++++++++++++++++++++++++++++
  'dirtitle':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then
    begin
     temp:=Image.GetParent(Fcurrdir);
     Write('Retitle directory '+temp+' ');
     if Image.RetitleDirectory(temp,Command[1]) then
     begin
      error:=-1;
      HasChanged:=True;
     end
     else error:=3
    end
    else error:=2//Nothing has been passed
   else error:=1;//No image
  //Eject command ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'eject':
   if Image.FormatNumber<>diInvalidImg then
   begin
    if Confirm then
    begin
     Image.Free;
     Image:=TDiscImage.Create;
     Fcurrdir:=0;
     HasChanged:=False;
     WriteLn(cmdGreen+'Image ejected.'+cmdNormal);
    end;
   end
   else error:=1;
  //Change exec or load address ++++++++++++++++++++++++++++++++++++++++++++++++
  'exec','load','type':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>2 then
     if IntToHex(StrToIntDef('$'+Command[2],0),8)
       =UpperCase(RightStr('00000000'+Command[2],8)) then
     begin
     Files:=nil;
     Files:=GetListOfFiles(Command[1]);
     if Length(Files)>0 then
      for Index:=0 to Length(Files)-1 do
      begin
       temp:=BuildFilename(Files[Index]);
       ok:=False;
       //Print the text - Load or Exec
       if(Command[0]='load')or(Command[0]='exec')then
       begin
        if format='exec' then format:='execution'; //Expand exec
        Write('Change '+format+' address for '+temp
             +' to 0x'+IntToHex(StrToIntDef('$'+Command[2],0),8)+' ');
       end;
       //Print the text - Filetype
       if Command[0]='type' then
       begin
        Command[2]:=RightStr('000'+Command[2],3); //Ensure filetype is 12 bits
        Write('Change filetype for '+temp+' to 0x'
             +IntToHex(StrToIntDef('$'+Command[2],0),3)+' ');
       end;
       //Attempt to update details
       if LowerCase(Command[0])='exec' then //Execution address
        ok:=Image.UpdateExecAddr(temp,StrToIntDef('$'+Command[2],0));
       if LowerCase(Command[0])='load' then //Load address
        ok:=Image.UpdateLoadAddr(temp,StrToIntDef('$'+Command[2],0));
       if LowerCase(Command[0])='type' then //Filetype
        ok:=Image.ChangeFileType(temp,Command[2]); //We can take a filetype name here
       //Report back
       if ok then
       begin
        HasChanged:=True;
        error:=-1;
       end
       else error:=3;
      end
      else WriteLn(cmdRed+'No files found'+cmdNormal);
     end
     else WriteLn(cmdRed+'Invalid hex number.'+cmdNormal)
    else error:=2//Nothing has been passed
   else error:=1;//No image
  //Exit the console application +++++++++++++++++++++++++++++++++++++++++++++++
  'exit':
   begin
    {$IFNDEF DIMCONSOLE}if Length(Command)=1 then{$ENDIF} //Complete exit
     if not Confirm then Command[0]:='';
{$IFNDEF DIMCONSOLE}
    if Length(Command)>1 then //Just to the GUI
     if Command[1]='togui' then WriteLn('Entering GUI.')
     else
     begin //Unless we have an unrecognised parameter
      WriteLn(cmdRed+'Unknown parameter'+cmdNormal);
      Command[0]:='';
     end;
{$ENDIF}
   end;
  //Extract and search commands ++++++++++++++++++++++++++++++++++++++++++++++++
  'extract','search':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then
    begin
     Files:=nil;
     for Index:=1 to Length(Command)-1 do
      Files:=GetListOfFiles(Command[Index],Files);
     if Command[0]='search' then
      WriteLn(IntToStr(Length(Files))+' file(s) found.');
      //Now go through all the results, if any, and extract each of them
     if Length(Files)>0 then //If there are any, of course
      for opt:=0 to Length(Files)-1 do
      begin
       temp:=BuildFilename(Files[opt]);
       //And extract or print it
       if Image.FileExists(temp,dir,entry) then
        if Command[0]='extract' then //Extract
        begin
         Write('Extracting '+temp+' ');
         //Ensure we are within range
         if dir<Length(Image.Disc)then
          if entry<Length(Image.Disc[dir].Entries)then
           DownLoadFile(dir,entry,'');
         //If we are outside, then it must be the root
         if dir>Length(Image.Disc)then
         begin
          Write(cmdRed+'Cannot extract the root in this way. ');
          WriteLn('Try selecting the root and entering ''extract *''.'+cmdNormal);
         end;
        end
        else WriteLn(temp); //Print
      end
     else
      if Command[0]='extract' then WriteLn(cmdRed+'No files found.'+cmdNormal);
    end
    else error:=2//Nothing has been passed
   else error:=1;//No image
  //Multi CSV output of files ++++++++++++++++++++++++++++++++++++++++++++++++++
  'filetocsv':
   if Length(Command)>1 then //Are there any files given?
   begin
    filelist:=TStringList.Create;
    for Index:=1 to Length(Command)-1 do//Just add a file
    begin
     //Can contain a wild card
     FindFirst(Command[Index],faDirectory,searchlist);
     repeat
      //These are previous and top directories
      if(searchlist.Name<>'.')and(searchlist.Name<>'..')then
       //We can't open directories
       if(searchlist.Attr AND faDirectory)<>faDirectory then
        //Make sure the file exists
        if FileExists(searchlist.Name) then
         //Add it to our list
         filelist.Add(ExtractFilePath(Command[Index])+searchlist.Name);
     until FindNext(searchlist)<>0;
     FindClose(searchlist);
    end;
    WriteLn('Processing images.');
    if filelist.Count>0 then SaveAsCSV(filelist) //Send to the procedure
    else WriteLn(cmdRed+'No images found.'+cmdNormal);
    filelist.Free;
   end
   else error:=2;//Nothing has been passed
  //Translate filetype +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'filetype':
   //List all filetypes
   if Length(Command)>1 then
    //Name passed?
    if IntToHex(StrToIntDef('$'+Command[1],0),3)<>UpperCase(Command[1]) then
    begin
     ptr:=Image.GetFileType(Command[1]);
     if ptr<>-1 then WriteLn('0x'+IntToHex(ptr,3))
     else WriteLn('Unknown filetype');
    end //No, hex number passed
    else WriteLn(Image.GetFileType(StrToInt('$'+Command[1])))
   else error:=2;
  //Fix ADFS dirs ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'fixdirs':
   if Image.FormatNumber<>diInvalidImg then
    if Image.MajorFormatNumber=diAcornADFS then
     if Image.FixDirectories then
     begin
      error:=-1;
      HasChanged:=True;
     end
     else error:=3
    else WriteLn(cmdRed+'Not possible in this format.'+cmdNormal)
   else error:=1;
  //Get the free space +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'free':
   if Image.FormatNumber<>diInvalidImg then ReportFreeSpace
   else error:=1;//No image
  //Help command +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'help':
   if Length(Command)=1 then
   begin
    WriteLn(cmdBlue+cmdBold+'Console Help'+cmdNormal);
    for Index:=0 to Length(Help)-1 do
    begin
     temp:=Help[Index];
     if Length(temp)>1 then
      if temp[1]<>' ' then temp:=cmdRed+cmdBold+temp
      else temp:=Copy(temp,2);
     WriteLn(WrapText(temp,ConsoleWidth)+cmdNormal);
    end;
   end
   else
    case Command[1] of
    'brokencodes': //ADFS Broken Directory Error Codes -------------------------
     begin
      WriteLn(cmdBlue+cmdBold+'ADFS Broken Directory Codes'+cmdNormal);
      for Index:=0 to Length(BrokenCodes)-1 do
      begin
       temp:=cmdRed+cmdBold+ReplaceStr(BrokenCodes[Index],':',cmdNormal);
       WriteLn(WrapText(temp,ConsoleWidth));
      end;
      WriteLn(WrapText(cmdGreen
             +'Codes can be any combination of the above, summed together.'
             +cmdNormal,ConsoleWidth));
     end;
    else WriteLn(cmdRed+'No help found for '+Command[1]+'.'+cmdNormal);
    end;
  //Open command +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'insert':
   if Confirm then
    if Length(Command)>1 then
     if FileExists(Command[1]) then
     begin
      WriteLn('Inserting image.');
      if Image.LoadFromFile(Command[1]) then
      begin
       WriteLn(cmdBold+Image.FormatString+cmdNormal+' image inserted OK.');
       Fcurrdir:=0;
       ReportFreeSpace;
       WriteLn(cmdBold+'Partitions: '+cmdNormal+IntToStr(Length(Image.Partitions)));
       HasChanged:=False;
      end
      else WriteLn(cmdRed+'Image not read.'+cmdNormal);
     end
     else WriteLn(cmdRed+'File not found.'+cmdNormal)
    else error:=2;
  //Change Interleave Method +++++++++++++++++++++++++++++++++++++++++++++++++++
  'interleave':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then
     if(Image.FormatNumber=diAcornADFS<<4+2)
     or(Image.FormatNumber=diAcornADFS<<4+$E)
     or(Image.MajorFormatNumber=diAcornFS)then
     begin
      //The option may have been supplied as a word or a number
      opt:=0;
      //First check for a word
      while(LowerCase(Command[1])<>Inter[opt])and(opt<High(Inter))do inc(opt);
      //Not found, convert to a number. This will be -1 if an unknown word is given
      if LowerCase(Command[1])<>Inter[opt] then opt:=StrToIntDef(Command[1],-1);
      //Can't be higher than what we know
      if(opt>=0)and(opt<=High(Inter))then
       if Image.ChangeInterleaveMethod(opt) then
       begin
        HasChanged:=True;
        WriteLn(cmdGreen+'Interleave changed to '
               +UpperCase(Inter[opt])+'.'+cmdNormal);
       end
       else WriteLn(cmdRed+'Failed to change interleave.'+cmdNormal)
      else WriteLn(cmdRed+'Invalid Interleave option.'+cmdNormal);
     end
     else WriteLn(cmdRed+'Not possible in this format.'+cmdNormal)
    else error:=2
   else error:=1;
  //Join partitions ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'join':WriteLn(cmdRed+'This command has not been implemented yet.'+cmdNormal);
  //Show the contents of a file ++++++++++++++++++++++++++++++++++++++++++++++++
  'list':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then
     if ValidFile(Command[1])then
     begin
      {$IFNDEF DIMCONSOLE}
      //We'll need to create a container
      SetLength(HexDump,1);
      HexDump[0]:=THexDumpForm.Create(nil);
      //Extract the file into this container
      if Image.ExtractFile(temp,HexDump[0].buffer,entry) then
      begin
       //Only display if it is text or BASIC
       if(HexDump[0].IsBasicFile)or(HexDump[0].IsTextFile)then
        HexDump[0].DecodeBasicFile
       else
        HexDump[0].btnSaveTextClick(nil);
      end
      else
       if not Fguiopen then
        WriteLn(cmdRed+'Failed to extract file.'+cmdNormal);
      //Free up the container
      HexDump[0].Free;
      SetLength(HexDump,0);
      {$ELSE}
      //Extract the file into this container
      HexDumpForm:=THexDumpForm.Create;
      if Image.ExtractFile(temp,HexDumpForm.buffer,entry) then
      begin
       //Only display if it is text or BASIC
       if(HexDumpForm.IsBasicFile)or(HexDumpForm.IsTextFile)then
        HexDumpForm.DecodeBasicFile
       else
        HexDumpForm.btnSaveTextClick(nil);
      end
      else WriteLn(cmdRed+'Failed to extract file.'+cmdNormal);
      HexDumpForm.Free;
      {$ENDIF}
     end
     else WriteLn(cmdRed+'Cannot find file '''+Command[1]+'''.'+cmdNormal)
    else error:=2
   else error:=1;
  //New Image ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'new':
   if Confirm then
    if Length(Command)>1 then
    begin
     known:=False;
     ok:=False;
     format:=UpperCase(Command[1]);
     if Length(Command)>2 then format:=format+UpperCase(Command[2]);
     //Create ADFS HDD
     if format='ADFSHDD' then
     begin
      newmap:=False; //Default
      dirtype:=0; //Default
      harddrivesize:=20*1024*1024; //20MB default size
      if Length(Command)>3 then
       if Length(Command[3])>3 then
       begin
        if UpperCase(Command[3][1])='N' then newmap:=True;
        if UpperCase(Command[3][2])='N' then dirtype:=1;//New dir
        if UpperCase(Command[3][2])='B' then dirtype:=2;//Big dir
        if(newmap)and(dirtype=0)then
         dirtype:=1; //Can't have old dir on new map
        if(not newmap)and(dirtype=2)then
         dirtype:=1; //Can't have big dir on old map
        //Get the image size
        harddrivesize:=GetDriveSize(Command[3]);
        //Check that it is not over, or under, the limits
        if harddrivesize<20*1024*1024 then
         harddrivesize:=20*1024*1024;  //20MB min
        if harddrivesize>1000*1024*1024 then
         harddrivesize:=1000*1024*1024;//1000MB max
        if(not newmap)and(harddrivesize>512*1024*1024)then
         harddrivesize:=512*1024*1024; //512MB max for old map
       end;
      //OK, now create it
      ok:=Image.FormatHDD(diAcornADFS,harddrivesize,True,newmap,dirtype,False);
      known:=True;
     end;
     //Create AFS HDD
     if format='AFS' then
      if Length(Command)>3 then
      begin
       //Get the image size
       harddrivesize:=GetDriveSize(Command[3]);
       //Get the AFS level (second parameter)
       dirtype:=StrToIntDef(RightStr(Command[2],1),2);
       //Is the specified image size big enough
       if(dirtype=2)and(harddrivesize<400)then harddrivesize:=400;
       if(dirtype=3)and(harddrivesize<640)then harddrivesize:=640;
       //But not too big
       if harddrivesize>512*1024 then harddrivesize:=512*1024;
       //Create it
       ok:=Image.FormatHDD(diAcornFS,
                           harddrivesize*1024,
                           True,False,dirtype,False);
       known:=True;
      end else error:=2;
     if format='DOSHDD' then //Create DOS HDD
      if Length(Command)>3 then
      begin
       //Get the image size
       harddrivesize:=GetDriveSize(Command[3]);
       //Work the most appropriate FAT
       if harddrivesize<33300 then dirtype:=diFAT16 else dirtype:=diFAT32;
       //Is the specified image size big enough
       if harddrivesize<20*1024 then harddrivesize:=20*1024;
       //But not too big
       if harddrivesize>1024*1024 then harddrivesize:=512*1024;
       //Create it
       ok:=Image.FormatHDD(diDOSPlus,
                           harddrivesize*1024,True,False,dirtype,False);
       known:=True;
      end else error:=2;
     if format='AMIGAHDD' then //Create Amiga HDD
      if Length(Command)>3 then
      begin
       //Get the image size
       harddrivesize:=GetDriveSize(Command[3]);
       //Is the specified image size big enough
       if harddrivesize<20*1024 then harddrivesize:=20*1024;
       //But not too big
       if harddrivesize>1024*1024 then harddrivesize:=512*1024;
       //Create it
       ok:=Image.FormatHDD(diAmiga,harddrivesize*1024,True,False,0,False);
       known:=True;
      end else error:=2;
     if Pos(format,DiscFormats)>0 then //Create other
     begin
      Index:=(Pos(format,DiscFormats) DIV 8)+1;
      //Create new image
      if(Index>=Low(DiscNumber))and(Index<=High(DiscNumber))then
       ok:=Image.FormatFDD(DiscNumber[Index] DIV $100,
                          (DiscNumber[Index] DIV $10)MOD $10,
                           DiscNumber[Index] MOD $10);
       known:=True;
     end;
     if ok then
     begin
      WriteLn(cmdGreen+UpperCase(Command[1])+' Image created OK.'+cmdNormal);
      ReportFreeSpace;
      HasChanged:=True;
      Fcurrdir:=0;
     end
     else
      if known then WriteLn(cmdRed+'Failed to create image.'+cmdNormal)
      else WriteLn(cmdRed+'Unknown format.'+cmdNormal)
    end else error:=2;
  //Change the current partition +++++++++++++++++++++++++++++++++++++++++++++++
  'partition':
   if Image.FormatNumber<>diInvalidImg then
    //Has a side/partition been specified?
    if Length(Command)>1 then
    begin
     ptr:=StrToIntDef(Command[1],0);
     if ptr>=Length(Image.Partitions) then ptr:=0;
     Fcurrdir:=Image.Partitions[ptr].RootRef;
     WriteLn(cmdGreen+'Partition '+IntToStr(ptr)+' selected.'+cmdNormal);
    end else error:=2
   else error:=1;
  //Change the disc boot option ++++++++++++++++++++++++++++++++++++++++++++++++
  'opt':
   if Image.FormatNumber<>diInvalidImg then
   begin
    //Has a side/partition been specified?
    if Length(Command)>2 then
     ptr:=StrToIntDef(Command[2],Image.Disc[Fcurrdir].Partition)
    else ptr:=Image.Disc[Fcurrdir].Partition; //Default is current side
    //Needs an option, of course
    if Length(Command)>1 then
    begin
     //The option may have been supplied as a word or a number
     opt:=0;
     //First check for a word
     while(LowerCase(Command[1])<>Options[opt])
       and(opt<High(Options))do inc(opt);
     //Not found, convert to a number. Will be -1 if an unknown word is given
     if LowerCase(Command[1])<>Options[opt]then opt:=StrToIntDef(Command[1],-1);
     //Can't be higher than what we know
     if(opt>=0)and(opt<=High(Options))then
     begin
      Write('Update boot option to '+UpperCase(Options[opt])+' ');
      if Image.UpdateBootOption(opt,ptr) then
      begin
       HasChanged:=True;
       error:=-1;
      end
      else error:=3;
     end
     else WriteLn(cmdRed+'Invalid boot option.'+cmdNormal)
    end
    else error:=2
   end
   else error:=1;
  //Rename a file ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'rename':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>2 then
     if ValidFile(Command[1]) then//Does it exist?
     begin
      //Attempt to rename
      Write('Rename '+temp+' to '+Command[2]+' ');
      opt:=Image.RenameFile(temp,Command[2]);
      if opt>=0 then
      begin
       error:=-1;
       HasChanged:=True;
      end
      else error:=3;
     end else WriteLn(cmdRed+''''+Command[1]+''' not found.'+cmdNormal)
    else error:=2
   else error:=1;
  //Show image report ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'report':
   if Image.FormatNumber<>diInvalidImg then btn_ShowReportClick(nil)
   else error:=1;
  //Run a script +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'runscript': if Length(Command)<2 then error:=2;
  //Save image +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'save':
   if Image.FormatNumber<>diInvalidImg then
   begin
    //Get the filename
    if Length(Command)>1 then temp:=Command[1]
    else temp:=Image.Filename; //None given, so use the image one
    //Compressed UEF?
    if Length(Command)>2 then ok:=UpperCase(Command[2])='TRUE' else ok:=False;
    //Save
    if Image.SaveToFile(temp,ok) then
    begin
     WriteLn('Image saved OK.');
     HasChanged:=False;
    end else WriteLn(cmdRed+'Image failed to save.'+cmdNormal);
   end
   else error:=1;
  //Save image as CSV ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'savecsv':
   if Image.FormatNumber<>diInvalidImg then
   begin
    //Get the filename
    if Length(Command)>1 then temp:=Command[1] else temp:='';
    //Fire the function - a blank filename will get replaced with the current one
    SaveAsCSV(temp);
    WriteLn(cmdGreen+'CSV output complete.'+cmdNormal);
   end
   else error:=1;
  //Split partitions +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'split':WriteLn(cmdRed+'This command has not been implemented yet.'+cmdNormal);
  //Change the timestamp +++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'stamp':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then
    begin
     Files:=nil;
     Files:=GetListOfFiles(Command[1]);
     if Length(Files)>0 then
      for Index:=0 to Length(Files)-1 do
      begin
       temp:=BuildFilename(Files[Index]);
       Write('Setting date/time stamp for '+temp+' ');
       if Image.TimeStampFile(temp,Now) then
       begin
        HasChanged:=True;
        error:=-1;
       end
       else error:=3;
      end
     else WriteLn(cmdRed+'No files found'+cmdNormal);
    end
    else error:=2
   else error:=1;
  //Change the disc title ++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'title':
   if Image.FormatNumber<>diInvalidImg then
   begin
    //Has a side/partition been specified?
    if Length(Command)>2 then
     ptr:=StrToIntDef(Command[2],Image.Disc[Fcurrdir].Partition)
    else ptr:=Image.Disc[Fcurrdir].Partition; //Default is current side
    //Needs a title, of course
    if Length(Command)>1 then
    begin
     Write('Update disc title: ');
     if Image.UpdateDiscTitle(Command[1],ptr) then
     begin
      HasChanged:=True;
      error:=-1;
     end
     else error:=3;
    end
    else error:=2
   end
   else error:=1;
  //Blank entry ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ''         :;//Just ignore
  //Something not recognised +++++++++++++++++++++++++++++++++++++++++++++++++++
 otherwise WriteLn(cmdRed+'Unknown command.'+cmdNormal);
 end;
 //Report any errors +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 case error of
  -1: WriteLn(cmdGreen+'Success.'+cmdNormal);
   1: WriteLn(cmdRed+'No Image inserted.'+cmdNormal);
   2: WriteLn(cmdRed+'Not enough parameters.'+cmdNormal);
   3: WriteLn(cmdRed+'Failed.'+cmdNormal);
 end;
end;
