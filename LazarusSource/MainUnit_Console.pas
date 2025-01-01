{-------------------------------------------------------------------------------
Parse commands sent through via the console
-------------------------------------------------------------------------------}
procedure TMainForm.ParseCommand(var Command: TStringArray);
var
 error        : Integer=0;
 Lcurrdir     : Integer=0;
 opt          : Integer=0;
 Index        : Integer=0;
 ptr          : Integer=0;
 Lparent      : String='';
 temp         : String='';
 format       : String='';
 dir          : Cardinal=0;
 entry        : Cardinal=0;
 harddrivesize: Cardinal=0;
 dirtype      : Byte=0;
 known        : Boolean=False;
 ok           : Boolean=False;
 newmap       : Boolean=False;
 searchlist   : TSearchRec;
 Files        : TSearchResults;
 filedetails  : TDirEntry;
 filelist     : TStringList;
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
 Configs : array[0..24] of array[0..1] of String = (
 ('Texture'          ,'I'),
 ('ADFS_L_Interleave','I'),
 ('Spark_Is_FS'      ,'B'),
 ('CreateINF'        ,'B'),
 ('Hide_CDR_DEL'     ,'B'),
 ('DFS_Zero_Sectors' ,'B'),
 ('DFS_Beyond_Edge'  ,'B'),
 ('DFS_Allow_Blanks' ,'B'),
 ('UEF_Compress'     ,'B'),
 ('Scan_SubDirs'     ,'B'),
 ('Open_DOS'         ,'B'),
 ('View_Options'     ,'I'),
 ('Debug_Mode'       ,'B'),
 ('CSVIncDir'        ,'B'),
 ('CSVIncFilename'   ,'B'),
 ('CSVIncReport'     ,'B'),
 ('CSVParent'        ,'B'),
 ('CSVFilename'      ,'B'),
 ('CSVLoadAddr'      ,'B'),
 ('CSVExecAddr'      ,'B'),
 ('CSVLength'        ,'B'),
 ('CSVAttributes'    ,'B'),
 ('CSVAddress'       ,'B'),
 ('CSVCRC32'         ,'B'),
 ('CSVMD5'           ,'B'));
 //Validate a filename, building a complete path if required
 function ValidFile(thisfile: String): Boolean;
 begin
  //Build a complete path to the file, if required
  if Image.FileExists(thisfile,dir,entry) then
   temp:=thisfile
  else
   temp:=Image.GetParent(Fcurrdir)
        +Image.GetDirSep(Image.Disc[Fcurrdir].Partition)
        +thisfile;
  //Does it exist?
  Result:=Image.FileExists(temp,dir,entry);
 end;
 //Report the free space
 procedure ReportFreeSpace;
 var
  free,used,total: QWord;
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
  Lconfirm: String;
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
//Main procedure definition starts here
begin
 ResetDirEntry(filedetails);
 if Length(Command)=0 then exit;
 //Convert the command to lower case
 Command[0]:=LowerCase(Command[0]);
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
     if ValidFile(Command[1]) then
     begin
      Write('Changing attributes for '+temp+' ');
      if Image.UpdateAttributes(temp,Command[2])then
      begin
       WriteLn('success.');
       HasChanged:=True;
      end else WriteLn('failed.');
     end else WriteLn(cmdRed+''''+Command[1]+''' not found.'+cmdNormal)
    end
    else error:=2
   else error:=1;
  //Add files ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'add':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then //Are there any files given?
     for Index:=1 to Length(Command)-1 do//Just add a file
     begin
      //Can contain a wild card
      FindFirst(Command[Index],faDirectory,searchlist);
      repeat
       //These are previous and top directories
       if(searchlist.Name<>'.')and(searchlist.Name<>'..')then
       begin
        temp:=ExtractFilePath(Command[Index])+searchlist.Name;
        if(searchlist.Attr AND faDirectory)=faDirectory then
        begin //Add directory
         Write('Adding directory: '''+temp+'''.');
         if AddDirectoryToImage(temp) then
         begin
          HasChanged:=True;
          WriteLn(' Success.');
         end else WriteLn(' Failed.');
        end
        else //Add a single file
        begin
         Write('Adding file: '''+temp+'''.');
         if AddFileToImage(temp)>=0 then
         begin
          HasChanged:=True;
          WriteLn(' Success.');
         end else WriteLn(' Failed.');
        end;
       end;
      until FindNext(searchlist)<>0;
      FindClose(searchlist);
     end
    else error:=2//Nothing has been passed
   else error:=1;//No image
  //Display a catalogue of the current directory +++++++++++++++++++++++++++++++
  'cat':
   if Image.FormatNumber<>diInvalidImg then
   begin
    //Default option - just catalogue the current directory
    opt:=Fcurrdir;
    ptr:=Fcurrdir;
    //Has a parameter been passed?
    if Length(Command)>1 then
     if(LowerCase(Command[1])='all')     //Cat all directories and entries
     or(LowerCase(Command[1])='dir')     //Just show all the directories
     or(LowerCase(Command[1])='root')then//Just show the roots
     begin
      opt:=0;
      ptr:=Length(Image.Disc)-1;
     end
     else Command[1]:='' //Invalid entry, so blank it
    else //No parameter passed, so create a blank one
    begin
     SetLength(Command,2);
     Command[1]:='';
    end;
    for Lcurrdir:=opt to ptr do
    begin
     //List the catalogue
     if(Command[1]='')or(LowerCase(Command[1])='all')then
     begin
      WriteLn(cmdBlue+StringOfChar('-',80)+cmdNormal);
      WriteLn(cmdBold+'Catalogue listing for directory '
              +Image.GetParent(Lcurrdir));
      Write(PadRight(Image.Disc[Lcurrdir].Title,40));
      WriteLn('Option: '+IntToStr(Image.BootOpt[Image.Disc[Lcurrdir].Partition])
             +' ('
             +UpperCase(Options[Image.BootOpt[Image.Disc[Lcurrdir].Partition]])
             +')');
      WriteLn('Number of entries: '
             +IntToStr(Length(Image.Disc[Lcurrdir].Entries)));
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
        end;
        //New line
        WriteLn();
       end;
     end;
     //List only the directories or roots
     if(LowerCase(Command[1])='dir')or(LowerCase(Command[1])='root')then
     begin
      //Roots have no parent, so will be '-1'
      Write(cmdBold);
      if Image.Disc[Lcurrdir].Parent=-1 then Write('Root: ')
      else if LowerCase(Command[1])='dir' then Write('Directory: ');
      Write(cmdNormal);
      if(LowerCase(Command[1])='dir')
      or((LowerCase(Command[1])='root')and(Image.Disc[Lcurrdir].Parent=-1))then
       WriteLn(Image.GetParent(Lcurrdir));
     end;
    end;
   end else error:=1;
  //Change the host directory ++++++++++++++++++++++++++++++++++++++++++++++++++
  'chdir': if Length(Command)>1 then SetCurrentDir(Command[1]) else error:=2;
  //Set a configuration option +++++++++++++++++++++++++++++++++++++++++++++++++
  'config':
   if Length(Command)>2 then
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
    if ok then WriteLn('Configuration option set.')
    else WriteLn(cmdRed+'Invalid configuration option.'+cmdNormal);
   end else error:=2;
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
     WriteLn('success.');
     HasChanged:=True;
    end
    else WriteLn('failed.');
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
           +Image.GetDirSep(Image.Disc[Fcurrdir].Partition)
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
        WriteLn(''''+Command[Index]+''' deleted.');
        HasChanged:=True;
       end
       else WriteLn(cmdRed+'Could not delete '''+Command[Index]+'''.'+cmdNormal);
      end
      else WriteLn(cmdRed+''''+Command[Index]+''' not found.'+cmdNormal);
     end
    else error:=2//Nothing has been passed
   else error:=1;//No image
  //Change directory +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'dir':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then
    begin
     temp:=Command[1];
     //Parent ?
     if temp[1]='^' then
      if Image.Disc[Fcurrdir].Parent>=0 then
       temp:=Image.GetParent(Image.Disc[Fcurrdir].Parent)+Copy(temp,2)
      else
       temp:=Image.GetParent(0)+Copy(temp,2);
     //Are there more parent specifiers?
     Lparent:=Image.GetDirSep(Image.Disc[Fcurrdir].Partition)+'^';
     while Pos(Lparent,temp)>1 do
     begin
      ptr:=Pos(Lparent,temp)-1;
      while(ptr>1)
        and(temp[ptr]<>Image.GetDirSep(Image.Disc[Fcurrdir].Partition))do
       dec(ptr);
      if ptr>1 then
       temp:=LeftStr(temp,ptr-1)+Copy(temp,Pos(Lparent,temp)+Length(Lparent));
      if ptr=1 then
       temp:=LeftStr(temp,ptr)+Copy(temp,Pos(Lparent,temp)+Length(Lparent));
     end;
     //Found, so make sure that dir and entry are within bounds
     if ValidFile(temp) then
     begin
      if dir>=Length(Image.Disc) then Fcurrdir:=0; //Root
      if dir<Length(Image.Disc) then
       if entry<Length(Image.Disc[dir].Entries) then
        if Image.Disc[dir].Entries[entry].DirRef>=0 then
         Fcurrdir:=Image.Disc[dir].Entries[entry].DirRef
        else WriteLn(cmdRed+''''+temp+''' is a file.'+cmdNormal)
       else Fcurrdir:=dir;
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
      WriteLn('Directory '''+Image.GetParent(Fcurrdir)+''' selected.')
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
      WriteLn('success.');
      HasChanged:=True;
     end
     else WriteLn('failed.');
    end
    else error:=2//Nothing has been passed
   else error:=1;//No image
  //Change exec or load address ++++++++++++++++++++++++++++++++++++++++++++++++
  'exec','load':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>2 then
     if IntToHex(StrToIntDef('$'+Command[2],0),8)
       =UpperCase(RightStr('00000000'+Command[2],8)) then
     begin
      //Does it exist?
      if ValidFile(Command[1]) then
      begin
       ok:=False;
       //Print the text
       format:=LowerCase(Command[0]);
       if format='exec' then format:='execution'; //Expand exec
       Write('Change '+format+' address for '+temp
            +' to 0x'+IntToHex(StrToIntDef('$'+Command[2],0),8)+' ');
       //Attempt to update address
       if LowerCase(Command[0])='exec' then //Execution address
        ok:=Image.UpdateExecAddr(Command[1],StrToIntDef('$'+Command[2],0));
       if LowerCase(Command[0])='load' then //Load address
        ok:=Image.UpdateLoadAddr(Command[1],StrToIntDef('$'+Command[2],0));
       //Report back
       if ok then
       begin
        HasChanged:=True;
        WriteLn('success.');
       end
       else WriteLn('failed.');
      end
      else WriteLn(cmdRed+''''+Command[1]+''' not found.'+cmdNormal);
     end
     else WriteLn(cmdRed+'Invalid hex number.'+cmdNormal)
    else error:=2//Nothing has been passed
   else error:=1;//No image
  //Exit the console application +++++++++++++++++++++++++++++++++++++++++++++++
  'exit': if not Confirm then Command[0]:='';
  //Enter the GUI application ++++++++++++++++++++++++++++++++++++++++++++++++++
  'exittogui': WriteLn('Entering GUI.');
  //Extract command ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'extract':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then
     for Index:=1 to Length(Command)-1 do
     begin
      ResetDirEntry(filedetails);
      //Select the file
      filedetails.Filename:=Command[Index];
      filedetails.Parent:=Image.GetParent(Fcurrdir);
      //First we look for the files - this will allow wildcarding
      Files:=Image.FileSearch(filedetails);
      //Now go through all the results, if any, and extract each of them
      if Length(Files)>0 then //If there are any, of course
       for opt:=0 to Length(Files)-1 do
       begin
        temp:='';
        //Build the filename
        if Files[opt].Parent<>'' then
         temp:=Files[opt].Parent
              +Image.GetDirSep(Image.Disc[Fcurrdir].Partition);
        temp:=temp+Files[opt].Filename;
        //And extract it
        if Image.FileExists(temp,dir,entry) then
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
        end;
       end
      else WriteLn(cmdRed+'No files found.'+cmdNormal);
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
    else WriteLn('No images found.');
    filelist.Free;
   end
   else error:=2;//Nothing has been passed
  //Get the free space +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'free':
   if Image.FormatNumber<>diInvalidImg then ReportFreeSpace
   else error:=1;//No image
  //Help command +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'help':
   begin
    WriteLn(cmdBlue+cmdBold+'Console Help'+cmdNormal);
    {format:='';
    if Length(Command)>1 then format:=Command[1];}
    for Index:=0 to Help.Lines.Count-1 do
    begin
     temp:=Help.Lines[Index];
     if Length(temp)>1 then
      if temp[1]<>' ' then temp:=cmdRed+cmdBold+temp
      else temp:=Copy(temp,2);
     WriteLn(temp+cmdNormal);
    end;
   end;
  //Open command +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  'insert':
   if Confirm then
    if Length(Command)>1 then
     if FileExists(Command[1]) then
     begin
      WriteLn('Reading image.');
      if Image.LoadFromFile(Command[1]) then
      begin
       WriteLn(cmdBold+Image.FormatString+cmdNormal+' image read OK.');
       Fcurrdir:=0;
       ReportFreeSpace;
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
        WriteLn('Interleave changed to '
               +UpperCase(Inter[opt])+'.');
       end
       else WriteLn(cmdRed+'Failed to change interleave.'+cmdNormal)
      else WriteLn(cmdRed+'Invalid Interleave option.'+cmdNormal);
     end
     else WriteLn(cmdRed+'Not possible in this format.'+cmdNormal)
    else error:=2
   else error:=1;
  //Show the contents of a file ++++++++++++++++++++++++++++++++++++++++++++++++
  'list':
   if Image.FormatNumber<>diInvalidImg then
    if Length(Command)>1 then
     if ValidFile(Command[1])then
     begin
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
       //Free up the container
       HexDump[0].Free;
       SetLength(HexDump,0);
      end
      else WriteLn(cmdRed+'Failed to extract file.'+cmdNormal)
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
     if UpperCase(format)='ADFSHDD' then
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
     if UpperCase(Command[1])='AFS' then
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
     if UpperCase(format)='DOSHDD' then //Create DOS HDD
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
     if UpperCase(format)='AMIGAHDD' then //Create Amiga HDD
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
      WriteLn(UpperCase(Command[1])+' Image created OK.');
      ReportFreeSpace;
      HasChanged:=True;
      Fcurrdir:=0;
     end
     else
      if known then WriteLn(cmdRed+'Failed to create image.'+cmdNormal)
      else WriteLn(cmdRed+'Unknown format.'+cmdNormal)
    end else error:=2;
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
       WriteLn('success.');
      end
      else WriteLn('failed.')
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
       WriteLn('success.');
       HasChanged:=True;
      end
      else WriteLn('failed ('+IntToStr(opt)+').');
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
    if Length(Command)>1 then temp:=Command[1]
    else temp:=Image.Filename; //None given, so use the image one
    //Make sure it has a csv extension
    temp:=LeftStr(temp,Length(temp)-Length(ExtractFileExt(temp)))+'.csv';
    SaveAsCSV(temp);
    WriteLn('CSV output complete.');
   end
   else error:=1;
  //Display the current configurations +++++++++++++++++++++++++++++++++++++++++
  'status':
   begin
    WriteLn(cmdBold+cmdBlue+'Configuration'+cmdNormal);
    WriteLn('Not all configurations are used by the console.');
    //Get the longest string
    ptr:=1;
    for Index:=0 to Length(Configs)-1 do
     if Length(Configs[Index,0])>ptr then ptr:=Length(Configs[Index,0]);
    //Display the current configs
    for Index:=0 to Length(Configs)-1 do
    begin
     Write(cmdRed+cmdBold+PadRight(Configs[Index,0],ptr)+cmdNormal+': ');
     case Configs[Index,1] of
      'B' : WriteLn(DIMReg.GetRegValB(Configs[Index,0]));
      'I' : WriteLn('0x'+IntToHex(DIMReg.GetRegValI(Configs[Index,0]),4));
      'S' : WriteLn(DIMReg.GetRegValS(Configs[Index,0]));
     end;
    end;
   end;
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
     Write('Update disc title ');
     if Image.UpdateDiscTitle(Command[1],ptr) then
     begin
      HasChanged:=True;
      WriteLn('success.');
     end
     else WriteLn('failed.')
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
  1: WriteLn(cmdRed+'No Image loaded.'+cmdNormal);
  2: WriteLn(cmdRed+'Not enough parameters.'+cmdNormal);
 end;
end;
