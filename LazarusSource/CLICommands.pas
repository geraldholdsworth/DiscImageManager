unit CLICommands;

{
CLI Command Processor - Command-line interface for disc image operations.
This unit provides command processing without requiring any GUI dependencies.

Copyright (C) 2018-2025 Gerald Holdsworth gerald@hollypops.co.uk

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public Licence as published by the Free
Software Foundation; either version 3 of the Licence, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public Licence for more
details.

A copy of the GNU General Public Licence is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.
}

{$MODE objFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DiscImage, DiscImageContext, Global;

type
  { TCLICommandProcessor - Processes CLI commands }
  TCLICommandProcessor = class
  private
    FContext: TDiscImageContext;
    FSettings: TRegistrySettings;
    FOwnsContext: Boolean;
    FOwnsSettings: Boolean;
    FConsoleWidth: Integer;
    // ANSI style modifiers
    FUseColors: Boolean;
    procedure WriteColored(const S: String; const Color: String = '');
    procedure WriteLnColored(const S: String; const Color: String = '');
    function GetCurrentPath: String;
    function ValidFile(const Filename: String; out Dir, Entry: Cardinal): Boolean;
    function BuildFilename(const AFile: TDirEntry): String;
    function GetListOfFiles(const FileSearch: String; AddTo: TSearchResults = nil): TSearchResults;
    function GetImageFilename(Dir, Entry: Integer): String;
    procedure DownloadFile(Dir, Entry: Integer; const Path: String);
    procedure DownloadDirectory(Dir, Entry: Integer; const Path: String);
    procedure ReportFreeSpace;
    function Confirm: Boolean;
    function GetDriveSize(const GivenSize: String): Cardinal;
    procedure ShowHelp;
    procedure ListCatalogueEx(const Mode: String);
    // Command handlers
    procedure CmdAccess(const Params: TStringArray);
    procedure CmdAdd(const Params: TStringArray);
    procedure CmdCat(const Params: TStringArray);
    procedure CmdCreate(const Params: TStringArray);
    procedure CmdDelete(const Params: TStringArray);
    procedure CmdDir(const Params: TStringArray);
    procedure CmdExtract(const Params: TStringArray);
    procedure CmdFree(const Params: TStringArray);
    procedure CmdInsert(const Params: TStringArray);
    procedure CmdNew(const Params: TStringArray);
    procedure CmdOpt(const Params: TStringArray);
    procedure CmdRename(const Params: TStringArray);
    procedure CmdSave(const Params: TStringArray);
    procedure CmdTitle(const Params: TStringArray);
    procedure CmdConfig(const Params: TStringArray);
    procedure CmdStatus(const Params: TStringArray);
    procedure CmdSearch(const Params: TStringArray);
    procedure CmdDefrag(const Params: TStringArray);
    procedure CmdDirTitle(const Params: TStringArray);
    procedure CmdExecLoadType(const Params: TStringArray);
    procedure CmdFind(const Params: TStringArray);
    procedure CmdFileToCSV(const Params: TStringArray);
    procedure CmdFileType(const Params: TStringArray);
    procedure CmdInterleave(const Params: TStringArray);
    procedure CmdList(const Params: TStringArray);
    procedure CmdReport(const Params: TStringArray);
    procedure CmdRunScript(const Params: TStringArray);
    procedure CmdSaveCSV(const Params: TStringArray);
    procedure CmdStamp(const Params: TStringArray);
    procedure WriteCSVForImage(LImage: TDiscImage; const CSVFilename: String);
  public
    constructor Create; overload;
    constructor Create(AContext: TDiscImageContext; ASettings: TRegistrySettings); overload;
    destructor Destroy; override;

    function ProcessCommand(const Command: TStringArray): Boolean;
    function ParseInput(const Input: String): TStringArray;

    property Context: TDiscImageContext read FContext;
    property Settings: TRegistrySettings read FSettings;
    property UseColors: Boolean read FUseColors write FUseColors;
    property ConsoleWidth: Integer read FConsoleWidth write FConsoleWidth;
  end;

  // ANSI color constants
const
  clNormal  = #$1B'[0m';
  clBold    = #$1B'[1m';
  clRed     = #$1B'[91m';
  clGreen   = #$1B'[92m';
  clYellow  = #$1B'[93m';
  clBlue    = #$1B'[94m';
  clMagenta = #$1B'[95m';
  clCyan    = #$1B'[96m';

  // Disc format strings
  DiscFormats =
    'DFSS80  DFSS40  DFSD80  DFSD40  WDFSS80 WDFSS40 WDFSD80 WDFSD40 ADFSS   ADFSM   ' +
    'ADFSL   ADFSD   ADFSE   ADFSE+  ADFSF   ADFSF+  C1541   C1571   C1581   AMIGADD ' +
    'AMIGAHD CFS     DOS+640 DOS+800 DOS360  DOS720  DOS1440 DOS2880 ';

  DiscNumber: array[1..28] of Integer = (
    $001, $000, $011, $010, $021, $020, $031, $030, $100, $110,
    $120, $130, $140, $150, $160, $170, $200, $210, $220, $400,
    $410, $500, $A00, $A01, $A02, $A03, $A04, $A05);

  BootOptions: array[0..3] of String = ('none', 'load', 'run', 'exec');
  Interleaves: array[0..3] of String = ('auto', 'seq', 'int', 'mux');

  // Configuration settings (registry) - matches GUI's Configs array
  Configs: array[0..42] of array[0..2] of String = (
    ('AddImpliedAttributes' ,'B','Add Implied Attributes for DFS/CFS/RFS'),
    ('ADFS_L_Interleave'    ,'I','0=Automatic; 1=Sequential; 2=Interleave; 3=Multiplex'),
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

implementation

{ TCLICommandProcessor }

constructor TCLICommandProcessor.Create;
begin
  inherited Create;
  FContext := TDiscImageContext.Create;
  FSettings := TRegistrySettings.Create('DiscImageManager');
  FOwnsContext := True;
  FOwnsSettings := True;
  FConsoleWidth := 80;
  FUseColors := True;
end;

constructor TCLICommandProcessor.Create(AContext: TDiscImageContext;
  ASettings: TRegistrySettings);
begin
  inherited Create;
  FContext := AContext;
  FSettings := ASettings;
  FOwnsContext := False;
  FOwnsSettings := False;
  FConsoleWidth := 80;
  FUseColors := True;
end;

destructor TCLICommandProcessor.Destroy;
begin
  if FOwnsContext then
    FContext.Free;
  if FOwnsSettings then
    FSettings.Free;
  inherited Destroy;
end;

procedure TCLICommandProcessor.WriteColored(const S: String; const Color: String);
begin
  if FUseColors and (Color <> '') then
    Write(Color + S + clNormal)
  else
    Write(S);
end;

procedure TCLICommandProcessor.WriteLnColored(const S: String; const Color: String);
begin
  WriteColored(S, Color);
  WriteLn;
end;

function TCLICommandProcessor.GetCurrentPath: String;
begin
  if FContext.Image.FormatNumber <> diInvalidImg then
    Result := FContext.Image.GetParent(FContext.CurrentDir)
  else
    Result := '';
end;

function TCLICommandProcessor.ValidFile(const Filename: String;
  out Dir, Entry: Cardinal): Boolean;
var
  Temp: String;
begin
  if FContext.Image.FileExists(Filename, Dir, Entry) then
    Result := True
  else
  begin
    Temp := FContext.Image.GetParent(FContext.CurrentDir) +
            FContext.Image.GetDirSep(FContext.Image.Disc[FContext.CurrentDir].Partition) +
            Filename;
    Result := FContext.Image.FileExists(Temp, Dir, Entry);
  end;
end;

function TCLICommandProcessor.BuildFilename(const AFile: TDirEntry): String;
begin
  Result := '';
  if AFile.Parent <> '' then
    Result := AFile.Parent +
              FContext.Image.GetDirSep(FContext.Image.Disc[FContext.CurrentDir].Partition);
  Result := Result + AFile.Filename;
end;

function TCLICommandProcessor.GetListOfFiles(const FileSearch: String; AddTo: TSearchResults): TSearchResults;
var
  FileDetails: TDirEntry;
begin
  ResetDirEntry(FileDetails);
  FileDetails.Filename := FileSearch;
  FileDetails.Parent := FContext.Image.GetParent(FContext.CurrentDir);
  Result := FContext.Image.FileSearch(FileDetails, AddTo);
end;

function TCLICommandProcessor.GetImageFilename(Dir, Entry: Integer): String;
begin
  Result := '';
  if (Dir >= 0) and (Dir < Length(FContext.Image.Disc)) then
    if (Length(FContext.Image.Disc[Dir].Entries) = 0) or (Entry = -1) then
      Result := FContext.Image.Disc[Dir].Directory
    else
      Result := FContext.Image.GetParent(Dir)
              + FContext.Image.GetDirSep(FContext.Image.Disc[Dir].Partition)
              + FContext.Image.Disc[Dir].Entries[Entry].Filename;
end;

procedure TCLICommandProcessor.DownloadFile(Dir, Entry: Integer; const Path: String);
var
  F: TFileStream;
  Buffer: TDIByteArray;
  ImageFilename, WindowsFilename, FullPath: String;
begin
  FullPath := Path;
  if (Length(FullPath) > 0) and (FullPath[Length(FullPath)] <> PathDelim) then
    FullPath := FullPath + PathDelim;

  // Object is a file, so download it
  if FContext.Image.Disc[Dir].Entries[Entry].DirRef = -1 then
  begin
    ImageFilename := GetImageFilename(Dir, Entry);
    WindowsFilename := FContext.Image.GetWindowsFilename(Dir, Entry);
    if FContext.Image.ExtractFile(ImageFilename, Buffer, Entry) then
    begin
      try
        F := TFileStream.Create(FullPath + WindowsFilename, fmCreate or fmShareDenyNone);
        try
          F.Position := 0;
          if Length(Buffer) > 0 then
            F.Write(Buffer[0], Length(Buffer));
          if FContext.CreateINF then
            FContext.Image.CreateINFFile(Dir, Entry, FullPath);
          WriteLnColored('Success.', clGreen);
        finally
          F.Free;
        end;
      except
        on E: Exception do
          WriteLnColored('failed to write.', clRed);
      end;
    end
    else
      WriteLnColored('failed to extract.', clRed);
  end
  else
    DownloadDirectory(Dir, Entry, FullPath + FContext.Image.GetWindowsFilename(Dir, Entry));
end;

procedure TCLICommandProcessor.DownloadDirectory(Dir, Entry: Integer; const Path: String);
var
  ImageFilename, FullPath: String;
  Ref: Cardinal;
  S, C: Integer;
begin
  FullPath := Path;
  if (Length(FullPath) > 0) and (FullPath[Length(FullPath)] <> PathDelim) then
    FullPath := FullPath + PathDelim;

  ImageFilename := GetImageFilename(Dir, Entry);

  if FContext.Image.FileExists(ImageFilename, Ref) then
  begin
    // Create the directory on the host filesystem
    if not DirectoryExists(FullPath) then
    begin
      CreateDir(FullPath);
      if FContext.CreateINF then
        FContext.Image.CreateINFFile(Dir, Entry, ExtractFilePath(ExcludeTrailingPathDelimiter(FullPath)));
    end;

    WriteLn;

    // Navigate into the directory
    S := FContext.Image.Disc[Dir].Entries[Entry].DirRef;

    // Iterate through the entries
    for C := 0 to Length(FContext.Image.Disc[S].Entries) - 1 do
    begin
      Write('Extracting '
            + FContext.Image.Disc[S].Entries[C].Parent
            + FContext.Image.GetDirSep(FContext.Image.Disc[S].Partition)
            + FContext.Image.Disc[S].Entries[C].Filename + ' ');
      DownloadFile(S, C, FullPath);
    end;
  end
  else
    WriteLnColored('Could not locate directory "' + ImageFilename + '".', clRed);
end;

procedure TCLICommandProcessor.ReportFreeSpace;
var
  FreeBytes, UsedBytes, TotalBytes: QWord;
begin
  FreeBytes := FContext.Image.FreeSpace(FContext.Image.Disc[FContext.CurrentDir].Partition);
  TotalBytes := FContext.Image.DiscSize(FContext.Image.Disc[FContext.CurrentDir].Partition);
  UsedBytes := TotalBytes - FreeBytes;
  WriteColored(IntToStr(FreeBytes), clBold);
  Write(' bytes free. ');
  WriteColored(IntToStr(UsedBytes), clBold);
  Write(' bytes used. ');
  WriteColored(IntToStr(TotalBytes), clBold);
  WriteLn(' bytes total.');
end;

function TCLICommandProcessor.Confirm: Boolean;
var
  Response: String;
begin
  Result := True;
  if FContext.HasChanged then
  begin
    Result := False;
    WriteLn('Image has been modified.');
    Write('Are you sure you want to continue? (yes/no): ');
    ReadLn(Response);
    if (Length(Response) > 0) and (LowerCase(Response[1]) = 'y') then
      Result := True;
  end;
end;

function TCLICommandProcessor.GetDriveSize(const GivenSize: String): Cardinal;
begin
  Result := StrToIntDef(GivenSize, 0);
  if UpperCase(RightStr(GivenSize, 1)) = 'M' then
    Result := StrToIntDef(LeftStr(GivenSize, Length(GivenSize) - 1), 0) * 1024;
end;

procedure TCLICommandProcessor.ShowHelp;
const
  HelpLines: array[0..166] of String = (
    ' Command parameters are separated by spaces and are separated from the command by a space. Use quotes (") to enclose parameters containing a space. For example:',
    ' add "This File.txt" ThatFile.txt',
    ' will add a file called ''This File.txt'' and another called ''ThatFile.txt'' to the image.',
    '',
    ' Square brackets [] indicate optional parameters.',
    ' Ellipses ... indicate multiples of the same parameter.',
    ' Bar | indicate to use only one of the parameters.',
    '',
    ' Valid wildcards are:',
    ' * : matches 0 or more characters',
    ' # : matches any character',
    ' | : when used before a search, it excludes these results from results already found',
    '',
    'access <file> [<attributes>]',
    ' Changes the file''s access rights, or attributes, to those given. Anything invalid is ignored. Filename can contain wildcards.',
    '',
    'add <file> [[<file>] ...]',
    ' Adds the files/directories listed in the local OS folder. Can contain wildcards.',
    '',
    'cat [<option>]',
    ' Displays a catalogue listing. If <option> is not given, then the current directory is shown. <option> can be one of the following:',
    ' all  : Displays a catalogue listing for the entire image.',
    ' dir  : Lists all the directories in the image.',
    ' root : Lists all the roots in the image.',
    '',
    'chdir <dirname>',
    ' Changes the host OS directory.',
    '',
    'compact [<partition>]',
    ' Performs a compaction/defrag on the selected partition. If none specified, it acts on the current partition. Same as defrag.',
    '',
    'config [<setting> <option>]',
    ' Sets a configuration setting. Note that not all configuration settings are used by the console and could result in unpredictable behaviour.',
    ' <option> can be:',
    ' Boolean : True or False.',
    ' Integer : Decimal number, unless preceded by 0x, $ or & for Hex number.',
    ' String  : Anything.',
    ' Passing config with no parameters will list the valid options.',
    '',
    'create [<dirname>]',
    ' Creates a new directory. If no name given, ''NewDir'' is used instead.',
    '',
    'defrag [<partition>]',
    ' Performs a compaction/defrag on the selected partition. If none specified, it acts on the current partition. Same as compact.',
    '',
    'delete <file> [[<file>] ...]',
    ' Deletes the files/directories listed. Wildcards not allowed.',
    '',
    'dir <dirname>',
    ' Changes to directory <dirname>. Use ''^'' to specify the parent directory.',
    '',
    'dirtitle <title>',
    ' Changes the currect directory title.',
    '',
    'exit',
    ' Quits console and application.',
    '',
    'exittogui',
    ' Quits the console and opens the GUI application.',
    '',
    'exec <filename> <address>',
    ' Updates the execution address for <filename> to be <address>, which must be a valid hex number. Filename can contain wildcards.',
    '',
    'extract <file> [[<file>] ...]',
    ' Extracts all files/directories listed to the local OS folder. Filenames can contain wildcards.',
    '',
    'filetocsv <file> [[<file>] ...]',
    ' Outputs a CSV format of the specified files. Can contain wildcards.',
    '',
    'filetype <hex>|<name>',
    ' Translates the hex number to known ADFS filetype, or vice-versa.',
    '',
    'find <file> [[<file>] ...]',
    ' Finds the files/directories listed in the local OS folder. Can contain wildcards.',
    '',
    'free',
    ' Displays the free space on the partition/side.',
    '',
    'insert <filename>',
    ' Loads image specified by <filename>.',
    '',
    'interleave <option>',
    ' Changes the current interleave method and re-organises the data.',
    ' <option> can be 0, 1, 2, or 3 or auto, seq, int, or mux.',
    ' Only valid for Acorn ADFS L or FS.',
    '',
    'load <filename> <address>',
    ' Updates the load address for <filename> to be <address>, which must be a valid hex number. Filename can contain wildcards.',
    '',
    'list <filename>',
    ' Displays the file specified (BBC BASIC listing/text output/hex dump).',
    '',
    'new <format> [<option> [<option2>]]',
    ' Creates a new image:',
    ' <format> <option> <option2>  Result',
    ' DFS      S80                 Acorn DFS single sided 80 track',
    ' DFS      S40                 Acorn DFS single sided 40 track',
    ' DFS      D80                 Acorn DFS double sided 80 track',
    ' DFS      D40                 Acorn DFS double sided 40 track',
    ' WDFS     S80                 Watford DFS single sided 80 track',
    ' WDFS     S40                 Watford DFS single sided 40 track',
    ' WDFS     D80                 Watford DFS double sided 80 track',
    ' WDFS     D40                 Watford DFS double sided 40 track',
    ' ADFS     S                   Acorn ADFS S',
    ' ADFS     M                   Acorn ADFS M',
    ' ADFS     L                   Acorn ADFS L',
    ' ADFS     D                   Acorn ADFS D',
    ' ADFS     E                   Acorn ADFS E',
    ' ADFS     E+                  Acorn ADFS E+',
    ' ADFS     F                   Acorn ADFS F',
    ' ADFS     F+                  Acorn ADFS F+',
    ' ADFS     HDD                 Old map, Old directory 20MB',
    ' ADFS     HDD      OO<cap>[M] Old map, Old directory <cap> size',
    ' ADFS     HDD      ON<cap>[M] Old map, New directory <cap> size',
    ' ADFS     HDD      NN<cap>[M] New map, New directory <cap> size',
    ' ADFS     HDD      NB<cap>[M] New map, Big directory <cap> size',
    ' AFS      <level>  <cap>[M]   Acorn FS Level <level> of <cap> size',
    ' CFS                          Acorn Cassette Filing System',
    ' C1541                        Commodore 1541',
    ' C1571                        Commodore 1571',
    ' C1581                        Commodore 1581',
    ' AMIGA    DD                  Commodore Amiga DD',
    ' AMIGA    HD                  Commodore Amiga HD',
    ' AMIGA    HDD      <cap>[M]   Commodore Amiga hard drive of <cap> size',
    ' DOS+     640                 DOS+ 640K',
    ' DOS+     800                 DOS+ 800K',
    ' DOS      360                 DOS 360K',
    ' DOS      720                 DOS 720K',
    ' DOS      1440                DOS 1.44MB',
    ' DOS      2880                DOS 2.88MB',
    ' DOS      HDD      <cap>[M]   DOS hard drive of <cap> size',
    ' <cap> is specified in KB, or MB if M is included.',
    '',
    'opt <option> [<side>]',
    ' Sets the boot option for the current side, or <side> if specified.',
    ' <option> can be 0, 1, 2, or 3, or can be none, load, run, or exec.',
    '',
    'rename <oldfilename> <newfilename>',
    ' Renames <oldfilename> to <newfilename>.',
    '',
    'report',
    ' Displays the image report.',
    '',
    'runscript <filename>',
    ' Runs a script. Cannot be used if one is already running.',
    '',
    'save [<filename>] [<compressed>]',
    ' Saves the current loaded image to the host OS.',
    ' If a UEF is required to be compressed, pass ''TRUE'' as the second parameter.',
    '',
    'savecsv [<filename>]',
    ' Outputs the contents of the currently loaded image in CSV format.',
    '',
    'search <file> [[<file>] ...]',
    ' Finds all files/directories listed. Filenames can contain wildcards.',
    '',
    'status',
    ' Displays the current configuration settings.',
    '',
    'stamp <file>',
    ' Timestamps the specified file with the current time and date.',
    '',
    'title <title> [<side>]',
    ' Sets the disc title for the current side, or <side> if specified.',
    '',
    'type <file> <filetype>',
    ' Updates the filetype for <filename> to be <filetype>, which must be a valid hex number. Filename can contain wildcards.'
  );
var
  I: Integer;
  Line, SBold, SRed, SNormal, SBlue: String;
begin
  if FUseColors then
  begin
    SBold := clBold;
    SRed := clRed;
    SNormal := clNormal;
    SBlue := clBlue;
  end
  else
  begin
    SBold := '';
    SRed := '';
    SNormal := '';
    SBlue := '';
  end;

  WriteLn(SBlue + SBold + 'Console Help' + SNormal);
  for I := Low(HelpLines) to High(HelpLines) do
  begin
    Line := HelpLines[I];
    if Length(Line) > 1 then
    begin
      if Line[1] <> ' ' then
        Line := SRed + SBold + Line
      else
        Line := Copy(Line, 2);
    end;
    WriteLn(WrapText(Line, FConsoleWidth) + SNormal);
  end;
end;

procedure TCLICommandProcessor.ListCatalogueEx(const Mode: String);
var
  StartDir, EndDir, Dir, Entry: Integer;
  ShowDirOnly, ShowRootOnly, ShowFull: Boolean;
  Partition: Integer;
const
  TimeDateFormat = 'dd/mm/yyyy hh:nn:ss';
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  ShowDirOnly := (Mode = 'dir');
  ShowRootOnly := (Mode = 'root');
  ShowFull := (Mode = '') or (Mode = 'all');

  if (Mode = 'all') or (Mode = 'dir') or (Mode = 'root') then
  begin
    StartDir := 0;
    EndDir := Length(FContext.Image.Disc) - 1;
  end
  else
  begin
    StartDir := FContext.CurrentDir;
    EndDir := FContext.CurrentDir;
  end;

  for Dir := StartDir to EndDir do
  begin
    Partition := FContext.Image.Disc[Dir].Partition;

    // Show directory/root listing only
    if ShowDirOnly or ShowRootOnly then
    begin
      if FContext.Image.Disc[Dir].Parent = -1 then
      begin
        WriteColored('Root: ', clBold);
        WriteLn(FContext.Image.GetParent(Dir));
      end
      else if ShowDirOnly then
      begin
        WriteColored('Directory: ', clBold);
        WriteLn(FContext.Image.GetParent(Dir));
      end;
    end
    else if ShowFull then
    begin
      // Full catalogue listing
      WriteLnColored(StringOfChar('-', FConsoleWidth), clBlue);
      WriteColored('Catalogue listing for directory ', clBold);
      WriteLn(FContext.Image.GetParent(Dir));
      Write(Format('%-40s', [FContext.Image.Disc[Dir].Title]));
      WriteLn('Option: ' + IntToStr(FContext.Image.BootOpt[Partition]) +
              ' (' + UpperCase(BootOptions[FContext.Image.BootOpt[Partition]]) + ')');
      WriteLn('Number of entries: ' + IntToStr(Length(FContext.Image.Disc[Dir].Entries)));
      WriteLn;

      if Length(FContext.Image.Disc[Dir].Entries) > 0 then
      begin
        for Entry := 0 to Length(FContext.Image.Disc[Dir].Entries) - 1 do
        begin
          with FContext.Image.Disc[Dir].Entries[Entry] do
          begin
            // Filename - padded to 10 chars
            Write(Format('%-10s', [Filename]));
            // Attributes
            Write(' (' + Attributes + ')');

            // Files (not directories)
            if DirRef = -1 then
            begin
              // Filetype - ADFS, Spark only
              if (FileType <> '') and
                 ((FContext.Image.MajorFormatNumber = diAcornADFS) or
                  (FContext.Image.MajorFormatNumber = diSpark)) then
                Write(' ' + FileType);

              // Timestamp - ADFS, Spark, FileStore, Amiga, DOS only
              if (TimeStamp > 0) and
                 ((FContext.Image.MajorFormatNumber = diAcornADFS) or
                  (FContext.Image.MajorFormatNumber = diSpark) or
                  (FContext.Image.MajorFormatNumber = diAcornFS) or
                  (FContext.Image.MajorFormatNumber = diAmiga) or
                  (FContext.Image.MajorFormatNumber = diDOSPlus)) then
                Write(' ' + FormatDateTime(TimeDateFormat, TimeStamp));

              // Load/Exec addresses (if no timestamp or for AFS)
              if (TimeStamp = 0) or (FContext.Image.MajorFormatNumber = diAcornFS) then
              begin
                Write(' ' + IntToHex(LoadAddr, 8));
                Write(' ' + IntToHex(ExecAddr, 8));
              end;

              // Length
              Write(' ' + ConvertToKMG(Length) +
                    ' (' + IntToHex(Length, 8) + ')');
            end;

            WriteLn;
          end;
        end;
      end;
    end;
  end;
end;

// Command handlers

procedure TCLICommandProcessor.CmdAccess(const Params: TStringArray);
var
  Files: TSearchResults;
  I: Integer;
  Temp, Attr: String;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: access <filename> [attributes]', clRed);
    Exit;
  end;

  Attr := '';
  if Length(Params) > 2 then
    Attr := Params[2];

  Files := GetListOfFiles(Params[1]);
  if Length(Files) > 0 then
  begin
    for I := 0 to Length(Files) - 1 do
    begin
      Temp := BuildFilename(Files[I]);
      Write('Changing attributes for ' + Temp + ' ');
      if FContext.Image.UpdateAttributes(Temp, Attr) then
      begin
        WriteLnColored('success.', clGreen);
        FContext.HasChanged := True;
      end
      else
        WriteLnColored('failed.', clRed);
    end;
  end
  else
    WriteLnColored('No files found.', clRed);
end;

procedure TCLICommandProcessor.CmdAdd(const Params: TStringArray);
type
  TOSFile = record
    Filename: String;
    IsDirectory: Boolean;
  end;

  procedure AddDirectoryRecursive(const DirPath: String);
  var
    SR: TSearchRec;
    DirName, Attr: String;
    FileDetails: TDirEntry;
    Buffer: TDIByteArray;
    F: TFileStream;
    Ok: Boolean;
    OldDir: Integer;
  begin
    // Create directory on the image
    DirName := ExtractFileName(ExcludeTrailingPathDelimiter(DirPath));
    Attr := 'DLR';
    Write('Adding directory: ''' + DirPath + '''.');
    if FContext.Image.CreateDirectory(DirName,
       FContext.Image.GetParent(FContext.CurrentDir), Attr) >= 0 then
    begin
      WriteLnColored(' Success.', clGreen);
      FContext.HasChanged := True;
    end
    else
    begin
      WriteLnColored(' Failed.', clRed);
      Exit;
    end;

    // Navigate into the newly created directory
    OldDir := FContext.CurrentDir;
    // Find the new directory entry to get its DirRef
    if Length(FContext.Image.Disc[OldDir].Entries) > 0 then
    begin
      if FContext.Image.Disc[OldDir].Entries[
         Length(FContext.Image.Disc[OldDir].Entries) - 1].DirRef >= 0 then
        FContext.CurrentDir := FContext.Image.Disc[OldDir].Entries[
           Length(FContext.Image.Disc[OldDir].Entries) - 1].DirRef;
    end;

    // Add all files and subdirectories
    if FindFirst(IncludeTrailingPathDelimiter(DirPath) + '*', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Name <> '.') and (SR.Name <> '..') and (SR.Name <> '') then
        begin
          if (SR.Attr and faDirectory) = faDirectory then
          begin
            // Recurse into subdirectory
            AddDirectoryRecursive(IncludeTrailingPathDelimiter(DirPath) + SR.Name);
          end
          else
          begin
            // Add file
            Write('Adding file: ''' +
                  IncludeTrailingPathDelimiter(DirPath) + SR.Name + '''.');
            try
              F := TFileStream.Create(
                   IncludeTrailingPathDelimiter(DirPath) + SR.Name,
                   fmOpenRead or fmShareDenyNone);
              try
                SetLength(Buffer, F.Size);
                if F.Size > 0 then F.ReadBuffer(Buffer[0], F.Size);
                ResetDirEntry(FileDetails);
                FileDetails.Filename := SR.Name;
                FileDetails.Parent := FContext.Image.GetParent(FContext.CurrentDir);
                FileDetails.Length := F.Size;
                Ok := FContext.Image.WriteFile(FileDetails, Buffer) >= 0;
              finally
                F.Free;
              end;
              if Ok then
              begin
                WriteLnColored(' Success.', clGreen);
                FContext.HasChanged := True;
              end
              else
                WriteLnColored(' Failed.', clRed);
            except
              WriteLnColored(' Error reading file.', clRed);
            end;
          end;
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;

    // Navigate back to original directory
    FContext.CurrentDir := OldDir;
  end;

var
  I, J, Ptr: Integer;
  OSFiles: array of TOSFile;
  FS: TSearchRec;
  Ok: Boolean;
  Temp: String;
  FileDetails: TDirEntry;
  Buffer: TDIByteArray;
  F: TFileStream;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Not enough parameters.', clRed);
    Exit;
  end;

  SetLength(OSFiles, 0);

  // Collate list of files/directories (matching GUI behavior)
  for I := 1 to Length(Params) - 1 do
  begin
    Ok := True;
    Temp := Params[I];

    // Check for exclusion prefix
    if (Length(Temp) > 0) and (Temp[1] = '|') then
    begin
      Ok := False;
      Temp := Copy(Temp, 2);
    end;

    if FindFirst(Temp, faAnyFile, FS) = 0 then
    begin
      repeat
        if (FS.Name <> '.') and (FS.Name <> '..') and (FS.Name <> '') then
        begin
          if Ok then
          begin
            // Add to list
            Ptr := Length(OSFiles);
            SetLength(OSFiles, Ptr + 1);
            OSFiles[Ptr].Filename := ExtractFilePath(Temp) + FS.Name;
            OSFiles[Ptr].IsDirectory := (FS.Attr and faDirectory) = faDirectory;
          end
          else
          begin
            // Remove from list
            Temp := ExtractFilePath(Temp) + FS.Name;
            for J := 0 to Length(OSFiles) - 1 do
              if (OSFiles[J].Filename = Temp) and
                 (OSFiles[J].IsDirectory = ((FS.Attr and faDirectory) = faDirectory)) then
                OSFiles[J].Filename := '';
          end;
        end;
      until FindNext(FS) <> 0;
      FindClose(FS);
    end;
  end;

  // Remove blank entries
  Ptr := 0;
  while Ptr < Length(OSFiles) do
  begin
    if OSFiles[Ptr].Filename = '' then
    begin
      if Ptr < Length(OSFiles) - 1 then
        for I := Ptr to Length(OSFiles) - 2 do
          OSFiles[I] := OSFiles[I + 1];
      SetLength(OSFiles, Length(OSFiles) - 1);
      Dec(Ptr);
    end;
    Inc(Ptr);
  end;

  WriteLn(IntToStr(Length(OSFiles)) + ' entries found.');

  // Now add them
  for Ptr := 0 to Length(OSFiles) - 1 do
  begin
    if OSFiles[Ptr].IsDirectory then
    begin
      // Add directory recursively
      AddDirectoryRecursive(OSFiles[Ptr].Filename);
    end
    else
    begin
      // Add a single file
      Write('Adding file: ''' + OSFiles[Ptr].Filename + '''.');
      try
        F := TFileStream.Create(OSFiles[Ptr].Filename,
             fmOpenRead or fmShareDenyNone);
        try
          SetLength(Buffer, F.Size);
          if F.Size > 0 then F.ReadBuffer(Buffer[0], F.Size);
          ResetDirEntry(FileDetails);
          FileDetails.Filename := ExtractFileName(OSFiles[Ptr].Filename);
          FileDetails.Parent := FContext.Image.GetParent(FContext.CurrentDir);
          FileDetails.Length := F.Size;
          Ok := FContext.Image.WriteFile(FileDetails, Buffer) >= 0;
        finally
          F.Free;
        end;
        if Ok then
        begin
          WriteLnColored(' Success.', clGreen);
          FContext.HasChanged := True;
        end
        else
          WriteLnColored(' Failed.', clRed);
      except
        WriteLnColored(' Error reading file.', clRed);
      end;
    end;
  end;
end;

procedure TCLICommandProcessor.CmdCat(const Params: TStringArray);
var
  Mode: String;
begin
  Mode := '';
  if Length(Params) > 1 then
    Mode := LowerCase(Params[1]);

  ListCatalogueEx(Mode);
end;

procedure TCLICommandProcessor.CmdCreate(const Params: TStringArray);
var
  DirName, Parent, Attr: String;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  DirName := 'NewDir';
  if Length(Params) > 1 then
    DirName := Params[1];

  Parent := FContext.Image.GetParent(FContext.CurrentDir);
  Attr := 'DLR';

  Write('Create new directory ''' + DirName + ''' ');
  if FContext.Image.CreateDirectory(DirName, Parent, Attr) >= 0 then
  begin
    WriteLnColored('success.', clGreen);
    FContext.HasChanged := True;
  end
  else
    WriteLnColored('failed.', clRed);
end;

procedure TCLICommandProcessor.CmdDelete(const Params: TStringArray);
var
  I: Integer;
  Dir, Entry: Cardinal;
  Temp: String;
  Ok: Boolean;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: delete <filename> [filename2] ...', clRed);
    Exit;
  end;

  for I := 1 to Length(Params) - 1 do
  begin
    Temp := FContext.Image.GetParent(FContext.CurrentDir) +
            FContext.Image.GetDirSep(FContext.Image.Disc[FContext.CurrentDir].Partition) +
            Params[I];

    Ok := FContext.Image.FileExists(Temp, Dir, Entry);
    if not Ok then
    begin
      Temp := Params[I];
      Ok := FContext.Image.FileExists(Temp, Dir, Entry);
    end;

    if Ok then
    begin
      // UEF and RFS use entry-based deletion
      if (FContext.Image.MajorFormatNumber <> diAcornUEF)
      and (FContext.Image.MajorFormatNumber <> diAcornRFS) then
        Ok := FContext.Image.DeleteFile(Temp)
      else
        Ok := FContext.Image.DeleteFile(Entry);
      if Ok then
      begin
        WriteLn('''' + Params[I] + ''' deleted.');
        FContext.HasChanged := True;
      end
      else
        WriteLnColored('Could not delete ''' + Params[I] + '''.', clRed);
    end
    else
      WriteLnColored('''' + Params[I] + ''' not found.', clRed);
  end;
end;

procedure TCLICommandProcessor.CmdDir(const Params: TStringArray);
var
  Temp, LParent: String;
  Dir, Entry: Cardinal;
  Opt, Ptr: Integer;
  Ok: Boolean;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: dir <path>', clRed);
    Exit;
  end;

  Temp := Params[1];
  Ok := False;

  // Handle parent directory specifier at start
  if (Length(Temp) > 0) and (Temp[1] = '^') then
  begin
    if FContext.Image.Disc[FContext.CurrentDir].Parent >= 0 then
      Temp := FContext.Image.GetParent(FContext.Image.Disc[FContext.CurrentDir].Parent) +
              Copy(Temp, 2)
    else
      Temp := FContext.Image.GetParent(0) + Copy(Temp, 2);
  end;

  // Handle in-path parent specifiers (e.g. $.dir1.^.dir2)
  LParent := FContext.Image.GetDirSep(FContext.Image.Disc[FContext.CurrentDir].Partition) + '^';
  while Pos(LParent, Temp) > 1 do
  begin
    Ptr := Pos(LParent, Temp) - 1;
    while (Ptr > 1)
      and (Temp[Ptr] <> FContext.Image.GetDirSep(FContext.Image.Disc[FContext.CurrentDir].Partition)) do
      Dec(Ptr);
    if Ptr > 1 then
      Temp := LeftStr(Temp, Ptr - 1) + Copy(Temp, Pos(LParent, Temp) + Length(LParent));
    if Ptr = 1 then
      Temp := LeftStr(Temp, Ptr) + Copy(Temp, Pos(LParent, Temp) + Length(LParent));
  end;

  // Try to find the directory - use ValidFile which tries both raw path
  // and current-directory-relative path (matching GUI's ValidFile behavior)
  if ValidFile(Temp, Dir, Entry) then
  begin
    if Dir >= Cardinal(Length(FContext.Image.Disc)) then
    begin
      FContext.CurrentDir := 0; // Root
      Ok := True;
    end
    else if Dir < Cardinal(Length(FContext.Image.Disc)) then
    begin
      if Entry < Cardinal(Length(FContext.Image.Disc[Dir].Entries)) then
      begin
        if FContext.Image.Disc[Dir].Entries[Entry].DirRef >= 0 then
        begin
          FContext.CurrentDir := FContext.Image.Disc[Dir].Entries[Entry].DirRef;
          Ok := True;
        end
        else
        begin
          WriteLnColored('''' + Temp + ''' is a file.', clRed);
          Exit;
        end;
      end
      else
      begin
        FContext.CurrentDir := Dir;
        Ok := True;
      end;
    end;
  end;

  // Are we on DFS and we have a drive specifier?
  if FContext.Image.MajorFormatNumber = diAcornDFS then
  begin
    Opt := 0; // Default drive 0
    if Length(Temp) > 1 then
      if Temp[1] = ':' then Opt := StrToIntDef(Temp[2], 0);
    if FContext.Image.DoubleSided and (Opt = 2) then
      Opt := Length(FContext.Image.Disc) - 1;
    // We'll ignore anything after the drive specifier
    FContext.CurrentDir := Opt;
    Ok := True;
  end;

  // Report back to the user
  if Ok then
    WriteLn('Directory ''' + FContext.Image.GetParent(FContext.CurrentDir) + ''' selected.')
  else
    WriteLnColored('''' + Temp + ''' does not exist.', clRed);
end;

procedure TCLICommandProcessor.CmdExtract(const Params: TStringArray);
var
  Files: TSearchResults;
  I: Integer;
  Temp: String;
  Dir, Entry: Cardinal;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: extract <filename> [filename2] ...', clRed);
    Exit;
  end;

  Files := nil;
  for I := 1 to Length(Params) - 1 do
    Files := GetListOfFiles(Params[I], Files);

  if Length(Files) = 0 then
  begin
    WriteLnColored('No files found.', clRed);
    Exit;
  end;

  for I := 0 to Length(Files) - 1 do
  begin
    Temp := BuildFilename(Files[I]);
    if FContext.Image.FileExists(Temp, Dir, Entry) then
    begin
      Write('Extracting ' + Temp + ' ');
      if (Dir < Cardinal(Length(FContext.Image.Disc))) and
         (Entry < Cardinal(Length(FContext.Image.Disc[Dir].Entries))) then
        DownloadFile(Dir, Entry, '');
      if Dir > Cardinal(Length(FContext.Image.Disc)) then
      begin
        WriteColored('Cannot extract the root in this way. ', clRed);
        WriteLnColored('Try selecting the root and entering ''extract *''.', clRed);
      end;
    end;
  end;
end;

procedure TCLICommandProcessor.CmdFree(const Params: TStringArray);
begin
  if FContext.Image.FormatNumber = diInvalidImg then
    WriteLnColored('No image loaded.', clRed)
  else
    ReportFreeSpace;
end;

procedure TCLICommandProcessor.CmdInsert(const Params: TStringArray);
begin
  if not Confirm then
    Exit;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: insert <filename>', clRed);
    Exit;
  end;

  if not FileExists(Params[1]) then
  begin
    WriteLnColored('File not found.', clRed);
    Exit;
  end;

  WriteLn('Reading image...');
  if FContext.LoadImage(Params[1]) then
  begin
    WriteColored(FContext.Image.FormatString, clBold);
    WriteLn(' image read OK.');
    FContext.CurrentDir := 0;
    ReportFreeSpace;
  end
  else
    WriteLnColored('Image not read.', clRed);
end;

procedure TCLICommandProcessor.CmdNew(const Params: TStringArray);
var
  Format: String;
  Index: Integer;
  Ok, Known, NewMap: Boolean;
  HardDriveSize: Cardinal;
  DirType: Byte;
begin
  if not Confirm then
    Exit;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: new <format> [size]', clRed);
    Exit;
  end;

  Known := False;
  Ok := False;
  Format := UpperCase(Params[1]);
  if Length(Params) > 2 then
    Format := Format + UpperCase(Params[2]);

  // ADFS HDD
  if UpperCase(Format) = 'ADFSHDD' then
  begin
    NewMap := False;
    DirType := 0;
    HardDriveSize := 20 * 1024 * 1024;

    if Length(Params) > 3 then
    begin
      if Length(Params[3]) > 3 then
      begin
        if UpperCase(Params[3][1]) = 'N' then NewMap := True;
        if UpperCase(Params[3][2]) = 'N' then DirType := 1;
        if UpperCase(Params[3][2]) = 'B' then DirType := 2;
        if NewMap and (DirType = 0) then DirType := 1;
        if (not NewMap) and (DirType = 2) then DirType := 1;
        HardDriveSize := GetDriveSize(Params[3]);
        if HardDriveSize < 20 * 1024 * 1024 then HardDriveSize := 20 * 1024 * 1024;
        if HardDriveSize > 1000 * 1024 * 1024 then HardDriveSize := 1000 * 1024 * 1024;
        if (not NewMap) and (HardDriveSize > 512 * 1024 * 1024) then
          HardDriveSize := 512 * 1024 * 1024;
      end;
    end;

    Ok := FContext.CreateHDDImage(diAcornADFS, HardDriveSize, DirType, NewMap, True);
    Known := True;
  end;

  // AFS HDD
  if (not Known) and (UpperCase(Params[1]) = 'AFS') then
  begin
    if Length(Params) > 3 then
    begin
      HardDriveSize := GetDriveSize(Params[3]);
      DirType := StrToIntDef(RightStr(Params[2], 1), 2);
      if (DirType = 2) and (HardDriveSize < 400) then HardDriveSize := 400;
      if (DirType = 3) and (HardDriveSize < 640) then HardDriveSize := 640;
      if HardDriveSize > 512 * 1024 then HardDriveSize := 512 * 1024;
      Ok := FContext.CreateHDDImage(diAcornFS, HardDriveSize * 1024, DirType, False, True);
      Known := True;
    end
    else
    begin
      WriteLnColored('Not enough parameters.', clRed);
      Exit;
    end;
  end;

  // DOS HDD
  if (not Known) and (UpperCase(Format) = 'DOSHDD') then
  begin
    if Length(Params) > 3 then
    begin
      HardDriveSize := GetDriveSize(Params[3]);
      if HardDriveSize < 33300 then DirType := diFAT16 else DirType := diFAT32;
      if HardDriveSize < 20 * 1024 then HardDriveSize := 20 * 1024;
      if HardDriveSize > 1024 * 1024 then HardDriveSize := 512 * 1024;
      Ok := FContext.CreateHDDImage(diDOSPlus, HardDriveSize * 1024, DirType, False, True);
      Known := True;
    end
    else
    begin
      WriteLnColored('Not enough parameters.', clRed);
      Exit;
    end;
  end;

  // Amiga HDD
  if (not Known) and (UpperCase(Format) = 'AMIGAHDD') then
  begin
    if Length(Params) > 3 then
    begin
      HardDriveSize := GetDriveSize(Params[3]);
      if HardDriveSize < 20 * 1024 then HardDriveSize := 20 * 1024;
      if HardDriveSize > 1024 * 1024 then HardDriveSize := 512 * 1024;
      Ok := FContext.CreateHDDImage(diAmiga, HardDriveSize * 1024, 0, False, True);
      Known := True;
    end
    else
    begin
      WriteLnColored('Not enough parameters.', clRed);
      Exit;
    end;
  end;

  // Standard floppy formats
  if not Known then
  begin
    Index := Pos(Format, DiscFormats);
    if Index > 0 then
    begin
      Index := (Index div 8) + 1;
      if (Index >= Low(DiscNumber)) and (Index <= High(DiscNumber)) then
      begin
        Ok := FContext.CreateNewImage(
          DiscNumber[Index] div $100,
          (DiscNumber[Index] div $10) mod $10,
          DiscNumber[Index] mod $10);
        Known := True;
      end;
    end;
  end;

  if Ok then
  begin
    WriteLn(UpperCase(Params[1]) + ' image created OK.');
    ReportFreeSpace;
    FContext.CurrentDir := 0;
  end
  else
  begin
    if Known then
      WriteLnColored('Failed to create image.', clRed)
    else
      WriteLnColored('Unknown format.', clRed);
  end;
end;

procedure TCLICommandProcessor.CmdOpt(const Params: TStringArray);
var
  Opt, Partition: Integer;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: opt <none|load|run|exec> [partition]', clRed);
    Exit;
  end;

  Partition := FContext.Image.Disc[FContext.CurrentDir].Partition;
  if Length(Params) > 2 then
    Partition := StrToIntDef(Params[2], Partition);

  // Find option by name
  Opt := 0;
  while (LowerCase(Params[1]) <> BootOptions[Opt]) and (Opt < High(BootOptions)) do
    Inc(Opt);
  if LowerCase(Params[1]) <> BootOptions[Opt] then
    Opt := StrToIntDef(Params[1], -1);

  if (Opt >= 0) and (Opt <= High(BootOptions)) then
  begin
    Write('Update boot option to ' + UpperCase(BootOptions[Opt]) + ' ');
    if FContext.Image.UpdateBootOption(Opt, Partition) then
    begin
      WriteLnColored('success.', clGreen);
      FContext.HasChanged := True;
    end
    else
      WriteLnColored('failed.', clRed);
  end
  else
    WriteLnColored('Invalid boot option.', clRed);
end;

procedure TCLICommandProcessor.CmdRename(const Params: TStringArray);
var
  Dir, Entry: Cardinal;
  Temp, NewName: String;
  Result: Integer;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 3 then
  begin
    WriteLnColored('Usage: rename <oldname> <newname>', clRed);
    Exit;
  end;

  if ValidFile(Params[1], Dir, Entry) then
  begin
    Temp := FContext.Image.GetParent(FContext.CurrentDir) +
            FContext.Image.GetDirSep(FContext.Image.Disc[FContext.CurrentDir].Partition) +
            Params[1];
    if not FContext.Image.FileExists(Temp, Dir, Entry) then
      Temp := Params[1];

    NewName := Params[2];
    Write('Rename ' + Temp + ' to ' + NewName + ' ');
    Result := FContext.Image.RenameFile(Temp, NewName);
    if Result >= 0 then
    begin
      WriteLnColored('success.', clGreen);
      FContext.HasChanged := True;
    end
    else
      WriteLnColored('failed (' + IntToStr(Result) + ').', clRed);
  end
  else
    WriteLnColored('''' + Params[1] + ''' not found.', clRed);
end;

procedure TCLICommandProcessor.CmdSave(const Params: TStringArray);
var
  Filename: String;
  Compress: Boolean;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) > 1 then
    Filename := Params[1]
  else
    Filename := FContext.Filename;

  if Filename = '' then
  begin
    WriteLnColored('No filename specified.', clRed);
    Exit;
  end;

  Compress := False;
  if Length(Params) > 2 then
    Compress := UpperCase(Params[2]) = 'TRUE';

  if FContext.SaveImage(Filename, Compress) then
    WriteLn('Image saved OK.')
  else
    WriteLnColored('Image failed to save.', clRed);
end;

procedure TCLICommandProcessor.CmdTitle(const Params: TStringArray);
var
  Partition: Integer;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: title <newtitle> [partition]', clRed);
    Exit;
  end;

  Partition := FContext.Image.Disc[FContext.CurrentDir].Partition;
  if Length(Params) > 2 then
    Partition := StrToIntDef(Params[2], Partition);

  Write('Update disc title ');
  if FContext.Image.UpdateDiscTitle(Params[1], Partition) then
  begin
    WriteLnColored('success.', clGreen);
    FContext.HasChanged := True;
  end
  else
    WriteLnColored('failed.', clRed);
end;

procedure TCLICommandProcessor.CmdConfig(const Params: TStringArray);
var
  Index, Ptr: Integer;
  Ok: Boolean;
  Dir: Cardinal;
begin
  // If enough parameters, attempt to set a config value
  if Length(Params) > 2 then
  begin
    Ok := False;
    for Index := 0 to Length(Configs) - 1 do
      if UpperCase(Params[1]) = UpperCase(Configs[Index, 0]) then
      begin
        Ok := True;
        case Configs[Index, 1] of
          'B': if LowerCase(Params[2]) = 'true' then
                 FSettings.SetBool(Configs[Index, 0], True)
               else
                 FSettings.SetBool(Configs[Index, 0], False);
          'I':
            begin
              Dir := 0;
              if LowerCase(LeftStr(Params[2], 2)) = '0x' then
                Dir := StrToIntDef('$' + Copy(Params[2], 3), 0);
              if (Params[2][1] = '$') or (Params[2][1] = '&') then
                Dir := StrToIntDef('$' + Copy(Params[2], 2), 0);
              if Dir = 0 then Dir := StrToIntDef(Params[2], 0);
              FSettings.SetInt(Configs[Index, 0], Dir);
            end;
          'S': FSettings.SetString(Configs[Index, 0], Params[2]);
        end;
      end;
    if Ok then
      WriteLn('Configuration option set.')
    else
      WriteLnColored('Invalid configuration option.', clRed);
  end
  else
  begin
    // List available config options
    WriteLnColored('Valid configuration options', clBlue + clBold);
    WriteLn('Not all configurations are used by the console.');
    // Find the longest key name for alignment
    Ptr := 1;
    for Index := 0 to Length(Configs) - 1 do
      if Length(Configs[Index, 0]) > Ptr then Ptr := Length(Configs[Index, 0]);
    // Display the config options
    for Index := 0 to Length(Configs) - 1 do
    begin
      WriteColored(PadRight(Configs[Index, 0], Ptr), clRed + clBold);
      Write(': ');
      case Configs[Index, 1] of
        'B': WriteLnColored('True|False', clRed);
        'I': WriteLnColored('<Integer>', clRed);
        'S': WriteLnColored('<String>', clRed);
      end;
      if Configs[Index, 2] <> '' then
        WriteLn(StringOfChar(' ', Ptr + 2) + Configs[Index, 2]);
    end;
  end;
end;

procedure TCLICommandProcessor.CmdStatus(const Params: TStringArray);
var
  Index, Ptr: Integer;
begin
  WriteLnColored('Current configuration settings', clBlue + clBold);
  WriteLn('Not all configurations are used by the console.');
  // Find the longest key name for alignment
  Ptr := 1;
  for Index := 0 to Length(Configs) - 1 do
    if Length(Configs[Index, 0]) > Ptr then Ptr := Length(Configs[Index, 0]);
  // Display current settings
  for Index := 0 to Length(Configs) - 1 do
  begin
    WriteColored(PadRight(Configs[Index, 0], Ptr), clRed + clBold);
    Write(': ');
    if FSettings.KeyExists(Configs[Index, 0]) then
      case Configs[Index, 1] of
        'B': WriteLn(BoolToStr(FSettings.GetBool(Configs[Index, 0], False), 'True', 'False'));
        'I': WriteLn('0x' + IntToHex(FSettings.GetInt(Configs[Index, 0], 0), 4));
        'S': WriteLn(FSettings.GetString(Configs[Index, 0], ''));
      end
    else
      WriteLnColored('Not set', clRed);
  end;
end;

procedure TCLICommandProcessor.CmdSearch(const Params: TStringArray);
var
  Files: TSearchResults;
  I: Integer;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: search <pattern>', clRed);
    Exit;
  end;

  Files := nil;
  for I := 1 to Length(Params) - 1 do
    Files := GetListOfFiles(Params[I], Files);

  WriteLn(IntToStr(Length(Files)) + ' file(s) found.');

  for I := 0 to Length(Files) - 1 do
    WriteLn(BuildFilename(Files[I]));
end;

procedure TCLICommandProcessor.CmdDefrag(const Params: TStringArray);
begin
  // TODO: Implement defrag in CLI mode. The GUI's Defrag method relies on
  // TMainForm.ImportFiles which handles complex cross-format file re-importing
  // with directory creation, attribute mapping, and format conversion.
  // A CLI implementation would need to replicate this logic independently.
  // See: MainUnit.pas TMainForm.Defrag and TMainForm.ImportFiles
  WriteLnColored('Defrag/compact is not available in CLI mode.', clRed);
  WriteLn('Please use the GUI application for this operation.');
end;

procedure TCLICommandProcessor.CmdDirTitle(const Params: TStringArray);
var
  DirPath: String;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: dirtitle <newtitle>', clRed);
    Exit;
  end;

  DirPath := FContext.Image.GetParent(FContext.CurrentDir);
  Write('Retitle directory ' + DirPath + ' ');
  if FContext.Image.RetitleDirectory(DirPath, Params[1]) then
  begin
    WriteLnColored('success.', clGreen);
    FContext.HasChanged := True;
  end
  else
    WriteLnColored('failed.', clRed);
end;

procedure TCLICommandProcessor.CmdExecLoadType(const Params: TStringArray);
var
  Files: TSearchResults;
  I: Integer;
  Temp, CmdType: String;
  Ok: Boolean;
  Value: Cardinal;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 3 then
  begin
    WriteLnColored('Usage: ' + Params[0] + ' <filename> <value>', clRed);
    Exit;
  end;

  CmdType := LowerCase(Params[0]);

  // Validate hex number (matching GUI behavior - validates all three commands)
  if IntToHex(StrToIntDef('$' + Params[2], 0), 8) <>
     UpperCase(RightStr('00000000' + Params[2], 8)) then
  begin
    WriteLnColored('Invalid hex number.', clRed);
    Exit;
  end;

  Files := GetListOfFiles(Params[1]);
  if Length(Files) > 0 then
  begin
    for I := 0 to Length(Files) - 1 do
    begin
      Temp := BuildFilename(Files[I]);
      Ok := False;

      // Print the text and perform action
      if (CmdType = 'load') or (CmdType = 'exec') then
      begin
        Write('Change ' + CmdType + ' address for ' + Temp + ' to 0x' +
              IntToHex(StrToIntDef('$' + Params[2], 0), 8) + ' ');
      end;
      if CmdType = 'type' then
      begin
        Params[2] := RightStr('000' + Params[2], 3); // Ensure filetype is 12 bits
        Write('Change filetype for ' + Temp + ' to 0x' +
              IntToHex(StrToIntDef('$' + Params[2], 0), 3) + ' ');
      end;
      // Attempt to update details
      if CmdType = 'exec' then
        Ok := FContext.Image.UpdateExecAddr(Temp, StrToIntDef('$' + Params[2], 0));
      if CmdType = 'load' then
        Ok := FContext.Image.UpdateLoadAddr(Temp, StrToIntDef('$' + Params[2], 0));
      if CmdType = 'type' then
        Ok := FContext.Image.ChangeFileType(Temp, Params[2]);

      if Ok then
      begin
        FContext.HasChanged := True;
        WriteLnColored('success.', clGreen);
      end
      else
        WriteLnColored('failed.', clRed);
    end;
  end
  else
    WriteLnColored('No files found.', clRed);
end;

procedure TCLICommandProcessor.CmdFind(const Params: TStringArray);
type
  THostFile = record
    Filename: String;
    IsDirectory: Boolean;
  end;
var
  SearchList: TSearchRec;
  I, J: Integer;
  HostFiles: array of THostFile;
  Ok: Boolean;
  Temp: String;
begin
  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: find <pattern> [pattern2] ...', clRed);
    Exit;
  end;

  SetLength(HostFiles, 0);

  for I := 1 to Length(Params) - 1 do
  begin
    Ok := True;
    Temp := Params[I];

    // Check for exclusion prefix
    if (Length(Temp) > 0) and (Temp[1] = '|') then
    begin
      Ok := False;
      Temp := Copy(Temp, 2);
    end;

    if FindFirst(Temp, faAnyFile, SearchList) = 0 then
    begin
      repeat
        if (SearchList.Name <> '.') and (SearchList.Name <> '..') and
           (SearchList.Name <> '') then
        begin
          if Ok then
          begin
            // Add to list
            J := Length(HostFiles);
            SetLength(HostFiles, J + 1);
            HostFiles[J].Filename := ExtractFilePath(Temp) + SearchList.Name;
            HostFiles[J].IsDirectory := (SearchList.Attr and faDirectory) = faDirectory;
          end
          else
          begin
            // Remove from list
            Temp := ExtractFilePath(Temp) + SearchList.Name;
            for J := 0 to Length(HostFiles) - 1 do
              if (HostFiles[J].Filename = Temp) and
                 (HostFiles[J].IsDirectory = ((SearchList.Attr and faDirectory) = faDirectory)) then
                HostFiles[J].Filename := '';
          end;
        end;
      until FindNext(SearchList) <> 0;
      FindClose(SearchList);
    end;
  end;

  // Remove blank entries
  J := 0;
  while J < Length(HostFiles) do
  begin
    if HostFiles[J].Filename = '' then
    begin
      if J < Length(HostFiles) - 1 then
        for I := J to Length(HostFiles) - 2 do
          HostFiles[I] := HostFiles[I + 1];
      SetLength(HostFiles, Length(HostFiles) - 1);
      Dec(J);
    end;
    Inc(J);
  end;

  WriteLn(IntToStr(Length(HostFiles)) + ' entries found.');

  for J := 0 to Length(HostFiles) - 1 do
  begin
    if HostFiles[J].IsDirectory then
    begin
      WriteColored('Directory', clBlue);
      WriteLn(': ''' + HostFiles[J].Filename + '''.');
    end
    else
    begin
      WriteColored('File', clBlue);
      WriteLn(': ''' + HostFiles[J].Filename + '''.');
    end;
  end;
end;

procedure TCLICommandProcessor.CmdFileToCSV(const Params: TStringArray);
var
  SearchList: TSearchRec;
  I: Integer;
  FileList: TStringList;
  LImage: TDiscImage;
  CurrFile, CSVFilename: String;
begin
  if Length(Params) < 2 then
  begin
    WriteLnColored('Not enough parameters.', clRed);
    Exit;
  end;

  FileList := TStringList.Create;
  try
    for I := 1 to Length(Params) - 1 do
    begin
      if FindFirst(Params[I], faAnyFile, SearchList) = 0 then
      begin
        repeat
          if (SearchList.Name <> '.') and (SearchList.Name <> '..') then
            if (SearchList.Attr and faDirectory) <> faDirectory then
              if FileExists(ExtractFilePath(Params[I]) + SearchList.Name) then
                FileList.Add(ExtractFilePath(Params[I]) + SearchList.Name);
        until FindNext(SearchList) <> 0;
        FindClose(SearchList);
      end;
    end;

    WriteLn('Processing images.');
    if FileList.Count > 0 then
    begin
      for I := 0 to FileList.Count - 1 do
      begin
        CurrFile := FileList[I];
        // Calculate CSV output filename
        CSVFilename := LeftStr(CurrFile,
                       Length(CurrFile) - Length(ExtractFileExt(CurrFile))) + '.csv';
        // If this is the currently loaded image, use its object
        if (FContext.Image.FormatNumber <> diInvalidImg) and
           (CurrFile = FContext.Image.Filename) then
        begin
          WriteCSVForImage(FContext.Image, CSVFilename);
        end
        else
        begin
          // Load a new image
          LImage := TDiscImage.Create;
          try
            if LImage.LoadFromFile(CurrFile) then
              WriteCSVForImage(LImage, CSVFilename)
            else
              WriteLnColored('Could not load image: ' + CurrFile, clRed);
          finally
            LImage.Free;
          end;
        end;
      end;
    end
    else
      WriteLn('No images found.');
  finally
    FileList.Free;
  end;
end;

procedure TCLICommandProcessor.CmdFileType(const Params: TStringArray);
var
  TypeNum: Integer;
begin
  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: filetype <name|number>', clRed);
    Exit;
  end;

  // Check if name was passed (not a hex number)
  if IntToHex(StrToIntDef('$' + Params[1], 0), 3) <> UpperCase(Params[1]) then
  begin
    // Name passed - look up number
    TypeNum := FContext.Image.GetFileType(Params[1]);
    if TypeNum <> -1 then
      WriteLn('0x' + IntToHex(TypeNum, 3))
    else
      WriteLn('Unknown filetype');
  end
  else
  begin
    // Number passed - look up name
    WriteLn(FContext.Image.GetFileType(StrToInt('$' + Params[1])));
  end;
end;

procedure TCLICommandProcessor.CmdInterleave(const Params: TStringArray);
var
  Opt: Integer;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: interleave <auto|seq|int|mux|0-3>', clRed);
    Exit;
  end;

  // Check if format supports interleave changes
  if not ((FContext.Image.FormatNumber = diAcornADFS shl 4 + 2) or
          (FContext.Image.FormatNumber = diAcornADFS shl 4 + $E) or
          (FContext.Image.MajorFormatNumber = diAcornFS)) then
  begin
    WriteLnColored('Not possible in this format.', clRed);
    Exit;
  end;

  // Find option by name
  Opt := 0;
  while (LowerCase(Params[1]) <> Interleaves[Opt]) and (Opt < High(Interleaves)) do
    Inc(Opt);
  if LowerCase(Params[1]) <> Interleaves[Opt] then
    Opt := StrToIntDef(Params[1], -1);

  if (Opt >= 0) and (Opt <= High(Interleaves)) then
  begin
    if FContext.Image.ChangeInterleaveMethod(Opt) then
    begin
      FContext.HasChanged := True;
      WriteLn('Interleave changed to ' + UpperCase(Interleaves[Opt]) + '.');
    end
    else
      WriteLnColored('Failed to change interleave.', clRed);
  end
  else
    WriteLnColored('Invalid interleave option.', clRed);
end;

procedure TCLICommandProcessor.CmdList(const Params: TStringArray);
var
  Dir, Entry: Cardinal;
  Temp: String;
  Buffer: TDIByteArray;
  I: Integer;
  BasicLength: Cardinal;
  Ptr: Integer;
  IsBasic, IsText: Boolean;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: list <filename>', clRed);
    Exit;
  end;

  if not ValidFile(Params[1], Dir, Entry) then
  begin
    WriteLnColored('Cannot find file ''' + Params[1] + '''.', clRed);
    Exit;
  end;

  // Build full path
  Temp := FContext.Image.GetParent(FContext.CurrentDir) +
          FContext.Image.GetDirSep(FContext.Image.Disc[FContext.CurrentDir].Partition) +
          Params[1];
  if not FContext.Image.FileExists(Temp, Dir, Entry) then
    Temp := Params[1];

  if FContext.Image.ExtractFile(Temp, Buffer, Entry) then
  begin
    if Length(Buffer) = 0 then
    begin
      WriteLn('(empty file)');
      Exit;
    end;

    // Check if it's a text file (simple check)
    IsText := True;
    for I := 0 to Length(Buffer) - 1 do
      if ((Buffer[I] < 32) and (Buffer[I] <> 10) and (Buffer[I] <> 13) and (Buffer[I] <> 9)) or
         (Buffer[I] > 126) then
      begin
        IsText := False;
        Break;
      end;

    // Check if it's a BASIC file
    IsBasic := False;
    if (Length(Buffer) > 0) and (Buffer[0] = $0D) then
    begin
      IsBasic := True;
      BasicLength := Length(Buffer);
      Ptr := 0;
      while (Ptr + 3 < BasicLength) and IsBasic do
      begin
        if (Buffer[Ptr + 1] = $FF) and (Buffer[Ptr + 3] < 5) then
          BasicLength := Ptr + 1
        else
        begin
          Inc(Ptr, Buffer[Ptr + 3]);
          if Ptr < Length(Buffer) then
            if Buffer[Ptr] <> $0D then
              IsBasic := False;
        end;
      end;
    end;

    if IsBasic then
      WriteLnColored('BASIC file detected - use GUI for full decoding.', clYellow)
    else if IsText then
    begin
      // Display as text
      Temp := '';
      I := 0;
      while I < Length(Buffer) do
      begin
        if (Buffer[I] >= 32) and (Buffer[I] < 127) then
          Temp := Temp + Chr(Buffer[I])
        else if (Buffer[I] = 10) or (Buffer[I] = 13) then
        begin
          WriteLn(Temp);
          Temp := '';
          // Skip CR+LF or LF+CR pairs
          if (I + 1 < Length(Buffer)) and
             (((Buffer[I] = 10) and (Buffer[I + 1] = 13)) or
              ((Buffer[I] = 13) and (Buffer[I + 1] = 10))) then
            Inc(I);
        end
        else if Buffer[I] = 9 then
          Temp := Temp + '        ';
        Inc(I);
      end;
      if Temp <> '' then
        WriteLn(Temp);
    end
    else
      WriteLnColored('Binary file - cannot display as text.', clYellow);
  end
  else
    WriteLnColored('Failed to extract file.', clRed);
end;

procedure TCLICommandProcessor.CmdReport(const Params: TStringArray);
var
  LReport: TStringList;
  I, Dir, Entry, ErrorCount: Integer;
  Filename: String;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  // Get the image report (same method used by GUI)
  LReport := FContext.Image.ImageReport(False);
  try
    // Print the report lines
    if LReport.Count > 0 then
      for I := 0 to LReport.Count - 1 do
        WriteLn(LReport[I]);
  finally
    LReport.Free;
  end;

  // Add file report section (matching GUI's btn_ShowReportClick)
  WriteLn;
  WriteLn('File report');
  WriteLn('===========');
  if not FContext.Image.ScanSubDirs then
    WriteLn('Please note that not all directories may have been read in');
  ErrorCount := 0;
  if Length(FContext.Image.Disc) > 0 then
    for Dir := 0 to Length(FContext.Image.Disc) - 1 do
    begin
      // Check for broken directories
      if FContext.Image.Disc[Dir].Broken then
      begin
        WriteLn(FContext.Image.GetParent(Dir) + ' is broken');
        Inc(ErrorCount);
      end;
      // Check files for errors
      if Length(FContext.Image.Disc[Dir].Entries) > 0 then
        for Entry := 0 to Length(FContext.Image.Disc[Dir].Entries) - 1 do
        begin
          Filename := FContext.Image.GetParent(Dir)
                    + FContext.Image.GetDirSep(FContext.Image.Disc[Dir].Partition)
                    + FContext.Image.Disc[Dir].Entries[Entry].Filename;
          if FContext.Image.GetFileCRC(Filename, Entry) = 'error' then
          begin
            WriteLn(Filename + ' could not be read');
            Inc(ErrorCount);
          end;
        end;
    end;
  WriteLn(IntToStr(ErrorCount) + ' error(s) found');

  // Add footer
  WriteLn(StringOfChar('_', 80));
  WriteLn(ApplicationTitle + ' v' + ApplicationVersion);
  WriteLn('by Gerald J Holdsworth');
  WriteLn('gerald@geraldholdsworth.co.uk');
end;

procedure TCLICommandProcessor.CmdRunScript(const Params: TStringArray);
var
  ScriptFile: TextFile;
  Line: String;
  CmdArray: TStringArray;
begin
  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: runscript <filename>', clRed);
    Exit;
  end;

  if not FileExists(Params[1]) then
  begin
    WriteLnColored('Script file not found.', clRed);
    Exit;
  end;

  AssignFile(ScriptFile, Params[1]);
  try
    Reset(ScriptFile);
    while not EOF(ScriptFile) do
    begin
      ReadLn(ScriptFile, Line);
      Line := Trim(Line);
      // Skip empty lines and comments
      if (Line <> '') and (Line[1] <> '#') and (Line[1] <> ';') then
      begin
        WriteLn('> ' + Line);
        CmdArray := ParseInput(Line);
        if not ProcessCommand(CmdArray) then
          Break; // Exit command was issued
      end;
    end;
    CloseFile(ScriptFile);
  except
    on E: Exception do
      WriteLnColored('Error reading script: ' + E.Message, clRed);
  end;
end;

procedure TCLICommandProcessor.WriteCSVForImage(LImage: TDiscImage;
  const CSVFilename: String);
var
  F: TFileStream;
  Dir, Entry: Integer;
  Line: String;
  HexLen: Byte;
  Report: TStringList;
  IncDir, IncFilename, IncReport: Boolean;
  CSVParent, CSVFilenameCol, CSVLoadAddr, CSVExecAddr: Boolean;
  CSVLength, CSVAttributes, CSVAddress, CSVCRC32, CSVMD5: Boolean;

  procedure WriteCSVLine(const S: String);
  var
    LineBytes: String;
  begin
    LineBytes := S + LineEnding;
    F.Write(LineBytes[1], Length(LineBytes));
  end;

begin
  // Read CSV preference settings from registry (matching GUI behavior)
  IncDir       := FSettings.GetBool('CSVIncDir',      False);
  IncFilename  := FSettings.GetBool('CSVIncFilename',  True);
  IncReport    := FSettings.GetBool('CSVIncReport',    True);
  CSVParent    := FSettings.GetBool('CSVParent',       True);
  CSVFilenameCol := FSettings.GetBool('CSVFilename',   True);
  CSVLoadAddr  := FSettings.GetBool('CSVLoadAddr',     True);
  CSVExecAddr  := FSettings.GetBool('CSVExecAddr',     True);
  CSVLength    := FSettings.GetBool('CSVLength',       True);
  CSVAttributes := FSettings.GetBool('CSVAttributes',  True);
  CSVAddress   := FSettings.GetBool('CSVAddress',      False);
  CSVCRC32     := FSettings.GetBool('CSVCRC32',        True);
  CSVMD5       := FSettings.GetBool('CSVMD5',          False);

  HexLen := 8;
  if LImage.MajorFormatNumber = diAcornDFS then HexLen := 6;

  try
    F := TFileStream.Create(CSVFilename, fmCreate or fmShareDenyNone);
    try
      // Write image filename and CRC
      if IncFilename then
        WriteCSVLine(LImage.Filename.QuotedString('"') + ',"0x' + LImage.CRC32 + '"');
      // Write report
      if IncReport then
      begin
        Report := LImage.ImageReport(True);
        try
          if Report.Count > 0 then
            for Dir := 0 to Report.Count - 1 do
              WriteCSVLine(Report[Dir]);
        finally
          Report.Free;
        end;
      end;
      // Write column headers
      Line := '';
      if CSVParent     then Line := Line + '"Parent",';
      if CSVFilenameCol then Line := Line + '"Filename",';
      if CSVLoadAddr   then Line := Line + '"Load Address",';
      if CSVExecAddr   then Line := Line + '"Execution Address",';
      if CSVLength     then Line := Line + '"Length",';
      if CSVAttributes then Line := Line + '"Attributes",';
      if CSVAddress    then Line := Line + '"Address",';
      if CSVCRC32      then Line := Line + '"CRC32",';
      if CSVMD5        then Line := Line + '"MD-5",';
      Line := LeftStr(Line, Length(Line) - 1);
      WriteCSVLine(Line);
      // Write entries
      for Dir := 0 to Length(LImage.Disc) - 1 do
        for Entry := 0 to Length(LImage.Disc[Dir].Entries) - 1 do
          if (LImage.Disc[Dir].Entries[Entry].DirRef = -1) or IncDir then
          begin
            Line := '';
            if CSVParent then
              Line := Line + LImage.GetParent(Dir).QuotedString('"') + ',';
            if CSVFilenameCol then
              Line := Line + LImage.Disc[Dir].Entries[Entry].Filename.QuotedString('"') + ',';
            if CSVLoadAddr then
              Line := Line + '"0x' +
                      IntToHex(LImage.Disc[Dir].Entries[Entry].LoadAddr, HexLen) + '",';
            if CSVExecAddr then
              Line := Line + '"0x' +
                      IntToHex(LImage.Disc[Dir].Entries[Entry].ExecAddr, HexLen) + '",';
            if CSVLength then
              Line := Line + '"0x' +
                      IntToHex(LImage.Disc[Dir].Entries[Entry].Length, HexLen) + '",';
            if CSVAttributes then
              Line := Line + '"' + LImage.Disc[Dir].Entries[Entry].Attributes + '",';
            if CSVAddress then
              Line := Line + '"0x' +
                      IntToHex(LImage.Disc[Dir].Entries[Entry].Sector, HexLen) + '",';
            if CSVCRC32 then
              Line := Line + '"0x' + LImage.GetFileCRC(
                      LImage.GetParent(Dir)
                      + LImage.GetDirSep(LImage.Disc[Dir].Partition)
                      + LImage.Disc[Dir].Entries[Entry].Filename) + '",';
            if CSVMD5 then
              Line := Line + '"0x' + LImage.GetFileMD5(
                      LImage.GetParent(Dir)
                      + LImage.GetDirSep(LImage.Disc[Dir].Partition)
                      + LImage.Disc[Dir].Entries[Entry].Filename) + '",';
            Line := LeftStr(Line, Length(Line) - 1);
            WriteCSVLine(Line);
          end;
      // Write footer
      WriteCSVLine('""');
      WriteCSVLine('"' + ApplicationTitle + ' v' + ApplicationVersion + '",'
                   + '"by Gerald J Holdsworth",'
                   + '"gerald@geraldholdsworth.co.uk"');
    finally
      F.Free;
    end;
    WriteLn('CSV output for ' + CSVFilename + ' complete.');
  except
    on E: Exception do
      WriteLnColored('Failed to write file stream "' + CSVFilename
                     + '": ' + E.Message, clRed);
  end;
end;

procedure TCLICommandProcessor.CmdSaveCSV(const Params: TStringArray);
var
  Filename: String;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  // Get the filename
  if Length(Params) > 1 then
    Filename := Params[1]
  else
    Filename := FContext.Image.Filename;

  if Filename = '' then
  begin
    WriteLnColored('No filename specified.', clRed);
    Exit;
  end;

  // Ensure .csv extension
  Filename := LeftStr(Filename, Length(Filename) - Length(ExtractFileExt(Filename))) + '.csv';

  WriteCSVForImage(FContext.Image, Filename);
end;

procedure TCLICommandProcessor.CmdStamp(const Params: TStringArray);
var
  Files: TSearchResults;
  I: Integer;
  Temp: String;
begin
  if FContext.Image.FormatNumber = diInvalidImg then
  begin
    WriteLnColored('No image loaded.', clRed);
    Exit;
  end;

  if Length(Params) < 2 then
  begin
    WriteLnColored('Usage: stamp <filename> [filename2] ...', clRed);
    Exit;
  end;

  Files := GetListOfFiles(Params[1]);
  if Length(Files) > 0 then
  begin
    for I := 0 to Length(Files) - 1 do
    begin
      Temp := BuildFilename(Files[I]);
      Write('Setting date/time stamp for ' + Temp);
      if FContext.Image.TimeStampFile(Temp, Now) then
      begin
        FContext.HasChanged := True;
        WriteLnColored(' success.', clGreen);
      end
      else
        WriteLnColored(' failed.', clRed);
    end;
  end
  else
    WriteLnColored('No files found.', clRed);
end;

function TCLICommandProcessor.ProcessCommand(const Command: TStringArray): Boolean;
var
  Cmd: TStringArray;
begin
  Result := True; // Continue running

  if Length(Command) = 0 then
    Exit;

  // Copy command array so we can modify it
  Cmd := Copy(Command);

  // 'ls' command is the same as 'find *'
  if LowerCase(Cmd[0]) = 'ls' then
  begin
    SetLength(Cmd, 2);
    Cmd[0] := 'find';
    Cmd[1] := '*';
  end;

  case LowerCase(Cmd[0]) of
    'access':            CmdAccess(Cmd);
    'add':               CmdAdd(Cmd);
    'cat':               CmdCat(Cmd);
    'chdir':             if Length(Cmd) > 1 then SetCurrentDir(Cmd[1]);
    'compact', 'defrag': CmdDefrag(Cmd);
    'config':            CmdConfig(Cmd);
    'create':            CmdCreate(Cmd);
    'delete':            CmdDelete(Cmd);
    'dir':               CmdDir(Cmd);
    'dirtitle':          CmdDirTitle(Cmd);
    'exec', 'load', 'type': CmdExecLoadType(Cmd);
    'exit':              if Confirm then Result := False;
    'exittogui':         Result := False;
    'extract':           CmdExtract(Cmd);
    'filetocsv':         CmdFileToCSV(Cmd);
    'filetype':          CmdFileType(Cmd);
    'find':              CmdFind(Cmd);
    'free':              CmdFree(Cmd);
    'help':              ShowHelp;
    'insert':            CmdInsert(Cmd);
    'interleave':        CmdInterleave(Cmd);
    'join':              WriteLnColored('This command has not been implemented yet.', clRed);
    'list':              CmdList(Cmd);
    'new':               CmdNew(Cmd);
    'opt':               CmdOpt(Cmd);
    'rename':            CmdRename(Cmd);
    'report':            CmdReport(Cmd);
    'runscript':         CmdRunScript(Cmd);
    'save':              CmdSave(Cmd);
    'savecsv':           CmdSaveCSV(Cmd);
    'search':            CmdSearch(Cmd);
    'split':             WriteLnColored('This command has not been implemented yet.', clRed);
    'stamp':             CmdStamp(Cmd);
    'status':            CmdStatus(Cmd);
    'title':             CmdTitle(Cmd);
    '':                  ; // Ignore empty commands
  else
    WriteLnColored('Unknown command: ' + Cmd[0], clRed);
  end;
end;

function TCLICommandProcessor.ParseInput(const Input: String): TStringArray;
var
  Index, J: Integer;
begin
  // Split the string at each space, unless enclosed by quotes
  Result := Input.Split(' ', '"');

  // Remove blank entries
  if Length(Result) > 0 then
  begin
    Index := 0;
    while Index < Length(Result) do
    begin
      if (Result[Index] = '') or (Result[Index] = ' ') then
      begin
        if Index < Length(Result) - 1 then
          for J := Index + 1 to Length(Result) - 1 do
            Result[J - 1] := Result[J];
        SetLength(Result, Length(Result) - 1);
        Dec(Index);
      end;
      Inc(Index);
    end;
  end;

  // Remove quotes
  if Length(Result) > 0 then
    for Index := 0 to Length(Result) - 1 do
      Result[Index] := Result[Index].DeQuotedString('"')
  else
  begin
    SetLength(Result, 1);
    Result[0] := '';
  end;
end;

end.
