program DiscImageManager;

{
This has grown beyond all my imagination.
}

{$MODE objFPC}{$H+}

uses
  Forms, datetimectrls,Interfaces,Global,
  Classes, SysUtils, CustApp,//For the console side of this
 {$IFDEF Windows}Windows,{$ENDIF}
  MainUnit in 'MainUnit.pas',
  DiscImage in 'DiscImage.pas',
  AboutUnit in 'AboutUnit.pas',
  NewImageUnit in 'NewImageUnit.pas',
  ProgressUnit in 'ProgressUnit.pas',
  ImageDetailUnit in 'ImageDetailUnit.pas',
  HexDumpUnit in 'HexDumpUnit.pas',
  SearchUnit in 'SearchUnit.pas',
  CustomDialogueUnit in 'CustomDialogueUnit.pas',
  HardDriveUnit in 'HardDriveUnit.pas',
  ErrorLogUnit in 'ErrorLogUnit.pas',
  SettingsUnit in 'SettinsUnit.pas',
  ImportSelectorUnit in 'ImportSelectorUnit.pas',
  PWordEditorUnit in 'PWordEditorUnit.pas',
  AFSPartitionUnit in 'AFSParitionUnit.pas',
  ChangeInterleaveUnit in 'ChangeInterleaveUnit.pas', GJHRegistryClass,
  CSVPrefUnit in 'CSVPrefUnit.pas',
  ImageReportUnit in 'ImageReportUnit.pas';

{$R *.res}

type

 { TConsoleApp }

 TConsoleApp = class(TCustomApplication)
 public
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
  function UserInterface: Boolean;
 private
  function ProcessInput(Input: String): TStringArray;
 end;

{-------------------------------------------------------------------------------
Create the class instance
-------------------------------------------------------------------------------}
constructor TConsoleApp.Create(TheOwner: TComponent);
begin
 inherited Create(TheOwner);
 StopOnException:=True;
end;

{-------------------------------------------------------------------------------
Destroy the class instance
-------------------------------------------------------------------------------}
destructor TConsoleApp.Destroy;
begin
 inherited Destroy;
end;

{-------------------------------------------------------------------------------
The user interface (this passes the actual code back to the GUI unit)
-------------------------------------------------------------------------------}
function TConsoleApp.UserInterface: Boolean;
var
 B         : Byte;
 input,
 script    : String;
 Lparams   : TStringArray;
 F         : TFileStream;
begin
 F:=nil;
 //Write out a header
 Write(MainForm.cmdRed+MainForm.cmdInverse);
 WriteLn(StringOfChar('*',80));
 Write(MainForm.cmdNormal+MainForm.cmdBold);
 Write(MainForm.ApplicationTitle+' Console V'+MainForm.ApplicationVersion);
 WriteLn(' by Gerald J Holdsworth');
 WriteLn();
 WriteLn(MainForm.platform+' '+MainForm.arch);
 WriteLn(MainForm.cmdNormal);
 //Did the user supply a file for commands to run?
 script:=Application.GetOptionValue('c','console');
 if script<>'' then
  if not FileExists(script) then
  begin
   WriteLn(MainForm.cmdRed+
           'File '''+script+''' does not exist.'+MainForm.cmdNormal);
   script:='';
  end
  else
  begin
   WriteLn('Running script '''+script+'''.');
   //Open the script file
   F:=TFileStream.Create(script,fmOpenRead or fmShareDenyNone);
  end;
 //Intialise the array
 Lparams:=nil;
 WriteLn(MainForm.cmdBold+'Ready'+MainForm.cmdNormal);
 repeat
  //Prompt for input
  write('>');
  //Read a line of input from the user
  if script='' then ReadLn(input)
  else
  begin //Or from the file
   input:='';
   B:=0;
   repeat
    if F.Position<F.Size then B:=F.ReadByte; //Read byte by byte
    if(B>31)and(B<127)then input:=input+Chr(B); //Valid printable character?
   until(B=$0A)or(F.Position=F.Size); //End of line with $0A or end of file
   WriteLn(input); //Output the line, as if entered by the user
  end;
  Lparams:=ProcessInput(input);
  //Parse the command
  MainForm.ParseCommand(Lparams);
  //End of the script? Then close the file
  if script<>'' then
   if F.Position=F.Size then
   begin
    F.Free;
    script:='';
   end;
  //Continue until the user specifies to exit
 until(Lparams[0]='exit')or(Lparams[0]='exittogui');
 //Script file still open? Then close it
 if script<>'' then F.Free;
 //Footer at close of console
 Write(MainForm.cmdRed+MainForm.cmdInverse);
 Write(StringOfChar('*',80));
 WriteLn(MainForm.cmdNormal);
 //Exit or not?
 Result:=LowerCase(Lparams[0])='exit';
end;

{-------------------------------------------------------------------------------
Process the input string
-------------------------------------------------------------------------------}
function TConsoleApp.ProcessInput(Input: String): TStringArray;
var
 Index,
 j     : Integer;
 tmp   : PChar;
begin
 //Split the string at each space, unless enclosed by quotes
 Result:=Input.Split(' ','"');
 //Anything entered?
 if Length(Result)>0 then
 begin //Remove any blank entries
  Index:=0;
  while Index<Length(Result) do
  begin
   if(Result[Index]='')or(Result[Index]=' ') then
   begin
    if Index<Length(Result)-1 then
     for j:=Index+1 to Length(Result)-1 do
      Result[j-1]:=Result[j];
    SetLength(Result,Length(Result)-1);
    dec(Index);
   end;
   inc(Index);
  end;
 end;
 if Length(Result)>0 then
  //Remove the quotes
  for Index:=0 to Length(Result)-1 do
  begin
   tmp:=PChar(Result[Index]);
   Result[Index]:=AnsiExtractQuotedStr(tmp,'"');
  end
 else //Input was empty, so create a blank entry
 begin
  SetLength(Result,1);
  Result[0]:='';
 end;
end;

{-------------------------------------------------------------------------------
Main program execution starts here
-------------------------------------------------------------------------------}
var
 ConsoleApp: TConsoleApp;
 exit      : Boolean;
begin
 //Create GUI application
 RequireDerivedFormResource:=True;
 Application.Scaled:=True;
 Application.Title:='Disc Image Manager';
 Application.Initialize;
 Application.CreateForm(TMainForm, MainForm);
 Application.CreateForm(TAboutForm, AboutForm);
 Application.CreateForm(TNewImageForm, NewImageForm);
 Application.CreateForm(TProgressForm, ProgressForm);
 Application.CreateForm(TImageDetailForm, ImageDetailForm);
 Application.CreateForm(TSearchForm, SearchForm);
 Application.CreateForm(TCustomDialogue, CustomDialogue);
 Application.CreateForm(THardDriveForm, HardDriveForm);
 Application.CreateForm(TErrorLogForm, ErrorLogForm);
 Application.CreateForm(TSettingsForm, SettingsForm);
 Application.CreateForm(TImportSelectorForm, ImportSelectorForm);
 Application.CreateForm(TPwordEditorForm, PwordEditorForm);
 Application.CreateForm(TAFSPartitionForm, AFSPartitionForm);
 Application.CreateForm(TChangeInterleaveForm, ChangeInterleaveForm);
 Application.CreateForm(TCSVPrefForm, CSVPrefForm);
 Application.CreateForm(TImageReportForm, ImageReportForm);
 {$IFDEF Windows}
 if Application.HasOption('c','console') then
 begin
  AllocConsole;
  IsConsole:=True;
  SysInitStdIO;
 end;
 {$ENDIF}
 //'console' passed as a parameter
 if Application.HasOption('c','console') then
 begin
  //Create the console application
  ConsoleApp:=TConsoleApp.Create(nil);
  ConsoleApp.Title:=MainForm.ApplicationTitle+' Console';
  //Run the user interface
  exit:=ConsoleApp.UserInterface;
  //Close the console application
  ConsoleApp.Free;
  //Close the GUI application if not needed, otherwise open the GUI application
  if exit then Application.Terminate else
  begin //Otherwise open the GUI application
   {$IFDEF Windows}
   IsConsole:=False;
   {$ENDIF}
   Application.Run;
  end;
 end else
 begin
  {$IFDEF Windows}
  IsConsole:=False;
  {$ENDIF}
  Application.Run; //Open as normal
 end;
end.
