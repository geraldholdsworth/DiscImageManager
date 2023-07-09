program DiscImageManager;

{
This has grown beyond all my imagination.
}

{$MODE objFPC}{$H+}

uses
  Forms, datetimectrls,Interfaces,Global,
  Classes, SysUtils, CustApp,//For the console side of this
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
//  procedure ParseCommand(Command: TStringArray);
  function ProcessInput(Input: String): TStringArray;
 end;

{ TConsoleApp }

constructor TConsoleApp.Create(TheOwner: TComponent);
begin
 inherited Create(TheOwner);
 StopOnException:=True;
end;

destructor TConsoleApp.Destroy;
begin
 inherited Destroy;
end;

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

var
 ConsoleApp: TConsoleApp;
 B         : Byte;
 input,
 script    : String;
 params    : TStringArray;
 F         : TFileStream;
begin
 //Create GUI application
 Application.Scaled:=True;
 Application.Title:='Disc Image Manager';
//'Disc Image Manager';
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
 //Do we have 'console' passed as a parameter?
 input:=Application.CheckOptions('d:','console:');
 //No, we have something else so quit to the GUI
 if input<>'' then //This will also quit if 'console' was supplied, but there was other text too
 begin
  WriteLn(input); //Display the errors
  WriteLn('Exiting to GUI.');
 end;
 //No errors, and 'console' passed as a parameter
 if(input='')and(Application.HasOption('c','console'))then
 begin
  //Create the console application
  ConsoleApp:=TConsoleApp.Create(nil);
  ConsoleApp.Title:=MainForm.ApplicationTitle+' Console';
  //Write out a header
  WriteLn('********************************************************************************');
  WriteLn(MainForm.ApplicationTitle+' V'+MainForm.ApplicationVersion);
  WriteLn('');
  WriteLn('Entering Console');
  //Did the user supply a file for commands to run?
  script:=Application.GetOptionValue('c','console');
  if script<>'' then
   if not FileExists(script) then
   begin
    WriteLn('File '''+script+''' does not exist.');
    script:='';
   end
   else
   begin
    WriteLn('Running script '''+script+'''.');
    //Open the script file
    F:=TFileStream.Create(script,fmOpenRead or fmShareDenyNone);
   end;
  //Intialise the array
  params:=nil;
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
   params:=ConsoleApp.ProcessInput(input);
   //Parse the command
   MainForm.ParseCommand(params);
   //End of the script? Then close the file
   if script<>'' then
    if F.Position=F.Size then
    begin
     F.Free;
     script:='';
    end;
   //Continue until the user specifies to exit
  until(params[0]='exit')or(params[0]='exittogui');
  //Script file still open? Then close it
  if script<>'' then F.Free;
  //Footer at close of console
  WriteLn('********************************************************************************');
  //Close the console application
  ConsoleApp.Free;
  //Close the GUI application if not needed
  if params[0]='exit' then Application.Terminate
  else Application.Run //Otherwise open the GUI application
 end else Application.Run; //Console application not specified, so open as normal
end.
