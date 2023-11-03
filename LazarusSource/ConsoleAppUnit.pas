unit ConsoleAppUnit;

interface

uses
 {$IFDEF Windows}Windows,{$ENDIF}
 Classes, SysUtils, CustApp, MainUnit, Forms;

type

 { TConsoleApp }

 TConsoleApp = class(TCustomApplication)
 public
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
  function UserInterface: Boolean;
  procedure ReadInput(var input: String);
 private
  ScriptOpen: Boolean;
  ScriptFile: TFileStream;
  function ProcessInput(Input: String): TStringArray;
 end;

function CheckConsole: Boolean;
var
 ConsoleApp: TConsoleApp;
 //Command line style modifiers
 cmdNormal,
 cmdBold,
 cmdItalic,
 cmdInverse,
 cmdRed,
 cmdGreen,
 cmdYellow,
 cmdBlue,
 cmdMagenta,
 cmdCyan       :String;
const
 //Command line font modifiers
 FcmdNormal = #$1B'[0m';
 FcmdBold   = #$1B'[1m';
 FcmdItalic = #$1B'[3m';
 FcmdInverse= #$1B'[7m';
 FcmdRed    = #$1B'[91m';
 FcmdGreen  = #$1B'[92m';
 FcmdYellow = #$1B'[93m';
 FcmdBlue   = #$1B'[94m';
 FcmdMagenta= #$1B'[95m';
 FcmdCyan   = #$1B'[96m';

implementation

{-------------------------------------------------------------------------------
Create the class instance
-------------------------------------------------------------------------------}
function CheckConsole: Boolean;
{$IFDEF Windows}
var
 hwConsole : hWnd;
 lwMode    : LongWord;
{$ENDIF}
begin
 Result:=False;
 //'console' passed as a parameter
 if Application.HasOption('c','console') then
 begin
  //Windows does not create a console for GUI applications, so we need to
  {$IFDEF Windows}
  //Blank the styles for older versions of Windows
  cmdNormal :='';
  cmdBold   :='';
  cmdItalic :='';
  cmdInverse:='';
  cmdRed    :='';
  cmdGreen  :='';
  cmdYellow :='';
  cmdBlue   :='';
  cmdMagenta:='';
  cmdCyan   :='';
  //Create the console
  AllocConsole;
  IsConsole:=True;
  SysInitStdIO;
  SetConsoleOutputCP(CP_UTF8);//So that the escape sequences will work
  //Try and enable virtual terminal processing
  hwConsole:=GetStdHandle(STD_OUTPUT_HANDLE);
  If GetConsoleMode(hwConsole,@lwMode)then
  begin
   lwMode:=lwMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
   if SetConsoleMode(hwConsole,lwMode)then
   begin
    {$ENDIF}
  //Set the styles for Windows that does support it, as well as macOS and Linux
  cmdNormal :=FcmdNormal;
  cmdBold   :=FcmdBold;
  cmdItalic :=FcmdItalic;//Ignored by Windows
  cmdInverse:=FcmdInverse;
  cmdRed    :=FcmdRed;
  cmdGreen  :=FcmdGreen;
  cmdYellow :=FcmdYellow;
  cmdBlue   :=FcmdBlue;
  cmdMagenta:=FcmdMagenta;
  cmdCyan   :=FcmdCyan;
    {$IFDEF Windows}
   end;
  end;
  {$ENDIF}
  //Create the console application
  ConsoleApp:=TConsoleApp.Create(nil);
  ConsoleApp.Title:=MainForm.ApplicationTitle+' Console';
  //Run the user interface
  Result:=ConsoleApp.UserInterface;
  //Close the console application
  ConsoleApp.Free;
  //Close the GUI application if not needed, otherwise open the GUI application
  if Result then Application.Terminate;
 end;
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
 input     : String;
 Lparams   : TStringArray;
 procedure OpenScript(script: String);
 begin
  if script<>'' then
   if ScriptOpen then
    WriteLn(cmdRed+'Script already running.'+cmdNormal)
   else
    if not FileExists(script) then
     WriteLn(cmdRed+
             'File '''+script+''' does not exist.'+cmdNormal)
    else
    begin
     WriteLn('Running script '''+script+'''.');
     //Open the script file
     ScriptFile:=TFileStream.Create(script,fmOpenRead or fmShareDenyNone);
     ScriptOpen:=True;
    end;
 end;
begin
 ScriptFile:=nil;
 ScriptOpen:=False;
 //Write out a header
 Write(cmdRed+cmdInverse);
 WriteLn(StringOfChar('*',80));
 Write(cmdNormal+cmdBold);
 Write(MainForm.ApplicationTitle+' Console V'+MainForm.ApplicationVersion);
 WriteLn(' by Gerald J Holdsworth');
 WriteLn();
 WriteLn(MainForm.platform+' '+MainForm.arch);
 WriteLn(cmdNormal);
 //Did the user supply a file for commands to run?
 OpenScript(Application.GetOptionValue('c','console'));
 //Intialise the array
 Lparams:=nil;
 WriteLn(cmdBold+'Ready'+cmdNormal);
 repeat
  //Prompt for input
  write('>');
  //Read a line of input from the user
  ReadInput(input);
  //Process the input
  Lparams:=ProcessInput(input);
  if Lparams[0]='runscript' then
   if Length(Lparams)>1 then
    OpenScript(Lparams[1]);
  //Parse the command
  MainForm.ParseCommand(Lparams);
  //End of the script? Then close the file
  if ScriptOpen then
   if ScriptFile.Position=ScriptFile.Size then
   begin
    ScriptFile.Free;
    ScriptOpen:=False;
   end;
  //Continue until the user specifies to exit
 until(Lparams[0]='exit')or(Lparams[0]='exittogui');
 //Script file still open? Then close it
 if ScriptOpen then ScriptFile.Free;
 //Footer at close of console
 Write(cmdRed+cmdInverse);
 Write(StringOfChar('*',80));
 WriteLn(cmdNormal);
 //Exit or not?
 Result:=LowerCase(Lparams[0])='exit';
end;

{-------------------------------------------------------------------------------
Get a line of input
-------------------------------------------------------------------------------}
procedure TConsoleApp.ReadInput(var input: String);
var
 B: Byte;
begin
 if not ScriptOpen then ReadLn(input)
 else
 begin //Or from the file
  input:='';
  B:=0;
  repeat
   if ScriptFile.Position<ScriptFile.Size then B:=ScriptFile.ReadByte; //Read byte by byte
   if(B>31)and(B<127)then input:=input+Chr(B); //Valid printable character?
  until(B=$0A)or(ScriptFile.Position=ScriptFile.Size); //End of line with $0A or end of file
  WriteLn(input); //Output the line, as if entered by the user
 end;
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

end.
