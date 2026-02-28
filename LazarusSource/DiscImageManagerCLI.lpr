program DiscImageManagerCLI;

{
Disc Image Manager - Command Line Interface Version
This version does not require X server or GUI toolkits.

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

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  DiscImage, DiscImageContext, CLICommands;

type
  { TDiscImageManagerCLI }
  TDiscImageManagerCLI = class(TCustomApplication)
  private
    FProcessor: TCLICommandProcessor;
    FScriptFile: TFileStream;
    FScriptOpen: Boolean;
    procedure OpenScript(const ScriptName: String);
    procedure ReadInput(var Input: String);
    function DetectColorSupport: Boolean;
    function FindPositionalArg: String;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp;
  end;

{ TDiscImageManagerCLI }

constructor TDiscImageManagerCLI.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FProcessor := TCLICommandProcessor.Create;
  FScriptFile := nil;
  FScriptOpen := False;
end;

destructor TDiscImageManagerCLI.Destroy;
begin
  if FScriptOpen and Assigned(FScriptFile) then
    FScriptFile.Free;
  FProcessor.Free;
  inherited Destroy;
end;

procedure TDiscImageManagerCLI.OpenScript(const ScriptName: String);
begin
  if ScriptName = '' then
    Exit;

  if FScriptOpen then
  begin
    WriteLn('Script already running.');
    Exit;
  end;

  if not FileExists(ScriptName) then
  begin
    WriteLn('Script file ''' + ScriptName + ''' does not exist.');
    Exit;
  end;

  WriteLn('Running script ''' + ScriptName + '''.');
  FScriptFile := TFileStream.Create(ScriptName, fmOpenRead or fmShareDenyNone);
  FScriptOpen := True;
end;

procedure TDiscImageManagerCLI.ReadInput(var Input: String);
var
  B: Byte;
begin
  if not FScriptOpen then
    ReadLn(Input)
  else
  begin
    Input := '';
    B := 0;
    repeat
      if FScriptFile.Position < FScriptFile.Size then
        B := FScriptFile.ReadByte;
      if (B > 31) and (B < 127) then
        Input := Input + Chr(B);
    until (B = $0A) or (FScriptFile.Position = FScriptFile.Size);
    WriteLn(Input);
  end;
end;

function TDiscImageManagerCLI.DetectColorSupport: Boolean;
var
  Term: String;
begin
  Result := False;
  {$IFDEF UNIX}
  Term := GetEnvironmentVariable('TERM');
  Result := (Term <> '') and (Pos('color', LowerCase(Term)) > 0) or
            (Pos('xterm', LowerCase(Term)) > 0) or
            (Pos('linux', LowerCase(Term)) > 0) or
            (Pos('ansi', LowerCase(Term)) > 0);
  {$ENDIF}
  {$IFDEF WINDOWS}
  // Windows 10+ supports ANSI colors
  Result := True;
  {$ENDIF}
end;

function TDiscImageManagerCLI.FindPositionalArg: String;
const
  // Options that take a value argument
  OptionsWithValue = 'es';
var
  I: Integer;
  Param: String;
begin
  Result := '';
  I := 1;
  while I <= ParamCount do
  begin
    Param := ParamStr(I);
    if (Length(Param) > 1) and (Param[1] = '-') then
    begin
      if (Length(Param) > 2) and (Param[2] = '-') then
        // Long option (--script, --execute, etc.): skip it and its value
        Inc(I, 2)
      else
      begin
        // Short option (-s, -n, etc.)
        if Pos(Param[2], OptionsWithValue) > 0 then
          Inc(I, 2) // Option takes a value, skip both
        else
          Inc(I);   // Flag-only option (-n, -h), skip just it
      end;
    end
    else
    begin
      // Not an option — this is a positional argument
      Result := Param;
      Exit;
    end;
  end;
end;

procedure TDiscImageManagerCLI.DoRun;
var
  Input: String;
  CmdParams: TStringArray;
  Running: Boolean;
begin
  // Check for help flag
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  // Check for non-interactive mode (single command)
  if HasOption('e', 'execute') then
  begin
    Input := GetOptionValue('e', 'execute');
    CmdParams := FProcessor.ParseInput(Input);
    FProcessor.ProcessCommand(CmdParams);
    Terminate;
    Exit;
  end;

  // Detect color support
  FProcessor.UseColors := DetectColorSupport and not HasOption('n', 'no-color');

  // Print header
  if FProcessor.UseColors then
    Write(clRed + clBold + StringOfChar('*', FProcessor.ConsoleWidth) + clNormal + LineEnding)
  else
    WriteLn(StringOfChar('*', FProcessor.ConsoleWidth));

  WriteLn(ApplicationTitle + ' CLI V' + ApplicationVersion);
  WriteLn('by Gerald J Holdsworth');
  WriteLn;
  {$IFDEF UNIX}
  WriteLn('Platform: ' + {$I %FPCTARGETOS%} + ' ' + {$I %FPCTARGETCPU%});
  {$ENDIF}
  WriteLn('Type ''help'' for available commands.');
  WriteLn;

  // Check for script file
  if HasOption('s', 'script') then
    OpenScript(GetOptionValue('s', 'script'));

  // Check for initial image file (positional argument, not an option value)
  Input := FindPositionalArg;
  if (Input <> '') and FileExists(Input) then
  begin
    WriteLn('Loading: ' + Input);
    if FProcessor.Context.LoadImage(Input) then
      WriteLn('Image loaded: ' + FProcessor.Context.Image.FormatString)
    else
      WriteLn('Failed to load image.');
  end;

  WriteLn('Ready');

  // Main command loop
  Running := True;
  while Running do
  begin
    // Prompt
    if FProcessor.Context.Image.FormatNumber <> diInvalidImg then
    begin
      if FProcessor.Context.HasChanged then
        Write('*');
      Write('[' + FProcessor.Context.Image.GetParent(FProcessor.Context.CurrentDir) + ']');
    end;
    Write('>');

    // Read input
    ReadInput(Input);

    // Parse and process
    CmdParams := FProcessor.ParseInput(Input);
    Running := FProcessor.ProcessCommand(CmdParams);

    // Check for end of script
    if FScriptOpen then
    begin
      if FScriptFile.Position = FScriptFile.Size then
      begin
        FScriptFile.Free;
        FScriptFile := nil;
        FScriptOpen := False;
      end;
    end;
  end;

  // Footer
  if FProcessor.UseColors then
    Write(clRed + clBold + StringOfChar('*', FProcessor.ConsoleWidth) + clNormal + LineEnding)
  else
    WriteLn(StringOfChar('*', FProcessor.ConsoleWidth));

  Terminate;
end;

procedure TDiscImageManagerCLI.WriteHelp;
begin
  WriteLn('Disc Image Manager CLI - Command-line disc image management tool');
  WriteLn;
  WriteLn('Usage: ', ExeName, ' [options] [image-file]');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -h, --help              Show this help message');
  WriteLn('  -e, --execute <cmd>     Execute a single command and exit');
  WriteLn('  -s, --script <file>     Run commands from a script file');
  WriteLn('  -n, --no-color          Disable colored output');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  ', ExeName, ' mydisk.ssd            Open mydisk.ssd');
  WriteLn('  ', ExeName, ' -e "new ADFSS"        Create new ADFS S image');
  WriteLn('  ', ExeName, ' -s commands.txt       Run script file');
  WriteLn;
  WriteLn('For interactive command help, type ''help'' at the prompt.');
end;

var
  Application: TDiscImageManagerCLI;

begin
  Application := TDiscImageManagerCLI.Create(nil);
  Application.Title := 'Disc Image Manager CLI';
  Application.Run;
  Application.Free;
end.
