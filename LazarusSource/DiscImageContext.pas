unit DiscImageContext;

{
Disc Image Context - Shared context for disc image operations.
This unit provides a common interface used by both CLI and GUI applications,
allowing the disc image processing library to be used without GUI dependencies.

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
  Classes, SysUtils, DiscImage;

type
  { TDiscImageContext - Shared context for disc image operations }
  TDiscImageContext = class
  private
    FImage: TDiscImage;
    FHasChanged: Boolean;
    FCurrentDir: Integer;
    FFilename: String;
    // Settings
    FCreateINF: Boolean;
    FAddImpliedAttributes: Boolean;
    FHideDEL: Boolean;
    FScanSubDirs: Boolean;
    FOpenDOS: Boolean;
    FCreateDSC: Boolean;
    FDFSZeroSecs: Boolean;
    FDFSBeyondEdge: Boolean;
    FDFSAllowBlank: Boolean;
    FSparkIsFS: Boolean;
    FADFSInterleave: Byte;
    procedure ApplySettings;
  public
    constructor Create;
    destructor Destroy; override;

    // Image operations
    function LoadImage(const AFilename: String): Boolean;
    function SaveImage(const AFilename: String; Uncompress: Boolean = False): Boolean;
    procedure CloseImage;
    function CreateNewImage(Major: Word; Minor: Byte; Tracks: Byte = 0): Boolean;
    function CreateHDDImage(Major: Word; Size: Cardinal; DirType: Byte = 0;
                            NewMap: Boolean = False; IDE: Boolean = False): Boolean;

    // Properties
    property Image: TDiscImage read FImage;
    property HasChanged: Boolean read FHasChanged write FHasChanged;
    property CurrentDir: Integer read FCurrentDir write FCurrentDir;
    property Filename: String read FFilename;

    // Settings
    property CreateINF: Boolean read FCreateINF write FCreateINF;
    property AddImpliedAttributes: Boolean read FAddImpliedAttributes write FAddImpliedAttributes;
    property HideDEL: Boolean read FHideDEL write FHideDEL;
    property ScanSubDirs: Boolean read FScanSubDirs write FScanSubDirs;
    property OpenDOS: Boolean read FOpenDOS write FOpenDOS;
    property CreateDSC: Boolean read FCreateDSC write FCreateDSC;
    property DFSZeroSecs: Boolean read FDFSZeroSecs write FDFSZeroSecs;
    property DFSBeyondEdge: Boolean read FDFSBeyondEdge write FDFSBeyondEdge;
    property DFSAllowBlank: Boolean read FDFSAllowBlank write FDFSAllowBlank;
    property SparkIsFS: Boolean read FSparkIsFS write FSparkIsFS;
    property ADFSInterleave: Byte read FADFSInterleave write FADFSInterleave;
  end;

  { TRegistrySettings - Cross-platform settings storage }
  TRegistrySettings = class
  private
    FSettings: TStringList;
    FFilename: String;
    FModified: Boolean;
  public
    constructor Create(const AAppName: String);
    destructor Destroy; override;

    function GetBool(const Key: String; Default: Boolean = False): Boolean;
    procedure SetBool(const Key: String; Value: Boolean);
    function GetInt(const Key: String; Default: Integer = 0): Integer;
    procedure SetInt(const Key: String; Value: Integer);
    function GetString(const Key: String; const Default: String = ''): String;
    procedure SetString(const Key: String; const Value: String);
    function KeyExists(const Key: String): Boolean;

    procedure Save;
    procedure Load;
  end;

// Application constants
const
  ApplicationTitle   = 'Disc Image Manager';
  ApplicationVersion = '1.49.2';

// Utility function
function ConvertToKMG(size: Int64): String;

implementation

{-------------------------------------------------------------------------------
Convert a size to a human readable format (KB, MB, GB)
-------------------------------------------------------------------------------}
function ConvertToKMG(size: Int64): String;
begin
  if size < 1024 then
    Result := IntToStr(size) + ' B'
  else if size < 1024 * 1024 then
    Result := FormatFloat('0.##', size / 1024) + ' KB'
  else if size < 1024 * 1024 * 1024 then
    Result := FormatFloat('0.##', size / (1024 * 1024)) + ' MB'
  else
    Result := FormatFloat('0.##', size / (1024 * 1024 * 1024)) + ' GB';
end;

{ TDiscImageContext }

constructor TDiscImageContext.Create;
begin
  inherited Create;
  FImage := TDiscImage.Create;
  FHasChanged := False;
  FCurrentDir := 0;
  FFilename := '';
  // Default settings
  FCreateINF := True;
  FAddImpliedAttributes := True;
  FHideDEL := True;
  FScanSubDirs := True;
  FOpenDOS := True;
  FCreateDSC := False;
  FDFSZeroSecs := False;
  FDFSBeyondEdge := True;
  FDFSAllowBlank := False;
  FSparkIsFS := True;
  FADFSInterleave := 0;
  ApplySettings;
end;

destructor TDiscImageContext.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure TDiscImageContext.ApplySettings;
begin
  if Assigned(FImage) then
  begin
    FImage.AddImpliedAttributes := FAddImpliedAttributes;
    FImage.ScanSubDirs := FScanSubDirs;
    FImage.OpenDOSPartitions := FOpenDOS;
    FImage.CreateDSC := FCreateDSC;
    FImage.AllowDFSZeroSectors := FDFSZeroSecs;
    FImage.DFSBeyondEdge := FDFSBeyondEdge;
    FImage.DFSAllowBlanks := FDFSAllowBlank;
    FImage.SparkAsFS := FSparkIsFS;
    FImage.InterleaveMethod := FADFSInterleave;
  end;
end;

function TDiscImageContext.LoadImage(const AFilename: String): Boolean;
begin
  Result := False;
  if FileExists(AFilename) then
  begin
    ApplySettings;
    Result := FImage.LoadFromFile(AFilename);
    if Result then
    begin
      FFilename := AFilename;
      FHasChanged := False;
      FCurrentDir := 0;
    end;
  end;
end;

function TDiscImageContext.SaveImage(const AFilename: String; Uncompress: Boolean): Boolean;
begin
  Result := FImage.SaveToFile(AFilename, Uncompress);
  if Result then
  begin
    FFilename := AFilename;
    FHasChanged := False;
  end;
end;

procedure TDiscImageContext.CloseImage;
begin
  FImage.Close;
  FFilename := '';
  FHasChanged := False;
  FCurrentDir := 0;
end;

function TDiscImageContext.CreateNewImage(Major: Word; Minor: Byte; Tracks: Byte): Boolean;
begin
  ApplySettings;
  Result := FImage.FormatFDD(Major, Minor, Tracks);
  if Result then
  begin
    FFilename := '';
    FHasChanged := True;
    FCurrentDir := 0;
  end;
end;

function TDiscImageContext.CreateHDDImage(Major: Word; Size: Cardinal; DirType: Byte;
                                          NewMap: Boolean; IDE: Boolean): Boolean;
begin
  ApplySettings;
  Result := FImage.FormatHDD(Major, Size, IDE, NewMap, DirType, False);
  if Result then
  begin
    FFilename := '';
    FHasChanged := True;
    FCurrentDir := 0;
  end;
end;

{ TRegistrySettings }

constructor TRegistrySettings.Create(const AAppName: String);
begin
  inherited Create;
  FSettings := TStringList.Create;
  FSettings.Duplicates := dupIgnore;
  FSettings.Sorted := True;
  FModified := False;

  // Determine settings file location
  {$IFDEF UNIX}
  FFilename := GetUserDir + '.' + LowerCase(AAppName) + '.conf';
  {$ELSE}
  FFilename := GetAppConfigDir(False) + AAppName + '.conf';
  {$ENDIF}

  Load;
end;

destructor TRegistrySettings.Destroy;
begin
  if FModified then
    Save;
  FSettings.Free;
  inherited Destroy;
end;

function TRegistrySettings.GetBool(const Key: String; Default: Boolean): Boolean;
var
  Value: String;
begin
  Value := FSettings.Values[Key];
  if Value = '' then
    Result := Default
  else
    Result := (LowerCase(Value) = 'true') or (Value = '1');
end;

procedure TRegistrySettings.SetBool(const Key: String; Value: Boolean);
begin
  if Value then
    FSettings.Values[Key] := 'true'
  else
    FSettings.Values[Key] := 'false';
  FModified := True;
end;

function TRegistrySettings.GetInt(const Key: String; Default: Integer): Integer;
var
  Value: String;
begin
  Value := FSettings.Values[Key];
  if Value = '' then
    Result := Default
  else
    Result := StrToIntDef(Value, Default);
end;

procedure TRegistrySettings.SetInt(const Key: String; Value: Integer);
begin
  FSettings.Values[Key] := IntToStr(Value);
  FModified := True;
end;

function TRegistrySettings.GetString(const Key: String; const Default: String): String;
begin
  Result := FSettings.Values[Key];
  if Result = '' then
    Result := Default;
end;

procedure TRegistrySettings.SetString(const Key: String; const Value: String);
begin
  FSettings.Values[Key] := Value;
  FModified := True;
end;

function TRegistrySettings.KeyExists(const Key: String): Boolean;
begin
  Result := FSettings.IndexOfName(Key) >= 0;
end;

procedure TRegistrySettings.Save;
var
  Dir: String;
begin
  try
    Dir := ExtractFilePath(FFilename);
    if (Dir <> '') and not DirectoryExists(Dir) then
      ForceDirectories(Dir);
    FSettings.SaveToFile(FFilename);
    FModified := False;
  except
    // Silently ignore save errors
  end;
end;

procedure TRegistrySettings.Load;
begin
  try
    if FileExists(FFilename) then
      FSettings.LoadFromFile(FFilename);
  except
    // Silently ignore load errors
  end;
end;

end.
