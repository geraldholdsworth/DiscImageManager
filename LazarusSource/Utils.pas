unit Utils;

{$MODE Delphi}

{
Copyright (c) 2002-2025 Damien Guard.  

Originally from Disk Image Manager https://github.com/damieng/diskimagemanager
Relicensed for this project under GNU GPL with permission.

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

interface

uses
  Classes, Graphics, LCLIntf, SysUtils, ComCtrls{, CommCtrl};

const
  BytesPerKB: integer = 1024;
  Power2: array[1..17] of integer =
    (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536);
  LVSCW_AUTOSIZE_BESTFIT = -3;

type
  TSpinBorderStyle = (bsRaised, bsLowered, bsNone);
  TDiskByteArray = array of byte;

function StrInt(I: integer): string;
function StrHex(I: integer): string;
function IntStr(S: string): integer;
function StrBlockClean(S: array of byte; Start, Len: integer): string;
function StrYesNo(IsEmpty: boolean): string;
function StrInByteArray(ByteArray: array of byte; SubString: string;
  Start: integer): boolean;
function StrBufPos(ByteArray: array of byte; SubString: string): integer;

function CompareBlock(A: array of char; B: string): boolean;
function CompareBlockStart(A: array of char; B: string; Start: integer): boolean;
function CompareBlockInsensitive(A: array of char; B: string): boolean;

function FontToDescription(ThisFont: TFont): string;
function FontFromDescription(Description: string): TFont;
function FontHumanReadable(ThisFont: TFont): string;
function FontCopy(ThisFont: TFont): TFont;

function BlockShiftToBlockSize(BlockShift: byte): integer;
function StrFileSize(Size: integer): string;
function CompareByLength(List: TStringList; Index1, Index2: integer): integer;

procedure DrawBorder(Canvas: TCanvas; var Rect: TRect; BorderStyle: TSpinBorderStyle);
procedure AutoResizeListView(const ListView: TListView;
  const Mode: integer = LVSCW_AUTOSIZE_BESTFIT);

implementation

// Get integer as a decimal string
function StrInt(I: integer): string;
begin
  Str(I, Result);
end;

// Get integer as a hex string
function StrHex(I: integer): string;
begin
  Result := Format('%.2x', [I]);
end;

// Get string as an integer
function IntStr(S: string): integer;
var
  Code: integer;
begin
  Val(S, Result, Code);
  if Code <> 0 then Result := 0;
end;

// Extract ASCII string from a char array
function StrBlockClean(S: array of byte; Start, Len: integer): string;
var
  Idx: integer;
begin
  Result := '';
  for Idx := Start to Start + Len - 1 do
    if S[Idx] > 31 then
      if S[Idx] < 128 then
        Result := Result + Chr(S[Idx])
      else
        Result := Result + Chr(S[Idx] - 128);
end;

// Compare two char arrays
function CompareBlock(A: array of char; B: string): boolean;
var
  Idx: integer;
begin
  Result := True;
  Idx := 0;
  while Result and (Idx < Length(B) - 1) do
  begin
    if A[Idx] <> B[Idx + 1] then
      Result := False;
    Inc(Idx);
  end;
end;

// Compare two char arrays
function CompareBlockStart(A: array of char; B: string; Start: integer): boolean;
var
  Idx: integer;
begin
  Result := True;
  Idx := 0;
  while Result and (Idx < Length(B) - 1) do
  begin
    if A[Idx + Start] <> B[Idx + 1] then
      Result := False;
    Inc(Idx);
  end;
end;

// Compare two char arrays case insensitively
function CompareBlockInsensitive(A: array of char; B: string): boolean;
var
  Idx: integer;
  AChar, BChar: char;
begin
  Result := True;
  Idx := 0;
  while Result and (Idx < Length(B) - 1) do
  begin
    AChar := UpCase(A[Idx]);
    BChar := UpCase(B[Idx + 1]);
    if AChar <> BChar then
      Result := False;
    Inc(Idx);
  end;
end;

// Draw a windows style 3D border
procedure DrawBorder(Canvas: TCanvas; var Rect: TRect; BorderStyle: TSpinBorderStyle);
var
  OTL, ITL, OBR, IBR: TColor;
begin
  case BorderStyle of
    bsLowered:
    begin
      OTL := clBtnShadow;
      ITL := cl3DDkShadow;
      IBR := cl3DLight;
      OBR := clBtnHighlight;
    end;

    bsRaised:
    begin
      OBR := clBtnShadow;
      IBR := cl3DDkShadow;
      OTL := clBtnHighlight;
      ITL := cl3DLight;
    end;

    else
      exit;
  end;

  with Canvas do
  begin
    Dec(Rect.Bottom);
    Dec(Rect.Right);
    Pen.Color := OTL;
    MoveTo(Rect.Left, Rect.Bottom);
    LineTo(Rect.Left, Rect.Top);
    LineTo(Rect.Right, Rect.Top);

    Pen.Color := OBR;
    LineTo(Rect.Right, Rect.Bottom);
    LineTo(Rect.Left, Rect.Bottom);
    InflateRect(Rect, -1, -1);

    Pen.Color := ITL;
    MoveTo(Rect.Left, Rect.Bottom);
    LineTo(Rect.Left, Rect.Top);
    LineTo(Rect.Right, Rect.Top);

    Pen.Color := IBR;
    LineTo(Rect.Right, Rect.Bottom);
    LineTo(Rect.Left, Rect.Bottom);
    Inc(Rect.Top);
    Inc(Rect.Left);
  end;
end;

// Convert a font into a textual description
function FontToDescription(ThisFont: TFont): string;
begin
  Result := ThisFont.Name + ',' + StrInt(ThisFont.Size) + 'pt,';
  if (fsBold in ThisFont.Style) then
    Result := Result + 'Bold';
  Result := Result + ',';
  if (fsItalic in ThisFont.Style) then
    Result := Result + 'Italic';
end;

// Create a font from a textual description
function FontFromDescription(Description: string): TFont;
var
  Break: TStringList;
begin
  Break := TStringList.Create;
  Break.Delimiter := ',';
  Break.DelimitedText := StringReplace(Description, ' ', '_', [rfReplaceAll]);
  Result := TFont.Create;
  Result.Name := StringReplace(Break[0], '_', ' ', [rfReplaceAll]);
  if Break.Count > 1 then
    Result.Size := IntStr(StringReplace(Break[1], 'pt', '', [rfReplaceAll]));
  if (Break.Count > 2) and (Break[2] = 'Bold') then
    Result.Style := Result.Style + [fsBold];
  if (Break.Count > 3) and (Break[3] = 'Italic') then
    Result.Style := Result.Style + [fsItalic];
  Break.Free;
end;

// Copy a font
function FontCopy(ThisFont: TFont): TFont;
begin
  Result := TFont.Create;
  with Result do
  begin
    Name := ThisFont.Name;
    Style := ThisFont.Style;
    Size := ThisFont.Size;
    Color := ThisFont.Color;
  end;
end;

// Font as human readable description
function FontHumanReadable(ThisFont: TFont): string;
begin
  Result := Trim(StringReplace(FontToDescription(ThisFont), ',', ' ', [rfReplaceAll]));
end;

function StrYesNo(IsEmpty: boolean): string;
begin
  if IsEmpty then
    Result := 'Yes'
  else
    Result := 'No';
end;

function StrBufPos(ByteArray: array of byte; SubString: string): integer;
var
  BIdx, SIdx, Last: integer;
begin
  Last := Length(ByteArray) - Length(SubString);
  Result := -1;

  for BIdx := 0 to Last do
  begin
    Result := BIdx;
    for SIdx := 1 to Length(SubString) do
    begin
      if ByteArray[BIdx + SIdx - 1] <> byte(SubString[SIdx]) then
      begin
        Result := -1;
        break;
      end;
    end;
    if Result <> -1 then break;
  end;
end;

function StrInByteArray(ByteArray: array of byte; SubString: string;
  Start: integer): boolean;
var
  Idx, Last: integer;
begin
  Result := True;
  Idx := 0;
  Last := Length(ByteArray) - Length(SubString) - 1;

  while (Result) and (Start + Idx < Last) and (Idx < Length(SubString)) do
  begin
    if ByteArray[Start + Idx] <> byte(SubString[Idx + 1]) then
      Result := False;
    Inc(Idx);
  end;
end;

function BlockShiftToBlockSize(BlockShift: byte): integer;
begin
  Result := 2 << (BlockShift + 6);
end;

function StrFileSize(Size: integer): string;
const
  Megabyte: integer = 1024 * 1024;
begin
  if Size < 1024 then
    Result := Format('%d bytes', [Size])
  else
  if Size < Megabyte then
    Result := Format('%d KB', [Size div 1024])
  else
    Result := Format('%d MB', [Size div Megabyte]);
end;

function CompareByLength(List: TStringList; Index1, Index2: integer): integer;
begin
  Result := Length(List[Index2]) - Length(List[Index1]);  // Longest first
  if Result = 0 then
    Result := CompareText(List[Index1], List[Index2]);  // Alphabetical if same length
end;

procedure AutoResizeColumn(const Column: TListColumn;
  const Mode: integer = LVSCW_AUTOSIZE_BESTFIT);
var
  Width: integer;
begin
{  case Mode of
    LVSCW_AUTOSIZE_BESTFIT:
    begin // Calculate thw widest of data or header and use that
      Column.Width := LVSCW_AUTOSIZE;
      Width := Column.Width;
      Column.Width := LVSCW_AUTOSIZE_USEHEADER;
      if Width > Column.Width then
        Column.Width := LVSCW_AUTOSIZE;
    end;

    LVSCW_AUTOSIZE: Column.Width := LVSCW_AUTOSIZE;
    LVSCW_AUTOSIZE_USEHEADER: Column.Width := LVSCW_AUTOSIZE_USEHEADER;
  end;}
end;

procedure AutoResizeListView(const ListView: TListView;
  const Mode: integer = LVSCW_AUTOSIZE_BESTFIT);
var
  i: integer;
begin
  for i := 0 to ListView.Columns.Count - 1 do
    AutoResizeColumn(ListView.Columns[i], Mode);
end;

end.
