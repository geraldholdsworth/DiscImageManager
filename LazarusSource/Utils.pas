unit Utils;

{$MODE Delphi}

{
  Disk Image Manager -  Utility functions

  Copyright (c) Damien Guard. All rights reserved.
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
}

interface

uses
  Classes, Graphics, LCLIntf, SysUtils;

const
  BytesPerKB: integer = 1024;
  Power2: array[1..17] of integer = (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536);

type
  TSpinBorderStyle = (bsRaised, bsLowered, bsNone);

function StrInt(I: integer): string;
function StrHex(I: integer): string;
function IntStr(S: string): integer;
function StrBlockClean(S: array of byte; Start, Len: integer): string;
function StrYesNo(IsEmpty: boolean): string;
function StrInByteArray(ByteArray: array of byte; SubString: string; Start: integer): boolean;
function StrBufPos(ByteArray: array of byte; SubString: string): integer;

function CompareBlock(A: array of char; B: string): boolean;
function CompareBlockStart(A: array of char; B: string; Start: integer): boolean;
function CompareBlockInsensitive(A: array of char; B: string): boolean;

function FontToDescription(ThisFont: TFont): string;
function FontFromDescription(Description: string): TFont;
function FontHumanReadable(ThisFont: TFont): string;
function FontCopy(ThisFont: TFont): TFont;

function StrFileSize(Size: integer): string;

procedure DrawBorder(Canvas: TCanvas; var Rect: TRect; BorderStyle: TSpinBorderStyle);

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
begin
  Val(S, Result);
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
  AChar, BChar: Char;
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
  Result.Size := IntStr(StringReplace(Break[1], 'pt', '', [rfReplaceAll]));
  if (Break[1] = 'Bold') then
    Result.Style := Result.Style + [fsBold];
  if (Break[2] = 'Italic') then
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

function StrInByteArray(ByteArray: array of byte; SubString: string; Start: integer): boolean;
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

end.
