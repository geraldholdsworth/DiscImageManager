unit UtilsCLI;

{$MODE Delphi}

{
CLI-compatible utility functions.
This unit provides a subset of Utils.pas functionality without GUI dependencies.

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
  Classes, SysUtils;

const
  BytesPerKB: integer = 1024;
  Power2: array[1..17] of integer = (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536);

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

function StrFileSize(Size: integer): string;

implementation

function StrInt(I: integer): string;
begin
  Result := IntToStr(I);
end;

function StrHex(I: integer): string;
begin
  Result := IntToHex(I, 8);
end;

function IntStr(S: string): integer;
begin
  Result := StrToIntDef(S, 0);
end;

function StrBlockClean(S: array of byte; Start, Len: integer): string;
var
  Idx: integer;
begin
  Result := '';
  for Idx := Start to Start + Len - 1 do
    if (S[Idx] >= 32) and (S[Idx] < 127) then
      Result := Result + Chr(S[Idx]);
end;

function StrYesNo(IsEmpty: boolean): string;
begin
  if IsEmpty then
    Result := 'Yes'
  else
    Result := 'No';
end;

function StrInByteArray(ByteArray: array of byte; SubString: string; Start: integer): boolean;
var
  Idx: integer;
begin
  Result := True;
  for Idx := 1 to Length(SubString) do
    if ByteArray[Start + Idx - 1] <> Ord(SubString[Idx]) then
    begin
      Result := False;
      Exit;
    end;
end;

function StrBufPos(ByteArray: array of byte; SubString: string): integer;
var
  Idx: integer;
begin
  Result := -1;
  for Idx := Low(ByteArray) to High(ByteArray) - Length(SubString) + 1 do
    if StrInByteArray(ByteArray, SubString, Idx) then
    begin
      Result := Idx;
      Exit;
    end;
end;

function CompareBlock(A: array of char; B: string): boolean;
var
  Idx: integer;
begin
  Result := True;
  for Idx := 1 to Length(B) do
    if A[Idx - 1] <> B[Idx] then
    begin
      Result := False;
      Exit;
    end;
end;

function CompareBlockStart(A: array of char; B: string; Start: integer): boolean;
var
  Idx: integer;
begin
  Result := True;
  for Idx := 1 to Length(B) do
    if A[Start + Idx - 1] <> B[Idx] then
    begin
      Result := False;
      Exit;
    end;
end;

function CompareBlockInsensitive(A: array of char; B: string): boolean;
var
  Idx: integer;
begin
  Result := True;
  for Idx := 1 to Length(B) do
    if UpCase(A[Idx - 1]) <> UpCase(B[Idx]) then
    begin
      Result := False;
      Exit;
    end;
end;

function StrFileSize(Size: integer): string;
begin
  if Size < BytesPerKB then
    Result := IntToStr(Size) + 'b'
  else if Size < BytesPerKB * BytesPerKB then
    Result := IntToStr(Size div BytesPerKB) + 'K'
  else
    Result := IntToStr(Size div (BytesPerKB * BytesPerKB)) + 'M';
end;

end.
