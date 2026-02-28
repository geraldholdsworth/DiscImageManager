unit Comparers;

{$mode Delphi}

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
  Classes, SysUtils, ComCtrls;

function CompareItems(Item1, Item2: TListItem; ListView: TListView): integer;
function CompareValues(Value1, Value2: string): integer;
function TryStrToFileBytes(const S: ansistring; out Value: integer): boolean;

implementation

function CompareItems(Item1, Item2: TListItem; ListView: TListView): integer;
var
  column: longint;
begin
  column := ListView.SortColumn - 1;

  if column = -1 then
    Result := CompareValues(Item1.Caption, Item2.Caption)
  else
    Result := CompareValues(Item1.SubItems[column], Item2.SubItems[column]);

  if ListView.SortDirection = sdDescending then Result := -Result;
end;

function CompareValues(Value1, Value2: string): integer;
var
  Date1, Date2: TDateTime;
  Float1, Float2: double;
  Size1, Size2: integer;
begin
  if TryStrToFileBytes(Value1, Size1) and TryStrToFileBytes(Value2, Size2) then
    Result := Trunc(Size1 - Size2)
  else
  if TryStrToDateTime(Value1, Date1) and TryStrToDateTime(Value2, Date2) then
    Result := Trunc(Date1 - Date2)
  else
  if TryStrToFloat(Value1, Float1) and TryStrToFloat(Value2, Float2) then
    Result := Trunc(Float1 - Float2)
  else
    Result := CompareText(Value1, Value2);
end;

function TryStrToFileBytes(const S: ansistring; out Value: integer): boolean;
var
  Parts: array of string;
  NumValue: integer;
begin
  Result := False;
  Parts := S.Split(' ');
  if High(Parts) = 1 then
  begin
    if not TryStrToInt(Parts[0], NumValue) then exit;
    if Parts[1] = 'bytes' then
    begin
      Value := NumValue;
      Result := True;
    end;
    if Parts[1] = 'KB' then
    begin
      Value := NumValue * 1024;
      Result := True;
    end;
    if Parts[1] = 'MB' then
    begin
      Value := NumValue * 1024 * 1024;
      Result := True;
    end;
  end;
end;

end.
