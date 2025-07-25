unit Comparers;

{$mode Delphi}

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
begin
  Result := False;
  Parts := S.Split(' ');
  if High(Parts) = 1 then
  begin
    if Parts[1] = 'bytes' then
    begin
      Value := StrToInt(Parts[0]);
      Result := True;
    end;
    if Parts[1] = 'KB' then
    begin
      Value := StrToInt(Parts[0]) * 1024;
      Result := True;
    end;
    if Parts[1] = 'MB' then
    begin
      Value := StrToInt(Parts[0]) * 1024 * 1024;
      Result := True;
    end;
  end;
end;

end.
