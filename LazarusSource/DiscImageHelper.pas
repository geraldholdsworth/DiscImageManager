unit DiscImageHelper;

{
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

{$mode ObjFPC}{$H+}

{$modeswitch TypeHelpers}

interface

uses
 Classes, SysUtils, DiscImage;

type
 //So we can open multiple images at once
 TDiscImages = array of TDiscImage;
 //Helper class for the above array
 TDIHelper = type helper for TDiscImages
  private
   procedure FreeUpElement(Index: Integer);
  public
   function Add(Index: Integer=-1;Image: TDiscImage=nil): Integer;
   function Count: Integer;
   function Last: TDiscImage;
   procedure Remove(Index: Integer=-1);
 end;

implementation

{-------------------------------------------------------------------------------
Free up an element, but don't reduce the array length
-------------------------------------------------------------------------------}
procedure TDIHelper.FreeUpElement(Index: Integer);
begin
 if(Index>=0)and(Index<Self.Count)then
  if Self[Index]<>nil then
  begin
   Self[Index].Free;
   Self[Index]:=nil;
  end;
end;

{-------------------------------------------------------------------------------
Add new entry to the array
-------------------------------------------------------------------------------}
function TDIHelper.Add(Index: Integer=-1;Image: TDiscImage=nil): Integer;
var
 I: Integer;
begin
 //If no index has been specified, then find an empty slot
 if(Index<0)or(Index>=Self.Count)then
 begin
  Index:=-1;
  if Self.Count>0 then for I:=0 to Self.Count-1 do if Self[I]=nil then Index:=I;
 end;
 //Still no index, add one to the end
 if Index<0 then
 begin
  SetLength(Self,Self.Count+1);
  Result:=Self.Count-1;
 end
 else Result:=Index;
 //So we have an index - make sure it is free.
 if(Result>=0)and(Result<Self.Count)then
  if Self[Result]<>nil then FreeUpElement(Result);//It isn't, so free it
 //Has an image been specified to clone?
 if Image<>nil then
  Self[Result]:=TDiscImage.Create
 else
  Self[Result]:=TDiscImage.Create(Image);
end;

{-------------------------------------------------------------------------------
Remove an entry from the array
-------------------------------------------------------------------------------}
procedure TDIHelper.Remove(Index: Integer=-1);
begin
 //Make sure that there is something to remove
 if Count>0 then
 begin
  //No index has been specified, so specify the last one
  if Index<0 then Index:=Self.Count-1;
  //Make sure the specified index is within range
  if Index<Length(Self) then
  begin
   //Free it
   FreeUpElement(Index);
   //Last one? Reduce the array length
   if Index=Count-1 then SetLength(Self,Index);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Counts the number of elements in the array
-------------------------------------------------------------------------------}
function TDIHelper.Count: Integer;
begin
 Result:=Length(Self);
end;

{-------------------------------------------------------------------------------
Returns the last, valid, element
-------------------------------------------------------------------------------}
function TDIHelper.Last: TDiscImage;
begin
 //By default, return a nil result
 Result:=nil;
 //Find the last, non removed, entry
 while(Result<>nil)and(Self.Count>0)do
  if Self.Count>0 then
  begin
   //Get the last element
   Result:=Self[Self.Count-1];
   //If the end element is nil, remove it
   if Result=nil then SetLength(Self,Self.Count-1);
  end;
end;

end.

