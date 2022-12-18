unit GJHRegistryClass;

{
TGJHRegistry class V1.00
Copyright (C) 2022 Gerald Holdsworth gerald@hollypops.co.uk

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

interface

uses
 Classes, Registry;

{$M+}

type
  TGJHRegistry = class
  private
   FRegistry : TRegistry;
   FRegKey   : String;
   procedure OpenReg(key: String);
   function ExtractKey(var V: String):String;
  published
   constructor Create(RegKey: String);
   function DeleteKey(key: String): Boolean;
   function GetRegValS(V: String;D: String): String;
   procedure GetRegValA(V: String;var D: array of Byte);
   function GetRegValI(V: String;D: Cardinal): Cardinal;
   function GetRegValB(V: String;D: Boolean): Boolean;
   procedure SetRegValS(V: String;D: String);
   procedure SetRegValA(V: String;var D: array of Byte);
   procedure SetRegValI(V: String;D: Cardinal);
   procedure SetRegValB(V: String;D: Boolean);
   function DoesKeyExist(V: String):Boolean;
  public
   destructor Destroy; override;
  end;

implementation

{-------------------------------------------------------------------------------
Class creator - initialises the global variables
-------------------------------------------------------------------------------}
constructor TGJHRegistry.Create(RegKey: String);
begin
 inherited Create;
 FRegKey:=RegKey;
end;

{-------------------------------------------------------------------------------
Class destructor
-------------------------------------------------------------------------------}
destructor TGJHRegistry.Destroy;
begin
 inherited;
end;

{-------------------------------------------------------------------------------
Open the registry key
-------------------------------------------------------------------------------}
procedure TGJHRegistry.OpenReg(key: String);
begin
 FRegistry:=TRegistry.Create;
 if key<>'' then key:='\'+key;
 FRegistry.OpenKey(FRegKey+key,true);
end;

{-------------------------------------------------------------------------------
Function to delete a key from the registry
-------------------------------------------------------------------------------}
function TGJHRegistry.DeleteKey(key: String): Boolean;
var
 x: Boolean;
begin
 x:=True;
 OpenReg(ExtractKey(key));
 if FRegistry.ValueExists(key) then x:=FRegistry.DeleteValue(key);
 FRegistry.Free;
 Result:=x;
end;

{-------------------------------------------------------------------------------
Function to read a string from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function TGJHRegistry.GetRegValS(V: String;D: String): String;
var
 X: String;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadString(V)
 else begin X:=D;FRegistry.WriteString(V,X);end;
 FRegistry.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to read an array from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
procedure TGJHRegistry.GetRegValA(V: String;var D: array of Byte);
var
 s: Integer;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then
 begin
  s:=FRegistry.GetDataSize(V);
  FRegistry.ReadBinaryData(V,D,s);
 end
 else
 begin
  FRegistry.WriteBinaryData(V,D,SizeOf(D));
 end;
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to read an integer from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function TGJHRegistry.GetRegValI(V: String;D: Cardinal): Cardinal;
var
 X: Cardinal;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadInteger(V)
 else begin X:=D;FRegistry.WriteInteger(V,X);end;
 FRegistry.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to read a boolean from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function TGJHRegistry.GetRegValB(V: String;D: Boolean): Boolean;
var
 X: Boolean;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadBool(V)
 else begin X:=D;FRegistry.WriteBool(V,X);end;
 FRegistry.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Does the specified key exist?
-------------------------------------------------------------------------------}
function TGJHRegistry.DoesKeyExist(V: String):Boolean;
begin
 OpenReg(ExtractKey(V));
 Result:=FRegistry.ValueExists(V);
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to save a string to the registry
-------------------------------------------------------------------------------}
procedure TGJHRegistry.SetRegValS(V: String;D: String);
begin
 OpenReg(ExtractKey(V));
 FRegistry.WriteString(V,D);
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to save an array to the registry
-------------------------------------------------------------------------------}
procedure TGJHRegistry.SetRegValA(V: String;var D: array of Byte);
begin
 OpenReg(ExtractKey(V));
 FRegistry.WriteBinaryData(V,D,SizeOf(D));
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to save an integer to the registry
-------------------------------------------------------------------------------}
procedure TGJHRegistry.SetRegValI(V: String;D: Cardinal);
begin
 OpenReg(ExtractKey(V));
 FRegistry.WriteInteger(V,D);
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to save a boolean to the registry
-------------------------------------------------------------------------------}
procedure TGJHRegistry.SetRegValB(V: String;D: Boolean);
begin
 OpenReg(ExtractKey(V));
 FRegistry.WriteBool(V,D);
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to extract key part of string
-------------------------------------------------------------------------------}
function TGJHRegistry.ExtractKey(var V: String):String;
begin
 Result:='';
 if Pos('\',V)>0 then
 begin
  Result:=Copy(V,1,Pos('\',V)-1);
  V:=Copy(V,Pos('\',V)+1);
 end;
end;

end.

