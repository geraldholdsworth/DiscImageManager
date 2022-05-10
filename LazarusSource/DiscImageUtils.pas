unit DiscImageUtils;

{
DiscImageUtils V1.44 - part of TDiscImage class

Copyright (C) 2018-2022 Gerald Holdsworth gerald@hollypops.co.uk

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

{$mode objfpc}{$H+}

interface

uses
 SysUtils,StrUtils;

type
//Define the TDIByteArray - saves using the System.Types unit for TByteDynArray
 TDIByteArray = array of Byte;
//Define the records to hold the catalogue
 TDirEntry     = record     //Not all fields are used on all formats
  Parent,                   //Complete path for parent directory (ALL)
  Filename,                 //Filename (ALL)
  ShortFilename,            //Long Filename (DOS)
  Attributes,               //File attributes (ADFS/DFS/D64/D71/D81/AmigaDOS)
  Filetype,                 //Full name filetype (ADFS/D64/D71/D81)
  ShortFileType: String;    //Filetype shortname (ADFS/D64/D71/D81)
  LoadAddr,                 //Load Address (ADFS/DFS)
  ExecAddr,                 //Execution Address (ADFS/DFS)
  Length,                   //Total length (ALL)
  Side,                     //Side of disc of location of data (DFS)
  Track,                    //Track of location of data (D64/D71/D81)
  DataFile,                 //Reserved for use by Repton Map Display
  ImageAddress: Cardinal;   //Reserved for use by Repton Map Display
  Sector,                   //Sector of disc of location of data (DFS/D64/D71/D81/AmigaDOS file)
                            //Sector of disc of location of header (AmigaDOS directory)
                            //Address of location of data (ADFS S/M/L/D)
                            //Indirect disc address of data (ADFS E/F/E+/F+)
  DirRef      : Integer;    //Reference to directory, if directory (ADFS/AmigaDOS)
  TimeStamp   : TDateTime;  //Timestamp (ADFS D/E/E+/F/F+)
  EOR         : Byte;       //Reserved for use by Repton Map Display
  isDOSPart   : Boolean;    //This file is the DOS partition
 end;
 type
//Define the records for an Acorn File Server password file
  TUserAccount = record
   Username,
   Password   : String;
   FreeSpace  : Cardinal;
   System,
   Locked     : Boolean;
   BootOption,
   AccessLevel: Byte;
 end;
 TUserAccounts  =array of TUserAccount;
 TSearchResults =array of TDirEntry;
 //General purpose procedures
 procedure ResetDirEntry(var Entry: TDirEntry);
 procedure RemoveTopBit(var title: String);
 function AddTopBit(title:String):String;
 procedure BBCtoWin(var f: String);
 procedure WintoBBC(var f: String);
 procedure RemoveSpaces(var s: String);
 procedure RemoveControl(var s: String);
 function IsBitSet(v,b: Integer): Boolean;
 function BreakDownInf(s: String): TStringArray;
 function FilenameToASCII(s: String): String;
 function GetAttributes(attr: String;format: Byte): String;
 function CompareString(S, mask: string; case_sensitive: Boolean): Boolean;
 //Some constants
 const
  diAcornDFS   = $000;
  diAcornADFS  = $001;
  diCommodore  = $002;
  diSinclair   = $003;
  diAmiga      = $004;
  diAcornUEF   = $005;
  diMMFS       = $006;
  diAcornFS    = $007;
  diSpark      = $008;
  diSJMDFS     = $009;
  diDOSPlus    = $00A;
  diInvalidImg = $00FF; //Needs to be changed to $FFFF
  diADFSOldMap = $00;
  diADFSNewMap = $01;
  diAmigaOFS   = $02;
  diAmigaFFS   = $03;
  diMaster512  = $01;
  diFAT12      = $12;
  diFAT16      = $16;
  diFAT32      = $32;
  diADFSOldDir = $00;
  diADFSNewDir = $01;
  diADFSBigDir = $02;
  diAmigaDir   = $10;
  diAmigaCache = $11;
  diUnknownDir = $FF;
implementation

{------------------------------------------------------------------------------}
//Reset a TDirEntry to blank
{------------------------------------------------------------------------------}
procedure ResetDirEntry(var Entry: TDirEntry);
begin
 with Entry do
 begin
  Parent       :='';
  Filename     :='';
  ShortFilename:='';
  Attributes   :='';
  Filetype     :='';
  ShortFiletype:='';
  LoadAddr     :=$0000;
  ExecAddr     :=$0000;
  Length       :=$0000;
  Side         :=$0000;
  Track        :=$0000;
  DataFile     :=$0000;
  ImageAddress :=$0000;
  Sector       :=$0000;
  DirRef       :=$0000;
  TimeStamp    :=0;
  EOR          :=$00;
  isDOSPart    :=False;
 end;
end;

{------------------------------------------------------------------------------}
//Remove top bit set characters
{------------------------------------------------------------------------------}
procedure RemoveTopBit(var title: String);
var
 t: Integer;
begin
 for t:=1 to Length(title) do title[t]:=chr(ord(title[t])AND$7F);
end;

{------------------------------------------------------------------------------}
//Add top bit to spaces
{------------------------------------------------------------------------------}
function AddTopBit(title:String):String;
var
 i: Integer;
begin
 //We'll set the top bit on spaces
 for i:=1 to Length(title) do
  if ord(title[i])=32 then title[i]:=chr(32OR$80);
 Result:=title;
end;

{------------------------------------------------------------------------------}
//Convert BBC to Windows filename
{------------------------------------------------------------------------------}
procedure BBCtoWin(var f: String);
var
 i: Integer;
begin
 for i:=1 to Length(f) do
 begin
  if f[i]='/' then f[i]:='.';
  if f[i]='?' then f[i]:='#';
  if f[i]='<' then f[i]:='$';
  if f[i]='>' then f[i]:='^';
  if f[i]='+' then f[i]:='&';
  if f[i]='=' then f[i]:='@';
  if f[i]=';' then f[i]:='%';
 end;
end;

{------------------------------------------------------------------------------}
//Convert Windows to BBC filename
{------------------------------------------------------------------------------}
procedure WintoBBC(var f: String);
var
 i: Integer;
begin
 for i:=1 to Length(f) do
 begin
  if f[i]='.' then f[i]:='/';
  if f[i]='#' then f[i]:='?';
  if f[i]='$' then f[i]:='<';
  if f[i]='^' then f[i]:='>';
  if f[i]='&' then f[i]:='+';
  if f[i]='@' then f[i]:='=';
  if f[i]='%' then f[i]:=';';
 end;
end;

{------------------------------------------------------------------------------}
//Removes trailing spaces from a string
{------------------------------------------------------------------------------}
procedure RemoveSpaces(var s: String);
var
 x: Integer;
begin
 //Start at the end
 x:=Length(s);
 if x>0 then
 begin
  while (s[x]=' ') and (x>0) do //Continue while the last character is a space
   dec(x);       //Move down the string
  s:=Copy(s,1,x);//Finally, remove the spaces
 end;
end;

{------------------------------------------------------------------------------}
//Removes control characters from a string
{------------------------------------------------------------------------------}
procedure RemoveControl(var s: String);
var
 x: Integer;
 o: String;
begin
 //New String
 o:='';
 //Iterate through the old string
 for x:=1 to Length(s) do
  //Only add the character to the new string if it is not a control character
  if ord(s[x])>31 then o:=o+s[x];
 //Change the old string to the new string
 s:=o;
end;

{------------------------------------------------------------------------------}
//Check to see if bit b is set in word v
{------------------------------------------------------------------------------}
function IsBitSet(v,b: Integer): Boolean;
var
 x: Integer;
begin
 Result:=False;
 if (b>=0) and (b<32) then
 begin
  x:=1 shl b;
  Result:=((v AND x)=x);
 end;
end;

{------------------------------------------------------------------------------}
//Break down an *.inf file entry
{------------------------------------------------------------------------------}
function BreakDownInf(s: String): TStringArray;
var
 i: Integer;
 f: String;
begin
 f:='';
 //Remove leading spaces, if any
 if s[1]=' ' then
 begin
  i:=0;
  while s[i+1]=' ' do inc(i);
  s:=RightStr(s,Length(s)-i);
 end;
 //First field has opening quote
 if s[1]='"' then
 begin
  //Remove it
  s:=RightStr(s,Length(s)-1);
  //Find the closing quote
  if Pos('"',s)>0 then //Assuming it has one
  begin
   i:=1;
   while s[i]<>'"' do inc(i);
   //Extract the field
   f:=LeftStr(s,i-1);
   //And replace with a non-quoted field
   s:='filename '+RightStr(s,Length(s)-i);
  end;
 end;
 //Remove double spaces
 while Pos('  ',s)>0 do s:=ReplaceStr(s,'  ',' ');
 //Then split the string into fields
 Result:=s.Split(' ');
 //If we previously found a quoted field, replace the first one with it
 if f<>'' then Result[0]:=f;
end;

{------------------------------------------------------------------------------}
//Ensures a string contains only visible ASCII characters
{------------------------------------------------------------------------------}
function FilenameToASCII(s: String): String;
var
 i: Integer;
begin
 for i:=1 to Length(s) do
  if(ord(s[i])<32)or(ord(s[i])>126)then s[i]:='?';
 Result:=s;
end;

{------------------------------------------------------------------------------}
//Convert a attribute byte into a string
{------------------------------------------------------------------------------}
function GetAttributes(attr: String;format: Byte):String;
var
 attr1 : String;
 attr2 : Byte;
begin
 {This converts a hex number to attributes. This hex number is different to what
 is used by ADFS internally (and saved to the disc images) but is what is
 returned by OSFILE A=5, or OS_File 5 on RISC OS.}
 attr1:=attr;
 attr2:=$00;
 Result:='';
 //Is it a hex number?
 if IntToHex(StrtoIntDef('$'+attr,0),2)=UpperCase(attr) then
 begin //Yes
  attr2:=StrToInt('$'+attr);
  attr1:='';
 end;
 //Read each attribute and build the string
 if(format>>4=diAcornDFS)
 or(format>>4=diAcornADFS)
 or(format>>4=diAcornUEF) then //ADFS, DFS and CFS
  if (Pos('L',attr1)>0)OR(attr2 AND$08=$08) then Result:=Result+'L';
 if format=diAcornADFS then //ADFS only
 begin
  if (Pos('R',attr1)>0)OR(attr2 AND$01=$01) then Result:=Result+'R';
  if (Pos('W',attr1)>0)OR(attr2 AND$02=$02) then Result:=Result+'W';
  if (Pos('E',attr1)>0)OR(attr2 AND$04=$04) then Result:=Result+'E';//Also P
  if (Pos('r',attr1)>0)OR(attr2 AND$10=$10) then Result:=Result+'r';
  if (Pos('w',attr1)>0)OR(attr2 AND$20=$20) then Result:=Result+'w';
  if (Pos('e',attr1)>0)OR(attr2 AND$40=$40) then Result:=Result+'e';
  if (Pos('l',attr1)>0)OR(attr2 AND$80=$80) then Result:=Result+'l';
 end;
end;

{------------------------------------------------------------------------------}
//Wildcard string comparison
{------------------------------------------------------------------------------}
function CompareString(S, mask: string; case_sensitive: Boolean): Boolean;
var
 sIndex,
 maskIndex: Integer;
begin
 if not case_sensitive then
 begin
  S   :=UpperCase(S);
  mask:=UpperCase(mask);
 end;
 Result   :=True;
 sIndex   :=1;
 maskIndex:=1;
 while(sIndex<=Length(S))and(maskIndex<=Length(mask))do
 begin
  case mask[maskIndex] of
   '#':
   begin //matches any character
    Inc(sIndex);
    Inc(maskIndex);
   end;
   '*':
   begin //matches 0 or more characters, so need to check for next character in mask
    Inc(maskIndex);
    if maskIndex>Length(mask) then
     // * at end matches rest of string
     Exit;
    //look for mask character in S
    while(sIndex<=Length(S))and(S[sIndex]<>mask[maskIndex])do
     Inc(sIndex);
    if sIndex>Length(S) then
    begin //character not found, no match
     Result:=false;
     Exit;
    end;
   end;
   else
    if S[sIndex]=mask[maskIndex] then
    begin
     Inc(sIndex);
     Inc(maskIndex);
    end
    else
    begin //no match
     Result:=False;
     Exit;
    end;
  end;
 end;
 //if we have reached the end of both S and mask we have a complete match,
 //otherwise we only have a partial match}
 if(sIndex<=Length(S))or(maskIndex<=Length(mask))then
 begin
  //If the last character of the mask is a '*' then just exit without changing the result
  if maskIndex=Length(mask) then
   if mask[maskIndex]='*' then exit;
  Result:=false;
 end;
end;

end.
