unit Global;

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

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

function ReadLine(var Stream: TFileStream;var Line: string): boolean;
function WriteLine(var Stream: TFileStream;Line: string): boolean;
function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;  MaxCol: Integer): string;
function WrapText(const Line: string; MaxCol: Integer): string; overload;

implementation

{-------------------------------------------------------------------------------
Reads a line from a TStream
-------------------------------------------------------------------------------}
function ReadLine(var Stream: TFileStream;var Line: string): boolean;
var
 RawLine: UTF8String='';
 ch     : AnsiChar=#0;
begin
 Result:=False;
 while (Stream.Read(ch,1)=1) and (ch<>#13) and (ch<>#10) do
 begin
  Result:=True;
  RawLine:=RawLine+UTF8String(ch);
 end;
 Line:=String(RawLine);
 if ch=#13 then
 begin
  Result:=True;
  if (Stream.Read(ch,1)=1) and (ch<>#10) then
   Stream.Seek(-1,soCurrent) // unread it if not LF character.
 end;
 if ch=#10 then
 begin
  Result:=True;
  if (Stream.Read(ch,1)=1) and (ch<>#13) then
   Stream.Seek(-1,soCurrent) // unread it if not CR character.
 end;
end;

{-------------------------------------------------------------------------------
Writes a string to the TFileStream, and terminates it with 0x0A
-------------------------------------------------------------------------------}
function WriteLine(var Stream: TFileStream;Line: string): boolean;
var
 l: Integer=0;
 x: Integer=0;
 S: UTF8String='';
begin
 S:=UTF8String(Line+#10);
 l:=Length(S);
 x:=Stream.Write(S[1],l);
 Result:=x=l;
end;

{-------------------------------------------------------------------------------
Wraps Text (modded version of the SysUtils method)
-------------------------------------------------------------------------------}
function WrapText(const Line, BreakStr: string; const BreakChars: TSysCharSet;  MaxCol: Integer): string;

const
  Quotes = ['''', '"'];

Var
  L : String;
  C,LQ,BC : Char;
  P,Q,BLen,Len : Integer;
  HB,IBC : Boolean;

begin
  Result:='';
  L:=Line;
  Blen:=Length(BreakStr);
  If (BLen>0) then
    BC:=BreakStr[1]
  else
    BC:=#0;
  Len:=Length(L);
  While (Len>0) do
    begin
    P:=1;
    LQ:=#0;
    HB:=False;
    IBC:=False;
    While ((P<=Len) and ((P<=MaxCol) or not IBC)) and ((LQ<>#0) or Not HB) do
      begin
      C:=L[P];
      If (C=LQ) then
        LQ:=#0
      else If (C in Quotes) then
        LQ:=C;
      If (LQ<>#0) then
        Inc(P)
      else
        begin
        HB:=((C=BC) and (BreakStr=Copy(L,P,BLen)));
        If HB then
          Inc(P,Blen)
        else
          begin
          If (P>=MaxCol) then
            IBC:=C in BreakChars;
          Inc(P);
          end;
        end;
      end;
    if P>MaxCol then
    begin
     Q:=P;
     dec(P);
     repeat
       dec(P);
       IBC:=L[P] in BreakChars;
     until(P=1)or(IBC);
     if P=1 then P:=Q;
    end;
    Result:=Result+Copy(L,1,P-1);
    Delete(L,1,P-1);
    if Length(L)>1 then while L[1] in BreakChars do Delete(L,1,1);
    Len:=Length(L);
    If (Len>0) and Not HB then
      Result:=Result+BreakStr;
    end;
end;

{-------------------------------------------------------------------------------
Wraps Text (modded version of the SysUtils method)
-------------------------------------------------------------------------------}
function WrapText(const Line: string; MaxCol: Integer): string;
begin
  Result:=WrapText(Line,sLineBreak, [' ', '-', #9], MaxCol);
end;

end.
