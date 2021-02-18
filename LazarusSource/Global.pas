unit Global;

{$mode objfpc}{$H+}

interface

uses
 Classes;

function ReadLine(var Stream: TFileStream;var Line: string): boolean;
function WriteLine(var Stream: TFileStream;Line: string): boolean;

implementation

{-------------------------------------------------------------------------------
Reads a line from a TStream
-------------------------------------------------------------------------------}
function ReadLine(var Stream: TFileStream;var Line: string): boolean;
var
 RawLine: UTF8String;
 ch     : AnsiChar;
begin
 RawLine:='';
 Result:=False;
 ch:=#0;
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
 l,x: Integer;
 S: UTF8String;
begin
 S:=UTF8String(Line+#10);
 l:=Length(S);
 x:=Stream.Write(S[1],l);
 Result:=x=l;
end;

end.
