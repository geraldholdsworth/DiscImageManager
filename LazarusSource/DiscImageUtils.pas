unit DiscImageUtils;

{$mode objfpc}{$H+}

interface

uses
 SysUtils, StrUtils;

type
//Define the TDIByteArray - saves using the System.Types unit for TByteDynArray
 TDIByteArray = array of Byte;
//Define the records to hold the catalogue
 TDirEntry     = record     //Not all fields are used on all formats
  Parent,                   //Complete path for parent directory (ALL)
  Filename,                 //Filename (ALL)
  Attributes,               //File attributes (ADFS/DFS/D64/D71/D81/AmigaDOS)
  Filetype,                 //Full name filetype (ADFS/D64/D71/D81)
  ShortFileType,            //Filetype shortname (ADFS/D64/D71/D81)
  CRC32        : AnsiString;//CRC32 for the file (ALL)
  LoadAddr,                 //Load Address (ADFS/DFS)
  ExecAddr,                 //Execution Address (ADFS/DFS)
  Length,                   //Total length (ALL)
  Side,                     //Side of disc of location of data (DFS)
  Track,                    //Track of location of data (D64/D71/D81)
  DataFile,                 //Reserved for use by Repton Map Display
  ImageAddress: Cardinal;   //Reserved for use by Repton Map Display
  Sector,                   //Sector of disc of location of data (DFS/D64/D71/D81/AmigaDOS file)
                            //Sector of disc of location of header (AmigaDOS directory)
                            //Address of location of data (ADFS S/M/L)
                            //Indirect disc address of data (ADFS D/E/F/E+/F+)
  DirRef      : Integer;    //Reference to directory, if directory (ADFS/AmigaDOS)
  TimeStamp   : TDateTime;  //Timestamp (ADFS D/E/E+/F/F+)
  EOR         : Byte;       //Reserved for use by Repton Map Display
 end;
 TSearchResults =array of TDirEntry;
 //General purpose procedures
 procedure ResetDirEntry(var Entry: TDirEntry);
 procedure RemoveTopBit(var title: AnsiString);
 procedure BBCtoWin(var f: AnsiString);
 procedure WintoBBC(var f: AnsiString);
 procedure RemoveSpaces(var s: AnsiString);
 procedure RemoveControl(var s: AnsiString);
 function IsBitSet(v,b: Integer): Boolean;
 function BreakDownInf(s: AnsiString): TStringArray;

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
 end;
end;

{------------------------------------------------------------------------------}
//Remove top bit set characters
{------------------------------------------------------------------------------}
procedure RemoveTopBit(var title: AnsiString);
var
 t: Integer;
begin
 for t:=1 to Length(title) do title[t]:=chr(ord(title[t])AND$7F);
end;

{------------------------------------------------------------------------------}
//Convert BBC to Windows filename
{------------------------------------------------------------------------------}
procedure BBCtoWin(var f: AnsiString);
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
procedure WintoBBC(var f: AnsiString);
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
procedure RemoveSpaces(var s: AnsiString);
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
procedure RemoveControl(var s: AnsiString);
var
 x: Integer;
 o: AnsiString;
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
function BreakDownInf(s: AnsiString): TStringArray;
var
 i: Integer;
 f: AnsiString;
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

{ To load a UEF file
uses ZStream;

type
  TDynByteArray = Array of Byte;

procedure Button2Click(Sender: TObject);
var
 F: TFileStream;
 buffer: TDynByteArray;
const
 source = '/Users/geraldholdsworth/OneDrive/Emulation/Tape Images/Repton-BRRR_B.uef';
 dest   = '/Users/geraldholdsworth/OneDrive/Emulation/Tape Images/Repton-BRRR_B inflated.uef';
begin
 buffer:=nil;
 F:=TFileStream.Create(source,fmOpenRead);
 F.Position:=0;
 SetLength(buffer,3);
 F.Read(buffer[0],3);
 F.Free;
 if(buffer[0]=$1F)and(buffer[1]=$8B)and(buffer[2]=$08) then
  InflateToFile(source,dest);
end;

function Inflate(source: String): TDynByteArray;
var
 GZ     : TGZFileStream;
 chunk  : array of Byte;
 cnt,
 i,
 buflen : Integer;
const
  ChunkSize=4096;
begin
 Result:=nil;
 chunk:=nil;
 GZ:=TGZFileStream.create(source,gzOpenRead);
 buflen:=0;
 SetLength(chunk,ChunkSize);
 repeat
  cnt:=GZ.Read(chunk[0],ChunkSize);
  SetLength(Result,buflen+cnt);
  for i:=0 to cnt-1 do
   Result[buflen+i]:=chunk[i];
  inc(buflen,cnt);
 until cnt<ChunkSize;
 GZ.Free;
end;

procedure InflateToFile(source,dest: String);
var
 F      : TFileStream;
 buffer : TDynByteArray;
begin
 buffer:=Inflate(source);
 F:=TFileStream.Create(dest,fmCreate);
 F.Position:=0;
 F.Write(buffer[0],Length(buffer));
 F.Free;
end;
}
end.
