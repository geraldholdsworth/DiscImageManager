//++++++++++++++++++ General Purpose Methods +++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
HTTP/URL Encode a string (percent encoding)
-------------------------------------------------------------------------------}
function HTTPEncode(const AStr: String): String;
const
  SafeChars: set of Char = ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'];
var
  I: Integer;
  Ch: Char;
begin
  Result := '';
  for I := 1 to Length(AStr) do
  begin
    Ch := AStr[I];
    if Ch in SafeChars then
      Result := Result + Ch
    else
      Result := Result + '%' + IntToHex(Ord(Ch), 2);
  end;
end;

{-------------------------------------------------------------------------------
HTTP/URL Decode a string (percent decoding)
-------------------------------------------------------------------------------}
function HTTPDecode(const AStr: String): String;
var
  I: Integer;
  Ch: Char;
  HexVal: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(AStr) do
  begin
    Ch := AStr[I];
    if (Ch = '%') and (I + 2 <= Length(AStr)) then
    begin
      HexVal := StrToIntDef('$' + Copy(AStr, I + 1, 2), -1);
      if HexVal >= 0 then
      begin
        Result := Result + Chr(HexVal);
        Inc(I, 3);
        Continue;
      end;
    end;
    Result := Result + Ch;
    Inc(I);
  end;
end;

{-------------------------------------------------------------------------------
Reset a TDirEntry to blank
-------------------------------------------------------------------------------}
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
  Sector       :=$0000;
  DirRef       :=$0000;
  TimeStamp    :=0;
  isDOSPart    :=False;
  Sequence     :=0;
 end;
end;

{-------------------------------------------------------------------------------
Remove top bit set characters
-------------------------------------------------------------------------------}
procedure RemoveTopBit(var title: String);
var
 t: Integer=0;
begin
 for t:=1 to Length(title) do title[t]:=chr(ord(title[t])AND$7F);
end;

{-------------------------------------------------------------------------------
Add top bit to spaces
-------------------------------------------------------------------------------}
function AddTopBit(title:String):String;
var
 i: Integer=0;
begin
 //We'll set the top bit on spaces
 for i:=1 to Length(title) do
  if ord(title[i])=32 then title[i]:=chr(32OR$80);
 Result:=title;
end;

{-------------------------------------------------------------------------------
Convert BBC to Windows filename
-------------------------------------------------------------------------------}
procedure BBCtoWin(var f: String);
var
 i: Integer=0;
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

{-------------------------------------------------------------------------------
Convert Windows to BBC filename
-------------------------------------------------------------------------------}
procedure WintoBBC(var f: String);
var
 i: Integer=0;
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

{-------------------------------------------------------------------------------
Removes trailing spaces from a string
-------------------------------------------------------------------------------}
procedure RemoveSpaces(var s: String);
var
 x: Integer=0;
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

{-------------------------------------------------------------------------------
Removes control characters from a string
-------------------------------------------------------------------------------}
procedure RemoveControl(var s: String);
var
 x: Integer=0;
 o: String='';
begin
 //Iterate through the old string
 for x:=1 to Length(s) do
  //Only add the character to the new string if it is not a control character
  if ord(s[x])>31 then o:=o+s[x];
 //Change the old string to the new string
 s:=o;
end;

{-------------------------------------------------------------------------------
Check to see if bit b is set in word v
------------------------------------------------------------------------------}
function IsBitSet(v,b: Integer): Boolean;
var
 x: Integer=0;
begin
 Result:=False;
 if (b>=0) and (b<32) then
 begin
  x:=1 shl b;
  Result:=((v AND x)=x);
 end;
end;

{-------------------------------------------------------------------------------
Parses an inf (passed in line) and outputs as JSON (passed in output)
-------------------------------------------------------------------------------}
procedure ParseInf(output: TObject; line: String);
var
 element : String='';
 elements: TStringArray=();
 fields  : TStringArray=();
 index   : Integer=0;
 start   : Integer=1;
 offset  : Integer=0;
const
 //Known field names (Filename is first)
 Field: array[0..9] of String=('Load Address'
                              ,'Execution Address'
                              ,'Length'
                              ,'Access'
                              ,'Modification Date'
                              ,'Modification Time'
                              ,'Creation Date'
                              ,'Creation Time'
                              ,'User Account'
                              ,'Auxliiary Account');
 //And their type (0=String, 1=Hex);
 FieldType: array[0..9] of Integer=(1,1,1,0,1,1,1,1,1,1);
 //De Quote a string (different to the string helper DeQuotedString) -----------
 function DeQuote(input: String): String;
 begin
  Result:=input;
  if Length(input)>1 then
   if(input[1]='"')and(input.EndsWith('"'))then
    Result:=Copy(input,2,Length(input)-2);
 end;
 //Parse a string according, dequoting and percent decoding --------------------
 function ParseString(input: String): String;
 begin
  //Remove any quotes, and de-escape quotes contained
  Result:=DeQuote(input);
  //Inside of quotes, then put it through HTTP Decoding
  if Result<>input then Result:=HTTPDecode(Result);
 end;
 //Validate the input string as a valid hex number. Return 0 if not ------------
 function ValidateHex(input: String): String;
 var
  TestHex: String='';
 begin
  //Default result
  Result:='0';
  //Only continue if there is something
  if(not input.IsEmpty)and(Length(input)<9)then
  begin
   //Remove any '$' at the start
   while input[1]='$' do
   begin
    input:=Copy(input,2);
    if input.IsEmpty then input:='0';
   end;
   //Now remove any '0x' at the start
   if Length(line)>1 then while LeftStr(line,2)='0x' do line:=Copy(line,3);
   //Create a test hex
   TestHex:=StringOfChar('F',Length(input)+1);
   //Finally, see if it is a valid hex and convert, if it is
   if IntToHex(StrToIntDef('$'+input,StrToInt('$'+TestHex))
              ,Length(input)+1)<>TestHex then
    Result:=input;
  end;
 end;
 //Check for a Key=Value -------------------------------------------------------
 function CheckForKeyValue(input: String): Boolean;
 begin
  Result:=Pos('=',input)>0;
 end;
 //Get a KEY=VALUE -------------------------------------------------------------
 function GetKeyValue(input: String): TStringArray;
 begin
  Result:=[];
  if CheckForKeyValue(input) then Result:=input.Split('=');
 end;
 //Parse a field ---------------------------------------------------------------
 procedure ParseField(num: Integer; name: String; Ltype: Byte);
 var
  value : String='';
  keyval: TStringArray=();
 begin
  //Are there enough fields?
  if Length(fields)>num then
  begin
   //Default values
   if Ltype=0 then value:='';         //String
   if Ltype=1 then value:='00000000'; //Hex
   //Is it a key=value
   if not CheckForKeyValue(fields[num]) then
   begin //No
    if Ltype=0 then value:=ParseString(fields[num]); //Parse a string
    if Ltype=1 then value:=ValidateHex(fields[num]); //Parse a hex
   end
   else
   begin //Yes
    //Get the Key and Value
    keyval:=GetKeyValue(fields[num]);
    //Assign them
    name  :=ParseString(keyval[0]);
    value :=ParseString(keyval[1]);
   end;
   //Add the result to the output
   if output is TJSONObject then
    (output as TJSONObject).Add(name,value); //JSON output
   if output is TXMLDocument then
    (output as TXMLDocument).Add(name,value);//XML output
  end;
 end;
 //Check a field to see if it contains an attribute (access) field
 function CheckForAccess(entry: String): Boolean;
 begin
  entry:=UpperCase(entry);
  Result:=(entry='LOCKED')or(entry='L')//DFS Locked flag
        or((Length(entry)=2)and(StrToIntDef('$'+entry,$FFF)<>$FFF))//2 digit hex
        or((not CheckForKeyValue(entry)) //Is not a key/value pair
           and(StrToIntDef('$'+entry,$FFF)=$FFF)and(UpperCase(entry)<>'FFF'));
 end;
//Main procedure definition ----------------------------------------------------
begin
 if(output is TJSONObject)      //Currently, only outputs to JSON or XML
 or(output is TXMLDocument)then
 begin
  //Do we have a 'CRC= '?
  line:=StringReplace(line,'CRC= ','CRC=',[rfReplaceAll]);
  //Only act on it if it is not blank
  if not line.IsEmpty then
  begin
   //Split the string into separate words, but not within quotes
   elements:=line.Split(' ','"');
   //Iterate through the elements
   for element in elements do //To add non-blank elements
    if not element.IsEmpty then Insert(element,fields,Length(fields));
   //Only continue if there is something to show
   if Length(fields)>0 then
   begin
    //Add the elements in the inf order
    if DeQuote(fields[0])='TAPE' then start:=1 else start:=0;
    ParseField(start,'Filename',0); //Filename
    offset:=0;
    if Length(fields)>start+1 then
     for Index:=start+1 to Length(fields)-1 do
      if Index<Length(Field) then //Known field (above)
      begin
       //Syntax 1 is <filename> <load> <exec> <access> <extra info> : for DFS
       if(Index=start+3)and(Length(fields)>start+3)then
       if CheckForAccess(fields[Index]) then offset:=1;
       //Syntax 3 is <filename> <access> <extra info> : for directories
       if(Index=start+1)and(Length(fields)>start+1)then
        if CheckForAccess(fields[Index]) then offset:=3;
       //Everything else - syntax 2
       ParseField(Index,Field[offset+Index-(start+1)]
                       ,FieldType[offset+Index-(start+1)]);
      end
      else                        //Extra field
       ParseField(Index,'Field '+IntToStr(Index),0);
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Ensures a string contains only visible ASCII characters
-------------------------------------------------------------------------------}
function FilenameToASCII(s: String): String;
var
 i: Integer=0;
begin
 for i:=1 to Length(s) do
  if(ord(s[i])<32)or(ord(s[i])>126)then s[i]:='?';
 Result:=s;
end;

{-------------------------------------------------------------------------------
Convert a attribute byte into a string
-------------------------------------------------------------------------------}
function GetAttributes(attr: String;format: Byte):String;
var
 attr1 : String='';
 attr2 : Byte=0;
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
 end else Result:=attr; //Not a hex, so just return what was passed
end;

{-------------------------------------------------------------------------------
Wildcard string comparison
-------------------------------------------------------------------------------}
function CompareString(S, mask: string; case_sensitive: Boolean): Boolean;
var
 sIndex   : Integer=1;
 maskIndex: Integer=1;
begin
 if not case_sensitive then
 begin
  S   :=UpperCase(S);
  mask:=UpperCase(mask);
 end;
 Result   :=True;
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

{-------------------------------------------------------------------------------
Convert a TDateTime to an AFS compatible Word
-------------------------------------------------------------------------------}
function DateTimeToAFS(timedate: TDateTime):Word;
var
 y: Byte=0;
 m: Byte=0;
 d: Byte=0;
begin
 y:=StrToIntDef(FormatDateTime('yyyy',timedate),1981)-1981;//Year
 m:=StrToIntDef(FormatDateTime('m',timedate),1);           //Month
 d:=StrToIntDef(FormatDateTime('d',timedate),1);           //Date
 Result:=((y AND$F)<<12)OR((y AND$F0)<<1)OR(d AND$1F)OR((m AND$F)<<8);
end;

{-------------------------------------------------------------------------------
Convert an AFS date to a TDateTime
-------------------------------------------------------------------------------}
function AFSToDateTime(date: Word):TDateTime;
var
 day   : Integer=0;
 month : Integer=0;
 year  : Integer=0;
begin
 Result:=0;
 if date=0 then exit;
 day:=date AND$1F;//Day;
 month:=(date AND$F00)>>8; //Month
 year:=((date AND$F000)>>12)+((date AND$E0)>>1)+1981; //Year
 if(day>0)and(day<32)and(month>0)and(month<13)then
  Result:=EncodeDate(year,month,day);
end;

{------------------------------------------------------------------------------
Validate a filename for Windows
-------------------------------------------------------------------------------}
procedure ValidateWinFilename(var f: String);
var
 i: Integer=0;
const
  illegal = '\/:*?"<>|'#0;
begin
 if Length(f)>0 then
  for i:=1 to Length(f) do
   if Pos(f[i],illegal)>0 then f[i]:=' ';
end;

{-------------------------------------------------------------------------------
Converts a decimal number to BCD
-------------------------------------------------------------------------------}
function DecToBCD(dec: Cardinal): Cardinal;
var
 s: String='';
 i: Integer=0;
begin
 Result:=0;
 s:=IntToStr(dec);
 for i:=Length(s) downto 1 do
  inc(Result,StrToInt(s[i])<<(4*(Length(s)-i)));
end;

{-------------------------------------------------------------------------------
Converts a BCD to decimal number
-------------------------------------------------------------------------------}
function BCDToDec(BCD: Cardinal): Cardinal;
var
 s: String='';
 i: Integer=0;
begin
 Result:=0;
 s:=IntToHex(BCD);
 for i:=Length(s) downto 1 do
  inc(Result,StrToIntDef(s[i],0)*(10**(Length(s)-i)));
end;

{-------------------------------------------------------------------------------
Reset a TFileEntry to default values
-------------------------------------------------------------------------------}
procedure ResetFileEntry(var fileentry: TFileEntry);
begin
 with fileentry do
 begin
  LoadAddr   :=0;
  ExecAddr   :=0;
  Length     :=0;
  Size       :=0;
  NumEntries :=0;
  Attributes :=0;
  DataOffset :=0;
  Filename   :='';
  Parent     :='';
  ArchiveName:='';
  Directory  :=False;
 end;
end;

{-------------------------------------------------------------------------------
Creates an XML document and adds a root
-------------------------------------------------------------------------------}
function CreateXML(name: String): TXMLDocument;
begin
 Result:=nil;
 if not name.IsEmpty then
 begin
  Result:=TXMLDocument.Create;
  Result.ValidateName(name);
  Result.AppendChild(Result.CreateElement(UniCodeString(name)));
 end;
end;

//XML Class Helper

{-------------------------------------------------------------------------------
Adds an element to the XML tree
-------------------------------------------------------------------------------}
procedure TXMLHelper.Add(name, value: String);
var
 Parent: TDOMNode=nil;
 LChar : Integer=0;
const
 //Allowed characters in an XML value
 PubidChar    =#$0D#$0A' ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
              +'0123456789-''()+,./:=?;!*#@$_%&';
begin
 if not name.IsEmpty then
 begin
  //Validate the value
  if not value.IsEmpty then
   for LChar:=1 to Length(value) do
    if Pos(value[LChar],PubidChar)=0 then value[LChar]:=' ';
  ValidateName(name);
  //Create the parent
  Parent:=self.CreateElement(UnicodeString(name));
  //Create and append the value, if not empty
  if not value.IsEmpty then
   Parent.AppendChild(self.CreateTextNode(UnicodeString(value)));
  //Add it to the document at the root level
  self.DocumentElement.AppendChild(Parent);
 end;
end;

{-------------------------------------------------------------------------------
Outputs the XML document as a string
-------------------------------------------------------------------------------}
function TXMLHelper.AsString: String;
var
 S: TStringStream;
begin
 //Set up the string stream
 S:=TStringStream.Create;
 //Output the XML to the stream
 WriteXMLFile(self,S);
 //Reset to the start
 S.Position:=0;
 //And output as a string
 Result:=Trim(S.ReadString(S.Size));
 //Clear up
 S.Free;
end;

{-------------------------------------------------------------------------------
Validate the name for allowed characters
-------------------------------------------------------------------------------}
procedure TXMLHelper.ValidateName(var name: String);
var
 LChar : Integer=0;
const
 //Allowed characters in an XML node name
 NameStartChar=':ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz';
 NameChar     =NameStartChar+'-.0123456789';
begin
 if not name.IsEmpty then
 begin
  //Validate the name
  if Pos(name[1],NameStartChar)=0 then name[1]:='_';      //Start character
  if Length(name)>1 then
   for LChar:=2 to Length(name) do                        //Subsequent characters
    if Pos(name[LChar],NameChar)=0 then name[LChar]:='_';
 end;
end;
