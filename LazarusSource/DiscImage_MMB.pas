//++++++++++++++++++++++++ MMB +++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies an MMB
-------------------------------------------------------------------------------}
function TDiscImage.ID_MMB: Boolean;
var
 c,i,ptr : Integer;
begin
 if FFormat=diInvalidImg then
 begin
  ResetVariables;
  //Check it is of the correct length (511 images * 200K + 0x2000 bytes)
  if GetDataLength=$63D0000 then
  begin
   FFormat:=diMMFS<<4;
   //Check the status bytes (last byte of each 16 byte entry)
   c:=0;
   for i:=1 to 512 do
   begin
    ptr:=ReadByte((16*i)-1);
    if(ptr=$00)or(ptr=$0F)or(ptr=$F0)or(ptr=$FF)then inc(c);
   end;
   if c<511 then FFormat:=diInvalidImg;
  end;
 end;
 Result:=FFormat>>4=diMMFS;
end;

{-------------------------------------------------------------------------------
Read MMB file
-------------------------------------------------------------------------------}
function TDiscImage.ReadMMBDisc: TDisc;
var
 i,j,
 dir   : Integer;
 ptr   : Cardinal;
 c     : Byte;
 d     : TDisc;
begin
 Result:=nil;
 //511 entries in an MMB file
 SetLength(Result,511);
 //Now go through them and read them in
 for i:=0 to 510 do
 begin
  //Inform the user
  UpdateProgress('Reading disc '+IntToStr(i));
  //Reset the entry
  ResetDir(Result[i]);
  //Pointer to each header entry
  ptr:=16+16*i;
  //If formatted, or locked, read the disc
  if(ReadByte(ptr+15)=$0F)or(ReadByte(ptr+15)=$00)then
  begin
   //First read the title from the header
   for j:=0 to 11 do
   begin
    c:=ReadByte(ptr+j);
    if(c>31)and(c<127)then Result[i].Directory:=Result[i].Directory+chr(c);
   end;
   Result[i].Title:=Result[i].Directory;
   Result[i].Directory:=IntToStr(i)+': '+Result[i].Directory;
   if ReadByte(ptr+15)=$00 then Result[i].Locked:=True
   else Result[i].Locked:=False;
   //Now read the disc, using the DFS routines
   d:=ReadDFSDisc(i);
   //Then transfer this across to the the one we are building up
   if Length(d)>0 then
    for dir:=0 to Length(d)-1 do
     if Length(d[dir].Entries)>0 then
      Result[i].Entries:=d[dir].Entries;
  end
  else //Entry is empty
   Result[i].Directory:=IntToStr(i)+': empty';
 end;
 disc_name[0]:='MMFS File';
 disc_size[0]:=511;
end;
