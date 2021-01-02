//++++++++++++++++++ Sinclair Spectrum +3/Amstrad ++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a Spectrum disc
-------------------------------------------------------------------------------}
function TDiscImage.ID_Sinclair: Boolean;
begin
 Result:=False;
 if FFormat=$FF then
 begin
  if Length(Fdata)>0 then
  begin
   if ReadString(0,-6)='MV-CPC'   then FFormat:=$30;
   if ReadString(0,-8)='EXTENDED' then FFormat:=$31;
   Result:=FFormat<>$FF;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Read Spectrum Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadSinclairDisc: TDisc;
begin
 {This functionality is not written yet}
end;