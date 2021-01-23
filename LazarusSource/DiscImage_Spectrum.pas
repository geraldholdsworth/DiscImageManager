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
 Result:=nil;
 {This functionality is not written yet}
end;

{-------------------------------------------------------------------------------
Write a file to Spectrum image
-------------------------------------------------------------------------------}
function TDiscImage.WriteSpectrumFile(file_details: TDirEntry;
                             var buffer: TDIByteArray): Integer;
begin
 Result:=-1;
end;

{-------------------------------------------------------------------------------
Create a new Spectrum image
-------------------------------------------------------------------------------}
function TDiscImage.FormatSpectrum(minor: Byte): TDisc;
begin
 Result:=nil;
end;

function TDiscImage.RenameSpectrumFile(oldfilename: AnsiString;var newfilename: AnsiString):Boolean;
begin
 Result:=False;
end;
function TDiscImage.DeleteSinclairFile(filename: AnsiString):Boolean;
begin
 Result:=False;
end;
function TDiscImage.UpdateSinclairFileAttributes(filename,attributes: AnsiString):Boolean;
begin
 Result:=False;
end;
function TDiscImage.UpdateSinclairDiscTitle(title: AnsiString): Boolean;
begin
 Result:=False;
end;
function TDiscImage.ExtractSpectrumFile(filename: AnsiString;
                                             var buffer: TDIByteArray): Boolean;
begin
 Result:=False;
end;
