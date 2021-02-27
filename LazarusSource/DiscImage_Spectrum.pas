//++++++++++++++++++ Sinclair Spectrum +3/Amstrad ++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a Spectrum disc
-------------------------------------------------------------------------------}
function TDiscImage.ID_Sinclair: Boolean;
begin
 Result:=False;
 if FFormat=$FF then
 begin
  if GetDataLength>0 then
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

{-------------------------------------------------------------------------------
Rename a Spectrum file
-------------------------------------------------------------------------------}
function TDiscImage.RenameSpectrumFile(oldfilename: String;var newfilename: String):Integer;
begin
 Result:=-6; //Unsupported in this format
end;

{-------------------------------------------------------------------------------
Delete a Spectrum file
-------------------------------------------------------------------------------}
function TDiscImage.DeleteSinclairFile(filename: String):Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Update attributes on a Spectrum file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateSinclairFileAttributes(filename,attributes: String):Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Change the disc title for a Spectrum disc
-------------------------------------------------------------------------------}
function TDiscImage.UpdateSinclairDiscTitle(title: String): Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Extract a file from a Spectrum image
-------------------------------------------------------------------------------}
function TDiscImage.ExtractSpectrumFile(filename: String;
                                             var buffer: TDIByteArray): Boolean;
begin
 Result:=False;
end;
