unit FormatAnalysis;

{
  Disk Image Manager -  Copyright 2002-2009 Envy Technologies Ltd.

  Virtual disk format and protection analysis
}

interface

uses DskImage, SysUtils;

function AnalyseFormat(Disk: TDSKDisk): string;
function DetectUniformFormat(Disk: TDSKDisk): string;
function DetectProtection(Side: TDSKSide): string;
function DetectInterleave(Track: TDskTrack): string;

implementation

uses Utils;

function AnalyseFormat(Disk: TDSKDisk): string;
var
  Protection: string;
  Format: string;
begin
  if Disk.GetFirstSector = nil then
    Result := 'Unformatted'
  else
  begin
    Result := '';
    Format := '';
    Protection := DetectProtection(Disk.Side[0]);
    if (Disk.IsUniform(True)) then
      Format := DetectUniformFormat(Disk);
    if (Format <> '') then
      Result := Format;
    if (Protection <> '') then
      Result := Trim(Result + ' ' + Protection);
    if (Result = '') then
      Result := 'Unknown';
  end;
end;

const
  EINSTEIN_SIGNATURE: array[0..4] of byte = ($00, $E1, $00, $FB, $00);

function DetectUniformFormat(Disk: TDSKDisk): string;
var
  FirstTrack: TDSKTrack;
  FirstSector: TDSKSector;
begin
  FirstTrack := Disk.GetLogicalTrack(0);
  FirstSector := FirstTrack.GetFirstLogicalSector();
  Result := '';

  // Amstrad formats (9 sectors, 512 size, SS or DS)
  if (FirstTrack.Sectors = 9) and (FirstSector.DataSize = 512) then
  begin
    case FirstSector.ID of
      1: begin
        Result := 'Amstrad PCW/Spectrum +3';
        case FirstSector.GetModChecksum(256) of
          1: Result := 'Amstrad PCW 9512';
          3: Result := 'Spectrum +3';
          255: Result := 'Amstrad PCW 8256';
        end;
        if (Disk.Sides = 1) then
          Result := Result + ' CF2'
        else
          Result := Result + ' CF2DD';
      end;
      65: Result := 'Amstrad CPC system';
      193: Result := 'Amstrad CPC data';
    end;

    // Add indicator where more tracks than normal
    if (Disk.Side[0].HighTrackCount > (Disk.Sides * 40)) then Result := Result + ' (oversized)';
    if (Disk.Side[0].HighTrackCount < (Disk.Sides * 40)) then Result := Result + ' (undersized)';
  end
  else
  begin
    // Other possible formats...
    case FirstSector.ID of
      1: if FirstTrack.Sectors = 8 then Result := 'Amstrad CPC IBM';
      65: Result := 'Amstrad CPC system custom (maybe)';
      193: Result := 'Amstrad CPC data custom (maybe)';
    end;
  end;

  // Custom speccy formats (10 sectors, SS)
  if (Disk.Sides = 1) and (FirstTrack.Sectors = 10) then
  begin
    // HiForm/Ultra208 (Chris Pile) + Ian Collier's skewed versions
    if (FirstSector.DataSize > 10) then
    begin
      if (FirstSector.Data[2] = 42) and (FirstSector.Data[8] = 12) then
        case FirstSector.Data[5] of
          0: if (FirstTrack.Sector[1].ID = 8) then
              case Disk.Side[0].Track[1].Sector[0].ID of
                7: Result := 'Ultra 208/Ian Max';
                8: Result := 'Maybe Ultra 208 or Ian Max (skew lost)';
                else
                  Result := 'Maybe Ultra 208 or Ian Max (custom skew)';
              end
            else
              Result := 'Possibly Ultra 208 or Ian Max (interleave lost)';
          1: if (FirstTrack.Sector[1].ID = 8) then
              case Disk.Side[0].Track[1].Sector[0].ID of
                7: Result := 'Ian High';
                1: Result := 'HiForm 203';
                else
                  Result := 'Maybe HiForm 203 or Ian High (custom skew)';
              end;
          else
            Result := 'Possibly HiForm or Ian High (interleave lost)';
        end;
      // Supermat 192 (Ian Cull)
      if (FirstSector.Data[7] = 3) and (FirstSector.Data[9] = 23) and (FirstSector.Data[2] = 40) then
        Result := 'Supermat 192/XCF2';
    end;
  end;

  // Sam Coupe formats
  if (Disk.Sides = 2) and (FirstTrack.Sectors = 10) and (Disk.Side[0].HighTrackCount = 80) and
    (FirstSector.ID = 1) and (FirstSector.DataSize = 512) then
  begin
    Result := 'MGT SAM Coupe';
    if StrInByteArray(FirstSector.Data, 'BDOS', 232) then
      Result := Result + ' BDOS'
    else
      case FirstSector.Data[210] of
        0, 255: Result := Result + ' SAMDOS';
        else
          Result := Result + ' MasterDOS';
      end;
  end;

  // Einstein format
  if FirstSector.DataSize > 10 then
    if CompareMem(@FirstSector.Data, @EINSTEIN_SIGNATURE, Length(EINSTEIN_SIGNATURE)) then
      Result := 'Einstein';
end;

// We have two techniques for copy-protection detection - ASCII signatures
// and structural characteristics.
function DetectProtection(Side: TDSKSide): string;
var
  TIdx, SIdx, Offset, LastTIdx: integer;
  Temp: string;
  Sector: TDSKSector;
begin
  Result := '';
  if (Side.Tracks < 2) or (Side.Track[0].Sectors < 1) or (Side.Track[0].Sector[0].DataSize < 128) then exit;

  // Alkatraz copy-protection
  Offset := StrBufPos(Side.Track[0].Sector[0].Data, ' THE ALKATRAZ PROTECTION SYSTEM   (C) 1987  Appleby Associates');
  if Offset > -1 then
  begin
    Result := Format('Alkatraz +3 (signed at T0/S0 +%d', [Offset]);
    exit;
  end;

  for TIdx := 0 to Side.Tracks - 2 do
    if (Side.Track[TIdx].Sectors = 18) and (Side.Track[TIdx].SectorSize = 256) then
      if (Side.Track[TIdx + 1].Sectors > 0) and (Side.Track[TIdx + 1].Sector[0].FDCStatus[2] = 64) then
      begin
        Result := Format('Alkatraz CPC (18 sector T%d)', [TIdx]);
        exit;
      end;

  // Frontier copy-protection
  if (Side.Tracks > 10) and (Side.Track[1].Sectors > 0) and (Side.Track[0].Sector[0].DataSize > 1) then
  begin
    Offset := StrBufPos(Side.Track[1].Sector[0].Data, 'W DISK PROTECTION SYSTEM. (C) 1990 BY NEW FRONTIER SOFT.');
    if Offset > -1 then
    begin
      Result := Format('Frontier (signed T1/S0 +%d)', [Offset]);
      exit;
    end;

    if (Side.Track[9].Sectors = 1) and (Side.Track[0].Sector[0].DataSize = 4096) and
      (Side.Track[0].Sector[0].FDCStatus[1] = 0) then
      Result := 'Frontier (probably, unsigned)';
  end;

  // Hexagon
  if (Side.Track[0].Sectors = 10) and (Side.Track[0].Sector[8].DataSize = 512) and (Side.Tracks > 2) then
  begin
    for TIdx := 0 to 3 do
    begin
      for SIdx := 0 to Side.Track[TIdx].Sectors - 1 do
      begin
        Offset := StrBufPos(Side.Track[TIdx].Sector[SIdx].Data, 'HEXAGON DISK PROTECTION c 1989');
        if Offset > -1 then
        begin
          Result := Format('Hexagon (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
          exit;
        end;
        Offset := StrBufPos(Side.Track[TIdx].Sector[SIdx].Data, 'HEXAGON Disk Protection c 1989');
        if Offset > -1 then
        begin
          Result := Format('Hexagon (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
          exit;
        end;
      end;

      if (Side.Track[TIdx].IsFormatted) and (Side.Track[TIdx].Sectors = 1) and (Side.Track[TIdx].Sector[0].FDCSize = 6) and
        (Side.Track[TIdx].Sector[0].FDCStatus[1] = 32) and (Side.Track[TIdx].Sector[0].FDCStatus[2] = 96) then
        Result := 'Hexagon (probably, unsigned)';
    end;
  end;

  // Paul Owens
  if (Side.Track[0].Sectors = 9) and (Side.Tracks > 10) and (Side.Track[1].Sectors = 0) then
  begin
    Offset := StrBufPos(Side.Track[0].Sector[2].Data, 'PAUL OWENS' + #128 + 'PROTECTION SYS');
    if Offset > -1 then
    begin
      Result := Format('Paul Owens (signed T0/S2 +%d)', [Offset]);
      exit;
    end
    else
    if (Side.Track[2].Sectors = 6) and (Side.Track[2].Sector[0].DataSize = 256) then
      Result := 'Paul Owens (probably, unsigned)';
  end;

  // Speedlock signatures somewhere... (usually 0 but not always)
  for TIdx := 0 to Side.Tracks - 1 do
    for SIdx := 0 to Side.Track[TIdx].Sectors - 1 do
    begin
      Sector := Side.Track[TIdx].Sector[SIdx];

      // Speedlock 1985 (CPC)
      Offset := StrBufPos(Sector.Data, 'SPEEDLOCK PROTECTION SYSTEM (C) 1985 ');
      if Offset > -1 then
      begin
        Result := Format('Speedlock 1985 (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
        exit;
      end;

      // Speedlock 1986 (CPC)
      Offset := StrBufPos(Sector.Data, 'SPEEDLOCK PROTECTION SYSTEM (C) 1986 ');
      if Offset > -1 then
      begin
        Result := Format('Speedlock 1986 (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
        exit;
      end;

      // Speedlock 1987 (CPC)
      Offset := StrBufPos(Sector.Data, 'SPEEDLOCK DISC PROTECTION SYSTEMS COPYRIGHT 1987 ');
      if Offset > -1 then
      begin
        Result := Format('Speedlock disc 1987 (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
        exit;
      end;

      // Speedlock 1987 vD/2.1 (CPC)
      Offset := StrBufPos(Sector.Data, 'SPEEDLOCK PROTECTION SYSTEM (C) 1987 D.LOOKER & D.AUBREY JONES : VERSION D/2.1');
      if Offset > -1 then
      begin
        Result := Format('Speedlock 1987 v2.1 (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
        exit;
      end;

      // Speedlock 1987 (CPC)
      Offset := StrBufPos(Sector.Data, 'SPEEDLOCK PROTECTION SYSTEM (C) 1987 ');
      if Offset > -1 then
      begin
        Result := Format('Speedlock 1987 (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
        exit;
      end;

      // Speedlock +3 1987
      Offset := StrBufPos(Sector.Data, 'SPEEDLOCK +3 DISC PROTECTION SYSTEM COPYRIGHT 1987 SPEEDLOCK ASSOCIATES');
      if Offset > -1 then
      begin
        Result := Format('Speedlock +3 1987 (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
        exit;
      end;

      // Speedlock +3 1988
      Offset := StrBufPos(Sector.Data, 'SPEEDLOCK +3 DISC PROTECTION SYSTEM COPYRIGHT 1988 SPEEDLOCK ASSOCIATES');
      if Offset > -1 then
      begin
        Result := Format('Speedlock +3 1988 (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
        exit;
      end;

      // Speedlock 1988
      Offset := StrBufPos(Sector.Data, 'SPEEDLOCK DISC PROTECTION SYSTEMS (C) 1988 SPEEDLOCK ASSOCIATES');
      if Offset > -1 then
      begin
        Result := Format('Speedlock 1988 (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
        exit;
      end;

      // Speedlock 1989
      Offset := StrBufPos(Sector.Data, 'SPEEDLOCK DISC PROTECTION SYSTEMS (C) 1989 SPEEDLOCK ASSOCIATES');
      if Offset > -1 then
      begin
        Result := Format('Speedlock 1989 (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
        exit;
      end;

      // Speedlock 1990
      Offset := StrBufPos(Sector.Data, 'SPEEDLOCK DISC PROTECTION SYSTEMS (C) 1990 SPEEDLOCK ASSOCIATES');
      if Offset > -1 then
      begin
        Result := Format('Speedlock 1990 (signed T%d/S%d +%d)', [TIdx, SIdx, Offset]);
        exit;
      end;
    end;

  // Unsigned Speedlock +3 1987
  if (Side.Track[0].Sectors = 9) and (Side.Track[1].Sectors = 5) and (Side.Track[1].Sector[0].DataSize = 1024) and
    (Side.Track[0].Sector[6].FDCStatus[2] = 64) and (Side.Track[0].Sector[8].FDCStatus[2] = 0) then
    Result := 'Speedlock +3 1987 (probably, unsigned)';

  // Unsigned Speedlock +3 1988
  if (Side.Track[0].Sectors = 9) and (Side.Track[1].Sectors = 5) and (Side.Track[1].Sector[0].DataSize = 1024) and
    (Side.Track[0].Sector[6].FDCStatus[2] = 64) and (Side.Track[0].Sector[8].FDCStatus[2] = 64) then
    Result := 'Speedlock +3 1988 (probably, unsigned)';

  // Unsigned Speedlock 1989/1990
  if (Side.Track[0].Sectors > 7) and (Side.Tracks > 40) and (Side.Track[1].Sectors = 1) and
    (Side.Track[1].Sector[0].ID = 193) and (Side.Track[1].Sector[0].FDCStatus[1] = 32) then
    Result := 'Speedlock 1989/1990 (probably, unsigned)';

  // Three Inch Loader
  Offset := StrBufPos(Side.Track[0].Sector[0].Data,
    '***Loader Copyright Three Inch Software 1988, All Rights Reserved. Three Inch Software, 73 Surbiton Road, Kingston upon Thames, KT1 2HG***');
  if Offset > -1 then
  begin
    Result := Format('Three Inch Loader type 1 (signed T0/S0 +%d)', [Offset]);
    exit;
  end;

  if Side.Track[0].Sectors > 7 then
  begin
    Offset := StrBufPos(Side.Track[0].Sector[7].Data,
      '***Loader Copyright Three Inch Software 1988, All Rights Reserved. Three Inch Software, 73 Surbiton Road, Kingston upon Thames, KT1 2HG***');
    if Offset > -1 then
    begin
      Result := Format('Three Inch Loader type 1-0-7 (signed T0/S7 +%d)', [Offset]);
      exit;
    end;
  end;

  Offset := StrBufPos(Side.Track[0].Sector[0].Data,
    '***Loader Copyright Three Inch Software 1988, All Rights Reserved. 01-546 2754');
  if Offset > -1 then
  begin
    Result := Format('Three Inch Loader type 2 (signed T0/S0 +%d)', [Offset]);
    exit;
  end;

  // Microprose Soccer
  if (Side.Tracks > 1) and (Side.Track[1].Sectors > 4) then
  begin
    Offset := StrBufPos(Side.Track[1].Sector[4].Data, 'Loader ' + #127 + '1988 Three Inch Software');
    if Offset > -1 then
    begin
      Result := Format('Three Inch Loader type 3-1-4 (signed T1/S4 +%d)', [Offset]);
      exit;
    end;
  end;

  // Laser loader (War in Middle Earth CPC)
  if (Side.Track[0].Sectors > 2) then
  begin
    Offset := StrBufPos(Side.Track[0].Sector[2].Data, 'Laser Load   By C.J.Pink For Consult Computer    Systems');
    if Offset > -1 then
      Result := Format('Laser Load by C.J. Pink (signed T0/S2 +%d)', [Offset]);
  end;

  // W.R.M. (Martech)
  if (Side.Tracks > 9) and (Side.Track[9].Sectors > 9) and (Side.Track[8].Sector[9].DataSize > 128) then
    if StrBufPos(Side.Track[0].Sector[9].Data, 'W.R.M Disc') = 0 then
      if StrBufPos(Side.Track[0].Sector[9].Data, 'Protection') > 0 then
        if StrBufPos(Side.Track[0].Sector[9].Data, 'System (c) 1987') > 0 then
        begin
          Result := 'W.R.M Disc Protection (signed T0/S9 +0)';
          exit;
        end;

  // P.M.S.Loader
  Offset := StrBufPos(Side.Track[0].Sector[0].Data, '[C] P.M.S. 1986');
  if Offset > -1 then
  begin
    Result := Format('P.M.S. 1986 (signed T0/S0 +%d)', [Offset]);
    exit;
  end;

  Offset := StrBufPos(Side.Track[0].Sector[0].Data, 'P.M.S. LOADER [C]1986');
  if Offset > -1 then
  begin
    Result := Format('P.M.S. Loader 1986 v1 (signed T0/S0 +%d)', [Offset]);
    exit;
  end;

  Offset := StrBufPos(Side.Track[0].Sector[0].Data, 'P.M.S.LOADER [C]1986');
  if Offset > -1 then
  begin
    Result := Format('P.M.S. Loader 1986 v2 (signed T0/S0 +%d)', [Offset]);
    exit;
  end;

  Offset := StrBufPos(Side.Track[0].Sector[0].Data, 'P.M.S.LOADER [C]1987');
  if Offset > -1 then
  begin
    Result := Format('P.M.S. 1987 (signed T0/S0 +%d)', [Offset]);
    exit;
  end;

  if ((Side.Tracks > 2) and Side.Track[0].IsFormatted) and (not Side.Track[1].IsFormatted and Side.Track[2].IsFormatted) then
    Result := 'P.M.S. Loader 1986/1987 (maybe, unsigned)';

  // Players?
  for TIdx := 0 to Side.Tracks - 1 do
    if (Side.Track[TIdx].Sectors = 16) then
    begin
      for SIdx := 0 to Side.Track[TIdx].Sectors - 1 do
      begin
        Sector := Side.Track[TIdx].Sector[SIdx];
        if (Sector.ID <> SIdx) or (Sector.FDCSize <> SIdx) then
          break;
      end;
      Result := Format('Players (maybe, super-sized %d byte track %d)', [Side.GetLargestTrackSize(), TIdx]);
    end;

  // Infogrames / Loriciel Gap
  if (Side.Tracks > 39) and (Side.Track[39].Sectors = 9) then
    for SIdx := 0 to Side.Track[39].Sectors - 1 do
    begin
      Sector := Side.Track[39].Sector[SIdx];
      if (Sector.FDCSize = 2) and (Sector.DataSize = 540) then // Can be used with others...
        Result := Format('Infogrames/Logiciel (gap data sector T39/S%d)', [SIdx]);
    end;

  // Rainbow Arts weak sector
  if (Side.Tracks > 40) and (Side.Track[40].Sectors = 9) then
    for SIdx := 0 to Side.Track[40].Sectors - 1 do
    begin
      Sector := Side.Track[40].Sector[SIdx];
      if (Sector.ID = 198) and (Sector.FDCStatus[1] = 32) and (Sector.FDCStatus[2] = 32) then
      begin
        Result := Format('Rainbow Arts (weak sector T40/S%d)', [SIdx]);
        exit;
      end;
    end;

  // Remi Herbulot
  if (Side.Track[0].Sectors > 6) then
    for SIdx := 0 to Side.Track[0].Sectors - 1 do
    begin
      Offset := StrBufPos(Side.Track[0].Sector[SIdx].Data, 'PROTECTION      Remi HERBULOT');
      if Offset > -1 then
      begin
        // Is used in conjuntion with following schemes so do not exit
        Result := Format('ERE/Remi HERBULOT (signed T0/S%d +%d)', [SIdx, Offset]);
      end;

      Offset := StrBufPos(Side.Track[0].Sector[SIdx].Data, 'PROTECTION  V2.1Remi HERBULOT');
      if Offset > -1 then
      begin
        // Is used in conjuntion with following schemes so do not exit
        Result := Format('ERE/Remi HERBULOT 2.1 (signed T0/S%d +%d)', [SIdx, Offset]);
      end;
    end;

  // KBI (CPC)
  LastTIdx := -1;
  for TIdx := 0 to Side.Tracks - 1 do
    if (Side.Track[TIdx].Sectors = 19) then
    begin
      LastTIdx := TIdx;
      Offset := StrBufPos(Side.Track[TIdx].Sector[1].Data, '(c) 1986 for KBI ');
      if Offset > -1 then
      begin
        if (Result <> '') then Result := ' + ' + Result;
        Result := Format('KBI-19 (signed T%d/S1 +%d)%s', [TIdx, Offset, Result]);
        exit;
      end;

      Offset := StrBufPos(Side.Track[TIdx].Sector[0].Data, 'ALAIN LAURENT GENERATION 5 1989');
      if Offset > -1 then
      begin
        if (Result <> '') then Result := ' + ' + Result;
        Result := Format('CAAV (signed T%d/S0 +%d)%s', [TIdx, Offset, Result]);
        exit;
      end;
    end;

  if (LastTIdx > 0) then
  begin
    if (Result <> '') then Result := ' + ' + Result;
    Result := Format('KBI-19 or CAAV (probably, unsigned track %d)%s', [LastTIdx, Result]);
    exit;
  end;

  if (Side.Tracks >= 40) and (Side.Track[39].Sectors = 10) and (Side.Track[38].Sectors = 9) then
  begin
    Sector := Side.Track[39].Sector[9];
    if (Sector.FDCStatus[1] = 32) and (Sector.FDCStatus[2] = 32) then
    begin
      Result := 'KBI-10';
      exit;
    end;
  end;

  // DiscSYS
  LastTIdx := -1;
  for TIdx := 0 to Side.Tracks - 1 do
  begin
    if (Side.Track[TIdx].Sectors = 16) then
      for SIdx := 0 to Side.Track[TIdx].Sectors - 1 do
      begin
        Sector := Side.Track[TIdx].Sector[SIdx];
        if (Sector.ID = SIdx) and (Sector.Track = SIdx) and (Sector.Side = SIdx) and (Sector.ID = SIdx) and
          (Sector.FDCSize = SIdx) then
        begin
          Result := Format('DiscSYS on track %d', [TIdx]);
          LastTIdx := TIdx;
        end;
      end;
  end;

  if LastTIdx > -1 then
    if (Side.Tracks > 2) and (Side.Track[2].Sectors > 4) and (Side.Track[2].Sector[4].DataSize > 160) then
    begin
      Temp := StrBlockClean(Side.Track[2].Sector[4].Data, 85, 22).Trim();
      if Temp.StartsWith('discsys', True) then
      begin
        Result := Result + ' (' + Temp.Substring(8) + ')';
        exit;
      end;
      if Temp.StartsWith('multi-', True) then
      begin
        Result := Result + ' (' + Temp + ')';
        exit;
      end;
    end;

  if LastTIdx = 1 then
    for SIdx := 0 to Side.Track[0].Sectors - 1 do
    begin
      Offset := StrBufPos(Side.Track[0].Sector[SIdx].Data, 'MEAN PROTECTION SYSTEM');
      if Offset > -1 then
      begin
        Result := Format('Mean Protection System (signed T0S%d +%d)', [SIdx, Offset]);
        exit;
      end;
    end;

  // Amsoft/EXOPAL
  if (Side.Tracks > 3) and (Side.Track[3].Sectors > 0) and (Side.Track[3].Sector[0].DataSize = 512) then
  begin
    Offset := StrBufPos(Side.Track[3].Sector[0].Data, 'Amsoft disc protection system');
    if Offset > 1 then
    begin
      Offset := StrBufPos(Side.Track[3].Sector[0].Data, 'EXOPAL');
      if Offset > -1 then
      begin
        Result := Format('Amsoft/EXOPAL (signed T3S0 +%d)', [Offset]);
        exit;
      end;
    end;
  end;

  // Armourloc - possibly anti-hacker rather than anti-copy as no weird sectors
  if (Side.Track[0].Sectors = 9) and (StrBufPos(Side.Track[0].Sector[0].Data, '0K free') = 2) then
  begin
    Result := 'ARMOURLOC';
  end;

  if (Side.Tracks > 3) and (Side.Track[0].IsFormatted) and (not Side.Track[1].IsFormatted) and (Side.Track[2].IsFormatted) then
  begin
    Offset := StrBufPos(Side.Track[0].Sector[0].Data, 'Disc format (c) 1986 Studio B Ltd.');
    if Offset > -1 then
      Result := Format('Studio B Disc format (signed T0S0 +%d)', [Offset]);

    Offset := StrBufPos(Side.Track[2].Sector[0].Data, 'DISCLOC');
    if Offset > -1 then
      Result := Format('DiscLoc/Oddball (signed T2S0 +%d)', [Offset]);
  end;

  // Unknown copy protection
  if (Result = '') and (not side.ParentDisk.IsUniform(True)) and (side.ParentDisk.HasFDCErrors) then
    Result := 'Unknown copy protection';
end;

function DetectInterleave(Track: TDskTrack): string;
var
  LowIdx, NextLowIdx, LowID, NextLowID, SIdx, ExpectedID: byte;
  Interleave: integer;
begin
  LowIdx := 255;
  NextLowIdx := 255;
  Interleave := 0;

  if (Track.Sectors < 3) then
  begin
    Result := ' Too few sectors';
    Exit;
  end;

  // Scan through the track and figure out the lowest two ID's
  LowID := 255;
  NextLowID := 255;
  for SIdx := 0 to Track.Sectors - 1 do
    if (Track.Sector[SIdx].ID < LowID) then
    begin
      NextLowID := LowID;
      NextLowIDX := LowIdx;
      LowID := Track.Sector[SIdx].ID;
      LowIdx := SIdx;
    end
    else
    if (Track.Sector[SIdx].ID < NextLowID) then
    begin
      NextLowID := Track.Sector[SIdx].ID;
      NextLowIdx := SIdx;
    end;

  // Make sure the ID's are sequential
  if (LowIdx < 255) and (NextLowIdx < 255) and (NextLowID = LowID - 1) then
  begin
    // Positive skew (or negative less than sector-count)
    if (LowIdx < NextLowIdx) then
      Interleave := NextLowIdx - LowIdx;
    // Negative skew (or positive greater than sector-count)
    if (LowIdx > NextLowIdx) then
      Interleave := LowIdx - NextLowIdx;
  end
  else
    Result := 'Non-sequential IDs';

  // Confirm the interleave for every sector
  ExpectedID := Track.Sector[0].ID;
  for SIdx := 0 to Track.Sectors - 1 do
    if Track.Sector[SIdx].ID <> ExpectedID then
    begin
      Result := Format('Expected %d but sector %d ID was %d not %d', [Interleave, SIdx, Track.Sector[SIdx].ID, ExpectedID]);
      Exit;
    end;

  Result := Format('%d', [Interleave]);
end;

end.
