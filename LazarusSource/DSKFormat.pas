unit DSKFormat;

interface

const
  // DSK file strings
  DiskInfoStandard = 'MV - CPCEMU Disk-File' + #13 + #10 + 'Disk-Info' + #13 + #10;
  DiskInfoExtended = 'EXTENDED CPC DSK File' + #13 + #10 + 'Disk-Info' + #13 + #10;
  DiskInfoTrack = 'Track-Info' + #13 + #10;
  DiskInfoTrackBroken = 'Track-Info   ';
  DiskSectorOffsetBlock = 'Offset-Info' + #13 + #10;
  CreatorSig = 'SPIN Disk Man';
  CreatorDU54 = 'Disk Image (DU54)' + #13 + #10;

  MaxTracks = 204;

type
  // DSK file format structure
  TDSKInfoBlock = packed record // Disk
    DiskInfoBlock: array[0..33] of char;
    Disk_Creator: array[0..13] of byte;  // diExtendedDSK only
    Disk_NumTracks: byte;
    Disk_NumSides: byte;
    Disk_StdTrackSize: word;            // diStandardDSK only
    Disk_ExtTrackSize: array[0..MaxTracks - 1] of byte; // diExtendedDSK only
  end;

  TTRKInfoBlock = packed record // Track
    TrackData: array[0..12] of char;
    TIB_pad1: array[0..2] of byte;
    TIB_TrackNum: byte;
    TIB_SideNum: byte;
    TIB_DataRate: byte;
    TIB_RecordingMode: byte;
    TIB_SectorSize: byte;
    TIB_NumSectors: byte;
    TIB_GapLength: byte;
    TIB_FillerByte: byte;
    SectorInfoList: array[0..231] of byte;
    //    SectorData: array[0..65535] of byte;     // Read separately to avoid messing where Offset-Info should be
  end;

  TSCTInfoBlock = packed record // Sector
    SIB_TrackNum: byte;
    SIB_SideNum: byte;
    SIB_ID: byte;
    SIB_Size: byte;
    SIB_FDC1: byte;
    SIB_FDC2: byte;
    SIB_DataLength: word;
  end;

  TOFFInfoBlock = packed record // Offset Info (SAMdisk)
    OFF_Marker: array[0..12] of char;
    OFF_Unused: byte;
  end;

  TOFFTrackEntry = packed record
    OFF_TrackLength: word;
  end;

implementation

end.
