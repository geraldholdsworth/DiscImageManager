unit DiscImage;

{
TDiscImage class V1.30
Manages retro disc images, presenting a list of files and directories to the
parent application. Will also extract files and write new files. Almost a complete
filing system in itself. Compatible with Acorn DFS, Acorn ADFS, UEF, Commodore
1541, Commodore 1571, Commodore 1581, and Commodore AmigaDOS.

Copyright (C) 2018-2021 Gerald Holdsworth gerald@hollypops.co.uk

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

{$MODE objFPC}{$H+}

interface

uses Classes,DiscImageUtils,Math,crc,ZStream,StrUtils,Spark;

{$M+}

//The class definition
type
 TDiscImage    = Class
 private
  type
  //Free space map
  TTrack = array of TDIByteArray; //TDIByteArray representing the sectors
  TSide  = array of TTrack;       //Sides
  //Directories
  TDir          = record
   Directory,                       //Directory name (ALL)
   Title       : String;            //Directory title (DFS/ADFS)
   Entries     : array of TDirEntry;//Entries (above)
   Broken      : Boolean;           //Flag if directory is broken (ADFS)
   ErrorCode   : Byte;              //Used to indicate error for broken directory (ADFS)
   Locked      : Boolean;           //Flag if disc is locked (MMFS)
   AFSPartition: Boolean;           //Is this in the AFS partition? (ADFS/AFS)
  end;
  //Collection of directories
  TDisc         = array of TDir;
  //Fragment
  TFragment     = record        //For retrieving the ADFS E/F fragment information
   Offset,
   Length,
   Zone         : Cardinal;
  end;
  //To collate fragments (ADFS/CDR/Amiga)
  TFragmentArray= array of TFragment;
  //Provides feedback
  TProgressProc = procedure(Fupdate: String) of Object;
 private
  FDisc         : TDisc;        //Container for the entire catalogue
  Fdata         : TDIByteArray; //Container for the image to be loaded into
  FDSD,                         //Double sided flag (Acorn DFS)
  FMap,                         //Old/New Map flag (Acorn ADFS) OFS/FFS (Amiga)
  FBootBlock,                   //Is disc an AmigaDOS Kickstart?
  Fupdating,                    //Has BeginUpdate been called?
  Finterleave,                  //Is the disc interleaved (ADFS L)
  FAFSPresent,                  //Is there an AFS partition present? (ADFS)
  FSparkAsFS    : Boolean;      //Deal with Spark archives as a filing system
  secsize,                      //Sector Size
  bpmb,                         //Bits Per Map Bit (Acorn ADFS New)
  nzones,                       //Number of zones (Acorn ADFS New)
  root,                         //Root address (not fragment)
  rootfrag,                     //Root indirect address (Acorn ADFS New)
  Fafsroot,                     //Root address of the AFS root partition
  afshead,                      //Address of the AFS header
  bootmap,                      //Offset of the map (Acorn ADFS)
  zone_spare,                   //Spare bits between zones (Acorn ADFS New)
  format_vers,                  //Format version (Acorn ADFS New)
  root_size,                    //Size of the root directory (Acorn ADFS New)
  afsroot_size,                 //Size of the AFS Root directory
  disc_id,                      //Disc ID (Acorn ADFS)
  emuheader,                    //Allow for any headers added by emulators
  namesize,                     //Size of the name area (Acorn ADFS Big Dir)
  brokendircount,               //Number of broken directories (ADFS)
  FMaxDirEnt    : Cardinal;     //Maximum number of directory entries in image
  disc_size,                    //Size of disc in bytes
  free_space    : Int64;        //Free space remaining
  FForceInter,                  //What to do about ADFS L Interleaving
  FFormat,                      //Format of the image
  secspertrack,                 //Number of sectors per track
  heads,                        //Number of heads (Acorn ADFS New)
  density,                      //Density (Acorn ADFS New)
  idlen,                        //Length of fragment ID in bits (Acorn ADFS New)
  skew,                         //Head skew (Acorn ADFS New)
  lowsector,                    //Lowest sector number (Acorn ADFS New)
  disctype,                     //Type of disc
  FDirType,                     //Directory Type (Acorn ADFS)
  share_size,                   //Share size (Acorn ADFS New)
  big_flag      : Byte;         //Big flag (Acorn ADFS New)
  disc_name,                    //Disc title(s)
  root_name,                    //Root title
  imagefilename,                //Filename of the disc image
  FFilename     : String;       //Copy of above, but doesn't get wiped
  dir_sep       : Char;         //Directory Separator
  free_space_map: TSide;        //Free Space Map
  bootoption    : TDIByteArray; //Boot Option(s)
  CFSFiles      : array of TDIByteArray;//All the data for the CFS files
  FProgress     : TProgressProc;//Used for feedback
  SparkFile     : TSpark;       //For reading in Spark archives
  Fpartitions   : array of TDiscImage;//Used for extra partitions
  procedure ResetVariables;
  function ReadString(ptr,term: Integer;control: Boolean=True): String;
  function ReadString(ptr,term: Integer;var buffer: TDIByteArray;
                                      control: Boolean=True): String; overload;
  function FormatToString: String;
  function FormatToExt: String;
  function ReadBits(offset,start,length: Cardinal): Cardinal;
  procedure WriteBits(value,offset,start,length: Cardinal);
  function RISCOSToTimeDate(filedatetime: Int64): TDateTime;
  function TimeDateToRISCOS(delphitime: TDateTime): Int64;
  function Read32b(offset: Cardinal; bigendian: Boolean=False): Cardinal;
  function Read32b(offset: Cardinal; var buffer: TDIByteArray;
                                      bigendian: Boolean=False): Cardinal; overload;
  function Read24b(offset: Cardinal; bigendian: Boolean=False): Cardinal;
  function Read24b(offset: Cardinal; var buffer: TDIByteArray;
                                      bigendian: Boolean=False): Cardinal; overload;
  function Read16b(offset: Cardinal; bigendian: Boolean=False): Word;
  function Read16b(offset: Cardinal; var buffer: TDIByteArray;
                                      bigendian: Boolean=False): Word; overload;
  function ReadByte(offset: Cardinal): Byte;
  function ReadByte(offset: Cardinal; var buffer: TDIByteArray): Byte; overload;
  procedure Write32b(value, offset: Cardinal; bigendian: Boolean=False); 
  procedure Write32b(value, offset: Cardinal; var buffer: TDIByteArray;
                                      bigendian: Boolean=False); overload;
  procedure Write24b(value, offset: Cardinal; bigendian: Boolean=False);
  procedure Write24b(value, offset: Cardinal; var buffer: TDIByteArray;
                                      bigendian: Boolean=False); overload;
  procedure Write16b(value: Word; offset: Cardinal; bigendian: Boolean=False);
  procedure Write16b(value: Word; offset: Cardinal; var buffer: TDIByteArray;
                                      bigendian: Boolean=False); overload;
  procedure WriteByte(value: Byte; offset: Cardinal);
  procedure WriteByte(value: Byte; offset: Cardinal; var buffer: TDIByteArray); overload;
  function GetDataLength: Cardinal;
  procedure SetDataLength(newlen: Cardinal);
  function ROR13(v: Cardinal): Cardinal;
  procedure ResetDir(var Entry: TDir);
  function MapFlagToByte: Byte;
  function MapTypeToString: String;
  function DirTypeToString: String;
  function GeneralChecksum(offset,length,chkloc,start: Cardinal;carry: Boolean): Cardinal;
  function GetImageCrc: String;
  function GetCRC(var buffer: TDIByteArray): String;
  function GetCRC16(start,len: Cardinal;var buffer: TDIByteArray): Cardinal;
  procedure UpdateProgress(Fupdate: String);
  function GetRootAddress: Cardinal;
  function Inflate(filename: String): TDIByteArray;
  //ADFS Routines
  function ID_ADFS: Boolean;
  function ReadADFSDir(dirname: String; sector: Cardinal): TDir;
  function CalculateADFSDirCheck(sector: Cardinal): Byte;
  function CalculateADFSDirCheck(sector: Cardinal;buffer:TDIByteArray): Byte; overload;
  function NewDiscAddrToOffset(addr: Cardinal;offset:Boolean=True): TFragmentArray;
  function OldDiscAddrToOffset(disc_addr: Cardinal): Cardinal;
  function OffsetToOldDiscAddr(offset: Cardinal): Cardinal;
  function ByteChecksum(offset,size: Cardinal): Byte;
  function ReadADFSDisc: TDisc;
  procedure ADFSFreeSpaceMap;
  procedure ADFSFillFreeSpaceMap(address: Cardinal;usage: Byte);
  function FormatADFSFloppy(minor: Byte): TDisc;
  procedure FormatOldMapADFS(disctitle: String);
  procedure FormatNewMapADFS(disctitle: String);
  function FormatADFSHDD(harddrivesize:Cardinal;newmap:Boolean;dirtype:Byte):TDisc;
  function UpdateADFSDiscTitle(title: String): Boolean;
  function UpdateADFSBootOption(option: Byte): Boolean;
  function ADFSGetFreeFragments(offset:Boolean=True): TFragmentArray;
  function WriteADFSFile(var file_details: TDirEntry;var buffer: TDIByteArray;
                         extend:Boolean=True): Integer;
  function ADFSFindFreeSpace(filelen: Cardinal;var fragid: Cardinal): TFragmentArray;
  function WriteFragmentedData(fragments: TFragmentArray;
                                            var buffer: TDIByteArray): Boolean;
  procedure ADFSAllocateFreeSpace(filelen,freeptr: Cardinal);
  procedure ADFSAllocateFreeSpace(filelen,fragid: Cardinal;fragments: TFragmentArray); overload;
  function ADFSSectorAlignLength(filelen: Cardinal): Cardinal;
  procedure ADFSCalcFileDate(var Entry: TDirEntry);
  function CreateADFSDirectory(var dirname,parent,attributes: String): Integer;
  procedure UpdateADFSCat(directory: String;newname: String='');
  function UpdateADFSFileAttributes(filename,attributes: String): Boolean;
  function ValidateADFSFilename(filename: String): String;
  function RetitleADFSDirectory(filename,newtitle: String): Boolean;
  function RenameADFSFile(oldfilename: String;var newfilename: String):Integer;
  procedure ConsolodateADFSFreeSpaceMap;
  procedure ConsolodateADFSFragments(fragid: Cardinal);
  function DeleteADFSFile(filename: String;
                         TreatAsFile:Boolean=False;extend:Boolean=True):Boolean;
  procedure ADFSDeAllocateFreeSpace(addr,len: Cardinal);
  procedure ADFSDeAllocateFreeSpace(addr: Cardinal); overload;
  function ExtractADFSFile(filename: String;var buffer: TDIByteArray): Boolean;
  function ExtractFragmentedData(fragments: TFragmentArray;
                            filelen: Cardinal;var buffer: TDIByteArray):Boolean;
  function MoveADFSFile(filename,directory: String): Integer;
  function ExtendADFSBigDir(dir: Cardinal;space: Integer;add: Boolean):Boolean;
  function ADFSBigDirSizeChange(dir,entry:Cardinal;extend:Boolean): Boolean;
  function ExtendADFSCat(dir: Cardinal;direntry: TDirEntry): Cardinal;
  procedure ReduceADFSCat(dir,entry: Cardinal);
  function FixBrokenADFSDirectories: Boolean;
  procedure FixADFSDirectory(dir,entry: Integer);
  function ADFSGetHardDriveParams(Ldiscsize:Cardinal;bigmap:Boolean;
              var Lidlen,Lzone_spare,Lnzones,Llog2bpmb,Lroot: Cardinal):Boolean;
  function UpdateADFSFileAddr(filename:String;newaddr:Cardinal;load:Boolean):Boolean;
  function UpdateADFSFileType(filename:String;newtype:String):Boolean;
  function UpdateADFSTimeStamp(filename:String;newtimedate:TDateTime):Boolean;
  //Acorn FileStore Routines
  function ID_AFS: Boolean;
  procedure ReadAFSPartition;
  function ReadAFSDirectory(dirname:String;addr: Cardinal):TDir;
  function ExtractAFSFile(filename: String;var buffer: TDIByteArray): Boolean;
  function AFSDiscAddrToOffset(disc_addr: Cardinal): Cardinal;
  function OffsetToAFSDiscAddr(offset: Cardinal): Cardinal;
  //DFS Routines
  function ID_DFS: Boolean;
  function ReadDFSDisc(mmbdisc:Integer=-1): TDisc;
  procedure DFSFreeSpaceMap(LDisc: TDisc);
  function ConvertDFSSector(address,side: Integer): Integer;
  function WriteDFSFile(file_details: TDirEntry;var buffer: TDIByteArray): Integer;
  procedure UpdateDFSCat(side: Integer);
  function ValidateDFSFilename(filename: String): String;
  function RenameDFSFile(oldfilename: String;var newfilename: String):Integer;
  function DeleteDFSFile(filename: String):Boolean;
  function UpdateDFSFileAttributes(filename,attributes: String): Boolean;
  function FormatDFS(minor,tracks: Byte): TDisc;
  function UpdateDFSDiscTitle(title: String;side: Byte): Boolean;
  function UpdateDFSBootOption(option,side: Byte): Boolean;
  function ExtractDFSFile(filename: String;var buffer: TDIByteArray): Boolean;
  function UpdateDFSFileAddr(filename:String;newaddr:Cardinal;load:Boolean):Boolean;
  //Commodore 1541/1571/1581 Routines
  function ID_CDR: Boolean;
  function ConvertDxxTS(format,track,sector: Integer): Integer;
  function ReadCDRDisc: TDisc;
  function FormatCDR(minor: Byte): TDisc;
  procedure CDRFreeSpaceMap;
  procedure CDRSetClearBAM(track,sector: Byte;used: Boolean);
  function UpdateCDRDiscTitle(title: String): Boolean;
  function ExtractCDRFile(filename:String;var buffer:TDIByteArray): Boolean;
  function WriteCDRFile(file_details: TDirEntry;var buffer: TDIByteArray): Integer;
  procedure UpdateCDRCat;
  function CDRFindNextSector(var track,sector: Byte): Boolean;
  function CDRFindNextTrack(var track,sector: Byte): Boolean;
  function RenameCDRFile(oldfilename: String;var newfilename: String):Integer;
  function DeleteCDRFile(filename: String):Boolean;
  function UpdateCDRFileAttributes(filename,attributes: String): Boolean;
  //Sinclair Spectrum +3/Amstrad Routines
  function ID_Sinclair: Boolean;
  function ReadSinclairDisc: TDisc;
  function FormatSpectrum(minor: Byte): TDisc;
  function WriteSpectrumFile(file_details: TDirEntry;var buffer: TDIByteArray): Integer;
  function RenameSpectrumFile(oldfilename: String;var newfilename: String):Integer;
  function DeleteSinclairFile(filename: String):Boolean;
  function UpdateSinclairFileAttributes(filename,attributes: String): Boolean;
  function UpdateSinclairDiscTitle(title: String): Boolean;
  function ExtractSpectrumFile(filename:String;var buffer:TDIByteArray):Boolean;
  //Commodore Amiga Routines
  function ID_Amiga: Boolean;
  function ReadAmigaDisc: TDisc;
  function ReadAmigaDir(dirname: String; offset: Cardinal): TDir;
  function AmigaBootChecksum(offset: Cardinal): Cardinal;
  function AmigaChecksum(offset: Cardinal): Cardinal;
  function ExtractAmigaFile(filename:String;var buffer:TDIByteArray):Boolean;
  function FormatAmiga(minor: Byte): TDisc;
  function WriteAmigaFile(var file_details: TDirEntry;var buffer: TDIByteArray): Integer;
  function CreateAmigaDirectory(var dirname,parent,attributes: String): Integer;
  function RetitleAmigaDirectory(filename, newtitle: String): Boolean;
  function RenameAmigaFile(oldfilename: String;var newfilename: String):Integer;
  function DeleteAmigaFile(filename: String):Boolean;
  function UpdateAmigaFileAttributes(filename,attributes: String): Boolean;
  function UpdateAmigaDiscTitle(title: String): Boolean;
  function MoveAmigaFile(filename,directory: String): Integer;
  //Acorn CFS Routines
  function ID_CFS: Boolean;
  function ReadUEFFile: TDisc;
  function CFSBlockStatus(status: Byte): String;
  function CFSTargetMachine(machine: Byte): String;
  function ExtractCFSFile(entry: Integer;var buffer:TDIByteArray):Boolean;
  procedure WriteUEFFile(filename: String;uncompress: Boolean=False);
  function FormatCFS:TDisc;
  function DeleteCFSFile(entry: Cardinal): Boolean;
  function UpdateCFSAttributes(entry: Cardinal;attributes:String): Boolean;
  function MoveCFSFile(entry: Cardinal;dest: Integer): Integer;
  function CopyCFSFile(entry: Cardinal;dest: Integer): Integer;
  function WriteCFSFile(var file_details: TDirEntry;var buffer: TDIByteArray): Integer;
  function RenameCFSFile(entry: Cardinal;newfilename: String): Integer;
  function UpdateCFSFileAddr(entry,newaddr:Cardinal;load:Boolean):Boolean;
  //MMFS Routines
  function ID_MMB: Boolean;
  function ReadMMBDisc: TDisc;
  //Spark Routines
  function ID_Spark: Boolean;
  function ReadSparkArchive: TDisc;
  function ExtractSparkFile(filename: String;var buffer: TDIByteArray): Boolean;
  function ReadAFSObject(offset: Cardinal): TDIByteArray;
  const
   //When the change of number of sectors occurs on Commodore 1541/1571 discs
   CDRhightrack : array[0..8] of Integer = (71,66,60,53,36,31,25,18, 1);
   //Number of sectors per track (Commodore 1541/1571)
   CDRnumsects  : array[0..7] of Integer = (17,18,19,21,17,18,19,21);
   //Commodore 64 Filetypes
   CDRFileTypes : array[0.. 5] of String = (
                                   'DELDeleted'  ,'SEQSequence' ,'PRGProgram'  ,
                                   'USRUser File','RELRelative' ,'CBMCBM'      );
   //Disc title for new images
   disctitle = 'DiscImgMgr';
   {$INCLUDE 'DiscImageRISCOSFileTypes.pas'}
 published
  //Methods
  constructor Create;
  function LoadFromFile(filename: String;readdisc: Boolean=True): Boolean;
  function IDImage: Boolean;
  procedure ReadImage;
  procedure SaveToFile(filename: String;uncompress: Boolean=False);
  procedure Close;
  function FormatFDD(major,minor,tracks: Byte): Boolean;
  function FormatHDD(major:Byte;harddrivesize:Cardinal;newmap:Boolean;dirtype:Byte):Boolean;
  function ExtractFile(filename:String;var buffer:TDIByteArray;entry:Cardinal=0): Boolean;
  function WriteFile(var file_details: TDirEntry; var buffer: TDIByteArray): Integer;
  function FileExists(filename: String;var Ref: Cardinal): Boolean;
  function FileExists(filename: String;var dir,entry: Cardinal): Boolean; overload;
  function ReadDiscData(addr,count,side: Cardinal;var buffer): Boolean;
  function WriteDiscData(addr,side: Cardinal;var buffer: TDIByteArray;
                                    count: Cardinal;start: Cardinal=0): Boolean;
  function FileSearch(search: TDirEntry): TSearchResults;
  function RenameFile(oldfilename: String;var newfilename: String;entry: Cardinal=0): Integer;
  function DeleteFile(filename: String;entry: Cardinal=0): Boolean;
  function MoveFile(filename,directory: String): Integer;
  function MoveFile(source: Cardinal;dest: Integer): Integer; overload;
  function CopyFile(filename,directory: String): Integer;
  function CopyFile(filename,directory,newfilename: String): Integer; overload;
  function CopyFile(source: Cardinal;dest: Integer): Integer; overload;
  function UpdateAttributes(filename,attributes: String;entry:Cardinal=0): Boolean;
  function UpdateDiscTitle(title: String;side: Byte): Boolean;
  function UpdateBootOption(option,side: Byte): Boolean;
  function CreateDirectory(var filename,parent,attributes: String): Integer;
  function RetitleDirectory(var filename,newtitle: String): Boolean;
  function GetFileCRC(filename: String;entry:Cardinal=0): String;
  function FixDirectories: Boolean;
  function SaveFilter(var FilterIndex: Integer):String;
  function UpdateLoadAddr(filename:String;newaddr:Cardinal;entry:Cardinal=0): Boolean;
  function UpdateExecAddr(filename:String;newaddr:Cardinal;entry:Cardinal=0): Boolean;
  function TimeStampFile(filename: String;newtimedate: TDateTime): Boolean;
  function ChangeFileType(filename,newtype: String): Boolean;
  function GetFileTypeFromNumber(filetype: Integer): String;
  function GetFileTypeFromName(filetype: String): Integer;
  procedure BeginUpdate;
  procedure EndUpdate;
  function ValidateFilename(parent:String;var filename:String): Boolean;
  function Partition(part: Cardinal):TDiscImage;
  //Properties
  property Disc:                TDisc         read FDisc;
  property FormatString:        String        read FormatToString;
  property FormatNumber:        Byte          read FFormat;
  property FormatExt:           String        read FormatToExt;
  property Title:               String        read disc_name;
  property DiscSize:            Int64         read disc_size;
  property FreeSpace:           Int64         read free_space;
  property DoubleSided:         Boolean       read FDSD;
  property MapType:             Byte          read MapFlagToByte;
  property DirectoryType:       Byte          read FDirType;
  property MapTypeString:       String        read MapTypeToString;
  property DirectoryTypeString: String        read DirTypeToString;
  property DirSep:              Char          read dir_sep;
  property Filename:            String        read imagefilename;
  property FreeSpaceMap:        TSide         read free_space_map;
  property BootOpt:             TDIByteArray  read bootoption;
  property RootAddress:         Cardinal      read GetRootAddress;
  property AFSRoot:             Cardinal      read Fafsroot;
  property CRC32:               String        read GetImageCrc;
  property ProgressIndicator:   TProgressProc write FProgress;
  property ADFSInterleaved:     Byte          read FForceInter write FForceInter;
  property MaxDirectoryEntries: Cardinal      read FMaxDirEnt;
  property SparkAsFS:           Boolean       read FSparkAsFS write FSparkAsFS;
  property AFSPresent:          Boolean       read FAFSPresent;
 public
  destructor Destroy; override;
 End;

implementation

uses
 SysUtils,DateUtils;

//++++++++++++++++++ Class definition starts here ++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Reset all the variables
-------------------------------------------------------------------------------}
procedure TDiscImage.ResetVariables;
begin
 //Default values
 SetLength(FDisc,0);
 FDSD          :=False;
 FMap          :=False;
 FBootBlock    :=True;
 FAFSPresent   :=False;
 secsize       :=$0100;
 bpmb          :=$0000;
 nzones        :=$0000;
 root          :=$0000;
 Fafsroot      :=$0000;
 bootmap       :=$0000;
 zone_spare    :=$0000;
 format_vers   :=$0000;
 root_size     :=$0000;
 afsroot_size  :=$0000;
 disc_id       :=$0000;
 disc_size     :=$0000;
 free_space    :=$0000;
 FFormat       :=diInvalidImg;
 secspertrack  :=$10;
 heads         :=$00;
 density       :=$00;
 idlen         :=$00;
 skew          :=$00;
 SetLength(bootoption,0);
 lowsector     :=$00;
 disctype      :=$00;
 FDirType      :=diUnknownDir;
 share_size    :=$00;
 big_flag      :=$00;
 disc_name     :='';
 emuheader     :=$0000;
 dir_sep       :='.';
 root_name     :='$';
 imagefilename :='';
 SetLength(free_space_map,0);
 Fupdating     :=False;
end;

{-------------------------------------------------------------------------------
Extract a string from ptr to the next chr(term) or length(-term)
-------------------------------------------------------------------------------}
function TDiscImage.ReadString(ptr,term: Integer;control: Boolean=True): String;
var
 buffer: TDIByteArray;
begin
 buffer:=nil;
 Result:=ReadString(ptr,term,buffer,control);
end;
function TDiscImage.ReadString(ptr,term: Integer;var buffer:TDIByteArray;
                                  control: Boolean=True): String;
var
 x : Integer;
 c,
 r : Byte;
begin
 //Counter
 x:=0;
 //Dummy result
 Result:='';
 //Are we excluding control characters?
 if control then c:=32 else c:=0;
 //Start with the first byte (we pre-read it to save multiple reads)
 r:=ReadByte(ptr+x,buffer);
 while (r>=c) and //Test for control character
       (((r<>term) and (term>=0)) or //Test for terminator character
        ((x<abs(term)) and (term<0))) do //Test for string length
 begin
  Result:=Result+chr(r); //Add it to the string
  inc(x);                //Increase the counter
  r:=ReadByte(ptr+x,buffer);    //Read the next character
 end;
end;

{-------------------------------------------------------------------------------
Convert a format byte to a string
-------------------------------------------------------------------------------}
function TDiscImage.FormatToString: String;
const
 FS  : array[0..8] of String = ('DFS',
                                'Acorn ADFS',
                                'Commodore',
                                'Sinclair Spectrum +3/Amstrad',
                                'Commodore Amiga',
                                'Acorn CFS',
                                'MMFS',
                                'Acorn File Store',
                                'Spark Archive');
 SUB : array[0..8] of array[0..15] of String =
 (('Acorn SSD','Acorn DSD','Watford SSD','Watford DSD','','','','','','','','','','','',''),
  ('S','M','L','D','E','E+','F','F+','','','','','','','','Hard Disc'),
  ('1541','1571','1581','1541 40 Track','1571 80 Track','','','','','','','','','','',''),
  ('','Extended','','','','','','','','','','','','','',''),
  ('DD','HD','','','','','','','','','','','','','','Hard Disc'),
  ('','','','','','','','','','','','','','','',''),
  ('','','','','','','','','','','','','','','',''),
  ('Level 1','Level 2','Level 3','Level 4','','','','','','','','','','','',''),
  ('','','','','','','','','','','','','','','',''));
begin
 Result:='';
 if FFormat>>4<=High(FS) then
  if FFormat mod $10<=High(SUB[FFormat>>4]) then
  begin
   Result:= FS[FFormat>>4];
   if SUB[FFormat>>4,FFormat MOD $10]<>'' then
    Result:=Result+' '+SUB[FFormat>>4,FFormat MOD $10];
  end;
end;

{-------------------------------------------------------------------------------
Convert a format byte to an extension
-------------------------------------------------------------------------------}
function TDiscImage.FormatToExt: String;
const
 EXT : array[0..8] of array[0..15] of String =
 (('ssd','dsd','ssd','dsd','','','','','','','','','','','',''),
  ('ads','adm','adl','adf','adf','adf','adf','adf','','','','','','','dat','hdf'),
  ('d64','d71','d81','d64','d71','','','','','','','','','','',''),
  ('','dsk','','','','','','','','','','','','','',''),
  ('adf','adf','','','','','','','','','','','','','','hdf'),
  ('uef','','','','','','','','','','','','','','',''),
  ('mmb','','','','','','','','','','','','','','',''),
  ('afs','afs','afs','afs','','','','','','','','','','','','afs'),
  ('zip','','','','','','','','','','','','','','',''));
begin
 Result:='img';
 if FFormat>>4<=High(EXT) then
  if FFormat mod $10<=High(EXT[FFormat>>4]) then
   Result:=EXT[FFormat>>4,FFormat MOD $10];
end;

{-------------------------------------------------------------------------------
Read upto 32 bits of data from the buffer, starting at offset(bytes)+start(bits)
-------------------------------------------------------------------------------}
function TDiscImage.ReadBits(offset,start,length: Cardinal): Cardinal;
var
 start_byte,
 start_bit,
 bit,b,pos  : Cardinal;
 lastbyte   : Byte;
begin
 //Reset the result
 Result:=0;
 //If the length is 0, nothing to read. Cardinals are 32 bits
 //(we could use Integers, but these are signed)
 if (length>0) and (length<33) then
 begin
  //Initialise the variables
  pos:=$FFFFFFFF;
  lastbyte:=0;
  //Iterate through the required number of bits
  for bit:=0 to length-1 do
  begin
   //Work out the byte offset, and the bit within
   start_byte:=(start+bit) div 8;
   start_bit :=(start+bit) mod 8;
   //And increase the result with the extracted bit, shifted right to account
   //for final position
   if pos<>offset+start_byte then
   begin
    //To save re-reading the same byte over and over
    pos:=offset+start_byte;
    lastbyte:=ReadByte(pos);
   end;
   b:=(lastbyte AND (1 shl start_bit))shr start_bit; //Read that bit
   inc(Result,b shl bit);                            //Add to the result
  end;
 end;
end;

{-------------------------------------------------------------------------------
Write upto 32 bits of data from the buffer, starting at offset(bytes)+start(bits)
-------------------------------------------------------------------------------}
procedure TDiscImage.WriteBits(value,offset,start,length: Cardinal);
var
 start_byte,
 start_bit,
 bit,
 b,c,
 pos        : Cardinal;
 lastbyte,
 lastcopy   : Byte;
begin
 //If the length is 0, nothing to write. Cardinals are 32 bits
 //(we could use Integers, but these are signed)
 if(length>0)and(length<33)then
 begin
  //Initialise the variables
  pos:=$FFFFFFFF;
  lastbyte:=0;
  lastcopy:=$FF;
  //Iterate through the required number of bits
  for bit:=0 to length-1 do
  begin
   //Work out the byte offset, and the bit within
   start_byte:=(start+bit) div 8;
   start_bit :=(start+bit) mod 8;
   //And increase the result with the extracted bit, shifted right to account
   //for final position
   if pos<>offset+start_byte then
   begin
    //To save re-reading the same byte over and over
    pos:=offset+start_byte;
    lastbyte:=ReadByte(pos);
    lastcopy:=lastbyte; //Take a copy to see if we need to write
   end;
   b:=((value AND (1 shl bit))shr bit)shl start_bit; //Bit to set
   c:=(1 shl start_bit)XOR$FF;                       //Bit to clear
   lastbyte:=(lastbyte AND c)OR b;                   //Set/clear the bit
   //Then write the byte back, if it has changed
   if lastbyte<>lastcopy then
   begin
    WriteByte(lastbyte,pos);
    lastcopy:=lastbyte; //Take a copy of the changed byte
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Converts a RISC OS Time/Date to a Delphi TDateTime
-------------------------------------------------------------------------------}
function TDiscImage.RISCOSToTimeDate(filedatetime: Int64): TDateTime;
var
 epoch      : TDateTime;
 riscosdays : Int64;
const
 dayincsec = 8640000; //24*3600*100 centi seconds = 1 day
begin
 //RISC OS counts from 00:00:00 1st January 1900
 epoch:=EncodeDateTime(1900,01,01,00,00,00,000);
 //Number of days in file timestamp
 riscosdays:=filedatetime div dayincsec;
 //Convert to Delphi TDateTime
 Result:=riscosdays+epoch                                 //Whole part
        +((filedatetime-riscosdays*dayincsec)/dayincsec); //Fraction part
end;

{-------------------------------------------------------------------------------
Converts a Delphi TDateTime to a RISC OS Time/Date
-------------------------------------------------------------------------------}
function TDiscImage.TimeDateToRISCOS(delphitime: TDateTime): Int64;
var
 days,time  : Int64;
const
 dayincsec = 8640000; //24*3600*100 centi seconds = 1 day
begin
 days:=Floor(delphitime);
 time:=Floor((delphitime-days)*dayincsec);
 dec(days,2);
 Result:=(days*dayincsec)+time;
end;

//Read/Write routines to/from the data - these are provided to make it easier to
//convert the class to keeping the image open and access the data as and when
//required rather than storing the entire thing in RAM.
//Methods: ReadByte, Read16b, Read24b, Read32b,
//         WriteByte, Write16b, Write24b, Write 32b,
//         GetDataLength, SetDataLength

{-------------------------------------------------------------------------------
Read in 4 bytes (word)
-------------------------------------------------------------------------------}
function TDiscImage.Read32b(offset: Cardinal; bigendian: Boolean=False): Cardinal;
var
 buffer: TDIByteArray;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Result:=Read32b(offset,buffer,bigendian);
end;
function TDiscImage.Read32b(offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False): Cardinal;
var
 i: Cardinal;
const
 x = 3;
begin
 Result:=0; //Default value
 if bigendian then
  //Big Endian
  for i:=0 to x do inc(Result,ReadByte(offset+(x-i),buffer)<<(8*i))
 else
  //Little Endian
  for i:=0 to x do inc(Result,ReadByte(offset+i,buffer)<<(8*i));
end;

{-------------------------------------------------------------------------------
Read in 3 bytes
-------------------------------------------------------------------------------}
function TDiscImage.Read24b(offset: Cardinal; bigendian: Boolean=False): Cardinal;
var
 buffer: TDIByteArray;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Result:=Read24b(offset,buffer,bigendian);
end;
function TDiscImage.Read24b(offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False): Cardinal;
var
 i: Cardinal;
const
 x = 2;
begin
 Result:=0; //Default value
 if bigendian then
  //Big Endian
  for i:=0 to x do inc(Result,ReadByte(offset+(x-i),buffer)<<(8*i))
 else
  //Little Endian
  for i:=0 to x do inc(Result,ReadByte(offset+i,buffer)<<(8*i));
end;

{-------------------------------------------------------------------------------
Read in 2 bytes
-------------------------------------------------------------------------------}
function TDiscImage.Read16b(offset: Cardinal; bigendian: Boolean=False): Word;
var
 buffer: TDIByteArray;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Result:=Read16b(offset,buffer,bigendian);
end;
function TDiscImage.Read16b(offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False): Word;
var
 i: Cardinal;
const
 x = 1;
begin
 Result:=0; //Default value
 if bigendian then
  //Big Endian
  for i:=0 to x do inc(Result,ReadByte(offset+(x-i),buffer)<<(8*i))
 else
  //Little Endian
  for i:=0 to x do inc(Result,ReadByte(offset+i,buffer)<<(8*i));
end;

{-------------------------------------------------------------------------------
Read in a byte
-------------------------------------------------------------------------------}
function TDiscImage.ReadByte(offset: Cardinal): Byte;
var
 buffer: TDIByteArray;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Result:=ReadByte(offset,buffer);
end;
function TDiscImage.ReadByte(offset: Cardinal;var buffer: TDIByteArray): Byte;
begin
 Result:=$FF;
 if buffer<>nil then
  if offset<Length(buffer) then Result:=buffer[offset];
 //If no buffer has been passed, resort to the standard function
 if buffer=nil then
 begin
  //Compensate for interleaving (ADFS L)
  if FFormat=diAcornADFS<<4+$02 then offset:=OldDiscAddrToOffset(offset);
  //Compensate for interleaving (AFS Floppy)
  if FFormat=diAcornFS<<4+$02   then offset:=AFSDiscAddrToOffset(offset);
  //Compensate for emulator header
  inc(offset,emuheader);
  //If we are inside the data, read the byte
  if offset<Length(Fdata) then Result:=Fdata[offset];
 end;
end;

{-------------------------------------------------------------------------------
Write 4 bytes (word)
-------------------------------------------------------------------------------}
procedure TDiscImage.Write32b(value, offset: Cardinal; bigendian: Boolean=False);
var
 buffer: TDIByteArray;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Write32b(value,offset,buffer,bigendian);
end;
procedure TDiscImage.Write32b(value, offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False);
var
 i: Cardinal;
const
 x = 3;
begin
 if bigendian then
  //Big Endian
  for i:=0 to x do WriteByte((value>>(i*8))mod$100,offset+(x-i),buffer)
 else
  //Little Endian
  for i:=0 to x do WriteByte((value>>(i*8))mod$100,offset+i,buffer);
end;

{-------------------------------------------------------------------------------
Write 3 bytes
-------------------------------------------------------------------------------}
procedure TDiscImage.Write24b(value,offset: Cardinal; bigendian: Boolean=False);
var
 buffer: TDIByteArray;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Write24b(value,offset,buffer,bigendian);
end;
procedure TDiscImage.Write24b(value,offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False);
var
 i: Cardinal;
const
 x = 2;
begin
 if bigendian then
  //Big Endian
  for i:=0 to x do WriteByte((value>>(i*8))mod$100,offset+(x-i),buffer)
 else
  //Little Endian
  for i:=0 to x do WriteByte((value>>(i*8))mod$100,offset+i,buffer);
end;

{-------------------------------------------------------------------------------
Write 2 bytes
-------------------------------------------------------------------------------}
procedure TDiscImage.Write16b(value: Word; offset: Cardinal; bigendian: Boolean=False);
var
 buffer: TDIByteArray;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Write16b(value,offset,buffer,bigendian);
end;
procedure TDiscImage.Write16b(value: Word; offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False);
var
 i: Cardinal;
const
 x = 1;
begin
 if bigendian then
  //Big Endian
  for i:=0 to x do WriteByte((value>>(i*8))mod$100,offset+(x-i),buffer)
 else
  //Little Endian
  for i:=0 to x do WriteByte((value>>(i*8))mod$100,offset+i,buffer);
end;

{-------------------------------------------------------------------------------
Write byte
-------------------------------------------------------------------------------}
procedure TDiscImage.WriteByte(value: Byte; offset: Cardinal);
var
 buffer: TDIByteArray;
begin
 SetLength(buffer,0);
 WriteByte(value,offset,buffer);
end;
procedure TDiscImage.WriteByte(value: Byte; offset: Cardinal;var buffer: TDIByteArray);
begin
 if buffer=nil then
 begin
  //Compensate for interleaving (ADFS L)
  if FFormat=diAcornADFS<<4+$02 then offset:=OldDiscAddrToOffset(offset);
  //Compensate for interleaving (AFS Floppy)
  if FFormat=diAcornFS<<4+$02   then offset:=AFSDiscAddrToOffset(offset);
  //Compensate for emulator header
  inc(offset,emuheader);
  //Will it go beyond the size of the array?
  if offset<Length(Fdata) then
   Fdata[offset]:=value; //Write the byte
 end
 else
  //Will it go beyond the size of the array?
  if offset<Length(buffer) then
   buffer[offset]:=value; //Write the byte
end;

{-------------------------------------------------------------------------------
Gets the length of the data
-------------------------------------------------------------------------------}
function TDiscImage.GetDataLength: Cardinal;
begin
 Result:=Length(Fdata);
end;

{-------------------------------------------------------------------------------
Sets the length of the data
-------------------------------------------------------------------------------}
procedure TDiscImage.SetDataLength(newlen: Cardinal);
begin
 SetLength(Fdata,newlen);
end;

{-------------------------------------------------------------------------------
Rotate Right 13 bits
-------------------------------------------------------------------------------}
function TDiscImage.ROR13(v: Cardinal): Cardinal;
begin
 //Shift right 13 bits OR shift left 32-13=19 bits
 Result:=(v shr 13) OR (v shl 19);
end;

{-------------------------------------------------------------------------------
Reset a TDir to blank
-------------------------------------------------------------------------------}
procedure TDiscImage.ResetDir(var Entry: TDir);
begin
 with Entry do
 begin
  Directory   :='';
  Title       :='';
  SetLength(Entries,0);
  Broken      :=False;
  ErrorCode   :=$00;
  Locked      :=False;
  AFSPartition:=False;
 end;
end;

{-------------------------------------------------------------------------------
Convert the Map flag to Map Type
-------------------------------------------------------------------------------}
function TDiscImage.MapFlagToByte: Byte;
begin
 Result:=$FF;                   //Default value for non-ADFS
 if FFormat>>4=diAcornADFS then //Is it ADFS?
 begin
  Result:=diADFSOldMap;              // ADFS Old Map
  if FMap then Result:=diADFSNewMap; // ADFS New Map
 end;
 if FFormat>>4=diAmiga then     //Is it Amiga?
 begin
  Result:=diAmigaOFS;                // AmigaDOS OFS
  if FMap then Result:=diAmigaFFS;   // AmigaDOS FFS
 end;
end;

{-------------------------------------------------------------------------------
Convert the Map flag to String
-------------------------------------------------------------------------------}
function TDiscImage.MapTypeToString: String;
begin
 Result:='';
 case MapFlagToByte of
  diADFSOldMap: Result:='ADFS Old Map';
  diADFSNewMap: Result:='ADFS New Map';
  diAmigaOFS  : Result:='AmigaDOS OFS';
  diAmigaFFS  : Result:='AmigaDOS FFS';
 end;
end;

{-------------------------------------------------------------------------------
Convert the Directory Type to String
-------------------------------------------------------------------------------}
function TDiscImage.DirTypeToString: String;
begin
 Result:='';
 case FDirType of
  diADFSOldDir: Result:='ADFS Old Directory';
  diADFSNewDir: Result:='ADFS New Directory';
  diADFSBigDir: Result:='ADFS Big Directory';
  diAmigaDir  : Result:='AmigaDOS Directory';
  diAmigaCache: Result:='AmigaDOS Directory Cache';
 end;
end;

{-------------------------------------------------------------------------------
Calculate Generic checksum - used by AmigaDOS and ADFS New Map checksums
-------------------------------------------------------------------------------}
function TDiscImage.GeneralChecksum(offset,length,chkloc,start: Cardinal;carry: Boolean): Cardinal;
var
 pointer,
 word    : Cardinal;
 acc     : Int64;
begin
 //Reset the accumulator to zero
 acc:=0;
 //Start the offset at 0+offset
 pointer:=start;
 repeat
  //Do not include the checksum itself
  if pointer<>chkloc then
  begin
   //Read the word
   word:=Read32b(offset+pointer,start=0);
   //Add each word to the accumulator
   inc(acc,word);
  end;
  //Move onto the next word
  inc(pointer,4);
  //Until the entire section is done.
 until pointer>=length;
 //Reduce from 64 bits to 32 bits
 word:=(acc MOD $100000000);
 if carry then inc(word,acc DIV $100000000);
 //Add the first word, if skipped, ignoreing the first byte (checksum)
 if start=$4 then
 begin
  inc(word,Read32b(offset) AND $FFFFFF00);
  Result:=((word AND $000000FF)
      XOR ((word AND $0000FF00) shr  8)
      XOR ((word AND $00FF0000) shr 16)
      XOR ((word AND $FF000000) shr 24))AND $FF;
 end
 else Result:=word;
end;

{-------------------------------------------------------------------------------
Calculate a CRC-32 for the image
-------------------------------------------------------------------------------}
function TDiscImage.GetImageCrc: String;
begin
 Result:=GetCRC(FData);
end;

{-------------------------------------------------------------------------------
Calculate a CRC-32 for the supplied buffer Byte array
-------------------------------------------------------------------------------}
function TDiscImage.GetCRC(var buffer: TDIByteArray): String;
var
 CRCValue: longword;
begin
 CRCValue:=crc.crc32(0,nil,0);
 CRCValue:=crc.crc32(0,@buffer[0],Length(buffer));
 Result  :=IntToHex(CRCValue,8);
end;

{-------------------------------------------------------------------------------
Calculate the CRC-16 value
-------------------------------------------------------------------------------}
function TDiscImage.GetCRC16(start,len: Cardinal;var buffer: TDIByteArray): Cardinal;
var
 addr: Cardinal;
 bit : Byte;
begin
 //Converted from the BBC BASIC version by J.G.Harston
 //mdfs.net
 Result:=0;
 for addr:=start to start+len-1 do
 begin
  if Length(buffer)>0 then
   Result:=Result XOR $100*buffer[addr] //EOR with current byte (supplied data)
  else
   Result:=Result XOR $100*ReadByte(addr);//EOR with current byte (file data)
  for bit:=1 to 8 do                    //Loop through 8 bits
  begin
   Result:=Result shl 1;                //Move CRC up one bit
   if Result AND $10000=$10000 then
    Result:=Result XOR $11021;          //EOR with XMODEM polynomic
  end;                                  //Ensuring CRC remains 16-bit value
 end;
 //Swap the MSB and LSB around
 Result:=((Result mod $100)*$100)+(Result div $100);
end;

{-------------------------------------------------------------------------------
Update the progress indicator
-------------------------------------------------------------------------------}
procedure TDiscImage.UpdateProgress(Fupdate: String);
begin
 //If the main program has defined a procedure then call it
 if Assigned(FProgress) then FProgress(Fupdate);
end;

{-------------------------------------------------------------------------------
Return the root address, depending on format
-------------------------------------------------------------------------------}
function TDiscImage.GetRootAddress: Cardinal;
begin
 Result:=root;
 if FFormat>>4=diAcornADFS then //New map will return the fragment ID
  if FMap then Result:=rootfrag;
end;

{-------------------------------------------------------------------------------
Inflate a GZip file, or just read the file into a buffer
-------------------------------------------------------------------------------}
function TDiscImage.Inflate(filename: String): TDIByteArray;
 function L_Inflate(Source: String): TDIByteArray;
 var
  GZ     : TGZFileStream;
  chunk  : TDIByteArray;
  cnt,
  i,
  buflen : Integer;
 const
   ChunkSize=4096; //4K chunks
 begin
  //Initialise the variables
  Result:=nil;
  chunk:=nil;
  //Open the stream
  try
   GZ:=TGZFileStream.Create(Source,gzOpenRead);
   //This is our length counter
   buflen:=0;
   //We'll be reading it in chunks
   SetLength(chunk,ChunkSize);
   repeat
    //Read in the next chunk
    cnt:=GZ.Read(chunk[0],ChunkSize);
    //Extend the buffer accordingly
    SetLength(Result,buflen+cnt);
    //Copy the chunk into the buffer
    for i:=0 to cnt-1 do Result[buflen+i]:=chunk[i];
    //Increase the buffer length counter
    inc(buflen,cnt);
    //Until we are done
   until cnt<ChunkSize;
   //Free up the stream
  except
  end;
  GZ.Free;
 end;
var
 F        : TFileStream;
 buffer,
 inflated : TDIByteArray;
 ptr,i,old: Cardinal;
 blockptrs: array of Cardinal;
 fn       : String;
begin
 buffer   :=nil;
 blockptrs:=nil;
 inflated :=nil;
 Result   :=nil;
 //Read in the entire file
 try
  F:=TFileStream.Create(filename,fmOpenRead or fmShareDenyNone);
  SetLength(buffer,F.Size);
  F.Read(buffer[0],F.Size);
 except
 end;
 F.Free;
 //First, is it actually a GZip file?
 if(buffer[$00]=$1F)and(buffer[$01]=$8B)and(buffer[$02]=$08)then
 begin
  //Count how many blocks and make note of their positions
  for ptr:=0 to Length(buffer)-10 do
   if(buffer[ptr]=$1F)and(buffer[ptr+1]=$8B)and(buffer[ptr+2]=$08)then
   begin
    //Make a note of the position
    SetLength(blockptrs,Length(blockptrs)+1);
    blockptrs[Length(blockptrs)-1]:=ptr;
   end;
 end;
 //Separate each block, if more than one
 if Length(blockptrs)>1 then
 begin
  //Add the file end to the end of the block pointers
  SetLength(blockptrs,Length(blockptrs)+1);
  blockptrs[Length(blockptrs)-1]:=Length(buffer);
  //Set up the container for the inflated file
  SetLength(Result,0);
  //Get a temporary filename
  fn:=GetTempDir+ExtractFileName(filename);
  //Iterate through the pointers
  for i:=0 to Length(blockptrs)-2 do
  begin
   //Create the temporary file and write the block to it
   try
    F:=TFileStream.Create(fn,fmCreate);
    F.Write(buffer[blockptrs[i]],blockptrs[i+1]-blockptrs[i]);
   except
   end;
   F.Free;
   //Inflate the block
   inflated:=L_Inflate(fn);
   old:=Length(Result); //Previous length of the inflated file
   //Increase the inflated file buffer to accomodate
   SetLength(Result,Length(Result)+Length(inflated));
   //Move the inflated data across
   for ptr:=0 to Length(inflated)-1 do Result[old+ptr]:=inflated[ptr];
  end;
  //Delete the temporary file
  if SysUtils.FileExists(fn) then DeleteFile(fn);
 end;
 //If just the one block, then don't bother splitting
 if Length(blockptrs)=1 then Result:=L_Inflate(filename);
 //If there are no blocks, then just return the entire file
 if Length(blockptrs)=0 then Result:=buffer;
end;

//++++++++++++++++++ Published Methods +++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Create the instance
-------------------------------------------------------------------------------}
constructor TDiscImage.Create;
begin
 inherited;
 //This just sets all the global and public variables to zero, or blank.
 ResetVariables;
 SetDataLength(0);
 SetLength(Fpartitions,0);
 //ADFS Interleaving option
 FForceInter:=0;
 //Deal with Spark archives as a filing system (i.e. in this class)
 FSparkAsFS:=True;
end;

{-------------------------------------------------------------------------------
Free the instance
-------------------------------------------------------------------------------}
destructor TDiscImage.Destroy;
begin
 Close;
 inherited;
end;

{-------------------------------------------------------------------------------
Calculate a CRC-32 for a file
-------------------------------------------------------------------------------}
function TDiscImage.GetFileCrc(filename: String;entry:Cardinal=0): String;
var
 buffer: TDIByteArray;
begin
 Result:='error';
 if ExtractFile(filename,buffer,entry) then Result:=GetCRC(buffer);
end;

{-------------------------------------------------------------------------------
Attempt to fix directories (entry point)
-------------------------------------------------------------------------------}
function TDiscImage.FixDirectories: Boolean;
begin
 Result:=False;
 //Only for ADFS
 if FFormat>>4=diAcornADFS then Result:=FixBrokenADFSDirectories;
end;

{-------------------------------------------------------------------------------
Construct a save filter
-------------------------------------------------------------------------------}
function TDiscImage.SaveFilter(var FilterIndex: Integer):String;
var
 currentformat,
 queryformat   : Byte;
 ext           : String;
 index         : Integer;
begin
 Result:='';
 //Save the current format
 currentformat:=FFormat;
 index:=0;
 for queryformat:=$00 to $8F do
 begin
  //Set the format
  FFormat:=queryformat;
  //Get the current extension
  ext:=FormatToExt;
  //If it is empty, skip
  if ext<>'' then
  begin
   //If the current string is not empty, add a delimiter
   if Result<>'' then Result:=Result+'|';
   //Add the format string and extension
   ext:=FormatToString+'|*.'+ext;
   Result:=Result+ext;
   inc(index);
   //Set the filter index, if we are at the current format
   if currentformat=queryformat then FilterIndex:=index;
  end;
 end;
 //Restore the current format
 FFormat:=currentformat;
end;

{-------------------------------------------------------------------------------
Load an image from a file, unGZIPping it, if necessary
-------------------------------------------------------------------------------}
function TDiscImage.LoadFromFile(filename: String;readdisc: Boolean=True): Boolean;
begin
 Result:=False;
 //Only read the file in if it actually exists (or rather, Windows can find it)
 if SysUtils.FileExists(filename) then
 begin
  //Make a note of the file being read in
  FFilename:=filename;
  //Free up the Spark instance, if used
  if FFormat>>4=diSpark then SparkFile.Free;
  //Blank off the variables
  ResetVariables;
  //Read the file in, uncompressing if need be
  FData:=Inflate(filename);
  //ID the image
  if(IDImage)and(readdisc)then ReadImage;
  //Return a true or false, depending on if FFormat is set.
  Result:=FFormat<>diInvalidImg;
  //Set the image filename, if a valid image
  If Result then imagefilename:=filename;
 end;
end;

{-------------------------------------------------------------------------------
ID an image
-------------------------------------------------------------------------------}
function TDiscImage.IDImage: Boolean;
begin
 //Reset the variables
 ResetVariables;
 //This check is done in the ID functions anyway, but we'll do it here also
 if GetDataLength>0 then
 begin
  //ID the type of image, from the data contents
  //These need to be ID-ed in a certain order as one type can look like another
  if not ID_ADFS     then //Acorn ADFS
  if not ID_AFS      then //AFS
  if not ID_CDR      then //Commodore
  if not ID_Amiga    then //Amiga
  if not ID_DFS      then //Acorn DFS
  if not ID_Sinclair then //Sinclair/Amstrad
  if not ID_CFS      then //Acorn CFS
  if not ID_MMB      then //MMFS
  if not ID_Spark    then //Spark archive
   ResetVariables;        //Reset everything
  //Just by the ID process:
  //ADFS 'F' can get mistaken for Commodore
  //Commodore and Amiga can be mistaken for DFS
 end;
 //Return a true or false result
 Result:=FFormat<>diInvalidImg;
end;

{-------------------------------------------------------------------------------
Read the disc in, depending on the format
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadImage;
var
 d: Cardinal;
begin
 case FFormat>>4 of
  diAcornDFS : FDisc:=ReadDFSDisc;     //Acorn DFS
  diAcornADFS: FDisc:=ReadADFSDisc;    //Acorn ADFS
  diCommodore: FDisc:=ReadCDRDisc;     //Commodore
  diSinclair : FDisc:=ReadSinclairDisc;//Sinclair/Amstrad
  diAmiga    : FDisc:=ReadAmigaDisc;   //Amiga
  diAcornUEF : FDisc:=ReadUEFFile;     //Acorn CFS
  diMMFS     : FDisc:=ReadMMBDisc;     //MMFS
  diSpark    : FDisc:=ReadSparkArchive;//Spark archive
 end;
 //Is this an ADFS L, with broken directories and is interleaved?
 if FForceInter=0 then //Automatic detection
 begin
  if(FFormat>>4=diAcornADFS)and(brokendircount>0)then
  begin
   //Try with it the opposite setting
   Finterleave:=not Finterleave;
   FDisc:=ReadADFSDisc;
   //Did that fail, then try as AFS
   if brokendircount>0 then
   begin
    Finterleave:=not Finterleave;
    //ResetVariables;
    //If not AFS, just revert
    {if not ID_AFS then
    begin
     ResetVariables;
     ID_ADFS;}
     FDisc:=ReadADFSDisc;
    //end;
   end;
  end;
 end;
 ReadAFSPartition; //Read in the AFS partition, if ADFS and one is present
 //Find the maximum directory entries
 if FFormat>>4<>diSpark then //Not Spark, as this already provides this info
 begin
  FMaxDirEnt:=0;
  if Length(FDisc)>0 then
   for d:=0 to Length(FDisc)-1 do
    if Length(FDisc[d].Entries)>FMaxDirEnt then
     FMaxDirEnt:=Length(FDisc[d].Entries);
 end
 else FMaxDirEnt:=SparkFile.MaxDirEnt;
end;

{-------------------------------------------------------------------------------
Saves an image to a file
-------------------------------------------------------------------------------}
procedure TDiscImage.SaveToFile(filename: String;uncompress: Boolean=False);
var
 FDiscDrive: TFileStream;
 ext: String;
begin
 //Validate the filename
 ext:=ExtractFileExt(filename); //First extract the extension
 if ext='' then //If it hasn't been given an extension, then give it the default
 begin
  filename:=LeftStr(filename,Length(filename)-Length(ext));
  filename:=filename+'.'+FormatToExt;
 end;
 if FFormat>>4<>diAcornUEF then //Not CFS
 begin
  //Create the stream
  try
   FDiscDrive:=TFileStream.Create(filename,fmCreate OR fmShareDenyNone);
   //Move to the beginning of the stream
   FDiscDrive.Position:=0;
   //Read the image into the data buffer
   FDiscDrive.Write(Fdata[0],Length(Fdata));
   //Close the stream
   FDiscDrive.Free;
   //Change the image's filename
   imagefilename:=filename;
  except
   //Could not create
  end;
 end;
 if FFormat>>4=diAcornUEF then WriteUEFFile(filename,uncompress); //CFS
end;

{-------------------------------------------------------------------------------
Closes an image file
-------------------------------------------------------------------------------}
procedure TDiscImage.Close;
begin
 ResetVariables;
end;

{-------------------------------------------------------------------------------
Create and format a new disc image
-------------------------------------------------------------------------------}
function TDiscImage.FormatFDD(major,minor,tracks: Byte): Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major MOD $10;
 minor :=minor MOD $10;
 tracks:=tracks MOD 2;
 case major of
  diAcornDFS://Create DFS
   begin
    FDisc:=FormatDFS(minor,tracks);
    Result:=Length(FDisc)>0;
   end;
  diAcornADFS://Create ADFS
   begin
    FDisc:=FormatADFSFloppy(minor);
    Result:=Length(FDisc)>0;
   end;
  diCommodore://Create Commodore 64/128
   begin
    FDisc:=FormatCDR(minor);
    Result:=Length(FDisc)>0;
   end;
  diSinclair://Create Sinclair/Amstrad
   begin
    FDisc:=FormatSpectrum(minor);
    Result:=Length(FDisc)>0;
   end;
  diAmiga://Create AmigaDOS
   begin
    FDisc:=FormatAmiga(minor);
    Result:=Length(FDisc)>0;
   end;
  diAcornUEF://Create CFS
   begin
    FDisc:=FormatCFS;
    Result:=Length(FDisc)>0;
   end;
 end;
end;

{-------------------------------------------------------------------------------
Create and format a new hard disc image
-------------------------------------------------------------------------------}
function TDiscimage.FormatHDD(major:Byte;harddrivesize:Cardinal;newmap:Boolean;
                                                          dirtype:Byte):Boolean;
begin
 Result:=False;
 case major of
  diAcornADFS: //Create ADFS
   begin
    FDisc:=FormatADFSHDD(harddrivesize,newmap,dirtype);
    Result:=Length(FDisc)>0;
   end;
 end;
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path (CFS, entry is entry number)
-------------------------------------------------------------------------------}
function TDiscImage.ExtractFile(filename:String;var buffer:TDIByteArray;
                                entry:Cardinal=0): Boolean;
begin
 //Start with a false result
 Result:=False;
 case FFormat>>4 of
  diAcornDFS :Result:=ExtractDFSFile(filename,buffer);     //Extract DFS
  diAcornADFS:Result:=ExtractADFSFile(filename,buffer);    //Extract ADFS
  diCommodore:Result:=ExtractCDRFile(filename,buffer);     //Extract Commodore 64/128
  diSinclair :Result:=ExtractSpectrumFile(filename,buffer);//Extract Sinclair/Amstrad
  diAmiga    :Result:=ExtractAmigaFile(filename,buffer);   //Extract AmigaDOS
  diAcornUEF :Result:=ExtractCFSFile(entry,buffer);        //Extract CFS
  diSpark    :Result:=ExtractSparkFile(filename,buffer);   //Extract Spark
  diAcornFS  :Result:=ExtractAFSFile(filename,buffer);     //Extract Acorn FileStore
 end;
end;

{-------------------------------------------------------------------------------
Save a file into the disc image, from buffer
-------------------------------------------------------------------------------}
function TDiscImage.WriteFile(var file_details: TDirEntry; var buffer: TDIByteArray): Integer;
var
 count : Integer;
begin
 //Start with a false result
 Result:=-2; //Error - disc full
 //Get the length of data to be written
 count:=file_details.Length;
 //There are only two sides (max)
 file_details.Side:=file_details.Side mod 2;
 //Only write a file if there is actually any data to be written
 if count>0 then
 begin
  //Can only write a file that will fit on the disc, or CFS
  if(count<=free_space)or(FFormat>>4=diAcornUEF)then
   case FFormat>>4 of
    diAcornDFS :Result:=WriteDFSFile(file_details,buffer);     //Write DFS
    diAcornADFS:Result:=WriteADFSFile(file_details,buffer);    //Write ADFS
    diCommodore:Result:=WriteCDRFile(file_details,buffer);     //Write Commodore 64/128
    diSinclair :Result:=WriteSpectrumFile(file_details,buffer);//Write Sinclair/Amstrad
    diAmiga    :Result:=WriteAmigaFile(file_details,buffer);   //Write AmigaDOS
    diAcornUEF :Result:=WriteCFSFile(file_details,buffer);     //Write CFS
   end;
 end
 else Result:=-8; //Error - zero length file
end;

{-------------------------------------------------------------------------------
Create a directory
-------------------------------------------------------------------------------}
function TDiscImage.CreateDirectory(var filename,parent,attributes: String): Integer;
begin
 //Start with a false result
 Result:=-1;
 case FFormat>>4 of
  diAcornDFS : exit;//Can't create directories on DFS
  diAcornADFS:      //Create directory on ADFS
    Result:=CreateADFSDirectory(filename,parent,attributes);
  diCommodore: exit;//Can't create directories on Commodore
  diSinclair : exit;//Can't create directories on Sinclair/Amstrad
  diAmiga    :      //Create directory on AmigaDOS
    Result:=CreateAmigaDirectory(filename,parent,attributes);
  diAcornUEF : exit;//Can't create directories on CFS
 end;
end;

{-------------------------------------------------------------------------------
Retitle a directory
-------------------------------------------------------------------------------}
function TDiscImage.RetitleDirectory(var filename,newtitle: String): Boolean;
begin
 //Start with a false result
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : exit;//DFS doesn't have directories
  diAcornADFS:      //Retitle ADFS directory
    Result:=RetitleADFSDirectory(filename,newtitle);
  diCommodore: exit;//Commodore doesn't have directories
  diSinclair : exit;//Sinclair/Amstrad doesn't have directories
  diAmiga    :      //Retitle AmigaDOS directory
    Result:=RetitleAmigaDirectory(filename,newtitle);
  diAcornUEF : exit;//CFS doesn't have directories
 end;
end;

{-------------------------------------------------------------------------------
Does a file exist?
-------------------------------------------------------------------------------}
function TDiscImage.FileExists(filename: String;var Ref: Cardinal): Boolean;
var
 dir,entry: Cardinal;
begin
 dir:=$FFFF;
 entry:=$FFFF;
 Result:=FileExists(filename,dir,entry);
 Ref:=dir*$10000+entry;
end;
function TDiscImage.FileExists(filename: String;var dir,entry: Cardinal): Boolean;
var
 Path   : array of String;
 i,j,l,
 ptr,
 level  : Integer;
 test,
 test2  : String;
begin
 //This will not work with CFS as you can have multiple files with the same name
 //in the same 'directory'. It will just find the first occurance.
 Result:=False;
 if filename=root_name then
 begin
  dir:=$FFFF;
  entry:=$FFFF;
  Result:=True;
  exit;
 end;
 //Not going to search if there is no tree to search in
 if Length(FDisc)>0 then
 begin
  SetLength(Path,0);
  j:=-1;
  ptr:=0;
  //Explode the pathname into an array, without the '.'
  if FFormat>>4<>diAcornDFS then //Not DFS
   while(Pos(dir_sep,filename)<>0)do
   begin
    SetLength(Path,Length(Path)+1);
    Path[Length(Path)-1]:=Copy(filename,0,Pos(dir_sep,filename)-1);
    filename:=Copy(filename,Pos(dir_sep,filename)+1,Length(filename));
   end;
  if FFormat>>4=diAcornDFS then //DFS
  begin
   SetLength(Path,2);
   Path[0]:=Copy(filename,0,Pos(root_name+dir_sep,filename));
   Path[1]:=Copy(filename,Pos(root_name+dir_sep,filename)
                         +Length(root_name)+Length(dir_sep),Length(filename));
  end;
  //If there is a path, then follow it
  if Length(Path)>0 then
  begin
   //filename gets truncated, so me be zero length at this point
   if(Length(filename)>0)and(FFormat>>4<>diAcornDFS)then
   begin
    //Otherwise, we'll add it to the end of the Path
    SetLength(Path,Length(Path)+1);
    Path[Length(Path)-1]:=filename;
   end;
   //Position into the Path array (i.e. directory level)
   level:=0;
   //Counters/Pointers
   i:=0;
   j:=-1;
   ptr:=-1;
   test:=UpperCase(AddTopBit(Path[level]));
   test2:=UpperCase(AddTopBit(FDisc[i].Directory));
   //Haven't matched the root, so need to look for it
   if test<>test2 then
    //So we'll continue until we find it
    while(i<Length(FDisc))and(test2<>test)do
    begin
     inc(i);
     test2:=UpperCase(AddTopBit(FDisc[i].Directory));
    end;
   //Using UpperCase makes it a case insensitive search
   if test=test2 then
    //Have we found the initial directory (usually '$')
    repeat
     //Let's move to the next directory level
     inc(level);
     //And search the entries
     j:=-1;
     repeat
      inc(j);
      test:='';
      if level<Length(Path) then
       test2:=UpperCase(AddTopBit(Path[level]))
      else
       test2:='not found';
      l:=j;
      //Just to make sure we don't search past the end of the arrays
      if (i>=0) and (i<Length(FDisc)) then
      begin
       if j<Length(FDisc[i].Entries) then
        test:=UpperCase(AddTopBit(FDisc[i].Entries[j].Filename));
       l:=Length(FDisc[i].Entries)-1;
      end;
     until (test2=test) or (j>=l);
     //So, we found the entry
     if test2=test then
     begin
      //Make a note
      ptr:=i;
      //Is it a directory? Then move down to the next level
      if (i>=0) and (i<Length(FDisc)) then
       if j<Length(FDisc[i].Entries) then
       begin
        test2:=UpperCase(AddTopBit(FDisc[i].Entries[j].Filename));
        i:=FDisc[i].Entries[j].DirRef;
        //Unless we are looking for a directory
        if level=Length(Path)-1 then
          if UpperCase(AddTopBit(Path[level]))=test2 then
           i:=-1;
       end;
     end
     else j:=-1;
    until (i=-1) or (j=-1); //End if it is not a directory, or is not found
  end;
   //Found, so return TRUE, with the references
  if j<>-1 then
  begin
   Result:=True;
   dir:=ptr;
   entry:=j;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Direct access to disc data
-------------------------------------------------------------------------------}
function TDiscImage.ReadDiscData(addr,count,side: Cardinal; var buffer): Boolean;
var
 i   : Cardinal;
 temp: TDIByteArray;
begin
 Result:=False;
 temp:=nil;
 if count>0 then //Make sure there is something to read
 begin
  //Simply copy from source to destination
  SetLength(temp,count);
  for i:=0 to count-1 do
  begin
   if FFormat>>4<>diAcornDFS then //All but DFS
    temp[i]:=ReadByte(addr+i);
   if FFormat>>4=diAcornDFS then  //DFS only
    temp[i]:=ReadByte(ConvertDFSSector(addr+i,side));
  end;
  //Do the move
  Move(temp[0],buffer,count);
  Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Direct access writing to disc
-------------------------------------------------------------------------------}
function TDiscImage.WriteDiscData(addr,side: Cardinal;var buffer: TDIByteArray;
                                    count: Cardinal;start: Cardinal=0): Boolean;
var
 i   : Cardinal;
begin
 Result:=False;
 if count=0 then exit;
 //Make sure the numbers fit
 if start+count<=Length(buffer) then
 begin
  //Sometimes the image file is smaller than the actual disc size
  if GetDataLength<disc_size then SetDataLength(disc_size);
  if FFormat>>4<>diAcornDFS then //not DFS
  begin
   //Ensure that the entire block will fit into the available space
   Result:=(addr+count)<=GetDataLength;
   //Simply copy from source to destination
   if Result then
    for i:=0 to count-1 do
     WriteByte(buffer[start+i],addr+i);
  end
  else //DFS
  begin
   //Ensure that the entire block will fit into the available space
   Result:=ConvertDFSSector(addr+count,side)<=GetDataLength;
   //Simply copy from source to destination
   if Result then
    for i:=0 to count-1 do
     WriteByte(buffer[start+i],ConvertDFSSector(addr+i,side));
  end;
 end;
end;

{-------------------------------------------------------------------------------
Searches for a file, and returns the result in a TSearchResults
-------------------------------------------------------------------------------}
function TDiscImage.FileSearch(search: TDirEntry): TSearchResults;
//Comparison functions...saves a lot of if...then statements
 function CompStr(S1,S2: String): Byte; //Compares Strings
 begin
  Result:=0;
  if S1<>'' then
   if CompareString(S2,S1,False) then Result:=1;
 end;
 function CompCar(S1,S2: Cardinal): Byte; //Compares Cardinals/Integers
 begin
  Result:=0;
  if(S1=S2)and(S1<>0)then Result:=1;
 end;
 function CompTSt(S1,S2: TDateTime): Byte; //Compares TDateTime
 begin
  Result:=0;
  if(S1=S2)and(S1<>0) then Result:=1;
 end;
//Function proper starts here
var
 dir,
 entry  : Integer;
 found,
 target : Byte;
begin
 Result:=nil;
 //Reset the search results array to empty
 SetLength(Result,0);
 //Has the complete path been included in the Filename?
 if Pos(DirSep,search.Filename)>0 then
 begin
  //Split filename into parent and filename
  if FFormat<>diAcornDFS then //Not DFS
  begin
   target:=Length(search.Filename);
   //Look for the last directory separator
   while(search.Filename[target]<>DirSep)and(target>1)do
    dec(target);
   //And split into parent and filename
   search.Parent:=LeftStr(search.Filename,target-1);
   search.Filename:=Copy(search.Filename,target+1);
  end;
  if FFormat=diAcornDFS then //DFS only
  begin
   //Is there a drive specifier, with root? :0.$ or :2.$
   if(search.Filename[1]=':')and(Copy(search.Filename,3,2)='.$')then
   begin
    search.Parent  :=LeftStr(search.Filename,4);
    search.Filename:=Copy(search.Filename,6);
   end;
   //Is there just a root?
   if Copy(search.Filename,1,2)='$.' then
   begin
    search.Parent  :='$.';
    search.Filename:=Copy(search.Filename,3);
   end;
   //After these, is the filename left blank?
   if search.Filename='' then
   begin
    search.Filename:=search.Parent;
    search.Parent  :='';
   end;
  end;
 end;
 //Work out what the search target is
 target:=0;
 //The search works by heading for a target. If the specified target is met
 //(i.e. all given fields match), then the search is a success.
 with search do
 begin
  if Parent       <>'' then inc(target);
  if Filename     <>'' then inc(target);
  if Attributes   <>'' then inc(target);
  if Filetype     <>'' then inc(target);
  if ShortFiletype<>'' then inc(target);
  if LoadAddr  <>$0000 then inc(target);
  if ExecAddr  <>$0000 then inc(target);
  if Length    <>$0000 then inc(target);
  if Side      <>$0000 then inc(target);
  if Track     <>$0000 then inc(target);
  if Sector    <>$0000 then inc(target);
  if DirRef    <>$0000 then inc(target);
  if TimeStamp <>0     then inc(target);
 end;
 //Only seach if there is something to search in
 if Length(FDisc)>0 then
  for dir:=0 to Length(FDisc)-1 do
   //Only search directories which have children
   if Length(FDisc[dir].Entries)>0 then
    for entry:=0 to Length(FDisc[dir].Entries)-1 do
    begin
     //Found counter
     found:=0;
     //Search parameters
     with FDisc[dir].Entries[entry] do
     begin
      inc(found,CompStr(search.Parent,       Parent));
      inc(found,CompStr(search.Filename,     Filename));
      inc(found,CompStr(search.Attributes,   Attributes));
      inc(found,CompStr(search.Filetype,     Filetype));
      inc(found,CompStr(search.ShortFiletype,ShortFiletype));
      inc(found,CompCar(search.LoadAddr,     LoadAddr));
      inc(found,CompCar(search.ExecAddr,     ExecAddr));
      inc(found,CompCar(search.Length,       Length));
      inc(found,CompCar(search.Side,         Side));
      inc(found,CompCar(search.Track,        Track));
      inc(found,CompCar(search.Sector,       Sector));
      inc(found,CompCar(search.DirRef,       DirRef));
      inc(found,CompTSt(search.TimeStamp,    TimeStamp));
     end;
     //Have we hit the target?
     //found is the number of matches found, and target is what we're aiming for
     if found=target then
     begin
      //Yes, so add this to the results
      SetLength(Result,Length(Result)+1);
      Result[Length(Result)-1]:=FDisc[dir].Entries[entry];
      //User will still need to call FileExists to get the dir and entry references
     end;
    end;
end;

{-------------------------------------------------------------------------------
Rename a file - oldfilename is full path, newfilename has no path
-------------------------------------------------------------------------------}
function TDiscImage.RenameFile(oldfilename: String;var newfilename: String;
                                    entry:Cardinal=0): Integer;
begin
 Result:=-1;//Failed to rename
 case FFormat>>4 of
  diAcornDFS : Result:=RenameDFSFile(oldfilename,newfilename);     //Rename DFS
  diAcornADFS: Result:=RenameADFSFile(oldfilename,newfilename);    //Rename ADFS
  diCommodore: Result:=RenameCDRFile(oldfilename,newfilename);     //Rename Commodore 64/128
  diSinclair : Result:=RenameSpectrumFile(oldfilename,newfilename);//Rename Sinclair/Amstrad
  diAmiga    : Result:=RenameAmigaFile(oldfilename,newfilename);   //Rename AmigaDOS
  diAcornUEF : Result:=RenameCFSFile(entry,newfilename);           //Rename CFS
 end;
end;

{-------------------------------------------------------------------------------
Deletes a file (given full pathname)
-------------------------------------------------------------------------------}
function TDiscImage.DeleteFile(filename: String;entry: Cardinal=0): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : Result:=DeleteDFSFile(filename);     //Delete DFS
  diAcornADFS: Result:=DeleteADFSFile(filename);    //Delete ADFS
  diCommodore: Result:=DeleteCDRFile(filename);     //Delete Commodore 64/128
  diSinclair : Result:=DeleteSinclairFile(filename);//Delete Sinclair/Amstrad
  diAmiga    : Result:=DeleteAmigaFile(filename);   //Delete AmigaDOS
  diAcornUEF : Result:=DeleteCFSFile(entry);        //Delete CFS
 end;
end;

{-------------------------------------------------------------------------------
Moves a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.MoveFile(filename,directory: String): Integer;
var
 oldfn: String;
begin
 Result:=-12;
 //Can only move files on DFS (between drives), ADFS, Amiga and CFS
 if FFormat>>4=diAcornDFS then //Move on DFS
 begin
  oldfn:=filename;
  //Moving and copying are the same, essentially
  Result:=CopyFile(filename,directory);
  //We just need to delete the original once copied
  if Result>-1 then DeleteFile(oldfn);
 end
 else
 begin
  if FFormat>>4=diAcornADFS then Result:=MoveADFSFile(filename,directory);
  if FFormat>>4=diAmiga     then Result:=MoveAmigaFile(filename,directory);
 end;
end;
function TDiscImage.MoveFile(source: Cardinal;dest: Integer): Integer;
begin
 Result:=-12;
 //Can only move files on DFS (between drives), ADFS, Amiga and CFS
 if FFormat>>4=diAcornUEF then //Move on CFS
  Result:=MoveCFSFile(source,dest);
end;

{-------------------------------------------------------------------------------
Copies a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.CopyFile(filename,directory: String): Integer;
begin
 Result:=CopyFile(filename,directory,filename);
end;
function TDiscImage.CopyFile(filename,directory,newfilename: String): Integer;
var
 buffer      : TDIByteArray;
 ptr,
 entry,
 dir,
 d,e         : Cardinal;
 tempfn,
 newparent   : String;
 file_details: TDirEntry;
begin
 ptr:=0;
 SetLength(buffer,0);
 //Need to extract the filename from the full path...and ensure the file exists
 Result:=-11; //Could not find file
 if FileExists(filename,dir,entry) then
 begin
  Result:=1; //Could not load file
  //Ensure the new location does not already have such a file
  file_details:=FDisc[dir].Entries[entry]; //Take a copy first
  file_details.Filename:=newfilename;      //The new filename
  //Are we copying a directory?
  if Fdisc[dir].Entries[entry].DirRef=-1 then //No, then continue
  begin
   //First, get the file into memory
   if ExtractFile(filename,buffer) then
   begin
    Result:=-5;//Unknown error
    //Is there a file of the same name at the new location?
    if FileExists(directory+DirSep+file_details.Filename,ptr) then
     //Delete the old one
     DeleteFile(directory+DirSep+file_details.Filename);
    //Set up the filedetails
    file_details.Parent:=directory;
    if FFormat>>4=diAcornDFS then //DFS
     if(directory[1]=':')and(directory[2]='2') then
      file_details.Side:=1
     else
      file_details.Side:=0;
    //Then write it back to the image
    Result:=WriteFile(file_details,buffer);
   end;
  end;
  if FDisc[dir].Entries[entry].DirRef>=0 then //Copying directory
  begin
   Result:=-10; //Can't move or copy to the same directory
   //Is there already a file/directory of that name at the destination?
   if not FileExists(directory+dirsep+newfilename,ptr) then
   begin
    //First, create a new directory at the destination
    Result:=CreateDirectory(newfilename,directory,FDisc[dir].Entries[entry].Attributes);
    //if successful, then copy all the files
    if Result>=0 then
    begin
     //Then iterate through each entry and copy them using recursion
     d:=FDisc[dir].Entries[entry].DirRef; //Get the directory reference
     //Work out the new parent path for the files
     newparent:=directory+dir_sep+newfilename;
     //Copy the files
     if Length(FDisc[d].Entries)>0 then
      for e:=0 to Length(FDisc[d].Entries)-1 do
      begin
       tempfn:=FDisc[d].Entries[e].Parent+dir_sep+FDisc[d].Entries[e].Filename;
       CopyFile(tempfn,newparent);
      end;
    end;
   end;
  end;
 end;
end;
function TDiscImage.CopyFile(source: Cardinal;dest: Integer): Integer;
begin
 Result:=-12;
 //Can only copy files on DFS (between drives), ADFS, Amiga and CFS
 if FFormat>>4=diAcornUEF then //Copy on CFS
  Result:=CopyCFSFile(source,dest);
end;

{-------------------------------------------------------------------------------
Set the attributes for a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateAttributes(filename,attributes: String;entry:Cardinal=0):Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : Result:=UpdateDFSFileAttributes(filename,attributes);     //Update DFS attributes
  diAcornADFS: Result:=UpdateADFSFileAttributes(filename,attributes);    //Update ADFS attributes
  diCommodore: Result:=UpdateCDRFileAttributes(filename,attributes);     //Update Commodore 64/128 attributes
  diSinclair : Result:=UpdateSinclairFileAttributes(filename,attributes);//Update Sinclair/Amstrad attributes
  diAmiga    : Result:=UpdateAmigaFileAttributes(filename,attributes);   //Update AmigaDOS attributes
  diAcornUEF : Result:=UpdateCFSAttributes(entry,attributes);            //Update CFS attributes
 end;
end;

{-------------------------------------------------------------------------------
Set the disc title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDiscTitle(title: String;side: Byte): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : Result:=UpdateDFSDiscTitle(title,side);//Title DFS Disc
  diAcornADFS: Result:=UpdateADFSDiscTitle(title);    //Title ADFS Disc
  diCommodore: Result:=UpdateCDRDiscTitle(title);     //Title Commodore 64/128 Disc
  diSinclair : Result:=UpdateSinclairDiscTitle(title);//Title Sinclair/Amstrad Disc
  diAmiga    : Result:=UpdateAmigaDiscTitle(title);   //Title AmigaDOS Disc
  diAcornUEF : Result:=False;                         //Can't retitle CFS
 end;
end;

{-------------------------------------------------------------------------------
Set the boot option
-------------------------------------------------------------------------------}
function TDiscImage.UpdateBootOption(option,side: Byte): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : Result:=UpdateDFSBootOption(option,side);//Update DFS Boot
  diAcornADFS: Result:=UpdateADFSBootOption(option);    //Update ADFS Boot
  diCommodore: exit;//Update Commodore 64/128 Boot ++++++++++++++++++++++++++++++++++++++
  diSinclair : exit;//Update Sinclair/Amstrad Boot ++++++++++++++++++++++++++++++++++++++
  diAmiga    : exit;//Update AmigaDOS Boot ++++++++++++++++++++++++++++++++++++++++++++++
  diAcornUEF : exit;//Can't update CFS boot option
 end;
end;

{-------------------------------------------------------------------------------
Change the load address
-------------------------------------------------------------------------------}
function TDiscImage.UpdateLoadAddr(filename:String;newaddr:Cardinal;entry:Cardinal=0): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : Result:=UpdateDFSFileAddr(filename,newaddr,True); //Update DFS Load Address
  diAcornADFS: Result:=UpdateADFSFileAddr(filename,newaddr,True);//Update ADFS Load Address
  diCommodore: exit;//Update Commodore 64/128 Load Address
  diSinclair : exit;//Update Sinclair/Amstrad Load Address
  diAmiga    : exit;//Update AmigaDOS Load Address
  diAcornUEF : Result:=UpdateCFSFileAddr(entry,newaddr,True);    //Update update CFS Load Address
 end;
end;

{-------------------------------------------------------------------------------
Change the execution address
-------------------------------------------------------------------------------}
function TDiscImage.UpdateExecAddr(filename:String;newaddr:Cardinal;entry:Cardinal=0): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : Result:=UpdateDFSFileAddr(filename,newaddr,False); //Update DFS Execution Address
  diAcornADFS: Result:=UpdateADFSFileAddr(filename,newaddr,False);//Update ADFS Execution Address
  diCommodore: exit;//Update Commodore 64/128 Execution Address
  diSinclair : exit;//Update Sinclair/Amstrad Execution Address
  diAmiga    : exit;//Update AmigaDOS Execution Address
  diAcornUEF : Result:=UpdateCFSFileAddr(entry,newaddr,False);    //Update update CFS Execution Address
 end;
end;

{-------------------------------------------------------------------------------
Timestamp the file
-------------------------------------------------------------------------------}
function TDiscImage.TimeStampFile(filename:String;newtimedate:TDateTime):Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : exit;//Update DFS Timestamp
  diAcornADFS: Result:=UpdateADFSTimeStamp(filename,newtimedate);//Update ADFS Timestamp
  diCommodore: exit;//Update Commodore 64/128 Timestamp
  diSinclair : exit;//Update Sinclair/Amstrad Timestamp
  diAmiga    : exit;//Update AmigaDOS Timestamp
  diAcornUEF : exit;//Update update CFS Timestamp
 end;
end;

{-------------------------------------------------------------------------------
Change the file's filetype
-------------------------------------------------------------------------------}
function TDiscImage.ChangeFileType(filename,newtype: String): Boolean;
begin
 Result:=False;
 case FFormat>>4 of
  diAcornDFS : exit;//Update DFS Filetype
  diAcornADFS: Result:=UpdateADFSFileType(filename,newtype);//Update ADFS Filetype
  diCommodore: exit;//Update Commodore 64/128 Filetype
  diSinclair : exit;//Update Sinclair/Amstrad Filetype
  diAmiga    : exit;//Update AmigaDOS Filetype
  diAcornUEF : exit;//Update update CFS Filetype
 end;
end;

{-------------------------------------------------------------------------------
Convert a filetype number to a string
-------------------------------------------------------------------------------}
function TDiscImage.GetFileTypeFromNumber(filetype: Integer): String;
var
 i: Integer;
begin
 filetype:=filetype AND$FFF;
 Result:='';
 i:=1;
 repeat
  if StrToInt('$'+LeftStr(ROFileTypes[i],3))=filetype then
   Result:=Copy(ROFileTypes[i],4);
  inc(i);
 until(i>Length(ROFileTypes))or(Result<>'');
 if Result='' then Result:=IntToHex(filetype,3);
end;

{-------------------------------------------------------------------------------
Convert a filetype name into a number
-------------------------------------------------------------------------------}
function TDiscImage.GetFileTypeFromName(filetype: String): Integer;
var
 i: Integer;
begin
 Result:=-1;
 i:=1;
 repeat
  if UpperCase(filetype)=UpperCase(Copy(ROFileTypes[i],4))then
   Result:=StrToInt('$'+LeftStr(ROFileTypes[i],3));
  inc(i);
 until(i>Length(ROFileTypes))or(Result<>-1);
end;

{-------------------------------------------------------------------------------
Updating is commencing
-------------------------------------------------------------------------------}
procedure TDiscImage.BeginUpdate;
begin
 Fupdating:=True;
end;

{-------------------------------------------------------------------------------
Updating is ending
-------------------------------------------------------------------------------}
procedure TDiscImage.EndUpdate;
begin
 Fupdating:=False;
 case FFormat>>4 of
  diAcornDFS : exit;//Update DFS
  diAcornADFS: ADFSFreeSpaceMap;//Update ADFS
  diCommodore: exit;//Update Commodore 64/128
  diSinclair : exit;//Update Sinclair/Amstrad
  diAmiga    : exit;//Update AmigaDOS
  diAcornUEF : exit;//Update CFS
 end;
end;

{-------------------------------------------------------------------------------
Makes sure that a copied/moved filename does not clash with one already there
-------------------------------------------------------------------------------}
function TDiscImage.ValidateFilename(parent:String;var filename:String): Boolean;
var
 ptr  : Cardinal;
 len,
 ctr  : Byte;
 newfn: String;
begin
 //Default return result
 Result:=False;
 len:=0; //Maximum file length
 //Work out the max file length for the system
 case FFormat>>4 of
  diAcornDFS : len:=7;
  diAcornADFS:
   case FDirType of
    0,1: len:=10;
    2  : len:=255;
   end;
  diCommodore: len:=16;
  diSinclair : exit;
  diAmiga    : len:=30;
  diAcornUEF : len:=10;
  diMMFS     : len:=7;
 end;
 if len=0 then exit; //Unsupported
 //Extract the filename
 while Pos('.',filename)>0 do filename:=Copy(filename,Pos('.',filename)+1);
 //CFS files can have multiple files with the same name
 if FFormat>>4=diAcornUEF then
 begin
  Result:=True;
  exit;
 end;
 //Validate it
 if FileExists(parent+DirSep+filename,ptr) then
 begin
  newfn:=filename;
  ctr:=0;
  repeat
   inc(ctr);
   while Length(newfn+IntToStr(ctr))>len do
    newfn:=LeftStr(newfn,Length(newfn)-1);
  until(not FileExists(parent+DirSep+newfn+IntToStr(ctr),ptr))or(ctr=0);
  if ctr>0 then
  begin
   filename:=newfn+IntToStr(ctr);
   Result:=True;
  end;
 end else Result:=True;
end;

{-------------------------------------------------------------------------------
Returns a partition 'part'
-------------------------------------------------------------------------------}
function TDiscImage.Partition(part: Cardinal):TDiscImage;
begin
 Result:=nil;
 if part<Length(FPartitions) then Result:=FPartitions[part];
end;

{$INCLUDE 'DiscImage_ADFS.pas'}
{$INCLUDE 'DiscImage_AFS.pas'}
{$INCLUDE 'DiscImage_DFS.pas'}
{$INCLUDE 'DiscImage_C64.pas'}
{$INCLUDE 'DiscImage_Spectrum.pas'}
{$INCLUDE 'DiscImage_Amiga.pas'}
{$INCLUDE 'DiscImage_CFS.pas'}
{$INCLUDE 'DiscImage_MMB.pas'}
{$INCLUDE 'DiscImage_Spark.pas'}

end.
