unit DiscImage;

{
TDiscImage class V1.49 and TSpark class V1.06
Manages retro disc images, presenting a list of files and directories to the
parent application. Will also extract files and write new files. Almost a complete
filing system in itself. Compatible with Acorn DFS, Acorn ADFS, UEF, Commodore
1541, Commodore 1571, Commodore 1581, Commodore AmigaDOS, Acorn File Server,
SparkFS, PackDir, MS-DOS, and Acorn DOS Plus.

Copyright ©2018-2025 Gerald Holdsworth gerald@hollypops.co.uk

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

DSK Image modules written by Damien Guard
}

{$MODE objFPC}{$H+}
{$modeswitch TypeHelpers}

interface

{
There is some code that is duplicated across procedures/functions, particuarly
with different filing systems. This is done to separate the systems and make
maintenance easier.
}

uses Classes,Math,crc,ZStream,StrUtils,SysUtils,Zipper,ExtCtrls,DateUtils,md5,
  DskImage,FileSystem,HTTPProtocol,fpjson,DOM,XMLWrite;

{$M+}

type
//Define the TDIByteArray - saves using the System.Types unit for TByteDynArray
 TDIByteArray = array of Byte;

//Define the records to hold the catalogue
 TDirEntry     = record     //Not all fields are used on all formats
  Parent,                   //Complete path for parent directory (ALL)
  Filename,                 //Filename (ALL)
  ShortFilename,            //Long Filename (DOS)
  Attributes,               //File attributes (ADFS/DFS/D64/D71/D81/AmigaDOS)
  Filetype,                 //Full name filetype (ADFS/D64/D71/D81)
  ShortFileType: String;    //Filetype shortname (ADFS/D64/D71/D81)
  LoadAddr,                 //Load Address (ADFS/DFS)
  ExecAddr,                 //Execution Address (ADFS/DFS)
  Length,                   //Total length (ALL)
  Side,                     //Side of disc of location of data (DFS)
  Track       : Cardinal;   //Track of location of data (D64/D71/D81)
  Sector,                   //Sector of disc of location of data (DFS/D64/D71/D81/AmigaDOS file)
                            //Sector of disc of location of header (AmigaDOS directory)
                            //Address of location of data (ADFS S/M/L/D)
                            //Indirect disc address of data (ADFS E/F/E+/F+)
  DirRef      : Integer;    //Reference to directory, if directory (ADFS/AmigaDOS)
  TimeStamp   : TDateTime;  //Timestamp (ADFS D/E/E+/F/F+)
  isDOSPart   : Boolean;    //This file is the DOS partition
  Sequence    : Byte;       //Sequence number for the file (ADFS S/M/L)
 end;
 TSearchResults =array of TDirEntry;

 //Define the records for an Acorn File Server password file
 type
  TUserAccount = record
   Username,
   Password   : String;
   FreeSpace  : Cardinal;
   System,
   Locked     : Boolean;
   BootOption,
   AccessLevel: Byte;
 end;
 TUserAccounts  =array of TUserAccount;

 //Define the record for a file entry in TSpark
 type
  TFileEntry = record
   LoadAddr,                 //Load Address
   ExecAddr,                 //Execution Address
   Length,                   //Uncompressed size
   Size,                     //Compressed size
   NumEntries,               //Number of directory entries
   Attributes,               //File attributes (hex)
   DataOffset : Cardinal;    //Where to find the data
   Filename,                 //RISC OS filename
   Parent,                   //RISC OS parent
   ArchiveName: String;      //Name (and path) in archive
   Directory  : Boolean;     //Is it a directory?
 end;

 //Class helper for XML documents
 type
  TXMLHelper = type helper for TXMLDocument
    procedure Add(name, value: String);
    function AsString: String;
    procedure ValidateName(var name: String);
  end;

 //General purpose procedures - globally accessible
 procedure ResetDirEntry(var Entry: TDirEntry);
 procedure RemoveTopBit(var title: String);
 function AddTopBit(title:String):String;
 procedure BBCtoWin(var f: String);
 procedure WintoBBC(var f: String);
 procedure RemoveSpaces(var s: String);
 procedure RemoveControl(var s: String);
 function IsBitSet(v,b: Integer): Boolean;
 procedure ParseInf(output: TObject; line: String);
 function FilenameToASCII(s: String): String;
 function GetAttributes(attr: String;format: Byte): String;
 function CompareString(S, mask: string; case_sensitive: Boolean): Boolean;
 function DateTimeToAFS(timedate: TDateTime): Word;
 function AFSToDateTime(date: Word):TDateTime;
 procedure ValidateWinFilename(var f: String);
 function DecToBCD(dec: Cardinal): Cardinal;
 function BCDToDec(BCD: Cardinal): Cardinal;
 procedure ResetFileEntry(var fileentry: TFileEntry);
 function CreateXML(name: String): TXMLDocument;
 //Some constants
 const
  diAcornDFS   = $000;
  diAcornADFS  = $001;
  diCommodore  = $002;
  diSinclair   = $003;
  diAmiga      = $004;
  diAcornUEF   = $005;
  diMMFS       = $006;
  diAcornFS    = $007;
  diSpark      = $008;
  diSJMDFS     = $009;
  diDOSPlus    = $00A;
  diAcornRFS   = $00B;
  diISO        = $00C;
  diInvalidImg = $00FF; //Needs to be changed to $FFFF
  diADFSOldMap = $00;
  diADFSNewMap = $01;
  diAmigaOFS   = $02;
  diAmigaFFS   = $03;
  diMaster512  = $01;
  diFAT12      = $12;
  diFAT16      = $16;
  diFAT32      = $32;
  diADFSOldDir = $00;
  diADFSNewDir = $01;
  diADFSBigDir = $02;
  diAmigaDir   = $10;
  diAmigaCache = $11;
  diUnknownDir = $FF;
  diFSMUnformat= $01;
  diFSMBlank   = $00;
  diFSMDir     = $FD;
  diFSMSystem  = $FE;
  diFSMUsed    = $FF;

 //TSpark class definition
 type
  TSpark = Class
  private
  type
   TFileList = array of TFileEntry;
   TProgressProc = procedure(Sender: TObject;const Fupdate: Double) of Object;
   private
   Fcache,                     //Data cache for receiving uncompressed file
   Fbuffer     : TDIByteArray; //Buffer to hold the archive
   ZipFilename : String;       //Filename of the archive
   FIsSpark    : Boolean;      //Is it a valid !Spark archive?
   FIsPack     : Boolean;      //Is it a valid !Pack archive?
   FFileList   : TFileList;    //List of files in archive
   FProgress   : TProgressProc;//Progress feedback
   Fversion    : String;       //Version of this class
   FTimeOut    : Cardinal;     //Length of time out, in seconds
   FMaxDirEnt  : Integer;      //Maximum size of directory
   FBitLength  : Integer;      //Bit length (for LZW)
   //Private methods
   function ExtractFiles: TFileList;
   function ExtractSparkFiles: TFileList;
   function ExtractPackFiles: TFileList;
   procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
                                                      AItem: TFullZipFileEntry);
   procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
                                                      AItem: TFullZipFileEntry);
   function GetUncompressedSize: Cardinal;
   function IsItSpark: Boolean;
   function FindCL(var EoCL: Integer;var buffer: TDIByteArray): Integer;
   procedure UpdateCL(CL,EoCL: Cardinal; var buffer: TDIByteArray);
   function ExtractFileDataFromSpark(index: Integer):TDIByteArray;
   function ExtractFileDataFromPack(index: Integer):TDIByteArray;
   procedure SaveData;
   function FindEntry(path: String;matchpath: Boolean;var CLptr: Cardinal;
                      var dataptr: Cardinal;var LBuffer: TDIByteArray): Boolean;
   function RenameTheFile(oldpath, newpath: String): Boolean;
   function DeleteTheFile(filename: String):Boolean;
   function CombineZIP(files: array of String; outputfile: String): Boolean;
   procedure WriteSignature(header: Byte;var buffer: TDIByteArray;ptr: Cardinal=0);
   //Private constants
   const
    NewAtts  : array[0..7] of Char = ('R','W','L','D','r','w',' ',' ');
    clsig    : array[0..3] of Byte = ($50,$4B,$01,$02);
    headersig: array[0..3] of Byte = ($50,$4B,$03,$04);
    eoclsig  : array[0..3] of Byte = ($50,$4B,$05,$06);
    spansig  : array[0..3] of Byte = ($50,$4B,$07,$08);//Added for completeness
    ZIPCL    = 0;
    ZIPHeader= 1;
    ZIPEoCL  = 2;
    ZIPSpan  = 3;
  published
   //Published methods
   constructor Create(filename: String;blank: Boolean=false);
   constructor Create(stream: TStream); overload;
   function ExtractFileData(Index: Integer):TDIByteArray;
   procedure WriteFile(var filetozip: TFileEntry;var buffer: TDIByteArray);
   procedure CreateDirectory(path: String);
   function RenameFile(oldpath, newpath: String): Boolean;
   function UpdateLoadExecAddress(path: String;load, exec: Cardinal): Boolean;
   function UpdateAttributes(path: String; attributes: Word): Boolean;
   function DeleteFile(filename: String):Boolean;
   procedure RISCOSFilename(path: String;addroot: Boolean;var filename: String;
                                                            var parent: String);
   procedure SwapDirSep(var path: String);
   function ConvertAttribute(attr: Byte): String;
   function ConvertAttribute(attr: String): Byte; overload;
   function Validate: Boolean;
   //Published properties
   property IsSpark:           Boolean       read IsItSpark;
   property FileList:          TFileList     read FFileList;
   property UncompressedSize:  Cardinal      read GetUncompressedSize;
   property ProgressIndicator: TProgressProc write FProgress;
   property Version:           String        read Fversion;
   property TimeOut:           Cardinal      read FTimeOut write FTimeOut;
   property MaxDirEnt:         Integer       read FMaxDirEnt;
  public
   destructor Destroy; override;
  end;

//DiscImage class definition
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
   Entries     : array of TDirEntry;//Entries
   Sequence,                        //Master Sequence Number (DFS/ADFS S/M/L)
   ErrorCode   : Byte;              //Used to indicate error for broken directory (ADFS)
   Deleted,                         //Used to indicate if this directory has been deleted
   Broken,                          //Flag if directory is broken (ADFS)
   Locked,                          //Flag if disc is locked (MMFS)
   BeenRead,                        //Flag if directory has been reed in
   DOSPartition,                    //Is this in the DOS Plus partition? (ADFS/DOS Plus)
   AFSPartition: Boolean;           //Is this in the AFS partition? (ADFS/AFS)
   Sector,                          //Where is this directory located (same as TDirEntry)
   Length,                          //How big is the directory (same as TDirEntry)
   Partition   : Cardinal;          //Which partition (side) is this on?
   Parent      : Integer;           //What is the TDir reference of the parent (-1 if none)
  end;
  //For use with ISO images
  TISOVolDes = record
   VDType      : Byte;
   Offset      : Cardinal;
   SystemID    : String;
   VolumeID    : String;
   Size        : Cardinal;
   Joilet      : Byte;
   NumDiscs    : Cardinal;
   DiscNum     : Cardinal;
   BlckSize    : Cardinal;
   PathSize    : Cardinal;
   PathTbl     : array[0..1] of Cardinal;
   oPathTbl    : array[0..1] of Cardinal;
   RootOffset  : Cardinal;
   RootLength  : Cardinal;
   VolSetID    : String;
   PublisID    : String;
   DataPrID    : String;
   AppID       : String;
   CopyID      : String;
   AbstID      : String;
   BibliID     : String;
   DateCre     : TDateTime;
   DateMod     : TDateTime;
   DateExp     : TDateTime;
   DateUse     : TDateTime;
  end;
  //Collection of directories
  TDisc         = array of TDir;
  //Partitions
  TPartition    = record               //Details about the partition
   Directory       : TDisc;            //All the directories
   Title,                              //Title of the partition
   RootTitle,                          //Title of the root directory
   RootName        : String;           //Root name ($, A:, C:, DF0, DH0, etc.)
   DirSep          : Char;             //Directory separator
   HeaderAddr,                         //Offset(s) of the header(s)
   FSMAddr         : array of Cardinal;//Offset(s) of the FSM/Map/Bitmap/FAT(s)
   FreeSpaceMap    : array of TTrack;  //The free space map
   DOSVolInRoot    : Boolean;          //Volume name is stored in the root (DOS)
   RootAddress,                        //Offset of the root
   SectorSize,                         //Sector Size
   DOSalloc,                           //Allocation Unit (DOS Plus)
   Version,                            //Format version
   Root_size,                          //Size of the root directory
   DOSBlocks,                          //Size of the DOS partition in blocks
   DOSCluster_size : Cardinal;         //Size of a DOS cluster
   FreeSpace,                          //Amount of free space in bytes
   PartitionSize   : QWord;            //Size of the partition in bytes
   Format,                             //Major format of this partition
   DOSFATSize,                         //Size of DOS Plus FAT in blocks
   DOSResSecs      : Word;             //Number of reserved blocks
   SecsPerTrack,                       //Number of sectors per track
   Heads,                              //Number of heads (Acorn ADFS New)
   Density,                            //Density (Acorn ADFS New)
   DOSFATType,                         //FAT Type - 12: FAT12, 16: FAT16, 32: FAT32
   DOSNumFATs,                         //Number of FATs in a DOS Plus image
   AmigaMapType    : Byte;             //OFS/FFS/PFS/OFS
  end;
  //Partitions
  TPartitions   = array of TPartition;
  //Fragment
  TFragment     = record        //For retrieving the ADFS E/F fragment information
   Offset,
   Length,
   Zone         : Cardinal;
  end;
  //To collate fragments (ADFS/CDR/Amiga/AFS)
  TFragmentArray= array of TFragment;
  //Provides feedback
  TProgressProc = procedure(Fupdate: String) of Object;
 private
  FDisc         : TDisc;        //Container for the entire catalogue
  FPartitions   : TPartitions;  //Container for the entire catalogue (partitioned)
  Fdata         : TDIByteArray; //Container for the image to be loaded into
  FDSD,                         //Double sided flag (Acorn DFS)
  FMap,                         //Old/New Map flag (Acorn ADFS) OFS/FFS (Amiga)
  FBootBlock,                   //Is disc an AmigaDOS Kickstart?
  Fupdating,                    //Has BeginUpdate been called?
  FAFSPresent,                  //Is there an AFS partition present? (ADFS)
  FDOSPresent,                  //Is there a DOS Plus partition present? (ADFS)
  FSparkAsFS,                   //Deal with Spark archives as a filing system
  FDFSzerosecs,                 //Allow zero length disc images for DFS?
  FDFSAllowBlank,               //Allow blank filenames
  FDFSBeyondEdge,               //Check for files going beyond the DFS disc edge
  FAddImpliedAttributes,        //Add 'RW' to the inf file for DFS/CFS/RFS
  FDOSVolInRoot,                //Volume name is stored in the root (DOS)
  FScanSubDirs,                 //Scan sub directories on opening (ADFS/Amiga/DOS/Spark)
  FOpenDOSPart,                 //Open DOS Partitions on ADFS
  FcreateDSC,                   //Create *.dsc files with ADFS hard drives
  FDOSUseSFN,                   //Use short filenames, even if there are long filenames (DOS)
  FHasDirs      : Boolean;      //Format is directory capable
  secsize,                      //Sector Size
  bpmb,                         //Bits Per Map Bit (Acorn ADFS New)
  dosalloc,                     //Allocation Unit (DOS Plus)
  nzones,                       //Number of zones (Acorn ADFS New)
  root,                         //Root address (not fragment)
  rootfrag,                     //Root indirect address (Acorn ADFS New)
  Fafsroot,                     //Root address of the AFS root partition
  Fdosroot,                     //Root address of the DOS Plus root partition
  afshead,                      //Address of the AFS header             
  afshead2,                     //Address of the AFS header copy
  doshead,                      //Address of the DOS Plus header, if exists
  doshead2,                     //Address of the backup DOS header, if exists (FAT32)
  dosmap,                       //Address of the DOS Plus FAT
  dosmap2,                      //Address of the second DOS Plus FAT (if applicable)
  bootmap,                      //Offset of the map (Acorn ADFS)
  zone_spare,                   //Spare bits between zones (Acorn ADFS New)
  format_vers,                  //Format version (Acorn ADFS New)
  root_size,                    //Size of the root directory (Acorn ADFS New)
  afsroot_size,                 //Size of the AFS Root directory
  dosroot_size,                 //Size of the DOS Plus Root directory
  cluster_size,                 //Size of a DOS cluster
  DOSBlocks,                    //Size of a DOS partition in blocks
  disc_id,                      //Disc ID (Acorn ADFS)
  emuheader,                    //Allow for any headers added by emulators
  namesize,                     //Size of the name area (Acorn ADFS Big Dir)
  brokendircount,               //Number of broken directories (ADFS)
  FMaxDirEnt    : Cardinal;     //Maximum number of directory entries in image
  DOSFATSize,                   //Size of DOS Plus FAT in blocks
  DOSResSecs,                   //Number of reserved blocks
  FISOFormat,                   //Format of the ISO internal image
  FFormat       : Word;         //Format of the image
  FDirID,                       //ADFS Directory ID (1=Hugo, 2=Nick, 3=SBPr/oven)
  FForceInter,                  //What to do about ADFS L/AFS Interleaving
  Finterleave,                  //Interleave method (1=seq,2=int,3=mux)
  secspertrack,                 //Number of sectors per track
  heads,                        //Number of heads (Acorn ADFS New)
  density,                      //Density (Acorn ADFS New)
  idlen,                        //Length of fragment ID in bits (Acorn ADFS New)
  skew,                         //Head skew (Acorn ADFS New)
  lowsector,                    //Lowest sector number (Acorn ADFS New)
  disctype,                     //Type of disc
  FDirType,                     //Directory Type (Acorn ADFS)
  share_size,                   //Share size (Acorn ADFS New)
  big_flag,                     //Big flag (Acorn ADFS New)
  FATType,                      //FAT Type - 12: FAT12, 16: FAT16, 32: FAT32
  DOSVersion,                   //Version of DOS being used (0, $28 or $29)
  NumFATs       : Byte;         //Number of FATs in a DOS Plus image
  Fcopyright,
  Fversion,
  root_name,                    //Root title
  dosrootname,                  //DOS Plus root name
  imagefilename,                //Filename of the disc image
  FFilename     : String;       //Copy of above, but doesn't get wiped
  dir_sep       : Char;         //Directory Separator
  free_space_map: TSide;        //Free Space Map
  disc_size,                    //Disc size per partition
  free_space    : array of QWord;//Free space per partition
  disc_name     : array of String;//Disc title(s)
  bootoption    : TDIByteArray; //Boot Option(s)
  FilesData     : array of TDIByteArray;//All the data for CFS or Spark files
  FProgress     : TProgressProc;//Used for feedback
  SparkFile     : TSpark;       //For reading in Spark archives
  FDSKImage     : TDSKImage;    //For reading in Sinclair/Amstrad DSK files
  ISOVolDes     : array of TISOVolDes;//ISO Volume Descriptors
  //Disc title for new images
  Fdisctitle,
  Fafsdisctitle,                //AFS has longer titles
  Famigadisctitle,              //Amiga has even longer titles
  Frfstitle,                    //ROM FS Header title
  Frfscopyright : String;       //Copyright string for ROM FS
  //Private methods
  procedure ResetVariables;
  procedure AddPartition;
  function ReadString(ptr,term: Integer;control: Boolean=True): String;
  function ReadString(ptr,term: Integer;var buffer: TDIByteArray;
                                       control: Boolean=True): String; overload;
  procedure WriteString(str: String;ptr,len: Cardinal;pad: Byte);
  procedure WriteString(str: String;ptr,len: Cardinal;pad: Byte;
                                            var buffer: TDIByteArray); overload;
  function FormatToString: String;
  function ISOFormatToString: String;
  function FormatToExt: String;
  function GetMajorFormatNumber: Word;
  function GetMinorFormatNumber: Byte;
  function GetDoubleSided: Boolean;
  function ReadBits(offset,start,length: Cardinal): Cardinal;
  procedure WriteBits(value,offset,start,length: Cardinal);
  procedure WriteBits(value,offset,start,length: Cardinal;
                                                buffer: TDIByteArray); overload;
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
  procedure RemoveDirectory(dirref: Cardinal);
  function DiscAddrToIntOffset(disc_addr: Cardinal): Cardinal;
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
  procedure WriteByte(value: Byte; offset: Cardinal;
                                            var buffer: TDIByteArray); overload;
  function GetDataLength: Cardinal;
  procedure SetDataLength(newlen: Cardinal);
  function ROR13(v: Cardinal): Cardinal;
  procedure ResetDir(var Entry: TDir);
  function MapFlagToByte: Byte;
  function MapTypeToString: String;
  function DirTypeToString: String;
  function GeneralChecksum(offset,length,chkloc,start: Cardinal;
                                                      carry: Boolean): Cardinal;
  function GetImageCrc: String;
  function GetMD5(var buffer: TDIByteArray): String;
  function GetCRC(var buffer: TDIByteArray): String;
  function GetCRC16(start,len: Cardinal;var buffer: TDIByteArray): Cardinal;
  function GetCRC16(start,len: Cardinal): Cardinal; overload;
  procedure UpdateProgress(Fupdate: String);
  function GetRootAddress: Cardinal;
  function Inflate(filename: String): TDIByteArray;
  function InterleaveString: String;
  function VolumeSerialNumber: Cardinal;
  procedure UpdateDirRef(dirref: Cardinal);
  procedure SetDefaultDiscTitle(ADiscTitle: String);
  procedure SetDefaultAFSDiscTitle(ADiscTitle: String);
  procedure SetDefaultAmigaDiscTitle(ADiscTitle: String);
  procedure SetDefaultRFSTitle(ADiscTitle: String);
  procedure SetDefaultRFSCopyRight(ADiscTitle: String);
  function EncodeString(input: String): String;
  //ADFS Routines
  function ID_ADFS: Boolean;
  function ReadADFSDir(dirname: String; sector: Cardinal): TDir;
  function GetADFSDirID(Head: Boolean=True): String;
  function CalculateADFSDirCheck(sector: Cardinal): Byte;
  function CalculateADFSDirCheck(sector: Cardinal;
                                          buffer: TDIByteArray): Byte; overload;
  function NewDiscAddrToOffset(addr: Cardinal;
                                          offset: Boolean=True): TFragmentArray;
  function OffsetToOldDiscAddr(offset: Cardinal): Cardinal;
  function ByteChecksum(offset,size: Cardinal;newmap: Boolean): Byte;
  function ByteChecksum(offset,size: Cardinal;newmap: Boolean;
                                      var buffer: TDIByteArray): Byte; overload;
  function ReadADFSDisc: Boolean;
  procedure ADFSFreeSpaceMap;
  procedure ADFSFillFreeSpaceMap(address: Cardinal;usage: Byte);
  function FormatADFSFloppy(minor: Byte): Boolean;
  procedure FormatOldMapADFS(disctitle: String);
  procedure FormatNewMapADFS(disctitle: String; ide: Boolean);
  function FormatADFSHDD(harddrivesize: Cardinal; newmap: Boolean; dirtype:Byte;
                                               ide,addheader: Boolean): Boolean;
  function UpdateADFSDiscTitle(title: String): Boolean;
  function UpdateADFSBootOption(option: Byte): Boolean;
  function ADFSGetFreeFragments(offset:Boolean=True;
                                          whichzone:Integer=-1): TFragmentArray;
  function WriteADFSFile(var file_details: TDirEntry;var buffer: TDIByteArray;
                         extend:Boolean=True): Integer;
  function ADFSFindFreeSpace(filelen: Cardinal;
                                          var fragid: Cardinal): TFragmentArray;
  function WriteFragmentedData(fragments: TFragmentArray;
                                             var buffer: TDIByteArray): Boolean;
  function ADFSAllocateFreeSpace(filelen,freeptr: Cardinal): Boolean;
  function ADFSAllocateFreeSpace(filelen,fragid: Cardinal;
                                  fragments: TFragmentArray): Boolean; overload;
  function ADFSSectorAlignLength(filelen: Cardinal;
                                             bpmbalign: Boolean=True): Cardinal;
  procedure ADFSCalcFileDate(var Entry: TDirEntry);
  function CreateADFSDirectory(var dirname,parent,attributes: String): Integer;
  procedure UpdateADFSCat(directory: String;newname: String='');
  function UpdateADFSFileAttributes(filename,attributes: String): Boolean;
  function ValidateADFSFilename(filename: String): String;
  function RetitleADFSDirectory(filename,newtitle: String): Boolean;
  function RenameADFSFile(oldfilename: String;var newfilename: String): Integer;
  procedure ConsolidateADFSFreeSpaceMap;
  procedure ConsolidateADFSFragments(fragid: Cardinal);
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
  function FixADFSDirectory(dir,entry: Integer):Boolean;
  function ADFSGetHardDriveParams(Ldiscsize: Cardinal; bigmap,ide: Boolean;
                                 var Lidlen,Lzone_spare,Lnzones,Llog2bpmb,Lroot,
                                     Llog2secsize,Llowsec: Cardinal): Boolean;
  function UpdateADFSFileAddr(filename: String; newaddr: Cardinal;
                                                        load: Boolean): Boolean;
  function UpdateADFSFileType(filename:String;newtype:String):Boolean;
  function UpdateADFSTimeStamp(filename:String;newtimedate:TDateTime):Boolean;
  function ExtractADFSPartition(side: Cardinal): TDIByteArray;
  function GetADFSMaxLength(lastentry:Boolean): Cardinal;
  function ADFSReport(CSV: Boolean): TStringList;
  procedure ADFSUpdateMasterSequence(dir: Cardinal);
  procedure ADFSUpdateFileSequence(dir,entry: Cardinal);
  //Acorn FileStore Routines
  function ID_AFS: Boolean;
  procedure ReadAFSPartition;
  function ReadAFSDirectory(dirname: String; addr: Cardinal): TDir;
  function ExtractAFSFile(filename: String;var buffer: TDIByteArray): Boolean;
  function ReadAFSObject(offset: Cardinal): TDIByteArray;
  function GetAFSObjLength(offset: Cardinal): Cardinal;
  function GetAllocationMap: Cardinal;
  function GetAllocationMap(sector: Cardinal;
                                         var spt: Cardinal): Cardinal; overload;
  procedure ReadAFSFSM;
  function AFSGetFreeSectors(used: Boolean=False): TFragmentArray;
  function AFSAllocateFreeSpace(size :Cardinal;
              var fragments: TFragmentArray; addheader: Boolean=True): Cardinal;
  procedure AFSDeAllocateFreeSpace(addr: Cardinal);
  procedure FinaliseAFSL2Map;
  function FormatAFS(harddrivesize: Cardinal;afslevel: Byte): Boolean;
  procedure WriteAFSPartition(afsdisctitle: String;harddrivesize: Cardinal);
  function CreateAFSDirectory(dirname,parent,attributes: String): Integer;
  function ExtendAFSDirectory(sector: Cardinal):Cardinal;
  function CreateAFSPassword(Accounts: TUserAccounts): Integer;
  function ReadAFSPassword: TUserAccounts;
  function WriteAFSFile(var file_details: TDirEntry;
                                             var buffer: TDIByteArray): Integer;
  function AFSAttrToByte(attr: String): Byte;
  procedure WriteAFSObject(offset: Cardinal;var buffer: TDIByteArray);
  function RenameAFSFile(oldname:String;var newname: String): Integer;
  procedure UpdateAFSDirectory(dirname: String);
  function ValidateAFSFilename(filename: String): String;
  function DeleteAFSFile(filename: String): Boolean;
  function RemoveAFSEntry(dir,entry: Cardinal): Boolean;
  function InsertAFSEntry(dir: Cardinal;file_details:TDirEntry): Integer;
  function MoveAFSFile(filename,directory: String): Integer;
  function UpdateAFSFileAddr(filename: String; newaddr: Cardinal;
                                                        load: Boolean): Boolean;
  function UpdateAFSTimeStamp(filename: String; newtimedate: TDateTime):Boolean;
  function UpdateAFSAttributes(filename,attributes: String): Boolean;
  function UpdateAFSDiscTitle(title: String): Boolean;
  function AddAFSPartition(size: Cardinal): Boolean;
  function AFSReport(CSV: Boolean): TStringList;
  //DFS Routines
  function ID_DFS: Boolean;
  function ReadDFSDisc(mmbdisc:Integer=-1): Boolean;
  procedure DFSFreeSpaceMap;
  function IsWatford(s: Integer): Boolean;
  function ConvertDFSSector(address,side: Integer): Integer;
  function WriteDFSFile(var file_details: TDirEntry;
                                             var buffer: TDIByteArray): Integer;
  procedure UpdateDFSCat(side: Integer);
  function ValidateDFSFilename(filename: String): String;
  function RenameDFSFile(oldfilename: String;var newfilename: String):Integer;
  function DeleteDFSFile(filename: String):Boolean;
  function UpdateDFSFileAttributes(filename,attributes: String): Boolean;
  function FormatDFS(minor,tracks: Byte): Boolean;
  function UpdateDFSDiscTitle(title: String;side: Byte): Boolean;
  function UpdateDFSBootOption(option,side: Byte): Boolean;
  function ExtractDFSFile(filename: String;var buffer: TDIByteArray): Boolean;
  function UpdateDFSFileAddr(filename: String; newaddr: Cardinal;
                                                         load: Boolean):Boolean;
  function ExtractDFSPartition(side: Cardinal): TDIByteArray;
  function AddDFSBlankSide(tracks: Byte): Boolean;
  function AddDFSSide(var buffer: TDIByteArray): Boolean;
  function AddDFSSide(filename: String): Boolean; overload;
  function MoveDFSFile(filename,directory: String): Integer;
  function DFSReport(CSV: Boolean): TStringList;
  procedure DFSUpdateSequence(side: Integer);
  //Commodore 1541/1571/1581 Routines
  function ID_CDR: Boolean;
  function ConvertDxxTS(format,track,sector: Integer): Integer;
  function ReadCDRDisc: Boolean;
  function FormatCDR(minor: Byte): Boolean;
  procedure CDRFreeSpaceMap;
  procedure CDRSetClearBAM(track,sector: Byte; used: Boolean);
  function UpdateCDRDiscTitle(title: String): Boolean;
  function ExtractCDRFile(filename: String; var buffer:TDIByteArray): Boolean;
  function WriteCDRFile(file_details: TDirEntry;
                                             var buffer: TDIByteArray): Integer;
  procedure UpdateCDRCat;
  function CDRFindNextSector(var track,sector: Byte): Boolean;
  function CDRFindNextTrack(var track,sector: Byte): Boolean;
  function RenameCDRFile(oldfilename: String;var newfilename: String):Integer;
  function DeleteCDRFile(filename: String):Boolean;
  function UpdateCDRFileAttributes(filename,attributes: String): Boolean;
  function CDRReport{(CSV: Boolean)}: TStringList;
  //Sinclair Spectrum +3/Amstrad Routines
  function ID_Sinclair: Boolean;
  function ReadSinclairDisc: Boolean;
  function FormatSpectrum(minor: Byte): Boolean;
  function WriteSpectrumFile(file_details: TDirEntry;
                                             var buffer: TDIByteArray): Integer;
  function RenameSpectrumFile(oldfilename: String;
                                               var newfilename: String):Integer;
  function DeleteSinclairFile(filename: String):Boolean;
  function UpdateSinclairFileAttributes(filename,attributes: String): Boolean;
  function UpdateSinclairDiscTitle(title: String): Boolean;
  function ExtractSpectrumFile(filename:String;var buffer:TDIByteArray):Boolean;
  //Commodore Amiga Routines
  function ID_Amiga: Boolean;
  function ReadAmigaDisc: Boolean;
  function ReadAmigaDir(dirname: String; offset: Cardinal): TDir;
  function AmigaBootChecksum(offset: Cardinal): Cardinal;
  function AmigaChecksum(offset: Cardinal): Cardinal;
  function ExtractAmigaFile(filename: String; var buffer: TDIByteArray):Boolean;
  function ExtractAmigaData(sector,filelen: Cardinal;
                                             var buffer: TDIByteArray): Boolean;
  function FormatAmigaFDD(minor: Byte): Boolean;
  function FormatAmigaHDD(harddrivesize: Cardinal): Boolean;
  procedure FormatAmiga(size: Cardinal);
  function WriteAmigaFile(var file_details: TDirEntry;
                                             var buffer: TDIByteArray): Integer;
  function CreateAmigaDirectory(var dirname,parent,attributes: String): Integer;
  function RenameAmigaFile(oldfilename: String;var newfilename: String):Integer;
  function DeleteAmigaFile(filename: String):Boolean;
  function UpdateAmigaFileAttributes(filename,attributes: String): Boolean;
  function UpdateAmigaDiscTitle(title: String): Boolean;
  function MoveAmigaFile(filename,directory: String): Integer;
  procedure ReadAmigaFSM;
  procedure AmigaFillFreeSpaceMap(address: Cardinal;usage: Byte);
  procedure ToAmigaTime(time: TDateTime;var days,mins,ticks: Cardinal);
  function FromAmigaTime(days, mins, ticks: Cardinal): TDateTime;
  function AmigaIntToStrAttr(attr: Cardinal): String;
  function AmigaStrToIntAttr(attr: String): Cardinal;
  function AmigaCalculateHashValue(filename: String): Cardinal;
  procedure AmigaAllocateFSMBlock(addr:Cardinal;used:Boolean;
                                                          var fsm:TDIByteArray);
  function GetAmigaFSMOffset(addr: Cardinal;var bit: Byte): Cardinal;
  function AmigaReadBitmap(var fsm: TDIByteArray): TFragmentArray;
  procedure AmigaWriteBitmap(fsmlist: TFragmentArray;var fsm: TDIByteArray);
  function AmigaFindFreeSpace(filelen: Cardinal): TFragmentArray;
  function UpdateAmigaTimeStamp(filename: String;
                                               newtimedate: TDateTime): Boolean;
  function GetAmigaChain(sector: Cardinal): TFragmentArray;
  procedure AmigaAddToChain(filename: String;paraddr,sector: Cardinal);
  function AmigaRemoveFromChain(filename: String;
                                              paraddr,sector: Cardinal):Boolean;
  procedure ValidateAmigaFile(var filename: String);
  function AmigaReport{(CSV: Boolean)}: TStringList;
  //Acorn CFS Routines
  function ID_CFS: Boolean;
  function ReadUEFFile: Boolean;
  function CFSBlockStatus(status: Byte): String;
  function CFSTargetMachine(machine: Byte): String;
  function ExtractCFSFile(entry: Integer; var buffer:TDIByteArray): Boolean;
  procedure WriteUEFFile(filename: String; uncompress: Boolean=False);
  function FormatCFS: Boolean;
  function DeleteCFSFile(entry: Cardinal): Boolean;
  function UpdateCFSAttributes(entry: Cardinal; attributes:String): Boolean;
  function MoveCFSFile(entry: Cardinal;dest: Integer): Integer;
  function CopyCFSFile(entry: Cardinal;dest: Integer): Integer;
  function WriteCFSFile(var file_details: TDirEntry;
                                             var buffer: TDIByteArray): Integer;
  function RenameCFSFile(entry: Cardinal; newfilename: String): Integer;
  function UpdateCFSFileAddr(entry,newaddr: Cardinal; load: Boolean): Boolean;
  //Acorn RFS Routines
  function ID_RFS: Boolean;
  function ValidRFSHeader(ptr: Cardinal;cfs: Boolean=False): Boolean;
  function ReadRFSImage: Boolean;
  procedure AdjustRFSOffsets(base: Cardinal);
  function FormatRFS: Boolean;
  function FormatRFS(title: String): Boolean; overload;
  function FormatRFS(title,copyright: String): Boolean; overload;
  function FormatRFS(title,copyright,version: String): Boolean; overload;
  function FormatRFS(title,copyright,version: String;binvers: Byte): Boolean; overload;
  function WriteRFSHeader(title,copyright,version: String;binvers: Byte): Cardinal;
  function ExtractRFSFile(entry: Integer;var buffer:TDIByteArray):Boolean;
  function WriteRFSFile(var file_details: TDirEntry;var buffer: TDIByteArray;
                                                   insert: Integer=-1): Integer;
  function DeleteRFSFile(entry: Cardinal): Boolean;
  procedure RFSReAdjustPointers(filepos: Cardinal;diff: Integer);
  function MoveRFSFile(entry: Cardinal;dest: Integer): Integer;
  function CopyRFSFile(entry: Cardinal;dest: Integer): Integer;
  function RenameRFSFile(entry: Cardinal; newfilename: String): Integer;
  function GetRFSVersionNumber: Byte;
  procedure SetRFSVersionNumber(newvalue: Byte);
  function UpdateRFSHeader(title,copyright,version: String): Boolean;
  function UpdateRFSTitle(title: String): Boolean;    
  function UpdateRFSVersion(version: String): Boolean;
  function UpdateRFSCopyright(copyright: String): Boolean;
  //MMFS Routines
  function ID_MMB: Boolean;
  function ReadMMBDisc: Boolean;
  //Spark Routines
  function ID_Spark: Boolean;
  function ReadSparkArchive: Boolean;
  function ExtractSparkFile(filename: String;var buffer: TDIByteArray): Boolean;
  function FormatSpark(Zipfilename: String): Boolean;
  function DeleteSparkFile(filename: String): Boolean;
  function UpdateSparkAttributes(filename,attributes: String): Boolean;
  function MoveSparkFile(filename, dest: String): Integer;
  function WriteSparkFile(var file_details: TDirEntry;
                                             var buffer: TDIByteArray): Integer;
  function RenameSparkFile(filename, newfilename: String): Integer;
  function UpdateSparkFileType(filename: String; newtype: String): Boolean;
  function UpdateSparkTimeStamp(filename: String;
                                               newtimedate: TDateTime): Boolean;
  function UpdateSparkFileAddr(filename: String; newaddr: Cardinal;
                                                        load: Boolean): Boolean;
  function CreateSparkDirectory(filename, parent, attributes: String): Integer;
  //DOS Plus Routines
  function ID_DOSPlus: Boolean;
  function IDDOSPartition(ctr: Cardinal): Boolean;
  procedure ReadDOSPartition;
  function ReadDOSHeader: Boolean;
  function ReadDOSDirectory(dirname: String; addr: Cardinal;
                                                       var len: Cardinal): TDir;
  function DOSExtToFileType(ext: String): String;
  function ConvertDOSTimeDate(time,date: Word): TDateTime;
  function DOSTime(time: TDateTime): Word;
  function DOSDate(date: TDateTime): Word;
  function ConvertDOSAttributes(attr: Byte): String;
  function ConvertDOSAttributes(attr: String): Byte; overload;
  function DOSClusterToOffset(cluster: Cardinal): Cardinal;
  function GetClusterEntry(cluster: Cardinal): Cardinal;
  procedure SetClusterEntry(cluster,entry: Cardinal);
  function IsClusterValid(cluster: Cardinal): Boolean;
  function GetClusterChain(cluster: Cardinal; len: Cardinal=0): TFragmentArray;
  procedure SetClusterChain(fragments: TFragmentArray);
  function ExtractDOSFile(filename: String; var buffer: TDIByteArray): Boolean;
  function ReadDOSObject(cluster: Cardinal;len: Cardinal=0): TDIByteArray;
  procedure ReadDOSFSM;
  function DOSGetFreeSectors(used: Boolean=False): TFragmentArray;
  function RenameDOSFile(oldname: String; var newname: String): Integer;
  function ValidateDOSFilename(filename: String;long: Boolean=False): String;
  procedure UpdateDOSDirectory(dirname: String);
  procedure AllocateDOSClusters(len: Cardinal; var fragments: TFragmentArray);
  procedure DeAllocateDOSClusters(len: Cardinal; var fragments: TFragmentArray);
  function WriteDOSObject(buffer: TDIByteArray;
                                            fragments: TFragmentArray): Boolean;
  function WriteDOSFile(var file_details: TDirEntry;
                                              var buffer: TDIByteArray):Integer;
  function InsertDOSEntry(dir: Cardinal;direntry: TDirEntry): Integer;
  function CreateDOSDirectory(dirname,parent,attributes: String): Integer;
  function DeleteDOSFile(filename: String): Boolean;
  procedure RemoveDOSEntry(dir, entry: Cardinal);
  function UpdateDOSAttributes(filename,attributes: String): Boolean;
  function UpdateDOSDiscTitle(title: String): Boolean;
  function UpdateDOSTimeStamp(filename: String; newtimedate: TDateTime):Boolean;
  function AddDOSPartition(size: Cardinal): Boolean;
  function FormatDOS(size: QWord;fat: Byte): Boolean;
  procedure WriteDOSHeader(offset, size: QWord;fat: Byte;bootable: Boolean);
  procedure WriteDOSHeader(offset, size: QWord;fat: Byte;bootable: Boolean;
                                             var buffer: TDIByteArray);overload;
  function MoveDOSFile(filename,directory: String): Integer;
  function DOSShortFilename(path,LFN: String;SFN :String=''): String;
  function BuildDOSFilename(f,e: String): String;
  function DOSReport(CSV: Boolean): TStringList;
  //ISO Methods
  function ID_ISO: Boolean;
  procedure ISOFSM;
  procedure ReadISOImage;
  function ISOGetPVDDateTime(offset: Cardinal): TDateTime;
  procedure ISOReadVolumeDescriptors;
  procedure ISOGetVDAndPathToUse(var ISOVD2Use: Cardinal;var ISOpath2use: Cardinal);
  function ISOGetDirDateTime(offset: Cardinal): TDateTime;
  function ISOAttributes(attr: Cardinal; attributes: String;
                                                    pad: Boolean=False): String;
  function ISOGetFullPath(dir: Integer): String;
  procedure ISOChangeDirName(dir,entry: Cardinal);
  procedure ISOReadPathTable(VDNum: Cardinal; pth2use: Cardinal=0);
  procedure ISOReadDirectories(VDNum: Cardinal);
  function ExtractISOFile(filename: String; var buffer: TDIByteArray): Boolean;
  //Private constants
  const
   //Formats
   FFormatString: array[0..$C] of String = ('DFS',
                                            'Acorn ADFS',
                                            'Commodore',
                                            'Spectrum +3',
                                            'Commodore Amiga',
                                            'Acorn CFS',
                                            'MMFS',
                                            'Acorn FS',
                                            'Spark Archive',
                                            'SJ Research MDFS',
                                            'DOS',
                                            'Acorn ROM FS',
                                            'ISO');
   //When the change of number of sectors occurs on Commodore 1541/1571 discs
   CDRhightrack : array[0..8] of Integer = (71,66,60,53,36,31,25,18, 1);
   //Number of sectors per track (Commodore 1541/1571)
   CDRnumsects  : array[0..7] of Integer = (17,18,19,21,17,18,19,21);
   //Commodore 64 Filetypes
   CDRFileTypes : array[0.. 5] of String = (
                                   'DELDeleted'  ,'SEQSequence' ,'PRGProgram'  ,
                                   'USRUser File','RELRelative' ,'CBMCBM'      );
   //Interleave types
   Fints        : array[0..2] of String=('Sequential','Interleave','Multiplex');
   //Default disc title for new images
   defdisctitle     = 'DiscImgMgr';
   defafsdisctitle  = 'DiscImageManager'; //AFS has longer titles
   defamigadisctitle= 'Disc Image Manager';//Amiga has even longer titles
   defrfstitle      = 'Disc Image Manager';//ROM FS Header title
   defrfscopyright  = '(C)GJH Software';//Copyright string for ROM FS
   //Root name to use when AFS is partition on ADFS
   afsrootname   = ':AFS$';
   //ROM FS Signature
   RFSsig        = #$00#$00#$00#$4C;
   //ROM FS Copyright String signature
   RFScrt        = #$28#$43#$29;
   //ROM FS Code Header (ends with 'DiscImageManager')
   ROMHDR: array[$38..$96] of Byte = (
                $AA,$E0,$0E,$F0,$19,$E0,$0D,$D0,$37,$98,$20,$7E,$80,$90,$31,$20,
                $7C,$80,$85,$F5,$A9,$97,$85,$F6,$A9,$80,$85,$F7,$D0,$20,$20,$7C,
                $80,$D0,$1D,$A5,$F7,$10,$04,$18,$6D,$86,$80,$AA,$09,$80,$85,$F7,
                $AC,$85,$80,$B1,$F6,$86,$F7,$E6,$F6,$D0,$02,$E6,$F7,$A8,$A2,$00,
                $8A,$A6,$F4,$60,$B5,$E7,$49,$0F,$29,$0F,$C5,$F4,$60,$00,$80,$44,
                $69,$73,$63,$49,$6D,$61,$67,$65,$4D,$61,$6E,$61,$67,$65,$72);
   //ROM FS usual size
   ROMFSSize     = 16384;
   //Attributes
   AmigaAttributes   = 'DEWRAPSHdewrlxia                ';
   ADFSOldAttributes = 'RWLDErweP ';//Not to be confused with what is returned from OSFILE
   ADFSNewAttributes = 'RWLDrw';
   {$INCLUDE 'DiscImageRISCOSFileTypes.pas'}
   {$INCLUDE 'DiscImageDOSFileTypes.pas'}
 published
  //Published methods
  function AddPartition(size: Cardinal;format: Byte): Boolean;
  function AddPartition(tracks: Byte): Boolean; overload;
  function AddPartition(filename: String): Boolean; overload;
  procedure BeginUpdate;
  function ChangeFileType(filename,newtype: String): Boolean;
  function ChangeInterleaveMethod(NewMethod: Byte): Boolean;
  procedure Close;
  function CopyFile(filename,directory: String): Integer;
  function CopyFile(filename,directory,newfilename: String): Integer; overload;
  function CopyFile(source: Cardinal;dest: Integer): Integer; overload;
  constructor Create;
  constructor Create(Clone: TDiscImage); overload;
  function CreateDirectory(var filename,parent,attributes: String): Integer;
  procedure CreateINFFile(dir,entry: Integer; path: String;filename: String='');
  function CreatePasswordFile(Accounts: TUserAccounts): Integer;
  procedure CreateRootInf(filename: String; dir: Integer);
  function DeleteFile(filename: String): Boolean;
  function DeleteFile(entry: Cardinal): Boolean; overload;
  function DiscSize(partition: QWord):QWord;
  procedure EndUpdate;
  function ExtractFile(filename:String;var buffer:TDIByteArray;
                                                     entry:Cardinal=0): Boolean;
  function ExtractFile(filename:String;Stream:TStream;entry:Cardinal=0)
                                                             : Boolean;overload;
  function FileExists(filename: String;var Ref: Cardinal;
                                                   sfn: Boolean=False): Boolean;
  function FileExists(filename: String;var dir,entry: Cardinal;
                                         sfn: Boolean=False): Boolean; overload;
  function FileExists(filename: String;var dir,entry: Integer;
                                         sfn: Boolean=False): Boolean; overload;
  function FileSearch(search: TDirEntry;AddTo: TSearchResults=nil): TSearchResults;
  function FixDirectories: Boolean;
  function FormatFDD(major: Word;minor: Byte;tracks: Byte=0): Boolean;
  function FormatFDD(major: Word;filename: String): Boolean; overload;
  function FormatFDD(major: Word): Boolean; overload;
  function FormatFDD(major: Word;title: String;version: String;
                            copyright: String;binvers: Byte): Boolean; overload;
  function FormatHDD(major:Word;harddrivesize:Cardinal):Boolean;
  function FormatHDD(major: Word;harddrivesize: Cardinal;
                                              dirtype: Byte): Boolean; overload;
  function FormatHDD(major:Word;harddrivesize:Cardinal;ide,newmap:Boolean;
                              dirtype:Byte;addheader:Boolean):Boolean; overload;
  function FreeSpace(partition: QWord):QWord;
  function GetDirSep(partition: Byte): Char;
  function GetFileCRC(filename: String;entry:Cardinal=0): String;
  function GetFileMD5(filename: String;entry:Cardinal=0): String;
  function GetFileType(filetype: String): Integer;
  function GetFileType(filetype: Integer): String; overload;
  function GetInterleaveString(Inter: Byte): String;
  function GetMaxLength: Cardinal;
  function GetNumberOfInterleaves: Byte;
  function GetParent(dir: Integer): String;
  function GetWindowsFilename(dir,entry: Integer): String;
  function IDImage: Boolean;
  function ImageReport(CSV: Boolean): TStringList;
  function LoadFromFile(filename: String;readdisc: Boolean=True): Boolean;
  function MoveFile(filename,directory: String): Integer;
  function MoveFile(source: Cardinal;dest: Integer): Integer; overload;
  function ReadDirectory(dirname: String): Integer;
  function ReadDiscData(addr,count,side,offset: Cardinal;
                                             var buffer: TDIByteArray): Boolean;
  procedure ReadImage;
  function ReadPasswordFile: TUserAccounts;
  function RenameFile(oldfilename: String;var newfilename: String): Integer;
  function RenameFile(entry: Cardinal;var newfilename: String):Integer;overload;
  function RetitleDirectory(var filename,newtitle: String): Boolean;
  function SaveFilter(var FilterIndex: Integer;thisformat: Integer=-1):String;
  function SaveToFile(filename: String;uncompress: Boolean=False): Boolean;
  function SeparatePartition(side: Cardinal;filename: String=''): Boolean;
  function TimeStampFile(filename: String;newtimedate: TDateTime): Boolean;
  function Title(partition: Cardinal):String;
  function UpdateAttributes(filename,attributes: String;
                                                     entry:Cardinal=0): Boolean;
  function UpdateBootOption(option,side: Byte): Boolean;
  function UpdateCopyright(copyright: String): Boolean;
  function UpdateDiscTitle(NewTitle: String;side: Byte): Boolean;
  function UpdateExecAddr(filename:String;newaddr:Cardinal;
                                                     entry:Cardinal=0): Boolean;
  function UpdateLoadAddr(filename:String;newaddr:Cardinal;
                                                     entry:Cardinal=0): Boolean;
  function UpdateVersionString(version: String): Boolean;
  procedure ValidateAttributes(var attributes: String);
  function ValidateFilename(parent:String;var filename:String): Boolean;
  function WriteDiscData(addr,side: Cardinal;var buffer: TDIByteArray;
                                    count: Cardinal;start: Cardinal=0): Boolean;
  function WriteFile(var file_details: TDirEntry;
                      var buffer: TDIByteArray;ShowFSM: Boolean=False): Integer;
  //Published properties
  property AddImpliedAttributes:Boolean       read FAddImpliedAttributes
                                              write FAddImpliedAttributes;
  property AFSPresent:          Boolean       read FAFSPresent;
  property AFSRoot:             Cardinal      read Fafsroot;
  property AllowDFSZeroSectors: Boolean       read FDFSzerosecs
                                              write FDFSzerosecs;
  property Copyright:           String        read Fcopyright;
  property DFSBeyondEdge:       Boolean       read FDFSBeyondEdge
                                              write FDFSBeyondEdge;
  property DFSAllowBlanks:      Boolean       read FDFSAllowBlank
                                              write FDFSAllowBlank;
  property BootOpt:             TDIByteArray  read bootoption;
  property CRC32:               String        read GetImageCrc;
  property CreateDSC:           Boolean       read FcreateDSC
                                              write FcreateDSC;
  property DefaultDiscTitle:    String        read Fdisctitle
                                              write SetDefaultDiscTitle;
  property DefaultAFSDiscTitle: String        read Fafsdisctitle
                                              write SetDefaultAFSDiscTitle;
  property DefaultAmigaDiscTitle:String       read Famigadisctitle
                                              write SetDefaultAmigaDiscTitle;
  property DefaultRFSTitle:     String        read Frfstitle
                                              write SetDefaultRFSTitle;
  property DefaultRFSCopyRight: String        read Frfscopyright
                                              write SetDefaultRFSCopyright;
  property DirectoryCapable:    Boolean       read FHasDirs;
  property DirectoryType:       Byte          read FDirType;
  property DirectoryTypeString: String        read DirTypeToString;
  property DirSep:              Char          read dir_sep;
  property Disc:                TDisc         read FDisc;
  property DOSPlusRoot:         Cardinal      read Fdosroot;
  property DOSPresent:          Boolean       read FDOSPresent;
  property DoubleSided:         Boolean       read GetDoubleSided;
  property Filename:            String        read imagefilename
                                              write imagefilename;
  property FormatExt:           String        read FormatToExt;
  property FormatNumber:        Word          read FFormat;
  property FormatString:        String        read FormatToString;
  property FreeSpaceMap:        TSide         read free_space_map;
  property HDDHeads:            Byte          read heads;
  property HeaderSize:          Cardinal      read emuheader;
  property InterleaveInUse:     String        read InterleaveString;
  property InterleaveInUseNum:  Byte          read FInterleave;
  property InterleaveMethod:    Byte          read FForceInter
                                              write FForceInter;
  property ISOFormatNumber:     Word          read FISOFormat;
  property ISOFormatString:     String        read ISOFormatToString;
  property MajorFormatNumber:   Word          read GetMajorFormatNumber;
  property MapType:             Byte          read MapFlagToByte;
  property MapTypeString:       String        read MapTypeToString;
  property MaxDirectoryEntries: Cardinal      read FMaxDirEnt;
  property MinorFormatNumber:   Byte          read GetMinorFormatNumber;
  property OpenDOSPartitions:   Boolean       read FOpenDOSPart
                                              write FOpenDOSPart;
  property Partitions:          TPartitions   read FPartitions;
  property ProgressIndicator:   TProgressProc write FProgress;
  property RAWData:             TDIByteArray  read Fdata;
  property RootAddress:         Cardinal      read GetRootAddress;
  property RootName:            String        read root_name;
  property ScanSubDirs:         Boolean       read FScanSubDirs
                                              write FScanSubDirs;
  property Sectors:             Byte          read secspertrack;
  property SparkAsFS:           Boolean       read FSparkAsFS
                                              write FSparkAsFS;
  property VersionNumber:       Byte          read GetRFSVersionNumber
                                              write SetRFSVersionNumber;
  property VersionString:       String        read Fversion;
 public
  destructor Destroy; override;
 End;

implementation

{This unit is split into sub units. Some code is replicated in the different
sub units. This is so each filing system can have it's own methods.}
{$INCLUDE 'DiscImageUtils.pas'}     //General purpose methods
{$INCLUDE 'DiscImage_Private.pas'}  //Module for private methods
{$INCLUDE 'DiscImage_Published.pas'}//Module for published methods
{$INCLUDE 'DiscImage_ADFS.pas'}     //Module for Acorn ADFS
{$INCLUDE 'DiscImage_AFS.pas'}      //Module for Acorn File Server
{$INCLUDE 'DiscImage_DFS.pas'}      //Module for Acorn/Watford DFS
{$INCLUDE 'DiscImage_C64.pas'}      //Module for Commodore 1541/1571/1581
{$INCLUDE 'DiscImage_Spectrum.pas'} //Module for Spectrum +3/Amstrad DSK
{$INCLUDE 'DiscImage_Amiga.pas'}    //Module for Commodore AmigaDOS
{$INCLUDE 'DiscImage_CFS.pas'}      //Module for Acorn Cassette Filing System (UEF)
{$INCLUDE 'DiscImage_RFS.pas'}      //Module for Acorn ROM FS
{$INCLUDE 'DiscImage_MMB.pas'}      //Module for MMFS - to be removed
{$INCLUDE 'DiscImage_Spark.pas'}    //Module for SparkFS
{$INCLUDE 'DiscImage_DOSPlus.pas'}  //Module for Acorn DOS Plus
{$INCLUDE 'DiscImage_ISO.pas'}      //Module for ISO

end.
