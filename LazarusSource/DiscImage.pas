unit DiscImage;

//This project is now covered by the GNU GPL v3 licence

{$MODE objFPC}{$H+}

interface

uses Classes,DiscImageUtils,Math,crc,ZStream,StrUtils;

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
  FBootBlock    : Boolean;      //Is disc an AmigaDOS Kickstart?
  secsize,                      //Sector Size
  bpmb,                         //Bits Per Map Bit (Acorn ADFS New)
  nzones,                       //Number of zones (Acorn ADFS New)
  root,                         //Root address
  bootmap,                      //Offset of the map (Acorn ADFS)
  zone_spare,                   //Spare bits between zones (Acorn ADFS New)
  format_vers,                  //Format version (Acorn ADFS New)
  root_size,                    //Size of the root directory (Acorn ADFS New)
  disc_id,                      //Disc ID (Acorn ADFS)
  emuheader,                    //Allow for any headers added by emulators
  namesize      : Cardinal;     //Size of the name area (Acorn ADFS Big Dir)
  disc_size,                    //Size of disc in bytes
  free_space    : Int64;        //Free space remaining
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
  imagefilename : String;       //Filename of the disc image
  dir_sep       : Char;         //Directory Separator
  free_space_map: TSide;        //Free Space Map
  bootoption    : TDIByteArray; //Boot Option(s)
  CFSFiles      : array of TDIByteArray;//All the data for the CFS files
  FProgress     : TProgressProc;//Used for feedback
  procedure ResetVariables;
  function ReadString(ptr,term: Integer;control: Boolean=True): String;
  function FormatToString: String;
  function FormatToExt: String;
  function ReadBits(offset,start,length: Cardinal): Cardinal;
  procedure WriteBits(value,offset,start,length: Cardinal);
  function RISCOSToTimeDate(filedatetime: Int64): TDateTime;
  function TimeDateToRISCOS(delphitime: TDateTime): Int64;
  function Read32b(offset: Cardinal; bigendian: Boolean=False): Cardinal;
  function Read24b(offset: Cardinal; bigendian: Boolean=False): Cardinal;
  function Read16b(offset: Cardinal; bigendian: Boolean=False): Word;
  function ReadByte(offset: Cardinal): Byte;
  procedure Write32b(value, offset: Cardinal; bigendian: Boolean=False);
  procedure Write24b(value, offset: Cardinal; bigendian: Boolean=False);
  procedure Write16b(value: Word; offset: Cardinal; bigendian: Boolean=False);
  procedure WriteByte(value: Byte; offset: Cardinal);
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
  //ADFS Routines
  function ID_ADFS: Boolean;
  function ReadADFSDir(dirname: String; sector: Cardinal): TDir;
  function CalculateADFSDirCheck(sector: Cardinal): Byte;
  function NewDiscAddrToOffset(addr: Cardinal;offset:Boolean=True): TFragmentArray;
  function OldDiscAddrToOffset(disc_addr: Cardinal): Cardinal;
  function OffsetToOldDiscAddr(offset: Cardinal): Cardinal;
  function ByteChecksum(offset,size: Cardinal): Byte;
  function ReadADFSDisc: TDisc;
  procedure ADFSFreeSpaceMap;
  procedure ADFSFillFreeSpaceMap(address: Cardinal;usage: Byte);
  function FormatADFS(minor: Byte): TDisc;
  function UpdateADFSDiscTitle(title: String): Boolean;
  function UpdateADFSBootOption(option: Byte): Boolean;
  function ADFSGetFreeFragments(offset:Boolean=True): TFragmentArray;
  function WriteADFSFile(var file_details: TDirEntry;var buffer: TDIByteArray;
                         directory:Boolean=False;extend:Boolean=True): Integer;
  function CreateADFSDirectory(var dirname,parent,attributes: String): Integer;
  procedure UpdateADFSCat(directory: String);
  function UpdateADFSFileAttributes(filename,attributes: String): Boolean;
  function ValidateADFSFilename(filename: String): String;
  function RetitleADFSDirectory(filename,newtitle: String): Boolean;
  function RenameADFSFile(oldfilename: String;var newfilename: String):Integer;
  procedure ConsolodateADFSFreeSpaceMap;
  function DeleteADFSFile(filename: String;
                         TreatAsFile:Boolean=False;extend:Boolean=True):Boolean;
  function ExtractADFSFile(filename: String;var buffer: TDIByteArray): Boolean;
  function MoveADFSFile(filename,directory: String): Integer;
  function ExtendADFSBigDir(dir: Cardinal;space: Integer;add: Boolean):Boolean;
  function ADFSBigDirSizeChange(dir,entry:Cardinal;extend:Boolean): Boolean;
  function ExtendADFSCat(dir: Cardinal;direntry: TDirEntry): Cardinal;
  procedure ReduceADFSCat(dir,entry: Cardinal);
  function FixBrokenADFSDirectories: Boolean;
  procedure FixADFSDirectory(dir,entry: Integer);
  //DFS Routines
  function ID_DFS: Boolean;
  function ReadDFSDisc: TDisc;
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
  procedure WriteUEFFile(filename: String;uncompress: Boolean=True);
  function FormatCFS:TDisc;
  function DeleteCFSFile(entry: Cardinal): Boolean;
  function UpdateCFSAttributes(entry: Cardinal;attributes:String): Boolean;
  function MoveCFSFile(entry: Cardinal;dest: Integer): Integer;
  function WriteCFSFile(var file_details: TDirEntry;var buffer: TDIByteArray): Integer;
  function RenameCFSFile(entry: Cardinal;newfilename: String): Integer;
  const
   //When the change of number of sectors occurs on Commodore 1541/1571 discs
   CDRhightrack : array[0..8] of Integer = (71,66,60,53,36,31,25,18, 1);
   //Number of sectors per track (Commodore 1541/1571)
   CDRnumsects  : array[0..7] of Integer = (17,18,19,21,17,18,19,21);
   //Commodore 64 Filetypes
   CDRFileTypes : array[0.. 5] of String = (
                                   'DELDeleted'  ,'SEQSequence' ,'PRGProgram'  ,
                                   'USRUser File','RELRelative' ,'CBMCBM'      );
   {$INCLUDE 'DiscImageRISCOSFileTypes.pas'}
 published
  //Methods
  constructor Create;
  function LoadFromFile(filename: String;readdisc: Boolean=True): Boolean;
  function IDImage: Boolean;
  procedure ReadImage;
  procedure SaveToFile(filename: String;uncompress: Boolean=True);
  procedure Close;
  function Format(major,minor,tracks: Byte): Boolean;
  function ExtractFile(filename:String;var buffer:TDIByteArray;entry:Cardinal=0): Boolean;
  function WriteFile(var file_details: TDirEntry; var buffer: TDIByteArray): Integer;
  function FileExists(filename: String; var Ref: Cardinal): Boolean;
  function ReadDiscData(addr,count,side: Cardinal;var buffer): Boolean;
  function WriteDiscData(addr,side: Cardinal;var buffer: TDIByteArray;
                                    count: Cardinal;start: Cardinal=0): Boolean;
  function FileSearch(search: TDirEntry): TSearchResults;
  function RenameFile(oldfilename: String;var newfilename: String;entry: Cardinal=0): Integer;
  function DeleteFile(filename: String;entry: Cardinal=0): Boolean;
  function MoveFile(filename, directory: String): Integer;
  function MoveFile(source: Cardinal;dest: Integer): Integer; overload;
  function CopyFile(filename, directory: String): Integer;
  function UpdateAttributes(filename,attributes: String;entry:Cardinal=0): Boolean;
  function UpdateDiscTitle(title: String;side: Byte): Boolean;
  function UpdateBootOption(option,side: Byte): Boolean;
  function CreateDirectory(var filename,parent,attributes: String): Integer;
  function RetitleDirectory(var filename,newtitle: String): Boolean;
  function GetFileCRC(filename: String;entry:Cardinal=0): String;
  function FixDirectories: Boolean;
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
  property RootAddress:         Cardinal      read root;
  property CRC32:               String        read GetImageCrc;
  property ProgressIndicator:   TProgressProc write FProgress;
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
 secsize       :=$0000;
 bpmb          :=$0000;
 nzones        :=$0000;
 root          :=$0000;
 bootmap       :=$0000;
 zone_spare    :=$0000;
 format_vers   :=$0000;
 root_size     :=$0000;
 disc_id       :=$0000;
 disc_size     :=$0000;
 free_space    :=$0000;
 FFormat       :=$FF;
 secspertrack  :=$00;
 heads         :=$00;
 density       :=$00;
 idlen         :=$00;
 skew          :=$00;
 SetLength(bootoption,0);
 lowsector     :=$00;
 disctype      :=$00;
 FDirType      :=$FF;
 share_size    :=$00;
 big_flag      :=$00;
 disc_name     :='';
 emuheader     :=$0000;
 dir_sep       :='.';
 root_name     :='$';
 imagefilename :='';
 SetLength(free_space_map,0);
end;

{-------------------------------------------------------------------------------
Extract a string from ptr to the next chr(term) or length(-term)
-------------------------------------------------------------------------------}
function TDiscImage.ReadString(ptr,term: Integer;control: Boolean=True): String;
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
 r:=ReadByte(ptr+x);
 while (r>=c) and //Test for control character
       (((r<>term) and (term>=0)) or //Test for terminator character
        ((x<abs(term)) and (term<0))) do //Test for string length
 begin
  Result:=Result+chr(r); //Add it to the string
  inc(x);                //Increase the counter
  r:=ReadByte(ptr+x);    //Read the next character
 end;
end;

{-------------------------------------------------------------------------------
Convert a format byte to a string
-------------------------------------------------------------------------------}
function TDiscImage.FormatToString: String;
const
 FS  : array[0..5] of String = ('DFS',
                                'Acorn ADFS',
                                'Commodore',
                                'Sinclair Spectrum +3/Amstrad',
                                'Commodore Amiga',
                                'Acorn CFS');
 SUB : array[0..5] of array[0..15] of String =
 (('Acorn SSD','Acorn DSD','Watford SSD','Watford DSD','','','','','','','','','','','',''),
  ('S','M','L','D','E','E+','F','F+','','','','','','','','Hard Disc'),
  ('1541','1571','1581','1541 40 Track','1571 80 Track','','','','','','','','','','',''),
  ('','Extended','','','','','','','','','','','','','',''),
  ('DD','HD','','','','','','','','','','','','','','Hard Disc'),
  ('','','','','','','','','','','','','','','',''));
begin
 if FFormat<>$FF then
 begin
  Result:= FS[FFormat DIV $10]+' '
         +SUB[FFormat DIV $10,FFormat MOD $10];
 end
 else Result:='unknown';
end;

{-------------------------------------------------------------------------------
Convert a format byte to an extension
-------------------------------------------------------------------------------}
function TDiscImage.FormatToExt: String;
const
 EXT : array[0..5] of array[0..15] of String =
 (('ssd','dsd','ssd','dsd','','','','','','','','','','','',''),
  ('ads','adm','adl','adf','adf','adf','adf','adf','','','','','','','','hdf'),
  ('d64','d71','d81','d64','d71','','','','','','','','','','',''),
  ('','dsk','','','','','','','','','','','','','',''),
  ('adf','adf','','','','','','','','','','','','','','hdf'),
  ('uef','','','','','','','','','','','','','','',''));
begin
 if FFormat<>$FF then
 begin
  Result:=EXT[FFormat DIV $10,FFormat MOD $10];
 end
 else Result:='unk';
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
 if (length>0) and (length<33) then
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
begin
 Result:=$FFFFFFFF; //Default value
 //Big Endian
 if bigendian then
  Result:=ReadByte(offset+3)
         +ReadByte(offset+2)*$100
         +ReadByte(offset+1)*$10000
         +ReadByte(offset+0)*$1000000
 else
  //Little Endian
  Result:=ReadByte(offset+0)
         +ReadByte(offset+1)*$100
         +ReadByte(offset+2)*$10000
         +ReadByte(offset+3)*$1000000;
end;

{-------------------------------------------------------------------------------
Read in 3 bytes
-------------------------------------------------------------------------------}
function TDiscImage.Read24b(offset: Cardinal; bigendian: Boolean=False): Cardinal;
begin
 Result:=$FFFFFF; //Default value
 //Big Endian
 if bigendian then
  Result:=ReadByte(offset+2)
         +ReadByte(offset+1)*$100
         +ReadByte(offset+0)*$10000
 else
  //Little Endian
  Result:=ReadByte(offset+0)
         +ReadByte(offset+1)*$100
         +ReadByte(offset+2)*$10000;
end;

{-------------------------------------------------------------------------------
Read in 2 bytes
-------------------------------------------------------------------------------}
function TDiscImage.Read16b(offset: Cardinal; bigendian: Boolean=False): Word;
begin
 Result:=$FFFF; //Default value
 //Big Endian
 if bigendian then
  Result:=ReadByte(offset+1)
         +ReadByte(offset+0)*$100
 else
  //Little Endian
  Result:=ReadByte(offset+0)
         +ReadByte(offset+1)*$100;
end;

{-------------------------------------------------------------------------------
Read in a byte
-------------------------------------------------------------------------------}
function TDiscImage.ReadByte(offset: Cardinal): Byte;
begin
 Result:=$FF; //Default value
 //Compensate for interleaving (ADFS L)
 if FFormat=$12 then offset:=OldDiscAddrToOffset(offset);
 //Compensate for emulator header
 inc(offset,emuheader);
 //If we are inside the data, read the byte
 if offset<GetDataLength then
  Result:=Fdata[offset];
end;

{-------------------------------------------------------------------------------
Write 4 bytes (word)
-------------------------------------------------------------------------------}
procedure TDiscImage.Write32b(value, offset: Cardinal; bigendian: Boolean=False);
begin
 if bigendian then
 begin
  //Big Endian
  WriteByte( value mod $100             ,offset+3);
  WriteByte((value div $100)    mod $100,offset+2);
  WriteByte((value div $10000)  mod $100,offset+1);
  WriteByte((value div $1000000)mod $100,offset+0);
 end
 else
 begin
  //Little Endian
  WriteByte( value mod $100,             offset+0);
  WriteByte((value div $100)    mod $100,offset+1);
  WriteByte((value div $10000)  mod $100,offset+2);
  WriteByte((value div $1000000)mod $100,offset+3);
 end;
end;

{-------------------------------------------------------------------------------
Write 3 bytes
-------------------------------------------------------------------------------}
procedure TDiscImage.Write24b(value,offset: Cardinal; bigendian: Boolean=False);
begin
 if bigendian then
 begin
  //Big Endian
  WriteByte( value mod $100             ,offset+2);
  WriteByte((value div $100)    mod $100,offset+1);
  WriteByte((value div $10000)  mod $100,offset+0);
 end
 else
 begin
  //Little Endian
  WriteByte( value mod $100             ,offset+0);
  WriteByte((value div $100)    mod $100,offset+1);
  WriteByte((value div $10000)  mod $100,offset+2);
 end;
end;

{-------------------------------------------------------------------------------
Write 2 bytes
-------------------------------------------------------------------------------}
procedure TDiscImage.Write16b(value: Word; offset: Cardinal; bigendian: Boolean=False);
begin
 if bigendian then
 begin
  //Big Endian
  WriteByte( value mod $100             ,offset+1);
  WriteByte((value div $100)    mod $100,offset+0);
 end
 else
 begin
  //Little Endian
  WriteByte( value mod $100             ,offset+0);
  WriteByte((value div $100)    mod $100,offset+1);
 end;
end;

{-------------------------------------------------------------------------------
Write byte
-------------------------------------------------------------------------------}
procedure TDiscImage.WriteByte(value: Byte; offset: Cardinal);
begin
 //Compensate for interleaving (ADFS L)
 if FFormat=$12 then offset:=OldDiscAddrToOffset(offset);
 //Compensate for emulator header
 inc(offset,emuheader);
 //Will it go beyond the size of the array?
 if offset>=GetDataLength then
  SetDataLength(offset+1); //Then increase the array to compensate
 //Write the byte
 Fdata[offset]:=value;
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
  Directory:='';
  Title    :='';
  SetLength(Entries,0);
  Broken   :=False;
  ErrorCode:=$00;
 end;
end;

{-------------------------------------------------------------------------------
Convert the Map flag to Map Type
-------------------------------------------------------------------------------}
function TDiscImage.MapFlagToByte: Byte;
begin
 Result:=$FF;               //Default value for non-ADFS
 if FFormat div $10=$1 then //Is it ADFS?
 begin
  Result:=$00;              // ADFS Old Map
  if FMap then Result:=$01; // ADFS New Map
 end;
 if FFormat div $10=$4 then //Is it Amiga?
 begin
  Result:=$02;              // AmigaDOS OFS
  if FMap then Result:=$03; // AmigaDOS FFS
 end;
end;

{-------------------------------------------------------------------------------
Convert the Map flag to String
-------------------------------------------------------------------------------}
function TDiscImage.MapTypeToString: String;
begin
 Result:='Not ADFS/AmigaDOS';
 case MapFlagToByte of
  $00: Result:='ADFS Old Map';
  $01: Result:='ADFS New Map';
  $02: Result:='AmigaDOS OFS';
  $03: Result:='AmigaDOS FFS';
 end;
end;

{-------------------------------------------------------------------------------
Convert the Directory Type to String
-------------------------------------------------------------------------------}
function TDiscImage.DirTypeToString: String;
begin
 Result:='Not ADFS/AmigaDOS';
 case FDirType of
  $00: Result:='ADFS Old Directory';
  $01: Result:='ADFS New Directory';
  $02: Result:='ADFS Big Directory';
  $10: Result:='AmigaDOS Directory';
  $11: Result:='AmigaDOS Directory Cache';
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
 if FFormat shr 4=1 then Result:=FixBrokenADFSDirectories;
end;

{-------------------------------------------------------------------------------
Load an image from a file, unGZIPping it, if necessary
-------------------------------------------------------------------------------}
function TDiscImage.LoadFromFile(filename: String;readdisc: Boolean=True): Boolean;
var
 FGZDiscDrive: TGZFileStream;
 FDiscDrive  : TFileStream;
 chunk       : TDIByteArray;
 cnt,
 i,
 buflen      : Integer;
 compressed,
 readOK      : Boolean;
const
  ChunkSize=4096; //4K chunks
begin
 Result:=False;
 //Only read the file in if it actually exists (or rather, Windows can find it)
 if SysUtils.FileExists(filename) then
 begin
  //Blank off the variables
  ResetVariables;
  //First we'll read the first two bytes using conventional TFileStream
  try
   FDiscDrive:=TFileStream.Create(filename,fmOpenRead OR fmShareDenyNone);
   SetLength(chunk,2);
   readOK:=False;
   compressed:=False;
   //Have we read 2 bytes?
   if FDiscDrive.Read(chunk[0],2)=2 then
   begin
    //Are they the GZ magic numbers?
    if(chunk[0]=$1F)and(chunk[1]=$8B)then compressed:=True;
    readOK:=True;
   end;
   //Read a compressed stream
   if(readOK)and(compressed)then
   begin
    //Close the uncompressed stream first
    FDiscDrive.Free;
    try
     FGZDiscDrive:=TGZFileStream.Create(filename,gzOpenRead);
     //Counter
     buflen:=0;
     //Set up the chunk buffer
     SetLength(chunk,ChunkSize);
     repeat
      //Read in the next chunk
      cnt:=FGZDiscDrive.Read(chunk[0],ChunkSize);
      //Extend the buffer accordingly
      SetDataLength(buflen+cnt);
      //Copy the chunk into the buffer
      for i:=0 to cnt-1 do Fdata[buflen+i]:=chunk[i];
      //Increase the buffer length counter
      inc(buflen,cnt);
      //Until we are done
     until cnt<ChunkSize;
     //Close the stream
     FGZDiscDrive.Free;
    except
     FGZDiscDrive:=nil;
    end;
   end;
   //Read the uncompressed file
   if(not compressed)and(readOK)then
   begin
    try
     FDiscDrive.Position:=0;
     SetLength(Fdata,FDiscDrive.Size);
     FDiscDrive.Read(Fdata[0],FDiscDrive.Size);
     FDiscDrive.Free;
    except
     FDiscDrive:=nil;
    end;
   end;
  except
  end;
  //ID the image
  if(IDImage)and(readdisc)then ReadImage;
  //Return a true or false, depending on if FFormat is set.
  Result:=FFormat<>$FF;
  //Set the image filename
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
  if not ID_CDR      then //Commodore
  if not ID_Amiga    then //Amiga
  if not ID_DFS      then //Acorn DFS
  if not ID_Sinclair then //Sinclair/Amstrad
  if not ID_CFS      then //Acorn CFS
   ResetVariables;        //Reset everything
  //Just by the ID process:
  //ADFS 'F' can get mistaken for Commodore
  //Commodore and Amiga can be mistaken for DFS
 end;
 //Return a true or false result
 Result:=FFormat<>$FF;
end;

{-------------------------------------------------------------------------------
Read the disc in, depending on the format
-------------------------------------------------------------------------------}
procedure TDiscImage.ReadImage;
begin
 case FFormat shr 4 of
  0: FDisc:=ReadDFSDisc;     //Acorn DFS
  1: FDisc:=ReadADFSDisc;    //Acorn ADFS
  2: FDisc:=ReadCDRDisc;     //Commodore
  3: FDisc:=ReadSinclairDisc;//Sinclair/Amstrad
  4: FDisc:=ReadAmigaDisc;   //Amiga
  5: FDisc:=ReadUEFFile;     //Acorn CFS
 end;
end;

{-------------------------------------------------------------------------------
Saves an image to a file
-------------------------------------------------------------------------------}
procedure TDiscImage.SaveToFile(filename: String;uncompress: Boolean=True);
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
 if FFormat shr 4<>5 then //Not CFS
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
 if FFormat shr 4=5 then WriteUEFFile(filename,uncompress); //CFS
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
function TDiscImage.Format(major,minor,tracks: Byte): Boolean;
begin
 Result:=False;
 //Make sure the numbers are within bounds
 major :=major MOD $10;
 minor :=minor MOD $10;
 tracks:=tracks MOD 2;
 case major of
  0://Create DFS
   begin
    FDisc:=FormatDFS(minor,tracks);
    Result:=Length(FDisc)>0;
   end;
  1://Create ADFS
   begin
    FDisc:=FormatADFS(minor);
    Result:=Length(FDisc)>0;
   end;
  2://Create Commodore 64/128
   begin
    FDisc:=FormatCDR(minor);
    Result:=Length(FDisc)>0;
   end;
  3://Create Sinclair/Amstrad
   begin
    FDisc:=FormatSpectrum(minor);
    Result:=Length(FDisc)>0;
   end;
  4://Create AmigaDOS
   begin
    FDisc:=FormatAmiga(minor);
    Result:=Length(FDisc)>0;
   end;
  5://Create CFS
   begin
    FDisc:=FormatCFS;
    Result:=Length(FDisc)>0;
   end;
 end;
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path (CFS, entry is entry number)
-------------------------------------------------------------------------------}
function TDiscImage.ExtractFile(filename:String;var buffer:TDIByteArray;
                                entry:Cardinal=0): Boolean;
var
 m: Byte;
begin
 //Start with a false result
 Result:=False;
 m:=FFormat DIV $10; //Major format
 case m of
  0:Result:=ExtractDFSFile(filename,buffer);     //Extract DFS
  1:Result:=ExtractADFSFile(filename,buffer);    //Extract ADFS
  2:Result:=ExtractCDRFile(filename,buffer);     //Extract Commodore 64/128
  3:Result:=ExtractSpectrumFile(filename,buffer);//Extract Sinclair/Amstrad
  4:Result:=ExtractAmigaFile(filename,buffer);   //Extract AmigaDOS
  5:Result:=ExtractCFSFile(entry,buffer);        //Extract CFS
 end;
end;

{-------------------------------------------------------------------------------
Save a file into the disc image, from buffer
-------------------------------------------------------------------------------}
function TDiscImage.WriteFile(var file_details: TDirEntry; var buffer: TDIByteArray): Integer;
var
 m     : Byte;
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
  m:=FFormat DIV $10; //Major format
  //Can only write a file that will fit on the disc, or CFS
  if(count<=free_space)or(m=5) then
   case m of
    0:Result:=WriteDFSFile(file_details,buffer);     //Write DFS
    1:Result:=WriteADFSFile(file_details,buffer);    //Write ADFS
    2:Result:=WriteCDRFile(file_details,buffer);     //Write Commodore 64/128
    3:Result:=WriteSpectrumFile(file_details,buffer);//Write Sinclair/Amstrad
    4:Result:=WriteAmigaFile(file_details,buffer);   //Write AmigaDOS
    5:Result:=WriteCFSFile(file_details,buffer);     //Write CFS
   end;
 end;
end;

{-------------------------------------------------------------------------------
Create a directory
-------------------------------------------------------------------------------}
function TDiscImage.CreateDirectory(var filename,parent,attributes: String): Integer;
var
 m     : Byte;
begin
 //Start with a false result
 Result:=-1;
 m:=FFormat DIV $10; //Major format
 case m of
  0: exit;//Can't create directories on DFS
  1:      //Create directory on ADFS
    Result:=CreateADFSDirectory(filename,parent,attributes);
  2: exit;//Can't create directories on Commodore
  3: exit;//Can't create directories on Sinclair/Amstrad
  4:      //Create directory on AmigaDOS
    Result:=CreateAmigaDirectory(filename,parent,attributes);
  5: exit;//Can't create directories on CFS
 end;
end;

{-------------------------------------------------------------------------------
Retitle a directory
-------------------------------------------------------------------------------}
function TDiscImage.RetitleDirectory(var filename,newtitle: String): Boolean;
var
 m     : Byte;
begin
 //Start with a false result
 Result:=False;
 m:=FFormat DIV $10; //Major format
 case m of
  0: exit;//DFS doesn't have directories
  1:      //Retitle ADFS directory
    Result:=RetitleADFSDirectory(filename,newtitle);
  2: exit;//Commodore doesn't have directories
  3: exit;//Sinclair/Amstrad doesn't have directories
  4:      //Retitle AmigaDOS directory
    Result:=RetitleAmigaDirectory(filename,newtitle);
  5: exit;//CFS doesn't have directories
 end;
end;

{-------------------------------------------------------------------------------
Does a file exist?
-------------------------------------------------------------------------------}
function TDiscImage.FileExists(filename: String; var Ref: Cardinal): Boolean;
var
 Path   : array of String;
 i,j,l,
 ptr,
 level  : Integer;
 test,
 test2  : String;
begin
 Result:=False;
 //Not going to search if there is no tree to search in
 if Length(FDisc)>0 then
 begin
  SetLength(Path,0);
  filename:=filename;
  j:=-1;
  ptr:=0;
  //Explode the pathname into an array, without the '.'
  if FFormat div $10<>$0 then //Not DFS
   while (Pos(dir_sep,filename)<>0) do
   begin
    SetLength(Path,Length(Path)+1);
    Path[Length(Path)-1]:=Copy(filename,0,Pos(dir_sep,filename)-1);
    filename:=Copy(filename,Pos(dir_sep,filename)+1,Length(filename));
   end;
  if FFormat div $10=$0 then //DFS
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
   if (Length(filename)>0) and (FFormat div $10<>$0) then
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
   //DFS - if the test is on the other side
   if (FFormat=$01) and (test<>test2) then
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
   Ref:=ptr*$10000+j;
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
 //Simply copy from source to destination
 //ReadByte will compensate if offset is out of range
 //All but DFS
 if FFormat shr 4<>0 then
 begin
  SetLength(temp,count);
  if count>0 then
   for i:=0 to count-1 do
    temp[i]:=ReadByte(addr+i);
 end;
 //DFS
 if FFormat shr 4=0 then
 begin
  SetLength(temp,count);
  if count>0 then
   for i:=0 to count-1 do
    temp[i]:=ReadByte(ConvertDFSSector(addr+i,side));
 end;
 if count>0 then
  Move(temp[0],buffer,count);
 Result:=True;
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
  if FFormat DIV $10>0 then //not DFS
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
  if (UpperCase(S1)=UpperCase(S2)) and (S1<>'') then Result:=1;
 end;
 function CompCar(S1,S2: Cardinal): Byte; //Compares Cardinals/Integers
 begin
  Result:=0;
  if (S1=S2) and (S1<>0) then Result:=1;
 end;
 function CompTSt(S1,S2: TDateTime): Byte; //Compares TDateTime
 begin
  Result:=0;
  if (S1=S2) and (S1<>0) then Result:=1;
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
 //Work out what the search target is
 target:=0;
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
var
 m: Byte;
begin
 Result:=-1;//Failed to rename
 m:=FFormat DIV $10; //Major format
 case m of
  0:Result:=RenameDFSFile(oldfilename,newfilename);     //Rename DFS
  1:Result:=RenameADFSFile(oldfilename,newfilename);    //Rename ADFS
  2:Result:=RenameCDRFile(oldfilename,newfilename);     //Rename Commodore 64/128
  3:Result:=RenameSpectrumFile(oldfilename,newfilename);//Rename Sinclair/Amstrad
  4:Result:=RenameAmigaFile(oldfilename,newfilename);   //Rename AmigaDOS
  5:Result:=RenameCFSFile(entry,newfilename);           //Rename CFS
 end;
end;

{-------------------------------------------------------------------------------
Deletes a file (given full pathname)
-------------------------------------------------------------------------------}
function TDiscImage.DeleteFile(filename: String;entry: Cardinal=0): Boolean;
var
 m: Byte;
begin
 Result:=False;
 m:=FFormat DIV $10; //Major format
 case m of
  0:Result:=DeleteDFSFile(filename);     //Delete DFS
  1:Result:=DeleteADFSFile(filename);    //Delete ADFS
  2:Result:=DeleteCDRFile(filename);     //Delete Commodore 64/128
  3:Result:=DeleteSinclairFile(filename);//Delete Sinclair/Amstrad
  4:Result:=DeleteAmigaFile(filename);   //Delete AmigaDOS
  5:Result:=DeleteCFSFile(entry);        //Delete CFS
 end;
end;

{-------------------------------------------------------------------------------
Moves a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.MoveFile(filename, directory: String): Integer;
begin
 Result:=-12;
 //Can only move files on DFS (between drives), ADFS, Amiga and CFS
 if FFormat shr 4=0 then //Move on DFS
 begin
  //Moving and copying are the same, essentially
  Result:=CopyFile(filename,directory);
  //We just need to delete the original once copied
  if Result>-1 then DeleteFile(filename);
 end;
 if FFormat shr 4=1 then Result:=MoveADFSFile(filename,directory);
 if FFormat shr 4=4 then Result:=MoveAmigaFile(filename,directory);
end;
function TDiscImage.MoveFile(source: Cardinal;dest: Integer): Integer;
begin
 Result:=-12;
 //Can only move files on DFS (between drives), ADFS, Amiga and CFS
 if FFormat shr 4=5 then //Move on CFS
  Result:=MoveCFSFile(source,dest);
end;

{-------------------------------------------------------------------------------
Copies a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.CopyFile(filename, directory: String): Integer;
var
 buffer      : TDIByteArray;
 ptr,
 entry,
 dir,
 d,e         : Cardinal;
// R           : Integer;
 newparent   : String;
 file_details: TDirEntry;
begin
 //Need to extract the filename from the full path...and ensure the file exists
 Result:=-1; //Could not load file
 if FileExists(filename,ptr) then
 begin
  Result:=-10;//Can't copy/move to same directory
  //FileExists returns a pointer to the file
  entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=ptr div $10000;  //Top 16 bits - directory reference
  //Make sure that we are not copying onto ourselves
  if Fdisc[dir].Entries[entry].Parent<>directory then
  begin
   Result:=-5;//Unknown error
   //Are we copying a directory?
   if Fdisc[dir].Entries[entry].DirRef=-1 then //No, then continue
   begin
    //First, get the file into memory
    if ExtractFile(filename,buffer) then
    begin
     //Set up the filedetails
     file_details:=FDisc[dir].Entries[entry];
     file_details.Parent:=directory;
     if FFormat shr 4=0 then //DFS
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
    //First, create a new directory at the destination
    Result:=CreateDirectory(FDisc[dir].Entries[entry].Filename,
                            directory,
                            FDisc[dir].Entries[entry].Attributes);
    //if successful, then copy all the files
    if Result>=0 then
    begin
     //Then iterate through each entry and copy them using recursion
     d:=FDisc[dir].Entries[entry].DirRef; //Get the directory reference
     //Work out the new parent path
     newparent:=directory+dir_sep+FDisc[dir].Entries[entry].Filename;
     if Length(FDisc[d].Entries)>0 then
      for e:=0 to Length(FDisc[d].Entries)-1 do
       CopyFile(FDisc[d].Entries[e].Parent+dir_sep+
                FDisc[d].Entries[e].Filename,newparent);
    end;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Set the attributes for a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateAttributes(filename,attributes: String;entry:Cardinal=0):Boolean;
var
 m: Byte;
begin
 Result:=False;
 m:=FFormat DIV $10; //Major format
 case m of
  0:Result:=UpdateDFSFileAttributes(filename,attributes);     //Update DFS attributes
  1:Result:=UpdateADFSFileAttributes(filename,attributes);    //Update ADFS attributes
  2:Result:=UpdateCDRFileAttributes(filename,attributes);     //Update Commodore 64/128 attributes
  3:Result:=UpdateSinclairFileAttributes(filename,attributes);//Update Sinclair/Amstrad attributes
  4:Result:=UpdateAmigaFileAttributes(filename,attributes);   //Update AmigaDOS attributes
  5:Result:=UpdateCFSAttributes(entry,attributes);            //Update CFS attributes
 end;
end;

{-------------------------------------------------------------------------------
Set the disc title
-------------------------------------------------------------------------------}
function TDiscImage.UpdateDiscTitle(title: String;side: Byte): Boolean;
var
 m: Byte;
begin
 Result:=False;
 m:=FFormat DIV $10; //Major format
 case m of
  0:Result:=UpdateDFSDiscTitle(title,side);//Title DFS Disc
  1:Result:=UpdateADFSDiscTitle(title);    //Title ADFS Disc
  2:Result:=UpdateCDRDiscTitle(title);     //Title Commodore 64/128 Disc
  3:Result:=UpdateSinclairDiscTitle(title);//Title Sinclair/Amstrad Disc
  4:Result:=UpdateAmigaDiscTitle(title);   //Title AmigaDOS Disc
  5:Result:=False;                         //Can't retitle CFS
 end;
end;

{-------------------------------------------------------------------------------
Set the boot option
-------------------------------------------------------------------------------}
function TDiscImage.UpdateBootOption(option,side: Byte): Boolean;
var
 m: Byte;
begin
 Result:=False;
 m:=FFormat DIV $10; //Major format
 case m of
  0:Result:=UpdateDFSBootOption(option,side);//Update DFS Boot
  1:Result:=UpdateADFSBootOption(option);    //Update ADFS Boot
  2: exit;//Update Commodore 64/128 Boot ++++++++++++++++++++++++++++++++++++++
  3: exit;//Update Sinclair/Amstrad Boot ++++++++++++++++++++++++++++++++++++++
  4: exit;//Update AmigaDOS Boot ++++++++++++++++++++++++++++++++++++++++++++++
  5: exit;//Can't update CFS boot option
 end;
end;

{$INCLUDE 'DiscImage_ADFS.pas'}
{$INCLUDE 'DiscImage_DFS.pas'}
{$INCLUDE 'DiscImage_C64.pas'}
{$INCLUDE 'DiscImage_Spectrum.pas'}
{$INCLUDE 'DiscImage_Amiga.pas'}
{$INCLUDE 'DiscImage_CFS.pas'}

end.
