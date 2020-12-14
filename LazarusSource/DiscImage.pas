unit DiscImage;

//This project is now covered by the GNU GPL v3 licence

{$MODE objFPC}

interface

uses Classes,ComCtrls;

{$M+}

type
//Define the TDIByteArray - saves using the System.Types unit for TByteDynArray
 TDIByteArray = array of Byte;
//Define the records to hold the catalogue
 TDirEntry     = record     //Not all fields are used on all formats
  Parent,                   //Complete path for parent directory (ALL)
  Filename,                 //Filename (ALL)
  Attributes,               //File attributes (ADFS/DFS/D64/D71/D81/AmigaDOS)
  Filetype,                 //Full name filetype (ADFS/D64/D71/D81)
  ShortFileType: AnsiString;//Filetype shortname (ADFS/D64/D71/D81)
  LoadAddr,                 //Load Address (ADFS/DFS)
  ExecAddr,                 //Execution Address (ADFS/DFS)
  Length,                   //Total length (ALL)
  Side,                     //Side of disc of location of data (DFS)
  Track,                    //Track of location of data (D64/D71/D81)
  DataFile,                 //Reserved for use by Repton Map Display
  ImageAddress: Cardinal;   //Reserved for use by Repton Map Display
  Sector,                   //Sector of disc of location of data (DFS/D64/D71/D81/AmigaDOS file)
                            //Sector of disc of location of header (AmigaDOS directory)
                            //Address of location of data (ADFS S/M/L)
                            //Indirect disc address of data (ADFS D/E/F/E+/F+)
  DirRef      : Integer;    //Reference to directory, if directory (ADFS/AmigaDOS)
  TimeStamp   : TDateTime;  //Timestamp (ADFS D/E/E+/F/F+)
  EOR         : Byte;       //Reserved for use by Repton Map Display
 end;
 TSearchResults =array of TDirEntry;
 TDir          = record
  Directory,                       //Directory name (ALL)
  Title       : AnsiString;        //Directory title (DFS/ADFS)
  Entries     : array of TDirEntry;//Entries (above)
  Broken      : Boolean;           //Flag if directory is broken (ADFS)
  ErrorCode   : Byte;              //Used to indicate error for broken directory (ADFS)
 end;
 TDisc         = array of TDir;
//For retrieving the ADFS E/F fragment information
 TFragment     = record
  Offset,
  Length      : Cardinal;
 end;
 TFragmentArray= array of TFragment;
 procedure ResetDirEntry(var Entry: TDirEntry);
//The class definition
type
 TDiscImage    = Class
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
  emuheader     : Cardinal;     //Allow for any headers added by emulators
  disc_size,                    //Size of disc in bytes
  free_space    : Int64;        //Free space remaining
  FFormat,                      //Format of the image
  secspertrack,                 //Number of sectors per track
  heads,                        //Number of heads (Acorn ADFS New)
  density,                      //Density (Acorn ADFS New)
  idlen,                        //Length of fragment ID in bits (Acorn ADFS New)
  skew,                         //Head skew (Acorn ADFS New)
  bootoption,                   //*OPT4,n
  lowsector,                    //Lowest sector number (Acorn ADFS New)
  disctype,                     //Type of disc
  FDirType,                     //Directory Type (Acorn ADFS)
  share_size,                   //Share size (Acorn ADFS New)
  big_flag      : Byte;         //Big flag (Acorn ADFS New)
  disc_name,
  root_name,                    //Disc title(s)
  imagefilename : AnsiString;   //Filename of the disc image
  dir_sep       : Char;         //Directory Separator
  FProgress     : TStatusBar;   //Progress Update
  procedure ResetVariables;
  function ReadString(ptr,term: Integer): AnsiString; overload;
  function ReadString(ptr,term: Integer;control: Boolean): AnsiString; overload;
  procedure RemoveSpaces(var s: AnsiString);
  procedure RemoveControl(var s: AnsiString);
  function FormatToString: AnsiString;
  function FormatToExt: AnsiString;
  function ReadBits(offset,start,length: Cardinal): Cardinal;
  function IsBitSet(v,b: Integer): Boolean;
  function ConvertTimeDate(filedatetime: Int64): TDateTime;
  function Read32b(offset: Cardinal): Cardinal; overload;
  function Read32b(offset: Cardinal; bigendian: Boolean): Cardinal; overload;
  function Read24b(offset: Cardinal): Cardinal; overload;
  function Read24b(offset: Cardinal; bigendian: Boolean): Cardinal; overload;
  function Read16b(offset: Cardinal): Word; overload;
  function Read16b(offset: Cardinal; bigendian: Boolean): Word; overload;
  function ReadByte(offset: Cardinal): Byte;
  procedure Write32b(value, offset: Cardinal); overload;
  procedure Write32b(value, offset: Cardinal; bigendian: Boolean); overload;
  procedure Write24b(value, offset: Cardinal); overload;
  procedure Write24b(value, offset: Cardinal; bigendian: Boolean); overload;
  procedure Write16b(value: Word; offset: Cardinal); overload;
  procedure Write16b(value: Word; offset: Cardinal; bigendian: Boolean); overload;
  procedure WriteByte(value: Byte; offset: Cardinal);
  function ROR13(v: Cardinal): Cardinal;
  procedure ResetDir(var Entry: TDir);
  function MapFlagToByte: Byte;
  function MapTypeToString: AnsiString;
  function DirTypeToString: AnsiString;
  procedure UpdateProgress(S: AnsiString);
  function GeneralChecksum(offset,length,chkloc,start: Cardinal;carry: Boolean): Cardinal;
  //ADFS Routines
  function ID_ADFS: Boolean;
  function ReadADFSDir(dirname: AnsiString; sector: Cardinal): TDir;
  function DiscAddrToOffset(addr: Cardinal): TFragmentArray;
  function ByteChecksum(offset,size: Cardinal): Byte;
  function ReadADFSDisc: TDisc;
  //DFS Routines
  function ID_DFS: Boolean;
  function ReadDFSDisc: TDisc;
  function ConvertSector(address,side: Integer): Integer;
  function WriteDFSFile(file_details: TDirEntry;m,f: Byte; var buffer: TDIByteArray): Integer;
  //Commodore 1541/1571/1581 Routines
  function ID_CDR: Boolean;
  function ConvertDxxTS(format,track,sector: Integer): Integer;
  function ReadCDRDisc: TDisc;
  //Sinclair Spectrum +3/Amstrad Routines
  function ID_Sinclair: Boolean;
  function ReadSinclairDisc: TDisc;
  //Commodore Amiga Routines
  function ID_Amiga: Boolean;
  function ReadAmigaDisc: TDisc;
  function ReadAmigaDir(dirname: AnsiString; offset: Cardinal): TDir;
  function AmigaBootChecksum(offset: Cardinal): Cardinal;
  function AmigaChecksum(offset: Cardinal): Cardinal;
 published
  //Methods
  constructor Create;
  destructor Destroy; override;
  procedure LoadFromFile(filename: AnsiString);
  procedure LoadFromStream(F: TStream);
  procedure SaveToFile(filename: AnsiString);
  procedure SaveToStream(F: TStream);
  function Format(fs,map,dir: Byte): Boolean;
  function ExtractFile(filename: AnsiString; var buffer: TDIByteArray): Boolean;
  function ExtractFileToStream(filename: AnsiString;F: TStream): Boolean;
  function WriteFile(file_details: TDirEntry; var buffer: TDIByteArray): Integer;
  function WriteFileFromStream(file_details: TDirEntry;F: TStream): Integer;
  function FileExists(filename: AnsiString; var Ref: Cardinal): Boolean;
  function ReadDiscData(addr,count,side: Cardinal; var buffer): Boolean;
  function ReadDiscDataToStream(addr,count,side: Cardinal; F: TStream): Boolean;
  function WriteDiscData(addr,side: Cardinal;var buffer: TDIByteArray; count: Cardinal): Boolean;
  function WriteDiscDataFromStream(addr,side: Cardinal; F: TStream): Boolean;
  function FileSearch(search: TDirEntry): TSearchResults;
  function RenameFile(oldfilename, newfilename: AnsiString): Boolean;
  function DeleteFile(filename: AnsiString): Boolean;
  function MoveFile(filename, directory: AnsiString): Integer;
  function CopyFile(filename, directory: AnsiString): Integer;
  //Properties
  property Disc:                TDisc      read FDisc;
  property FormatString:        AnsiString read FormatToString;
  property FormatNumber:        Byte       read FFormat;
  property FormatExt:           AnsiString read FormatToExt;
  property Title:               AnsiString read disc_name;
  property DiscSize:            Int64      read disc_size;
  property FreeSpace:           Int64      read free_space;
  property DoubleSided:         Boolean    read FDSD;
  property MapType:             Byte       read MapFlagToByte;
  property DirectoryType:       Byte       read FDirType;
  property MapTypeString:       AnsiString read MapTypeToString;
  property DirectoryTypeString: AnsiString read DirTypeToString;
  property ProgressUpdate:      TStatusBar write FProgress;
  property DirSep:              Char       read dir_sep;
  property Filename:            AnsiString read imagefilename;
 End;

implementation

uses
 SysUtils,DateUtils;

{-------------------------------------------------------------------------------
Reset a TDirEntry to blank (not part of the TDiscImage class)
-------------------------------------------------------------------------------}
procedure ResetDirEntry(var Entry: TDirEntry);
begin
 with Entry do
 begin
  Parent       :='';
  Filename     :='';
  Attributes   :='';
  Filetype     :='';
  ShortFiletype:='';
  LoadAddr     :=$0000;
  ExecAddr     :=$0000;
  Length       :=$0000;
  Side         :=$0000;
  Track        :=$0000;
  DataFile     :=$0000;
  ImageAddress :=$0000;
  Sector       :=$0000;
  DirRef       :=$0000;
  TimeStamp    :=0;
  EOR          :=$00;
 end;
end;

//++++++++++++++++++ Class definition starts here ++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Create the instance
-------------------------------------------------------------------------------}
constructor TDiscImage.Create;
begin
 inherited;
 //This just sets all the global and public variables to zero, or blank.
 ResetVariables;
 SetLength(Fdata,0);
 FPRogress:=nil;
end;

{-------------------------------------------------------------------------------
Free the instance
-------------------------------------------------------------------------------}
destructor TDiscImage.Destroy;
begin
 inherited;
end;

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
 bootoption    :=$00;
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
end;

{-------------------------------------------------------------------------------
Load an image from a file (just calls LoadFromStream)
-------------------------------------------------------------------------------}
procedure TDiscImage.LoadFromFile(filename: AnsiString);
var
 FDiscDrive: TFileStream;
begin
 //Only read the file in if it actually exists (or rather, Windows can find it)
 if SysUtils.FileExists(filename) then
 begin
  //Create the stream
  FDiscDrive:=TFileStream.Create(filename,fmOpenRead);
  //Call the procedure to read from the stream
  LoadFromStream(FDiscDrive);
  //Close the stream
  FDiscDrive.Free;
  imagefilename:=filename;
 end;
end;

{-------------------------------------------------------------------------------
Load an image from a stream (e.g. FileStream)
-------------------------------------------------------------------------------}
procedure TDiscImage.LoadFromStream(F: TStream);
begin
 //Blank off the variables
 ResetVariables;
 //Ensure there is enough space in the buffer
 SetLength(Fdata,F.Size);
 //Move to the beginning of the stream
 F.Position:=0;
 UpdateProgress('Loading file');
 //Read the image into the data buffer
 F.Read(Fdata[0],Length(Fdata));
 //This check is done in the ID functions anyway, but we'll do it here also
 if Length(Fdata)>0 then
 begin
  UpdateProgress('IDing the data');
  //ID the type of image, from the data contents
  if ID_DFS      then FDisc:=ReadDFSDisc;     //Acorn DFS
  if ID_ADFS     then FDisc:=ReadADFSDisc;    //Acorn ADFS
  if ID_CDR      then FDisc:=ReadCDRDisc;     //Commodore
  if ID_Sinclair then FDisc:=ReadSinclairDisc;//Sinclair/Amstrad
  if ID_Amiga    then FDisc:=ReadAmigaDisc;   //Amiga
  if FFormat=$FF then ResetVariables;
 end;
 UpdateProgress('');
end;

{-------------------------------------------------------------------------------
Saves an image to a file
-------------------------------------------------------------------------------}
procedure TDiscImage.SaveToFile(filename: AnsiString);
var
 FDiscDrive: TFileStream;
begin
 //Create the stream
 FDiscDrive:=TFileStream.Create(filename,fmCreate);
 //Call the procedure to read from the stream
 SaveToStream(FDiscDrive);
 //Close the stream
 FDiscDrive.Free;
end;

{-------------------------------------------------------------------------------
Saves an image to a stream
-------------------------------------------------------------------------------}
procedure TDiscImage.SaveToStream(F: TStream);
begin
 //Move to the beginning of the stream
 F.Position:=0;
 //Read the image into the data buffer
 F.Write(Fdata[0],Length(Fdata));
end;

{-------------------------------------------------------------------------------
Create and format a new disc image
-------------------------------------------------------------------------------}
function TDiscImage.Format(fs: Byte; map: Byte; dir: Byte): Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Extract a string from ptr to the next chr(term) or length(-term)
-------------------------------------------------------------------------------}
function TDiscImage.ReadString(ptr,term: Integer): AnsiString;
begin
 Result:=ReadString(ptr,term,True);
end;
function TDiscImage.ReadString(ptr,term: Integer;control: Boolean): AnsiString;
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
Removes trailing spaces from a string
-------------------------------------------------------------------------------}
procedure TDiscImage.RemoveSpaces(var s: AnsiString);
var
 x: Integer;
begin
 //Start at the end
 x:=Length(s);
 if x>0 then
 begin
  while (s[x]=' ') and (x>0) do //Continue while the last character is a space
   dec(x);       //Move down the string
  s:=Copy(s,0,x);//Finally, remove the spaces
 end;
end;

{-------------------------------------------------------------------------------
Removes control characters from a string
-------------------------------------------------------------------------------}
procedure TDiscImage.RemoveControl(var s: AnsiString);
var
 x: Integer;
 o: AnsiString;
begin
 //New String
 o:='';
 //Iterate through the old string
 for x:=1 to Length(s) do
  //Only add the character to the new string if it is not a control character
  if ord(s[x])>31 then o:=o+s[x];
 //Change the old string to the new string
 s:=o;
end;

{-------------------------------------------------------------------------------
Extracts a file, filename contains complete path, directory separator is '.'
-------------------------------------------------------------------------------}
function TDiscImage.ExtractFile(filename: AnsiString; var buffer: TDIByteArray): Boolean;
var
 source        : Integer;
 entry,dir,
 frag,dest,
 fragptr,len,
 filelen       : Cardinal;
 fragments     : TFragmentArray;
begin
 //Does the file actually exist?
 Result:=FileExists(filename,fragptr);
 //Yes, so load it - there is nothing to stop a directory header being extracted
 //if passed in the filename parameter.
 if Result then
 begin
  //FileExists returns a pointer to the file
  entry:=fragptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=fragptr div $10000;  //Top 16 bits - directory reference
  //Make space to receive the file
  filelen:=FDisc[dir].Entries[entry].Length;
  SetLength(buffer,filelen);
  //Default values
  fragptr:=$0000;
  frag   :=0;
  //Get the starting position
  case FFormat shr 4 of
   //Acorn DFS
   $0: fragptr:=FDisc[dir].Entries[entry].Sector*$100; //Side is accounted for later
   //Acorn ADFS
   $1:
   begin
    if not FMap then //Old Map
     fragptr:=FDisc[dir].Entries[entry].Sector*$100;
    if FMap then //New Map
     //Get the fragment offsets of the file
     fragments:=DiscAddrToOffset(FDisc[dir].Entries[entry].Sector);
   end;
   //Commodore D61/D71/D81
   $2: fragptr:=ConvertDxxTS(FFormat AND $F,
                             FDisc[dir].Entries[entry].Track,
                             FDisc[dir].Entries[entry].Sector);
   //Commodore Amiga
   $4: fragptr:=Cardinal(FDisc[dir].Entries[entry].Sector);
  end;
  dest  :=0;      //Length pointer/Destination pointer
  len   :=filelen;//Amount of data to read in
  source:=fragptr;//Source pointer
  repeat
   //Fragmented filing systems, so need to work out source and length
   case FFormat shr 4 of
    $1:                           //Acorn ADFS New Map
    if FMap then
     if frag<Length(fragments) then
     begin
      source:=fragments[frag].Offset;           //Source of data
      len   :=fragments[frag].Length;           //Amount of data
     end;
    $2:                           //Commodore D64/D71/D81
    begin
     source:=fragptr+2;                        //Source of data
     len   :=254;                              //Amount of data
    end;
    $4:                           //Commodore Amiga
    begin
     source:=Integer(fragptr*secsize)+$18;     //Source of data
     len   :=Read32b(fragptr*secsize+$C,True);//Amount of data
    end;
   end;
   //Make sure we don't read too much
   if dest+len>filelen then
    len:=filelen-dest;
   //Read the data into the buffer
   ReadDiscData(source,len,FDisc[dir].Entries[entry].Side,buffer[dest]);
   //Move the size pointer on, by the amount read
   inc(dest,len);
   //Get the next block pointer
   case FFormat shr 4 of
    //Acorn ADFS - move onto next fragment
    $1: inc(frag);
    //Commodore d64/D71/D81 - find next block
    $2: fragptr:=ConvertDxxTS(FFormat AND $F,
                              ReadByte(fragptr),
                              ReadByte(fragptr+1));
    //Commodore Amiga - find next block
    $4: fragptr:=Read32b(fragptr*secsize+$10,True);
   end;
  until dest>=filelen; //Once we've reached the file length, we're done
 end;
end;

{-------------------------------------------------------------------------------
Extract a file into a memory stream
-------------------------------------------------------------------------------}
function TDiscImage.ExtractFileToStream(filename: AnsiString; F: TStream): Boolean;
var
 buffer: TDIByteArray;
begin
 //This just uses the previous function to get the file
 Result:=ExtractFile(filename,buffer);
 if Result then
 begin
  //Before we save it to the supplied stream
  F.Position:=0;
  F.Write(buffer[0],Length(buffer));
  F.Position:=0;
 end;
end;

{-------------------------------------------------------------------------------
Save a file into the disc image, from buffer
-------------------------------------------------------------------------------}
function TDiscImage.WriteFile(file_details: TDirEntry; var buffer: TDIByteArray): Integer;
var
 m,f   : Byte;
 count : Integer;
begin
 //Start with a false result
 Result:=-1;
 //Get the length of data to be written
 count:=file_details.Length;
 //There are only two sides (max)
 file_details.Side:=file_details.Side mod 2;
 //Only write a file if there is actually any data to be written
 if count>0 then
 begin
  //Can only write a file that will fit on the disc
  if count<=free_space then
  begin
   m:=FFormat DIV $10; //Major format
   f:=FFormat MOD $10; //Minor format (sub format)
   case m of
    0:      //Write DFS ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Result:=WriteDFSFile(file_details,m,f,buffer);
    1: exit;//Write ADFS +++++++++++++++++++++++++++++++++++++++++++++++++++++++
    2: exit;//Write Commodore 64/128 +++++++++++++++++++++++++++++++++++++++++++
    3: exit;//Write Sinclair/Amstrad +++++++++++++++++++++++++++++++++++++++++++
    4: exit;//Write AmigaDOS +++++++++++++++++++++++++++++++++++++++++++++++++++
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Save a file into the disc image, from stream
-------------------------------------------------------------------------------}
function TDiscImage.WriteFileFromStream(file_details: TDirEntry;F: TStream): Integer;
var
 buffer: TDIByteArray;
begin
 //Copy the stream into a buffer
 SetLength(buffer,F.Size);
 F.Position:=0;
 F.Read(buffer[0],F.Size);
 //Then call the preceeding function to do the work
 Result:=WriteFile(file_details,buffer);
 //Return the pointer to the beginning of the stream
 F.Position:=0;
end;

{-------------------------------------------------------------------------------
Rename a file - oldfilename is full path, newfilename has no path
-------------------------------------------------------------------------------}
function TDiscImage.RenameFile(oldfilename, newfilename: AnsiString): Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Deletes a file (given full pathname)
-------------------------------------------------------------------------------}
function TDiscImage.DeleteFile(filename: AnsiString): Boolean;
begin
 Result:=False;
end;

{-------------------------------------------------------------------------------
Moves a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.MoveFile(filename, directory: AnsiString): Integer;
begin
 //Moving and copying are the same, essentially
 Result:=CopyFile(filename,directory);
 //We just need to delete the original once copied
 if Result<>-1 then DeleteFile(filename);
end;

{-------------------------------------------------------------------------------
Copies a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.CopyFile(filename, directory: AnsiString): Integer;
var
 buffer      : TDIByteArray;
 ptr,
 entry,
 dir         : Cardinal;
 file_details: TDirEntry;
begin
 //Need to extract the filename from the full path...and ensure the file exists
 Result:=-1;
 if FileExists(filename,ptr) then
 begin
  //FileExists returns a pointer to the file
  entry:=ptr mod $10000;  //Bottom 16 bits - entry reference
  dir  :=ptr div $10000;  //Top 16 bits - directory reference
  //Make sure that we are not copying onto ourselves
  if Fdisc[dir].Entries[entry].Parent<>directory then
  begin
   //First, get the file into memory
   if ExtractFile(filename,buffer) then
   begin
    //Set up the filedetails
    file_details:=FDisc[dir].Entries[entry];
    file_details.Parent:=directory;
    //Then write it back to the image
    Result:=WriteFile(file_details,buffer);
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Convert a format byte to a string
-------------------------------------------------------------------------------}
function TDiscImage.FormatToString: AnsiString;
const
 FS  : array[0..4] of AnsiString = ('DFS',
                                'Acorn ADFS',
                                'Commodore',
                                'Sinclair Spectrum +3/Amstrad',
                                'Commodore Amiga');
 SUB : array[0..4] of array[0..15] of AnsiString =
 (('Acorn SSD','Acorn DSD','Watford SSD','Watford DSD','','','','','','','','','','','',''),
  ('S','M','L','D','E','E+','F','F+','','','','','','','','Hard Disc'),
  ('1541','1571','1581','','','','','','','','','','','','',''),
  ('','Extended','','','','','','','','','','','','','',''),
  ('DD','HD','','','','','','','','','','','','','','Hard Disc'));
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
function TDiscImage.FormatToExt: AnsiString;
const
 EXT : array[0..4] of array[0..15] of AnsiString =
 (('ssd','dsd','ssd','dsd','','','','','','','','','','','',''),
  ('adf','adf','adf','adf','adf','adf','adf','adf','','','','','','','','hdf'),
  ('d64','d71','d81','','','','','','','','','','','','',''),
  ('','dsk','','','','','','','','','','','','','',''),
  ('adf','adf','','','','','','','','','','','','','','hdf'));
begin
 if FFormat<>$FF then
 begin
  Result:=EXT[FFormat DIV $10,FFormat MOD $10];
 end
 else Result:='unk';
end;

{-------------------------------------------------------------------------------
Does a file exist?
-------------------------------------------------------------------------------}
function TDiscImage.FileExists(filename: AnsiString; var Ref: Cardinal): Boolean;
var
 Path   : array of AnsiString;
 i,j,l,
 ptr,
 level  : Integer;
 test,
 test2  : AnsiString;
begin
 Result:=False;
 //Not going to search if there is no tree to search in
 if Length(FDisc)>0 then
 begin
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
   test:=UpperCase(Path[level]);
   test2:=UpperCase(FDisc[i].Directory);
   if (FFormat=$01) and (test<>test2) then
    inc(i);
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
       test2:=UpperCase(Path[level])
      else
       test2:='not found';
      l:=j;
      //Just to make sure we don't search past the end of the arrays
      if (i>=0) and (i<Length(FDisc)) then
      begin
       if j<Length(FDisc[i].Entries) then
        test:=UpperCase(FDisc[i].Entries[j].Filename);
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
        test2:=UpperCase(FDisc[i].Entries[j].Filename);
        i:=FDisc[i].Entries[j].DirRef;
        //Unless we are looking for a directory
        if level=Length(Path)-1 then
          if UpperCase(Path[level])=test2 then
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
Searches for a file, and returns the result in a TSearchResults
-------------------------------------------------------------------------------}
function TDiscImage.FileSearch(search: TDirEntry): TSearchResults;
//Comparison functions...saves a lot of if...then statements
 function CompStr(S1,S2: AnsiString): Byte; //Compares Strings
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
Read upto 32 bits of data from the buffer, starting at offset(bytes)+start(bits)
-------------------------------------------------------------------------------}
function TDiscImage.ReadBits(offset,start,length: Cardinal): Cardinal;
var
 start_byte,
 start_bit,
 bit,b,prev : Cardinal;
 lastbyte   : Byte;
begin
 //Reset the result
 Result:=0;
 //If the length is 0, nothing to read. Cardinals are 32 bits
 //(we could use Integers, but these are signed)
 if (length>0) and (length<33) then
 begin
  prev:=$FFFFFFFF;
  lastbyte:=0;
  //Iterate through the required number of bits
  for bit:=0 to length-1 do
  begin
   //Work out the byte offset, and the bit within
   start_byte:=(start+bit) div 8;
   start_bit :=(start+bit) mod 8;
   //And increase the result with the extracted bit, shifted right to account
   //for final position
   if prev<>offset+start_byte then
   begin
    //To save re-reading the same byte over and over
    prev:=offset+start_byte;
    lastbyte:=ReadByte(prev);
   end;
   b:=(lastbyte AND (1 shl start_bit))shr start_bit;
   inc(Result,b shl bit);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Check to see if bit b is set in word v
-------------------------------------------------------------------------------}
function TDiscImage.IsBitSet(v,b: Integer): Boolean;
var
 x: Integer;
begin
 Result:=False;
 if (b>=0) and (b<32) then
 begin
  x:=1 shl b;
  Result:=((v AND x)=x);
 end;
end;

{-------------------------------------------------------------------------------
Converts a RISC OS Time/Date to a Delphi TDateTime
-------------------------------------------------------------------------------}
function TDiscImage.ConvertTimeDate(filedatetime: Int64): TDateTime;
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
Read in 4 bytes (word)
-------------------------------------------------------------------------------}
function TDiscImage.Read32b(offset: Cardinal): Cardinal;
begin
 Result:=Read32b(offset,False);
end;
function TDiscImage.Read32b(offset: Cardinal; bigendian: Boolean): Cardinal;
begin
 Result:=$FFFFFFFF; //Default value
 if offset<Cardinal(Length(Fdata)) then
  //Big Endian
  if bigendian then
   Result:=Fdata[offset+3]
          +Fdata[offset+2]*$100
          +Fdata[offset+1]*$10000
          +Fdata[offset+0]*$1000000
  else
   //Little Endian
   Result:=Fdata[offset+0]
          +Fdata[offset+1]*$100
          +Fdata[offset+2]*$10000
          +Fdata[offset+3]*$1000000;
end;

{-------------------------------------------------------------------------------
Read in 3 bytes
-------------------------------------------------------------------------------}
function TDiscImage.Read24b(offset: Cardinal): Cardinal;
begin
 Result:=Read24b(offset,False);
end;
function TDiscImage.Read24b(offset: Cardinal; bigendian: Boolean): Cardinal;
begin
 Result:=$FFFFFF; //Default value
 if offset<Cardinal(Length(Fdata)) then
  //Big Endian
  if bigendian then
   Result:=Fdata[offset+2]
          +Fdata[offset+1]*$100
          +Fdata[offset+0]*$10000
  else
   //Little Endian
   Result:=Fdata[offset+0]
          +Fdata[offset+1]*$100
          +Fdata[offset+2]*$10000;
end;

{-------------------------------------------------------------------------------
Read in 2 bytes
-------------------------------------------------------------------------------}
function TDiscImage.Read16b(offset: Cardinal): Word;
begin
 Result:=Read16b(offset,False);
end;
function TDiscImage.Read16b(offset: Cardinal; bigendian: Boolean): Word;
begin
 Result:=$FFFF; //Default value
 if offset<Cardinal(Length(Fdata)) then
  //Big Endian
  if bigendian then
   Result:=Fdata[offset+1]
          +Fdata[offset+0]*$100
  else
   //Little Endian
   Result:=Fdata[offset+0]
          +Fdata[offset+1]*$100;
end;

{-------------------------------------------------------------------------------
Read in a byte
-------------------------------------------------------------------------------}
function TDiscImage.ReadByte(offset: Cardinal): Byte;
begin
 Result:=$FF; //Default value
 if offset<Cardinal(Length(Fdata)) then
  Result:=Fdata[offset];
end;

{-------------------------------------------------------------------------------
Write 4 bytes (word)
-------------------------------------------------------------------------------}
procedure TDiscImage.Write32b(value, offset: Cardinal);
begin
 Write32b(value,offset,False);
end;
procedure TDiscImage.Write32b(value, offset: Cardinal; bigendian: Boolean);
begin
 if offset<Cardinal(Length(Fdata)) then
  if bigendian then
  begin
   //Big Endian
   Fdata[offset+3]:= value mod $100;
   Fdata[offset+2]:=(value div $100)    mod $100;
   Fdata[offset+1]:=(value div $10000)  mod $100;
   Fdata[offset+0]:=(value div $1000000)mod $100;
  end
  else
  begin
   //Little Endian
   Fdata[offset+0]:= value mod $100;
   Fdata[offset+1]:=(value div $100)    mod $100;
   Fdata[offset+2]:=(value div $10000)  mod $100;
   Fdata[offset+3]:=(value div $1000000)mod $100;
  end;
end;

{-------------------------------------------------------------------------------
Write 3 bytes
-------------------------------------------------------------------------------}
procedure TDiscImage.Write24b(value, offset: Cardinal);
begin
 Write24b(value,offset,False);
end;
procedure TDiscImage.Write24b(value,offset: Cardinal; bigendian: Boolean);
begin
 if offset<Cardinal(Length(Fdata)) then
  if bigendian then
  begin
   //Big Endian
   Fdata[offset+2]:= value mod $100;
   Fdata[offset+1]:=(value div $100)    mod $100;
   Fdata[offset+0]:=(value div $10000)  mod $100;
  end
  else
  begin
   //Little Endian
   Fdata[offset+0]:= value mod $100;
   Fdata[offset+1]:=(value div $100)    mod $100;
   Fdata[offset+2]:=(value div $10000)  mod $100;
  end;
end;

{-------------------------------------------------------------------------------
Write 2 bytes
-------------------------------------------------------------------------------}
procedure TDiscImage.Write16b(value: Word; offset: Cardinal);
begin
 Write16b(value,offset,False);
end;
procedure TDiscImage.Write16b(value: Word; offset: Cardinal; bigendian: Boolean);
begin
 if offset<Cardinal(Length(Fdata)) then
  if bigendian then
  begin
   //Big Endian
   Fdata[offset+1]:= value mod $100;
   Fdata[offset+0]:=(value div $100)    mod $100;
  end
  else
  begin
   //Little Endian
   Fdata[offset+0]:= value mod $100;
   Fdata[offset+1]:=(value div $100)    mod $100;
  end
end;

{-------------------------------------------------------------------------------
Write byte
-------------------------------------------------------------------------------}
procedure TDiscImage.WriteByte(value: Byte; offset: Cardinal);
begin
 if offset<Cardinal(Length(Fdata)) then
  Fdata[offset]:=value;
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
function TDiscImage.MapTypeToString: AnsiString;
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
function TDiscImage.DirTypeToString: AnsiString;
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
Direct access to disc data
-------------------------------------------------------------------------------}
function TDiscImage.ReadDiscData(addr,count,side: Cardinal; var buffer): Boolean;
var
 i   : Cardinal;
 temp: TDIByteArray;
begin
 //If DFS we need to account for sides
 if FFormat DIV $10>0 then
 begin
  //Entire block must be within the length of the data
  Result:=(addr+count)<Cardinal(Length(Fdata));
  if Result then
   //Simply copy from source to destination
   Move(Fdata[addr],buffer,count);
 end
 else
 begin
  //Entire block must be within the length of the data
  Result:=ConvertSector(addr+count,side)<Length(Fdata);
  //Simply copy from source to destination
  if Result then
  begin
   SetLength(temp,count);
   for i:=0 to count-1 do
    temp[i]:=ReadByte(ConvertSector(addr+i,side));
   Move(temp[0],buffer,count);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Direct access to disc data, into a stream
-------------------------------------------------------------------------------}
function TDiscImage.ReadDiscDataToStream(addr,count,side: Cardinal;F: TStream): Boolean;
var
 buffer: TDIByteArray;
begin
 //Set the length of the buffer
 SetLength(buffer,count);
 //Use the previous function to get the data
 Result:=ReadDiscData(addr,count,side,buffer[0]);
 if Result then
 begin
  //Then save it to the supplied stream
  F.Position:=0;
  F.Write(buffer[0],Length(buffer));
  F.Position:=0;
 end;
end;

{-------------------------------------------------------------------------------
Direct access writing to disc
-------------------------------------------------------------------------------}
function TDiscImage.WriteDiscData(addr,side: Cardinal;var buffer: TDIByteArray; count: Cardinal): Boolean;
var
 i   : Cardinal;
begin
 if FFormat DIV $10>0 then //not DFS
 begin
  //Ensure that the entire block will fit into the available space
  Result:=(addr+count)<=Cardinal(Length(Fdata));
  //Simply copy from source to destination
  if Result then
   for i:=0 to count-1 do
    WriteByte(buffer[i],addr+i);
 end
 else //DFS
 begin
  //Ensure that the entire block will fit into the available space
  Result:=ConvertSector(addr+count,side)<=Length(Fdata);
  //Simply copy from source to destination
  if Result then
   for i:=0 to count-1 do
    WriteByte(buffer[i],ConvertSector(addr+i,side));
 end;
end;

{-------------------------------------------------------------------------------
Direct access writing to disc from stream
-------------------------------------------------------------------------------}
function TDiscImage.WriteDiscDataFromStream(addr,side: Cardinal; F: TStream): Boolean;
var
 buffer: TDIByteArray;
begin
 //Set the length of the buffer
 SetLength(buffer,F.Size);
 //Copy the stream into the buffer
 F.Position:=0;
 F.Write(buffer[0],F.Size);
 //Use the previous function to write the data
 Result:=WriteDiscData(addr,side,buffer,Length(buffer));
 F.Position:=0;
end;

{-------------------------------------------------------------------------------
Update the progress label
-------------------------------------------------------------------------------}
procedure TDiscImage.UpdateProgress(S: AnsiString);
begin
 if FProgress<>nil then
  if FProgress.Panels.Count>0 then
   begin
    FProgress.Panels[0].Text:=S;
    FProgress.Update;
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

//++++++++++++++++++ Acorn ADFS ++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies an ADFS disc and which type
-------------------------------------------------------------------------------}
function TDiscImage.ID_ADFS: Boolean;
var
 Check0,
 Check1,
 Check0a,
 Check1a  : Byte;
 i,ctr,
 dr_size,
 dr_ptr,
 zone     : Cardinal;
begin
 Result:=False;
 if FFormat=$FF then
 begin
  UpdateProgress('Checking for ADFS');
  //Is there actually any data?
  if Length(Fdata)>0 then
  begin
   //Check for Old Map
   Check0   :=ReadByte(emuheader+$0FF);
   Check1   :=ReadByte(emuheader+$1FF);
   Check0a  :=ByteCheckSum(emuheader+$0000,$100);
   Check1a  :=ByteCheckSum(emuheader+$0100,$100);
   //Do the checksums on both sectors
   if  (Check0a=Check0)
   and (Check1a=Check1) then
   begin
    //Checks are successful, now find out which type of disc: S/M/L/D
    Result:=True;
    FFormat:=$10; //Default to ADFS S
    FMap:=False;  //Set to old map
    FDirType:=0;  //Set to old directory
    ctr:=0;
    //Check the disc title area for all zeros
    for i:=$F7 to $FB do
     inc(ctr,ReadByte(emuheader+i));
    for i:=$1F6 to $1FA do
     inc(ctr,ReadByte(emuheader+i));
    //If there are all zeros, then it is an old directory
    if ctr<>0 then
    begin
     //otherwise is a new directory
     FDirType:=1;
     FFormat:=$13; // ADFS D
    end;
    disc_size:=Read24b(emuheader+$0FC)*$100;
    //The above checks will pass through if the first 512 bytes are all zeros,
    //meaning a, e.g., Commodore 64 image will be IDed as an ADFS Old Map.
    //So, we need to check the disc size is not zero also.
    if disc_size=0 then
    begin
     Result:=False;
     ResetVariables;
    end
    else
    begin
     //Not a reliable way of determining disc shape. However, there are three
     //different sizes of the same format.
     FFormat:=$1F; //Default to hard drive
     case disc_size of
      163840: FFormat:=$10; // ADFS S
      327680: FFormat:=$11; // ADFS M
      655360: FFormat:=$12; // ADFS L
     end;
    end;
   end;
   if not Result then
   begin
    FMap:=True;         //Assume New Map for now
    ctr:=0;
    dr_ptr:=$0000;
    UpdateProgress('Checking for New Map ADFS...locating the disc record');
    repeat
     if ctr=0 then dr_ptr:=$0004;   //Point the disc record to $0004
     if ctr=1 then dr_ptr:=$0DC0;   //Point the disc record to $0DC0
     if ctr=2 then emuheader:=$0200;//Might have a header, added by an emulator
     //Then find the map
     dr_size   :=60; //Disc record size
     //Read some values from the disc record in the boot block
     //These are the minimum we require to find the map
     if emuheader+dr_ptr+$40<Cardinal(Length(FData)) then
     begin
      secsize   :=1 shl ReadByte(emuheader+dr_ptr+$00); //Sector size
      bpmb      :=1 shl ReadByte(emuheader+dr_ptr+$05); //Bits per map bit
      nzones    :=ReadByte(emuheader+dr_ptr+$09)
                 +ReadByte(emuheader+dr_ptr+$2A)*$100;  //nzones is 2 bytes, for E+ and F+
      zone_spare:=Read16b(emuheader+dr_ptr+$0A);        //Zone spare bits
     end;
     //If there are more than 2 zones, we need the disc record size in bits
     if nzones>2 then
      zone:=dr_size*8
     else
      zone:=0;
     //Calculate the start of the map
     bootmap:=((nzones div 2)*(8*secsize-zone_spare)-zone)*bpmb;
     //If the bootmap is within the size of the disc, and there is at least
     //a single zone then continue
     if (emuheader+bootmap+nzones*secsize<Cardinal(Length(Fdata))) and (nzones>0) then
     begin
      Result:=True;
      //Check the checksums for each zone
      Check1:=$00;
      for zone:=0 to nzones-1 do
      begin
       //ZoneCheck checksum
       Check0:=ReadByte(emuheader+bootmap+zone*secsize+$00);
       //CrossCheck checksum
       Check1:=Check1 XOR ReadByte(emuheader+bootmap+zone*secsize+$03);
       //Check failed, reset format
       if Check0<>GeneralChecksum(emuheader+bootmap+zone*secsize,
                                  secsize,secsize+4,$4,true) then
        Result:=False;
      end;
      //Cross zone check - should be $FF
      if Check1<>$FF then
       Result:=False;
     end;
     //Check the bootblock checksum
     if (ctr>0) and (Result) then
     begin
      Check0:=ReadByte(emuheader+$0C00+$1FF);
      if ByteChecksum(emuheader+$0C00,$200)<>Check0 then Result:=False;
     end;
     inc(ctr);
    until (Result) or (ctr=3);
    if Result then
     case ctr of
      1: FFormat:=$14; //ADFS E/E+
      2: FFormat:=$16; //ADFS F/F+
      3: FFormat:=$1F; //ADFS Hard Drive
     end;
    //Check for type of directory, and change the format if necessary
    if Result then
    begin
     FDirType:=1; //New Directory
     //Determine if it is a '+' format by reading the version flag
     if ReadByte(emuheader+dr_ptr+$2C)>0 then
     begin
      if FFormat<>$1F then inc(FFormat);
      FDirType:=2;
     end;
    end;
   end;
  end;
  if not Result then
   ResetVariables;
 end;
end;

{-------------------------------------------------------------------------------
Read ADFS Directory
-------------------------------------------------------------------------------}
function TDiscImage.ReadADFSDir(dirname: AnsiString; sector: Cardinal): TDir;
var
 Entry              : TDirEntry;
 temp,
 StartName,EndName,
 dirtitle,pathname  : AnsiString;
 ptr,
 dircheck,numentrys,
 dirsize,namesize,
 entrys,nameheap,
 tail,NameLen,
 entrysize,offset,
 NameOff,amt,
 EndOfChk           : Cardinal;
 addr               : TFragmentArray;
 StartSeq,EndSeq,
 dirchk,NewDirAtts  : Byte;
 validdir,validentry,
 endofentry         : Boolean;
const
 //Attributes
 OldAtts: array[0..9] of Char = ('R','W','L','D','E','r','w','e','P',' ');
 NewAtts: array[0..7] of Char = ('R','W','L','D','r','w',' ',' ');
 //RISC OS Filetypes (as at RISC OS 5.23)
 FileTypes: array[1..79] of AnsiString = (
 '004AIM'     ,'0E1Index'   ,'132ICO'     ,'190DSK'     ,'191PCWDisc' ,
 '194D20Disc' ,'195D2Disc'  ,'196D10Disc' ,'19BMyZ80'   ,'1A6AcornCPM',
 '5F4SparkScr','68EPackdDir','690Clear'   ,'691Degas'   ,'692IMG'     ,
 '693IFF'     ,'694MacPaint','695GIF'     ,'696Pineappl','697PCX'     ,
 '698QRT'     ,'699MTV'     ,'69ACadSoft' ,'69BIrlam'   ,'69CBMP'     ,
 '69EPBMPlus' ,'A91Zip'     ,'ABACPIO'    ,'ABFCabinet' ,'ADFPDF'     ,
 'AE9Alarms'  ,'AF1Music'   ,'AFFDrawFile','B60PNG'     ,'BBCBBC ROM' ,
 'BD9DiscP'   ,'BDADisc'    ,'BE8PhotoCD' ,'C46Tar'     ,'C85JPEG'    ,
 'DDCArchive' ,'DEADXF'     ,'F95Code'    ,'F9DDiscCD'  ,'F9EDiscDP'  ,
 'F9FDiscD'   ,'FAEResource','FB4DiscR'   ,'FB5NoDisc'  ,'FC3Patch'   ,
 'FC6PrntDefn','FC8DOSDisc' ,'FCASquash'  ,'FCCDevice'  ,'FCEFloppy'  ,
 'FCFCache'   ,'FD6TaskExec','FD7TaskObey','FDCSoftLink','FE4DOS'     ,
 'FE6UNIX Ex' ,'FEADesktop' ,'FEBObey'    ,'FECTemplate','FEDPalette' ,
 'FF0TIFF'    ,'FF2Config'  ,'FF4Printout','FF5PoScript','FF6Font'    ,
 'FF7BBC font','FF8Absolute','FF9Sprite'  ,'FFAModule'  ,'FFBBASIC'   ,
 'FFCUtility' ,'FFDData'    ,'FFECommand' ,'FFFText'                   );
begin
 RemoveControl(dirname);
 UpdateProgress('Reading ADFS directory "'+dirname+'"');
 //Store complete path
 pathname:=dirname;
 //Store directory name
 if Pos(dir_sep,dirname)>0 then
 begin
  temp:=dirname;
  repeat
   temp:=Copy(temp,Pos(dir_sep,temp)+1,Length(temp))
  until Pos(dir_sep,temp)=0;
  Result.Directory:=temp;
 end
 else
  Result.Directory:=dirname;
 //Initialise some of the variables
 StartSeq        :=$00;
 EndSeq          :=$FF;
 numentrys       :=0;
 tail            :=$00;
 dirsize         :=$00;
 nameheap        :=$00;
 entrys          :=0;
 entrysize       :=$00;
 NewDirAtts      :=$00;
 dirchk          :=0;
 namesize        :=$00;
 SetLength(addr,0);
 //Get the offset address
 if FMap then
 begin
  //New Map, so the sector will be an internal disc address
  addr:=DiscAddrToOffset(sector);
  //But we need it as an offset into the data
  if Length(addr)>0 then
   sector:=addr[0].Offset;
 end
 else sector:=sector*$100; //Is Old Map, so offset is just the sector * $100
 //Read in the directory header
 case FDirType of
  0,1: //Old and New Directory
  begin
   StartSeq :=ReadByte(emuheader+sector);        //Start Sequence Number to match with end
   StartName:=ReadString(emuheader+sector+1,-4); //Hugo or Nick
   if FDirType=0 then //Old Directory
   begin
    numentrys:=47;                     //Number of entries per directory
    dirsize  :=1280;                   //Directory size in bytes
    tail     :=$35;                    //Size of directory tail
   end;
   if FDirType=1 then //New Directory
   begin
    numentrys:=77;                     //Number of entries per directory
    dirsize  :=2048;                   //Directory size in bytes
    tail     :=$29;                    //Size of directory tail
   end;
   entrys   :=$05;                     //Pointer to entries, from sector
   entrysize:=$1A;                     //Size of each entry
  end;
  2:   //Big Directory
  begin
   StartSeq :=ReadByte(emuheader+sector);         //Start sequence number to match with end
   StartName:=ReadString(emuheader+sector+$04,-4);//Should be SBPr
   NameLen  :=Read32b(emuheader+sector+$08);     //Length of directory name
   dirsize  :=Read32b(emuheader+sector+$0C);     //Directory size in bytes
   numentrys:=Read32b(emuheader+sector+$10);     //Number of entries in this directory
   namesize :=Read32b(emuheader+sector+$14);     //Size of the name heap in bytes
   dirname  :=ReadString(emuheader+sector+$1C,-NameLen);//Directory name
   entrys   :=(($1C+NameLen+1+3)div 4)*4;         //Pointer to entries, from sector
   tail     :=$08;                                //Size of directory tail
   entrysize:=$1C;                                //Size of each entry
   nameheap :=entrys+numentrys*entrysize;         //Offset of name heap
  end;
 end;
 //Now we know the size of the directory, we can read in the tail
 tail:=dirsize-tail;
 //Not all of the tail is read in
 case FDirType of
  0:
  begin
   dirtitle:=ReadString(emuheader+sector+tail+$0E,-19);//Title of the directory
   EndSeq  :=ReadByte(emuheader+sector+tail+$2F);      //End sequence number to match with start
   EndName :=ReadString(emuheader+sector+tail+$30,-4); //Hugo or Nick
   dirchk  :=ReadByte(emuheader+sector+tail+$34);      //Directory Check Byte
  end;
  1:
  begin
   dirtitle:=ReadString(emuheader+sector+tail+$06,-19);//Title of the directory
   EndSeq  :=ReadByte(emuheader+sector+tail+$23);      //End sequence number to match with start
   EndName :=ReadString(emuheader+sector+tail+$24,-4); //Hugo or Nick
   dirchk  :=ReadByte(emuheader+sector+tail+$28);      //Directory Check Byte
  end;
  2:
  begin
   EndName :=ReadString(emuheader+sector+tail+$00,-4); //Should be oven
   EndSeq  :=ReadByte(emuheader+sector+tail+$04);      //End sequence number to match with start
   dirtitle:=dirname;                                  //Does not have a directory title
   dirchk  :=ReadByte(emuheader+sector+tail+$07);      //Directory Check Byte
  end;
 end;
 //Check for broken directory
 //This can result in having a valid directory structure, but a broken directory
 //ADFS normally refuses to list broken directories, but we will list them anyway,
 //just marking the directory as broken and return an error code
 Result.ErrorCode:=0;
 if (EndSeq<>StartSeq) then
  Result.ErrorCode:=Result.ErrorCode OR $01;
 if ((FDirType<2) and (StartName<>EndName)) then
  Result.ErrorCode:=Result.ErrorCode OR $02;
 if ((FDirType=2) and ((StartName<>'SBPr') or (EndName<>'oven'))) then
  Result.ErrorCode:=Result.ErrorCode OR $04;
 Result.Broken:=Result.ErrorCode<>$00;
 //Check for valid directory
 //We won't try and get the directory structure if it appears that it is invalid
 //Could just be that one of the names has got corrupt, but could be much worse
 validdir:=False;
 if ((FDirType<2) and (StartName=EndName) and ((StartName='Hugo') or (StartName='Nick')))
 or ((FDirType=2) and (StartName='SBPr') and (EndName='oven')) then
  validdir:=True;
 //Load the entries
 if validdir then
 begin
  //Set up the array
  SetLength(Result.Entries,0);
  //Pointer to entry number - we'll use this later to find the end of the list
  ptr:=0;
  //Flag for a valid entry
  validentry:=True;
  while (ptr<numentrys) and (validentry) do
  begin
   //Offset to entry
   offset:=sector+entrys+ptr*entrysize;
   //Blank the entries
   ResetDirEntry(Entry);
   Entry.Parent:=pathname;
   //Read in the entries
   case FDirType of
    0,1: //Old and New Directory
     if ReadByte(emuheader+offset)<>0 then //0 marks the end of the entries
     begin
      Entry.Filename :=ReadString(emuheader+offset,-10,True);//Filename (including attributes for old)
      Entry.LoadAddr :=Read32b(emuheader+offset+$0A);  //Load Address (can be timestamp)
      Entry.ExecAddr :=Read32b(emuheader+offset+$0E);  //Execution Address (can be filetype)
      Entry.Length   :=Read32b(emuheader+offset+$12);  //Length in bytes
      Entry.Sector   :=Read24b(emuheader+offset+$16);   //How to find the file
      temp:='';
      //Old directories - attributes are in the filename's top bit
      if FDirType=0 then
      begin
       endofentry:=False;
       for amt:=0 to Length(Entry.Filename)-1 do
       begin
        if ord(Entry.Filename[amt+1])shr 7=1 then
         temp:=temp+OldAtts[amt];
        if ord(Entry.Filename[amt+1])AND$7F=$0D then endofentry:=True;
        //Clear the top bit
        if not endofentry then
         Entry.Filename[amt+1]:=chr(ord(Entry.Filename[amt+1]) AND $7F)
        else
         Entry.Filename[amt+1]:=' ';
       end;
       RemoveSpaces(Entry.Filename);
       //Reverse the attribute order to match actual ADFS
       for amt:=Length(temp) downto 1 do
        Entry.Attributes:=Entry.Attributes+temp[amt];//Attributes
      end;
      //New directories - attributes are separate, so filenames can have top bit set
      if FDirType=1 then
       NewDirAtts   :=ReadByte(emuheader+offset+$19);  //Attributes will be disected with Big
     end
     else validentry:=False;
    2: //Big Directory
    begin
     Entry.LoadAddr :=Read32b(emuheader+offset+$00);  //Load Address
     Entry.ExecAddr :=Read32b(emuheader+offset+$04);  //Execution Address
     Entry.Length   :=Read32b(emuheader+offset+$08);  //Length in bytes
     Entry.Sector   :=Read32b(emuheader+offset+$0C);  //How to find file
     NewDirAtts     :=Read32b(emuheader+offset+$10);  //Attributes (as New)
     NameLen        :=Read32b(emuheader+offset+$14);  //Length of filename
     NameOff        :=Read32b(emuheader+offset+$18);  //Offset into heap of filename
     Entry.Filename :=ReadString(emuheader+sector+nameheap+NameOff,-NameLen); //Filename
    end;
   end;
   RemoveControl(Entry.Filename);
   //Add up the used space
   if FMap then
    if Entry.Length mod secsize>0 then
     inc(free_space,(((Entry.Length)div secsize)+1)*secsize)
    else
     inc(free_space,Entry.Length);
   //Attributes for New and Big
   if FDirType>0 then
   begin
    temp:='';
    for amt:=0 to 7 do
     if IsBitSet(NewDirAtts,amt) then temp:=temp+NewAtts[amt];
    //Reverse the attribute order to match actual ADFS
    for amt:=Length(temp) downto 1 do
     Entry.Attributes:=Entry.Attributes+temp[amt];//Attributes
   end;
   //If we have a valid entry then we can see if it is filetyped/datestamped
   //and add it to the list
   if validentry then
   begin
    //RISC OS - file may be datestamped and filetyped
    if (Entry.LoadAddr shr 20=$FFF) and (FFormat>$12) then
    begin
     //Get the 12 bit filetype
     temp:=IntToHex((Entry.LoadAddr AND $000FFF00)div $100,3);
     amt:=0;
     //Look it up in the table of RISC OS issued types
     repeat
      inc(amt);
     until (Integer(amt)=Length(FileTypes)) OR (temp=Copy(FileTypes[amt],1,3));
     //Found? Then assign to the Filetype property
     if temp=Copy(FileTypes[amt],1,3) then
      Entry.Filetype:=Copy(FileTypes[amt],4,Length(FileTypes[amt]))
     else
     //Otherwise just put the 12 bit filetype in
      Entry.Filetype:=temp;
     Entry.ShortFiletype:=temp;
     //Now sort the timestamp
     Entry.TimeStamp:=ConvertTimeDate(Entry.ExecAddr+
                                     (Entry.LoadAddr AND $FF)*$100000000);
    end;
    //Not a directory - default. Will be determined later
    Entry.DirRef:=-1;
    //Add to the result
    SetLength(Result.Entries,Length(Result.Entries)+1);
    Result.Entries[Length(Result.Entries)-1]:=Entry;
    //Move on to next
    inc(ptr);
   end;
  end;
  //Now we can run the directory check on DirCheckByte
  if FDirType>0 then //But only for New and Big Directories
  //This has virtually the same loop repeated 5 times - but it is less code to
  //do it like this, than a single loop with if...then conditions to determine
  //the different iterations.
  begin
   dircheck:=0;
   amt:=0;
   //Stage 1: All the whole words at the start of the directory are accumulated
   //Start of directory includes the nameheap for Big Directories
   if FDirType<2 then EndOfChk:=entrys+ptr*entrysize
   else EndOfChk:=nameheap+namesize;
   while amt+3<EndOfChk do
   begin
    offset:=Read32b(emuheader+sector+amt);
    dircheck:=offset XOR ROR13(dircheck);
    inc(amt,4);
   end;
   //Stage 2: The bytes (<4) at the start of the directory are accumulated
   //individually.
   while amt<EndOfChk do
   begin
    offset:=ReadByte(emuheader+sector+amt);
    dircheck:=offset XOR ROR13(dircheck);
    inc(amt);
   end;
   //Stage 3: The first byte at the beginning of the directory tail is skipped.
   amt:=tail;
   //But not with Big Directories
   if FDirType<2 then inc(amt);
   //Stage 4: The whole words in the directory tail are accumulated, except the
   //very last word which is excluded as it contains the check byte.
   while amt+3<dirsize-4 do
   begin
    offset:=Read32b(emuheader+sector+amt);
    dircheck:=offset XOR ROR13(dircheck);
    inc(amt,4);
   end;
   //Stage 4a: Big Directories also accumulate the final few bytes, but not the
   //final byte
   if FDirType=2 then
    while amt<dirsize-1 do
    begin
     offset:=ReadByte(emuheader+sector+amt);
     dircheck:=offset XOR ROR13(dircheck);
     inc(amt);
    end;
   //Stage 5: The accumulated word has its four bytes exclusive ORd (EOR) together.
   dircheck:=(dircheck AND $FF)
        XOR ((dircheck shr 24) AND $FF)
        XOR ((dircheck shr 16) AND $FF)
        XOR ((dircheck shr  8) AND $FF);
   //This value is the check byte.
   if dirchk<>dircheck then
   begin
    Result.Broken:=True;
    Result.ErrorCode:=Result.ErrorCode OR $08;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Convert an ADFS New Map address to buffer offset address, with fragment lengths
-------------------------------------------------------------------------------}
function TDiscImage.DiscAddrToOffset(addr: Cardinal): TFragmentArray;
var
 fragid          : TFragmentArray;
 i,j,sector,id,
 allmap,len,off,
 zone,start,
 start_zone,
 zonecounter,
 id_per_zone     : Cardinal;
const
 dr_size = $40; //Size of disc record + header (zone 0)
 header  = 4;   //Size of zone header only (zones >0)
begin
 SetLength(Result,0);
 if FMap then //Only works for new maps
 begin
  if (addr=bootmap+(nzones*secsize*2)) and (FMap) then
  begin
   //We've been given the address of the root, but we know where this is so no
   //need to calculate it.
   SetLength(Result,1);
   Result[0].Offset:=addr;
   case FDirType of
    0: Result[0].Length:=$500;
    1: Result[0].Length:=$800;
    2: Result[0].Length:=root_size;
   end;
  end
  else
  begin
   //Set up an array
   SetLength(fragid,0);
   //Go through the allocation map, looking for the fragment
   //First we need to know how many ids per zone there are (max)
   id_per_zone:=((secsize*8)-zone_spare)div(idlen+1);
   //Then work out the start zone
   start_zone:=((addr DIV $100) mod $10000)div id_per_zone;
   //This is because the first fragment of an object does not necessarily
   //appear in zone order. Later fragments could be in earlier zones.
   for zonecounter:=0 to nzones-1 do
   begin
    //Account for which zone to start searching from
    zone:=(zonecounter+start_zone)mod nzones;
    //This is the start of where we take the offsets from
    start :=bootmap+dr_size;
    //Work out the end of this zone
    allmap:=(zone+1)*secsize*8-dr_size*8;
    //i is the bit counter - we need to move onto the next zone boundary
    i     :=zone*secsize*8;
    if zone>0 then dec(i,dr_size*8-header*8);
    repeat
     //Mark the offset
     off:=i;
     //Read in idlen number of bits
     id:=ReadBits(emuheader+start,i,idlen);
     //and move the pointer on idlen bits
     inc(i,idlen);
     if id<>0 then
     begin
      //Now find the end of the fragment entry
      j:=i-1;
      repeat
       inc(j);
      until (IsBitSet(ReadByte(emuheader+start+(j div 8)),j mod 8)) or (j>=allmap);
      //Move the pointer on, after the '1'
      i:=j;
      //And remember it
      len:=(j+1)*bpmb;
      //Does it match the id we are looking for?
      if id=(addr div $100)mod$10000 then
      begin
       off:=(off-(zone_spare*zone))*bpmb;
       off:=off mod disc_size;
       //Fragment ID found, so add it - there could be a few entries
       SetLength(fragid,Length(fragid)+1);
       fragid[Length(fragid)-1].Offset:=off;
       //Save the length
       fragid[Length(fragid)-1].Length:=len-fragid[Length(fragid)-1].Offset;
      end;
     end else inc(i);
     inc(i);
    until i>=allmap;
   end;
   //Now we need to set up the result array and convert from addresses to offsets
   SetLength(Result,Length(fragid));
   if Length(fragid)>0 then
    for i:=0 to Length(fragid)-1 do
    begin
     sector:=addr mod $100;
     //Sector needs to have 1 subtracted, if >=1
     if sector>=1 then dec(sector);
     //Then calculate the offset
     Result[i].Offset:=(fragid[i].Offset+(sector*secsize));
     //Add the length of this fragment
     Result[i].Length:=fragid[i].Length;
    end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Calculate Boot Block or Old Map Free Space Checksum
-------------------------------------------------------------------------------}
function TDiscImage.ByteChecksum(offset,size: Cardinal): Byte;
var
 acc,
 pointer: Cardinal;
begin
 //Reset the accumulator
 acc:=0;
 //Iterate through the block, ignoring the final byte (which contains the
 //checksum)
 for pointer:=size-2 downto 0 do
 begin
  //Add in the carry
  inc(acc,acc div $100);
  //and reset the carry
  acc:=acc and $FF;
  //Add each byte to the accumulator
  inc(acc,ReadByte(offset+pointer));
 end;
 //Add any carries and return an 8 bit number
 Result:=acc and $FF;
// Result:=((acc div $100)+(acc mod $100)) and $FF;
end;

{-------------------------------------------------------------------------------
Read ADFS Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadADFSDisc: TDisc;
var
 d,ptr    : Cardinal;
 OldName0,
 OldName1 : AnsiString;
begin
 //Initialise some variables
 root   :=$00; //Root address (set to zero so we can id the disc)
 SetLength(Result,0);
 UpdateProgress('Reading ADFS catalogue');
 //Read in the header information (that hasn't already been read in during
 //the initial checks
 if FFormat<>$FF then //But only if the format is valid
 begin
  //ADFS Old Map
  if not FMap then
  begin
   d:=2;
   root:=0;
   root_size:=$500;
   repeat
    if (ReadString(emuheader+(d*$100)+1,-4)='Hugo')
    or (ReadString(emuheader+(d*$100)+1,-4)='Nick') then root:=d;
    inc(d);
   until (d=(disc_size div $100)-1) or (root>0);
   if root=0 then
    ResetVariables //Failed to find root, so reset the format
   else
   begin
    OldName0 :=ReadString(emuheader+$0F7,-5);
    OldName1 :=ReadString(emuheader+$1F6,-5);
    //Re-assemble the disc title
    disc_name:='          ';
    if FDirType=1 then //S/M/L do not have disc title
    begin
     root_size:=$800;
     if Length(OldName0)>0 then
      for d:=0 to Length(OldName0)-1 do
       disc_name[ d*2 ]  :=OldName0[d];
     if Length(OldName1)>0 then
      for d:=0 to Length(OldName1)-1 do
       disc_name[(d*2)+1]:=OldName1[d];
    end;
    RemoveSpaces(disc_name);
    //Add up the free space
    d:=0;
    while d<ReadByte(emuheader+$1FE) do
    begin
     inc(free_space,Read24b(emuheader+$100+d)*$100);
     inc(d,3);
    end;
   end;
  end;
  //ADFS New Map
  if FMap then
  begin
   //Disc description starts at offset 4 and is 60 bytes long
   //Not all of these values will be used
   secsize     :=1 shl ReadByte(emuheader+bootmap+$04);
   secspertrack:=ReadByte(emuheader+bootmap+$05);
   heads       :=ReadByte(emuheader+bootmap+$06);
   density     :=ReadByte(emuheader+bootmap+$07);
   idlen       :=ReadByte(emuheader+bootmap+$08);
   bpmb        :=1 shl ReadByte(emuheader+bootmap+$09);
   skew        :=ReadByte(emuheader+bootmap+$0A);
   bootoption  :=ReadByte(emuheader+bootmap+$0B);
   lowsector   :=ReadByte(emuheader+bootmap+$0C);
   nzones      :=ReadByte(emuheader+bootmap+$0D);
   zone_spare  :=Read16b(emuheader+bootmap+$0E);
   root        :=Read32b(emuheader+bootmap+$10); //This may be overwritten later
   disc_size   :=Read32b(emuheader+bootmap+$14);
   disc_id     :=Read16b(emuheader+bootmap+$18);
   disc_name   :=ReadString(emuheader+bootmap+$1A,-10);
   disctype    :=Read32b(emuheader+bootmap+$24);
   //Newer attributes for E+ and F+
   disc_size   :=disc_size+Read32b(emuheader+bootmap+$28)*$100000000;
   share_size  :=1 shl ReadByte(emuheader+bootmap+$2C);
   big_flag    :=ReadByte(emuheader+bootmap+$2D);
   nzones      :=nzones+ReadByte(emuheader+bootmap+$2E)*$100;
   format_vers :=Read32b(emuheader+bootmap+$30);
   root_size   :=Read32b(emuheader+bootmap+$34);
   //The root always follows the map
   root:=bootmap+(nzones*secsize*2);
   //Update the Format, now we know the disc size
   if disc_size>1638400 then FFormat:=$1F;
  end;
  if root>$00 then //If root is still $00, then we have failed to id the disc
  begin
   //Create an entry for the root
   SetLength(Result,1);
   //Blank the values
   ResetDir(Result[0]);
   if FMap then
   begin
    //First 0x1000 bytes of the image hold the defect map and disc record
    free_space:=$1000;
    //Add the size of the root to the used space
    if root_size>0 then
     inc(free_space,root_size)
    else
     inc(free_space,$800);
    //Add the size of the zones
    inc(free_space,secsize*nzones*2);
    //It is easier to add up the length of files to work out the free space
    //rather than get this info from the byte allocation map
   end;
   //Read the root
   Result[0]:=ReadADFSDir(root_name,root);
   //Now iterate through the entries and find the sub-directories
   d:=0;
   repeat
    //If there are actually any entries
    if Length(Result[d].Entries)>0 then
    begin
     //Go through the entries
     for ptr:=0 to Length(Result[d].Entries)-1 do
      //And add them if they are valid
      if Result[d].Entries[ptr].Filename<>'' then
      begin
       //Attribute has a 'D', so drill down
       if Pos('D',Result[d].Entries[ptr].Attributes)>0 then
       begin
        //Once found, list their entries
        SetLength(Result,Length(Result)+1);
        //Read in the contents of the directory
        Result[Length(Result)-1]:=ReadADFSDir(Result[d].Entries[ptr].Parent+dir_sep
                                         +Result[d].Entries[ptr].Filename,
                                          Result[d].Entries[ptr].Sector);
        //Update the directory reference
        Result[d].Entries[ptr].DirRef:=Length(Result)-1;
       end;
      end;
    end;
    inc(d);
   //The length of disc will increase as more directories are found
   until d=Cardinal(Length(Result));
  end;
 end;
 //Turn the used space into amount of free space
 if FMap then free_space:=disc_size-free_space;
end;

//++++++++++++++++++ Acorn DFS +++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a DFS disc and which type
-------------------------------------------------------------------------------}
function TDiscImage.ID_DFS: Boolean;
var
 c,i   : Byte;
 t0,t1 : Integer;
begin
 Result:=False;
 if FFormat=$FF then
 begin
  UpdateProgress('Checking for DFS');
  //Is there actually any data?
  if Length(Fdata)>0 then
  begin
   Result:=True;
   // Offset 0x0001 should have 9 bytes without top bit set and >31
   c:=0;
   for i:=0 to 8 do
    if ((ReadByte($0001+i)>31) AND (ReadByte($0001+i)<127))
    or (ReadByte($0001+i)=0) then inc(c);
   if c<>9 then Result:=False;
   // Offset 0x0100 should have 4 bytes without top bit set and >31
   c:=0;
   for i:=0 to 3 do
    if ((ReadByte($0100+i)>31) AND (ReadByte($0100+i)<127))
    or (ReadByte($0100+i)=0) then inc(c);
   if c<>4 then Result:=False;
   // Offset 0x0105 should have bits 0,1 and 2 clear
   if (ReadByte($0105) AND $7)<>0 then Result:=False;
   // Offset 0x0106 should have bits 2,3,6 and 7 clear
   if (ReadByte($0106) AND $CC)<>0 then Result:=False;
   //Above checks have passed
   if Result then
   begin
    FDSD:=True; //Double sided flag
    // Offset 0x0A01 should have 9 bytes without top bit set and >31
    c:=0;
    for i:=0 to 8 do
     if ((ReadByte($0A01+i)>31) AND (ReadByte($0A01+i)<127))
     or (ReadByte($0A01+i)=0)  then inc(c);
    if c<>9 then FDSD:=False;
    // Offset 0x0B00 should have 4 bytes without top bit set and >31
    c:=0;
    for i:=0 to 3 do
     if ((ReadByte($0B00+i)>31) AND (ReadByte($0B00+i)<127))
     or (ReadByte($0B00+i)=0) then inc(c);
    if c<>4 then FDSD:=False;
    // Offset 0x0105 should have bits 0,1 and 2 clear
    if (ReadByte($0B05) AND $7)<>0 then FDSD:=False;
    // Offset 0x0106 should have bits 2,3,6 and 7 clear
    if (ReadByte($0B06) AND $CC)<>0 then FDSD:=False;
    //Number of sectors, side 0
    t0:=ReadByte($0107)+((ReadByte($0106)AND$3)shl 8);
    //DS tests passed, get the number of sectors, side 1
    if FDSD then
     t1:=ReadByte($0B07)+((ReadByte($0B06)AND$3)shl 8)
    else
     t1:=t0;
    //Number of tracks needs to be a multiple of 10
    if (t0 mod 10<>0) and (t1 mod 10<>0) then
     Result:=False
    else
    //Valid numbers of tracks (40 or 80)
    if  ((t0 div 10=40) or (t0 div 10=80))
    and ((t1 div 10=40) or (t1 div 10=80)) then
     if FDSD then
      FFormat:=$01
     else
      FFormat:=$00;
   end;
   //Test for Watford DFS - we'll only test one side.
   if Result then
   begin
    // Offset 0x0200 should have 8 bytes of 0xAA
    c:=0;
    for i:=0 to 7 do
     if ReadByte($0200+i)=$AA then inc(c);
    // Offset 0x0300 should have 4 bytes of 0x00
    for i:=0 to 3 do
     if ReadByte($0300+i)=$00 then inc(c);
    // Disc size should match also
    if  (c=12) and (Read16b($306)=Read16b($106)) then
     inc(FFormat,2);
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Converts a sector and side address into file offset address
-------------------------------------------------------------------------------}
function TDiscImage.ConvertSector(address,side: Integer): Integer;
var
 sector,
 offset: Integer;
begin
 //For an single sided disc, this is just the address
 if not FDSD then
  Result:=address
 //Otherwise, needs a bit of jiggery pokery, as the sides are interleaved
 else
 begin
  sector:=address DIV $100; //Sectors are $100 in size, and we need to know the sector
  offset:=address MOD $100; //Offset within the sector
  //Annoyingly, it is the tracks which are interleaved, not the sectors.
  //On Acorn DFS discs, there are 10 tracks per sector
  Result:=(((sector MOD 10)+(20*(sector DIV 10))+(10*side))*$100)+offset;
 end;
end;

{-------------------------------------------------------------------------------
Read Acorn DFS Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadDFSDisc: TDisc;
var
 s,t,c,f,
 locked,
 ptr,amt,
 diroff  : Integer;
 temp    : AnsiString;
begin
 UpdateProgress('Reading DFS Catalogue');
 if (FFormat AND $1)=1 then //Double sided image
  SetLength(Result,2)
 else                       //Single sided image
  SetLength(Result,1);
 s:=0;
 repeat
  ResetDir(Result[s]);
  //Number of entries on disc side
  t:=ReadByte(ConvertSector($105,s)) div 8;
  if (FFormat>$01) and (FFormat<$04) then //Extra files on Watford DFS
   inc(t,ReadByte(ConvertSector($305,s))div 8);
  SetLength(Result[s].Entries,t);
  //Directory name - as DFS only has $, this will be the drive number + '$'
  Result[s].Directory:=':'+IntToStr(s*2)+dir_sep+root_name;
  //Get the disc title(s)
  Result[s].Title:=ReadString(ConvertSector($000,s),-8)
                  +ReadString(ConvertSector($100,s),-4);
  RemoveSpaces(Result[s].Title);
  RemoveControl(Result[s].Title);
  if s>0 then disc_name:=disc_name+' and ';
  disc_name:=disc_name+Result[s].Title;
  //Disc Size
  inc(disc_size, (ReadByte(ConvertSector($107,s))
               +((ReadByte(ConvertSector($106,s)) AND $3)shl 8))*$100);
  //Directory size
  inc(free_space,$200);
  //Read the catalogue
  for f:=1 to t do
  begin
   //Reset the variables
   ResetDirEntry(Result[s].Entries[f-1]);
   //Is it a Watford, and are we in the Watford area?
   diroff:=$000;
   ptr:=f;
   if (FFormat>$01) and (FFormat<$04) then
    if (f>31) then
    begin
     diroff:=$200;
     ptr:=f-31;
    end;
   //Read in the filename
   temp:='';
   for c:=0 to 6 do
   begin
    amt:=ReadByte(ConvertSector(diroff+($08*ptr)+c,s)) AND $7F;
    if amt>32 then temp:=temp+chr(amt);
   end;
   Result[s].Entries[f-1].Filename:=temp;
   //Get the directory character
   temp:=chr(ReadByte(ConvertSector(diroff+($08*ptr)+7,s))AND $7F);
   if temp=' ' then temp:=root_name; //Acorn Atom DOS root is ' '
   //If the directory is not root, add it to the filename
   if temp<>root_name then
    Result[s].Entries[f-1].Filename:=temp+dir_sep
                                      +Result[s].Entries[f-1].Filename;
   //Make up a parent directory pathname so this can be found
   Result[s].Entries[f-1].Parent:=':'+IntToStr(s*2)+dir_sep+root_name;
   //Is it locked? This is actually the top bit of the final filename character
   locked:=(ReadByte(ConvertSector(diroff+($08*ptr)+7,s))AND $80) shr 7;
   if locked=1 then
    Result[s].Entries[f-1].Attributes:='L'
   else
    Result[s].Entries[f-1].Attributes:='';
   //Load address
   Result[s].Entries[f-1].LoadAddr:=
                  (((ReadByte(ConvertSector(diroff+$106+($08*ptr),s))AND $0C)shl 14)*$55)
                    +Read16b( ConvertSector(diroff+$100+($08*ptr),s));
   //Execution address
   Result[s].Entries[f-1].ExecAddr:=
                  (((ReadByte(ConvertSector(diroff+$106+($08*ptr),s))AND $C0)shl 10)*$55)
                  +  Read16b( ConvertSector(diroff+$102+($08*ptr),s));
   //Length
   Result[s].Entries[f-1].Length:=
                  (((ReadByte(ConvertSector(diroff+$106+($08*ptr),s))AND $30)shl 12)*$55)
                  +  Read16b( ConvertSector(diroff+$104+($08*ptr),s));
   inc(free_space,(Result[s].Entries[f-1].Length div $100)*$100);
   if Result[s].Entries[f-1].Length mod $100>0 then inc(free_space,$100);
   //Sector of start of data
   Result[s].Entries[f-1].Sector:=
                  ((ReadByte(ConvertSector(diroff+$106+($08*ptr),s))AND $3)shl 8)
                   +ReadByte(ConvertSector(diroff+$107+($08*ptr),s));
   //Which side it is on
   Result[s].Entries[f-1].Side:=s;
   //Not a directory - not used in DFS
   Result[s].Entries[f-1].DirRef:=-1;
  end;
  //Next side
  if (FFormat AND $1=1) then inc(s) else s:=2;
 until s=2;
 free_space:=disc_size-free_space;
end;

{-------------------------------------------------------------------------------
Write Acorn DFS File
-------------------------------------------------------------------------------}
function TDiscImage.WriteDFSFile(file_details: TDirEntry;m,f: Byte;
  var buffer: TDIByteArray): Integer;
var
 t     : Byte;
 i,l,
 pos,
 count,
 newlen,
 filen,
 size1,
 size2 : Integer;
 fn,dn : AnsiString;
 success: Boolean;
begin
 Result:=-1;
 count:=file_details.Length;
 //Overwrite the parent
 file_details.Parent:=':'+IntToStr(file_details.Side*2)+dir_sep+root_name;
 //Check that the filename is valid
 for i:=1 to Length(file_details.Filename) do
 begin
  //Remove top-bit set characters
  file_details.Filename[i]:=chr(ord(file_details.Filename[i]) AND $7F);
  //and remove control codes
  if ord(file_details.Filename[i])<32 then
   file_details.Filename[i]:=chr(ord(file_details.Filename[i])+32);
 end;
 //Ensure that the root has not been included
 if  (file_details.Filename[1]=root_name)
 and (file_details.Filename[2]=dir_sep) then
  file_details.Filename:=Copy(file_details.Filename,3,Length(file_details.Filename));
 //Is it not too long, including any directory specifier?
 if (file_details.Filename[2]=dir_sep) then
  file_details.Filename:=Copy(file_details.Filename,1,9)
 else
  file_details.Filename:=Copy(file_details.Filename,1,7);
 //Can the catalogue be extended?
 l:=Length(FDisc[file_details.Side].Entries);
 if ((l<31) and (f<2))         // Max 31 entries for Acorn DFS
 or ((l<62) and (f>1)) then    // and 62 entries for Watford DFS
 begin
  //Extend the catalogue by 1
  SetLength(FDisc[file_details.Side].Entries,l+1);
  Inc(l);
  filen:=0; //File 0 means no space, so add at the beginning
  if(l>1)then //Not the first entry?
  begin
   //Find if there is space inside the catalogue to insert the file
   size2:=count div $100;//Size, up to the next boundary, of the file being inserted
   if count mod $100>0 then inc(size2);
   for i:=l-2 downto 1 do
   begin
    //Size, up to the next boundary, of the existing file
    size1:=FDisc[file_details.Side].Entries[i].Length div $100;
    if FDisc[file_details.Side].Entries[i].Length mod $100>0 then inc(size1);
    //Check to see if it will fit above this file
    if FDisc[file_details.Side].Entries[i].Sector+size1+size2
                   <=FDisc[file_details.Side].Entries[i-1].Sector then filen:=i;
   end;
   //Move everything above this down by 1
   for i:=l-1 downto filen+1 do
    FDisc[file_details.Side].Entries[i]:=FDisc[file_details.Side].Entries[i-1];
   //Find the next available sector, from the previous entry in the catalogue
   pos:=FDisc[file_details.Side].Entries[filen+1].Length div $100;
   if FDisc[file_details.Side].Entries[filen+1].Length mod $100>0 then inc(pos);
   pos:=pos+FDisc[file_details.Side].Entries[filen+1].Sector;
  end
  else
  begin //First sector for the data, if first entry
   if f<2 then pos:=2; //Acorn DFS is sector 2
   if f>1 then pos:=4; //Watford DFS is sector 4
  end;
  //Add the entry at the insert point
  FDisc[file_details.Side].Entries[filen]:=file_details;
  //and update the entry we're writing to point to this sector
  FDisc[file_details.Side].Entries[filen].Sector:=pos;
  //Extend the image size, if necessary (image size != data size)
  newlen:=size2*$100;
  if ConvertSector(pos*$100+newlen,file_details.Side)>Length(Fdata) then
   SetLength(FData,ConvertSector(pos*$100+newlen,file_details.Side));
  //Then write the actual data
  success:=WriteDiscData(pos*$100,file_details.Side,buffer,count);
  //Update the catalogue, if successful
  if success then
  begin
   //Update the number of catalogue entries
   if l<32 then
    WriteByte(l*8,ConvertSector($105,file_details.Side));
   if f>1 then //Extra files on Watford DFS
    if l>31 then
    begin
     WriteByte( 31*8,   ConvertSector($105,file_details.Side));
     WriteByte((l-31)*8,ConvertSector($305,file_details.Side));
    end;
   //Update the free space
   dec(free_space,file_details.Length);
   //Update the catalogue
   for i:=0 to l-1 do
   begin
    //Filename
    fn:=FDisc[file_details.Side].Entries[i].Filename;
    //Directory specifier
    dn:=root_name; //Default will be root
    //Is there a directory specifier in the filename?
    if fn[2]=dir_sep then
    begin
     //Yes update the specifier
     dn:=fn[1];
     //and shorten the filename
     fn:=Copy(fn,3,Length(fn));
    end;
    //Now write the filename into the image
    for t:=0 to 6 do
     if t<Length(fn) then
      WriteByte(ord(fn[t+1]),ConvertSector($000+t+$08*(i+1),file_details.Side))
     else //Pad with spaces
      WriteByte($20,         ConvertSector($000+t+$08*(i+1),file_details.Side));
    //Directory specifier
    t:=Ord(dn[1]);
    //Attribute
    if StrPos(PChar(FDisc[file_details.Side].Entries[i].Attributes),'L')<>nil then
     t:=t OR $80;
    //Write the directory specifier and attribute together
    WriteByte(t,             ConvertSector($000+7+$08*(i+1),file_details.Side));
    //Load address
    Write16b(FDisc[file_details.Side].Entries[i].LoadAddr and $FFFF,
                               ConvertSector($100+$08*(i+1),file_details.Side));
    //Execution address
    Write16b(FDisc[file_details.Side].Entries[i].ExecAddr and $FFFF,
                               ConvertSector($102+$08*(i+1),file_details.Side));
    //Length
    Write16b(FDisc[file_details.Side].Entries[i].Length   and $FFFF,
                               ConvertSector($104+$08*(i+1),file_details.Side));
    //Start Sector
    WriteByte(FDisc[file_details.Side].Entries[i].Sector  and $FF,
                               ConvertSector($107+$08*(i+1),file_details.Side));
    //Extra bits for Load,Execution,Length and Start Sector
    t:=((Integer(FDisc[file_details.Side].Entries[i].Sector)  and   $300) shr  8) //bits 0,1
     OR((Integer(FDisc[file_details.Side].Entries[i].LoadAddr)and $30000) shr 14) //bits 2,3
     OR((Integer(FDisc[file_details.Side].Entries[i].Length  )and $30000) shr 12) //bits 4,5
     OR((Integer(FDisc[file_details.Side].Entries[i].ExecAddr)and $30000) shr 10);//bits 6,7
    WriteByte(t,               ConvertSector($106+$08*(i+1),file_details.Side));
   end;
   Result:=filen;//Pointer to where it was inserted
  end
  //or revert back if not
  else
  begin
   for i:=filen to l-2 do
    FDisc[file_details.Side].Entries[i]:=FDisc[file_details.Side].Entries[i+1];
   SetLength(FDisc,l-1);
  end;
  //The data written will get overwritten anyway if failed.
 end;
end;

//++++++++++++++++++ Commodore +++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a Commodore 1541/1571/1581 disc and which type
-------------------------------------------------------------------------------}
function TDiscImage.ID_CDR: Boolean;
var
 BAM,
 hdr,
 i   : Cardinal;
 ctr : Byte;
begin
 Result:=False;
 if FFormat=$FF then
 begin
  //Is there actually any data?
  if Length(Fdata)>0 then
  begin
   //IDing a 1541/1571
   ctr:=0;
   //BAM is at track 18 sector 0
   BAM:=ConvertDxxTS(0,18,0); //Get the BAM address - track 18 sector 0
   //BAM offset 0x02 should be 0x41 or 0x00
   if (ReadByte(BAM+$02)=$41)
   or (ReadByte(BAM+$02)=$00) then
    inc(ctr);
   //BAM offset 0xA0, 0xA1, 0xA4, and 0xA7-0xAA should be 0xA0
   if  (ReadByte(BAM+$A0)=$A0)
   and (ReadByte(BAM+$A1)=$A0)
   and (ReadByte(BAM+$A4)=$A0) then
    inc(ctr,3);
   for i:=$A7 to $AA do
    if ReadByte(BAM+i)=$A0 then
     inc(ctr);
   //BAM offset 0xA5 should be 0x32 and 0xA6 should be 0x41 ("2A")
   if  (ReadByte(BAM+$A5)=$32)
   and (ReadByte(BAM+$A6)=$41) then
    inc(ctr,2);
   //Succesful checks
   //BAM offset 0x03 will be 0x00 for 1541 and 0x80 for 1571
   if (ctr=10) and (ReadByte(BAM+$03)=$00) then FFormat:=$20; //Single sided : 1541
   if (ctr=10) and (ReadByte(BAM+$03)=$80) then FFormat:=$21; //Double sided : 1571
   //BAM is also at track 53 sector 0, for a double sided disc
   //IDing a 1581
   if FFormat=$FF then //Don't need to ID a 1581 if we already have a 1541/1571
   begin
    ctr:=0;
    //header is at track 40 sector 0
    hdr:=ConvertDxxTS(2,40,0);
    //header offset 0x02 should be 0x44
    if ReadByte(hdr+$02)=$44 then inc(ctr);
    //header offset 0x03 should be 0x00
    if ReadByte(hdr+$03)=$00 then inc(ctr);
    //header offset 0x14, 0x15, 0x18, 0x1B, 0x1C should be 0xA0
    if  (Read16b(hdr+$14)=$A0A0)
    and (ReadByte(hdr+$18)=$A0) and (Read16b(hdr+$1B)=$A0A0) then
     inc(ctr,5);
    //header offset 0x19 should 0x33 and 0x1A should be 0x44 ("3D")
    if  Read16b(hdr+$19)=$4433 then
     inc(ctr,2);
    //BAM, side 0, is at track 40 sector 1
    BAM:=ConvertDxxTS(2,40,1);
    //BAM offset 0x00, 0x01 should be 0x28 & 2
    if Read16b(BAM+$00)=$0228 then
     inc(ctr,2);
    //BAM offset 0x02 should be 0x44
    if ReadByte(BAM+$02)=$44 then
     inc(ctr);
    //BAM offset 0x04 & 0x05 should be the same is header offset 0x16 & 0x17
    if  (ReadByte(BAM+$04)=ReadByte(hdr+$16))
    and (ReadByte(BAM+$05)=ReadByte(hdr+$17)) then
     inc(ctr,2);
    //BAM, side 2, is at track 40 sector 2
    BAM:=ConvertDxxTS(2,40,2);
    //as above, except
    //BAM offset 0x00, 0x01 should be 0 & 0xFF
    if Read16b(BAM+$00)=$FF00 then
     inc(ctr,2);
    //Successful checks
    if ctr=16 then FFormat:=$22; //1581
   end;
   FDSD  :=(FFormat>$20)and(FFormat<$2F); //Set/reset the DoubleSided flag
   Result:=FFormat<>$FF;                  //Return TRUE if succesful ID
   If Result then FMap:=False;            //and reset the NewMap flag
  end;
 end;
end;

{-------------------------------------------------------------------------------
Converts a track and sector address into a file offset address (Commodore)
-------------------------------------------------------------------------------}
function TDiscImage.ConvertDxxTS(format,track,sector: Integer): Integer;
var
 x,c: Integer;
const
 //When the change of number of sectors occurs
 hightrack : array[0..8] of Integer = (71,66,60,53,36,31,25,18, 1);
 //Numbeber of sectors per track
 numsects  : array[0..7] of Integer = (17,18,19,21,17,18,19,21);
begin
 Result:=0;
 c:=0;
 //1541 has only 36 tracks
 if (format=0) AND (track>40) then track:=-1;
 //So if it is 36-40, compensate
 if (format=0) AND (track>35) then
 begin
  c:=track-35;
  track:=35;
 end;
 //1571 has only 70 tracks
 if (format=1) AND (track>70) then track:=-1;
 case format of
  0,1: //1541 & 1571
   if track<hightrack[0] then
   begin
    //Start at the end
    x:=7;
    while track>hightrack[x] do
    begin
     //Increase the tally by the number of sectors
     inc(Result,(hightrack[x]-hightrack[x+1])*numsects[x]);
     //Move to next entry
     dec(x);
    end;
    //Then add on the number of tracks * sectors
    inc(Result,(track+c-hightrack[x+1])*numsects[x]);
   end;
  2: Result:=(track-1)*40; //1581
 end;
 //Add on the sectors
 inc(Result,sector);
 //Multiply by the bytes per sector
 Result:=Result*$100;
 //If the track is invalid, return an invalid number
 if track=-1 then Result:=$FFFFF;
end;

{-------------------------------------------------------------------------------
Read Commodore Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadCDRDisc: TDisc;
var
 ptr,t,s,amt,
 file_chain,
 file_ptr,p,
 ch,c,f,dirTr :Integer;
 temp         : AnsiString;
const
 //Commodore 64 Filetypes
 FileTypes   : array[0.. 5] of AnsiString = (
 'DELDeleted' ,'SEQSequence','PRGProgram' ,'USRUser File','RELRelative',
 'CBMCBM'     );
begin
 UpdateProgress('Reading D64/D71/D81 catalogue');
 SetLength(Result,1);
 ResetDir(Result[0]);
 //Get the format
 f:=FFormat AND $F; //'f' is the format - 0: D64, 1: D71, 2: D81
 dirTr:=18; //D64 and D71 disc info is on track 18, sector 0
 if f=2 then dirTr:=40; //D81 disc info is on track 40, sector 0
 //Read the Header
 ptr:=ConvertDxxTS(f,dirTr,0); //Get the offset address of the header
 //Get the disc title
 temp:='';
 if f=2 then c:=$04 else c:=$90; //Location of disc title
 for ch:=0 to 15 do
 begin
  p:=ReadByte(ptr+c+ch);
  if (p>32) and (p<>$A0) then temp:=temp+chr(p);
 end;
 RemoveControl(temp);
 disc_name:=temp;
 //Size of the disc
 if f=2 then
  disc_size:=ConvertDxxTS(f,80,40)
 else
 begin
  disc_size:=ConvertDxxTS(f,35,17);
  if FDSD then disc_size:=disc_size*2;
 end;
 //Get the location of the directory
 t:=ReadByte(ptr+0);
 s:=ReadByte(ptr+1);
 //Calculate the free space (D64/D71)
 if f<2 then
 begin
  //Free space, side 0
  for c:=1 to 35 do //35 tracks
   inc(free_space,ReadByte(ptr+c*4)*$100);
  //Free space, side 1 (D71 - D64 will be zeros anyway)
  for c:=0 to 34 do //another 35 tracks
   inc(free_space,ReadByte(ptr+$DD+c)*$100);
 end;
 //Calculate the free space (D81)
 if f=2 then
  for ch:=1 to 2 do //Sector (0 is header, 1 is BAM side 0, 2 is BAM side 1)
  begin
   ptr:=ConvertDxxTS(f,dirTr,ch);
   for c:=0 to 39 do //40 tracks
    inc(free_space,ReadByte(ptr+$10+c*6)*$100);
  end;
 //Calculate where the first directory is
 ptr:=ConvertDxxTS(f,t,s);
 amt:=0;
 //Set the root directory name
 Result[0].Directory:=root_name;
 repeat
  //Track/Sector for next link or 00/FF for end
  t:=ReadByte(ptr);
  s:=ReadByte(ptr+1);
  for c:=0 to 7 do
   if ReadByte(ptr+(c*$20)+2)>$00 then
   begin
    SetLength(Result[0].Entries,amt+1);
    ResetDirEntry(Result[0].Entries[amt]);
    Result[0].Entries[amt].Parent:=root_name;
    //First track/sector of Fdata
    Result[0].Entries[amt].Track :=ReadByte(ptr+(c*$20)+3);
    Result[0].Entries[amt].Sector:=ReadByte(ptr+(c*$20)+4);
    //Filetype
    Result[0].Entries[amt].ShortFiletype:=
                           Copy(FileTypes[ReadByte(ptr+(c*$20)+2) AND $0F],1,3);
    Result[0].Entries[amt].Filetype:=
                           Copy(FileTypes[ReadByte(ptr+(c*$20)+2) AND $0F],4);
    //Attributes
    if (ReadByte(ptr+(c*$20)+2) AND $40)=$40 then //Locked
     Result[0].Entries[amt].Attributes:=Result[0].Entries[amt].Attributes+'L';
    if (ReadByte(ptr+(c*$20)+2) AND $80)=$80 then // Closed
     Result[0].Entries[amt].Attributes:=Result[0].Entries[amt].Attributes+'C';
    //Length of file - in sectors
    Result[0].Entries[amt].Length:=Read16b(ptr+(c*$20)+$1E);
    //now follow the chain to find the exact file length}
    file_ptr:=ConvertDxxTS(f,
      Result[0].Entries[amt].Track,Result[0].Entries[amt].Sector); //first sector
    //Now read the rest of the chain
    for file_chain:=1 to Result[0].Entries[amt].Length-1 do
     file_ptr:=ConvertDxxTS(f,ReadByte(file_ptr),ReadByte(file_ptr+1));
    //and get the partial usage of final sector
    if ReadByte(file_ptr)=$00 then
     Result[0].Entries[amt].Length:=
                 ((Result[0].Entries[amt].Length-1)*254)+ReadByte(file_ptr+1)-1;
    //Filename
    temp:='';
    for ch:=0 to 15 do
    begin
     p:=ReadByte(ptr+(c*$20)+5+ch);
     if (p>32) and (p<>$A0) then temp:=temp+chr(p);
    end;
    Result[0].Entries[amt].Filename:=temp;
    //Not a directory - not used by D64/D71/D81
    Result[0].Entries[amt].DirRef:=-1;
    inc(amt);
   end;
  //If not end of directory, go to next block
  if (t<>$00) and (s<>$FF) then ptr:=ConvertDxxTS(f,t,s);
 until (t=$00) and (s=$FF);
end;

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

//++++++++++++++++++ Commodore Amiga +++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a Commodore Amiga disc
-------------------------------------------------------------------------------}
function TDiscImage.ID_Amiga: Boolean;
var
 Checksum1,
 Checksum2 : Cardinal;
 ctr       : Integer;
 temp      : AnsiString;
const
 DiscIDs   : array[0..3] of AnsiString = ('DOS','PFS','KICK','KICKSUP');
begin
 Result:=False;
 if FFormat=$FF then
 begin
  //Only continue if there is data
  if Length(Fdata)>0 then
  begin
   //Find the disc ID
   ctr:=-1;
   repeat
    inc(ctr);
    temp:=ReadString(0,-Length(DiscIDs[ctr]));
   until (temp=DiscIDs[ctr]) or (ctr=High(DiscIDs));
   if temp=DiscIDs[ctr] then
   begin
    if Read32b($8,True)<>$370 then FBootBlock:=False; //No boot block
    //Read the boot block checksum
    if FBootBlock then
    begin
     Checksum1:=Read32b($4,True);
     //And calculate what it should be
     Checksum2:=AmigaBootChecksum($0);
    end
    else
    begin
     //No bootblock, so set both checksums to be the same
     Checksum1:=0;
     Checksum2:=0;
    end;
    //And make sure they match
    if Checksum1=Checksum2 then
    begin
     //Default directory type
     FDirType :=$00;
     //Get more details from the boot block, if there is one
     if FBootBlock then
     begin
      FMap    :=IsBitSet(ReadByte($03),0);   //AmigaDOS OFS/FFS
      FDirType:=(ReadByte($03) AND $4)shr 2; //AmigaDOS DIRC
     end;
     inc(FDirType,$10);
     secsize  :=$200;                        //Sector size
     FFormat  :=$4F;                         //Amiga format (hard disc)
     density  :=0;                           //Hard disc
     //Find the root
     root:=$002; //Start search at sector 2
     repeat
      //Make sure the checksums are not equal
      Checksum1:=$00;
      Checksum2:=$FF;
      //Find the primary and secondary types for a root block
      if root*secsize+secsize<Cardinal(Length(Fdata)) then
       if  (Read32b(root*secsize     ,True)=2)
       and (Read32b(root*secsize+$1FC,True)=1) then
       begin
        //Rootblock Checksum
        Checksum1:=Read32b(root*secsize+$14,True);
        Checksum2:=AmigaChecksum(root*secsize);
       end
       else
        //Check next sector
        inc(root);
     until (Checksum1=Checksum2)
        or (root*secsize+secsize>=Cardinal(Length(Fdata)));
     //Update the format. Anything else is a hard drive (already set)
     if (Checksum1=Checksum2) and (root=$370) then
     begin
      FFormat  :=$40;                         //Amiga format (DD)
      density  :=2;                           //Double Density
     end;
     if (Checksum1=Checksum2) and (root=$6E0) then
     begin
      FFormat  :=$41;                         //Amiga format (HD)
      density  :=4;                           //High Density
     end;
     //Set the disc size
     disc_size:=root*secsize*2;
     //Set the directory separator
     dir_sep:='/';
     //and the root name
     root_name:='root';
     //More checks to ensure we have the root
     if (Checksum1<>Checksum2)
     or (Read32b(root*secsize+$000,True)<>$02)
     or (Read32b(root*secsize+$00C,True)<>$48) then
     //these are, of course, only valid for a floppy image
      ResetVariables;
    end;
   end;
   Result:=FFormat<>$FF;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Read Commodore Amiga Disc
-------------------------------------------------------------------------------}
function TDiscImage.ReadAmigaDisc: TDisc;
var
 d,ptr,
 sectors,
 maxetry : Integer;
 fsm,
 fsmptr  : Cardinal;
 b,c     : Byte;
begin
 //Initialise some variables
 SetLength(Result,0);
 UpdateProgress('Reading Commodore Amiga Disc');
 if FFormat<>$FF then
 begin
  //Total number of sectors will be double where the root is
  sectors   :=root*2;
  //Disc size
  disc_size :=Cardinal(sectors)*secsize;
  //Disc name
  disc_name :=ReadString(root*secsize+$1B1,-(root*secsize+$1B0));
  //Work out the free space
  UpdateProgress('Calculating Free Space');
  free_space:=secsize*2; //Allow for the boot block, even if there isn't one
  dec(sectors,2);        //The first two sectors will still be allocated for one
  //Free Space Map pointer - starts at the root block
  fsmptr    :=root*secsize+$13C;
  //Maximum number of entries in the bitmap block
  maxetry   :=25;
  while sectors>0 do
  begin
   ptr       :=0;
   repeat
    //Get the next bitmap block
    fsm:=Read32b(fsmptr+Cardinal(ptr),True)*secsize;
    //Iterate through the bitmap block
    if fsm>0 then
    begin
     for d:=4 to secsize do
     begin
      //Get the next map byte
      b:=ReadByte(fsm+Cardinal(d));
      //Go through each bit in this byte
      for c:=0 to 7 do
      begin
       //If a bit is set, that sector is free
       if (IsBitSet(b,c)) and (sectors>0) then
        inc(free_space,secsize);
       //Take account of each sector
       dec(sectors);
      end;
     end;
    end;
    //Next bitmap block pointer
    inc(ptr,4);
   until (fsm=0) or (ptr>=maxetry*4) or (sectors<=0);
   //Get next Free Space Map pointer, if it is extended.
   fsmptr:=Read32b(fsmptr+Cardinal(ptr))*secsize;
   //Calculate the maximum number of entries
   maxetry:=(secsize-4) div 4;
   //If free space map pointer is 0, we have run out of bitmap blocks, so zero
   //the sector count
   if fsmptr=0 then sectors:=0;
  end;
  //Create an entry for the root
  SetLength(Result,1);
  //Blank the values
  ResetDir(Result[0]);
  //We'll start by reading the root
  Result[0]:=ReadAmigaDir(root_name,root);
  //Now iterate through the entries and find the sub-directories
  d:=0;
  repeat
   //If there are actually any entries
   if Length(Result[d].Entries)>0 then
   begin
    //Go through the entries
    for ptr:=0 to Length(Result[d].Entries)-1 do
     //And add them if they are valid
     if Result[d].Entries[ptr].Filename<>'' then
     begin
      //Attribute has a 'F', so drill down
      if Pos('F',Result[d].Entries[ptr].Attributes)>0 then
      begin
       //Once found, list their entries
       SetLength(Result,Length(Result)+1);
       //Read in the contents of the directory
       Result[Length(Result)-1]:=ReadAmigaDir(Result[d].Entries[ptr].Parent+dir_sep
                                         +Result[d].Entries[ptr].Filename,
                                          Result[d].Entries[ptr].Sector);
       //Update the directory reference
       Result[d].Entries[ptr].DirRef:=Length(Result)-1;
      end;
     end;
   end;
   inc(d);
  //The length of disc will increase as more directories are found
  until d>=Length(Result);
 end;
end;

{-------------------------------------------------------------------------------
Read Commodore Amiga Directory
-------------------------------------------------------------------------------}
function TDiscImage.ReadAmigaDir(dirname: AnsiString; offset: Cardinal): TDir;
var
 address,
 i,att,a,
 link,ent: Cardinal;
 Entry   : TDirEntry;
const
 attributes: array[0..31] of Char =
  ('D','E','W','R','A','P','S','H',
   'd','e','w','r','d','e','w','r',
   ' ',' ',' ',' ',' ',' ',' ',' ',
   ' ',' ',' ',' ',' ',' ',' ',' ');
begin
 ResetDir(Result);
 //If the checksum checks out, read in the contents
 if Read32b(offset*secsize+$14,True)=AmigaChecksum(offset*secsize) then
 begin
  //Directory Name
  if offset=root then
   Result.Directory:=dirname
  else
   Result.Directory:=ReadString(offset*secsize+$1B1,-ReadByte(offset*secsize+$1B0));
  //Go through the hash table and find the entries.
  ent:=Read32b(offset*secsize+$0C,True); //Size of hash table
  if ent=0 then ent:=(secsize div 4)-56; //if 0, then it should be BSIZE/4 - 56
  for i:=0 to ent-1 do
  begin
   //Get the sector of the next entry
   link:=Read32b(offset*secsize+$18+i*4,True);
   //Is entry found (0 if no entry)
//   if link<>0 then
   while link<>0 do
   begin
    ResetDirEntry(Entry);
    //Work out the offset absolute address
    address:=link*secsize;
    //Make sure checksum is valid
    if Read32b(address+$14,True)=AmigaChecksum(address) then
    begin
     //Read in the details
     Entry.Sector  :=Read32b(address+$10,True); //Sector of the first data block
     Entry.Filename:=ReadString(address+$1B1,-ReadByte(address+$1B0));
     Entry.Length  :=Read32b(address+$144,True);
     Entry.Parent  :=dirname;
     if Read32b(address+$1FC,True)=2 then //This is a directory
     begin
      //We'll use 'F' for directory, as 'D' is used for something else
      Entry.Attributes:='F';
      Entry.Filetype  :='Directory';
      Entry.Length    :=secsize;
      Entry.Sector    :=link; //Sector will therefore point to the header
     end;
     //Attributes
     att           :=Read32b(address+$140,True);
     for a:=0 to 31 do
      if not IsBitSet(att,a) then
       Entry.Attributes:=Entry.Attributes+attributes[a];
     RemoveSpaces(Entry.Attributes);
     //Not a directory - default. Will be determined later
     Entry.DirRef:=-1;
     //Add to the result
     SetLength(Result.Entries,Length(Result.Entries)+1);
     Result.Entries[Length(Result.Entries)-1]:=Entry;
     //Read the next in the link chain, if any (0 will exit the loop)
     link:=Read32b(address+$1F0,True);
    end
    //If checksum is invalid, mark the link as 0, which will exit the loop
    else link:=0;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Calculate Amiga Boot Block checksum
-------------------------------------------------------------------------------}
function TDiscImage.AmigaBootChecksum(offset: Cardinal): Cardinal;
begin
 Result:=not GeneralChecksum(offset,$400,$004,0,True);
end;

{-------------------------------------------------------------------------------
Calculate Amiga regular checksum
-------------------------------------------------------------------------------}
function TDiscImage.AmigaChecksum(offset: Cardinal): Cardinal;
begin
 Result:=-GeneralChecksum(offset,$200,$014,0,False);
end;

end.
