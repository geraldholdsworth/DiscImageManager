//++++++++++++++++++ Private methods +++++++++++++++++++++++++++++++++++++++++++

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
 FDOSPresent   :=False;
 FDOSVolInRoot :=False;
 secsize       :=$0100;
 bpmb          :=$0000;
 nzones        :=$0000;
 root          :=$0000;
 Fafsroot      :=$0000;
 Fdosroot      :=$0000;
 bootmap       :=$0000;
 zone_spare    :=$0000;
 format_vers   :=$0000;
 root_size     :=$0000;
 afsroot_size  :=$0000;
 dosroot_size  :=$0000;
 disc_id       :=$0000;
 SetLength(disc_size,1);
 disc_size[0]  :=$0000;
 SetLength(free_space,1);
 free_space[0] :=$0000;
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
 SetLength(disc_name,1);
 disc_name[0]  :='';
 emuheader     :=$0000;
 dir_sep       :='.';
 root_name     :='$';
 imagefilename :='';
 doshead       :=0;
 doshead2      :=0;
 dosmap        :=0;
 dosmap2       :=0;
 FATType       :=0;
 DOSVersion    :=0;
 NumFATs       :=0;
 DOSFATSize    :=0;
 DOSResSecs    :=0;
 cluster_size  :=0;
 DOSBlocks     :=0;
 SetLength(free_space_map,0);
 Fupdating     :=False;
end;

{-------------------------------------------------------------------------------
Reset the partition to match the global variables
-------------------------------------------------------------------------------}
procedure TDiscImage.AddPartition;
var
 part: Integer;
begin
 part:=Length(FPartitions);
 SetLength(FPartitions,part+1);
 FPartitions[part].Directories:=FDisc;
 FPartitions[part].Title:=disc_name[part];
// FPartitions[part].RootTitle:=
 FPartitions[part].RootName:=root_name;
 FPartitions[part].DirSep:=dir_sep;
// FPartitions[part].HeaderAddr:=
// FPartitions[part].FSMAddr
{   FreeSpaceMap    : array of TTrack;  //The free space map
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
}
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
 while(r>=c)and //Test for control character
    (((r<>term)and(term>=0))or //Test for terminator character
     ((x<abs(term))and(term<0)))do //Test for string length
 begin
  Result:=Result+chr(r); //Add it to the string
  inc(x);                //Increase the counter
  r:=ReadByte(ptr+x,buffer);    //Read the next character
 end;
end;

{-------------------------------------------------------------------------------
Write a string to a buffer, for length len padded with pad
-------------------------------------------------------------------------------}
procedure TDiscImage.WriteString(str: String;ptr,len: Cardinal;pad: Byte);
var
 buffer: TDIByteArray;
begin
 buffer:=nil;
 WriteString(str,ptr,len,pad,buffer);
end;
procedure TDiscImage.WriteString(str: String;ptr,len: Cardinal;pad: Byte;
                                                      var buffer: TDIByteArray);
var
 x : Integer;
 c : Byte;
begin
 if(str='')and(len>0)then str:=AddCharR(chr(pad),str,len);
 //Only do something if a string has been supplied
 if str<>'' then
 begin
  //if no length specified, use the length of the supplied string
  if len=0 then len:=Length(str);
  //Loop through each character
  for x:=0 to len-1 do
  begin
   //If string is less than specified length, pad it with this byte
   c:=pad;
   if x<Length(str) then c:=Ord(str[x+1]);
   //Write it to the specified buffer, or general data area if no buffer
   WriteByte(c,ptr+x,buffer);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Convert a format byte to a string
-------------------------------------------------------------------------------}
function TDiscImage.FormatToString: String;
const
 FS  : array[0..$A] of String = ('DFS',
                                'Acorn ADFS',
                                'Commodore',
                                'Sinclair Spectrum +3/Amstrad',
                                'Commodore Amiga',
                                'Acorn CFS',
                                'MMFS',
                                'Acorn FS',
                                'Spark Archive',
                                'SJ Research MDFS',
                                'DOS');
 SUB : array[0..$A] of array[0..15] of String =
 (('Acorn SSD','Acorn DSD','Watford SSD','Watford DSD','','Acorn/Watford DSD','','Watford/Acorn DSD','','','','','','','',''),
  ('S','M','L','D','E','E+','F','F+','','','','','','','Hybrid','Hard Disc'),
  ('1541','1571','1581','1541 40 Track','1571 80 Track','','','','','','','','','','',''),
  ('','Extended','','','','','','','','','','','','','',''),
  ('DD','HD','','','','','','','','','','','','','','Hard Disc'),
  ('','','','','','','','','','','','','','','',''),
  ('','','','','','','','','','','','','','','',''),
  ('Level 1','Level 2','Level 3','Level 4','','','','','','','','','','','',''),
  ('','','','','','','','','','','','','','','',''),
  ('','','','','','','','','','','','','','','',''),
  ('Plus','FAT12','FAT16','FAT32','','','','','','','','','','','',''));
begin
 Result:='';
 if GetMajorFormatNumber<=High(FS) then
  if GetMinorFormatNumber<=High(SUB[GetMajorFormatNumber]) then
  begin
   Result:= FS[GetMajorFormatNumber];
   if SUB[GetMajorFormatNumber,GetMinorFormatNumber]<>'' then
    Result:=Result+' '+SUB[GetMajorFormatNumber,GetMinorFormatNumber];
  end;
 //ADFS with AFS partition
 if(FFormat=diAcornADFS<<4+ 2)and(FAFSPresent)then
  Result:=Result+'/'+FS[diAcornFS];
 //ADFS with DOS Plus partition
 if((FFormat=diAcornADFS<<4+ 2)
 or (FFormat=diAcornADFS<<4+$F))and(FDOSPresent)then
  Result:=Result+'/'+FS[diDOSPlus];
end;

{-------------------------------------------------------------------------------
Convert a format byte to an extension
-------------------------------------------------------------------------------}
function TDiscImage.FormatToExt: String;
const
 EXT : array[0..$A] of array[0..15] of String =
 (('ssd','dsd','ssd','dsd','','dsd','','dsd','','','','','','','',''),//DFS
  ('ads','adm','adl','adf','adf','adf','adf','adf','','','','','','','dat','hdf'),//ADFS
  ('d64','d71','d81','d64','d71','','','','','','','','','','',''),//Commodore 64
  ('','dsk','','','','','','','','','','','','','','hdf'),//Sinclair Spectrum
  ('adf','adf','','','','','','','','','','','','','','hdf'),//Commodore Amiga
  ('uef','','','','','','','','','','','','','','',''),//CFS
  ('mmb','','','','','','','','','','','','','','',''),//MMFS
  ('afs','afs','afs','afs','','','','','','','','','','','','afs'),//Acorn File Server
  ('zip','','','','','','','','','','','','','','',''),//!Spark
  ('dat','','','','','','','','','','','','','','',''),//SJ MDFS
  ('img','fat12','fat16','fat32','','','','','','','','','','','',''));//DOS and DOS Plus
begin
 Result:='img';
 if GetMajorFormatNumber<=High(EXT) then
  if GetMinorFormatNumber<=High(EXT[GetMajorFormatNumber]) then
   Result:=EXT[GetMajorFormatNumber,GetMinorFormatNumber];
end;

{-------------------------------------------------------------------------------
Get the major format number
-------------------------------------------------------------------------------}
function TDiscImage.GetMajorFormatNumber: Word;
begin
 Result:=FFormat>>4;
end;

{-------------------------------------------------------------------------------
Get the minor format number
-------------------------------------------------------------------------------}
function TDiscImage.GetMinorFormatNumber: Byte;
begin
 Result:=FFormat mod $10;
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
 buffer: TDIByteArray;
begin
 SetLength(buffer,0);
 WriteBits(value,offset,start,length,buffer);
end;
procedure TDiscImage.WriteBits(value,offset,start,length: Cardinal;buffer:TDIByteArray);
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
    lastbyte:=ReadByte(pos,buffer);
    lastcopy:=lastbyte; //Take a copy to see if we need to write
   end;
   b:=((value AND (1 shl bit))shr bit)shl start_bit; //Bit to set
   c:=(1 shl start_bit)XOR$FF;                       //Bit to clear
   lastbyte:=(lastbyte AND c)OR b;                   //Set/clear the bit
   //Then write the byte back, if it has changed
   if lastbyte<>lastcopy then
   begin
    WriteByte(lastbyte,pos,buffer);
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
  //Compensate for interleaving (ADFS L & AFS)
  offset:=DiscAddrToIntOffset(offset);
  //Compensate for emulator header
  inc(offset,emuheader);
  //If we are inside the data, read the byte
  if offset<Length(Fdata) then Result:=Fdata[offset];
 end;
end;

{-------------------------------------------------------------------------------
Calculate offset into image given the disc address (Interleaved or Multiplexed)
-------------------------------------------------------------------------------}
function TDiscImage.DiscAddrToIntOffset(disc_addr: Cardinal): Cardinal;
var
 track_size,
 track,
 side,
 data_offset  : Cardinal;
const
 tracks   = 80;
 oldheads = 2;
begin
 Result:=disc_addr;
 if(FForceInter=0)and(GetMajorFormatNumber=diAcornADFS)then
  if(FFormat<>diAcornADFS<<4+$02)then exit;
 //ADFS L or AFS with 'INT' option
 if((GetMajorFormatNumber=diAcornADFS{<<4+$02})or(GetMajorFormatNumber=diAcornFS))
 and(Finterleave>1)then
 begin
  //Variables not set, then set them to default
  if secspertrack=0 then secspertrack:=16;
  if secsize=0 then secsize:=256;
  //Track Size;
  track_size:=secspertrack*secsize;
  //Track number
  track:=(disc_addr DIV track_size) MOD tracks;
  //Offset into the sector for the data
  data_offset:=disc_addr MOD track_size;
  //Final result
  case FInterleave of
   2:
    begin
     //Which side
     side:=disc_addr DIV (tracks*track_size);
     Result:=(track_size*side)+(track*track_size*oldheads)+data_offset; //INT
    end;
   3:
    begin
     side:=track mod 2;
     Result:=(((track div 2)+(side*tracks))*track_size)+data_offset;//MUX-2
    end;
   4:
    begin
     side:=tracks div 2;
     Result:=((((track*side)mod(tracks*2))
              +((track*side)div(tracks*2)))*track_size)+data_offset;//MUX-4
    end;
  end;
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
  //Compensate for interleaving (ADFS L & AFS)
  offset:=DiscAddrToIntOffset(offset);
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
  DOSPartition:=False;
  Partition   :=0;
  Sector      :=0;
  Parent      :=-1;
  BeenRead    :=False;
  Deleted     :=False;
 end;
end;

{-------------------------------------------------------------------------------
Convert the Map flag to Map Type
-------------------------------------------------------------------------------}
function TDiscImage.MapFlagToByte: Byte;
begin
 Result:=diUnknownDir;          //Default value for non-ADFS
 if GetMajorFormatNumber=diAcornADFS then //Is it ADFS?
 begin
  Result:=diADFSOldMap;              // ADFS Old Map
  if FMap then Result:=diADFSNewMap; // ADFS New Map
 end;
 if GetMajorFormatNumber=diAmiga then     //Is it Amiga?
 begin
  Result:=diAmigaOFS;                // AmigaDOS OFS
  if FMap then Result:=diAmigaFFS;   // AmigaDOS FFS
 end;
 if GetMajorFormatNumber=diDOSPlus then   //Is it DOS
  Result:=FATType;
end;

{-------------------------------------------------------------------------------
Convert the Map flag to String
-------------------------------------------------------------------------------}
function TDiscImage.MapTypeToString: String;
begin
 Result:='';
 //ADFS and AmigaDOS
 if(GetMajorFormatNumber=diAcornADFS)or(GetMajorFormatNumber=diAmiga)then
 begin
  case MapFlagToByte of
   diADFSOldMap: Result:='ADFS Old Map';
   diADFSNewMap: Result:='ADFS New Map';
   diAmigaOFS  : Result:='AmigaDOS OFS';
   diAmigaFFS  : Result:='AmigaDOS FFS';
  end;
 end;
 if GetMajorFormatNumber=diDOSPlus then
 begin
  case FATType of
   diFAT12 : Result:='FAT12';
   diFAT16 : Result:='FAT16';
   diFAT32 : Result:='FAT32';
  end;
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
 if GetMajorFormatNumber=diAcornADFS then //New map will return the fragment ID
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
 if Length(buffer)<10 then exit;
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

{-------------------------------------------------------------------------------
Convert the interleave into a string
-------------------------------------------------------------------------------}
function TDiscImage.InterleaveString: String;
const
 ints:array[0..2] of String=('Sequential','Interleave','Multiplex');
begin
 Result:='';
 if(GetMajorFormatNumber=diAcornDFS)and(FDSD)then Result:=ints[1];
 {if(FFormat=diAcornADFS<<4+2)
 or(FFormat=diAcornADFS<<4+$E)}
 if(GetMajorFormatNumber=diAcornADFS)
 or(GetMajorFormatNumber=diAcornFS)then
  if FInterleave-1<=High(ints) then Result:=ints[FInterleave-1];
end;

{-------------------------------------------------------------------------------
Volume Serial Number Calculation
-------------------------------------------------------------------------------}
function TDiscImage.VolumeSerialNumber: Cardinal;
var
 year,
 month,
 day,
 hour,
 minute,
 second,
 ms     : Word;
 time   : TDateTime;
begin
 time:=Now; //Based on the current time
 DecodeDateTime(time,year,month,day,hour,minute,second,ms);
 Result:=(month+second)<<24+(day+ms)<<16+hour<<8+minute+year;
end;

{-------------------------------------------------------------------------------
Update directory references
-------------------------------------------------------------------------------}
procedure TDiscImage.UpdateDirRef(dirref: Cardinal);
var
 d,e: Cardinal;
begin
 //Update all the directory references
 if Length(FDisc)>0 then
  for d:=0 to Length(FDisc)-1 do
  begin
   //Update any parents
   if FDisc[d].Parent>dirref then dec(FDisc[d].Parent);
   if Length(FDisc[d].Entries)>0 then
    for e:=0 to Length(FDisc[d].Entries)-1 do
     if FDisc[d].Entries[e].DirRef>dirref then
      dec(FDisc[d].Entries[e].DirRef); //And the DirRef
  end;
end;
