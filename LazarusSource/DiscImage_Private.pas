//++++++++++++++++++ Private methods +++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Reset all the variables
-------------------------------------------------------------------------------}
procedure TDiscImage.ResetVariables;
begin
 //Default values
 SetLength(FDisc,0);
// FDSD          :=False;
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
 FHasDirs      :=False;
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
 Fcopyright    :='';
 Fversion      :='';
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
 FPartitions[part].Directory:=FDisc;
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
 buffer: TDIByteArray=nil;
begin
 buffer:=nil;
 Result:=ReadString(ptr,term,buffer,control);
end;
function TDiscImage.ReadString(ptr,term: Integer;var buffer:TDIByteArray;
                                  control: Boolean=True): String;
var
 x : Integer=0;//Counter
 c : Byte=0;
 r : Byte=0;
begin
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
 buffer: TDIByteArray=nil;
begin
 WriteString(str,ptr,len,pad,buffer);
end;
procedure TDiscImage.WriteString(str: String;ptr,len: Cardinal;pad: Byte;
                                                      var buffer: TDIByteArray);
var
 x : Integer=0;
 c : Byte=0;
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
 FS  : array[0..$B] of String = ('DFS',
                                'Acorn ADFS',
                                'Commodore',
                                'Sinclair Spectrum +3/Amstrad',
                                'Commodore Amiga',
                                'Acorn CFS',
                                'MMFS',
                                'Acorn FS',
                                'Spark Archive',
                                'SJ Research MDFS',
                                'DOS',
                                'Acorn ROM FS');
 SUB : array[0..$B] of array[0..15] of String =
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
  ('Plus','FAT12','FAT16','FAT32','','','','','','','','','','','',''),
  ('','','','','','','','','','','','','','','',''));
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
 EXT : array[0..$B] of array[0..15] of String =
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
  ('img','fat12','fat16','fat32','','','','','','','','','','','',''),//DOS and DOS Plus
  ('rom','','','','','','','','','','','','','','','')//ROM FS
  );
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
Get the double sided flag
-------------------------------------------------------------------------------}
function TDiscImage.GetDoubleSided: Boolean;
begin
 Result:=(FFormat>>4=diAcornDFS)AND(FFormat AND 1=1);
end;

{-------------------------------------------------------------------------------
Read upto 32 bits of data from the buffer, starting at offset(bytes)+start(bits)
-------------------------------------------------------------------------------}
function TDiscImage.ReadBits(offset,start,length: Cardinal): Cardinal;
var
 start_byte : Cardinal=0;
 start_bit  : Cardinal=0;
 bit        : Cardinal=0;
 b          : Cardinal=0;
 pos        : Cardinal=0;
 lastbyte   : Byte=0;
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
 buffer: TDIByteArray=nil;
begin
 SetLength(buffer,0);
 WriteBits(value,offset,start,length,buffer);
end;
procedure TDiscImage.WriteBits(value,offset,start,length: Cardinal;buffer:TDIByteArray);
var
 start_byte : Cardinal=0;
 start_bit  : Cardinal=0;
 bit        : Cardinal=0;
 b          : Cardinal=0;
 c          : Cardinal=0;
 pos        : Cardinal=$FFFFFFFF;
 lastbyte   : Byte=0;
 lastcopy   : Byte=$FF;
begin
 //If the length is 0, nothing to write. Cardinals are 32 bits
 //(we could use Integers, but these are signed)
 if(length>0)and(length<33)then
 begin
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
 riscosdays : Int64=0;
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
 days  : Int64=0;
 time  : Int64=0;
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
 buffer: TDIByteArray=nil;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Result:=Read32b(offset,buffer,bigendian);
end;
function TDiscImage.Read32b(offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False): Cardinal;
var
 i: Cardinal=0;
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
 buffer: TDIByteArray=nil;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Result:=Read24b(offset,buffer,bigendian);
end;
function TDiscImage.Read24b(offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False): Cardinal;
var
 i: Cardinal=0;
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
 buffer: TDIByteArray=nil;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Result:=Read16b(offset,buffer,bigendian);
end;
function TDiscImage.Read16b(offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False): Word;
var
 i: Cardinal=0;
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
 buffer: TDIByteArray=nil;
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
 track_size : Cardinal=0;
 track      : Cardinal=0;
 side       : Cardinal=0;
 data_offset: Cardinal=0;
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
 buffer: TDIByteArray=nil;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Write32b(value,offset,buffer,bigendian);
end;
procedure TDiscImage.Write32b(value, offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False);
var
 i: Cardinal=0;
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
 buffer: TDIByteArray=nil;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Write24b(value,offset,buffer,bigendian);
end;
procedure TDiscImage.Write24b(value,offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False);
var
 i: Cardinal=0;
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
 buffer: TDIByteArray=nil;
begin
 //Need to pass a zero length array, as we can't simply pass 'nil'
 SetLength(buffer,0);
 Write16b(value,offset,buffer,bigendian);
end;
procedure TDiscImage.Write16b(value: Word; offset: Cardinal;var buffer: TDIByteArray;
                                  bigendian: Boolean=False);
var
 i: Cardinal=0;
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
 buffer: TDIByteArray=nil;
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
 pointer : Cardinal=0;
 word    : Cardinal=0;
 acc     : Int64=0; //Reset the accumulator to zero
begin
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
Calculate a MD5 for the supplied buffer Byte array
-------------------------------------------------------------------------------}
function TDiscImage.GetMD5(var buffer: TDIByteArray): String;
begin
 if Length(buffer)>0 then Result:=MD5Print(MD5Buffer(buffer[0],Length(buffer)))
 else Result:='00000000';
end;

{-------------------------------------------------------------------------------
Calculate a CRC-32 for the supplied buffer Byte array
-------------------------------------------------------------------------------}
function TDiscImage.GetCRC(var buffer: TDIByteArray): String;
var
 CRCValue: longword=0;
begin
 if Length(buffer)>0 then
 begin
  CRCValue:=crc.crc32(0,nil,0);
  CRCValue:=crc.crc32(0,@buffer[0],Length(buffer));
  Result  :=IntToHex(CRCValue,8);
 end
 else Result:='00000000';
end;

{-------------------------------------------------------------------------------
Calculate the CRC-16 value
-------------------------------------------------------------------------------}
function TDiscImage.GetCRC16(start,len: Cardinal): Cardinal;
var
 dummy: TDIByteArray=nil;
begin
 SetLength(dummy,0);
 Result:=GetCRC16(start,len,dummy);
end;
function TDiscImage.GetCRC16(start,len: Cardinal;var buffer: TDIByteArray): Cardinal;
var
 addr: Cardinal=0;
 bit : Byte=0;
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
  GZ     : TGZFileStream=nil;
  chunk  : TDIByteArray=nil;
  cnt    : Integer=0;
  i      : Integer=0;
  buflen : Integer=0;
 const
   ChunkSize=4096; //4K chunks
 begin
  //Initialise the variables
  Result:=nil;
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
 F        : TFileStream=nil;
 buffer   : TDIByteArray=nil;
 inflated : TDIByteArray=nil;
 ptr      : Cardinal=0;
 i        : Cardinal=0;
 old      : Cardinal=0;
 blockptrs: array of Cardinal=nil;
 fn       : String='';
begin
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
begin
 Result:='';
 if(GetMajorFormatNumber=diAcornDFS)and(GetDoubleSided)then Result:=Fints[1];
 {if(FFormat=diAcornADFS<<4+2)
 or(FFormat=diAcornADFS<<4+$E)}
 if(GetMajorFormatNumber=diAcornADFS)
 or(GetMajorFormatNumber=diAcornFS)then
  if FInterleave-1<=High(Fints) then Result:=Fints[FInterleave-1];
end;

{-------------------------------------------------------------------------------
Volume Serial Number Calculation
-------------------------------------------------------------------------------}
function TDiscImage.VolumeSerialNumber: Cardinal;
var
 year   : Word=0;
 month  : Word=0;
 day    : Word=0;
 hour   : Word=0;
 minute : Word=0;
 second : Word=0;
 ms     : Word=0;
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
 d: Cardinal=0;
 e: Cardinal=0;
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

{-------------------------------------------------------------------------------
Remove a directory from the internal array
-------------------------------------------------------------------------------}
procedure TDiscImage.RemoveDirectory(dirref: Cardinal);
var
 i    : Cardinal=0;
 entry: Cardinal=0;
begin
 if dirref<Length(FDisc)-1 then
  for i:=dirref to Length(FDisc)-2 do
  begin
   FDisc[i].Directory   :=FDisc[i+1].Directory;
   FDisc[i].Title       :=FDisc[i+1].Title;
   FDisc[i].ErrorCode   :=FDisc[i+1].ErrorCode;
   FDisc[i].Deleted     :=FDisc[i+1].Deleted;
   FDisc[i].Broken      :=FDisc[i+1].Broken;
   FDisc[i].Locked      :=FDisc[i+1].Locked;
   FDisc[i].BeenRead    :=FDisc[i+1].BeenRead;
   FDisc[i].DOSPartition:=FDisc[i+1].DOSPartition;
   FDisc[i].AFSPartition:=FDisc[i+1].AFSPartition;
   FDisc[i].Sector      :=FDisc[i+1].Sector;
   FDisc[i].Length      :=FDisc[i+1].Length;
   FDisc[i].Partition   :=FDisc[i+1].Partition;
   FDisc[i].Parent      :=FDisc[i+1].Parent;
   SetLength(FDisc[i].Entries,Length(FDisc[i+1].Entries));
   if Length(FDisc[i].Entries)>0 then
    for entry:=0 to Length(FDisc[i].Entries)-1 do
     FDisc[i].Entries[entry]:=FDisc[i+1].Entries[entry];
  end;
 SetLength(FDisc,Length(FDisc)-1);
end;

{-------------------------------------------------------------------------------
Update the disc titles
-------------------------------------------------------------------------------}
procedure TDiscImage.SetDefaultDiscTitle(ADiscTitle: String);
begin
 if ADiscTitle<>'' then Fdisctitle:=ADiscTitle;
end;
procedure TDiscImage.SetDefaultAFSDiscTitle(ADiscTitle: String);
begin
 if ADiscTitle<>'' then Fafsdisctitle:=ADiscTitle;
end;
procedure TDiscImage.SetDefaultAmigaDiscTitle(ADiscTitle: String);
begin
 if ADiscTitle<>'' then Famigadisctitle:=ADiscTitle;
end;
procedure TDiscImage.SetDefaultRFSTitle(ADiscTitle: String);
begin
 if ADiscTitle<>'' then Frfstitle:=ADiscTitle;
end;
procedure TDiscImage.SetDefaultRFSCopyRight(ADiscTitle: String);
begin
 if ADiscTitle<>'' then Frfscopyright:=ADiscTitle;
end;

//++++++++++++++++++ TSpark Private methods ++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Extract the list of files
-------------------------------------------------------------------------------}
function TSpark.ExtractFiles: TFileList;
begin
 Result:=nil;
 if FIsSpark then Result:=ExtractSparkFiles;
 if FIsPack  then Result:=ExtractPackFiles;
end;

{-------------------------------------------------------------------------------
Extract the list of files (!Spark)
-------------------------------------------------------------------------------}
function TSpark.ExtractSparkFiles: TFileList;
var
 ptr        : Cardinal=0;
 EoCL       : Integer=-1;
 CL         : Integer=-1;
 ctr        : Cardinal=0;
 fnc        : Cardinal=0;
 fnL        : LongWord=0;
 exL        : LongWord=0;
 cmL        : LongWord=0;
 fn         : String='';
 zipfn      : String='';
 exists     : Integer=0;
 temp       : TFileEntry=();
begin
 Result:=nil;
 ResetFileEntry(temp);
 if FIsSpark then
 begin
  FBitLength:=0; //Not used with !Spark
  //Find the 'End of central library'
  CL:=FindCL(EoCL,Fbuffer);
  //Only continue of we have a marker
  if(EoCL<>-1)and(CL<>-1)then
  begin
   //Set up the result to the number of entries
   SetLength(Result,Fbuffer[EoCL+$0A]+Fbuffer[EoCL+$0B]<<8);
   //Now iterate through to find each entry
   ptr:=0; //Pointer into the central library
   ctr:=0; //File count
   while ptr+CL<EoCL do
   begin
    //Make sure we have space, as the reported number of entries may be wrong
    if ctr+1>Length(Result) then SetLength(Result,ctr+1);
    //Read in the values
    fnL                   :=Fbuffer[CL+ptr+$1C]          //Length of filename
                           +Fbuffer[CL+ptr+$1D]<<8;
    exL                   :=Fbuffer[CL+ptr+$1E]          //Length of extra field
                           +Fbuffer[CL+ptr+$1F]<<8;
    cmL                   :=Fbuffer[CL+ptr+$20]          //Length of comment field
                           +Fbuffer[CL+ptr+$21]<<8;
    Result[ctr].DataOffset:=Fbuffer[CL+ptr+$2A]          //Offset to file header
                           +Fbuffer[CL+ptr+$2B]<< 8
                           +Fbuffer[CL+ptr+$2C]<<16
                           +Fbuffer[CL+ptr+$2D]<<24;
    Result[ctr].Size      :=Fbuffer[Result[ctr].DataOffset+$12] //Compressed size
                           +Fbuffer[Result[ctr].DataOffset+$13]<< 8
                           +Fbuffer[Result[ctr].DataOffset+$14]<<16
                           +Fbuffer[Result[ctr].DataOffset+$15]<<24;
    Result[ctr].Length    :=Fbuffer[Result[ctr].DataOffset+$16] //Uncompressed size
                           +Fbuffer[Result[ctr].DataOffset+$17]<< 8
                           +Fbuffer[Result[ctr].DataOffset+$18]<<16
                           +Fbuffer[Result[ctr].DataOffset+$19]<<24;
    //Get the internal zip name
    zipfn:='';
    for fnc:=0 to fnL-1 do zipfn:=zipfn+chr(Fbuffer[CL+ptr+$2E+fnc]);
    //Determine if this is a directory or not
    if zipfn[Length(zipfn)]='/' then
    begin
     Result[ctr].Directory:=True;
     zipfn:=LeftStr(zipfn,Length(zipfn)-1);
    end else Result[ctr].Directory:=False;
    fn:=zipfn;
    //Swap the directory separators around
    SwapDirSep(fn);
    //Split the filename and parent (without the root)
    RISCOSFilename(zipfn,false,Result[ctr].Filename,Result[ctr].Parent);
    //And remember the zip filename, for later extraction
    Result[ctr].ArchiveName:=zipfn;
    //Defaults if not RISC OS
    Result[ctr].LoadAddr  :=0;
    Result[ctr].ExecAddr  :=0;
    Result[ctr].Attributes:=0;
    //Look for the 'AR' and 'ARC0' signature in the extra field
    if exL>=20 then
    begin //RISC OS stuff
     if (Fbuffer[CL+ptr+$2E+fnL+$00]=$41)
     and(Fbuffer[CL+ptr+$2E+fnL+$01]=$43) //'AC'
     and(Fbuffer[CL+ptr+$2E+fnL+$04]=$41)
     and(Fbuffer[CL+ptr+$2E+fnL+$05]=$52)
     and(Fbuffer[CL+ptr+$2E+fnL+$06]=$43) //ARC0
     and(Fbuffer[CL+ptr+$2E+fnL+$07]=$30)then
     begin
      //The two bytes inbetween the AC and ARC0 should be exL-4
      Result[ctr].LoadAddr  :=Fbuffer[CL+ptr+$2E+fnL+$08]     //Load Address
                             +Fbuffer[CL+ptr+$2E+fnL+$09]<< 8
                             +Fbuffer[CL+ptr+$2E+fnL+$0A]<<16
                             +Fbuffer[CL+ptr+$2E+fnL+$0B]<<24;
      Result[ctr].ExecAddr  :=Fbuffer[CL+ptr+$2E+fnL+$0C]     //Execution Address
                             +Fbuffer[CL+ptr+$2E+fnL+$0D]<<8
                             +Fbuffer[CL+ptr+$2E+fnL+$0E]<<16
                             +Fbuffer[CL+ptr+$2E+fnL+$0F]<<24;
      Result[ctr].Attributes:=Fbuffer[CL+ptr+$2E+fnL+$10]     //Attributes
                             +Fbuffer[CL+ptr+$2E+fnL+$11]<<8
                             +Fbuffer[CL+ptr+$2E+fnL+$12]<<16
                             +Fbuffer[CL+ptr+$2E+fnL+$13]<<24;
     end;
    end;
    //Move the pointer onto the next file
    inc(ptr,fnL+exL+cmL+$2E);
    //And next entry
    inc(ctr);
   end;
  end;
  //Go through each entry and make sure that the parent exists for each one
  ctr:=0;
  while ctr<Length(Result) do
  begin
   //Flag to see if the parent already exists
   exists:=-1;
   //Check every entry, except blank or root
   if(Result[ctr].Parent<>'')and(Result[ctr].Parent<>'$')then
   begin
    for ptr:=0 to Length(Result)-1 do
     if Result[ptr].Directory then //If it is a directory
      if((Result[ctr].Parent=Result[ptr].Parent+'.'+Result[ptr].Filename)
      and(Result[ptr].Parent<>''))
      or((Result[ctr].Parent=Result[ptr].Filename)
      and(Result[ptr].Parent=''))then
       exists:=ptr; //And it exists, then mark it so
    //If it does exist, but is after the current entry, move it down
    if exists>ctr then
    begin
     //Remember it
     temp:=Result[exists];
     //Move from the current one to this one upwards
     for ptr:=exists downto ctr+1 do Result[ptr]:=Result[ptr-1];
     //Now put the original in this position
     Result[ctr]:=temp;
     //And move ctr up to match
     inc(ctr);
    end;
    //If it doesn't exist, then we create it just before
    if exists=-1 then
    begin
     //Make space
     SetLength(Result,Length(Result)+1);
     //Move everything up by one
     for ptr:=Length(Result)-1 downto ctr+1 do
      Result[ptr]:=Result[ptr-1];
     //Take a note
     fn:=Result[ctr].Parent;
     //Drop in our 'new' directory
     Result[ctr].Directory:=True;
     SwapDirSep(fn);
     Result[ctr].ArchiveName:=fn+'/';
     SwapDirSep(fn);
     Result[ctr].Length:=0;
     Result[ctr].Size:=0;
     Result[ctr].NumEntries:=0;
     Result[ctr].Attributes:=$0F;
     Result[ctr].DataOffset:=Length(Fbuffer);
     //And split the filename and parent
     RISCOSFilename(fn,True,Result[ctr].Filename,Result[ctr].Parent);
     //Remove the root specifier
     if Length(Result[ctr].Parent)>2 then
      Result[ctr].Parent:=Copy(Result[ctr].Parent,3)
     else
      Result[ctr].Parent:='';
     inc(ctr);
    end;
   end;
   inc(ctr);
  end;
  //Analyse the results to count the number of directory entries, per directory
  if Length(Result)>0 then
  begin
   FMaxDirEnt:=0;
   //We will need to go through each entry
   for ctr:=0 to Length(Result)-1 do
   begin
    //Reset the counter to zero, directory or no directory
    Result[ctr].NumEntries:=0;
    //If it is a directory, then look into it further
    if Result[ctr].Directory then
    begin
     //Prepare the parent name
     fn:=Result[ctr].Filename;
     if Result[ctr].Parent<>'' then
      fn:=Result[ctr].Parent+'.'+fn;
     //Now go through all entries and count the number of files that have this
     //as a parent
     for ptr:=0 to Length(Result)-1 do
      if Result[ptr].Parent=fn then inc(Result[ctr].NumEntries);
     //Update the maximum directory size
     if Result[ctr].NumEntries>FMaxDirEnt then
      FMaxDirEnt:=Result[ctr].NumEntries;
    end;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Extract the list of files (!PackDir)
-------------------------------------------------------------------------------}
function TSpark.ExtractPackFiles: TFileList;
var
 ptr        : Cardinal=0;
 fnptr      : Cardinal=0;
 ctr        : Cardinal=0;
 fn         : String='';
 dircount   : array of Integer=nil;
 dirref     : array of Integer=nil;
 parent     : Integer=0;
begin
 Result:=nil;
 if FIsPack then
 begin
  //Bit length
  FBitLength:=Fbuffer[$05]
             +Fbuffer[$06]<< 8
             +Fbuffer[$07]<<16
             +Fbuffer[$08]<<24
             +12;
  parent:=-1;
  //First entry at 09
  ptr:=$09;
  while ptr<Length(Fbuffer)-1 do
  begin
   //Add a new entry
   ctr:=Length(Result);
   SetLength(Result,ctr+1);
   //Set the parent for the initial directory
   if ctr=1 then Result[ctr].Parent:='';
   //Get the filename
   fn:='';  //Filename
   fnptr:=0;//Filename length pointer
   while Fbuffer[ptr+fnptr]<>0 do //Continue until we hit a NUL
   begin
    fn:=fn+chr(Fbuffer[ptr+fnptr]); //Add to the filename
    inc(fnptr);                     //Next character
   end;
   inc(fnptr);                      //Move the pointer past the NUL
   //Now we need to remove any directory specifier to leave just the basic
   while Pos('.',fn)>0 do fn:=Copy(fn,Pos('.',fn)+1);
   Result[ctr].FileName   :=fn;     //Update the filename
   Result[ctr].ArchiveName:=fn;     //This is not used here, so make it the same
   //Keep track of the directories
   if parent>=0 then
   begin
    //Get the parent name
    fn:=Result[dirref[parent]].Parent;
    //Is it blank, if not add the directory separator
    if fn<>'' then fn:=fn+'.';
    //Now set the parent of this entry
    Result[ctr].Parent:=fn+Result[dirref[parent]].Filename;
    //Increase the number of entries
    inc(dircount[parent]);
    //Have we reached the limit? The move up the tree until we reach one we haven't
    while(parent>0)and(dircount[parent]=Result[dirref[parent]].NumEntries) do
     dec(parent);
   end;
   //Load address
   Result[ctr].LoadAddr:=Fbuffer[ptr+fnptr+$00]
                        +Fbuffer[ptr+fnptr+$01]<< 8
                        +Fbuffer[ptr+fnptr+$02]<<16
                        +Fbuffer[ptr+fnptr+$03]<<24;
   //Exec address
   Result[ctr].ExecAddr:=Fbuffer[ptr+fnptr+$04]
                        +Fbuffer[ptr+fnptr+$05]<< 8
                        +Fbuffer[ptr+fnptr+$06]<<16
                        +Fbuffer[ptr+fnptr+$07]<<24;
   //Uncompressed file length or number of entries
   Result[ctr].Length  :=Fbuffer[ptr+fnptr+$08]
                        +Fbuffer[ptr+fnptr+$09]<< 8
                        +Fbuffer[ptr+fnptr+$0A]<<16
                        +Fbuffer[ptr+fnptr+$0B]<<24;
   //Attributes
   Result[ctr].Attributes:=Fbuffer[ptr+fnptr+$0C]
                          +Fbuffer[ptr+fnptr+$0D]<< 8
                          +Fbuffer[ptr+fnptr+$0E]<<16
                          +Fbuffer[ptr+fnptr+$0F]<<24;
   //Where to find the data (only valid for files)
   Result[ctr].DataOffset:=0;
   //Entry type (not first entry, which is a directory)
   Result[ctr].Directory:=True;
   if ctr>0 then
    Result[ctr].Directory:=Fbuffer[ptr+fnptr+$10]
                          +Fbuffer[ptr+fnptr+$11]<< 8
                          +Fbuffer[ptr+fnptr+$12]<<16
                          +Fbuffer[ptr+fnptr+$13]<<24=1;
   //If entry is a file, get the compressed data length
   Result[ctr].Size:=0;
   if not Result[ctr].Directory then
   begin
    Result[ctr].Size:=Fbuffer[ptr+fnptr+$14]
                     +Fbuffer[ptr+fnptr+$15]<< 8
                     +Fbuffer[ptr+fnptr+$16]<<16
                     +Fbuffer[ptr+fnptr+$17]<<24;
    Result[ctr].DataOffset:=ptr+fnptr+$18;
    //If it is -1, then it is uncompressed
    if Result[ctr].Size=$FFFFFFFF then Result[ctr].Size:=Result[ctr].Length;
    //No entries, as it is a file
    Result[ctr].NumEntries:=0;
   end
   else
   begin //Is a directory, so get the number of entries and reset the length
    Result[ctr].NumEntries:=Result[ctr].Length;
    Result[ctr].Length    :=0;
    //Update the parent for the next file
    parent:=Length(dircount);
    SetLength(dircount,parent+1);
    SetLength(dirref,parent+1);
    dircount[parent]:=0;   //Entry count for this directory
    dirref[parent]  :=ctr; //Reference for this directory
   end;
   //Move onto the next entry
   if ctr=0 then
    inc(ptr,Result[ctr].Size+fnptr+$10)
   else
    if not Result[ctr].Directory then
     inc(ptr,Result[ctr].Size+fnptr+$18)
    else
     inc(ptr,Result[ctr].Size+fnptr+$14);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Stream creation event handler
-------------------------------------------------------------------------------}
procedure TSpark.DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
 AStream:=TMemoryStream.Create;
end;

{-------------------------------------------------------------------------------
Stream done event handler
-------------------------------------------------------------------------------}
procedure TSpark.DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
 //Copy the contents across to the cache
 AStream.Position:=0;
 SetLength(Fcache,AStream.Size);
 AStream.Read(Fcache[0],AStream.Size);
 //Free up the stream.
 Astream.Free;
end;

{-------------------------------------------------------------------------------
Extract, and decompress, the actual data
-------------------------------------------------------------------------------}
function TSpark.ExtractFileData(Index: Integer):TDIByteArray;
begin
 Result:=nil;
 //Make sure we are within limits
 if(index>=0)and(index<Length(FFileList))then
 begin
  if FIsSpark then Result:=ExtractFileDataFromSpark(index);
  if FIsPack  then Result:=ExtractFileDataFromPack(index);
 end;
end;

{-------------------------------------------------------------------------------
Extract, and decompress, the actual data (!Spark)
-------------------------------------------------------------------------------}
function TSpark.ExtractFileDataFromSpark(index: Integer):TDIByteArray;
var
 ZipFile   : TUnZipper=nil;
 sl        : TStringList=nil;
 starttime,
 nowtime   : TDateTime;
 temp      : Boolean=False;
begin
 Result:=nil;
 if FIsSpark then
 begin
  //Save the file, if we have no filename
  if ZipFileName='' then
  begin
   ZipFileName:=GetTempFileName;
   SaveData;
   temp:=True;
  end;
  SetLength(Fcache,0);
  //And there is some data to decompress
  if FFileList[Index].Size>0 then
  begin
   //Create a string list
   sl:=TStringList.Create;
   //And populate it with the filename
   sl.Add(FFileList[Index].ArchiveName);
   //And create the Unzipper
   ZipFile:=TUnZipper.Create;
   try
    //Set the filename
    ZipFile.FileName       :=ZipFileName;
    //And event handlers
    ZipFile.OnCreateStream :=@DoCreateOutZipStream;
    ZipFile.OnDoneStream   :=@DoDoneOutZipStream;
    //Including the progress update handler, if the user has assigned one
    if Assigned(FProgress) then
     ZipFile.OnProgress:=FProgress;
    //Now send the list of one filename to the unzipper method
    ZipFile.UnZipFiles(sl);
    //Create a 'TimeOut' timer
    starttime:=Round(Time*100000);
    //TDateTime is a Double, with the time being the fraction part
    //So multiplying by 100000 gets the number of seconds
    nowtime:=starttime;
    repeat
     //Wait until the cache gets filled up, or the timer expires
     nowtime:=Round(Time*100000);
    until(Length(Fcache)>0)or(nowtime-starttime>=FTimeOut);
    //Copy the cache to the result
    Result:=Fcache;
    //And empty the cache
    SetLength(Fcache,0);
   finally
    //And finally free up the unzipper and string list
    ZipFile.Free;
    sl.Free;
   end;
  end;
  //Delete the temporary file
  if temp then
  begin
   DeleteFile(ZipFileName);
   ZipFileName:='';
  end;
 end;
end;

{-------------------------------------------------------------------------------
Extract, and decompress, the actual data (!PackDir)
-------------------------------------------------------------------------------}
function TSpark.ExtractFileDataFromPack(index: Integer):TDIByteArray;
var
 buffer     : TDIByteArray=nil;
 i          : Cardinal=0;
 max        : Cardinal=0;
 cc         : Cardinal=0;
 codesize   : Cardinal=0;
 codemask   : Cardinal=0;
 nextfree   : Cardinal=0;
 prev_code  : Cardinal=0;
 bitbuffer  : Cardinal=0;
 bitsremain : Cardinal=0;
 outptr     : Cardinal=0;
 eoi        : Cardinal=0;
 code       : Cardinal=0;
 save_code  : Cardinal=0;
 temp_code  : Cardinal=0;
 firstchar  : Byte=0;
 stackptr   : Cardinal=0;
 prefix     : array of Cardinal=nil;
 suffix     : TDIByteArray=nil;
 stack      : TDIByteArray=nil;
const lzwbits = 8;
begin
 Result:=nil;
 if FIsPack then
 begin
  //Set the buffer and fill it with the compressed data
  SetLength(buffer,FFileList[index].Size);
  for i:=0 to Length(buffer)-1 do
   buffer[i]:=Fbuffer[FFileList[index].DataOffset+i];
  //Set up the output buffer
  SetLength(Result,FFileList[index].Length);
  //Is the data actually compressed?
  if Length(Result)=Length(buffer) then
   for i:=0 to Length(buffer)-1 do
    Result[i]:=buffer[i] //If not, then just return the data as is.
  else //Otherwise we need to decompress it
 //Code converted from C. Original code can be found at:
 //https://theblackzone.net/posts/2019/unpacking-packdir-files-on-linux/pkdir.zip
  begin
   //The maximum number of codes is derived from the bits used. For example
   //13 bits results in 1<<13=8192 codes.
   max:=1<<FBitLength;
   //Allocate memory for code tables
   SetLength(prefix,max+1);
   SetLength(suffix,max+1);
   SetLength(stack,max+1);
   codesize:=lzwbits+1;
   codemask:=(1<<codesize)-1;
   cc:=1<<lzwbits;
   eoi:=cc+1;
   nextfree:=cc+2;
   firstchar:=0;
   //Initialise tables
   for i:=0 to cc do
   begin
    prefix[i]:=0;
    suffix[i]:=i;
    stack[i]:=0;
   end;
   i:=0;
   prev_code:=$FFFFFFFF;
   bitbuffer:=0;
   bitsremain:=0;
   stackptr:=0;
   outptr:=0;
   while i<Length(buffer) do
   begin
    bitbuffer:=bitbuffer or (buffer[i]<<bitsremain);
    inc(bitsremain,lzwbits);
    inc(i);
    //Decode while there are enough bits in the buffer
    while bitsremain>codesize do
    begin
     code:=bitbuffer and codemask;
     dec(bitsremain,codesize);
     bitbuffer:=bitbuffer>>codesize;
     //Is it a CLEAR code? Reinitialise
     if code=cc then
     begin
      codesize:=lzwbits+1;
      codemask:=(1<<codesize)-1;
      cc:=1<<lzwbits;
      eoi:=cc+1;
      nextfree:=cc+2;
      prev_code:=$FFFFFFFF;
      continue;
     end;
     //Handle EOI code
     if code=eoi then
     begin
      i:=Length(buffer); //Force end of main loop
      break;
     end;
     //First code of the stream? Direct output
     if prev_code=$FFFFFFFF then
     begin
      if outptr<Length(Result) then
      begin
       Result[outptr]:=suffix[code];
       inc(outptr);
      end;
      prev_code:=code;
      firstchar:=code;
      continue;
     end;
     //Decoding part
     save_code:=code;
     if code>=nextfree then
     begin
      stack[stackptr]:=firstchar;
      inc(stackptr);
      code:=prev_code;
     end;
     while code>cc do
     begin
      temp_code:=code;
      if code=prefix[code] then exit; //Error
      stack[stackptr]:=suffix[code];
      inc(stackptr);
      code:=prefix[code];
      if temp_code=prefix[code] then exit; //Error
     end;
     firstchar:=suffix[code];
     stack[stackptr]:=suffix[code];
     inc(stackptr);
     if nextfree<max then
     begin
      prefix[nextfree]:=prev_code;
      suffix[nextfree]:=firstchar;
      inc(nextfree);
      //Increase the code size if all codes for the current bit length are used
      if((nextfree AND codemask)=0)AND(nextfree<max)then
      begin
       inc(codesize);
       inc(codemask,nextfree);
      end;
     end;
     prev_code:=save_code;
     //Output what has been accumulated on the stack
     while stackptr>0 do
     begin
      dec(stackptr);
      if outptr<Length(Result) then
      begin
       Result[outptr]:=stack[stackptr];
       inc(outptr);
      end;
     end;
    end;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Save the buffer data to disc
-------------------------------------------------------------------------------}
procedure TSpark.SaveData;
var
 tempfile: TFileStream=nil;
begin
 if ZipFilename<>'' then
 begin
  tempfile:=TFileStream.Create(ZipFilename,fmOpenWrite or fmShareDenyNone);
  tempfile.Position:=0;
  tempfile.Write(Fbuffer[0],Length(Fbuffer));
  tempfile.Size:=Length(Fbuffer);//Ensures the file is the correct length
  tempfile.Free;
 end;
end;

{-------------------------------------------------------------------------------
Find a file entry in the central library and main header
-------------------------------------------------------------------------------}
function TSpark.FindEntry(path: String;matchpath: Boolean;var CLptr: Cardinal;
                      var dataptr: Cardinal;var LBuffer: TDIByteArray): Boolean;
var
 CL    : Integer=-1;
 EoCL  : Integer=-1;
 temp  : String='';
 fnL   : Word=0;
 index : Integer=0;
begin
 Result:=False;
 //Get the location of the central library
 CL:=FindCL(EoCL,Lbuffer);
 //If it exists
 if(CL<>EoCL)and(CL<>-1)and(EoCL<Length(Lbuffer))then
 begin
  //Find the entry in the Central Library (and, hence, the main header)
  CLptr:=CL;
  temp:='';
  while(temp<>path)and(CLptr+$2E<EoCL)do
  begin
   if (Lbuffer[CLptr]=$50)       //Entry signature
   and(Lbuffer[CLptr+1]=$4B)
   and(Lbuffer[CLptr+2]=$01)
   and(Lbuffer[CLptr+3]=$02)then
   begin
    //Filename length
    if matchpath then fnL:=Length(path) //Either match the path
    else fnL:=Lbuffer[CLptr+$1C]+Lbuffer[CLptr+$1D]<<8; //Or the given length
    //Get the location in the main data area
    dataptr:=Lbuffer[CLptr+$2A]
            +Lbuffer[CLptr+$2B]<<8
            +Lbuffer[CLptr+$2C]<<16
            +Lbuffer[CLptr+$2D]<<24;
    temp:=''; //Build the filename
    if CLptr+$2E+fnL<Length(Lbuffer) then
     for index:=0 to fnL-1 do temp:=temp+chr(Lbuffer[CLptr+$2E+index]);
   end;
   inc(CLptr); //Next byte
  end;
  //If we found it, move ptr back and return a positive
  if temp=path then
  begin
   dec(CLptr);
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Rename, or move, a file or directory (internal)
-------------------------------------------------------------------------------}
function TSpark.RenameTheFile(oldpath, newpath: String): Boolean;
var
 ptr     : Cardinal=0;
 dataptr : Cardinal=0;
 EoCL    : Integer=-1;
 CL      : Integer=-1;
 hdrpos  : Cardinal=0;
 index   : Integer=0;
 fnL     : Integer=0;
 diff    : Integer=0;
begin
 Result:=False;
 if FIsPack then exit;
 //No blanks
 if(oldpath<>'')and(newpath<>'')then
 begin
  //We need to make sure that if we are renaming a directory, both have the '/'
  if(oldpath[Length(oldpath)]='/')and(newpath[Length(newpath)]<>'/')then
   newpath:=newpath+'/';
  if(oldpath[Length(oldpath)]<>'/')and(newpath[Length(newpath)]='/')then
   oldpath:=oldpath+'/';
  //Make sure that the root is not part of either path
  if Length(oldpath)>2 then
   if oldpath[1]='$' then oldpath:=Copy(oldpath,3);
  if Length(newpath)>2 then
   if newpath[1]='$' then newpath:=Copy(newpath,3);
  //Is there any data, both paths have something, and are not the same
  if(Length(Fbuffer)>0)and(oldpath<>newpath)then
  begin
   //Get the Central Library markers
   CL:=FindCL(EoCL,Fbuffer);
   //And the difference between the two names
   diff:=Length(newpath)-Length(oldpath); //+ve move forwards, -ve move back
   //Find the entry in the main header
   ptr:=0;
   dataptr:=0;
   if FindEntry(oldpath,true,ptr,dataptr,FBuffer) then
   begin
    //Filename length
    fnL:=Fbuffer[dataptr+$1A]+Fbuffer[dataptr+$1B]<<8;
    //New name is bigger?
    if diff>0 then
    begin
     //Extend
     SetLength(Fbuffer,Length(Fbuffer)+diff);
     for index:=Length(Fbuffer)-1 downto dataptr+$1E+Length(oldpath) do
      Fbuffer[index]:=Fbuffer[index-diff];
    end;
    //New name is shorter?
    if diff<0 then
    begin
     //Contract
     for index:=dataptr+$1E+Length(oldpath) to Length(Fbuffer)-1 do
      Fbuffer[index+diff]:=Fbuffer[index];
     SetLength(Fbuffer,Length(Fbuffer)+diff);
    end;
    //Update the filename
    for index:=1 to Length(newpath) do Fbuffer[dataptr+$1D+index]:=Ord(newpath[index]);
    //Update the filename length
    Fbuffer[dataptr+$1A]:=(fnL+diff)mod$100;
    Fbuffer[dataptr+$1B]:=(fnL+diff)>>8;
    //Update the central library markers
    inc(EoCL,diff);
    inc(CL,diff);
    UpdateCL(CL,EoCL,Fbuffer);
     //New name is bigger?
     if diff>0 then
     begin
      //Extend
      SetLength(Fbuffer,Length(Fbuffer)+diff);
      for index:=Length(Fbuffer)-1 downto ptr+$2E+Length(oldpath) do
       Fbuffer[index]:=Fbuffer[index-diff];
     end;
     //New name is shorter?
     if diff<0 then
     begin
      //Contract
      for index:=ptr+$2E+Length(oldpath) to Length(Fbuffer)-1 do
       Fbuffer[index+diff]:=Fbuffer[index];
      SetLength(Fbuffer,Length(Fbuffer)+diff);
     end;
     //Update the filename
     for index:=1 to Length(newpath) do Fbuffer[ptr+$2D+index]:=Ord(newpath[index]);
     //Update the filename length
     Fbuffer[ptr+$1C]:=(fnL+diff)mod$100;
     Fbuffer[ptr+$1D]:=(fnL+diff)>>8;
     //Update the data offsets for all the subsequent entries
     for index:=ptr+1 to EoCL+diff do
      if (Fbuffer[index]=$50)       //Entry signature
      and(Fbuffer[index+1]=$4B)
      and(Fbuffer[index+2]=$01)
      and(Fbuffer[index+3]=$02)then
      begin
       hdrpos:=Fbuffer[index+$2A]       //Get the current
              +Fbuffer[index+$2B]<<8
              +Fbuffer[index+$2C]<<16
              +Fbuffer[index+$2D]<<24;
       inc(hdrpos,diff);                //Adjust
       Fbuffer[index+$2A]:=hdrpos mod$100;//Put back
       Fbuffer[index+$2B]:=(hdrpos>>8) mod$100;
       Fbuffer[index+$2C]:=(hdrpos>>16) mod$100;
       Fbuffer[index+$2D]:=(hdrpos>>24) mod$100;
      end;
     //Update the central library markers, again
     inc(EoCL,diff);
     UpdateCL(CL,EoCL,Fbuffer);
     //Save the data
     SaveData;
     Result:=True;
     //Update the entry in the list of files
     if Length(FFileList)>0 then
      for index:=0 to Length(FFileList)-1 do
       if((Copy(FFileList[index].ArchiveName,1,Length(oldpath))=oldpath)
       and(oldpath[Length(oldpath)]='/'))
       or(FFileList[index].ArchiveName=oldpath)then
       begin
        //Name as held in the archive
        if FFileList[index].ArchiveName=oldpath then
         FFileList[index].ArchiveName:=newpath
        else
         FFileList[index].ArchiveName:=newpath
                              +Copy(FFileList[index].ArchiveName,Length(oldpath));
        RISCOSFilename(newpath,True,FFileList[index].Filename,FFileList[index].Parent);
       end;
    //If it is a directory then we recurse until they are all changed
    if oldpath[Length(oldpath)]='/' then
     while Result do Result:=RenameFile(oldpath,newpath);
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Delete a file/directory (internal)
-------------------------------------------------------------------------------}
function TSpark.DeleteTheFile(filename: String):Boolean;
var
 EoCL    : Integer=-1;
 CL      : Integer=-1;
 hdrsize : Cardinal=0;
 clsize  : Cardinal=0;
 fnL     : Cardinal=0;
 exL     : Cardinal=0;
 cmL     : Cardinal=0;
 ptr     : Cardinal=0;
 CLptr   : Cardinal=0;
 data    : Cardinal=0;
 index   : Cardinal=0;
 match   : Boolean=False;
begin
 //Default return - a false result does not mean it is a fail.
 Result:=False;
 //Remove the root, if present
 if Length(filename)>2 then if filename[1]='$' then filename:=Copy(filename,3);
 //Find the Central library
 CL:=FindCL(EoCL,Fbuffer);
 if(EoCL<>CL)and(CL<>-1)then
 begin
  //Find the entry in both the CL and header
  if filename[Length(filename)]='/' then match:=True else match:=False;
  if FindEntry(filename,match,CLptr,ptr,FBuffer) then
  begin
   //Move the data following the entry in the header down
   fnL:=Fbuffer[ptr+$1A]+Fbuffer[ptr+$1B]<<8; //Filename length
   exL:=Fbuffer[ptr+$1C]+Fbuffer[ptr+$1D]<<8; //Extra length
   hdrsize:=$1E+fnL+exL
           +Fbuffer[ptr+$12]
           +Fbuffer[ptr+$13]<<8
           +Fbuffer[ptr+$14]<<16
           +Fbuffer[ptr+$15]<<24;             //Total size of entry in the header
   for index:=ptr+hdrsize to Length(Fbuffer)-1 do
    Fbuffer[index-hdrsize]:=Fbuffer[index];
   //Recalculate, and update, the EoCL
   dec(CL,hdrsize);
   dec(EoCL,hdrsize);
   UpdateCL(CL,EoCL,Fbuffer);
   SetLength(Fbuffer,Length(Fbuffer)-hdrsize);
   //Move the data following the entry in the CL down, updating the data pointers
   dec(CLptr,hdrsize);
   cmL:=Fbuffer[CLptr+$20]+Fbuffer[CLptr+$21]<<8;
   CLsize:=$2E+fnL+exL+cmL;
   for index:=CLptr+CLsize to Length(Fbuffer)-1 do
   begin
    //Update the data pointers for the other entries
    if (Fbuffer[index]=$50)
    and(Fbuffer[index+1]=$4B)
    and(Fbuffer[index+2]=$01)
    and(Fbuffer[index+3]=$02)then
    begin
     //Read in
     data:=Fbuffer[index+$2A]
          +Fbuffer[index+$2B]<<8
          +Fbuffer[index+$2C]<<16
          +Fbuffer[index+$2D]<<24;
     //Adjust
     dec(data,hdrsize);
     //Put back
     Fbuffer[index+$2A]:=data mod$100;
     Fbuffer[index+$2B]:=(data>>8)mod$100;
     Fbuffer[index+$2C]:=(data>>16)mod$100;
     Fbuffer[index+$2D]:=(data>>24)mod$100;
    end;
    //And move the data down
    Fbuffer[index-CLsize]:=Fbuffer[index];
   end;
   //Recalculate, and update, the EoCL and CL size
   dec(EoCL,CLsize);
   UpdateCL(CL,EoCL,Fbuffer);
   SetLength(Fbuffer,Length(Fbuffer)-CLsize);
   //Recalculate, and update, the number of entries in the CL
   data:=0;
   for index:=CL to EoCL do
    if (Fbuffer[index]=$50)
    and(Fbuffer[index+1]=$4B)
    and(Fbuffer[index+2]=$01)
    and(Fbuffer[index+3]=$02)then inc(data);
   //Number of entries
   Fbuffer[EoCL+$8]:=data mod$100;
   Fbuffer[EoCL+$9]:=(data>>8)mod$100;
   Fbuffer[EoCL+$A]:=data mod $100;
   Fbuffer[EoCL+$B]:=(data>>8)mod$100;
   //Save the data
   SaveData;
   //Remove it from the list of files
   if Length(FFileList)>0 then
   begin
    for index:=0 to Length(FFileList)-1 do
     if FFileList[index].ArchiveName=filename then
      ptr:=index;
    if ptr<Length(FFileList)-1 then
     for index:=ptr to Length(FFileList)-2 do
      FFileList[index]:=FileList[index+1];
    SetLength(FFileList,Length(FFileList)-1);
   end;
   //If all OK, set a +ve result
   Result:=True;
   //Call the function again with the same parameters, if it is a directory
   if filename[Length(filename)]='/' then
    while Result do Result:=DeleteFile(filename);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Get the total size of all files, uncompressed
-------------------------------------------------------------------------------}
function TSpark.GetUncompressedSize: Cardinal;
var
 i: Cardinal=0;
begin
 Result:=0;
 if Length(FFileList)>0 then
  for i:=0 to Length(FFileList)-1 do
   inc(Result,FFileList[i].Length);
end;

{-------------------------------------------------------------------------------
Is it a valid file we can handle?
-------------------------------------------------------------------------------}
function TSpark.IsItSpark: Boolean;
begin
 Result:=FIsSpark OR FIsPack;
end;

{-------------------------------------------------------------------------------
Finds the central library and returns the end of central library
-------------------------------------------------------------------------------}
function TSpark.FindCL(var EoCL: Integer;var buffer: TDIByteArray): Integer;
var
 i: Integer=0;
begin
 //Start with a default (i.e., not found)
 Result:=-1;
 EoCL  :=-1;
 //Can't have a file smaller than 22 bytes
 if Length(buffer)>=22 then
 begin
  //Start here
  i:=Length(buffer)-3;
  //And work backwards until we find the EoCL marker
  repeat
    dec(i);
  until((buffer[i  ]=$50)
     and(buffer[i+1]=$4B)
     and(buffer[i+2]=$05)
     and(buffer[i+3]=$06))
     or (i=0);
  //Found OK?
  if (buffer[i  ]=$50)
  and(buffer[i+1]=$4B)
  and(buffer[i+2]=$05)
  and(buffer[i+3]=$06)
  and(i+$13<Length(buffer))then
  begin
   //Mark it
   EoCL:=i;
   //Retreive where the central library starts
   Result:=buffer[i+$10]
          +buffer[i+$11]<<8
          +buffer[i+$12]<<16
          +buffer[i+$13]<<24;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update the Central Library pointer, with EoCL having already been moved
-------------------------------------------------------------------------------}
procedure TSpark.UpdateCL(CL,EoCL: Cardinal; var buffer: TDIByteArray);
var
 ptr : Cardinal=0;
begin
 if(EoCL<>0)and(CL<>0)then
 begin
  //Work out the length
  ptr:=EoCL-CL;
  //And write it
  buffer[EoCL+$0C]:= ptr     mod$100;
  buffer[EoCL+$0D]:=(ptr>> 8)mod$100;
  buffer[EoCL+$0E]:=(ptr>>16)mod$100;
  buffer[EoCL+$0F]:=(ptr>>24)mod$100;
  //And the location of the central library
  buffer[EoCL+$10]:= CL     mod$100;
  buffer[EoCL+$11]:=(CL>> 8)mod$100;
  buffer[EoCL+$12]:=(CL>>16)mod$100;
  buffer[EoCL+$13]:=(CL>>24)mod$100;
 end;
end;

{-------------------------------------------------------------------------------
Merges two ZIP files
-------------------------------------------------------------------------------}
function TSpark.CombineZIP(files: array of String; outputfile: String): Boolean;
var
 input     : TFileStream=nil;
 output    : TFileStream=nil;
 inbuffer  : array[0..1] of TDIByteArray=(nil,nil);
 outbuffer : TDIByteArray=nil;
 ptr       : Integer=0;
 cnt       : Integer=0;
 fileptr   : Cardinal=0;
 CL        : array[0..1] of Integer=(-1,-1);
 EoCL      : array[0..1] of Integer=(-1,-1);
 temp      : Cardinal=0;
 numfiles  : Cardinal=0;
 CLsize    : Cardinal=0;
 filesize  : Cardinal=0;
begin
 Result:=False;
 if Length(files)>=2 then //Only works with two files, the rest are ignored
 begin
  Result:=True;
  //Read in the files
  for ptr:=0 to 1 do
  begin
   input:=TFileStream.Create(files[ptr],fmOpenRead OR fmShareDenyNone);
   input.Position:=0;
   SetLength(inbuffer[ptr],input.Size);
   input.Read(inbuffer[ptr][0],input.Size);
   input.Free;
   //Get the position of the central library for each
   CL[ptr]:=FindCL(EoCL[ptr],inbuffer[ptr]);
   Result:=(Result)AND(CL[ptr]<>-1)AND(EoCL[ptr]<>-1);
   //Count the number of files stored
   if EoCL[ptr]<>-1 then
    inc(numfiles,inbuffer[ptr][EoCL[ptr]+$A]+inbuffer[ptr][EoCL[ptr]+$B]<<8);
  end;
  //Create the output file
  if Result then
  begin
   //This will be the eventual central library size
   CLsize:=(EoCL[0]-CL[0])+(EoCL[1]-CL[1]);
   //This will be the eventual file size
   filesize:=CL[0]+CL[1]+CLsize+22;
   SetLength(outbuffer,filesize);
   //Write the files. The files from the second ZIP goes where the first CL was
   fileptr:=0;
   for cnt:=0 to 1 do
   begin
    for ptr:=0 to CL[cnt]-1 do outbuffer[fileptr+ptr]:=inbuffer[cnt][ptr];
    inc(fileptr,CL[cnt]);
   end;
   //Write the CLs
   for cnt:=0 to 1 do
   begin
    //We'll need to find each file entry and adjust by adding CL1 to the adddress
    for ptr:=CL[cnt] to EoCL[cnt]-1 do
    begin
     outbuffer[fileptr-CL[cnt]+ptr]:=inbuffer[cnt][ptr];
     if(cnt>0)and(ptr>$2D)then
      //Found a file?
      if (inbuffer[cnt][ptr-$2E]=$50)
      and(inbuffer[cnt][ptr-$2D]=$4B)
      and(inbuffer[cnt][ptr-$2C]=$01)
      and(inbuffer[cnt][ptr-$2B]=$02)then
      begin
       //Get the data offset
       temp:=inbuffer[cnt][ptr-4]
            +inbuffer[cnt][ptr-3]<<8
            +inbuffer[cnt][ptr-2]<<16
            +inbuffer[cnt][ptr-1]<<24;
       //Adjust the data offset
       inc(temp,CL[cnt-1]);
       //Save back
       outbuffer[(fileptr-CL[cnt]+ptr)-4]:= temp AND $000000FF;
       outbuffer[(fileptr-CL[cnt]+ptr)-3]:=(temp AND $0000FF00)>>8;
       outbuffer[(fileptr-CL[cnt]+ptr)-2]:=(temp AND $00FF0000)>>16;
       outbuffer[(fileptr-CL[cnt]+ptr)-1]:=(temp AND $FF000000)>>24;
      end;
    end;
    inc(fileptr,EoCL[cnt]-CL[cnt]);
   end;
   //Write the central directory
   fileptr:=filesize-22;
   //Signature
   outbuffer[fileptr    ]:=$50;
   outbuffer[fileptr+$01]:=$4B;
   outbuffer[fileptr+$02]:=$05;
   outbuffer[fileptr+$03]:=$06;
   //Number of files on this disc
   outbuffer[fileptr+$08]:= numfiles AND $00FF;
   outbuffer[fileptr+$09]:=(numfiles AND $FF00)>>8;
   //Total number of files
   outbuffer[fileptr+$0A]:= numfiles AND $00FF;
   outbuffer[fileptr+$0B]:=(numfiles AND $FF00)>>8;
   //Central Library size
   outbuffer[fileptr+$0C]:= CLsize AND $000000FF;
   outbuffer[fileptr+$0D]:=(CLsize AND $0000FF00)>>8;
   outbuffer[fileptr+$0E]:=(CLsize AND $00FF0000)>>16;
   outbuffer[fileptr+$0F]:=(CLsize AND $FF000000)>>24;
   //Data size
   outbuffer[fileptr+$10]:= (CL[0]+CL[1]) AND $000000FF;
   outbuffer[fileptr+$11]:=((CL[0]+CL[1]) AND $0000FF00)>>8;
   outbuffer[fileptr+$12]:=((CL[0]+CL[1]) AND $00FF0000)>>16;
   outbuffer[fileptr+$13]:=((CL[0]+CL[1]) AND $FF000000)>>24;
   //Save the data to a file
   output:=TFileStream.Create(outputfile,fmCreate OR fmShareDenyNone);
   output.Position:=0;
   output.Write(outbuffer[0],Length(outbuffer));
   output.Free;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Writes a ZIP header to the buffer
-------------------------------------------------------------------------------}
procedure TSpark.WriteSignature(header: Byte;var buffer: TDIByteArray;ptr: Cardinal=0);
var
 index: Integer;
begin
 if Length(buffer)>3 then
  for index:=0 to 3 do
   case header of
    ZIPCL    : buffer[ptr+index]:=clsig[index];
    ZIPHeader: buffer[ptr+index]:=headersig[index];
    ZIPEoCL  : buffer[ptr+index]:=eoclsig[index];
    ZIPSpan  : buffer[ptr+index]:=spansig[index];
   end;
end;
