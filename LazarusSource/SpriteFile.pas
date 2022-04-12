unit SpriteFile;

{
TSpriteFile class V1.04 written by Gerald Holdsworth
Class to load and convert RISC OS sprites into Windows Bitmap and PNG.

Copyright (C) 2018-2022 Gerald Holdsworth gerald@hollypops.co.uk

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

{$mode objfpc}{$H+}

interface

uses
 Classes,SysUtils,Graphics,Math,Dialogs,FPImage,IntfGraphics,GraphType,StrUtils;

{$M+}

type
 TDynByteArray = array of Byte;
 TProgressProc = procedure(Fupdate: Integer) of Object;
 TSprite = record
  Name          : String;                   // Name of the sprite
  Offset,                                   // Offset of this sprite
  Next,                                     // Offset to next sprite
  WidthWord,                                // Width of sprite in words - 1
  ScanLines,                                // Height of sprite in scan lines -1
  LeftBit,                                  // Left hand wastage (zero in new format)
  RightBit,                                 // Right hand wastage
  ModeData,                                 // Raw mode data
  PixelData,                                // Offset to pixel data
  Transparency,                             // Offset to mask (or pixel data if none)
  PixWidth,                                 // Width of sprite in pixels (calculated)
  PaletteColours: Cardinal;                 // Mask colour
  HasPalette    : Boolean;                  // Does sprite have a palette?
  ModeFlag,                                 // Mode flag (calculated)
  SpriteType,                               // Sprite type (calculated)
  BPP,                                      // Bits per pixel (calculated)
  OS,                                       // Risc OS compatibility
  PaletteType,                              // What type of palette? Yes (1), No (0) or Partial (2)
  TransFormat   : Byte;                     // Old (1), New (2), Wide (3) or no (0) mask format (calculated)
  Palette       : TDynByteArray;            // Palette (if not in file, loaded from resource or built from pixel data)
  Image         : TBitmap;                  // Actual bitmap image
  PNG           : TPortableNetworkGraphic;  // PNG of image
  Mask          : array of TDynByteArray;   // Transparency Mask
 end;
 TSpriteFile = class
 private
  FSpritePool  : TDynByteArray;//The actual sprite file data
  FSpriteFile  : String;       //Filename of the sprite file
  FProgress    : TProgressProc;//Used for feedback
  FSpriteCount,                //Number of sprites
  FFirstSprite,                //Pointer to first sprite
  FLastWord    : Cardinal;     //Pointer to last free word in file
  function ReadSpriteFile(var data: TDynByteArray): Integer;
  function GetSpriteName(ptr: Cardinal): String;
  function ReadSpriteHeaderFromPool(ptr: Cardinal): TSprite;
  function ReadSpriteFromPool(ptr: Cardinal): TSprite;
  function SavePaletteFile(Afilename: String;spritedata: TSprite):Boolean;
  procedure UpdateProgress(Fupdate: Integer);
  function IsValidImageFile(filename: String):Boolean;
  procedure UpdateHeader;
  function DecodeModeFlags(modeflag: Byte): String;
  function DecodeSpriteType(spritetype: Byte): String;
  function DecodeMaskType(transformat: Byte): String;
  function DecodeOS(os: Byte): String;
  function DecodePaletteType(pal: Byte): String;
  function DeleteSpriteFromPool(ptr: Cardinal): Boolean;
  {$INCLUDE 'SpriteFilePalettes.pas'}
 published
  //Methods
  constructor Create;
  function LoadSpriteFile(Afilename: String):Byte;
  function LoadSpriteFileFromStream(f: TStream): Byte;
  function SaveSpriteFile(Afilename: String):Boolean;
  function ReadSprite(sprite: Integer;headeronly: Boolean=False): TSprite;
  function ReadSprite(spritename: String;headeronly: Boolean=False): TSprite; overload;
  function SavePaletteFile(Afilename: String;sprite: Integer):Boolean; overload;
  function SavePaletteFile(Afilename,spritename: String):Boolean; overload;
  function ModeFlag(spritenumber: Integer): String;
  function ModeFlag(spritename: String): String; overload;
  function SpriteType(spritenumber: Integer): String;
  function SpriteType(spritename: String): String; overload;
  function MaskFormat(spritenumber: Integer): String;
  function MaskFormat(spritename: String): String; overload;
  function OS(spritenumber: Integer): String;
  function OS(spritename: String): String; overload;
  function PaletteType(spritenumber: Integer): String;
  function PaletteType(spritename: String): String; overload;
  function DeleteSprite(spritename: String): Boolean;
  function DeleteSprite(sprite: Integer): Boolean; overload;
  function ImportImage(filename: String;arthur: Boolean=False): Boolean;
  function IsValidSpriteName(name: String): Boolean;
  procedure CloseCurrentFile;
  function SpriteName(sprite: Integer): String;
  function RenameSprite(oldname,newname: String): Boolean;
  //Properties
  property SpriteFile       : String   read FSpriteFile;
  property ProgressIndicator: TProgressProc write FProgress;
  property SpriteCount      : Cardinal read FSpriteCount;
  property FirstSprite      : Cardinal read FFirstSprite;
  property LastFreeWord     : Cardinal read FLastWord;
 public
  destructor Destroy; override;
 end;

implementation

{-------------------------------------------------------------------------------
Reads a newly opened sprite file and adds the sprites to the sprite pool
-------------------------------------------------------------------------------}
function TSpriteFile.ReadSpriteFile(var data:TDynByteArray): Integer;
var
  x,t,c,i      : Integer;
  name,
  newname      : String;
  ptr,
  LSpriteCount,
  LFirstSprite,
  LLastWord,
  LNext        : Cardinal;
begin
 //Set up the variables
 Result:=0;
 //Get the number of sprites
 LSpriteCount:=(data[$03]<<24)
              +(data[$02]<<16)
              +(data[$01]<<8)
              + data[$00];
 //Get the position of the first sprite
 LFirstSprite:=(data[$07]<<24)
              +(data[$06]<<16)
              +(data[$05]<<8)
              + data[$04];
 dec(LFirstSprite,4); //We decrease each variable by four, as we don't have the initial word
 ptr:=LFirstSprite;
 //Get the position of the last free word - this is the end of the file
 LLastWord:=(data[$0B]<<24)
           +(data[$0A]<<16)
           +(data[$09]<<8)
           + data[$08];
 dec(LLastWord,4);
 if(ptr>Length(data))or(LLastWord>Length(data))then //Invalid Sprite File
 begin
  Result:=1; //First sprite pointer, or last free word, is beyond sprite pool
  exit;
 end;
 //Confirm the sprite data is OK, and add each one to the sprite pool
 for x:=0 to LSpriteCount-1 do
 begin
  //Pointer to next sprite
  if ptr+3<Length(data) then
   LNext:=(data[ptr+$03]<<24)
         +(data[ptr+$02]<<16)
         +(data[ptr+$01]<<8)
         + data[ptr+$00]
  else
   LNext:=LLastWord;
  if ptr+LNext<=LLastWord then //Sprite pointer OK
  begin
   //Sprite name
   name:='';
   for t:=0 to 11 do
   begin
    c:=data[ptr+$04+t]AND$7F;//macOS doesn't like top bit set characters
    if(c>0)and(c<32)then c:=c OR $20;//Remove any control characters
    if c>0 then name:=name+chr(c);
   end;
   //Check it is OK, or alter if is the same as another already in the pool
   name:=LowerCase(LeftStr(name,12));
   i:=0;
   newname:=name;
   while not IsValidSpriteName(newname) do
   begin
    inc(i);
    newname:=RightStr(name+IntToStr(i),12);
   end;
   name:=newname;
   //Write it back to the data
   for t:=0 to 11 do
   begin
    c:=0;
    if t<Length(name) then
     c:=Ord(name[t+1]);
    data[ptr+$04+t]:=c;
   end;
   //Now copy the data to the pool
   if Length(FSpritePool)=0 then
   begin
    SetLength(FSpritePool,FFirstSprite); //First sprite, so setup the header area
    FLastWord:=Length(FSpritePool);
   end;
   //Make space for it in the pool
   SetLength(FSpritePool,Length(FSpritePool)+LNext);
   //Copy it across
   for t:=0 to LNext-1 do
    FSpritePool[FLastWord+t]:=data[ptr+t];
   //Update the last free word
   inc(FLastWord,LNext);
   //And update the sprite counter
   inc(FSpriteCount);
  end;
  //Next sprite
  inc(ptr,LNext);
  //Update the progress indicator
  UpdateProgress(Round((x/LSpriteCount)*100));
 end;
 //Finally, update the sprite pool header
 UpdateHeader;
end;

{-------------------------------------------------------------------------------
Read a sprites name, given sprite pointer
-------------------------------------------------------------------------------}
function TSpriteFile.GetSpriteName(ptr: Cardinal): String;
var
  t,c: Integer;
begin
 Result:='';
 for t:=0 to 11 do
 begin
  c:=FSpritePool[ptr+$04+t]AND$7F;//macOS doesn't like top bit set characters
  if(c>0)and(c<32)then c:=c OR$20;//Remove any control characters
  if c>0 then Result:=Result+chr(c);
 end;
 //Still no spritename? Can't save it to disc.
 if Result='' then Result:='Sprite'+IntToHex(ptr,6);
 //Remove any extraneous spaces from the beginning or end
 while(Result[Length(Result)]=' ')and(Length(Result)>1)do
  Result:=LeftStr(Result,Length(Result)-1);
 while(Result[1]=' ')and(Length(Result)>1)do
  Result:=RightStr(Result,Length(Result)-1);
end;

{-------------------------------------------------------------------------------
Read the header from an individual sprite, given the pointer into the pool
-------------------------------------------------------------------------------}
function TSpriteFile.ReadSpriteHeaderFromPool(ptr: Cardinal): TSprite;
var
 t,p,t2: Cardinal;
const
 //BPP colour depth of the Arthur/RISC OS 2/RISC OS 3.1 modes
 modes: array[0..53] of Byte = (1,2,3,2,1,2,1,3,2,3,
                                4,2,3,4,3,4,3,3,1,2,
                                3,4,3,1,4,1,2,3,4,1,
                                2,3,4,1,2,3,4,1,2,3,
                                4,1,2,4,1,2,3,4,3,4,
                                1,2,3,4);
begin
 Result.Offset:=ptr;
 //Pointer to next sprite
 Result.Next:=(FSpritePool[ptr+$03]<<24)
             +(FSpritePool[ptr+$02]<<16)
             +(FSpritePool[ptr+$01]<<8)
             + FSpritePool[ptr+$00];
 if ptr+Result.Next>FLastWord then exit;//Invalid Sprite File
 //Sprite name
 Result.Name:=GetSpriteName(ptr);
 //Width of sprite (in words) minus 1
 Result.WidthWord :=(FSpritePool[ptr+$13]<<24)
                   +(FSpritePool[ptr+$12]<<16)
                   +(FSpritePool[ptr+$11]<<8)
                   + FSpritePool[ptr+$10];
 //Height of sprite (in scanlines) minus 1
 Result.ScanLines:=(FSpritePool[ptr+$17]<<24)
                  +(FSpritePool[ptr+$16]<<16)
                  +(FSpritePool[ptr+$15]<<8)
                  + FSpritePool[ptr+$14];
 //First bit used (aka left hand wastage)
 Result.LeftBit:=(FSpritePool[ptr+$1B]<<24)
                +(FSpritePool[ptr+$1A]<<16)
                +(FSpritePool[ptr+$19]<<8)
                + FSpritePool[ptr+$18];
 //Last bit used (aka right hand wastage)
 Result.RightBit :=(FSpritePool[ptr+$1F]<<24)
                  +(FSpritePool[ptr+$1E]<<16)
                  +(FSpritePool[ptr+$1D]<<8)
                  + FSpritePool[ptr+$1C];
 //Pointer to first pixel of sprite
 Result.PixelData:=(FSpritePool[ptr+$23]<<24)
                  +(FSpritePool[ptr+$22]<<16)
                  +(FSpritePool[ptr+$21]<<8)
                  +(FSpritePool[ptr+$20]);
 //Pointer to transparent mask
 Result.Transparency:=(FSpritePool[ptr+$27]<<24)
                     +(FSpritePool[ptr+$26]<<16)
                     +(FSpritePool[ptr+$25]<<8)
                     +(FSpritePool[ptr+$24]);
 //Mode data
 Result.ModeData:= (FSpritePool[ptr+$2B]<<24)
                  +(FSpritePool[ptr+$2A]<<16)
                  +(FSpritePool[ptr+$29]<<8)
                  + FSpritePool[ptr+$28];
 //Header read in, now the calculations +++++++++++++++++++++++++++++++++++++++
 //Mode flag
 if Result.ModeData<128 then                //Old format
 begin
  //Not sprite type, but the actual screen mode, as old format
  Result.SpriteType:=modes[Result.ModeData mod 54];   //no modes 54+
  Result.ModeFlag:=0;
  Result.OS:=0;
 end
 else
  if (Result.ModeData>>27)AND$0F=$0F then   //RISC OS 5
  begin
   Result.SpriteType:=(Result.ModeData>>20)AND$7F;
   Result.ModeFlag:=(Result.ModeData>>8)AND$FF;
   Result.OS:=2;
  end
  else                                         //RISC OS 3.5
  begin
   Result.SpriteType:=(Result.ModeData>>27)AND$0F;
   Result.ModeFlag:=0;
   Result.OS:=1;
  end;
 //Bits per pixel
 Result.BPP:=0;
 case Result.SpriteType of
   0: ;//Invalid - used to id Arthur modes
   1: Result.BPP:= 1; //1bpp palletised
   2: Result.BPP:= 2; //2bpp palletised
   3: Result.BPP:= 4; //4bpp palletised
   4: Result.BPP:= 8; //8bpp palletised
   5: Result.BPP:=16; //16bpp 1:5:5:5 TBGR
   6: Result.BPP:=32; //32bpp 8:8:8:8 TBGR
   7: Result.BPP:=32; //CMYK
   8: ;//24bpp
   9: ;//JPEG
  10: Result.BPP:=16; //16bpp 5:6:5 TBGR
  15: ;//Invalid - used for id RISC OS 5 sprite mode words
  16: Result.BPP:=16; //16bpp 4:4:4:4
  17: ;//4:2:0 YCbCr
  18: ;//4:2:4 YCbCr
  //11-14 and 19-127 Reserved
 end;
 if Result.SpriteType=7 then Result.OS:=3;
 //Pixel Width
 if Result.BPP>0 then //BPP of 0 means that we can't handle it, currently
 begin
  Result.PixWidth:=((Result.WidthWord*32)
                       +Result.RightBit+1
                       -Result.LeftBit)
                       div Result.BPP;
  //Now we read in the data +++++++++++++++++++++++++++++++++++++++++++++++++++
  //Have we a palette?
  if Result.PixelData>Result.Transparency then
   p:=Result.Transparency
  else
   p:=Result.PixelData;
  //By default
  Result.PaletteType:=0;
  Result.HasPalette:=False;
  Result.PaletteColours:=0;
  if Result.BPP<16 then //sprites with bpp of 16 or more have no palette
   if p>$2C then //Yes, palette is in the file
   begin
    Result.PaletteType:=1;
    Result.HasPalette:=True;
    //Number of entries in palette
    Result.PaletteColours:=(p-$2C) div 8;
    //Partial palette? 8bpp only
    if(Result.BPP=8)AND(Result.PaletteColours<256)then Result.PaletteType:=2;
   end;
  //Is there a transparent mask?
  if Result.Transparency<>Result.PixelData then
  begin
   //We do, so read in transparent mask
   if Result.Transparency>Result.PixelData then
   begin //Transparency data is after pixel data
    t:=Result.Next-Result.Transparency; //Size of transparency
    t2:=Result.Transparency-Result.PixelData; //Size of pixel data
   end
   else
   begin //Transparency data is before pixel data
    t2:=Result.Next-Result.PixelData; //Size of Pixel Data
    t:=Result.PixelData-Result.Transparency; //Size of Transparency
   end;
   //Work out if it is old format or new format
   //If new format, the sizes will be different
   if t<t2 then
    //Look at bit 31 of the mode data - if set, it is a 'wide mask'.
    //This means that the mask will be 8bpp, whatever. Therefore, the size of
    //the pixel data and the mask data will be different.
    if Result.ModeData>>31=0 then
     Result.TransFormat:=2
    else
     Result.TransFormat:=3
   else //If the sizes are the same, it can still be a new mask
    if Result.ModeData>127 then//Old masks are used when an old style mode is specified
     Result.TransFormat:=2
    else
     Result.TransFormat:=1;
  end
  else //No mask
   Result.TransFormat:=0;
 end;
end;

{-------------------------------------------------------------------------------
Read an individual sprite in, given the pointer into the pool
-------------------------------------------------------------------------------}
function TSpriteFile.ReadSpriteFromPool(ptr: Cardinal): TSprite;
var
 buffer   : TDynByteArray;
 maskbpp,
 r,g,b,a,
 swap,
 c,m,y,k  : Byte;
 sx,sy,
 bx,p,t,
 tp       : Cardinal;
 mask     : Boolean;
 img      : TLazIntfImage;
 col      : TFPColor;
begin
 maskbpp:=0;
 //Sprite header
 Result:=ReadSpriteHeaderFromPool(ptr);
 if Result.BPP>0 then //BPP of 0 means that we can't handle it, currently
 begin
  if Result.BPP<16 then //sprites with bpp of 16 or more have no palette
  begin
   //Assign enough memory for the palette
   SetLength(Result.Palette,(1<<Result.BPP)*4);
   //We don't have a palette in file, or it is partial
   if(not Result.HasPalette)or(Result.PaletteType=2)then
   begin
    //Use standard palette (constants in file SpriteFilePalettes.pas)
    SetLength(buffer,0);
    case Result.BPP of
     1: buffer:=ColourPalette2;    //2 colour palette
     2: buffer:=ColourPalette4;    //4 colour palette
     4: buffer:=ColourPalette16;   //16 colour palette
     8: buffer:=ColourPalette256;  //256 colour palette
    end;
    //Extract the palette entries, discarding the VDU19,cn,16
    for t:=0 to (1<<Result.BPP)-1 do
    begin
     Result.Palette[ t*4   ]:=buffer[(t*6)+5];//Blue
     Result.Palette[(t*4)+1]:=buffer[(t*6)+4];//Green
     Result.Palette[(t*4)+2]:=buffer[(t*6)+3];//Red
     Result.Palette[(t*4)+3]:=$00;            //Alpha
    end;
   end;
   //We do have a palette in file - if partial, we will overwrite the standard one
   if Result.HasPalette then
   begin
    sx:=0;  //Pointer into our palette
    t:=$2C; //Pointer into the data
    repeat
     Result.Palette[sx  ]:=FSpritePool[ptr+t+3]; //Blue
     Result.Palette[sx+1]:=FSpritePool[ptr+t+2]; //Green
     Result.Palette[sx+2]:=FSpritePool[ptr+t+1]; //Red
     Result.Palette[sx+3]:=$00;           //Alpha (0x00)
     inc(sx,4);
     inc(t,8);
    until sx>=Result.PaletteColours*4; //t>=p-1;
   end;
  end;
  //Set up the mask array
  SetLength(Result.Mask,Result.PixWidth,Result.ScanLines+1);
  for sx:=0 to Length(Result.Mask)-1 do
   for sy:=0 to Length(Result.Mask[sx])-1 do
    Result.Mask[sx,sy]:=$00;
  if Result.TransFormat=2 then maskbpp:=1; //Mask is 1bpp
  if Result.TransFormat=3 then maskbpp:=8; //Mask is 8bpp Alpha
  if Result.TransFormat=1 then maskbpp:=Result.BPP;
  //All information now gathered from the file, for this sprite, so now create
  //the image. We still need to read in the mask and sprite data.
  //
  //Create the bitmap container in the array
  Result.Image:=TBitmap.Create;
  Result.PNG:=TPortableNetworkGraphic.Create;
  Result.PNG.PixelFormat:=pf32bit;
  //Setup the PNG container
  img:=TLazIntfImage.Create(0,0,[riqfRGB,riqfAlpha]);
  img.SetSize(Result.PixWidth,Result.ScanLines+1);
  //Extract the sprite data +++++++++++++++++++++++++++++++++++++++++++++++++++
  for sy:=0 to Result.ScanLines do
  begin
   //Loops through each pixel
   for sx:=0 to Result.PixWidth-1 do
   begin
    col.Alpha:=$FF;
    col.Red:=0;
    col.Green:=0;
    col.Blue:=0;
    //We will read in the mask data first +++++++++++++++++++++++++++++++++++++
    if Result.TransFormat>0 then //Check we have a mask
     if sx<Length(Result.Mask) then //And the x and y are within bounds
      if sy<Length(Result.Mask[sx]) then
      begin
       //Pointer to mask pixel in sprite
       tp:=Result.Transparency+ptr+
           (((((Result.PixWidth*maskbpp)+7)div 8)+3)div 4)*4*sy+
           Floor(sx*(maskbpp/8))+
           (Result.LeftBit div 8);
       //Byte(s) containing the pixel
       t:=0;
       for bx:=0 to Ceil(maskbpp/8)-1 do t:=t+FSpritePool[tp+bx]<<(bx*8);
       //Take account of the left hand wastage
       t:=t>>(Result.LeftBit mod 8);
       mask:=False;
       //The PRM says zero is invisible, anything else is visible
       case maskbpp of
        1: mask:=t and($1<<(sx mod 8))=0;    //1bpp
        2: mask:=t and($3<<((sx mod 4)*2))=0;//2bpp
        4: mask:=t AND($F<<((sx mod 2)*4))=0;//4bpp
        8,                                   //8bpp
        16,                                  //16bpp
        24,                                  //24bpp
        32: mask:=t and $FF=0;               //32bpp
       end;
       if mask then Result.Mask[sx,sy]:=$FF else Result.Mask[sx,sy]:=$00;
       if Result.TransFormat=3 then //Wide mask, so this is the alpha value
        Result.Mask[sx,sy]:=t and $FF;
       col.Alpha:=$FF-Result.Mask[sx,sy];
      end;
    //Pointer to pixel in sprite
    p:=Result.PixelData+ptr+
            (((((Result.PixWidth*Result.BPP)+7)div 8)+3)div 4)*4*sy+
            Floor(sx*(Result.BPP/8))+
            (Result.LeftBit div 8);
    //Byte(s) containing the pixel
    t:=0;
    for bx:=0 to Ceil(Result.BPP/8)-1 do t:=t+FSpritePool[p+bx]<<(bx*8);
    //Take account of the left hand wastage
    t:=t>>(Result.LeftBit mod 8);
    r:=0;
    g:=0;
    b:=0;
    a:=$FF;
    case Result.BPP of
     1:
     begin
      r:=Result.Palette[((t>>(sx mod 8))AND 1)*4+2];
      g:=Result.Palette[((t>>(sx mod 8))AND 1)*4+1];
      b:=Result.Palette[((t>>(sx mod 8))AND 1)*4+0];
     end;
     2:
     begin
      r:=Result.Palette[((t>>((sx mod 4)*2))AND 3)*4+2];
      g:=Result.Palette[((t>>((sx mod 4)*2))AND 3)*4+1];
      b:=Result.Palette[((t>>((sx mod 4)*2))AND 3)*4+0];
     end;
     4:
     begin
      r:=Result.Palette[((t>>((sx mod 2)*4))and$F)*4+2];
      g:=Result.Palette[((t>>((sx mod 2)*4))and$F)*4+1];
      b:=Result.Palette[((t>>((sx mod 2)*4))and$F)*4+0];
     end;
     8:
     begin
      r:=Result.Palette[t*4+2];
      g:=Result.Palette[t*4+1];
      b:=Result.Palette[t*4+0];
     end;
     16:
     begin
      //1:5:5:5 - Sprite Type 5
      if Result.SpriteType=5 then
      begin
       b:=(t AND $7C00)>>7;
       g:=(t AND $3E0)>>2;
       r:=(t AND $1F)<<3;
       a:=((t AND $8000)>>15)*$FF;
      end;
      //5:6:5 - Sprite Type 10
      if Result.SpriteType=10 then
      begin
       b:=(t AND $F800)>>8;
       g:=(t AND $7E0)>>3;
       r:=(t AND $1F)<<3;
       a:=0;
      end;
      //4:4:4:4 - Sprite Type 16
      if Result.SpriteType=16 then
      begin
       b:=(t AND $F00)>>4;
       g:= t AND $F0;
       r:=(t AND $F)<<4;
       a:=(t AND $F000)>>8;
      end;
     end;
     32:
     begin
      //BGR
      r:=t and $FF;
      g:=(t>>8)and$FF;
      b:=(t>>16)and$FF;
      a:=(t>>24)and$FF;
      if Result.TransFormat=3 then
       a:=Result.Mask[sx,sy]; //Alpha
     end;
    end;
    //RGB
    if ((Result.ModeFlag>>6)AND$1=1)     //Modeflag bit 6 is set
    and((Result.ModeFlag>>4)AND$3=0)then //bits 4 and 5 are clear
    begin
     swap:=b;
     b:=r;
     r:=swap;
    end;
    //CMYK
    if(Result.SpriteType=7) //Sprite Type 7 (RISC OS SIX)
    or (((Result.ModeFlag>>6)AND$3=0) //Mode flag bits 6 & 7 clear
    and((Result.ModeFlag>>4)AND$3=1))then //Mode flag bit 4 set & bit 5 clear
    begin
     //CMYK is stored KKYYMMCC, so k is a, y is b, m is g and r is c
     c:=r;
     m:=g;
     y:=b;
     k:=a;
     //Convert CMYK to RGB
     r:=Round(255*(1-c/255)*(1-k/255));
     g:=Round(255*(1-m/255)*(1-k/255));
     b:=Round(255*(1-y/255)*(1-k/255));
     //And blank off the alpha
     a:=0;
    end;
    col.Red:=r;
    col.Green:=g;
    col.Blue:=b;
    //Alpha sprite? Only if bit 7 of the Modeflag is set, and bits 4 and 5 are not
    if (Result.TransFormat=0)
    and((Result.ModeFlag>>4)AND$3=0)
    and((Result.ModeFlag>>6)AND$2<>$2)then
     a:=$FF;
    //Old and New masks have been applied already
    if(Result.TransFormat=0)or(Result.TransFormat=3)then
     col.Alpha:=a;
    //TFPColor is 16 bit per colour
    col.Alpha:=col.Alpha or col.Alpha<<8;
    col.Red  :=col.Red   or col.Red<<8;
    col.Green:=col.Green or col.Green<<8;
    col.Blue :=col.Blue  or col.Blue<<8;
    //Write the pixel for PNG
    img.Colors[sx,sy]:=col;
   end;
  end;
  Result.PNG.LoadFromIntfImage(img);
  img.Free;
  Result.Image.Assign(Result.PNG);
 end;
 //Move onto the next sprite
 ptr:=ptr+Result.Next;
end;

{-------------------------------------------------------------------------------
Save the data for a given sprite data
-------------------------------------------------------------------------------}
function TSpriteFile.SavePaletteFile(Afilename: String;spritedata: TSprite): Boolean;
var
 F         : TFileStream;
 buffer    : TDynByteArray;
 x         : Integer;
begin
 if spritedata.HasPalette then
 begin
 //Create the data to save - num of colours * 6 bytes
 SetLength(buffer,(Length(spritedata.Palette)div 4)*6);
  for x:=0 to (Length(spritedata.Palette)div 4)-1 do
  begin
   buffer[x*6+0]:=19; //VDU19,col,16,R,G,B
   buffer[x*6+1]:=x;
   buffer[x*6+2]:=16;
   buffer[x*6+3]:=spritedata.Palette[x*4+2];//Red
   buffer[x*6+4]:=spritedata.Palette[x*4+1];//Green
   buffer[x*6+5]:=spritedata.Palette[x*4+0];//Blue
  end;
  //Create the file and write the data out
  try
   F:=TFileStream.Create(Afilename,fmCreate or fmShareDenyNone);
   F.Write(buffer[0],Length(buffer));
   F.Free;
   Result:=True;
  except
   Result:=False;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update any progress indicators
-------------------------------------------------------------------------------}
procedure TSpriteFile.UpdateProgress(Fupdate: Integer);
begin
 //If the main program has defined a procedure then call it
 if Assigned(FProgress) then
  FProgress(Fupdate);
end;

{-------------------------------------------------------------------------------
Check to see if a supplied image filename is a valid BMP, PNG or JPEG
-------------------------------------------------------------------------------}
function TSpriteFile.IsValidImageFile(filename: String):Boolean;
var
 buffer : TDynByteArray;
 F      : TFileStream;
 bmp,
 png,
 jpg    : Boolean;
 j      : Integer;
 size   : Cardinal;
 const
  pngsig: array[0..$F] of Byte=
  ($89,$50,$4E,$47,$0D,$0A,$1A,$0A,$00,$00,$00,$0D,$49,$48,$44,$52);
begin
 //Initialise the variables
 buffer:=nil;
 png:=True;
 bmp:=False;
 jpg:=False;
 //Read the image in
 try
  F:=TFileStream.Create(filename,fmOpenRead or fmShareDenyNone);
  size:=F.Size;
  SetLength(buffer,16);
  F.Read(buffer[0],16);
  F.Free;
  //Bitmap will have 'BM' followed by the file size
  bmp:=(buffer[0]=ord('B'))and(buffer[1]=ord('M'))
        and(buffer[2]+buffer[3]<<8+buffer[4]<<16+buffer[5]<<24=size);
  //PNG have a 16 byte signature
  for j:=0 to 15 do
   if buffer[j]<>pngsig[j] then png:=False;
  //JPEGs are in chunks, the first is FF,D8
  jpg:=(buffer[0]=$FF)and(buffer[1]=$D8);
  //Return the result
  Result:=png or bmp or jpg;
 except
  Result:=False;
 end;
end;

{-------------------------------------------------------------------------------
Update the sprite pool header
-------------------------------------------------------------------------------}
procedure TSpriteFile.UpdateHeader;
begin
 //Size of pool
 FSpritePool[$00]:= Length(FSpritePool)     AND$FF;
 FSpritePool[$01]:=(Length(FSpritePool)>> 8)AND$FF;
 FSpritePool[$02]:=(Length(FSpritePool)>>16)AND$FF;
 FSpritePool[$03]:=(Length(FSpritePool)>>24)AND$FF;
 //Number of sprites in pool
 FSpritePool[$04]:= FSpriteCount     AND$FF;
 FSpritePool[$05]:=(FSpriteCount>> 8)AND$FF;
 FSpritePool[$06]:=(FSpriteCount>>16)AND$FF;
 FSpritePool[$07]:=(FSpriteCount>>24)AND$FF;
 //Offset to first sprite
 FSpritePool[$08]:= FFirstSprite     AND$FF;
 FSpritePool[$09]:=(FFirstSprite>> 8)AND$FF;
 FSpritePool[$0A]:=(FFirstSprite>>16)AND$FF;
 FSpritePool[$0B]:=(FFirstSprite>>24)AND$FF;
 //Offset to last free word
 FSpritePool[$0C]:= FLastWord     AND$FF;
 FSpritePool[$0D]:=(FLastWord>> 8)AND$FF;
 FSpritePool[$0E]:=(FLastWord>>16)AND$FF;
 FSpritePool[$0F]:=(FLastWord>>24)AND$FF;
end;

{-------------------------------------------------------------------------------
Convert a mode flag into text
-------------------------------------------------------------------------------}
function TSpriteFile.DecodeModeFlags(modeflag: Byte): String;
begin
 Result:='';
 if modeflag AND $1=$1 then
  Result:=Result+' Full res interlace';
 if modeflag AND $2=$2 then
  Result:=Result+' Greyscale';
 case (modeflag>>4)AND$3 of //bits 4 & 5
  0: //RGB
   case modeflag>>6 of
    0: Result:=Result+' RGB - TBGR';//bits 6 & 7 clear
    1: Result:=Result+' RGB - TRGB';//bit 7 clear, 6 set
    2: Result:=Result+' RGB - ABGR';//bit 7 set, 6 clear
    3: Result:=Result+' RGB - ARGB';//bits 6 & 7 set
   end;
  1: //Misc
   case modeflag>>6 of
    0: Result:=Result+' KYMC';    //bits 6 & 7 clear
    1: Result:=Result+' Reserved';
    2: Result:=Result+' Reserved';
    3: Result:=Result+' Reserved';
   end;
  2:
   case modeflag>>6 of
    0: Result:=Result+' YCbCr - ITU-R BT.601 Full'; //bits 6 & 7 clear
    1: Result:=Result+' YCbCr - ITU-R BT.601 Video';//bit 7 clear, 6 set
    2: Result:=Result+' YCbCr - ITU-R BT.709 Full'; //bit 7 set, 6 clear
    3: Result:=Result+' YCbCr - ITU-R BT.709 Video';//bits 6 & 7 set
   end;
  3: Result:=Result+' Reserved';
 end;
 if Result[1]=' ' then Result:=Copy(Result,2);
end;

{-------------------------------------------------------------------------------
Convert a sprite type into text
-------------------------------------------------------------------------------}
function TSpriteFile.DecodeSpriteType(spritetype: Byte): String;
const
 //Sprite type string
 ModeStr: array[0..18] of String = ('Arthur mode','1bpp','2bpp','4bpp','8bpp',
                                    '16bpp 1:5:5:5 TBGR','32bpp 8:8:8:8 TBGR',
                                    '32bpp CMYK','24bpp','JPEG data',
                                    '16bpp 5:6:5 TBGR','Reserved','Reserved',
                                    'Reserved','Reserved','RISC OS 5',
                                    '16bpp 4:4:4:4','4:2:0 YCbCr','4:2:2 YCbCr');
begin
 if spritetype<=High(ModeStr) then
  Result:=ModeStr[spritetype]
 else
  Result:='undefined';
end;

{-------------------------------------------------------------------------------
Convert a mask type into text
-------------------------------------------------------------------------------}
function TSpriteFile.DecodeMaskType(transformat: Byte): String;
const
 masktype : array[0..3] of String = ('None','Old','New','Alpha');
begin
 if transformat<=High(masktype) then
  Result:=masktype[transformat]
 else
  Result:='undefined';
end;

{-------------------------------------------------------------------------------
Convert an OS number into text
-------------------------------------------------------------------------------}
function TSpriteFile.DecodeOS(os: Byte): String;
const
 //OS Compatibility string
 OSstr  : array[0..3]  of String = ('Arthur','RISC OS 3.5','RISC OS 5.21','RISC OS SIX');
begin
 if os<=High(OSstr) then
  Result:=OSstr[os]
 else
  Result:='undefined';
end;

{-------------------------------------------------------------------------------
Convert a palette type into text
-------------------------------------------------------------------------------}
function TSpriteFile.DecodePaletteType(pal: Byte): String;
const
 //Palette Types
 palStr : array[0..2] of String = ('No','Yes','Yes - Partial');
begin
 if pal<=High(palStr) then
  Result:=palStr[pal]
 else
  Result:='undefined';
end;

{-------------------------------------------------------------------------------
Delete a sprite from the pool
-------------------------------------------------------------------------------}
function TSpriteFile.DeleteSpriteFromPool(ptr: Cardinal): Boolean;
var
 next,t: Cardinal;
begin
 Result:=False;
 //If there is only one sprite, then just close the file
 if FSpriteCount=1 then
 begin
  CloseCurrentFile;
  Result:=True;
 end
 else
 begin
  if(ptr>FFirstSprite)and(ptr<FLastWord)then
  begin
   //We have the start, so get the end
   next:=FSpritePool[ptr+0]
        +FSpritePool[ptr+1]<<8
        +FSpritePool[ptr+2]<<16
        +FSpritePool[ptr+3]<<24;
   //Now move everything from next downwards to ptr
   if ptr+next<FLastWord then
    for t:=ptr to (FLastWord-next)-1 do
     FSpritePool[t]:=FSpritePool[next+t];
  end;
  //Reduce the sprite pool size
  SetLength(FSpritePool,FLastWord-next);
  //Move the pointer down
  dec(FLastWord,next);
  //Reduce the number of sprites
  dec(FSpriteCount);
  //Update the header
  UpdateHeader;
  Result:=True;
 end;
 Result:=True;
end;

{Published methods ************************************************************}

{-------------------------------------------------------------------------------
Class creator - initialises the global variables
-------------------------------------------------------------------------------}
constructor TSpriteFile.Create;
begin
 inherited Create;
 CloseCurrentFile; //This also sets up a new pool
end;

{-------------------------------------------------------------------------------
Loads a new sprite file and calls the ReadSpriteFile function
-------------------------------------------------------------------------------}
function TSpriteFile.LoadSpriteFileFromStream(F: TStream): Byte;
var
 error: Byte;
 data : TDynByteArray;
begin
 //Default error - unable to read
 error:=3;
 F.Position:=0;
 //We'll just read the data in
 SetLength(data,F.Size);
 F.Read(data[0],F.Size);
 //Read, and add to the sprite pool, the newly opened file
 error:=ReadSpriteFile(data);
 //And exit, hopefully with no errors
 Result:=error;
end;

{-------------------------------------------------------------------------------
Loads a new sprite file, from a memory stream, & calls the ReadSpriteFile function
-------------------------------------------------------------------------------}
function TSpriteFile.LoadSpriteFile(Afilename: String):Byte;
var
 F    : TFileStream;
 error: Byte;
 data : TDynByteArray;
begin
 //Default error - unable to read
 error:=3;
 //Open the file and read the data in
 try
  F:=TFileStream.Create(Afilename,fmOpenRead or fmShareDenyNone);
  F.Position:=0;
  //We'll just read the data in
  SetLength(data,F.Size);
  F.Read(data[0],F.Size);
  F.Free;
  //First sprite file? Set the filename
  if FSpriteFile='' then FSpriteFile:=Afilename;
 except
  //On exception, exit the function
  exit;
 end;
 //Read, and add to the sprite pool, the newly opened file
 error:=ReadSpriteFile(data);
 //And exit, hopefully with no errors
 Result:=error;
end;

{-------------------------------------------------------------------------------
Saves the entire sprite pool to disc
-------------------------------------------------------------------------------}
function TSpriteFile.SaveSpriteFile(Afilename: String):Boolean;
var
 F    : TFileStream;
begin
 if Length(FSpritePool)>4 then
 begin
  //Create the file and write the data out
  try
   F:=TFileStream.Create(Afilename,fmCreate or fmShareDenyNone);
   F.Write(FSpritePool[$04],Length(FSpritePool)-4);
   F.Free;
   Result:=True;
  except
   Result:=False;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Read an individual sprite in, given sprite number
-------------------------------------------------------------------------------}
function TSpriteFile.ReadSprite(sprite: Integer;headeronly: Boolean=False): TSprite;
var
  x   : Integer;
  ptr,
  next: Cardinal;
begin
 //Ensure it is within bounds
 if(sprite>=0)and(sprite<FSpriteCount) then
 begin
  //Iterate through the sprite pool
  x:=0;
  ptr:=FFirstSprite;
  while x<>sprite do
  begin
   //Get the pointer to the next sprite
   next:=(FSpritePool[ptr+$03]<<24)
        +(FSpritePool[ptr+$02]<<16)
        +(FSpritePool[ptr+$01]<<8)
        + FSpritePool[ptr+$00];
   //Move to the next sprite
   inc(ptr,next);
   //And increase our counter
   inc(x);
  end;
  //Read the sprite in
  if headeronly then
   Result:=ReadSpriteHeaderFromPool(ptr)
  else
   Result:=ReadSpriteFromPool(ptr);
 end;
end;

{-------------------------------------------------------------------------------
Read an individual sprite in, given sprite name
-------------------------------------------------------------------------------}
function TSpriteFile.ReadSprite(spritename: String;headeronly: Boolean=False): TSprite;
var
 ptr,
 next: Cardinal;
 name: String;
begin
 //Iterate through the sprite pool
 ptr:=FFirstSprite;
 repeat
  //Get the pointer to the next sprite
  next:=(FSpritePool[ptr+$03]<<24)
       +(FSpritePool[ptr+$02]<<16)
       +(FSpritePool[ptr+$01]<<8)
       + FSpritePool[ptr+$00];
  //Get the sprite name
  name:=GetSpriteName(ptr);
  //If it does not match, move onto the next one
  if name<>spritename then inc(ptr,next);
 until(name=spritename)or(ptr>=FLastWord);
 //If we have found it, then read it in
 if ptr<FLastWord then
  if headeronly then
   Result:=ReadSpriteHeaderFromPool(ptr)
  else
   Result:=ReadSpriteFromPool(ptr);
end;

{-------------------------------------------------------------------------------
Get a sprite name from the sprite number
-------------------------------------------------------------------------------}
function TSpriteFile.SpriteName(sprite: Integer): String;
var
  x   : Integer;
  ptr,
  next: Cardinal;
begin
 //Ensure it is within bounds
 if(sprite>=0)and(sprite<FSpriteCount) then
 begin
  //Iterate through the sprite pool
  x:=0;
  ptr:=FFirstSprite;
  while x<>sprite do
  begin
   //Get the pointer to the next sprite
   next:=(FSpritePool[ptr+$03]<<24)
        +(FSpritePool[ptr+$02]<<16)
        +(FSpritePool[ptr+$01]<<8)
        + FSpritePool[ptr+$00];
   //Move to the next sprite
   inc(ptr,next);
   //And increase our counter
   inc(x);
  end;
  //Read the sprite in
  Result:=GetSpriteName(ptr);
 end;
end;

{-------------------------------------------------------------------------------
Save the data for a given sprite
-------------------------------------------------------------------------------}
function TSpriteFile.SavePaletteFile(Afilename, spritename: String):Boolean;
var
 spritedata: TSprite;
begin
 spritedata.HasPalette:=False;
 spritedata:=ReadSprite(spritename);
 if spritedata.HasPalette then
  Result:=SavePaletteFile(Afilename,spritedata);
end;

{-------------------------------------------------------------------------------
Save the data for a given sprite name
-------------------------------------------------------------------------------}
function TSpriteFile.SavePaletteFile(Afilename: String;sprite: Integer):Boolean;
var
 spritedata: TSprite;
begin
 spritedata.HasPalette:=False;
 spritedata:=ReadSprite(sprite);
 if spritedata.HasPalette then
  Result:=SavePaletteFile(Afilename,spritedata);
end;

{-------------------------------------------------------------------------------
Class destructor
-------------------------------------------------------------------------------}
destructor TSpriteFile.Destroy;
begin
 inherited;
 CloseCurrentFile;
end;

{-------------------------------------------------------------------------------
Convert the mode flag for a given sprite number
-------------------------------------------------------------------------------}
function TSpriteFile.ModeFlag(spritenumber: Integer): String;
var
 spritedata: TSprite;
begin
 Result:='';
 spritedata.ModeFlag:=0;
 spritedata:=ReadSprite(spritenumber,True);
 if spritedata.OS=2 then Result:=DecodeModeFlags(spritedata.ModeFlag);
end;

{-------------------------------------------------------------------------------
Convert the mode flag for a given sprite name
-------------------------------------------------------------------------------}
function TSpriteFile.ModeFlag(spritename: String): String;
var
 spritedata: TSprite;
begin
 Result:='';
 spritedata.ModeFlag:=0;
 spritedata:=ReadSprite(spritename,True);
 if spritedata.OS=2 then Result:=DecodeModeFlags(spritedata.ModeFlag);
end;

{-------------------------------------------------------------------------------
Convert the sprite type for given sprite number
-------------------------------------------------------------------------------}
function TSpriteFile.SpriteType(spritenumber: Integer): String;
var
 spritedata: TSprite;
begin
 spritedata.SpriteType:=0;
 spritedata:=ReadSprite(spritenumber,True);
 Result:=DecodeSpriteType(spritedata.SpriteType);
end;

{-------------------------------------------------------------------------------
Convert the sprite type for given sprite name
-------------------------------------------------------------------------------}
function TSpriteFile.SpriteType(spritename: String): String;
var
 spritedata: TSprite;
begin
 spritedata.SpriteType:=0;
 spritedata:=ReadSprite(spritename,True);
 Result:=DecodeSpriteType(spritedata.SpriteType);
end;

{-------------------------------------------------------------------------------
Convert the mask format for given sprite number
-------------------------------------------------------------------------------}
function TSpriteFile.MaskFormat(spritenumber: Integer): String;
var
 spritedata: TSprite;
begin
 spritedata.TransFormat:=0;
 spritedata:=ReadSprite(spritenumber,True);
 Result:=DecodeMaskType(spritedata.TransFormat);
end;

{-------------------------------------------------------------------------------
Convert the mask format for given sprite name
-------------------------------------------------------------------------------}
function TSpriteFile.MaskFormat(spritename: String): String;
var
 spritedata: TSprite;
begin
 spritedata.TransFormat:=0;
 spritedata:=ReadSprite(spritename,True);
 Result:=DecodeMaskType(spritedata.TransFormat);
end;

{-------------------------------------------------------------------------------
Convert the OS for given sprite number
-------------------------------------------------------------------------------}
function TSpriteFile.OS(spritenumber: Integer): String;
var
 spritedata: TSprite;
begin
 spritedata.OS:=0;
 spritedata:=ReadSprite(spritenumber,True);
 Result:=DecodeOS(spritedata.OS);
end;

{-------------------------------------------------------------------------------
Convert the OS for given sprite name
-------------------------------------------------------------------------------}
function TSpriteFile.OS(spritename: String): String;
var
 spritedata: TSprite;
begin
 spritedata.OS:=0;
 spritedata:=ReadSprite(spritename,True);
 Result:=DecodeOS(spritedata.OS);
end;

{-------------------------------------------------------------------------------
Convert the palette type for given sprite number
-------------------------------------------------------------------------------}
function TSpriteFile.PaletteType(spritenumber: Integer): String;
var
 spritedata: TSprite;
begin
 spritedata.PaletteType:=0;
 spritedata:=ReadSprite(spritenumber,True);
 Result:=DecodePaletteType(spritedata.PaletteType);
end;

{-------------------------------------------------------------------------------
Convert the palette type for given sprite name
-------------------------------------------------------------------------------}
function TSpriteFile.PaletteType(spritename: String): String;
var
 spritedata: TSprite;
begin
 spritedata.PaletteType:=0;
 spritedata:=ReadSprite(spritename,True);
 Result:=DecodePaletteType(spritedata.PaletteType);
end;

{-------------------------------------------------------------------------------
Import a PNG/BMP/JPEG image and add to the sprite pool
-------------------------------------------------------------------------------}
function TSpriteFile.ImportImage(filename: String;arthur: Boolean=False): Boolean;
var
 buffer,
 LSpritePool: TDynByteArray;
 img        : TLazIntfImage;
 col        : TFPColor;
 x,y,
 numcols    : Integer;
 colours    : array of String;
 mask,
 BPP,
 maskBPP    : Byte;
 colsrc,
 newname    : String;
 pixsize,
 ptr,p,t,
 LLastWord  : Cardinal;
 spritedata : TSprite;
begin
 Result:=False;
 colours:=nil;
 if IsValidImageFile(filename) then
 begin
  numcols:=0;
  SetLength(colours,257);
  img:=TLazIntfImage.Create(0,0,[riqfRGB, riqfAlpha]);
  img.LoadFromFile(filename);
  SetLength(buffer,img.Height*img.Width*4);
  mask:=0;//No mask
  for y:=0 to img.Height-1 do
   for x:=0 to img.Width-1 do
   begin
    col:=img.Colors[x,y];
    buffer[(y*img.Width*4)+(x*4)+3]:=col.Alpha>>8;
    if(col.Alpha>>8=$00)and(mask=0)then mask:=1;// Binary mask
    if(col.Alpha>>8>$00)and(col.Alpha>>8<$FF)then mask:=2; //Alpha mask
    buffer[(y*img.Width*4)+(x*4)+2]:=col.Red  >>8;
    buffer[(y*img.Width*4)+(x*4)+1]:=col.Green>>8;
    buffer[(y*img.Width*4)+(x*4)+0]:=col.Blue >>8;
    if numcols<257 then
    begin
     colsrc:=IntToHex(col.Blue>>8,2)+IntToHex(col.Green>>8,2)+IntToHex(col.Red>>8,2);
     if AnsiIndexStr(colsrc,colours)=-1 then
     begin
      colours[numcols]:=colsrc;
      inc(numcols);
     end;
    end;
   end;
  BPP:=0;
  repeat
   if BPP=0 then inc(BPP) else BPP:=BPP*2;
  until numcols<1<<BPP;
  if BPP=16 then BPP:=32;
  //We will create a new sprite file, then call the method to add to the pool
  LLastWord:=$0C;
  colsrc:=ExtractFilename(filename);
  colsrc:=LowerCase(LeftStr(colsrc,Length(colsrc)-Length(ExtractFileExt(filename))));
  colsrc:=LeftStr(colsrc,12);
  x:=0;
  newname:=colsrc;
  while not IsValidSpriteName(newname) do
  begin
   inc(x);
   newname:=RightStr(colsrc+IntToStr(x),12);
  end;
  spritedata.Name:=newname;
  spritedata.BPP:=BPP;
  spritedata.PixWidth:=img.Width;
  spritedata.ScanLines:=img.Height-1;
  spritedata.LeftBit:=0;
  spritedata.WidthWord:=Ceil((img.Width*BPP)/32)-1;
  spritedata.RightBit:=(img.Width*BPP)-(spritedata.WidthWord*32)-1;
  pixsize:=((spritedata.WidthWord+1)*4)*img.Height;
  if BPP<16 then //Point to pixel data, after the palette
  begin
   spritedata.PixelData:=$2C+8*(1<<BPP); //Two copies of each colour
   spritedata.HasPalette:=True;
   spritedata.PaletteColours:=1<<BPP;
   spritedata.PaletteType:=1;
  end
  else
  begin
   spritedata.PixelData:=$2C;
   spritedata.HasPalette:=False;
   spritedata.PaletteColours:=0;
   spritedata.PaletteType:=0;
   //Arthur did not have 16 or 32 bpp sprites
   arthur:=False;
  end;
  if mask>0 then
  begin
   spritedata.Transparency:=spritedata.PixelData+pixsize;
   if mask=1 then
    maskBPP:=1
   else
    maskBPP:=8;
   //Old masks are same BPP as main sprite
   if arthur then maskBPP:=BPP;
   pixsize:=Ceil((img.Width*maskBPP)/32)*4*img.Height;
   SetLength(spritedata.Mask,img.Width,img.Height);
   for y:=0 to img.Height-1 do
    for x:=0 to img.Width-1 do
    begin
     //Mask is stored $00 for transparent up to $FF for full pixel
     spritedata.Mask[x,y]:=buffer[y*img.Width*4+(x*4)+3];
     //Arthur masks are binary, either $00 for transparent or $FF for pixel
     if arthur then
      if spritedata.Mask[x,y]>0 then
       spritedata.Mask[x,y]:=$FF;
    end;
  end
  else
  begin
   spritedata.Transparency:=spritedata.PixelData;
   maskBPP:=0;
  end;
  spritedata.Next:=spritedata.Transparency+pixsize;
  ptr:=LLastWord;
  inc(LLastWord,spritedata.Next);
  if mask>0 then
   spritedata.TransFormat:=mask+1
  else
   spritedata.TransFormat:=0;
  spritedata.ModeFlag:=0;
  case BPP of
   1 : spritedata.SpriteType:=1;
   2 : spritedata.SpriteType:=2;
   4 : spritedata.SpriteType:=3;
   8 : spritedata.SpriteType:=4;
   16: spritedata.SpriteType:=5;
   32: spritedata.SpriteType:=6;
  end;
  //For Arthur, use Mode 19 (2bpp), 20 (4bpp) or 21 (8bpp)
  if arthur then
  begin
   spritedata.OS:=0;//Arthur
   case BPP of
    1: spritedata.ModeData:=18;
    2: spritedata.ModeData:=19;
    4: spritedata.ModeData:=20;
    8: spritedata.ModeData:=21;
   end;
  end
  else
  begin
   spritedata.OS:=1;//RISC OS 3.50
   spritedata.ModeData:=((mask AND 2)<<30)                 //Wide mask?
                              OR(spritedata.SpriteType<<27)//Sprite Type
                              OR(180<<14)                           //V DPI
                              OR(180<<1)                            //H DPI
                              OR 1;                                 //Set
  end;
  if BPP<16 then
  begin
   SetLength(spritedata.Palette,(1<<BPP)*4);
   for x:=0 to Length(spritedata.Palette)-1 do
    spritedata.Palette[x]:=0;
   for x:=0 to numcols-1 do
   begin //BGRA
    p:=StrToInt('$'+colours[x]);
    spritedata.Palette[x*4+0]:=p>>16;
    spritedata.Palette[x*4+1]:=(p>>8)AND$FF;
    spritedata.Palette[x*4+2]:=p AND$FF;
    spritedata.Palette[x*4+3]:=$00;
   end;
  end;
  img.Free;
  SetLength(LSpritePool,LLastWord);
  for x:=0 to LLastWord-1 do LSpritePool[x]:=0;
  //What we pass to the read sprite routine does not include the first word
  inc(LLastWord,4); //But the offsets are as if they do
  //Number of sprites in pool
  LSpritePool[$00]:=1;
  //Offset to first sprite
  LSpritePool[$04]:=$10;
  //Offset to last free word
  LSpritePool[$08]:= LLastWord     AND$FF;
  LSpritePool[$09]:=(LLastWord>> 8)AND$FF;
  LSpritePool[$0A]:=(LLastWord>>16)AND$FF;
  LSpritePool[$0B]:=(LLastWord>>24)AND$FF;
  //Sprite header
  LSpritePool[ptr+$00]:= spritedata.Next     AND$FF;
  LSpritePool[ptr+$01]:=(spritedata.Next>> 8)AND$FF;
  LSpritePool[ptr+$02]:=(spritedata.Next>>16)AND$FF;
  LSpritePool[ptr+$03]:=(spritedata.Next>>24)AND$FF;
  for x:=0 to 11 do
  begin
   y:=0;
   if x<Length(spritedata.Name) then
    y:=Ord(spritedata.Name[x+1]);
   LSpritePool[ptr+$04+x]:=y;
  end;
  LSpritePool[ptr+$10]:= spritedata.WidthWord     AND$FF;
  LSpritePool[ptr+$11]:=(spritedata.WidthWord>> 8)AND$FF;
  LSpritePool[ptr+$12]:=(spritedata.WidthWord>>16)AND$FF;
  LSpritePool[ptr+$13]:=(spritedata.WidthWord>>24)AND$FF;
  LSpritePool[ptr+$14]:= spritedata.ScanLines     AND$FF;
  LSpritePool[ptr+$15]:=(spritedata.ScanLines>> 8)AND$FF;
  LSpritePool[ptr+$16]:=(spritedata.ScanLines>>16)AND$FF;
  LSpritePool[ptr+$17]:=(spritedata.ScanLines>>24)AND$FF;
  LSpritePool[ptr+$18]:= spritedata.LeftBit     AND$FF;
  LSpritePool[ptr+$19]:=(spritedata.LeftBit>> 8)AND$FF;
  LSpritePool[ptr+$1A]:=(spritedata.LeftBit>>16)AND$FF;
  LSpritePool[ptr+$1B]:=(spritedata.LeftBit>>24)AND$FF;
  LSpritePool[ptr+$1C]:= spritedata.RightBit     AND$FF;
  LSpritePool[ptr+$1D]:=(spritedata.RightBit>> 8)AND$FF;
  LSpritePool[ptr+$1E]:=(spritedata.RightBit>>16)AND$FF;
  LSpritePool[ptr+$1F]:=(spritedata.RightBit>>24)AND$FF;
  LSpritePool[ptr+$20]:= spritedata.PixelData     AND$FF;
  LSpritePool[ptr+$21]:=(spritedata.PixelData>> 8)AND$FF;
  LSpritePool[ptr+$22]:=(spritedata.PixelData>>16)AND$FF;
  LSpritePool[ptr+$23]:=(spritedata.PixelData>>24)AND$FF;
  LSpritePool[ptr+$24]:= spritedata.Transparency     AND$FF;
  LSpritePool[ptr+$25]:=(spritedata.Transparency>> 8)AND$FF;
  LSpritePool[ptr+$26]:=(spritedata.Transparency>>16)AND$FF;
  LSpritePool[ptr+$27]:=(spritedata.Transparency>>24)AND$FF;
  LSpritePool[ptr+$28]:= spritedata.ModeData     AND$FF;
  LSpritePool[ptr+$29]:=(spritedata.ModeData>> 8)AND$FF;
  LSpritePool[ptr+$2A]:=(spritedata.ModeData>>16)AND$FF;
  LSpritePool[ptr+$2B]:=(spritedata.ModeData>>24)AND$FF;
  //Write the palette data
  if BPP<16 then
   for x:=0 to (Length(spritedata.Palette)div 4)-1 do
   begin
    LSpritePool[ptr+$2C+x*8+0]:=$00;
    LSpritePool[ptr+$2C+x*8+1]:=spritedata.Palette[x*4+2];
    LSpritePool[ptr+$2C+x*8+2]:=spritedata.Palette[x*4+1];
    LSpritePool[ptr+$2C+x*8+3]:=spritedata.Palette[x*4+0];
    LSpritePool[ptr+$2C+x*8+4]:=LSpritePool[ptr+$2C+x*8+0];
    LSpritePool[ptr+$2C+x*8+5]:=LSpritePool[ptr+$2C+x*8+1];
    LSpritePool[ptr+$2C+x*8+6]:=LSpritePool[ptr+$2C+x*8+2];
    LSpritePool[ptr+$2C+x*8+7]:=LSpritePool[ptr+$2C+x*8+3];
   end;
  //Write the pixel and mask data
  for y:=0 to spritedata.ScanLines do
   for x:=0 to spritedata.PixWidth-1 do
   begin
    p:=spritedata.PixelData+ptr+
       (((((spritedata.PixWidth*BPP)+7)div 8)+3)div 4)*4*y+Floor(x*(BPP/8));
    colsrc:=IntToHex(buffer[(y*spritedata.PixWidth+x)*4+0],2)
           +IntToHex(buffer[(y*spritedata.PixWidth+x)*4+1],2)
           +IntToHex(buffer[(y*spritedata.PixWidth+x)*4+2],2);
    case BPP of
     1 : LSpritePool[p]:=LSpritePool[p]OR(AnsiIndexStr(colsrc,colours)AND$1)<<(x mod 8);
     2 : LSpritePool[p]:=LSpritePool[p]OR(AnsiIndexStr(colsrc,colours)AND$3)<<((x mod 4)*2);
     4 : LSpritePool[p]:=LSpritePool[p]OR(AnsiIndexStr(colsrc,colours)AND$7)<<((x mod 2)*4);
     8 : LSpritePool[p]:=AnsiIndexStr(colsrc,colours);
     16:
      begin
       LSpritePool[p+0]:=(buffer[(y*spritedata.PixWidth+x)*4+0]AND$F8)>>1
                 OR(buffer[(y*spritedata.PixWidth+x)*4+1]AND$C0)>>6;
       LSpritePool[p+1]:=(buffer[(y*spritedata.PixWidth+x)*4+1]AND$38)<<2
                 OR(buffer[(y*spritedata.PixWidth+x)*4+2]AND$F8)>>3;
      end;
     32:
      begin
       LSpritePool[p+0]:=buffer[(y*spritedata.PixWidth+x)*4+0];
       LSpritePool[p+1]:=buffer[(y*spritedata.PixWidth+x)*4+1];
       LSpritePool[p+2]:=buffer[(y*spritedata.PixWidth+x)*4+2];
       LSpritePool[p+3]:=buffer[(y*spritedata.PixWidth+x)*4+3];
      end;
    end;
    if mask>0 then
    begin
     t:=spritedata.Transparency+ptr+
        (((((spritedata.PixWidth*maskBPP)+7)div 8)+3)div 4)*4*y+Floor(x*(maskBPP/8));
     case maskBPP of
      1 : LSpritePool[t]:=LSpritePool[t]OR((spritedata.Mask[x,y])>>7)<<(x mod 8);
      2 : LSpritePool[t]:=LSpritePool[t]OR((spritedata.Mask[x,y])>>6)<<((x mod 4)*2);
      4 : LSpritePool[t]:=LSpritePool[t]OR((spritedata.Mask[x,y])>>4)<<((x mod 2)*4);
      8 : LSpritePool[t]:=spritedata.Mask[x,y];
      16:
      begin
       LSpritePool[t+0]:=spritedata.Mask[x,y];
       LSpritePool[t+1]:=spritedata.Mask[x,y];
      end;
      32:
      begin
       LSpritePool[t+0]:=spritedata.Mask[x,y];
       LSpritePool[t+1]:=spritedata.Mask[x,y];
       LSpritePool[t+2]:=spritedata.Mask[x,y];
       LSpritePool[t+3]:=spritedata.Mask[x,y];
      end;
     end;
    end;
   end;
  //We now have a virtual file, so add it to the pool
  Result:=ReadSpriteFile(LSpritePool)=0;
  if(Result)and(FSpriteFile='')then FSpriteFile:='Untitled,ff9';
 end;
end;

{-------------------------------------------------------------------------------
Has the supplied name already been used?
-------------------------------------------------------------------------------}
function TSpriteFile.IsValidSpriteName(name: String): Boolean;
var
 x    : Integer;
 ptr,
 next : Cardinal;
begin
 Result:=Length(name)<13;
 if FSpriteCount>0 then
 begin
  //Iterate through the sprite pool
  ptr:=FFirstSprite;
  for x:=0 to FSpriteCount-1 do
  begin
   if name=GetSpriteName(ptr) then Result:=False;
   //Get the pointer to the next sprite
   next:=(FSpritePool[ptr+$03]<<24)
        +(FSpritePool[ptr+$02]<<16)
        +(FSpritePool[ptr+$01]<<8)
        + FSpritePool[ptr+$00];
   //Move to the next sprite
   inc(ptr,next);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Close the current pool and re-initialise the variables
-------------------------------------------------------------------------------}
procedure TSpriteFile.CloseCurrentFile;
begin
 SetLength(FSpritePool,0);
 FSpriteFile:='';
 FFirstSprite:=$10;
 FLastWord:=0;
 FSpriteCount:=0;
end;

{-------------------------------------------------------------------------------
Rename a sprite
-------------------------------------------------------------------------------}
function TSpriteFile.RenameSprite(oldname,newname: String): Boolean;
var
 sprite: TSprite;
 t,c   : Integer;
begin
 Result:=False;
 //Make sure the proposed new name is valid
 if IsValidSpriteName(newname) then
 begin
  //Now get the pointer to the sprite
  sprite.Offset:=0;
  sprite:=ReadSprite(oldname,True); //Read just the header
  if sprite.Offset>=FFirstSprite then //If found
  begin
   //Write the new name
   for t:=0 to 11 do
   begin
    if t<Length(newname) then c:=Ord(newname[t+1]) else c:=0;
    FSpritePool[sprite.Offset+4+t]:=c;
   end;
   //Finish with a positive result
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Delete a sprite, given name
-------------------------------------------------------------------------------}
function TSpriteFile.DeleteSprite(spritename: String): Boolean;
var
 sprite: TSprite;
begin
 Result:=False;
 //Now get the pointer to the sprite
 sprite.Offset:=0;
 sprite:=ReadSprite(spritename,True); //Read just the header
 if sprite.Offset>=FFirstSprite then //If found
  Result:=DeleteSpriteFromPool(sprite.Offset);
end;

{-------------------------------------------------------------------------------
Delete a sprite, given number
-------------------------------------------------------------------------------}
function TSpriteFile.DeleteSprite(sprite: Integer): Boolean;
var
 spritedata: TSprite;
begin
 Result:=False;
 //Now get the pointer to the sprite
 spritedata.Offset:=0;
 spritedata:=ReadSprite(sprite,True); //Read just the header
 if spritedata.Offset>=FFirstSprite then //If found
  Result:=DeleteSpriteFromPool(spritedata.Offset);
end;

end.
