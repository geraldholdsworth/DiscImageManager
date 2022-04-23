unit Spark;

{
TSpark class V1.05
Decompress a Zip or PackDir archive, preserving the extra RISC OS information.
Thank you to David Pilling for his assistance.

Copyright (C) 2021-2022 Gerald Holdsworth gerald@hollypops.co.uk

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}

interface

uses Classes,SysUtils,Zipper,ExtCtrls,DateUtils,Math;

{$M+}

type
 TDynByteArray = array of Byte;
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
 type
  TSpark = Class
  private
  type
   TFileList = array of TFileEntry;
   TProgressProc = procedure(Sender: TObject;const Fupdate: Double) of Object;
   private
   Fcache,                     //Data cache for receiving uncompressed file
   Fbuffer     : TDynByteArray;//Buffer to hold the archive
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
   function FindEoCL(var CL: Cardinal): Cardinal;
   procedure UpdateCL(CL,EoCL: Cardinal);
   function ExtractFileDataFromSpark(index: Integer):TDynByteArray;
   function ExtractFileDataFromPack(index: Integer):TDynByteArray;
   procedure SaveData;
   function FindEntry(path: String;matchpath: Boolean;var CLptr: Cardinal;
                                                  var dataptr: Cardinal): Boolean;
   function RenameTheFile(oldpath, newpath: String): Boolean;
   function DeleteTheFile(filename: String):Boolean;
   //Private constants
   const
    NewAtts: array[0..7] of Char = ('R','W','L','D','r','w',' ',' ');
  published
   //Published methods
   constructor Create(filename: String;blank: Boolean=false);
   constructor Create(stream: TStream); overload;
   function ExtractFileData(Index: Integer):TDynByteArray;
   procedure WriteFile(var filetozip: TFileEntry;var buffer: TDynByteArray);
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

implementation

//++++++++++++++++++ Private methods +++++++++++++++++++++++++++++++++++++++++++

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
 ptr,
 EoCL,
 CL,
 ctr,
 fnc        : Cardinal;
 fnL,
 exL,
 cmL        : LongWord;
 fn,
 zipfn      : String;
 exists     : Integer;
 temp       : TFileEntry;
begin
 Result:=nil;
 if FIsSpark then
 begin
  FBitLength:=0; //Not used with !Spark
  //Find the 'End of central library'
  CL:=0;
  EoCL:=FindEoCL(CL);
  //Only continue of we have a marker
  if EoCL<>0 then
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
 ptr,
 fnptr,
 ctr        : Cardinal;
 fn         : String;
 dircount,
 dirref     : array of Integer;
 parent     : Integer;
begin
 Result:=nil;
 dircount:=nil;
 dirref:=nil;
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
function TSpark.ExtractFileData(Index: Integer):TDynByteArray;
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
function TSpark.ExtractFileDataFromSpark(index: Integer):TDynByteArray;
var
 ZipFile   : TUnZipper;
 sl        : TStringList;
 starttime,
 nowtime   : TDateTime;
 temp      : Boolean;
begin
 Result:=nil;
 temp:=False;
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
function TSpark.ExtractFileDataFromPack(index: Integer):TDynByteArray;
var
 buffer     : TDynByteArray;
 i,
 max,
 codesize,
 codemask,
 cc,
 eoi,
 nextfree,
 prev_code,
 bitbuffer,
 bitsremain,
 outptr,
 code,
 save_code,
 temp_code  : Cardinal;
 firstchar  : Byte;
 stackptr   : Cardinal;
 prefix     : array of Cardinal;
 suffix,
 stack      : TDynByteArray;
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
 tempfile: TFileStream;
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
                                                var dataptr: Cardinal): Boolean;
var
 CL,
 EoCL  : Cardinal;
 temp  : String;
 fnL   : Word;
 index : Integer;
begin
 Result:=False;
 //Get the location of the central library
 CL:=0;
 EoCL:=FindEoCL(CL);
 //If it exists
 if CL<>EoCL then
 begin
  //Find the entry in the Central Library (and, hence, the main header)
  CLptr:=CL;
  temp:='';
  while(temp<>path)and(CLptr<EoCL)do
  begin
   if (Fbuffer[CLptr]=$50)       //Entry signature
   and(Fbuffer[CLptr+1]=$4B)
   and(Fbuffer[CLptr+2]=$01)
   and(Fbuffer[CLptr+3]=$02)then
   begin
    //Filename length
    if matchpath then fnL:=Length(path) //Either match the path
    else fnL:=Fbuffer[CLptr+$1C]+Fbuffer[CLptr+$1D]<<8; //Or the given length
    //Get the location in the main data area
    dataptr:=Fbuffer[CLptr+$2A]
            +Fbuffer[CLptr+$2B]<<8
            +Fbuffer[CLptr+$2C]<<16
            +Fbuffer[CLptr+$2D]<<24;
    temp:=''; //Build the filename
    for index:=0 to fnL-1 do temp:=temp+chr(Fbuffer[CLptr+$2E+index]);
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
 ptr,
 dataptr,
 EoCL,
 CL,
 hdrpos  : Cardinal;
 index,
 fnL,
 diff    : Integer;
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
   EoCL:=FindEoCL(CL);
   //And the difference between the two names
   diff:=Length(newpath)-Length(oldpath); //+ve move forwards, -ve move back
   //Find the entry in the main header
   ptr:=0;
   dataptr:=0;
   if FindEntry(oldpath,true,ptr,dataptr) then
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
    UpdateCL(CL,EoCL);
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
     UpdateCL(CL,EoCL);
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
 EoCL,
 CL,
 hdrsize,
 clsize,
 fnL,
 exL,
 cmL,
 ptr,
 CLptr,
 data,
 index : Cardinal;
 match : Boolean;
begin
 //Default return - a false result does not mean it is a fail.
 Result:=False;
 //Remove the root, if present
 if Length(filename)>2 then if filename[1]='$' then filename:=Copy(filename,3);
 //Set up our variables
 CL:=0;
 ptr:=0;
 CLptr:=0;
 //Find the Central library
 EoCL:=FindEoCL(CL);
 if EoCL<>CL then
 begin
  //Find the entry in both the CL and header
  if filename[Length(filename)]='/' then match:=True else match:=False;
  if FindEntry(filename,match,CLptr,ptr) then
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
   UpdateCL(CL,EoCL);
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
   UpdateCL(CL,EoCL);
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
 i: Cardinal;
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
Find the 'End of central library'
-------------------------------------------------------------------------------}
function TSpark.FindEoCL(var CL: Cardinal): Cardinal;
var
 ptr: Cardinal;
begin
 //Set up the result - 0 means not found
 Result:=0;
 if Length(Fbuffer)>4 then
 begin
  //Start at the end
  ptr :=Length(Fbuffer)-4;
  //And decrease the pointer until we find the signature, or get to the start of file
  repeat
   dec(ptr);
  until((Fbuffer[ptr]  =$50)
     and(Fbuffer[ptr+1]=$4B)
     and(Fbuffer[ptr+2]=$05)
     and(Fbuffer[ptr+3]=$06))
     or(ptr=0);
  //Signature found, so mark it
  if (Fbuffer[ptr]=$50)
  and(Fbuffer[ptr+1]=$4B)
  and(Fbuffer[ptr+2]=$05)
  and(Fbuffer[ptr+3]=$06)then Result:=ptr;
  //Return the central library beginning too
  if Result>0 then
   CL:=Fbuffer[Result+$10]
      +Fbuffer[Result+$11]<<8
      +Fbuffer[Result+$12]<<16
      +Fbuffer[Result+$13]<<24;
 end;
end;

{-------------------------------------------------------------------------------
Update the Central Library pointer, with EoCL having already been moved
-------------------------------------------------------------------------------}
procedure TSpark.UpdateCL(CL,EoCL: Cardinal);
var
 ptr : Cardinal;
begin
 if(EoCL<>0)and(CL<>0)then
 begin
  //Work out the length
  ptr:=EoCL-CL;
  //And write it
  Fbuffer[EoCL+$0C]:=ptr mod $100;
  Fbuffer[EoCL+$0D]:=(ptr>>8)mod$100;
  Fbuffer[EoCL+$0E]:=(ptr>>16)mod$100;
  Fbuffer[EoCL+$0F]:=(ptr>>24)mod$100;
  //And the location of the central library
  Fbuffer[EoCL+$10]:=CL mod$100;
  Fbuffer[EoCL+$11]:=(CL>>8)mod$100;
  Fbuffer[EoCL+$12]:=(CL>>16)mod$100;
  Fbuffer[EoCL+$13]:=(CL>>24)mod$100;
 end;
end;

//++++++++++++++++++ Published Methods +++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Create the instance
-------------------------------------------------------------------------------}
constructor TSpark.Create(filename: String;blank: Boolean=false);
var
 F: TFileStream;
begin
 //Set the filename
 ZipFilename:=filename;
 //Create a blank file
 if blank then
 begin
  F:=TFileStream.Create(ZipFilename,fmCreate or fmShareDenyNone);
  F.Free;
 end;
 //Create a stream
 F:=TFileStream.Create(ZipFilename,fmOpenRead or fmShareDenyNone);
 //Read it using the overloaded constructor
 Create(F);
 //And free it up
 F.Free;
end;
constructor TSpark.Create(Stream: TStream);
begin
 inherited Create;
 //Initialise the variables
 Fbuffer    :=nil;
 Fversion   :='1.05';
 FTimeOut   :=30;
 FIsSpark   :=False;
 FIsPack    :=False;
 //Read the zip file into memory from the stream
 SetLength(Fbuffer,Stream.Size);
 Stream.Position:=0;
 Stream.Read(Fbuffer[0],Stream.Size);
 if Length(Fbuffer)<5 then exit;
 //Check it is a !Spark file (or just a zip file)
 if (Fbuffer[0]=$50)
 and(Fbuffer[1]=$4B)
 and(Fbuffer[2]=$03)
 and(Fbuffer[3]=$04)then FIsSpark:=True;
 //Is it a !PackDir file
 if (Fbuffer[0]=$50)
 and(Fbuffer[1]=$41)
 and(Fbuffer[2]=$43)
 and(Fbuffer[3]=$4B)
 and(Fbuffer[4]=$00)then FIsPack:=True;
 //It is neither
 if(not FIsSpark)and(not FIsPack)then SetLength(Fbuffer,0);//Clear the buffer
 //The extract the file details - the function will select the appropriate algorithm
 FFileList:=ExtractFiles;
end;

{-------------------------------------------------------------------------------
Destroy (free) the instance
-------------------------------------------------------------------------------}
destructor TSpark.Destroy;
begin
 inherited;
end;

{-------------------------------------------------------------------------------
Write a file to the archive
-------------------------------------------------------------------------------}
procedure TSpark.WriteFile(var filetozip: TFileEntry;var buffer: TDynByteArray);
var
 tempname : String;
 tempfile : TFileStream;
 zipfile  : TZipper;
 EoCL,
 CL,ptr,
 dataptr  : Cardinal;
 fnL,exL  : Word;
 index,
 adjust   : Integer;
const newExL = $18;
begin
 if FIsPack then exit;
 if filetozip.Directory then CreateDirectory(filetozip.ArchiveName)
 else
 begin
  //Zipper will only zip up existing files, so we'll need to save the data to a
  //temporary file first, then zip that.
  tempname:=GetTempFileName;//Get a temporary name
  tempfile:=TFileStream.Create(tempname,fmCreate);//Create the file
  tempfile.Position:=0;
  tempfile.Write(buffer[0],Length(buffer)); //Write the file data to it
  tempfile.Free; //Close the file
  //Now we can open the zipfile
  zipfile:=TZipper.Create;
  try
   zipfile.Filename:=ZipFilename; //Set the zipfile name
   zipfile.Entries.AddFileEntry(tempname,filetozip.ArchiveName); //Add the file
   zipfile.ZipAllFiles; //Then zip all the files
  finally
   zipfile.Free; //And close it
  end;
  //Finally, delete the temporary file
  DeleteFile(tempname);
  //Then we will need to load the zip file in and change the values and add to
  //the library at the end.
  tempfile:=TFileStream.Create(ZipFilename,fmOpenRead or fmShareDenyNone);
  SetLength(Fbuffer,tempfile.Size);
  tempfile.Position:=0;
  tempfile.Read(Fbuffer[0],tempfile.Size);
  tempfile.Free;
  CL:=0;
  EoCL:=FindEoCL(CL); //Get the end of central library marker
  if EoCL<>0 then
  begin
   //Find the entry for this file
   if FindEntry(filetozip.ArchiveName,false,ptr,dataptr) then
   begin
    //Get the variable length fields lengths
    fnL:=Fbuffer[ptr+$1C]+Fbuffer[ptr+$1D]<<8;
    exL:=Fbuffer[ptr+$1E]+Fbuffer[ptr+$1F]<<8;
    //We need to add up to 24 bytes, so everything from the next entry needs to shift along
    adjust:=0;
    if exL<newExL then adjust:=newExL-exL;
    if adjust>0 then
    begin
     SetLength(Fbuffer,Length(Fbuffer)+adjust);
     for index:=Length(Fbuffer)-1 downto ptr+$2E+fnL+exL+adjust do
      Fbuffer[index]:=Fbuffer[index-adjust];
    end;
    //Update the OS type to RISC OS
    Fbuffer[ptr+$05]:=13;
    //Update the extra field length
    Fbuffer[ptr+$1E]:=(exL+adjust)mod$100;
    Fbuffer[ptr+$1F]:=(exL+adjust)>>8;
    //Fill in the extra field
    Fbuffer[ptr+$2E+fnL]:=$41;//A
    Fbuffer[ptr+$2F+fnL]:=$43;//C
    //Length of extra -4
    Fbuffer[ptr+$30+fnL]:=((exL+adjust)-4)mod$100;
    Fbuffer[ptr+$31+fnL]:=((exL+adjust)-4)>>8;
    Fbuffer[ptr+$32+fnL]:=$41;//A
    Fbuffer[ptr+$33+fnL]:=$52;//R
    Fbuffer[ptr+$34+fnL]:=$43;//C
    Fbuffer[ptr+$35+fnL]:=$30;//0
    //Load address
    Fbuffer[ptr+$36+fnL]:=filetozip.LoadAddr mod $100;
    Fbuffer[ptr+$37+fnL]:=(filetozip.LoadAddr>>8)mod $100;
    Fbuffer[ptr+$38+fnL]:=(filetozip.LoadAddr>>16)mod $100;
    Fbuffer[ptr+$39+fnL]:=(filetozip.LoadAddr>>24)mod $100;
    //Exec address
    Fbuffer[ptr+$3A+fnL]:=filetozip.ExecAddr mod $100;
    Fbuffer[ptr+$3B+fnL]:=(filetozip.ExecAddr>>8)mod $100;
    Fbuffer[ptr+$3C+fnL]:=(filetozip.ExecAddr>>16)mod $100;
    Fbuffer[ptr+$3D+fnL]:=(filetozip.ExecAddr>>24)mod $100;
    //Attributes
    Fbuffer[ptr+$3E+fnL]:=filetozip.Attributes;
    //Blank off the rest
    if adjust>0 then
    begin
     for index:=17 to adjust-1 do Fbuffer[ptr+fnL+index+$2E]:=0;
     //Now we need to replicate this extra field into the main header
     SetLength(Fbuffer,Length(Fbuffer)+adjust);//First, extend again
     for index:=Length(Fbuffer)-1 downto dataptr+$1E+fnL+exL+adjust do
      Fbuffer[index]:=Fbuffer[index-adjust];
    end;
    //Now just replicate from above
    inc(ptr,adjust);//Don't forget this has moved too
    for index:=0 to newExL-1 do
     Fbuffer[dataptr+$1E+fnL+index]:=Fbuffer[ptr+$2E+fnL+index];
    //And update the field in the main header
    Fbuffer[dataptr+$1C]:=(exL+adjust)mod$100;
    Fbuffer[dataptr+$1D]:=(exL+adjust)>>8;
    //Get the compressed size of the file
    filetozip.Length:=Length(buffer);
    filetozip.Size:=Fbuffer[dataptr+$12]
                   +Fbuffer[dataptr+$13]<<8
                   +Fbuffer[dataptr+$14]<<16
                   +Fbuffer[dataptr+$15]<<24;
    //And the data offset
    filetozip.DataOffset:=dataptr;
    //Update the End of Central Library
    inc(EoCL,adjust*2);
    //Update Offset of CL
    inc(CL,adjust);
    UpdateCL(CL,EoCL); //Write both fields
    //Save it back again
    SaveData;
    //And now add it to the overall list
    SetLength(FFileList,Length(FFileList)+1);
    FFileList[Length(FFileList)-1]:=filetozip;
    //Set the spark flag
    FIsSpark:=True;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Create an empty directory
-------------------------------------------------------------------------------}
procedure TSpark.CreateDirectory(path: String);
var
 index,
 ctr      : Integer;
 buffer   : TDynByteArray;
 EoCL,CL,
 offset   : Cardinal;
 datetime : QWord;
 year,
 month,
 day,
 hour,
 minute,
 second,
 ms       : Word;
 filetozip: TFileEntry;
const
 headersig: array[0..3] of Byte = ($50,$4B,$03,$04);
 clsig    : array[0..3] of Byte = ($50,$4B,$01,$02);
 eoclsig  : array[0..3] of Byte = ($50,$4B,$05,$06);
begin
 if FIsPack then exit;
 //Make sure that the path is not empty
 if path<>'' then
 begin
  //Ensure it ends with a directory specifier
  if path[Length(path)]<>'/' then path:=path+'/';
  //And does not start with the root
  if Length(path)>2 then if path[1]='$' then path:=Copy(path,3);
  //Set up the buffer for the header entry
  SetLength(buffer,Length(path)+$36);
  for index:=0 to 3 do buffer[index]:=headersig[index];
  //Version
  buffer[4]:=$0A;
  //Blank the rest
  for index:=5 to $1D do buffer[index]:=$00;
  //Modification time $0A/$0B
  DecodeDateTime(Now,year,month,day,hour,minute,second,ms);
  datetime:=(hour<<$B)OR(minute<<5)OR(second div 2);
  buffer[$0A]:=datetime mod $100;
  buffer[$0B]:=datetime>>8;
  //Modification date $0C/$0D
  datetime:=((year-1980)<<9)OR(month<<5)OR day;
  buffer[$0C]:=datetime mod $100;
  buffer[$0D]:=datetime>>8;
  //Filename length
  buffer[$1A]:=Length(path)mod$100;
  buffer[$1B]:=Length(path)>>8;
  //Extra length
  buffer[$1C]:=$18;
  //Filename
  for index:=1 to Length(path) do buffer[$1D+index]:=Ord(path[index]);
  //Fill in the extra field
  buffer[$1E+Length(path)]:=$41;//A
  buffer[$1F+Length(path)]:=$43;//C
  //Length of extra -4
  buffer[$20+Length(path)]:=$14;
  buffer[$21+Length(path)]:=$00;
  buffer[$22+Length(path)]:=$41;//A
  buffer[$23+Length(path)]:=$52;//R
  buffer[$24+Length(path)]:=$43;//C
  buffer[$25+Length(path)]:=$30;//0
  //Load address
  datetime:=((Floor(Now)-2)*8640000)+Floor((Now-Floor(Now))*8640000);
  buffer[$26+Length(path)]:=(datetime>>32)mod$100;//last byte of the time/date
  buffer[$27+Length(path)]:=$FD;
  buffer[$28+Length(path)]:=$FF;
  buffer[$29+Length(path)]:=$FF;
  //Exec address
  buffer[$2A+Length(path)]:= datetime mod $100; //RISC OS time/date
  buffer[$2B+Length(path)]:=(datetime>>8)mod $100;
  buffer[$2C+Length(path)]:=(datetime>>16)mod $100;
  buffer[$2D+Length(path)]:=(datetime>>24)mod $100;
  //Attributes
  buffer[$2E+Length(path)]:=$0F;
  //Blank off the rest
  for index:=0 to 7 do buffer[$2F+Length(path)+index]:=$00;
  //Find the end of the list
  CL:=0;
  EoCL:=FindEoCL(CL);
  //Increase the archive size
  SetLength(Fbuffer,Length(Fbuffer)+Length(buffer));
  //Move the data up
  if EoCL>0 then
   for index:=Length(Fbuffer)-1 downto CL do
    Fbuffer[index]:=Fbuffer[index-Length(buffer)];
  //There is no EoCL, so we need to create one
  if EoCL=0 then
  begin
   EoCL:=Length(Fbuffer);
   SetLength(Fbuffer,Length(Fbuffer)+$16);
   //Write the signature
   for index:=0 to 3 do Fbuffer[EoCL+index]:=eoclsig[index];
   //Reset the EoCL back to 0, for now
   EoCL:=0;
  end;
  //Then insert where the CL was
  for index:=0 to Length(buffer)-1 do Fbuffer[CL+index]:=buffer[index];
  //Remember where we put it
  offset:=CL;
  //Adjust the CL and EoCL
  inc(EoCL,Length(buffer));
  inc(CL,Length(buffer));
  //Update the CL location
  UpdateCL(CL,EoCL);
  //Now add the entry to the central database
  SetLength(buffer,Length(buffer)+$10);
  //Now move data around - start at the end so we don't overwrite anything
  for index:=Length(buffer)-1 downto $2E do buffer[index]:=buffer[index-$10];
  //Middle part
  for index:=$1F downto $06 do buffer[index]:=buffer[index-2];
  //CL signature
  for index:=0 to 3 do buffer[index]:=clsig[index];
  //Version
  buffer[$04]:=$14;
  //OS
  buffer[$05]:=$0D;
  //Zero out the non-required fields
  for index:=$20 to $29 do buffer[index]:=$00;
  //Offset of main entry
  buffer[$2A]:=offset mod$100;
  buffer[$2B]:=(offset>>8)mod$100;
  buffer[$2C]:=(offset>>16)mod$100;
  buffer[$2D]:=(offset>>24)mod$100;
  //Now to insert this into the Central Library
  //Increase the archive size
  SetLength(Fbuffer,Length(Fbuffer)+Length(buffer));
  //Move the data up
  for index:=Length(Fbuffer)-1 downto EoCL do
   Fbuffer[index]:=Fbuffer[index-Length(buffer)];
  //Then insert where the EoCL was
  for index:=0 to Length(buffer)-1 do Fbuffer[EoCL+index]:=buffer[index];
  //And update again
  inc(EoCL,Length(buffer));
  UpdateCL(CL,EoCL);
  //Count the number of entries
  ctr:=0;
  for index:=CL to EoCL do
   if (Fbuffer[index]=$50)
   and(Fbuffer[index+1]=$4B)
   and(Fbuffer[index+2]=$01)
   and(Fbuffer[index+3]=$02)then inc(ctr);
  //Number of entries
  Fbuffer[EoCL+$8]:=ctr mod$100;
  Fbuffer[EoCL+$9]:=(ctr>>8)mod$100;
  Fbuffer[EoCL+$A]:=ctr mod $100;
  Fbuffer[EoCL+$B]:=(ctr>>8)mod$100;
  //Save it back again
  SaveData;
  //Populate the entry
  filetozip.ArchiveName:=path;
  filetozip.Attributes:=$0F;
  filetozip.LoadAddr:=((datetime>>32)mod$100)+$FFFFFD00;
  filetozip.ExecAddr:=datetime and $FFFFFFFF;
  filetozip.DataOffset:=offset;
  filetozip.Directory:=True;
  filetozip.Size:=0;
  filetozip.Length:=0;
  filetozip.NumEntries:=0;
  //Sort the filename and path out for RISC OS
  RISCOSFilename(path,True,filetozip.Filename,filetozip.Parent);
  //And now add it to the overall list
  SetLength(FFileList,Length(FFileList)+1);
  FFileList[Length(FFileList)-1]:=filetozip;
  //Set the spark flag
  FIsSpark:=True;
 end;
end;

{-------------------------------------------------------------------------------
Rename, or move, a file or directory (published)
-------------------------------------------------------------------------------}
function TSpark.RenameFile(oldpath, newpath: String): Boolean;
begin
 Result:=False;
 //We do this to eliminate any false positives. Some will still get through
 if(oldpath<>'')and(newpath<>'')and(oldpath<>newpath)and(Length(Fbuffer)>0)then
 begin
  //Fire off the function
  Result:=RenameTheFile(oldpath,newpath);
  //A directory will return a false result, whatever
  if oldpath[Length(oldpath)]='/' then Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Delete a file/directory (published)
-------------------------------------------------------------------------------}
function TSpark.DeleteFile(filename: String):Boolean;
begin
 Result:=False;
 //We do this to eliminate any false positives. Some will still get through
 if(filename<>'')and(Length(Fbuffer)>0)then
 begin
  //Fire off the function
  Result:=DeleteTheFile(filename);
  //A directory will return a false result, whatever
  if filename[Length(filename)]='/' then Result:=True;
 end;
end;

{-------------------------------------------------------------------------------
Update the load and execution address
-------------------------------------------------------------------------------}
function TSpark.UpdateLoadExecAddress(path: String;load, exec: Cardinal): Boolean;
var
 ptr,
 dataptr : Cardinal;
 fnL     : Word;
 index   : Integer;
begin
 Result:=False;
 if FIsPack then exit;
 //No blanks
 if path<>'' then
 begin
  //Remove the root, if present
  if Length(path)>2 then if path[1]='$' then path:=Copy(path,3);
  //Set up our variables
  ptr:=0;
  dataptr:=0;
  fnL:=Length(path);
  //Find the entry
  if FindEntry(path,False,ptr,dataptr) then
  begin
   //Update the entries (CL)
   Fbuffer[ptr+fnL+$36]:=load mod$100;
   Fbuffer[ptr+fnL+$37]:=(load>>8)mod$100;
   Fbuffer[ptr+fnL+$38]:=(load>>16)mod$100;
   Fbuffer[ptr+fnL+$39]:=(load>>24)mod$100;
   Fbuffer[ptr+fnL+$3A]:=exec mod$100;
   Fbuffer[ptr+fnL+$3B]:=(exec>>8)mod$100;
   Fbuffer[ptr+fnL+$3C]:=(exec>>16)mod$100;
   Fbuffer[ptr+fnL+$3D]:=(exec>>24)mod$100;
   //Update the entries (header)
   Fbuffer[dataptr+fnL+$26]:=load mod$100;
   Fbuffer[dataptr+fnL+$27]:=(load>>8)mod$100;
   Fbuffer[dataptr+fnL+$28]:=(load>>16)mod$100;
   Fbuffer[dataptr+fnL+$29]:=(load>>24)mod$100;
   Fbuffer[dataptr+fnL+$2A]:=exec mod$100;
   Fbuffer[dataptr+fnL+$2B]:=(exec>>8)mod$100;
   Fbuffer[dataptr+fnL+$2C]:=(exec>>16)mod$100;
   Fbuffer[dataptr+fnL+$2D]:=(exec>>24)mod$100;
   //Save the data
   SaveData;
   //Update the entry in the list of files
   if Length(FFileList)>0 then
    for index:=0 to Length(FFileList)-1 do
     if FFileList[index].ArchiveName=path then
     begin
      FFileList[index].LoadAddr:=load;
      FFileList[index].ExecAddr:=exec;
     end;
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update a file's attributes
-------------------------------------------------------------------------------}
function TSpark.UpdateAttributes(path: String; attributes: Word): Boolean;
var
 ptr,
 dataptr : Cardinal;
 fnL     : Word;
 index   : Integer;
begin
 Result:=False;
 if FIsPack then exit;
 //No blanks
 if path<>'' then
 begin
  //Remove the root if present
  if Length(path)>2 then if path[1]='$' then path:=Copy(path,3);
  //Set up our variables
  ptr:=0;
  dataptr:=0;
  fnL:=Length(path);
  //Find the entry
  if FindEntry(path,False,ptr,dataptr) then
  begin
   //Update the entries (CL)
   Fbuffer[ptr+fnL+$3E]:=attributes mod$100;
   //Update the entries (header)
   Fbuffer[dataptr+fnL+$2E]:=attributes mod$100;
   //Save the data
   SaveData;
   //Update the entry in the list of files
   if Length(FFileList)>0 then
    for index:=0 to Length(FFileList)-1 do
     if FFileList[index].ArchiveName=path then
      FFileList[index].Attributes:=attributes;
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Split a path into parent and filename for RISC OS
-------------------------------------------------------------------------------}
procedure TSpark.RISCOSFilename(path: String;addroot: Boolean;
                                       var filename: String;var parent: String);
var
 index: Integer;
begin
 filename:='';
 if addroot then parent:='$' else parent:='';
 //Remove any trailing '/'
 if path[Length(path)]='/' then path:=LeftStr(path,Length(path)-1);
 //Swap the '/' for '.' and vice versa
 SwapDirSep(path);
 //Is there a directory separator?
 if Pos('.',path)=0 then //No
  filename:=path
 else
 begin
  //Yes, find the last one
  index:=Length(path);
  while(path[index]<>'.')and(index>1)do dec(index);
  filename:=Copy(path,index+1);
  if addroot then parent:=parent+'.';
  parent:=parent+Copy(path,1,index-1);
 end;
end;

{-------------------------------------------------------------------------------
Swap directory separators
-------------------------------------------------------------------------------}
procedure TSpark.SwapDirSep(var path: String);
var
 index: Integer;
begin
 for index:=1 to Length(path) do
  if path[index]='/' then path[index]:='.'
  else if path[index]='.' then path[index]:='/';
end;

{-------------------------------------------------------------------------------
Convert an attribute byte to a string (ADFS compatible)
-------------------------------------------------------------------------------}
function TSpark.ConvertAttribute(attr: Byte): String;
var
 cnt : Byte;
 temp: String;
begin
 Result:='';
 //Collate the attributes
 for cnt:=0 to 7 do
  if attr AND(1<<cnt)=1<<cnt then
   Result:=Result+NewAtts[cnt];
 //Reverse the attribute order to match actual ADFS
 temp:='';
 if Length(Result)>0 then
  for cnt:=Length(Result) downto 1 do
   temp:=temp+Result[cnt];
 Result:=temp;
end;

{-------------------------------------------------------------------------------
Convert an attribute string to a byte (ADFS compatible)
-------------------------------------------------------------------------------}
function TSpark.ConvertAttribute(attr: String): Byte;
var
 cnt : Byte;
begin
 //Convert the attributes from a string to a byte
 Result:=0;
 for cnt:=0 to Length(NewAtts)-1 do
  if Pos(NewAtts[cnt],attr)>0 then
   Result:=Result OR 1<<cnt;
end;

end.
