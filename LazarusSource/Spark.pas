unit Spark;

{
TSpark class V1.04
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

uses Classes,SysUtils,Zipper,ExtCtrls;

{$M+}

type
 TDynByteArray = array of Byte;
 type
  TSpark = Class
  private
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
   function ExtractFiles: TFileList;
   function ExtractSparkFiles: TFileList;
   function ExtractPackFiles: TFileList;
   procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
                                                      AItem: TFullZipFileEntry);
   procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
                                                      AItem: TFullZipFileEntry);
   function GetUncompressedSize: Cardinal;
   function IsItSpark: Boolean;
   function ExtractFileDataFromSpark(index: Integer):TDynByteArray;
   function ExtractFileDataFromPack(index: Integer):TDynByteArray;
  published
   constructor Create(filename: String);
   constructor Create(stream: TStream); overload;
   function ExtractFileData(Index: Integer):TDynByteArray;
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

{-------------------------------------------------------------------------------
Create the instance
-------------------------------------------------------------------------------}
constructor TSpark.Create(filename: String);
var
 F: TFileStream;
begin
 //Set the filename
 ZipFilename:=filename;
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
 Fversion   :='1.04';
 FTimeOut   :=30;
 FIsSpark   :=False;
 FIsPack    :=False;
 //Read the zip file into memory from the stream
 SetLength(Fbuffer,Stream.Size);
 Stream.Position:=0;
 Stream.Read(Fbuffer[0],Stream.Size);
 //Check it is a !Spark file
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
// fileoffset,
 ctr,
 fnc        : Cardinal;
 c          : Char;
 fnL,
 exL,
 cmL        : LongWord;
 fn,
 zipfn      : String;
begin
 Result:=nil;
 if FIsSpark then
 begin
  FBitLength:=0; //Not used with !Spark
  //Find the 'End of central library'
  ptr :=Length(Fbuffer)-4; //Start at the end
  EoCL:=0;
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
  and(Fbuffer[ptr+3]=$06)then EoCL:=ptr;
  //Only continue of we have a marker
  if EoCL<>0 then
  begin
   //Now we can get the start of the central library
   CL:=Fbuffer[EoCL+$10]
      +Fbuffer[EoCL+$11]<<8
      +Fbuffer[EoCL+$12]<<16
      +Fbuffer[EoCL+$13]<<24;
   //Set up the result to the number of entries
   SetLength(Result,Fbuffer[EoCL+$0A]+Fbuffer[EoCL+$0B]<<8);
   //Now iterate through to find each entry
   ptr:=0; //Pointer into the central library
   ctr:=0; //File count
   while ptr+CL<EoCL do
   begin
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
    //Get the RISC OS filename, and internal zip name
    fn   :='';
    zipfn:='';
    for fnc:=0 to fnL-1 do
    begin
     c:=chr(Fbuffer[CL+ptr+$2E+fnc]);
     zipfn:=zipfn+c;
     if c='/' then c:='.' //Swap the zip directory separator for a RISC OS one
     else if c='.' then c:=','; //And remove the period for a comma (usually filetype)
     fn:=fn+c;
    end;
    //Determine if this is a directory or not
    if fn[Length(fn)]='.' then
    begin
     Result[ctr].Directory:=True;
     fn:=LeftStr(fn,Length(fn)-1);
    end else Result[ctr].Directory:=False;
    //And remove the parent, if any
    Result[ctr].Parent:='';
    if Pos('.',fn)>0 then
    begin
     fnc:=Length(fn);
     while fn[fnc]<>'.' do dec(fnc);
     Result[ctr].Parent:=LeftStr(fn,fnc-1);
     fn:=Copy(fn,fnc+1);
    end;
    //Set the filename to the RISC OS filename
    Result[ctr].Filename:=fn;
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
 if(index>0)and(index<Length(FFileList))then
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
 F         : TFileStream;
begin
 Result:=nil;
 temp:=False;
 if FIsSpark then
 begin
  //Save the file, if we have no filename
  if ZipFileName='' then
  begin
   ZipFileName:=GetTempFileName;
   F:=TFileStream.Create(ZipFileName,fmCreate);
   F.Write(Fbuffer[0],Length(Fbuffer));
   F.Free;
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
 //https://theblackzone.net/posts/2019/unpacking-packdir-files-on-linux.html
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

end.

