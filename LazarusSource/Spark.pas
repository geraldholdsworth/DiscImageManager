unit Spark;

{
TSpark class
Decompress a Zip archive, preserving the extra RISC OS information, as per David
Pilling's !Spark on RISC OS.

Copyright (C) 2021 Gerald Holdsworth gerald@hollypops.co.uk

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
     Size       : Cardinal;    //Compressed size
     Attributes : Byte;        //File attributes (hex)
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
   FIsSpark    : Boolean;      //Is it a valid archive?
   FFileList   : TFileList;    //List of files in archive
   FProgress   : TProgressProc;//Progress feedback
   function ExtractFiles: TFileList;
   procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
                                                      AItem: TFullZipFileEntry);
   procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
                                                      AItem: TFullZipFileEntry);
  published
   constructor Create(filename: String);
   function ExtractFileData(Index: Integer):TDynByteArray;
   property IsSpark: Boolean read FIsSpark;
   property FileList: TFileList read FFileList;
   property ProgressIndicator:   TProgressProc write FProgress;
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
 inherited Create;
 //Initialise the variables
 Fbuffer:=nil;
 ZipFilename:=filename;
 //Read the zip file into memory
 F:=TFileStream.Create(ZipFilename,fmOpenRead or fmShareDenyNone);
 SetLength(Fbuffer,F.Size);
 F.Read(Fbuffer[0],F.Size);
 F.Free;
 //Check it is a ZIP file
 if (Fbuffer[0]=$50)
 and(Fbuffer[1]=$4B)
 and(Fbuffer[2]=$03)
 and(Fbuffer[3]=$04)then
 begin
  //and mark as such
  FIsSpark:=True;
  //The extract the file details
  FFileList:=ExtractFiles;
 end
 else
 begin
  //Otherwise mark as false
  FIsSpark:=False;
  //And clear the buffer
  SetLength(Fbuffer,0);
 end;
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
var
 ptr,
 EoCL,
 CL,
 fileoffset,
 ctr,
 fnc        : Cardinal;
 OS         : Byte;
 c          : Char;
 fnL,
 exL,
 cmL        : LongWord;
 fn,
 zipfn      : String;
begin
 Result:=nil;
 if not FIsSpark then exit;
 //Find the 'End of central library'
 ptr:=Length(Fbuffer)-4; //Start at the end
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
 and(Fbuffer[ptr+3]=$06)then
  EoCL:=ptr;
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
   OS :=Fbuffer[CL+ptr+$05];    //OS (13=RISC OS)
   fnL:=Fbuffer[CL+ptr+$1C]
       +Fbuffer[CL+ptr+$1D]<<8; //Length of filename
   exL:=Fbuffer[CL+ptr+$1E]
       +Fbuffer[CL+ptr+$1F]<<8; //Length of extra field
   cmL:=Fbuffer[CL+ptr+$20]
       +Fbuffer[CL+ptr+$21]<<8; //Length of comment field
   fileoffset:=Fbuffer[CL+ptr+$2A]               //Offset to file header
              +Fbuffer[CL+ptr+$2B]<< 8
              +Fbuffer[CL+ptr+$2C]<<16
              +Fbuffer[CL+ptr+$2D]<<24;
   Result[ctr].Size:=Fbuffer[fileoffset+$12]            //Compressed size
                    +Fbuffer[fileoffset+$13]<< 8
                    +Fbuffer[fileoffset+$14]<<16
                    +Fbuffer[fileoffset+$15]<<24;
   Result[ctr].Length:=Fbuffer[fileoffset+$16]          //Uncompressed size
                      +Fbuffer[fileoffset+$17]<< 8
                      +Fbuffer[fileoffset+$18]<<16
                      +Fbuffer[fileoffset+$19]<<24;
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
   //RISC OS stuff
   if OS=13 then
   begin
    //Extra fields - starts with 'AC'$14$00'ARC0'
    Result[ctr].LoadAddr  :=Fbuffer[CL+ptr+$2E+fnL+$08]     //Load Address
                           +Fbuffer[CL+ptr+$2E+fnL+$09]<< 8
                           +Fbuffer[CL+ptr+$2E+fnL+$0A]<<16
                           +Fbuffer[CL+ptr+$2E+fnL+$0B]<<24;
    Result[ctr].ExecAddr  :=Fbuffer[CL+ptr+$2E+fnL+$0C]     //Execution Address
                           +Fbuffer[CL+ptr+$2E+fnL+$0D]<<8
                           +Fbuffer[CL+ptr+$2E+fnL+$0E]<<16
                           +Fbuffer[CL+ptr+$2E+fnL+$0F]<<24;
    Result[ctr].Attributes:=Fbuffer[CL+ptr+$2E+fnL+$10];    //Attributes
   end;
   //Move the pointer onto the next file
   inc(ptr,fnL+exL+cmL+$2E);
   //And next entry
   inc(ctr);
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
Extract, and uncompress, the actual data
-------------------------------------------------------------------------------}
function TSpark.ExtractFileData(Index: Integer):TDynByteArray;
var
 ZipFile   : TUnZipper;
 sl        : TStringList;
 starttime,
 nowtime   : TDateTime;
begin
 Result:=nil;
 SetLength(Fcache,0);
 //Make sure the index is in range
 if(Index>=0)and(Index<Length(FFileList))then
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
    until(Length(Fcache)>0)or(nowtime-starttime>=30);
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
end;

end.

