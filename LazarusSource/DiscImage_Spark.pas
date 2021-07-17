//++++++++++++++++++ !Spark ++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a Spark archive
-------------------------------------------------------------------------------}
function TDiscImage.ID_Spark: Boolean;
begin
 Result:=False;
 if(FFormat=diInvalidImg)or(FSparkAsFS)then
 begin
  if GetDataLength>0 then //Any data?
  begin
   //Open the file in TSpark
   SparkFile:=TSpark.Create(FFilename);
   //And make sure it is valid
   if SparkFile.IsSpark then
   begin
    //Set the format
    FFormat:=diSpark<<4;
    FDirType:=diADFSNewDir; //Just to fool some of the methods
   end
   else SparkFile.Free; //Otherwise free the instance
   Result:=FFormat<>diInvalidImg; //Return a result
  end;
 end;
end;

{-------------------------------------------------------------------------------
Read Spark archive
-------------------------------------------------------------------------------}
function TDiscImage.ReadSparkArchive: TDisc;
var
  index,
  cnt,
  ref   : Integer;
  d,
  e     : Cardinal;
  pnt,
  temp  : String;
  OldDisc: TDisc;
const
 NewAtts: array[0..7] of Char = ('R','W','L','D','r','w',' ',' ');
begin
 //In order to be able to use FileExists, we need to populate FDisc
 OldDisc:=FDisc; //So take a note
 //We will be returning a TDisc in Result
 Result:=nil;
 //Set up the array for the basic root entry
 SetLength(FDisc,1);
 ResetDir(FDisc[0]);
 FDisc[0].Directory:='$';
 //Now go through all the entries i the spark and add them
 for index:=0 to Length(SparkFile.FileList)-1 do
 begin
  //Work out the parent, relative to root
  pnt:=SparkFile.FileList[index].Parent;
  if pnt='' then pnt:='$' else pnt:='$.'+pnt;
  //Directory reference, default
  ref:=-1;
  //Now, does the parent exist?
  if FileExists(pnt,d,e) then
  begin
   //Are we adding a directory
   if SparkFile.FileList[index].Directory then
   begin
    //Take a note of the array reference
    ref:=Length(FDisc);
    //And increase the array length
    SetLength(FDisc,ref+1);
    //Reset the new entry
    ResetDir(FDisc[ref]);
    //And add the filename
    FDisc[ref].Directory:=SparkFile.FileList[index].Filename;
    if d<>$FFFF then
    begin
     FDisc[ref].Parent:=FDisc[d].Entries[e].DirRef;
     FDisc[ref].Sector:=FDisc[d].Entries[e].Sector;
    end
    else
    begin
     FDisc[ref].Parent:=0;
     FDisc[ref].Sector:=root;
    end;
   end;
   //FileExists returns pointers to what it found
   if pnt='$' then d:=0 //But we need to up it to take account of root
   else d:=FDisc[d].Entries[e].DirRef;
   //Get the pointer into the entries
   e:=Length(FDisc[d].Entries);
   //And extend the entries array
   SetLength(FDisc[d].Entries,e+1);
   //Populate it
   FDisc[d].Entries[e].Filename:=SparkFile.FileList[index].Filename;
   FDisc[d].Entries[e].Parent:=pnt;
   FDisc[d].Entries[e].LoadAddr:=SparkFile.FileList[index].LoadAddr;
   FDisc[d].Entries[e].ExecAddr:=SparkFile.FileList[index].ExecAddr;
   FDisc[d].Entries[e].Length:=SparkFile.FileList[index].Length;
   FDisc[d].Entries[e].Sector:=SparkFile.FileList[index].DataOffset;
   FDisc[d].Entries[e].Track:=Index;//Reference into the archive
   FDisc[d].Entries[e].DirRef:=ref; //Directory reference from earlier
   //Collate the attributes
   temp:='';
   for cnt:=0 to 7 do
    if IsBitSet(SparkFile.FileList[index].Attributes,cnt) then
     temp:=temp+NewAtts[cnt];
   //Reverse the attribute order to match actual ADFS
   if Length(temp)>0 then
    for cnt:=Length(temp) downto 1 do
     FDisc[d].Entries[e].Attributes:=FDisc[d].Entries[e].Attributes+temp[cnt];
   //Calculate the timestamp and filetype
   ADFSCalcFileDate(FDisc[d].Entries[e]);
  end;
 end;
 //Disc size (total uncompressed size)
 disc_size[0]:=SparkFile.UncompressedSize;
 //Return a result
 Result:=FDisc;
 //And restore FDisc to what it was
 FDisc:=OldDisc;
end;

{-------------------------------------------------------------------------------
Extract a file from Spark archive
-------------------------------------------------------------------------------}
function TDiscImage.ExtractSparkFile(filename: String;var buffer: TDIByteArray): Boolean;
var
 d,e  : Cardinal;
 index: Integer;
begin
 //Default return result
 Result   :=False;
 //Does the file actually exist?
 if FileExists(filename,d,e) then
 begin
  //And is within range
  if d<Length(FDisc) then
   if e<Length(FDisc[d].Entries) then
   begin
    //Get the index, stashed away in the track parameter
    index:=FDisc[d].Entries[e].Track;
    //And inflate the file
    buffer:=SparkFile.ExtractFileData(index);
    //Return an OK, if something got read
    Result:=Length(buffer)>0;
   end;
 end;
end;
