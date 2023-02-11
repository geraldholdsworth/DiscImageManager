//++++++++++++++++++ !Spark ++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Identifies a Spark archive
-------------------------------------------------------------------------------}
function TDiscImage.ID_Spark: Boolean;
begin
 Result:=False;
 if(FFormat=diInvalidImg)or(FSparkAsFS)then
 begin
  if(GetDataLength>0)and(FFilename<>'')then //Any data?
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
function TDiscImage.ReadSparkArchive: Boolean;
var
  index,
  ref   : Integer;
  d,
  e     : Cardinal;
  pnt   : String;
//  OldDisc: TDisc;
begin
 //In order to be able to use FileExists, we need to populate FDisc
// OldDisc:=FDisc; //So take a note
 //We will be returning a TDisc in Result
 Result:=False;
 //Set up the array for the basic root entry
 SetLength(FDisc,1);
 ResetDir(FDisc[0]);
 FDisc[0].Directory:='$';
 FDisc[0].BeenRead:=True;
 //Now go through all the entries in the spark and add them
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
     FDisc[ref].BeenRead:=True;
    end
    else
    begin
     FDisc[ref].Parent:=0;
     FDisc[ref].Sector:=root;
     FDisc[ref].BeenRead:=True;
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
   FDisc[d].Entries[e].Attributes:=
               SparkFile.ConvertAttribute(SparkFile.FileList[index].Attributes);
   //Calculate the timestamp and filetype
   ADFSCalcFileDate(FDisc[d].Entries[e]);
  end;
 end;
 //Disc size (total uncompressed size)
 disc_size[0]:=SparkFile.UncompressedSize;
 //Return a result
 Result:=Length(FDisc)>0;
 //And restore FDisc to what it was
 //FDisc:=OldDisc;
end;

{-------------------------------------------------------------------------------
Extract a file from Spark archive
-------------------------------------------------------------------------------}
function TDiscImage.ExtractSparkFile(filename: String;
                                             var buffer: TDIByteArray): Boolean;
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

{-------------------------------------------------------------------------------
Create a new Spark archive
-------------------------------------------------------------------------------}
function TDiscImage.FormatSpark(Zipfilename: String): Boolean;
begin
 Result:=True;
 if Zipfilename='' then exit;
 //Set up the TDisc structure for return
 SetLength(FDisc,1);
 ResetDir(FDisc[0]);
 //Set the root directory name
 root_name:='$';
 FDisc[0].Directory:=root_name;
 FDisc[0].BeenRead:=True;
 //Set the format
 FFormat:=diSpark<<4;
 //Create a blank file
 SparkFile:=TSpark.Create(Zipfilename,True);
 //Set the filename
 imagefilename:=Zipfilename;
end;

{-------------------------------------------------------------------------------
Delete a file from a Spark archive
-------------------------------------------------------------------------------}
function TDiscImage.DeleteSparkFile(filename: String): Boolean;
var
 index,
 dir,
 entry  : Cardinal;
 olddir : String;
begin
 Result:=False;
 //Ensure the file actually exists
 if FileExists(filename,dir,entry) then
 begin
  //Convert the filename
  SparkFile.SwapDirSep(filename);
  if FDisc[dir].Entries[entry].DirRef<>-1 then filename:=filename+'/';
  if filename[1]='$' then filename:=Copy(filename,3);
  //Delete the file/directory
  SparkFile.DeleteFile(filename);
  //Return a postive result (the above method will return a false)
  Result:=True;
  //This needs to replicate when the Spark class is doing for the local copy
  //i.e., for directories, delete all contents as well as the directory
  if entry<Length(FDisc[dir].Entries)-1 then
   for index:=entry to Length(FDisc[dir].Entries)-2 do
    FDisc[dir].Entries[index]:=FDisc[dir].Entries[index+1];
  SetLength(FDisc[dir].Entries,Length(FDisc[dir].Entries)-1);
  //Did we remove the last child of the directory?
  if Length(FDisc[dir].Entries)=0 then
  begin
   //Then we'll recreate it, as it will have gone
   olddir:=GetParent(dir);
   SparkFile.SwapDirSep(olddir);
   olddir:=olddir+'/';
   SparkFile.CreateDirectory(olddir);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update the file attributes of a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateSparkAttributes(filename,attributes: String): Boolean;
var
 dir,
 entry : Cardinal;
begin
 Result:=False;
 //Ensure the file actually exists
 if FileExists(filename,dir,entry) then
 begin
  //Convert the filename
  SparkFile.SwapDirSep(filename);
  if FDisc[dir].Entries[entry].DirRef<>-1 then filename:=filename+'/';
  if filename[1]='$' then filename:=Copy(filename,3);
  //Update the attributes
  if SparkFile.UpdateAttributes(filename,SparkFile.ConvertAttribute(attributes)) then
  begin
   //And update the local copy of the attributes
   FDisc[dir].Entries[entry].Attributes:=attributes;
   //And set a positive result
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Move a file from one directory to another
-------------------------------------------------------------------------------}
function TDiscImage.MoveSparkFile(filename, dest: String): Integer;
var
 sdir,sentry,
 ddir,dentry : Cardinal;
 olddir      : String;
 entry,
 index       : Integer;
begin
 Result:=-1;
 //Moving is just the same as renaming, but supplying the entire path for both
 //source and destination.
 if FileExists(filename,sdir,sentry) then //Make sure the source file exists
  if FileExists(dest,ddir,dentry) then //And the destination exists
  begin
   //As we are just renaming, tag the filename onto the end of the destination
   dest:=dest+dir_sep+FDisc[sdir].Entries[sentry].Filename;
   //Swap the directory separators for both
   SparkFile.SwapDirSep(filename);
   SparkFile.SwapDirSep(dest);
   //Add the trailing '/' if the source is a directory
   if FDisc[sdir].Entries[sentry].DirRef<>-1 then
   begin
    filename:=filename+'/';
    dest:=dest+'/';
   end;
   //Rename it
   SparkFile.RenameFile(filename,dest);
   //Update the local copy - destination
   entry:=FDisc[ddir].Entries[dentry].DirRef;
   SetLength(FDisc[entry].Entries,Length(FDisc[entry].Entries)+1);
   FDisc[entry].Entries[Length(FDisc[entry].Entries)-1]:=FDisc[sdir].Entries[sentry];
   Result:=Length(FDisc[entry].Entries)-1;
   //Update the local copy - source
   if Length(FDisc[sdir].Entries)=1 then SetLength(FDisc[sdir].Entries,0)
   else
   begin
    //If not the end one
    if sentry<Length(FDisc[sdir].Entries)-1 then
     //Move everything down one
     for index:=sentry to Length(FDisc[sdir].Entries)-2 do
      FDisc[sdir].Entries[index]:=FDisc[sdir].Entries[index+1];
    //And drop the last one off the list
    SetLength(FDisc[sdir].Entries,Length(FDisc[sdir].Entries)-1);
   end;
   //Did we remove the last child of the source directory?
   if Length(FDisc[sdir].Entries)=0 then
   begin
    //Then we'll recreate it, as it will have gone
    olddir:=GetParent(sdir);
    SparkFile.SwapDirSep(olddir);
    olddir:=olddir+'/';
    SparkFile.CreateDirectory(olddir);
   end;
  end;
end;

{-------------------------------------------------------------------------------
Add a file to an existing archive
-------------------------------------------------------------------------------}
function TDiscImage.WriteSparkFile(var file_details: TDirEntry;
                                             var buffer: TDIByteArray): Integer;
var
 dir          : Integer;
 timestamp    : Int64;
 ref,ptr      : Cardinal;
 filetozip    : TFileEntry;
begin
 dir    :=0;
 //Start with a negative result
 Result:=-3;//File already exists
 //First make sure it doesn't exist already
 if not FileExists(file_details.Parent+dir_sep+file_details.Filename,ref)then
  //Get the directory where we are adding it to, and make sure it exists
  if FileExists(file_details.Parent,ref)then
  begin
   //Where we are inserting this into
   if file_details.Parent='$' then
    dir  :=0
   else
    dir  :=FDisc[ref div $10000].Entries[ref mod $10000].DirRef;
   //Has it been read in?
   if not FDisc[dir].BeenRead then ReadDirectory(file_details.Parent);
   //Get the length of the file
   Result:=-7; //Map full
   //Now update the directory (local copy)
   if file_details.filename<>'$' then
   begin
    //Get the number of entries in the directory
    ref:=Length(FDisc[dir].Entries);
    //Convert load/exec address into filetype and datestamp, if necessary
    ADFSCalcFileDate(file_details); //Spark is based on ADFS
    //Now we add the entry into the directory catalogue
    ptr:=ExtendADFSCat(dir,file_details); //Spark is based on ADFS
    //Not a directory
    FDisc[dir].Entries[ptr].DirRef:=-1;
    //Filetype and Timestamp for Arthur and RISC OS ADFS
    if (FDisc[dir].Entries[ptr].LoadAddr=0)
    and(FDisc[dir].Entries[ptr].ExecAddr=0)
    then
    begin
     FDisc[dir].Entries[ptr].LoadAddr:=$FFF00000;
     //Set the filetype, if not already set
     if FDisc[dir].Entries[ptr].ShortFileType<>'' then
     begin
      FDisc[dir].Entries[ptr].LoadAddr:=FDisc[dir].Entries[ptr].LoadAddr OR
         (StrToIntDef('$'+FDisc[dir].Entries[ptr].ShortFileType,0)<<8);
      FDisc[dir].Entries[ptr].FileType:=
       GetFiletypeFromNumber(StrToIntDef('$'+FDisc[dir].Entries[ptr].ShortFileType,0));
     end;
     //Timestamp it, if not already done
     timestamp:=TimeDateToRISCOS(Now);
     FDisc[dir].Entries[ptr].TimeStamp:=Now;
     FDisc[dir].Entries[ptr].LoadAddr:=FDisc[dir].Entries[ptr].LoadAddr OR
         (timestamp DIV $100000000);
     FDisc[dir].Entries[ptr].ExecAddr:=timestamp MOD $100000000;
    end;
    //File imported from DFS, expand the tube address, if needed
    if (FDisc[dir].Entries[ptr].LoadAddr>>16=$00FF)
    and(FDisc[dir].Entries[ptr].ExecAddr>>16=$00FF)then
    begin
     FDisc[dir].Entries[ptr].LoadAddr:=
                             (FDisc[dir].Entries[ptr].LoadAddr AND $0000FFFF)
                             OR$FFFF0000;
     FDisc[dir].Entries[ptr].ExecAddr:=
                             (FDisc[dir].Entries[ptr].ExecAddr AND $0000FFFF)
                             OR$FFFF0000;
    end;
    //Is the file actually a directory?
    if Pos('D',file_details.Attributes)>0 then
    begin
     //Increase the number of directories by one
     SetLength(FDisc,Length(FDisc)+1);
     //Then assign DirRef
     FDisc[dir].Entries[ptr].DirRef:=Length(FDisc)-1;
     //Assign the directory properties
     FDisc[Length(FDisc)-1].Directory:=FDisc[dir].Entries[ptr].Filename;
     FDisc[Length(FDisc)-1].Title    :=FDisc[dir].Entries[ptr].Filename;
     FDisc[Length(FDisc)-1].Broken   :=False;
     FDisc[Length(FDisc)-1].Parent   :=dir;
     FDisc[Length(FDisc)-1].Sector   :=FDisc[dir].Entries[ptr].Sector;
     SetLength(FDisc[Length(FDisc)-1].Entries,0);
    end;
    //And send the result back to the client
    Result:=ptr;
    //Write the result to the ZIP file
    filetozip.Filename:=FDisc[dir].Entries[ptr].Filename; //RISC OS filename
    filetozip.Parent:=FDisc[dir].Entries[ptr].Parent;     //RISC OS parent
    //The actual name in the archive
    filetozip.ArchiveName:=FDisc[dir].Entries[ptr].Parent
                          +'.'+FDisc[dir].Entries[ptr].Filename;
    //Remove the root name, if present
    if LeftStr(filetozip.ArchiveName,2)='$.' then
     filetozip.ArchiveName:=Copy(filetozip.ArchiveName,3);
    //Need to swap ADFS directory and extension separators
    SparkFile.SwapDirSep(filetozip.ArchiveName);
    //Convert the attributes from a string to a byte
    filetozip.Attributes:=
                 SparkFile.ConvertAttribute(FDisc[dir].Entries[ptr].Attributes);
    //Is it a directory?
    filetozip.Directory:=FDisc[dir].Entries[ptr].DirRef<>-1;
    //Load and execution addresses (i.e., filetype and datestamp)
    filetozip.ExecAddr:=FDisc[dir].Entries[ptr].ExecAddr;
    filetozip.LoadAddr:=FDisc[dir].Entries[ptr].LoadAddr;
    //File length
    filetozip.Size:=FDisc[dir].Entries[ptr].Length;
    //Write the file
    SparkFile.WriteFile(filetozip,buffer);
    //Update the used space
    disc_size[0]:=SparkFile.UncompressedSize;
   end else Result:=0;
  end else Result:=-6; //Directory does not exist
end;

{-------------------------------------------------------------------------------
Rename a file
-------------------------------------------------------------------------------}
function TDiscImage.RenameSparkFile(filename, newfilename: String): Integer;
var
 dir,
 entry,
 ptr    : Cardinal;
begin
 Result:=-1;
 //Does the file exist?
 if FileExists(filename,dir,entry) then
  //And the proposed filename not exist?
  if not FileExists(GetParent(dir)+dir_sep+newfilename,ptr) then
  begin
   //Swap the directory separators
   SparkFile.SwapDirSep(filename);
   //Update our local copy
   FDisc[dir].Entries[entry].Filename:=newfilename;
   //Are we renaming a directory?
   if FDisc[dir].Entries[entry].DirRef<>-1 then
   begin
    //If the title is the same, change it also
    if FDisc[FDisc[dir].Entries[entry].DirRef].Title=FDisc[dir].Entries[entry].Filename then
     FDisc[FDisc[dir].Entries[entry].DirRef].Title:=newfilename;
    //And the directory name
    FDisc[FDisc[dir].Entries[entry].DirRef].Directory:=newfilename;
   end;
   //Add the path for the proposed filename
   newfilename:=GetParent(dir)+dir_sep+newfilename;
   //And swap the separators
   SparkFile.SwapDirSep(newfilename);
   //Is it a directory?
   if FDisc[dir].Entries[entry].DirRef<>-1 then
   begin
    //Add the trailing '/'
    filename:=filename+'/';
    newfilename:=newfilename+'/';
   end;
   SparkFile.RenameFile(filename,newfilename);
   //Return the original entry point
   Result:=entry;
  end;
end;

{-------------------------------------------------------------------------------
Update the file type of a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateSparkFileType(filename: String; newtype: String): Boolean;
var
 dir,
 entry,
 load,
 exec  : Cardinal;
 newft : Integer;
begin
 Result:=False;
 load:=0;
 exec:=0;
 //Ensure the file actually exists
 if FileExists(filename,dir,entry) then
 begin
  //Hex number?
  if IntToHex(StrToIntDef('$'+newtype,0),3)<>UpperCase(newtype) then
   newft:=GetFileTypeFromName(newtype) //No, so translate
  else
   newft:=StrToInt('$'+newtype);       //Yes, just convert
  //Valid filetype?
  if newft>=0 then
  begin
   //Get the execution address
   exec:=FDisc[dir].Entries[entry].ExecAddr;
   //Calculate the new load address
   load:=FDisc[dir].Entries[entry].LoadAddr;
   //Set the top 12 bits to indicate filetyped
   load:=load OR$FFF00000;
   //Clear the filetype area, preserving the rest
   load:=load AND$FFF000FF;
   //Set the filetype
   load:=load OR(newft<<8);
   //Convert the filename
   SparkFile.SwapDirSep(filename);
   if FDisc[dir].Entries[entry].DirRef<>-1 then filename:=filename+'/';
   if filename[1]='$' then filename:=Copy(filename,3);
   //Update the load and exec addresses
   if SparkFile.UpdateLoadExecAddress(filename,load,exec) then
   begin
    //And update the local copy of the filetypes
    FDisc[dir].Entries[entry].ShortFileType:=IntToHex(newft,3);
    FDisc[dir].Entries[entry].Filetype:=GetFileTypeFromNumber(newft);
    //And set a positive result
    Result:=True;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update the timestamp of a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateSparkTimeStamp(filename: String;newtimedate:TDateTime): Boolean;
var
 dir,
 entry,
 load,
 exec  : Cardinal;
 rotd  : QWord;
begin
 Result:=False;
 load:=0;
 exec:=0;
 //Ensure the file actually exists
 if FileExists(filename,dir,entry) then
 begin
  //Convert to RISC OS time format
  rotd:=TimeDatetoRISCOS(newtimedate);//RISC OS TimeDate is 40 bits long
  //Calculate the new load address
  load:=FDisc[dir].Entries[entry].LoadAddr;
  //Set the top 12 bits to indicate filetyped, and preserve the rest
  load:=load OR$FFF00000;
  //Clear the bottom 8 bits
  load:=load AND$FFFFFF00;
  //Set the bottom 8 bits with the new time
  load:=load OR(rotd>>32);
  //Now calculate the new exec address
  exec:=rotd AND $FFFFFFFF;
  //Convert the filename
  SparkFile.SwapDirSep(filename);
  if FDisc[dir].Entries[entry].DirRef<>-1 then filename:=filename+'/';
  if filename[1]='$' then filename:=Copy(filename,3);
  //Update the load and exec addresses
  if SparkFile.UpdateLoadExecAddress(filename,load,exec) then
  begin
   //Update the local copy
   FDisc[dir].Entries[entry].LoadAddr:=load;
   FDisc[dir].Entries[entry].ExecAddr:=exec;
   FDisc[dir].Entries[entry].TimeStamp:=newtimedate;
   //Return a positive result
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Update the file type of a file
-------------------------------------------------------------------------------}
function TDiscImage.UpdateSparkFileAddr(filename: String; newaddr: Cardinal;
                                                        load: Boolean): Boolean;
var
 dir,
 entry,
 loadad,
 execad  : Cardinal;
begin
 Result:=False;
 loadad:=0;
 execad:=0;
 //Ensure the file actually exists
 if FileExists(filename,dir,entry) then
 begin
  //Get the current load address
  loadad:=FDisc[dir].Entries[entry].LoadAddr;
  //Get the current exec address
  execad:=FDisc[dir].Entries[entry].LoadAddr;
  //Change the one specified
  if load then loadad:=newaddr else execad:=newaddr;
  //Convert the filename
  SparkFile.SwapDirSep(filename);
  if FDisc[dir].Entries[entry].DirRef<>-1 then filename:=filename+'/';
  //Send to the class
  if SparkFile.UpdateLoadExecAddress(filename,loadad,execad) then
  begin
   //Update the local copy
   FDisc[dir].Entries[entry].LoadAddr:=loadad;
   FDisc[dir].Entries[entry].ExecAddr:=execad;
   //Return a positive result
   Result:=True;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Create a directory
-------------------------------------------------------------------------------}
function TDiscImage.CreateSparkDirectory(filename, parent, attributes: String): Integer;
var
 file_details: TDirEntry;
 buffer      : TDIByteArray;
begin
 buffer:=nil;
 SetLength(buffer,0);
 //Set up the TDirEntry
 file_details.Filename:=filename;
 file_details.Parent:=parent;
 if Pos('D',attributes)<1 then attributes:='D'+attributes;
 file_details.Attributes:=attributes;
 //And just pass to the WriteFile function above
 Result:=WriteSparkFile(file_details,buffer);
end;
