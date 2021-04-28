unit MainUnit;

{
Disc Image Manager written by Gerald Holdsworth
Started out as a demo for the TDiscImage class but blossomed into a full-blown
application, ported across from Delphi to Lazarus and hence then compiled on
macOS and Linux in addition to the original Windows version. The underlying class
has also grown from being just a reader to also a writer.
Extra 'gimmicks' have been added over time, to utilise the code in the underlying
class.

Copyright (C) 2018-2021 Gerald Holdsworth gerald@hollypops.co.uk

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

{$MODE objFPC}{$H+}

interface

uses
  SysUtils,Classes,Graphics,Controls,Forms,Dialogs,StdCtrls,DiscImage,Global,
  DiscImageUtils,ExtCtrls,Buttons,ComCtrls,Menus,DateUtils,ImgList,StrUtils,
  Clipbrd,HexDumpUnit,Spark;

type
 //We need a custom TTreeNode, as we want to tag on some extra information
 TMyTreeNode = class(TTreeNode)
  private
   FParentDir : Integer;
   FIsDir     : Boolean;
  public
   property ParentDir: Integer read FParentDir write FParentDir;//Parent directory reference
   property IsDir    : Boolean read FIsDir write FIsDir;        //Is it a directory
 end;
 //Form definition

  { TMainForm }

  TMainForm = class(TForm)
   cb_C64_c: TCheckBox;
   cb_C64_l: TCheckBox;
   cb_DFS_l: TCheckBox;
   cb_ADFS_pubp: TCheckBox;
   cb_ADFS_ownw: TCheckBox;
   cb_ADFS_ownr: TCheckBox;
   cb_ADFS_ownl: TCheckBox;
   cb_ADFS_owne: TCheckBox;
   cb_ADFS_pubr: TCheckBox;
   cb_ADFS_pubw: TCheckBox;
   cb_ADFS_pube: TCheckBox;
   C64AttributeLabel: TLabel;
   ed_title: TEdit;
   FilenameLabel: TLabel;
   icons: TImageList;
   FileImages: TImageList;
   StateIcons: TImageList;
   lb_FileName: TLabel;
   menuFileSearch: TMenuItem;
   ADFSAttrPanel: TPanel;
   DFSAttrPanel: TPanel;
   C64AttrPanel: TPanel;
   DFSAttributeLabel: TLabel;
   FilenamePanel: TPanel;
   FiletypePanel: TPanel;
   ExecAddrPanel: TPanel;
   LoadAddrPanel: TPanel;
   LengthPanel: TPanel;
   DirTitlePanel: TPanel;
   LocationPanel: TPanel;
   CRC32Panel: TPanel;
   ToolSplitter: TSplitter;
   TimeStampPanel: TPanel;
   ParentPanel: TPanel;
   TextureTile: TImage;
   MiscButtons: TImageList;
   imgCopy: TImage;
   PubAttributeLabel: TLabel;
   CRC32Label: TLabel;
   OAAttributeLabel: TLabel;
   lb_CRC32: TLabel;
   Main_Menu: TMainMenu;
   ImageMenu: TMenuItem;
   menuCloseImage: TMenuItem;
   HexDumpMenu: TMenuItem;
   HexDump1: TMenuItem;
   menuHexDump: TMenuItem;
   menuFixADFS: TMenuItem;
   menuSplitDFS: TMenuItem;
   btn_FileSearch: TToolButton;
   ToolsMenu: TMenuItem;
   menuSaveAsCSV: TMenuItem;
   menuRenameFile: TMenuItem;
   menuNewDir: TMenuItem;
   menuDeleteFile: TMenuItem;
   menuAbout: TMenuItem;
   FilesMenu: TMenuItem;
   HelpMenu: TMenuItem;
   menuNewImage: TMenuItem;
   menuOpenImage: TMenuItem;
   menuSaveImage: TMenuItem;
   menuImageDetails: TMenuItem;
   menuExtractFile: TMenuItem;
   menuAddFile: TMenuItem;
   SaveCSV: TSaveDialog;
   sb_Clipboard: TSpeedButton;
   DelayTimer: TTimer;
   ToolBarImages: TImageList;
   AddNewFile: TOpenDialog;
   SaveImage: TSaveDialog;
   MainToolBar: TToolBar;
   btn_OpenImage: TToolButton;
   btn_SaveImage: TToolButton;
   btn_Delete: TToolButton;
   btn_Rename: TToolButton;
   btn_AddFiles: TToolButton;
   btn_NewImage: TToolButton;
   btn_ImageDetails: TToolButton;
   btn_NewDirectory: TToolButton;
   btn_CloseImage: TToolButton;
   btn_SaveAsCSV: TToolButton;
   btn_SplitDFS: TToolButton;
   btn_FixADFS: TToolButton;
   btn_HexDump: TToolButton;
   ToolButton3: TToolButton;
   btn_download: TToolButton;
   ToolButton5: TToolButton;
   btn_About: TToolButton;
   OpenImageFile: TOpenDialog;
   ExtractDialogue: TSaveDialog;
   DirList: TTreeView;
   FileInfoPanel: TPanel;
   img_FileType: TImage;
   lb_FileType: TLabel;
   FileTypeLabel: TLabel;
   ExecAddrLabel: TLabel;
   lb_execaddr: TLabel;
   lb_loadaddr: TLabel;
   LoadAddrLabel: TLabel;
   lb_timestamp: TLabel;
   LengthLabel: TLabel;
   TimestampLabel: TLabel;
   lb_length: TLabel;
   lb_Parent: TLabel;
   ParentLabel: TLabel;
   lb_title: TLabel;
   DirTitleLabel: TLabel;
   LocationLabel: TLabel;
   lb_location: TLabel;
   FileDetailsLabel: TLabel;
   ImageContentsPanel: TPanel;
   lb_contents: TLabel;
   File_Menu: TPopupMenu;
   ExtractFile1: TMenuItem;
   RenameFile1: TMenuItem;
   DeleteFile1: TMenuItem;
   ImageDetails: TStatusBar;
   AddFile1: TMenuItem;
   NewDirectory1: TMenuItem;
   //Events - mouse clicks
   procedure btn_CloseImageClick(Sender: TObject);
   procedure btn_FileSearchClick(Sender: TObject);
   procedure btn_FixADFSClick(Sender: TObject);
   procedure btn_ImageDetailsClick(Sender: TObject);
   procedure btn_NewDirectoryClick(Sender: TObject);
   procedure btn_SaveAsCSVClick(Sender: TObject);
   procedure btn_SplitDFSClick(Sender: TObject);
   procedure FileInfoPanelResize(Sender: TObject);
   procedure HexDumpSubItemClick(Sender: TObject);
   procedure lb_titleClick(Sender: TObject);
   procedure btn_NewImageClick(Sender: TObject);
   procedure btn_SaveImageClick(Sender: TObject);
   procedure AttributeChangeClick(Sender: TObject);
   procedure RenameFile1Click(Sender: TObject);
   procedure DeleteFile1Click(Sender: TObject);
   procedure btn_downloadClick(Sender: TObject);
   procedure btn_OpenImageClick(Sender: TObject);
   procedure sb_ClipboardClick(Sender: TObject);
   procedure btn_AboutClick(Sender: TObject);
   procedure DirListDblClick(Sender: TObject);
   procedure AddFile1Click(Sender: TObject);
   //Events - TTreeView Drag & Drop
   procedure DirListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
   procedure DirListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
   procedure DirListMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
   procedure DraggedItemMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
   procedure DraggedItemMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
   procedure DirListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
   procedure DirListMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
   procedure DelayTimerTimer(Sender: TObject);
   //Events - Form
   procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
   procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
   procedure FormShow(Sender: TObject);
   procedure FormCreate(Sender: TObject);
   //Events - Other
   procedure ed_titleEditingDone(Sender: TObject);
   procedure DirListCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
   procedure DirListEditingEnd(Sender: TObject; Node: TTreeNode;
    Cancel: Boolean);
   procedure DirListGetImageIndex(Sender: TObject; Node: TTreeNode);
   procedure DirListChange(Sender: TObject; Node: TTreeNode);
   procedure DirListEditing(Sender: TObject; Node: TTreeNode;
     var AllowEdit: Boolean);
   procedure DirListCustomDraw(Sender: TCustomTreeView; const ARect: TRect;
    var DefaultDraw: Boolean);
   procedure DirListCustomDrawArrow(Sender: TCustomTreeView;
    const ARect: TRect; ACollapsed: Boolean);
   procedure DirListCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
    State: TCustomDrawState; var DefaultDraw: Boolean);
   procedure ImageDetailsDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
    const Rect: TRect);
   procedure FileInfoPanelPaint(Sender: TObject);
   //Misc
   function CreateDirectory(dirname,attr: String): TTreeNode;
   procedure ImportFiles(NewImage: TDiscImage);
   function QueryUnsaved: Boolean;
   function GetFilePath(Node: TTreeNode): String;
   procedure DeleteFile(confirm: Boolean);
   procedure DisableControls;
   procedure ParseCommandLine(cmd: String);
   procedure ResetFileFields;
   procedure ExtractFiles(ShowDialogue: Boolean);
   function GetImageFilename(dir,entry: Integer): String;
   function GetWindowsFilename(dir,entry: Integer): String;
   procedure DownLoadFile(dir,entry: Integer; path: String);
   procedure CreateINFFile(dir,entry: Integer; path: String);
   procedure DownLoadDirectory(dir,entry: Integer; path: String);
   procedure OpenImage(filename: String);
   procedure AddDirectoryToTree(CurrDir: TTreeNode; dir: Integer; var highdir: Integer);
   procedure ShowNewImage(title: String);
   function ConvertToKMG(size: Int64): String;
   function IntToStrComma(size: Int64): String;
   procedure ValidateFilename(var f: String);
   procedure SelectNode(filename: String);
   procedure CloseAllHexDumps;
   function AddFileToTree(ParentNode: TTreeNode;importfilename: String;
                                  index: Integer;dir: Boolean): TTreeNode;
   procedure AddDirectoryToImage(dirname: String);
   procedure AddSparkToImage(filename: String);
   function AddFileToImage(filename: String):Boolean;
   function AddFileToImage(filename: String;filedetails: TDirEntry;
                               buffer:TDIByteArray=nil):Boolean; overload;
   procedure UpdateImageInfo;
   procedure ArrangeFileDetails;
   procedure ReportError(error: String);
   function AskConfirm(confim,okbtn,cancelbtn,ignorebtn: String): TModalResult;
   procedure ShowInfo(info: String);
   function GetCopyMode(Shift: TShiftState): Boolean;
   function GetNodeAt(Y: Integer): TTreeNode;
   procedure UpdateProgress(Fupdate: String);
   procedure TileCanvas(c: TCanvas);
   procedure TileCanvas(c: TCanvas;rc: TRect); overload;
  private
   var
    //To keep track of renames
    PathBeforeEdit,
    NameBeforeEdit:String;
    //Stop the checkbox OnClick from firing when just changing the values
    DoNotUpdate   :Boolean;
    //Has the image changed since last saved?
    HasChanged    :Boolean;
    //Item being dragged on Directory List
    DraggedItem   :TTreeNode;
    //To keep track of if we are dragging and if the mouse button is down
    IsDragging,
    MouseIsDown   :Boolean;
    //To remember where we started with the drag operation
    CursorPos     :TPoint;
    //The mouse 'cursor' while we are dragging
    ObjectDrag    :TImage;
    //Keep track of which hex dump windows are open
    HexDump       :array of THexDumpForm;
    //Flag whether to close the application when there are command line params
    KeepOpen      :Boolean;
    //Reporting of errors
    ErrorReporting:Boolean;
    //Delay flag
    progsleep     :Boolean;
   const
    //RISC OS Filetypes - used to locate the appropriate icon in the ImageList
    FileTypes: array[3..140] of String =
                              ('0E1','1A6','1AD','3FB','004','6A2','11D','18A',
                               '19B','68E','69A','69B','69C','69D','69E','102',
                               '108','132','190','191','194','195','196','690',
                               '691','692','693','694','695','696','697','698',
                               '699','A91','AAD','ABA','ABF','ACA','AD0','ADF',
                               'AE4','AE6','AE7','AE9','AF1','AFF','B2F','B9F',
                               'B22','B24','B25','B26','B27','B28','B60','B61',
                               'B62','BA6','BBC','BD6','BD9','BDA','BDF','BE8',
                               'BF8','C1D','C1E','C7D','C27','C32','C46','C85',
                               'CAE','CE5','D00','D01','D3C','D94','DB0','DDC',
                               'DEA','DFE','F7A','F7B','F7E','F7F','F9D','F9E',
                               'F9F','F78','F79','F80','F83','F89','F91','FAE',
                               'FAF','FB0','FB1','FB2','FB4','FB5','FC2','FC3',
                               'FC6','FC8','FCA','FCC','FCD','FCE','FD3','FD4',
                               'FD6','FD7','FDC','FE1','FE4','FE5','FE6','FEA',
                               'FEB','FEC','FED','FF0','FF1','FF2','FF4','FF5',
                               'FF6','FF7','FF8','FF9','FFA','FFB','FFC','FFD',
                               'FFE','FFF');
    //To add more filetypes, increase the array, then ensure that 'riscoshigh'
    //(below) matches. Finally, make sure they are in the correct order in the
    //ImageList components: FileImages
    Applications: array[152..184] of String =
                              ('!alarm','!boot','!chars','!closeup','!configure',
                               '!dpingscan','!draw','!edit','!flasher','!fontprint',
                               '!fonts','!help','!hform','!hopper','!maestro',
                               '!netsurf','!omni','!paint','!printedit',
                               '!printers','!puzzle','!resetboot','!routines',
                               '!scicalc','!serialdev','!showscrap','!sparkfs',
                               '!sparkfs1','!sparkfs2','!sparkfs3','!squash',
                               '!system','!t1tofont');
    //Windows extension - used to translate from RISC OS to Windows
    Extensions: array[1..42] of String =
                         ('004aim','132ico' ,'190dsk' ,'198z80'   ,'1A6CPM',
                          '692img','693iff' ,'695gif' ,'697pcx'   ,'698qrt',
                          '699mtv','69Cbmp' ,'69Dtga' ,'69Epbm'   ,'A91zip',
                          'AADsvg','ABFcab' ,'ADFpdf' ,'B60png'   ,'BBCrom',
                          'C46tar','C85jpg' ,'C85jpeg','DDCarc'   ,'DDCarchive',
                          'DEAdxf','F76edid','F78jng' ,'F79css'   ,'F81js',
                          'F83mng','F91uri' ,'FAFhtm' ,'FAFhtml'  ,'FE4dos',
                          'FF0tif','FF0tiff','FF6ttf' ,'FF9sprite','FFBbas',
                          'FFDdat','FFFtxt');
    //These point to certain icons used when no filetype is found, or non-ADFS
    //The numbers are indexes into the TImageList components 'FileSizeTypes'
    //and 'FileImages' (which are smaller versions of the above.
    appicon     =   0;
    directory   =   1;
    directory_o =   2;
    riscoshigh  = 140;
    //images 3 to 139 inclusive are the known filetypes, listed above
    loadexec    = riscoshigh+ 1; //RISC OS file with Load/Exec specified
    unknown     = riscoshigh+ 3; //RISC OS unknown filetype
    nonadfs     = riscoshigh+ 5; //All other filing systems, except for D64/D71/D81
    prgfile     = riscoshigh+ 4; //D64/D71/D81 PRG file
    seqfile     = riscoshigh+ 5; //D64/D71/D81 SEQ file
    usrfile     = riscoshigh+ 6; //D64/D71/D81 USR file
    delfile     = riscoshigh+ 7; //D64/D71/D81 DEL file
    relfile     = riscoshigh+ 5; //D64/D71/D81 REL file
    cbmfile     = riscoshigh+ 5; //D64/D71/D81 CBM file
    mmbdisc     = riscoshigh+ 9; //MMFS Disc with image
    mmbdisclock = riscoshigh+10; //MMFS Locked disc
    mmbdiscempt = riscoshigh+11; //MMFS Empty slot
    appstart    = riscoshigh+12; //Common application sprites
    //Icons for status bar - index into TImageList 'icons'
    changedicon   = 0;
    acornlogo     = 1;
    amigalogo     = 2;
    bbclogo       = 3;
    commodorelogo = 4;
    riscoslogo    = 5;
    sinclairlogo  = 6;
  public
   //The image
   Image: TDiscImage;
   const
    //Application Title
    ApplicationTitle   = 'Disc Image Manager';
    ApplicationVersion = '1.24';
   procedure AfterConstruction; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  AboutUnit,NewImageUnit,ImageDetailUnit,ProgressUnit,SplitDFSUnit,SearchUnit,
  CustomDialogueUnit;

{------------------------------------------------------------------------------}
//Rescale the form
{------------------------------------------------------------------------------}
procedure TMainForm.AfterConstruction;
var
 i: Integer;
begin
{
See: http://zarko-gajic.iz.hr/delphi-high-dpi-road-ensuring-your-ui-looks-correctly/
and https://wiki.lazarus.freepascal.org/High_DPI
}
 inherited;
 if Screen.PixelsPerInch<>DesignTimePPI then
 begin
  //Status Bar
  ImageDetails.Height:=
                  Round(ImageDetails.Height*Screen.PixelsPerInch/DesignTimePPI);
  ImageDetails.Panels[0].Width:=ImageDetails.Height;
  for i:=0 to -1+ImageDetails.Panels.Count do
   ImageDetails.Panels[i].Width:=
         Round(ImageDetails.Panels[i].Width*Screen.PixelsPerInch/DesignTimePPI);
  MainToolBar.ImagesWidth:=
              Round(MainToolBar.ImagesWidth*Screen.PixelsPerInch/DesignTimePPI);
  //Can use TMonitor.PixelsPerInch to scale to a big monitor
 end;
end;

{------------------------------------------------------------------------------}
//Add a new file to the disc image
{------------------------------------------------------------------------------}
procedure TMainForm.AddFile1Click(Sender: TObject);
var
  i: Integer;
  files: array of String;
begin
 //Open the dialogue box
 if AddNewFile.Execute then
  if AddNewFile.Files.Count>0 then
  begin
   //Set up the string array
   SetLength(files,AddNewFile.Files.Count);
   //Add all the filenames to it
   for i:=0 to AddNewFile.Files.Count-1 do
    files[i]:=AddNewFile.Files[i];
   //Now pass this onto the Form's Drop Files procedure to handle
   FormDropFiles(Sender,files);
  end;
end;

{------------------------------------------------------------------------------}
//Add a directory to an image
{------------------------------------------------------------------------------}
procedure TMainForm.AddDirectoryToImage(dirname: String);
var
 OriginalNode,
 NewNode      : TTreeNode;
 inffile,
 attr,
 importname   : String;
 F            : TFileStream;
 chr          : Char;
 fields       : array of String;
 Dir          : TSearchRec;
begin
 //First, if there is no selection, make one, or if multiple, select the root
 if (DirList.SelectionCount=0) OR (DirList.SelectionCount>1) then
 begin
  DirList.ClearSelection;
  DirList.Items[0].Selected:=True;
 end;
 OriginalNode:=DirList.Selected;
 //If ADFS, create the directory, then select it
 if Image.FormatNumber shr 4=1 then
 begin
  importname:=ExtractFilename(dirname);
  attr      :='DLR';
  //Is there an inf file?
  if FileExists(dirname+'.inf') then
  begin
   inffile:='';
   //Read in the first line
   try
    F:=TFileStream.Create(dirname+'.inf',fmOpenRead OR fmShareDenyNone);
    F.Position:=0;
    while (F.Read(chr,1)=1) and (Ord(chr)>31) and (Ord(chr)<127) do
     inffile:=inffile+chr;
    F.Free;
    fields:=BreakDownInf(inffile);
    //Then extract the fields
    if Length(fields)>0 then importname:=fields[0];
    if Length(fields)>4 then attr      :=fields[4];
   except
    //Could not load
   end;
  end;
  //Convert a Windows filename to a BBC filename
  WinToBBC(importname);
  //Remove spaces for non-big directories, and ensure is 10 chars or less
  if Image.DirectoryType<>2 then
  begin
   importname:=ReplaceStr(importname,' ','_');
   importname:=LeftStr(importname,10);
  end;
  //Create the directory
  NewNode:=CreateDirectory(importname,attr);
  if NewNode<>nil then //Success
  begin
   //And select it
   DirList.ClearSelection;
   NewNode.Selected:=True;
   //Now we import everything inside this
   FindFirst(dirname+pathdelim+'*',faDirectory,Dir);
   repeat
    if (Dir.Name<>'.') and (Dir.Name<>'..') then
    begin
     if (Dir.Attr AND faDirectory)=faDirectory then
      AddDirectoryToImage(dirname+pathdelim+Dir.Name)
     else
      AddFileToImage(dirname+pathdelim+Dir.Name);
    end;
   until FindNext(Dir)<>0;
   FindClose(Dir);
  end;
 end;
 //Revert to the original selection
 DirList.ClearSelection;
 OriginalNode.Selected:=True;
end;

{------------------------------------------------------------------------------}
//Add a spark archive to an image
{------------------------------------------------------------------------------}
procedure TMainForm.AddSparkToImage(filename: String);
var
 SparkFile  : TSpark;
 Index      : Integer;
 filedetails: TDirEntry;
 ParentDir  : String;
 buffer     : TDIByteArray;
begin
 //Make sure the file exists
 if FileExists(filename) then
 begin
  //Open up as a spark archive
  SparkFile:=TSpark.Create(filename);
  //And make sure it is valid
  if SparkFile.IsSpark then
  begin
   //Get, or set, the selected node
   if (DirList.SelectionCount=0) OR (DirList.SelectionCount>1) then
   begin
    DirList.ClearSelection;
    DirList.Items[0].Selected:=True;
   end;
   ParentDir:=GetImageFilename(TMyTreeNode(DirList.Selected).ParentDir,
                                           DirList.Selected.Index);
   //And make sure that there are some entries
   if Length(SparkFile.FileList)>0 then
    for Index:=0 to Length(SparkFile.FileList)-1 do
    begin
     //Select the parent node
     if SparkFile.FileList[Index].Parent<>'' then
      SelectNode(ParentDir+Image.DirSep+SparkFile.FileList[Index].Parent)
     else
      SelectNode(ParentDir);
     filedetails.Attributes:=GetAttributes(
                               IntToHex(SparkFile.FileList[Index].Attributes,2),
                               Image.FormatNumber>>4);
     //Add a directory
     if SparkFile.FileList[Index].Directory then
      CreateDirectory(SparkFile.FileList[Index].Filename,'D'+filedetails.Attributes);
     //Add a file
     if not SparkFile.FileList[Index].Directory then
     begin
      //Get the data
      buffer:=SparkFile.ExtractFileData(Index);
      //Set up the Dir Entry
      if SparkFile.FileList[Index].Filename<>'' then //We need a filename
      begin
       filedetails.Filename  :=SparkFile.FileList[Index].Filename;
       filedetails.LoadAddr  :=SparkFile.FileList[Index].LoadAddr;
       filedetails.ExecAddr  :=SparkFile.FileList[Index].ExecAddr;
       filedetails.Length    :=SparkFile.FileList[Index].Length;
       //Add the file
       if not AddFileToImage('',filedetails,buffer) then exit;
      end;
     end;
    end;
  end;
  SparkFile.Free;
 end;
end;

{------------------------------------------------------------------------------}
//Add a file to an image
{------------------------------------------------------------------------------}
function TMainForm.AddFileToImage(filename: String):Boolean;
var
 filedetails: TDirEntry;
begin
 filedetails.Attributes:='';
 filedetails.LoadAddr:=0;
 filedetails.ExecAddr:=0;
 filedetails.Filename:='';
 Result:=AddFileToImage(filename,filedetails);
end;
function TMainForm.AddFileToImage(filename:String;filedetails: TDirEntry;
                               buffer:TDIByteArray=nil):Boolean;
var
  NewFile        : TDirEntry;
  index          : Integer;
  side           : Cardinal;
  i              : Byte;
  importfilename,
  inffile,
  execaddr,
  loadaddr,
  attr1,
  attributes,
  filetype       : String;
  fields         : array of String;
  chr            : Char;
  F              : TFileStream;
begin
 Result:=False;
 //If there is nothing in the buffer
 if Length(buffer)=0 then
 begin
  //Then load the file, if it exists
  if not FileExists(filename) then exit;
  //Load the file from the host
  try
   F:=TFileStream.Create(filename,fmOpenRead OR fmShareDenyNone);
   F.Position:=0;
   //Set the buffer length to what the host returns
   SetLength(buffer,F.Size);
   //Read the file in
   F.Read(buffer[0],F.Size);
   F.Free;
  except
  end;
 end;
 //First, if there is no selection, make one, or if multiple, select the root
 if(DirList.SelectionCount=0)OR(DirList.SelectionCount>1)then
 begin
  DirList.ClearSelection;
  DirList.Items[0].Selected:=True;
 end;
 //Make sure that there is a destination selected - should be after last command
 if DirList.SelectionCount=1 then
  //And it is a directory
  if TMyTreeNode(DirList.Selected).IsDir then
  begin
   //Find out which side of a DFS disc it is
   if (Image.FormatNumber mod 2=1)
   and(Image.FormatNumber>>4=diAcornDFS)then //Only for DFS double sided
   //As with DFS we can only Add with the root selected, the index will be the side
    side:=DirList.Selected.Index
   else
   //Not double sided or DFS, so assume side 0
    side:=0;
   //Extract the filename
   if filedetails.Filename='' then
    importfilename:=ExtractFileName(filename)
   else
    importfilename:=ExtractFileName(filedetails.Filename);
   //If we still have no filename, make one up
   if importfilename='' then importfilename:='NewFile';
   //Reject any *.inf files
   if LowerCase(RightStr(importfilename,4))<>'.inf' then
   begin
    //Initialise the strings
    execaddr:='00000000';
    loadaddr:='00000000';
    attr1   :='';
    filetype:='';
    //Does the filename contain the filetype?
    if ((Pos(',',importfilename)>0)
    or  (Pos('.',importfilename)>0))
    and((Image.FormatNumber>>4=diAcornADFS)      //ADFS
    or  (Image.FormatNumber>>4=diCommodore))then //Commodore
    begin
     i:=Length(importfilename);
     while (importfilename[i]<>'.')and(importfilename[i]<>',')do dec(i);
     //Get the filetype
     filetype:=LowerCase(Copy(importfilename,i+1));
     //And remove it from the filename
     importfilename:=LeftStr(importfilename,i-1);
     //Decode the filetype - convert it to hex
     for index:=1 to Length(Extensions) do
      if Copy(Extensions[index],4)=LowerCase(filetype) then
       filetype:=LeftStr(Extensions[index],3);
     //ADFS
     if Image.FormatNumber>>4=diAcornADFS then
     begin
      filetype:=IntToHex(StrToIntDef('$'+filetype,0),3);
      if filetype='000' then filetype:='';//None, so reset
     end;
    end;
    //ADFS, DFS & CFS only stuff
    if((Image.FormatNumber>>4=diAcornDFS)
     or(Image.FormatNumber>>4=diAcornADFS)
     or(Image.FormatNumber>>4=diAcornUEF))
    and(filename<>'')then
    begin
     //Is there an inf file?
     if FileExists(filename+'.inf') then
     begin
      inffile:='';
      //Read in the first line
      try
       F:=TFileStream.Create(filename+'.inf',fmOpenRead OR fmShareDenyNone);
       F.Position:=0;
       while (F.Read(chr,1)=1) and (Ord(chr)>31) and (Ord(chr)<127) do
        inffile:=inffile+chr;
       F.Free;
       fields:=BreakDownInf(inffile);
       //Then extract the fields
       if Length(fields)>0 then importfilename:=fields[0];
       if Length(fields)>1 then loadaddr      :=fields[1];
       if length(fields)>2 then execaddr      :=fields[2];
       if Length(fields)>4 then attr1         :=fields[4];
      except
       //Could not load
      end;
     end;
    end;
    //Initialise the TDirArray
    ResetDirEntry(NewFile);
    //Supplied attributes override anything else
    if filedetails.Filename<>'' then importfilename:=filedetails.Filename;
    if filedetails.Attributes<>'' then attr1:=filedetails.Attributes;
    if filedetails.LoadAddr  <>0  then loadaddr:=IntToHex(filedetails.LoadAddr,8);
    if filedetails.ExecAddr  <>0  then execaddr:=IntToHex(filedetails.ExecAddr,8);
    //Decode the attributes
    attributes:=''; //Default
    if attr1='' then
    begin
     if Image.FormatNumber>>4=diAcornADFS then attributes:='WR';//Default for ADFS
     if Image.FormatNumber>>4=diCommodore then attributes:='C' ;//Default for Commodore
    end;
    attributes:=attributes+GetAttributes(attr1,Image.FormatNumber shr 4);
    //Validate the filename (ADFS, DFS & CFS only)
    if(Image.FormatNumber>>4=diAcornDFS)
     or(Image.FormatNumber>>4=diAcornADFS)
     or(Image.FormatNumber>>4=diAcornUEF)then
    begin
     //Remove any extraenous specifiers
     while (importfilename[4]=Image.DirSep) do
      importfilename:=RightStr(importfilename,Length(importfilename)-2);
     //If root, remove the directory specifier
     if (importfilename[2]=Image.DirSep) and (importfilename[1]='$') then
      importfilename:=RightStr(importfilename,Length(importfilename)-2);
     //Convert a Windows filename to a BBC filename
     WinToBBC(importfilename);
     //Check to make sure that a DFS directory hasn't been changed
     if((Image.FormatNumber>>4=diAcornDFS)
      or(Image.FormatNumber>>4=diAcornADFS))
     and(importfilename[2]='/')then
      importfilename[2]:=Image.DirSep;
     //Remove any spaces, unless it is a big directory
     if Image.DirectoryType<>2 then
      importfilename:=ReplaceStr(importfilename,' ','_');
    end;
    //Setup the record
    NewFile.Filename     :=importfilename;
    NewFile.ExecAddr     :=StrToInt('$'+execaddr);
    NewFile.LoadAddr     :=StrToInt('$'+loadaddr);
    NewFile.Side         :=side;
    NewFile.Attributes   :=attributes;
    NewFile.DirRef       :=-1; //Not a directory
    NewFile.ShortFileType:=filetype;
    if Image.FormatNumber>>4=diAcornADFS then //Need the selected directory for ADFS
     if DirList.Selected.Text='$' then NewFile.Parent:='$'
     else
      NewFile.Parent    :=GetImageFilename(TMyTreeNode(DirList.Selected).ParentDir,
                                           DirList.Selected.Index);
    //Set the length - the actual length overrides everything else
    NewFile.Length:=Length(buffer);
    //Write the File
    index:=Image.WriteFile(NewFile,buffer);
    //Function returns pointer to next item (or parent if no children)
    if index>-1 then //File added OK
    begin
     HasChanged:=True;
     AddFileToTree(DirList.Selected,NewFile.Filename,index,False);
     Result:=True;
    end
    else
    begin
     //For some reason the operation failed to write the data
     if index=-1 then ReportError('Could not load "'+NewFile.Filename+'"');
     if index=-2 then ReportError('Could not add "'+NewFile.Filename+'" - image full');
     if index=-3 then ReportError('File "'+NewFile.Filename+'" already exists in image');
     if index=-4 then ReportError('Catalogue full');
     if index=-5 then ReportError('Could not write file "'+NewFile.Filename+'" - error unknown');
     if index=-6 then ReportError('Destination directory does not exist');
     If index=-7 then ReportError('Could not write file "'+NewFile.Filename+'" - map full');
     if index=-8 then ReportError('Could not write file "'+NewFile.Filename+'" - nothing to write');
     if index=-9 then ReportError('Could not extend directory');
    end;
   end;
  end
  else ReportError('"'+DirList.Selected.Text+'" is not a directory')
 else
  if DirList.SelectionCount=0 then
   ReportError('No destination directory selected')
  else
   ReportError('Cannot add to multiple selection');
end;

{------------------------------------------------------------------------------}
//Add a file or directory to the TTreeView, under ParentNode
{------------------------------------------------------------------------------}
function TMainForm.AddFileToTree(ParentNode: TTreeNode;importfilename: String;
                                  index: Integer;dir: Boolean): TTreeNode;
begin
 Result:=nil;
 RemoveTopBit(importfilename);
 //Now add the entry to the Directory List
 if ParentNode.HasChildren then
  //Insert it before the one specified
  if index<ParentNode.Count then
   Result:=DirList.Items.Insert(ParentNode.Items[index],importfilename)
  else //Unless this is the last one
   Result:=DirList.Items.AddChild(ParentNode,importfilename)
 else
  //Is the first child, so just add it
  Result:=DirList.Items.AddChildFirst(ParentNode,importfilename);
 if ParentNode.Parent<>nil then //No parent, so will be the root
  TMyTreeNode(Result).ParentDir:=Image.Disc[TMyTreeNode(ParentNode).ParentDir].
                                     Entries[ParentNode.Index].DirRef
 else
  TMyTreeNode(Result).ParentDir:=ParentNode.Index; //But may not be the only root
 TMyTreeNode(Result).IsDir:=dir;
 DirList.Repaint;
 //And update the free space display
 UpdateImageInfo;
end;

{------------------------------------------------------------------------------}
//About box
{------------------------------------------------------------------------------}
procedure TMainForm.btn_AboutClick(Sender: TObject);
var
 platform: String;
begin
 //Update the Application Title
 AboutForm.lb_Title.Caption:=ApplicationTitle;
 //Determine the current platform (compile time directive)
 platform:='';
 {$IFDEF Darwin}
 platform:=' macOS';            //Apple Mac OS X
 {$ENDIF}
 {$IFDEF Windows}
 platform:=' Windows';          //Microsoft Windows
 {$ENDIF}
 {$IFDEF Linux}
 platform:=' Linux';            //Linux
 {$ENDIF}
 {$IFDEF CPU32}
 platform:=platform+' 32 bit';  //32 bit CPU
 {$ENDIF}
 {$IFDEF CPU64}
 platform:=platform+' 64 bit';  //64 bit CPU
 {$ENDIF}
 {$IFDEF CPUARM}
 platform:=platform+' ARM';     //ARM CPU
 {$ENDIF}
 //Update the Application Version
 AboutForm.lb_Version.Caption:='Version '+ApplicationVersion+platform;
 //Show the Form, as a modal
 AboutForm.ShowModal;
end;

{------------------------------------------------------------------------------}
//User has clicked download file button
{------------------------------------------------------------------------------}
procedure TMainForm.btn_downloadClick(Sender: TObject);
begin
 ExtractFiles(True);
end;

{------------------------------------------------------------------------------}
//Extract files from image
{------------------------------------------------------------------------------}
procedure TMainForm.ExtractFiles(ShowDialogue: Boolean);
var
 Node       : TTreeNode;
 entry,
 dir,s      : Integer;
 saver,
 showsave,
 selectroot : Boolean;
begin
 selectroot:=False;
 showsave:=not ShowDialogue; //Indicates whether the dialogue has been shown
 //if the root is selected, then select everything else
 if DirList.Items[0].Selected then
 begin
  selectroot:=True;
  //Deselect the root
  DirList.ClearSelection;
  //Does it have children?
  if DirList.Items.Item[0].HasChildren then
  begin
   //Go through them, starting at the first child
   Node:=DirList.Items.Item[0].GetFirstChild;
   //Continue until we run out of children
   while Node<>nil do
   begin
    //Select it
    Node.Selected:=True;
    //And get the next one, if there is one
    Node:=Node.GetNextSibling;
   end;
  end;
 end;
 //Only continue if something is selected
 if DirList.SelectionCount>0 then
  for s:=0 to DirList.SelectionCount-1 do
  begin
   //Get one of the selected Nodes
   Node:=DirList.Selections[s];
   //Only act on it if there is one selected
   if Node<>nil then
   begin
    //Get the entry and directory references into 'Image'
    entry:=Node.Index;
    dir:=-1;//This will remain as -1 with the root
    if Node.Parent<>nil then
     dir  :=TMyTreeNode(Node).ParentDir;
    if dir>=0 then //dir = -1 would be the root
    begin
     //Open the Save As dialogue box, but only if the first in the list
     if ShowDialogue then saver:=False;
     if not showsave then
     begin
      //Indicate that we have shown the dialogue
      showsave:=True;
      //Set the default filename
      if selectroot then //Root was selected
      begin
       //Set the default filename as either the disc title or '$'
       if Image.Title='' then
        ExtractDialogue.FileName:=DirList.Items[0].Text
       else
        ExtractDialogue.FileName:=Image.Title;
      end
      else
       ExtractDialogue.FileName:=GetWindowsFilename(dir,entry);
      //Get the result
      saver:=ExtractDialogue.Execute;
      //User clicked on Cancel, so exit
      if not saver then exit;
      if (saver) and (selectroot) then //Root was selected, so create the directory
      begin
       CreateDir(ExtractDialogue.FileName);
       ExtractDialogue.Filename:=ExtractDialogue.Filename+PathDelim+'root';
      end;
     end;
     if s>0 then saver:=True;
     //Download a single file
     if saver then
      //Do not download if the parent is selected, as this will get downloaded anyway
      if not DirList.Selections[s].Parent.Selected then
       DownLoadFile(dir,entry,ExtractFilePath(ExtractDialogue.FileName));
    end;
   end;
  end;
end;

{------------------------------------------------------------------------------}
//Create an Image filename
{------------------------------------------------------------------------------}
function TMainForm.GetImageFilename(dir,entry: Integer): String;
begin
 if Length(Image.Disc[dir].Entries)=0 then
  Result:=Image.Disc[dir].Directory
 else
  Result:=Image.Disc[dir].Entries[entry].Parent+Image.DirSep
         +Image.Disc[dir].Entries[entry].Filename;
end;

{------------------------------------------------------------------------------}
//Create a Windows filename
{------------------------------------------------------------------------------}
function TMainForm.GetWindowsFilename(dir,entry: Integer): String;
var
 extsep: Char;
begin
 extsep:=#0;
 //Get the filename
 Result:=Image.Disc[dir].Entries[entry].Filename;
 //Convert BBC chars to PC
 BBCtoWin(Result);
 //Replace any non-valid characters
 ValidateFilename(Result);
 //Add the filetype to the end, if any (but not directories)
 if (Image.Disc[dir].Entries[entry].ShortFileType<>'')
 and(Image.Disc[dir].Entries[entry].DirRef=-1) then
 begin
  if Image.FormatNumber shr 4<2 then extsep:=','; //DFS and ADFS
  if(Image.FormatNumber shr 4=2)                  //Commodore
  or(Image.FormatNumber shr 4=4)then extsep:='.'; //Amiga
  Result:=Result+extsep+Image.Disc[dir].Entries[entry].ShortFileType;
 end;
end;

{------------------------------------------------------------------------------}
//Download a file
{------------------------------------------------------------------------------}
procedure TMainForm.DownLoadFile(dir,entry: Integer;path: String);
var
 F              : TFileStream;
 buffer         : TDIByteArray;
 imagefilename,
 windowsfilename: String;
begin
 // Ensure path ends in a directory separator
 if path[Length(path)]<>PathDelim then path:=path+PathDelim;
 //Object is a file, so download it
 if Image.Disc[dir].Entries[entry].DirRef=-1 then
 begin
  //Get the full path and filename
  imagefilename  :=GetImageFilename(dir,entry);
  windowsfilename:=GetWindowsFilename(dir,entry);
  if Image.ExtractFile(imagefilename,buffer,entry) then
  begin
   //Save the buffer to the file
   try
    F:=TFileStream.Create(path+windowsfilename,fmCreate OR fmShareDenyNone);
    F.Position:=0;
    F.Write(buffer[0],Length(buffer));
    F.Free;
    CreateINFFile(dir,entry,path);
   except
    //Could not create file
   end;
  end
  //Happens if the file could not be located
  else ReportError('Could not locate file "'+imagefilename+'"');
 end
 else DownLoadDirectory(dir,entry,path+windowsfilename);
end;

{------------------------------------------------------------------------------}
//Create an inf file
{------------------------------------------------------------------------------}
procedure TMainForm.CreateINFFile(dir,entry: Integer; path: String);
var
 F              : TFileStream;
 inffile,
 imagefilename,
 windowsfilename: String;
 attributes,
 hexlen         : Byte;
 t              : Integer;
const
 adfsattr = 'RWELrwel';
begin
 hexlen:=8;
 if Image.FormatNumber>>4=diAcornDFS then hexlen:=6;
 //DFS, ADFS and CFS only
 if(Image.FormatNumber>>4=diAcornDFS)
 or(Image.FormatNumber>>4=diAcornADFS)
 or(Image.FormatNumber>>4=diAcornUEF)then
 begin
  imagefilename:=Image.Disc[dir].Entries[entry].Filename;
  windowsfilename:=GetWindowsFilename(dir,entry);
  //Add the root, if DFS and no directory specifier
  if(Image.FormatNumber>>4=diAcornDFS)and(imagefilename[2]<>'.')then
   imagefilename:=RightStr(Image.Disc[dir].Entries[entry].Parent,1)+'.'+imagefilename;
  //Put quotes round the filename if it contains a space
  if Pos(' ',imagefilename)>0 then imagefilename:='"'+imagefilename+'"';
  //Create the string
  inffile:=PadRight(LeftStr(imagefilename,12),12)+' '
          +IntToHex(Image.Disc[dir].Entries[entry].LoadAddr,hexlen)+' '
          +IntToHex(Image.Disc[dir].Entries[entry].ExecAddr,hexlen)+' '
          +IntToHex(Image.Disc[dir].Entries[entry].Length,hexlen);
  //Create the attributes
  attributes:=$00;
  if(Image.FormatNumber shr 4=0)or(Image.FormatNumber shr 4=5)then //DFS and CFS
   if Image.Disc[dir].Entries[entry].Attributes='L' then attributes:=$08;
  if Image.FormatNumber shr 4=1 then //ADFS
   for t:=0 to 7 do
    if Pos(adfsattr[t+1],Image.Disc[dir].Entries[entry].Attributes)>0 then
     inc(attributes,1 shl t);
  inffile:=inffile+' '+IntToHex(attributes,2)
          +' CRC32='+Image.GetFileCRC(Image.Disc[dir].Entries[entry].Parent+
                                      Image.DirSep+
                                      Image.Disc[dir].Entries[entry].Filename,
                                      entry);
  //Create the inf file
  try
   F:=TFileStream.Create(path+windowsfilename+'.inf',fmCreate OR fmShareDenyNone);
   F.Position:=0;
   F.Write(inffile[1],Length(inffile));
   F.Free;
  except
   //Could not create
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Download an entire directory
{------------------------------------------------------------------------------}
procedure TMainForm.DownLoadDirectory(dir,entry: Integer;path: String);
var
 imagefilename,
 windowsfilename: String;
 ref            : Cardinal;
 c,s            : Integer;
begin
 ref:=0;
 // Ensure path ends in a directory separator
 if path[Length(path)]<>PathDelim then path:=path+PathDelim;
 //Get the full path and filename
 imagefilename:=GetImageFilename(dir,entry);
 //Convert to Windows filename
 windowsfilename:=GetWindowsFilename(dir,entry);
 if Image.FileExists(imagefilename,ref) then
 begin
  //Create the directory
  if not DirectoryExists(path+windowsfilename) then
  begin
   CreateDir(path+windowsfilename);
   CreateINFFile(dir,entry,path);
  end;
  //Navigate into the directory
  s:=Image.Disc[dir].Entries[entry].DirRef;
  //Iterate through the entries
  for c:=0 to Length(Image.Disc[s].Entries)-1 do
   DownLoadFile(s,c,path+windowsfilename);
 end
 //Happens if the file could not be located
 else ReportError('Could not locate directory "'+imagefilename+'"');
end;

{------------------------------------------------------------------------------}
//User has clicked on the button to open a new image
{------------------------------------------------------------------------------}
procedure TMainForm.btn_OpenImageClick(Sender: TObject);
begin
 if QueryUnsaved then
  //Show the open file dialogue box
  if OpenImageFile.Execute then
   //And then open the image
   OpenImage(OpenImageFile.FileName);
end;

{------------------------------------------------------------------------------}
//Open a disc image from file
{------------------------------------------------------------------------------}
procedure TMainForm.OpenImage(filename: String);
begin
 //Show a progress message
 ProgressForm.Show;
 //Process the messages to close the file dialogue box
 Application.ProcessMessages;
 //Close any open hex dump windows
 CloseAllHexDumps;
 Image.ProgressIndicator:=@UpdateProgress;
 //Load the image and create the catalogue
 if Image.LoadFromFile(filename) then
 begin
  HasChanged:=False;
  //Update the display
  ShowNewImage(Image.Filename);
 end
 else
  ReportError('"'+filename
                        +'" has not been recognised as a valid disc image that '
                        +ApplicationTitle+' can open.');
 //Close the progress message
 ProgressForm.Hide;
end;

{------------------------------------------------------------------------------}
//Copy CRC32 to clipboard
{------------------------------------------------------------------------------}
procedure TMainForm.sb_ClipboardClick(Sender: TObject);
begin
 Clipboard.AsText:=lb_CRC32.Caption;
end;

{------------------------------------------------------------------------------}
//Adds a directory to the TreeView - is called recursively to drill down the tree
{------------------------------------------------------------------------------}
procedure TMainForm.AddDirectoryToTree(CurrDir: TTreeNode; dir: Integer; var highdir: Integer);
var
 entry: Integer;
 Node: TTreeNode;
begin
 //Make a note of the dir ref, it is the highest
 if dir>highdir then highdir:=dir;
 //Set the 'IsDir' flag to true, as this is a directory
 TMyTreeNode(CurrDir).IsDir:=True;
 //Iterate though all the entries
 for entry:=0 to Length(Image.Disc[dir].Entries)-1 do
 begin
  //Adding new nodes for each one
  Node:=AddFileToTree(CurrDir,Image.Disc[dir].Entries[entry].Filename,entry,false);
  //If it is, indeed, a direcotry, the dir ref will point to the sub-dir
  if Image.Disc[dir].Entries[entry].DirRef>=0 then
  //and we'll recursively call ourself to add these entries
   AddDirectoryToTree(Node,Image.Disc[dir].Entries[entry].DirRef,highdir);
 end;
end;

{------------------------------------------------------------------------------}
//Reset the display for a new/loaded image
{------------------------------------------------------------------------------}
procedure TMainForm.ShowNewImage(title: String);
var
 //Used as a marker to make sure all directories are displayed.
 //Some double sided discs have each side as separate discs
 highdir    : Integer;
begin
  //Clear all the labels, and enable/disable the buttons
  ResetFileFields;
  //Clear the search fields
  SearchForm.ResetSearchFields;
  //Change the application title (what appears on SHIFT+TAB, etc.)
  Caption:=ApplicationTitle;
  if title<>'' then Caption:=Caption+' - '+ExtractFileName(title);
  //If the format is a recognised one
  if Image.FormatString<>'' then
  begin
   //Populate the tree view
   DirList.Items.Clear;
   //Set the highdir to zero - which will be root to start with
   highdir:=0;
   //Then add the directories, if there is at least one
   if Length(Image.Disc)>0 then
   begin
    //Start by adding the root (could be more than one root, particularly on
    //double sided discs)
    repeat
     //This will initiate the recursion through the directory structure, per side
     AddDirectoryToTree(DirList.Items.Add(nil,Image.Disc[highdir].Directory),highdir,highdir);
     //Finished on this directory structure, so increase the highdir
     inc(highdir);
     //and continue until we have everything on the disc. This will, in effect,
     //add the second root for double sided discs.
    until highdir=Length(Image.Disc);
    //Expand the top level of the tree (but not MMB)
    if Image.FormatNumber>>4<>6 then DirList.TopItem.Expand(False);
    //And the root for the other side of the disc
    if Image.DoubleSided then
    begin
     //First, we need to find it
     repeat
      inc(highdir)
      //If there is one, of course - but it must be a directory
     until (highdir>=DirList.Items.Count) or (TMyTreeNode(DirList.Items[highdir-1]).IsDir);
     if highdir>DirList.Items.Count then
      highdir:=DirList.Items.Count;
     //Found? then expand it
     if TMyTreeNode(DirList.Items[highdir-1]).IsDir then
      DirList.Items[highdir-1].Expand(False);
    end;
   end;
   //Populate the info box
   UpdateImageInfo;
   //Enable the controls
   btn_SaveImage.Enabled :=True;
   menuSaveImage.Enabled :=True;
   btn_SaveAsCSV.Enabled :=True;
   menuSaveAsCSV.Enabled :=True;
   btn_CloseImage.Enabled:=True;
   menuCloseImage.Enabled:=True;
   btn_FileSearch.Enabled:=True;
   menuFileSearch.Enabled:=True;
   if Length(Image.FreeSpaceMap)>0 then
   begin
    btn_ImageDetails.Enabled:=True;
    menuImageDetails.Enabled:=True;
   end;
   if Image.FormatNumber shr 4=1 then
   begin
    btn_FixADFS.Enabled:=True;
    menuFixADFS.Enabled:=True;
   end;
   //Enable the directory view
   DirList.Enabled:=True;
  end;
end;

{------------------------------------------------------------------------------}
//Update the Image information display
{------------------------------------------------------------------------------}
procedure TMainForm.UpdateImageInfo;
var
 i: Integer;
 title: String;
begin
 //Only if there is a valid image
 if Image.FormatNumber<>$FF then
 begin
  //Image Format
  ImageDetails.Panels[1].Text:=Image.FormatString;
  //Disc name
  title:=Image.Title;
  RemoveTopBit(title);//Ensure top bit not set
  ImageDetails.Panels[2].Text:=title;
  //Disc size
  ImageDetails.Panels[3].Text:=ConvertToKMG(Image.DiscSize)
                           +' ('+IntToStrComma(Image.DiscSize)+' Bytes)';
  //Free space
  ImageDetails.Panels[4].Text:=ConvertToKMG(Image.FreeSpace)
                           +' ('+IntToStrComma(Image.FreeSpace)+' Bytes)';
  //Double sided or not
  if Image.DoubleSided then
   ImageDetails.Panels[5].Text:='Double Sided'
  else
   ImageDetails.Panels[5].Text:='Single Sided';
  //Map type (ADFS/AmigaDOS only)
  ImageDetails.Panels[6].Text:=Image.MapTypeString;
  //Directory type (ADFS/AmigaDOS only)
  ImageDetails.Panels[7].Text:=Image.DirectoryTypeString;
 end
 else //Reset all to blank
  for i:=1 to 7 do ImageDetails.Panels[i].Text:='';
 //Update the status bar
 ImageDetails.Repaint;
end;

{------------------------------------------------------------------------------}
//Re-arrange, and show/hide, the various elements in the File Details panel
{------------------------------------------------------------------------------}
procedure TMainForm.ArrangeFileDetails;
var
 cbpos: Integer;
procedure ArrangeComponent(c,p: TControl;l: TLabel);
begin
 c.Visible:=l.Caption<>'';
 if c.Visible then inc(cbpos);
 c.Height :=l.Top+l.Height;
 c.Top    :=p.Top+p.Height;
end;
begin
 cbpos:=0;
 //Show or hide the headers
 ArrangeComponent(FilenamePanel ,FileDetailsLabel,lb_FileName); //Filename
 ArrangeComponent(FileTypePanel ,FilenamePanel   ,lb_FileType); //Filetype
 img_FileType.Top      :=lb_FileType.Top+lb_FileType.Height;
 img_FileType.Width    :=DirList.ImagesWidth*2;
 img_FileType.Height   :=DirList.ImagesWidth*2;
 img_FileType.Left     :=(FileTypePanel.Width-img_FileType.Width)div 2;
 FileTypePanel.Height  :=img_FileType.Top+img_FileType.Height;  //Filetype graphic
 ArrangeComponent(ParentPanel   ,FileTypePanel   ,lb_Parent);   //Parent
 ArrangeComponent(ExecAddrPanel ,ParentPanel     ,lb_execaddr); //Exec Address
 ArrangeComponent(LoadAddrPanel ,ExecAddrPanel   ,lb_loadaddr); //Load Address
 ArrangeComponent(LengthPanel   ,LoadAddrPanel   ,lb_length);   //Length
 ArrangeComponent(TimestampPanel,LengthPanel     ,lb_timestamp);//Timestamp
 ArrangeComponent(DirTitlePanel ,TimestampPanel  ,lb_title);    //Dir Title
 ed_title.Top          :=lb_title.Top;
 ArrangeComponent(LocationPanel ,DirTitlePanel   ,lb_location); //Location
 ArrangeComponent(CRC32Panel    ,LocationPanel   ,lb_CRC32);    //CRC32
 sb_Clipboard.Top      :=lb_CRC32.Top+lb_CRC32.Height;
 sb_Clipboard.Width    :=DirList.ImagesWidth+5;
 sb_Clipboard.Height   :=DirList.ImagesWidth+5;
 sb_Clipboard.ImageWidth:=DirList.ImagesWidth+5;
 sb_Clipboard.Left     :=(CRC32Panel.Width-sb_Clipboard.Width)div 2;
 CRC32Panel.Height     :=sb_Clipboard.Top+sb_Clipboard.Height;  //Clipboard button
 //Make the appropriate panel visible
 //DFS and UEF
 if(Image.FormatNumber>>4=diAcornDFS)
 or(Image.FormatNumber>>4=diAcornUEF)then
 begin
  //Make it visible
  if cbpos>0 then DFSAttrPanel.Visible:=True;
  //Position it below the CRC32 section
  DFSAttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
  //Position the tick box inside
  DFSAttributeLabel.Top:=0;
  DFSAttributeLabel.Left:=(DFSAttrPanel.Width-DFSAttributeLabel.Width)div 2;
  cb_DFS_l.Top:=DFSAttributeLabel.Top+DFSAttributeLabel.Height;
  cb_DFS_l.Left:=(DFSAttrPanel.Width-cb_DFS_l.Width)div 2;
  //And change the panel height to accomodate
  DFSAttrPanel.Height:=cb_DFS_l.Top+cb_DFS_l.Height;
 end;
  //ADFS
  if Image.FormatNumber>>4=diAcornADFS then
  begin
   //Make it visible
   if cbpos>0 then ADFSAttrPanel.Visible:=True;
   //Position it below the CRC32 section
   ADFSAttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
   //Position the ticks box inside - Owner Access
   OAAttributeLabel.Top:=0;
   OAAttributeLabel.Left:=(ADFSAttrPanel.Width-OAAttributeLabel.Width)div 2;
   cb_ADFS_ownw.Top:=OAAttributeLabel.Top+OAAttributeLabel.Height;
   cb_ADFS_ownr.Top:=OAAttributeLabel.Top+OAAttributeLabel.Height;
   cb_ADFS_ownl.Top:=OAAttributeLabel.Top+OAAttributeLabel.Height;
   cb_ADFS_owne.Top:=OAAttributeLabel.Top+OAAttributeLabel.Height;
   cbpos:=ADFSAttrPanel.Width div 4; //Equally space them
   cb_ADFS_ownw.Left:=cbpos*0;
   cb_ADFS_ownr.Left:=cbpos*1;
   cb_ADFS_ownl.Left:=cbpos*2;
   cb_ADFS_owne.Left:=cbpos*3;
   //Position the ticks box inside - Public Access
   PubAttributeLabel.Top:=cb_ADFS_ownw.Top+cb_ADFS_ownw.Height;
   PubAttributeLabel.Left:=(ADFSAttrPanel.Width-PubAttributeLabel.Width)div 2;
   cb_ADFS_pubw.Top:=PubAttributeLabel.Top+PubAttributeLabel.Height;
   cb_ADFS_pubr.Top:=PubAttributeLabel.Top+PubAttributeLabel.Height;
   cb_ADFS_pube.Top:=PubAttributeLabel.Top+PubAttributeLabel.Height;
   cb_ADFS_pubp.Top:=PubAttributeLabel.Top+PubAttributeLabel.Height;
   cb_ADFS_pubw.Left:=cbpos*0;
   cb_ADFS_pubr.Left:=cbpos*1;
   cb_ADFS_pube.Left:=cbpos*2;
   cb_ADFS_pubp.Left:=cbpos*3;
   //And change the panel height to accomodate
   ADFSAttrPanel.Height:=cb_ADFS_pubw.Top+cb_ADFS_pubw.Height;
   //Show/hide those not applicable for new/old directories
   cb_ADFS_owne.Visible:=Image.FormatNumber mod $10<3;
   cb_ADFS_pube.Visible:=Image.FormatNumber mod $10<3;
   cb_ADFS_pubp.Visible:=Image.FormatNumber mod $10<3;
  end;
  //Commodore 64
  if Image.FormatNumber>>4=diCommodore then
  begin
   //Make it visible
   if cbpos>0 then C64AttrPanel.Visible:=True;
   //Position it below the CRC32 section
   C64AttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
   //Position the ticks box inside
   C64AttributeLabel.Top:=0;
   C64AttributeLabel.Left:=(C64AttrPanel.Width-C64AttributeLabel.Width)div 2;
   cb_DFS_l.Top:=C64AttributeLabel.Top+C64AttributeLabel.Height;
   cb_C64_c.Top:=0;
   cb_C64_l.Top:=0;
   //Equally space the boxes
   cbpos:=C64AttrPanel.Width-cbpos div 2;
   cb_C64_c.Left:=cbpos*0;
   cb_C64_l.Left:=cbpos*1;
   //And change the panel height to accomodate
   C64AttrPanel.Height:=cb_C64_l.Top+cb_C64_l.Height;
  end;
end;

{------------------------------------------------------------------------------}
//This is called when the selection changes on the TreeView
{------------------------------------------------------------------------------}
procedure TMainForm.DirListChange(Sender: TObject; Node: TTreeNode);
var
 entry,
 dir,
 ft       : Integer;
 filename,
 filetype,
 location,
 title,
 temp     : String;
 multiple : Char;
 R        : TRect;
begin
 filetype:='';
 //Reset the fields to blank
 ResetFileFields;
 //More than one?
 multiple:=' ';
 if DirList.SelectionCount>1 then
  multiple:='s';
 //Change the menu names - we'll change these to 'Directory', if needed, later
 ExtractFile1.Caption   :='&Extract File'+multiple+'...';
 menuExtractFile.Caption:='&Extract File'+multiple+'...';
 btn_download.Hint      :='Extract File'+multiple;
 RenameFile1.Caption    :='&Rename File'+multiple;
 menuRenameFile.Caption :='&Rename File'+multiple;
 btn_Rename.Hint        :='Rename File'+multiple;
 DeleteFile1.Caption    :='&Delete File'+multiple;
 menuDeleteFile.Caption :='&Delete File'+multiple;
 btn_Delete.Hint        :='Delete File'+multiple;
 //Enable the buttons, if there is a selection, otherwise disable
 btn_download.Enabled   :=DirList.SelectionCount>0;
 ExtractFile1.Enabled   :=DirList.SelectionCount>0;
 menuExtractFile.Enabled:=DirList.SelectionCount>0;
 DeleteFile1.Enabled    :=DirList.SelectionCount>0;
 btn_Delete.Enabled     :=DirList.SelectionCount>0;
 menuDeleteFile.Enabled :=DirList.SelectionCount>0;
 HexDump1.Enabled       :=DirList.SelectionCount=1;
 menuHexDump.Enabled    :=DirList.SelectionCount=1;
 btn_HexDump.Enabled    :=DirList.SelectionCount=1;
 //Disable the Add Files and Rename menu
 AddFile1.Enabled        :=False;
 btn_AddFiles.Enabled    :=False;
 menuAddFile.Enabled     :=False;
 RenameFile1.Enabled     :=False;
 btn_Rename.Enabled      :=False;
 menuRenameFile.Enabled  :=False;
 NewDirectory1.Enabled   :=False;
 btn_NewDirectory.Enabled:=False;
 menuNewDir.Enabled      :=False;
 //If only a single item selected
 if (Node<>nil) and (DirList.SelectionCount=1) then
 begin
  //Update the image
  DirListGetImageIndex(Sender, Node);
  //Enable the Add Files and Rename menu
  AddFile1.Enabled      :=True;
  btn_AddFiles.Enabled  :=True;
  menuAddFile.Enabled   :=True;
  RenameFile1.Enabled   :=True;
  btn_Rename.Enabled    :=True;
  menuRenameFile.Enabled:=True;
  if(Image.FormatNumber shr 4=1)OR(Image.FormatNumber shr 4=4)then //ADFS and Amiga
  begin
   NewDirectory1.Enabled   :=True;
   btn_NewDirectory.Enabled:=True;
   menuNewDir.Enabled      :=True;
  end;
  //Get the entry and dir references
  entry:=Node.Index;
  dir:=-1;
  //Clear the filename variable
  filename:='';
  //If the node does not have a parent, then the dir ref is the one contained
  //in the extra info. Otherwise is -1
  if Node.Parent<>nil then
   dir  :=TMyTreeNode(Node).ParentDir;
  //Then, get the filename and filetype of the file...not root directory
  if dir>=0 then
  begin
   filename:=Image.Disc[dir].Entries[entry].Filename;
   //Attributes
   DoNotUpdate   :=True; //Make sure the event doesn't fire
   //DFS and UEF
   if(Image.FormatNumber>>4=diAcornDFS)
   or(Image.FormatNumber>>4=diAcornUEF)then
    //Tick/untick it
    cb_DFS_l.Checked:=Pos('L',Image.Disc[dir].Entries[entry].Attributes)>0;
   //ADFS
   if Image.FormatNumber>>4=diAcornADFS then
   begin
    //Tick/untick them
    cb_ADFS_ownw.Checked:=Pos('W',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_ADFS_ownr.Checked:=Pos('R',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_ADFS_ownl.Checked:=Pos('L',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_ADFS_owne.Checked:=Pos('E',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_ADFS_pubw.Checked:=Pos('w',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_ADFS_pubr.Checked:=Pos('r',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_ADFS_pube.Checked:=Pos('e',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_ADFS_pubp.Checked:=Pos('P',Image.Disc[dir].Entries[entry].Attributes)>0;
   end;
   //Commodore 64
   if Image.FormatNumber>>4=diCommodore then
   begin
    //Tick/untick them
    cb_C64_l.Checked:=Pos('L',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_C64_c.Checked:=Pos('C',Image.Disc[dir].Entries[entry].Attributes)>0;
   end;
   DoNotUpdate   :=False;  //Re-enable the event firing
   //Filetype
   filetype:=Image.Disc[dir].Entries[entry].Filetype;
  end
  else //Disable buttons as we are on the root
  begin
   btn_download.Enabled   :=True; //Except for download, as we can now do this
   ExtractFile1.Enabled   :=True;
   menuExtractFile.Enabled:=True;
   DeleteFile1.Enabled    :=False;
   btn_Delete.Enabled     :=False;
   menuDeleteFile.Enabled :=False;
   RenameFile1.Enabled    :=False;
   btn_Rename.Enabled     :=False;
   menuRenameFile.Enabled :=False;
  end;
  //If it is a directory, however, we need to get the filename from somewhere else
  //and the filetype will be either Directory or Application (RISC OS only)
  if TMyTreeNode(Node).IsDir then
  begin
   //Determine if it is a Directory or an ADFS Application
   if dir>=0 then
   begin
    if filename='' then
     filename:=Image.Disc[dir].Directory;
    //Pick up from the Node if it is an application or not
    if(Node.ImageIndex=appicon)
    or(Node.ImageIndex>=appstart)then
     filetype:='Application'
    else
     filetype:='Directory';
    //Change the menu text
    ExtractFile1.Caption   :='&Extract '+filetype+'...';
    menuExtractFile.Caption:='&Extract '+filetype+'...';
    btn_download.Hint      :='Extract '+filetype;
    RenameFile1.Caption    :='&Rename '+filetype;
    menuRenameFile.Caption :='&Rename '+filetype;
    btn_Rename.Hint        :='Rename '+filetype;
    DeleteFile1.Caption    :='&Delete '+filetype;
    menuDeleteFile.Caption :='&Delete '+filetype;
    btn_Delete.Hint        :='Delete '+filetype;
    //Report if directory is broken and include the error code
    if (Image.Disc[dir].Entries[entry].DirRef>=0)
    and(Image.Disc[dir].Entries[entry].DirRef<Length(Image.Disc))then
    begin
     if Image.Disc[Image.Disc[dir].Entries[entry].DirRef].Broken then
      filetype:=filetype+' (BROKEN - 0x'
               +IntToHex(
                  Image.Disc[Image.Disc[dir].Entries[entry].DirRef].ErrorCode
                  ,2)+')';
     //Title of the subdirectory
     title:=Image.Disc[Image.Disc[dir].Entries[entry].DirRef].Title;
     RemoveTopBit(title);
     lb_title.Caption:=title;
     ed_title.Enabled:=True; //Can be edited
    end;
   end
   else
   begin //Root directory
    filename:=Image.Disc[entry].Directory;
    filetype:='Root Directory';
    title:=Image.Disc[entry].Title;
    RemoveTopBit(title);
    lb_title.Caption:=title; //Title
    ed_title.Enabled:=True; //Can be edited
    //Report if directory is broken and include the error code
    if Image.Disc[entry].Broken then
     filetype:=filetype+' (BROKEN - 0x'
                       +IntToHex(Image.Disc[0].ErrorCode,2)+')';
   end;
  end
  else //Can only add files to a directory
  begin
   AddFile1.Enabled        :=False;
   btn_AddFiles.Enabled    :=False;
   menuAddFile.Enabled     :=False;
   NewDirectory1.Enabled   :=False;
   btn_NewDirectory.Enabled:=False;
   menuNewDir.Enabled      :=False;
  end;
{  //Testing - can be safetly removed
  if TMyTreeNode(Node).ParentDir<>-1 then
  filename:=filename
  +' dir:'+IntToStr(dir)+' entry:'+IntToStr(entry)
  +' ParentDir:'+IntToStr(TMyTreeNode(Node).ParentDir);}
  //Filename
  RemoveTopBit(filename);
  lb_FileName.Caption:=filename;
  //Filetype Image
  ft:=Node.ImageIndex;
  if (ft=directory) or (ft=directory_o) then
  begin
   if Copy(filename,1,1)='!' then //or application
    ft:=appicon
   else
    ft:=directory;
  end;
  //Create a purple background, as a transparent mask
{  img_FileType.Transparent:=True;
  img_FileType.Canvas.Pen.Color:=FileInfoPanel.Color;// $FF00FF;
  img_FileType.Canvas.Brush.Color:=FileInfoPanel.Color;// $FF00FF;
  img_FileType.Canvas.Rectangle(0,0,img_FileType.Width,img_FileType.Height);}
  img_FileType.Canvas.Draw(0,0,TextureTile.Picture.Bitmap);
  //Paint the picture onto it
//  FileImages.GetBitmap(ft,img_FileType.Picture.Bitmap);
  R.Top:=0;
  R.Left:=0;
  R.Width:=img_FileType.Width;
  R.Height:=img_FileType.Height;
  FileImages.StretchDraw(img_FileType.Canvas,ft,R);
  //Filetype text - only show for certain systems
  if(Image.FormatNumber shr 4=1) //ADFS
  or(Image.FormatNumber shr 4=2) //C64
  or(Image.FormatNumber shr 4=4) then //AmigaDOS
   lb_FileType.Caption:=filetype;
  if dir>=0 then
  begin
   //CRC32
   lb_CRC32.Caption:=Image.GetFileCRC(Image.Disc[dir].Entries[entry].Parent+
                                      Image.DirSep+filename,entry);
   //Parent
   temp:=Image.Disc[dir].Entries[entry].Parent;
   RemoveTopBit(temp);
   lb_parent.Caption:=temp;
   //Timestamp - ADFS only
   if(Image.Disc[dir].Entries[entry].TimeStamp>0)
   and(Image.FormatNumber shr 4=1)then
    lb_timestamp.Caption:=FormatDateTime('hh:nn:ss dd mmm yyyy',
                                       Image.Disc[dir].Entries[entry].TimeStamp)
   else
    if Image.Disc[dir].Entries[entry].DirRef=-1 then
    begin
     //Load address
     lb_loadaddr.Caption:='0x'+IntToHex(Image.Disc[dir].Entries[entry].LoadAddr,8);
     //Execution address
     lb_execaddr.Caption:='0x'+IntToHex(Image.Disc[dir].Entries[entry].ExecAddr,8);
    end;
   //Length
   lb_length.Caption:=ConvertToKMG(Image.Disc[dir].Entries[entry].Length)+
                   ' (0x'+IntToHex(Image.Disc[dir].Entries[entry].Length,8)+')';
   //Location of object - varies between formats
   location:='';
   //ADFS Old map - Sector is an offset
   if Image.MapType=$00 then
    location:='Offset: 0x'+IntToHex(Image.Disc[dir].Entries[entry].Sector,8)+' ';
   //ADFS New map - Sector is an indirect address (fragment and sector)
   if Image.MapType=$01 then
    location:='Indirect address: 0x'+IntToHex(Image.Disc[dir].Entries[entry].Sector,8)+' ';
   //Commodore formats - Sector and Track
   if ((Image.FormatNumber>=$20) and (Image.FormatNumber<=$2F)) then
    location:='Track ' +IntToStr(Image.Disc[dir].Entries[entry].Track)+' ';
   //All other formats - Sector
   if (Image.FormatNumber<=$0F)
   or ((Image.FormatNumber>=$20) and (Image.FormatNumber<=$2F))
   or ((Image.FormatNumber>=$40) and (Image.FormatNumber<=$4F)) then
    location:=location+'Sector '+IntToStr(Image.Disc[dir].Entries[entry].Sector)+' ';
   //DFS - indicates which side also
   if Image.FormatNumber<=$0F then
    location:=location+'Side '  +IntToStr(Image.Disc[dir].Entries[entry].Side);
   //CFS - indicates offset to starting block
   if Image.FormatNumber shr 4=$5 then
    location:='Starting Block 0x'+IntToHex(Image.Disc[dir].Entries[entry].Sector,8);
   lb_location.Caption:=location;
  end;
  if dir=-1 then
  begin
   //Location of root - varies between formats
   location:='';
   //ADFS Old map - Sector is an offset
   if Image.MapType=$00 then
    location:='Offset: 0x'+IntToHex(Image.RootAddress,8);
   //ADFS New map - Sector is an indirect address (fragment and sector)
   if Image.MapType=$01 then
    location:='Indirect address: 0x'+IntToHex(Image.RootAddress,8);
   lb_location.Caption:=location;
  end;
  ArrangeFileDetails;
 end;
end;

{------------------------------------------------------------------------------}
//Called when the TreeView is updated, and it wants to know which icon to use
{------------------------------------------------------------------------------}
procedure TMainForm.DirListGetImageIndex(Sender: TObject; Node: TTreeNode);
var
 ft,i,dir,entry: Integer;
 filetype      : String;
begin
 //The directory and entry references, as always
 dir  :=TMyTreeNode(Node).ParentDir;
 entry:=Node.Index;
 //Are we ADFS?
 if((Image.FormatNumber>=$10)and(Image.FormatNumber<=$1F))
 and(Length(Image.Disc)>0)then
 begin
  //Default is a file with load and exec address
  ft:=loadexec;
  //If it is not a directory
  if not TMyTreeNode(Node).IsDir then
  begin
   //And has a timestamp
   if Image.Disc[dir].Entries[entry].TimeStamp>0 then
   begin
    //Then we become an unknown filetype, until found that is
    ft:=unknown;
    //Start of search - the filetype constant array
    i:=Low(FileTypes)-1;
    //The filetype of the file
    filetype:=Image.Disc[dir].Entries[entry].ShortFiletype;
    //Just iterate through until we find a match, or get to the end
    repeat
     inc(i)
    until (filetype=FileTypes[i])
      or (i=riscoshigh);//High(FileTypes));
    //Do we have a match? Make a note
    if filetype=FileTypes[i] then ft:=i;
   end;
  end;
 end
 else
  //Is it a Commodore format?
  if Image.FormatNumber>>4=diCommodore then
  begin
   //Default is a PRG file
   ft:=prgfile;
   //Directory has a different icon
   if not TMyTreeNode(Node).IsDir then
   begin
    //Get the appropriate filetype - no array for this as there is only 5 of them
    if Image.Disc[dir].Entries[entry].ShortFileType='SEQ' then ft:=seqfile;
    if Image.Disc[dir].Entries[entry].ShortFileType='USR' then ft:=usrfile;
    if Image.Disc[dir].Entries[entry].ShortFileType='REL' then ft:=relfile;
    if Image.Disc[dir].Entries[entry].ShortFileType='DEL' then ft:=delfile;
    if Image.Disc[dir].Entries[entry].ShortFileType='CBM' then ft:=cbmfile;
   end;
  end
  else
   //If not ADFS or Commodore, use a default icon
   ft:=nonadfs; //(which is actually from ADFS in RISC OS 3.11!!)
 //Diectory icons
 if TMyTreeNode(Node).IsDir then
 begin
  //Different icon if it is expanded, i.e. open
  if Node.Expanded then
   ft:=directory_o
  else
   //to a closed one
   ft:=directory;
  //If RISC OS, and an application
  if Image.FormatNumber>>4=diAcornADFS then //ADFS only
   if(Image.DirectoryType=1)OR(Image.DirectoryType=2)then //New or Big
    if Node.Text[1]='!' then
    begin
     ft:=appicon; //Default icon for application
     //Start of search - the applications constant array
     i:=appstart-1;
     //Just iterate through until we find a match, or get to the end
     repeat
      inc(i)
     until(LowerCase(Node.Text)=Applications[i])
       or (i=High(Applications));
     //Do we have a match? Make a note
     if LowerCase(Node.Text)=Applications[i] then ft:=i;
    end;
  //If MMB
  if Image.FormatNumber>>4=6 then
  begin
   ft:=mmbdisc;
   if Image.Disc[Node.Index].Locked then ft:=mmbdisclock;
   if RightStr(Node.Text,5)='empty' then ft:=mmbdiscempt;
  end;
 end;
 //Tell the system what the ImageList reference is
 Node.ImageIndex:=ft;
 //And ensure it stays selected
 Node.SelectedIndex:=Node.ImageIndex;
end;

{------------------------------------------------------------------------------}
//Initialise Form
{------------------------------------------------------------------------------}
procedure TMainForm.FormShow(Sender: TObject);
var
 i: Integer;
begin
 //Keep the application open
 KeepOpen:=True;
 //Initial width and height of form
 Width:=749;
 Height:=515;
 //Enable or disable buttons
 DisableControls;
 //Reset the file details panel
 ResetFileFields;
 //Clear the search fields
 SearchForm.ResetSearchFields;
 //Clear the status bar
 UpdateImageInfo;
 //Reset the tracking variables
 PathBeforeEdit           :='';
 NameBeforeEdit           :='';
 //Reset the delay timer
 progsleep                :=False;
 //There are some commands
 if ParamCount>0 then
 begin
  //Close the application after parsing
  KeepOpen:=False;
  //Parse each one
  for i:=1 to ParamCount do ParseCommandLine(ParamStr(i));
  //Save and close the application as there are commands
  if HasChanged then
  begin
   Image.SaveToFile(Image.Filename);
   HasChanged:=False;
  end;
  //Close the application, unless otherwise specified
  if not KeepOpen then MainForm.Close;
 end;
end;

{------------------------------------------------------------------------------}
//Disable/Enable all the controls for initialisation
{------------------------------------------------------------------------------}
procedure TMainForm.DisableControls;
begin
 //Enable or disable buttons
 btn_NewImage.Enabled     :=True;
 btn_OpenImage.Enabled    :=True;
 btn_SaveImage.Enabled    :=False;
 btn_CloseImage.Enabled   :=False;
 btn_ImageDetails.Enabled :=False;
 btn_About.Enabled        :=True;
 btn_download.Enabled     :=False;
 btn_Delete.Enabled       :=False;
 btn_Rename.Enabled       :=False;
 btn_AddFiles.Enabled     :=False;
 btn_NewDirectory.Enabled :=False;
 btn_SaveAsCSV.Enabled    :=False;
 btn_FixADFS.Enabled      :=False;
 btn_FileSearch.Enabled   :=False;
 //Pop up Menu items
 ExtractFile1.Enabled     :=False;
 RenameFile1.Enabled      :=False;
 DeleteFile1.Enabled      :=False;
 AddFile1.Enabled         :=False;
 NewDirectory1.Enabled    :=False;
 //Main Menu items
 menuNewImage.Enabled     :=True;
 menuOpenImage.Enabled    :=True;
 menuSaveImage.Enabled    :=False;
 menuSaveAsCSV.Enabled    :=False;
 menuCloseImage.Enabled   :=False;
 menuImageDetails.Enabled :=False;
 menuExtractFile.Enabled  :=False;
 menuRenameFile.Enabled   :=False;
 menuDeleteFile.Enabled   :=False;
 menuAddFile.Enabled      :=False;
 menuNewDir.Enabled       :=False;
 menuAbout.Enabled        :=True;
 menuFixADFS.Enabled      :=False;
 menuFileSearch.Enabled   :=False;
 //Close the search window
 SearchForm.Close;
 //Disable the directory view
 DirList.Enabled          :=False;
 //Reset the changed variable
 HasChanged               :=False;
end;

{------------------------------------------------------------------------------}
//Parse any command line options
{------------------------------------------------------------------------------}
procedure TMainForm.ParseCommandLine(cmd: String);
var
 index,
 side       : Integer;
 r          : Boolean;
 option,
 param,
 param2     : String;
 Dir        : TSearchRec;
 Files      : TSearchResults;
 F          : TFileStream;
 fields     : TStringArray;
 filedetails: TDirEntry;
const DiscFormats = //Accepted format strings
 'DFSS    DFSS40  DFSD    DFSD40  WDFSS   WDFSS40 WDFSD   WDFSD40 ADFSS   ADFSM   '+
 'ADFSL   ADFSD   ADFSE   ADFSE+  ADFSF   ADFSF+  C1541   C1571   C1581   AMIGADD '+
 'AMIGAHD CFS     ';
 const DiscNumber : array[1..22] of Integer = //Accepted format numbers
 ($001   ,$000   ,$011   ,$010   ,$021   ,$020   ,$031   ,$030   ,$100   ,$110,
  $120   ,$130   ,$140   ,$150   ,$160   ,$170   ,$200   ,$210   ,$220   ,$400,
  $410   ,$500);
begin
 SetLength(fields,0);
 //Collate the parameters
 option:=cmd;
 param :='';
 param2:='';
 //Split the parameter into command and attribute
 if Pos(':',option)>1 then
 begin
  param :=RightStr(option,Length(option)-Pos(':',option));
  option:=LowerCase(LeftStr (option,Pos(':',option)-1));
 end;
 if param<>'' then
 begin
  //Remove any quotes from the second parameter
  if (param[1]='"')and(param[Length(param)]='"') then
   param:=Copy(param,2,Length(param)-2);
  //Is there a second paramter?
  if(Pos('"|"',param)>1)and(Pos('"|"',param)<Length(param)-3)then
  begin
   param2:=Copy(param,Pos('"|"',param)+3);   //Get the right most part
   param :=LeftStr(param,Pos('"|"',param)-1);//Get the left most part
  end else
  if(Pos('|',param)>1)and(Pos('|',param)<Length(param)-1)then
  begin
   param2:=Copy(param,Pos('|',param)+1);   //Get the right most part
   param :=LeftStr(param,Pos('|',param)-1);//Get the left most part
  end;
  //Commands that require at least one parameter -------------------------------
  //Run commands from file command
  if (option='--cmdfile') or (option='-f') then
  begin
   try
    F:=TFileStream.Create(param,fmOpenRead OR fmShareDenyNone);
    F.Position:=0;
    while ReadLine(F,option) do ParseCommandLine(option);
    F.Free;
   finally
    // Runs after the above, even if an error occurs
   end;
  end;
  //Open command +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (option='--insert') or (option='-i') then
   OpenImage(param);
  //New Image command ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (option='--new') or (option='-n') then
  begin
   index:=(Pos(UpperCase(param),DiscFormats) DIV 8)+1;
   //Create new image
   if Image.Format(DiscNumber[index] DIV $100,
                  (DiscNumber[index] DIV $10)MOD $10,
                   DiscNumber[index] MOD $10) then
   begin
    HasChanged:=True;
    ShowNewImage(Image.Filename);
   end;
  end;
  //Commands that require at least one parameter and an image ------------------
  if Image.FormatNumber<>$FF then
  begin
   //Add new file command +++++++++++++++++++++++++++++++++++++++++++++++++++++++
   if (option='--add') or (option='-a') then
   begin
    //Optional second and subsequent parameters
    if param2<>'' then
    begin
     //Are there more options in the second parameter?
     if Pos('|',param2)>0 then fields:=param2.Split('|');
     if Length(fields)>0 then param2:=fields[0];
     //Select destination directory
     SelectNode(param2);
    end;
    //param can contain a wild card
    FindFirst(param,faDirectory,Dir);
    repeat
     if (Dir.Name<>'.') and (Dir.Name<>'..') then //These are previous and top directories
     begin //Add a directory and the contents
      if (Dir.Attr AND faDirectory)=faDirectory then
       AddDirectoryToImage(ExtractFilePath(param)+Dir.Name)
      else //Add a single file
      begin
       //No extra details have been supplied
       if Length(fields)=0 then AddFileToImage(ExtractFilePath(param)+Dir.Name);
       //Extra details supplied
       if Length(fields)>1 then //Change so that first field is filename, then load, exec, attributes
       begin                    //Will need to change the procedure definition to pass two parameters - filename and filedetails
        filedetails.Filename:=fields[1];
        //Load address
        if Length(fields)>2 then
         filedetails.LoadAddr:=StrToInt('$'+fields[2]);
        //Execution address
        if Length(fields)>3 then
         filedetails.ExecAddr:=StrToInt('$'+fields[3])
        else
         filedetails.ExecAddr:=0;
        //Attributes
        if Length(fields)>4 then
         filedetails.Attributes:=fields[4]
        else
         filedetails.Attributes:='';
        //All these fields will be decoded in this procedure
        AddFileToImage(ExtractFilePath(param)+Dir.Name,filedetails);
       end;
      end;
     end;
    until FindNext(Dir)<>0;
    FindClose(Dir);
   end;
   //Update title +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   if (option='--title') or (option='-t')
   or (option='--title1') or (option='-t1') then
   begin
    side:=0;
    if (option='--title1') or (option='-t1') then side:=1;
    r:=Image.UpdateDiscTitle(param,side);
    HasChanged:=HasChanged OR r;
   end;
   //Update boot option +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   if (option='--opt') or (option='-o')
   or (option='--opt1') or (option='-o1') then
   begin
    side:=0;
    if (option='--opt1') or (option='-o1') then side:=1;
    if LowerCase(param)='none' then param:='0';
    if LowerCase(param)='load' then param:='1';
    if LowerCase(param)='run'  then param:='2';
    if LowerCase(param)='exec' then param:='3';
    index:=StrToIntDef(param,0)mod$10;
    if index<4 then
    begin
     r:=Image.UpdateBootOption(index,side);
     HasChanged:=HasChanged OR r;
    end;
   end;
   //Delete selected file +++++++++++++++++++++++++++++++++++++++++++++++++++++++
   if (option='--delete') or (option='-d') then
   begin
    //Select the file
    SelectNode(param);
    //Now delete it - no confirmations
    DeleteFile(false);
   end;
   //Extract files ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   if (option='--extract') or (option='-e') then
   begin
    //Select the destination OS folder
    if param2='' then ExtractDialogue.FileName:=PathDelim
    else ExtractDialogue.FileName:=param2+PathDelim;
    ResetDirEntry(filedetails);
    //Select the file
    filedetails.Filename:=param;
    //First we look for the files - this will allow wildcarding
    Files:=Image.FileSearch(filedetails);
    //Now go through all the results, if any, and extract each of them
    if Length(Files)>0 then //If there are any, of course
     for index:=0 to Length(Files)-1 do
     begin
      param:='';
      //Build the filename
      if Files[index].Parent<>'' then
       param:=Files[index].Parent+Image.DirSep;
      param:=param+Files[Index].Filename;
      //Then find and select the node
      SelectNode(param);
      //And extract it
      ExtractFiles(False);
     end
    else
    begin //If there were no results, just do it the old fashioned way
     SelectNode(param);
     ExtractFiles(False);
    end;
   end;
   //Commands that require a second parameter -----------------------------------
   if param2<>'' then
   begin
    //Rename selected file ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (option='--rename') or (option='-r') then
    begin
     //Rename the file. If it fails, it will return false.
     r:=Image.RenameFile(param,param2)>=0;
     HasChanged:=HasChanged OR r;
    end;
    //Set attributes for selected file +++++++++++++++++++++++++++++++++++++++++
    if (option='--access') or (option='-ac') then
    begin
     //Set access rights on the specified file
     r:=Image.UpdateAttributes(param,param2);
     HasChanged:=HasChanged OR r;
    end;
    //Change selected directory title ++++++++++++++++++++++++++++++++++++++++++
    if (option='--dirtitle') or (option='-dt') then
    begin
     //Change the selected directory's title
     r:=Image.RetitleDirectory(param,param2);
     HasChanged:=HasChanged OR r;
    end;
   end;
  end;
 end;
 //Commands that do not require any parameters ---------------------------------
 //Create a new directory under selected directory (ADFS/Amiga) ++++++++++++++++
 if (option='--create') or (option='-c') then
 begin
  //Select the parent, if one is specified
  if param2<>'' then SelectNode(param2);
  //Then create the directory
  if param='' then param:='NewDir'; //Default name, if none supplied
  if CreateDirectory(param,'DLR')<>nil then HasChanged:=True;
 end;
 //Save image ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 if (option='--save') or (option='-s') then
  if Image.FormatNumber<>$FF then
  begin
   if param='' then param:=Image.Filename;
   Image.SaveToFile(param,UpperCase(param2)='TRUE');
   Caption:=ApplicationTitle+' - '+ExtractFileName(Image.Filename);
   HasChanged:=False;
   //Update the status bar
   UpdateImageInfo
  end;
 //Keep Application Open +++++++++++++++++++++++++++++++++++++++++++++++++++++++
 if (option='--keepopen') or (option='-k') then
  KeepOpen:=True;
end;

{------------------------------------------------------------------------------}
//This is called when the form is created - i.e. when the application is created
{------------------------------------------------------------------------------}
procedure TMainForm.FormCreate(Sender: TObject);
begin
 //Just updates the title bar
 Caption:=ApplicationTitle;
 //Create the image instance
 Image:=TDiscImage.Create;
 //Used for dragging and dropping
 imgCopy.Parent:=DirList;
 //Turn error reporting on
 ErrorReporting:=True;
end;

{------------------------------------------------------------------------------}
//Select a node
{------------------------------------------------------------------------------}
procedure TMainForm.SelectNode(filename: String);
var
 i,
 found  : Integer;
 dirname:String;
 Node   :TTreeNode;
begin
 //Unselect everything
 DirList.ClearSelection;
 //Marker for our found item
 found:=-1;
 //Go through each one
 i:=0;
 while(i<DirList.Items.Count)AND(found=-1)do
 begin
  //Prepare the full path
  dirname:=DirList.Items[i].Text;
  //We will start at this node and work back to build the path
  Node:=DirList.Items[i];
  while Node.Parent<>nil do
  begin
   Node:=Node.Parent;
   dirname:=Node.Text+Image.DirSep+dirname;
  end;
  //If it matches, then take a note
  if dirname=filename then found:=i;
  inc(i);
 end;
 if found>=0 then //and select it
  DirList.Items[found].Selected:=True;
 //We'll need to give the tree focus, so it shows up
 DirList.SetFocus;
end;

{------------------------------------------------------------------------------}
//This just creates our custom TTreeNode
{------------------------------------------------------------------------------}
procedure TMainForm.DirListCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass:=TMyTreeNode;
end;

{------------------------------------------------------------------------------}
//Save the Image to disc
{------------------------------------------------------------------------------}
procedure TMainForm.btn_SaveImageClick(Sender: TObject);
var
 ext,
 filename: String;
begin
 //Remove the existing part of the original filename
 filename:=ExtractFileName(Image.Filename);
 ext:=ExtractFileExt(filename);
 filename:=LeftStr(filename,Length(filename)-Length(ext));
 //Populate the filename part of the dialogue
 SaveImage.FileName:=filename+'.'+Image.FormatExt;
 SaveImage.DefaultExt:=Image.FormatExt;
 //Show the dialogue
 If SaveImage.Execute then
 begin
  //Save the image
  Image.SaveToFile(SaveImage.FileName);
  Caption:=ApplicationTitle+' - '+ExtractFileName(Image.Filename);
  HasChanged:=False;
  //Update the status bar
  UpdateImageInfo
 end;
end;

{------------------------------------------------------------------------------}
//Query about unsaved changes
{------------------------------------------------------------------------------}
function TMainForm.QueryUnsaved: Boolean;
begin
 Result:=True;
 if HasChanged then
 begin
  Result:=AskConfirm('You have unsaved changes. Do you wish to continue?',
                            'Yes','No','')=mrOK;
 end;
end;

{------------------------------------------------------------------------------}
//Application is asking whether it can close
{------------------------------------------------------------------------------}
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 CanClose:=QueryUnsaved;
end;

{------------------------------------------------------------------------------}
//Open an image, add a new file, or import another image to the disc image
{------------------------------------------------------------------------------}
procedure TMainForm.FormDropFiles(Sender: TObject;
 const FileNames: array of String);
var
 msg,fmt,
 FileName  : String;
 NewImage  : TDiscImage;
 open      : Byte;
 SparkFile : TSpark;
 sparksize : Cardinal;
 isspark   : Boolean;
 confirm   : TModalResult;
{const
 dlg = 'Open, Add, or Import Contents?';}
begin
 //Create a new DiscImage instance
 NewImage:=TDiscImage.Create;
 for FileName in FileNames do
 begin
  //If it is not a directory
  if not DirectoryExists(Filename) then
  begin
   //Open or add ($00=ignore, $01=open, $02=add, $03=ask)
   open:=$00;
   //Load and ID the file
   NewImage.LoadFromFile(FileName,false);
   //Valid image?
   if NewImage.FormatNumber<>$FF then open:=open OR $01; //Is an image
   //Get the detected format string
   fmt:=NewImage.FormatString;
   //Is there something loaded?
   if Image.Filename<>''         then open:=open OR $02; //Something is open
   //Go through the different states
   if open=$03 then //Ask user
   begin
    //Prepare message
    msg:='There is already an image open.'#13#10;
    msg:=msg+'Open this file as a';
    //Is the first letter a vowel?
    if(fmt[1]='A')or(fmt[1]='E')or(fmt[1]='I')or(fmt[1]='O')or(fmt[1]='U')then
     msg:=msg+'n';
    msg:=msg+' '+fmt+' image, add this file to existing image, ';
    msg:=msg+'or import this file''s contents to existing image?';
    //Pose question
    confirm:=AskConfirm(msg,'Open','Add','Import');
    if confirm=mrOK     then open:=$01;
    if confirm=mrCancel then open:=$02;
    if confirm=mrIgnore then open:=$03;
   end;
   if open=$02 then //Add file
   begin
    //Is this 32bit ADFS?
    if(Image.DirectoryType>0)and(Image.FormatNumber shr 4=1)then
    begin
     //See if it is a spark archive
     SparkFile:=TSpark.Create(FileName);
     sparksize:=SparkFile.UncompressedSize;
     isspark:=SparkFile.IsSpark;
     SparkFile.Free;
     //Is it a valid Spark Archive?
     if(isspark)and(sparksize<Image.FreeSpace)then
     begin //Ask user if they would like it uncompressed
      if AskConfirm('This has been detected as a Spark archive. '
                   +'Would you like to uncompress and add the contents?',
                    'Yes',
                    'No - just add',
                    '')=mrOK then
      begin
       open:=$00; //Yes, so reset the open flag so it doesn't add it
       AddSparkToImage(FileName);
      end;
     end;
    end;
    if open=$02 then //No, then just add it
     AddFileToImage(FileName);
   end;
   if open=$01 then //Open image
    if QueryUnsaved then OpenImage(FileName);
   if open=$03 then //Import contents
    ImportFiles(NewImage);
  end
  else //Bulk add from a directory
   AddDirectoryToImage(FileName);
 end;
 //Free up the DiscImage instance
 NewImage.Free;
end;

{------------------------------------------------------------------------------}
//Import an image's contents into the current image
{------------------------------------------------------------------------------}
procedure TMainForm.ImportFiles(NewImage: TDiscImage);
var
 Node      : TTreeNode;
 newentry  : TDirEntry;
 rootname  : String;
 curformat,
 newformat : Byte;
 dir,
 entry     : Cardinal;
 index     : Integer;
 buffer    : TDIByteArray;
begin
 //Show a progress message
 ProgressForm.Show;
 //Process the messages to close the file dialogue box
 Application.ProcessMessages;
 //Import the contents
 NewImage.ReadImage;//First, read in the catalogue
 //Turn error reporting off
 ErrorReporting:=False;
 //Get the name of the directory where this is being imported
 if DirList.SelectionCount>0 then
 begin
  if TMyTreeNode(DirList.Selections[0]).IsDir then
   Node:=DirList.Selections[0]
  else
   Node:=DirList.Selections[0].Parent;
  //We need the complete path, from the root
  rootname:=Node.Text;
  while Node.Parent<>nil do
  begin
   Node:=Node.Parent;
   rootname:=Node.Text+Image.DirSep+rootname;
  end;
 end
 else //Nothing selected, then this is the root
  rootname:=Image.Disc[0].Directory;
 curformat:=Image.FormatNumber shr 4;   //Format of the current open image
 newformat:=NewImage.FormatNumber shr 4;//Format of the importing image
 //Go through each directory
 if Length(NewImage.Disc)>0 then
  for dir:=0 to Length(NewImage.Disc)-1 do
   if Length(NewImage.Disc[dir].Entries)>0 then
    for entry:=0 to Length(NewImage.Disc[dir].Entries)-1 do
    begin
     newentry:=NewImage.Disc[dir].Entries[entry];
     //Validate the filename, as it could be different across file systems
     if newformat<>0 then WinToBBC(newentry.Filename); //Not DFS
     //DFS and C64 don't have directories, so the parent is the selected node
     if(newformat=2)or(newformat=0) then newentry.Parent:=rootname;
     //If going to DFS or C64
     if(curformat=0)or(curformat=2) then
     begin
      //If coming from ADFS or Amiga, and is inside a directory,
      //add the first letter of this.
      if((newformat=1)or(newformat=4))
      and(newentry.Parent<>NewImage.Disc[0].Directory)then
      begin
       index:=Length(newentry.Parent);
       while(newentry.Parent[index]<>NewImage.DirSep)and(index>1)do dec(index);
       if index=Length(newentry.Parent) then index:=0;
       newentry.Filename:=newentry.Parent[index+1]+'.'+newentry.Filename;
      end;
      //Make the root the parent
      newentry.Parent:=rootname;
     end;
     //Coming from DFS and first letter holds the directory?
     //And going to ADFS or Amiga?
     if(newformat=0)and(newentry.Filename[2]='.')and((curformat=1)or(curformat=4)) then
     begin
      //Then create the directory
      SelectNode(rootname);
      //This will fail if already created, but we've suppressed errors
      CreateDirectory(newentry.Filename[1],'DLR');
      newentry.Parent:=rootname+Image.DirSep+newentry.Filename[1];
      newentry.Filename:=Copy(newentry.Filename,3,Length(newentry.Filename));
     end;
     //Going to ADFS, from another system, ensure it has 'WR' attributes
     if(curformat=1)and(curformat<>newformat)then
     begin
      if Pos('W',newentry.Attributes)=0 then
       newentry.Attributes:=newentry.Attributes+'W';
      if Pos('R',newentry.Attributes)=0 then
       newentry.Attributes:=newentry.Attributes+'R';
     end;
     //Select the parent directory
     SelectNode(newentry.Parent);
     //Is it a directory we're adding? ADFS and Amiga only
     if(newentry.DirRef>=0)and((curformat=1)or(curformat=4)) then
     begin
      //Create the directory
      if newentry.Filename<>'$' then
       CreateDirectory(newentry.Filename,'DLR');
     end;
     //Is it a file
     if newentry.DirRef=-1 then
      //Read the file in
      if NewImage.ExtractFile(NewImage.Disc[dir].Entries[entry].Parent
                             +NewImage.DirSep
                             +NewImage.Disc[dir].Entries[entry].Filename,
                              buffer,entry) then
      begin
       //Write it out to the current image
       index:=Image.WriteFile(newentry,buffer);
       //Then add it to the tree, if successful
       if index>=0 then
        AddFileToTree(DirList.Selected,newentry.Filename,index,False);
      end;
    end;
 //Turn error reporting on
 ErrorReporting:=True;
 //Hide the progress message
 ProgressForm.Hide;
end;

{------------------------------------------------------------------------------}
//User has clicked on directory title
{------------------------------------------------------------------------------}
procedure TMainForm.lb_titleClick(Sender: TObject);
begin
 //If there is only one selection
 if DirList.SelectionCount=1 then
  //And it is a directory
  if TMyTreeNode(DirList.Selected).IsDir then
   //And the editor is enabled
   if ed_title.Enabled then
   begin //Then show it, with the title text
    ed_title.Visible  :=True;
    ed_title.Text     :=lb_title.Caption;
    ed_title.SetFocus; //Put the cursor in the control
    lb_title.Visible  :=False;
   end;
end;

{------------------------------------------------------------------------------}
//User has done editing directory title
{------------------------------------------------------------------------------}
procedure TMainForm.ed_titleEditingDone(Sender: TObject);
var
 filename,
 newtitle  : String;
begin
 //Get the filename of the directory being edited
 filename:=lb_Filename.Caption;
 //Add the path, if there is one
 if lb_Parent.Caption<>'' then
  filename:=lb_Parent.Caption+Image.DirSep+filename;
 //Then send to be retitled
 newtitle:=ed_title.Text;
 if newtitle<>lb_title.Caption then //Only if something has changed
 begin
  if Image.RetitleDirectory(filename,newtitle) then
  begin
   lb_title.Caption:=newtitle; //If success, then change the text
   HasChanged:=True;
  end;
 end;
 lb_title.Visible:=True;
 ed_title.Visible:=False;
end;

{------------------------------------------------------------------------------}
//Image Detail Display
{------------------------------------------------------------------------------}
procedure TMainForm.btn_ImageDetailsClick(Sender: TObject);
var
 t,s,side  : Integer;
 col       : TColor;
 FSM       : array of TImage;
 FSMlabel  : array of TLabel;
 titles    : array[0..1] of TEdit;
 boots     : array[0..1] of TComboBox;
 bootlbs   : array[0..1] of TLabel;
 title     : String;
 numsides,
 size,skip : Byte;
begin
 size:=8; //Pixel size
 //Add the editable controls to arrays - makes it easier later on
 titles[0] :=ImageDetailForm.edDiscTitle0;
 titles[1] :=ImageDetailForm.edDiscTitle1;
 boots[0]  :=ImageDetailForm.cbBootOption0;
 boots[1]  :=ImageDetailForm.cbBootOption1;
 bootlbs[0]:=ImageDetailForm.lbBootOption0;
 bootlbs[1]:=ImageDetailForm.lbBootOption1;
 //CRC32
 ImageDetailForm.lbCRC32.Caption:=Image.CRC32;
 //How many sides
 numsides:=Length(Image.FreeSpaceMap);
 //Show the Free Space Map graphic
 if numsides>0 then //Is there a free space map?
 begin
  //Setup the TImage array for all sides
  SetLength(FSM,numsides);
  SetLength(FSMlabel,numsides);
  for side:=0 to numsides-1 do
  begin
   //Create the image and give it a parent
   FSM[side]:=TImage.Create(ImageDetailForm);
   FSM[side].Parent:=ImageDetailForm;
   //FSM[side].Canvas.PixelFormat:=pf4bit;
   //Position it
   FSM[side].Top:=20;
   FSM[side].Left:=4+(204*side);
   //Create the image label and give it a parent
   FSMlabel[side]:=TLabel.Create(ImageDetailForm);
   FSMlabel[side].Parent:=ImageDetailForm;
   //Position it
   FSMlabel[side].Top:=0;
   FSMlabel[side].Left:=4+(204*side);
   //We'll stretch it later
   FSM[side].Stretch:=False;
   //Set the initial size
   while (size>1) AND (Length(Image.FreeSpaceMap[0])*size>100000) do dec(size);
   //Work out what tracks to skip (images over 100000 pixels high will crash)
   skip:=((Length(Image.FreeSpaceMap[0])*size)div 100000)+1;
   //Set the graphic size
   t:=Length(Image.FreeSpaceMap[0])*size;
   s:=Length(Image.FreeSpaceMap[0,0])*size;
   FSM[side].Height:=t div skip;
   FSM[side].Width:=s;
   //Now draw all the sectors in tracks
   for t:=0 to Length(Image.FreeSpaceMap[side])-1 do
    if t mod skip=0 then
    for s:=0 to Length(Image.FreeSpaceMap[side,t])-1 do
    begin
     //Colour for free space
     col:=ImageDetailForm.colFree.Brush.Color;
     //Other colours
     if Image.FreeSpaceMap[side,t,s]=$FF then
      col:=ImageDetailForm.colFile.Brush.Color;      //Unknown/Files
     if Image.FreeSpaceMap[side,t,s]=$FE then
      col:=ImageDetailForm.colSystem.Brush.Color;    //System
     if Image.FreeSpaceMap[side,t,s]=$FD then
      col:=ImageDetailForm.colDir.Brush.Color;       //Directories
     //Change the canvas colour
     FSM[side].Canvas.Pen.Color:=col;
     FSM[side].Canvas.Brush.Color:=col;
     //Now draw a rectangle to represent the sector
     FSM[side].Canvas.Rectangle(s*size,    (t div skip)*size,
                                (s+1)*size,((t div skip)+1)*size);
    end;
   //Stretch the image
   FSM[side].Stretch:=True;
   //And resize to fit the window
   FSM[side].Height:=ImageDetailForm.ClientHeight-24;
   FSM[side].Width:=200;
   //Set the label size
   FSMlabel[side].Height:=20;
   FSMlabel[side].Width:=200;
   FSMlabel[side].Alignment:=taCenter;
   FSMlabel[side].AutoSize:=False;
   //And fill in the details
   FSMlabel[side].Font.Style:=[fsBold];
   FSMlabel[side].Caption:='Free Space Map Side '+IntToStr(side);
   //Disc Title
   if Image.FormatNumber shr 4=0 then title:=Image.Disc[side].Title
   else title:=Image.Title;
   //Remove top bit - this can cause havoc with Mac OS
   RemoveTopBit(title);
   //Set the edit box
   titles[side].Text:=title;
   titles[0].Enabled:=True;
   //Limit the length
   if Image.FormatNumber shr 4=0 then titles[0].MaxLength:=12; //DFS
   if Image.FormatNumber shr 4=1 then titles[0].MaxLength:=10; //ADFS
   //Boot Option
   boots[side].Visible  :=True;
   if Length(Image.BootOpt)>0 then
    boots[side].ItemIndex:=Image.BootOpt[side]
   else
    boots[side].Visible:=False;
   bootlbs[side].Visible:=boots[side].Visible;
   end;
  //Change the dialogue box width
  ImageDetailForm.ClientWidth:=(Length(Image.FreeSpaceMap)*204)
                              +4+ImageDetailForm.Legend.Width;
  //Show/Hide the second detail panel
  ImageDetailForm.pnSide1.Visible:=numsides>1;
  //Show the window, modally
  if ImageDetailForm.ShowModal=mrOK then
  begin
   for side:=0 to numsides-1 do
   begin
    //Update Disc Title
    if Image.UpdateDiscTitle(titles[side].Text,side) then
     HasChanged:=True;
    //Update Boot Option
    if Image.UpdateBootOption(boots[side].ItemIndex,side) then
     HasChanged:=True;
   end;
   UpdateImageInfo;
  end;
  //Clear up and close
  for side:=0 to numsides-1 do
  begin
   FSM[side].Free;
   FSMlabel[side].Free;
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Close currently open image
{------------------------------------------------------------------------------}
procedure TMainForm.btn_CloseImageClick(Sender: TObject);
begin
 if QueryUnsaved then
 begin
  CloseAllHexDumps;
  Image.Close;
  ShowNewImage('');
  UpdateImageInfo;
  DisableControls;
 end;
end;

{------------------------------------------------------------------------------}
//Opens the file search window
{------------------------------------------------------------------------------}
procedure TMainForm.btn_FileSearchClick(Sender: TObject);
begin
 SearchForm.Show;
end;

{------------------------------------------------------------------------------}
//Fix the ADFS Broken Directories
{------------------------------------------------------------------------------}
procedure TMainForm.btn_FixADFSClick(Sender: TObject);
var
 c: Boolean;
begin
 //Show a progress message
 ProgressForm.Show;
 //Process the messages to close the file dialogue box
 Application.ProcessMessages;
 //Run the procedure
 c:=Image.FixDirectories;
 //If it changed, the refresh the display (as some directories might not have
 if c then ShowNewImage(Image.Filename); //got read first time round
 //Change the Has Changed flag
 HasChanged:=HasChanged OR c;
 //Hide the progress message
 ProgressForm.Hide;
end;

{------------------------------------------------------------------------------}
//Create a new directory click
{------------------------------------------------------------------------------}
procedure TMainForm.btn_NewDirectoryClick(Sender: TObject);
var
 dirname,
 parentdir : String;
 x         : Integer;
 ref       : Cardinal;
begin
 ref:=0;
 //Assume the root, for now
 parentdir:='$';
 if DirList.Selected.Text<>'$' then //If not the root, get the parent directory
  parentdir:=GetImageFilename(TMyTreeNode(DirList.Selected).ParentDir,
                                          DirList.Selected.Index);
 //Default name
 dirname:='NewDir';
 //Make sure it doesn't exist
 x:=0;
 while Image.FileExists(parentdir+Image.DirSep+dirname+IntToStr(x),ref) do
  inc(x);
 dirname:=dirname+IntToStr(x);
 //Create the directory
 CreateDirectory(dirname,'DLR');
end;

{------------------------------------------------------------------------------}
//Save all the image's file details to a CSV file
{------------------------------------------------------------------------------}
procedure TMainForm.btn_SaveAsCSVClick(Sender: TObject);
var
 F        : TFileStream;
 dir,
 entry    : Integer;
 filename,
 ext      : String;
 hexlen   : Byte;
begin
 //Remove the existing part of the original filename
 filename:=ExtractFileName(Image.Filename);
 ext:=ExtractFileExt(filename);
 filename:=LeftStr(filename,Length(filename)-Length(ext));
 //Add the csv extension
 SaveCSV.Filename:=filename+'.csv';
 //Show the dialogue box
 if SaveCSV.Execute then
 begin
  hexlen:=8;
  if Image.FormatNumber>>4=diAcornDFS then hexlen:=6;
  //Show a progress message
  ProgressForm.Show;
  //Process the messages to close the file dialogue box
  Application.ProcessMessages;
  //Open a new file
  F:=TFileStream.Create(SaveCSV.FileName,fmCreate OR fmShareDenyNone);
  //Write the image details
  WriteLine(F,'"'+Image.Filename+'","'+Image.CRC32+'"');
  //Write the headers
  WriteLine(F,'"Parent","Filename","Load Address","Execution Address","Length","Attributes","CRC32"');
  //Go through each directory
  for dir:=0 to Length(Image.Disc)-1 do
   //And each entry in that directory
   for entry:=0 to Length(Image.Disc[dir].Entries)-1 do
    //write out each entry
    WriteLine(F,'"'+Image.Disc[dir].Entries[entry].Parent+'","'
                   +Image.Disc[dir].Entries[entry].Filename+'","'
                   +IntToHex(Image.Disc[dir].Entries[entry].LoadAddr,hexlen)+'","'
                   +IntToHex(Image.Disc[dir].Entries[entry].ExecAddr,hexlen)+'","'
                   +IntToHex(Image.Disc[dir].Entries[entry].Length,hexlen)+'","'
                   +Image.Disc[dir].Entries[entry].Attributes+'","'
                   +Image.GetFileCRC(Image.Disc[dir].Entries[entry].Parent
                                    +Image.DirSep
                                    +Image.Disc[dir].Entries[entry].Filename)+'"');
  //Finally free up the file stream
  F.Free;
  //Close the progress window
  ProgressForm.Hide;
 end;
end;

{------------------------------------------------------------------------------}
//Split a DSD or combine two SSDs
{------------------------------------------------------------------------------}
procedure TMainForm.btn_SplitDFSClick(Sender: TObject);
begin
 SplitDFSForm.ShowModal;
 //Report back to the user the result
 if SplitDFSForm.ModalResult=mrOK then
  ShowInfo('Operation was a success');
 if SplitDFSForm.ModalResult=mrAbort then
  ReportError('Operation failed');
 //We'll ignore cancel, as this was a user operation
end;

{------------------------------------------------------------------------------}
//Paint the Directory Tree
{------------------------------------------------------------------------------}
procedure TMainForm.DirListCustomDraw(Sender: TCustomTreeView;
 const ARect: TRect; var DefaultDraw: Boolean);
begin
 //Tile the tree
 TileCanvas(Sender.Canvas,ARect);
 //Set the height of the icons to match the font
 if DirList.Items.Count>0 then
  DirList.ImagesWidth:=DirList.Items[0].Height-3;
 DefaultDraw:=True;
end;

{------------------------------------------------------------------------------}
//Paint the Directory Tree - The state arrow to the left
{------------------------------------------------------------------------------}
procedure TMainForm.DirListCustomDrawArrow(Sender: TCustomTreeView;
 const ARect: TRect; ACollapsed: Boolean);
var
 Index: Integer;
 TV   : TTreeView;
begin
 if Sender is TTreeView then
 begin
  TV:=TTreeView(Sender);
  if ACollapsed then Index:=1 else Index:=0;
  TImageList(TV.StateImages).StretchDraw(TV.Canvas,Index,ARect);
 end;
end;

{------------------------------------------------------------------------------}
//Paint the Directory Tree - ensures the selected item is visible
{------------------------------------------------------------------------------}
procedure TMainForm.DirListCustomDrawItem(Sender: TCustomTreeView;
 Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
 NodeRect : TRect;
 indent,
 arrowin,
 arrowsize: Integer;
 TV       : TTreeView;
begin
 if Sender is TTreeView then
 begin
  TV:=TTreeView(Sender);
  //Only concerned if it is selected
  if cdsSelected in State then
   with TV.Canvas do
   begin
    indent:=(Node.Level*TV.Indent)+TV.Indent+1;
    //We'll 'claim' the draw
    DefaultDraw:=False;
    if(TMyTreeNode(Node).IsDir)and(Node.HasChildren)then
    begin
     NodeRect:=Node.DisplayRect(False);
     //Draw the button
     arrowsize:=DirList.ExpandSignSize;
     {$IFDEF Darwin}
     dec(arrowsize); //For some reason macOS size is 1px smaller
     {$ENDIF}
     //Centralise it
     arrowin:=(NodeRect.Height-arrowsize)div 2;
     //Adjust the lefthand position to accomodate the arrow
     NodeRect.Left:=NodeRect.Left+indent+arrowin-NodeRect.Height-1;
     //And the top
     NodeRect.Top:=NodeRect.Top+arrowin;
     //Set the size
     NodeRect.Width:=arrowsize;
     NodeRect.Height:=arrowsize;
     //And call the OnCustomDrawArrow procedure
     DirListCustomDrawArrow(Sender,NodeRect,not Node.Expanded);
    end;
    //Draw the Image
    NodeRect:=Node.DisplayRect(False);
    NodeRect.Left:=NodeRect.Left+indent;
    NodeRect.Top:=NodeRect.Top+1;
    NodeRect.Width:=TV.ImagesWidth;
    NodeRect.Height:=NodeRect.Width;
    TImageList(TV.Images).StretchDraw(TV.Canvas,Node.ImageIndex,NodeRect);
    //Write out the text
    NodeRect:=Node.DisplayRect(False);
    NodeRect.Left:=NodeRect.Left+indent+TV.ImagesWidth+7;
    NodeRect.Top:=NodeRect.Top+2;
    Brush.Color:=TV.SelectionColor; //Background
    Font.Color:=TV.SelectionFontColor; //Foreground
    TextOut(NodeRect.Left,NodeRect.Top,Node.Text);
   end;
 end;
end;

{------------------------------------------------------------------------------}
//The File Info panel has been resized
{------------------------------------------------------------------------------}
procedure TMainForm.FileInfoPanelResize(Sender: TObject);
begin
 ArrangeFileDetails;
end;

{------------------------------------------------------------------------------}
//Paint the panels
{------------------------------------------------------------------------------}
procedure TMainForm.FileInfoPanelPaint(Sender: TObject);
begin
 if Sender is TPanel then
  TileCanvas(TPanel(Sender).Canvas); //for a TPanel
 if Sender is TToolBar then
  TileCanvas(TToolBar(Sender).Canvas); //for a TToolBar
 if Sender is TForm then
  TileCanvas(TForm(Sender).Canvas); //For a TForm
end;

{------------------------------------------------------------------------------}
//Delay timer
{------------------------------------------------------------------------------}
procedure TMainForm.DelayTimerTimer(Sender: TObject);
begin
 progsleep:=False;
end;

{------------------------------------------------------------------------------}
//Create a new directory
{------------------------------------------------------------------------------}
function TMainForm.CreateDirectory(dirname, attr: String): TTreeNode;
var
 parentdir: String;
 index    : Integer;
 Node     : TTreeNode;
begin
 Result:=nil;
 //Assume the root, for now
 parentdir:='$';
 if DirList.Selected.Text<>'$' then //If not the root, get the parent directory
  parentdir:=GetImageFilename(TMyTreeNode(DirList.Selected).ParentDir,
                                          DirList.Selected.Index);
 //Add it
 index:=Image.CreateDirectory(dirname,parentdir,attr);
 //Function returns pointer to next item (or parent if no children)
 if index>-1 then //Directory added OK
 begin
  HasChanged:=True;
  Node:=AddFileToTree(DirList.Selected,dirname,index,True);
  //Select the new node
  DirList.ClearSelection;
  Node.Selected:=True;
  //Return the newly created directory as a node
  Result:=Node;
 end
  else
  begin//For some reason the operation failed to write the data
   if index=-1 then ReportError('Cannot create a directory on this platform');
   if index=-2 then ReportError('Could not add directory - image full');
   if index=-3 then ReportError('Directory "'+dirname+'" already exists in image');
   if index=-4 then ReportError('Catalogue full');
   if index=-5 then ReportError('Could not write directory - error unknown');
   if index=-6 then ReportError('Destination directory does not exist');
   If index=-7 then ReportError('Could not write directory - map full');
   if index=-8 then ReportError('Could not write directory - nothing to write');
   if index=-9 then ReportError('Could not extend parent directory');
  end;
end;

{------------------------------------------------------------------------------}
//During a drag, check if the user has pressed a key modifier
{------------------------------------------------------------------------------}
procedure TMainForm.DirListKeyDown(Sender: TObject; var Key: Word;
 Shift: TShiftState);
begin
 if(MouseIsDown)and(IsDragging)then
  ImgCopy.Visible:=GetCopyMode(Shift);
end;

{------------------------------------------------------------------------------}
//During a drag, check if the user has released a key modifier
{------------------------------------------------------------------------------}
procedure TMainForm.DirListKeyUp(Sender: TObject; var Key: Word;
 Shift: TShiftState);
begin
 if(MouseIsDown)and(IsDragging)then
  ImgCopy.Visible:=True;
end;

{------------------------------------------------------------------------------}
//User has initiated a drag operation on the directory list
{------------------------------------------------------------------------------}
procedure TMainForm.DirListMouseDown(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
 if ssLeft in Shift then //Only if left button is clicked
  //ADFS, double sided DFS, or AmigaDOS
  if((Image.FormatNumber>>4=diAcornDFS) AND (Image.DoubleSided))
  or (Image.FormatNumber>>4=diAcornADFS)
  or (Image.FormatNumber>>4=diAmiga) then
  begin
   //Remember the node being dragged
   DraggedItem:=DirList.GetNodeAt(X,Y);
   //If it is valid
   if DraggedItem<>nil then
   begin
    //Set the mouse down flag
    MouseIsDown :=True;
    //Remember the mouse position
    CursorPos.X:=X;
    CursorPos.Y:=Y;
   end;
  end;
end;

{------------------------------------------------------------------------------}
//Mouse Move event handler for the 'ghost' image
{------------------------------------------------------------------------------}
procedure TMainForm.DraggedItemMouseMove(Sender: TObject; Shift: TShiftState; X,
 Y: Integer);
begin
 //Adjust the X and Y and pass through to the DirList event handler
 DirListMouseMove(Sender,Shift,X+ObjectDrag.Left,Y+ObjectDrag.Top);
end;

{------------------------------------------------------------------------------}
//Mouse Up event handler for the 'ghost' image
{------------------------------------------------------------------------------}
procedure TMainForm.DraggedItemMouseUp(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
 //Adjust the X and Y and pass through to the DirList event handler
 DirListMouseUp(Sender,Button,Shift,X+ObjectDrag.Left,Y+ObjectDrag.Top);
end;

{------------------------------------------------------------------------------}
//Gets the node located at the same Y co-ordinate as supplied
{------------------------------------------------------------------------------}
function TMainForm.GetNodeAt(Y: Integer): TTreeNode;
var
 xpos: Integer;
begin
 //We'll find the node by the mouse pointer
 xpos:=-10;
 Result:=nil;
 //'GetNodeAt' only returns the node *under* the mouse, so we ignore the 'X'
 while(Result=nil)and(xpos<DirList.Width)do
 begin
  //So we go from left to right until we find a node, or reach the other side
  inc(xpos,10);
  Result:=DirList.GetNodeAt(xpos,Y);
 end;
end;

{------------------------------------------------------------------------------}
//User has dragged an item around the directory list
{------------------------------------------------------------------------------}
procedure TMainForm.DirListMouseMove(Sender: TObject; Shift: TShiftState; X,
 Y: Integer);
var
 Xmovement,
 Ymovement,
 H,
 threshold : Integer;
 R,IR      : TRect;
 B         : TBitmap;
 Dst       : TTreeNode;
 copymode  : Boolean;
 pt        : TPoint;
begin
 //Get the copy mode, depending on key pressed
 copymode:=GetCopyMode(Shift);
 //Is the mouse button down?
 if MouseIsDown then
 begin
  //Work out how far the mouse has moved
  Xmovement:=Abs(X-CursorPos.X);
  Ymovement:=Abs(Y-CursorPos.Y);
  //If it has moved more than 10 pixels then user is dragging
  if(Xmovement>10)or(Ymovement>10)then
  begin
   //We are dragging the control
   IsDragging:=True;
   //Are we over another node?
   Dst:=DirList.GetNodeAt(X,Y);//Node under the cursor
   //Dst:=GetNodeAt(Y);
   if(DraggedItem<>nil)AND(Dst<>nil)AND(DraggedItem<>Dst) then
   begin
    //If it is not a directory, then get the parent
    if not TMyTreeNode(Dst).IsDir then
     Dst:=Dst.Parent;
    //Clear any selections and then highlight the original item
    DirList.ClearSelection;
    DraggedItem.Selected:=True;
    //Only allow copying if it is a different parent and not within itself
    if(DraggedItem<>Dst)AND(DraggedItem.Parent<>Dst)then
    begin
     //Highlight it
     Dst.Selected:=True;
     //Expand the folder tree
     Dst.Expand(False);
    end;
   end;
   //See if we have created an image to move, if not then do so
   if ObjectDrag=nil then
   begin
    //Get the bounding box of the dragged item
    R:=DraggedItem.DisplayRect(False);
    //Now create a bitmap copy of this
    B:=TBitmap.Create;
    B.Width:=R.Width;
    B.Height:=R.Height;
    //We need it transparent
    B.Transparent:=True;
    B.TransparentColor:=clFuchsia; //This is treated a transparent
    B.Canvas.Brush.Color:=clFuchsia;
    B.Canvas.FillRect(Rect(0,0,R.Width,R.Height));
    //Put the file icon in
    IR.Top:=1;
    IR.Left:=0;
    IR.Width:=DirList.ImagesWidth;
    IR.Height:=DirList.ImagesWidth;
    //We need to scale the image, as it could be any size. The size is picked up
    //from the DirList.ImagesWidth above.
    FileImages.StretchDraw(B.Canvas,DraggedItem.ImageIndex,IR);
    //Set up the font
    B.Canvas.Font:=DirList.Font;
    B.Canvas.Font.Quality:=fqNonAntialiased; //Otherwise we get a purple 'shadow'
    B.Canvas.Font.Style:=[fsBold];
    //Centralise the text
    H:=B.Canvas.TextHeight(DraggedItem.Text);
    //Again, we need to consult the DirList.ImagesWidth
    B.Canvas.TextOut(DirList.ImagesWidth+8, //Place it 8px from the graphic
                    (DirList.ImagesWidth-H)div 2, //And central vertically
                     DraggedItem.Text);     //The actual text
    //Now create the new image
    ObjectDrag:=TImage.Create(DirList);
    ObjectDrag.Parent:=DirList;
    //Assign the mouse move event handler
    ObjectDrag.OnMouseMove:=@DraggedItemMouseMove;
    ObjectDrag.OnMouseUp:=@DraggedItemMouseUp;
    //Set the size
    ObjectDrag.Width:=B.Width;
    ObjectDrag.Height:=B.Height;
    //Put our bitmap in
    ObjectDrag.Picture.Assign(B);
    ObjectDrag.Transparent:=True;
    //Make sure we can see it
    ObjectDrag.Visible:=True;
    //And free up the bitmap
    B.Free;
   end;
   //Position our copied control above the mouse
   ObjectDrag.Top:=Y-(ObjectDrag.Height div 2);
   ObjectDrag.Left:=X-(Fileimages.Width+4);
   //Show and position the 'copy' plus icon
   imgCopy.Visible:=copymode;
   imgCopy.Top:=Y+8;
   imgCopy.Left:=X+8;
   //Scroll the control, if near the edge
   threshold:=20; //Distance from the edge to start autoscrolling
   //That is, in the absence of any items on the tree
   if DirList.Items.Count>0 then
    //Get the height of the first item as the threshold
    threshold:=DirList.Items[0].Height;
   //Check where the mouse is over the TTreeView
   while((Y>DirList.Height-threshold)and(Y<DirList.Height))
      or((Y>0)AND(Y<threshold))do
   begin
    //We will need to reset the 'Y' variable, otherwise this will be an endless loop
    pt:=ScreenToClient(Mouse.CursorPos);
    Y:=pt.Y-DirList.Top-ImageContentsPanel.Top; //Take account of the component's location
    //We'll find the node by the mouse pointer
    Dst:=GetNodeAt(Y);
    //If we find a node, and we're not in a delay.
    if(Dst<>nil)and(not progsleep)then
    begin
     //Are we at the top, then get the previous node (if any)
     if Y<threshold then
      Dst:=Dst.GetPrevVisible
     else//Otherwise, get the next (if any)
      Dst:=Dst.GetNextVisible;
     //If we have a node, scroll to make it visible
     if Dst<>nil then Dst.MakeVisible;
     //Set the delay flag
     progsleep:=True;
    end;
    //Make sure all the messages are processed - otherwise it will hang
    Application.ProcessMessages;
   end;
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Determine whether to copy or move
{------------------------------------------------------------------------------}
function TMainForm.GetCopyMode(Shift: TShiftState): Boolean;
begin
 //Default result
 Result:=True;
 //Look at the key modifiers
 {$IFDEF Darwin}   //For the Mac
 if ssMeta in Shift then Result:=False; //Move
 if ssAlt in Shift  then Result:=True;  //Copy
 {$ELSE}           //For Windows or Linux
 if ssShift in Shift then Result:=False; //Move
 if ssCtrl in Shift  then Result:=True;  //Copy
 {$ENDIF}
end;

{------------------------------------------------------------------------------}
//User has dropped an item over the directory list
{------------------------------------------------------------------------------}
procedure TMainForm.DirListMouseUp(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
var
 Xmovement,
 Ymovement,
 index,i,
 dir,
 entry      : Integer;
 ref        : Cardinal;
 Dst,
 NewNode    : TTreeNode;
 copymode   : Boolean;
begin
 copymode:=GetCopyMode(Shift);
 //Remove the copied control
 if ObjectDrag<>nil then ObjectDrag.Free;
 ObjectDrag:=nil;
 //And hide the 'copy' plus icon
 imgCopy.Visible:=False;
 //If we are in dragging mode, and the mouse is down
 if(MouseIsDown)and(IsDragging)then
 begin
  //Work out how far the mouse has moved
  Xmovement:=Abs(X-CursorPos.X);
  Ymovement:=Abs(Y-CursorPos.Y);
  //If it has moved more than 10 pixels then user is dragging
  if(Xmovement>10)or(Ymovement>10)then
  begin
   //Are we over another node?
   Dst:=DirList.GetNodeAt(X,Y);
   //Dst:=GetNodeAt(Y);
   if(DraggedItem<>nil)AND(Dst<>nil)AND(DraggedItem<>Dst) then
   begin
    //If it is not a directory, then get the parent
    if not TMyTreeNode(Dst).IsDir then
     Dst:=Dst.Parent;
    //Only allow copying if it is a different parent and not within itself
    if(DraggedItem<>Dst)AND(DraggedItem.Parent<>Dst)then
    begin
     //Do the copy/move
     if copymode then //copy
      index:=Image.CopyFile(GetFilePath(DraggedItem),GetFilePath(Dst))
     else             //move
     begin
      index:=Image.MoveFile(GetFilePath(DraggedItem),GetFilePath(Dst));
      //Update any open hexdumps
      if index>=0 then
       if Length(HexDump)>0 then
        for i:=0 to Length(HexDump)-1 do
        begin
         //Update the window title
         if HexDump[i].Caption=GetFilePath(DraggedItem) then
          HexDump[i].Caption:=GetFilePath(Dst);
         //Update the menu item
         if HexDumpMenu.Count>i then
          if HexDumpMenu.Items[i].Caption=GetFilePath(DraggedItem) then
           HexDumpMenu.Items[i].Caption:=GetFilePath(Dst);
        end;
     end;
     //Update the display
     if index>=0 then
     begin
      HasChanged:=True;
      NewNode:=AddFileToTree(Dst,DraggedItem.Text,index,
                             TMyTreeNode(DraggedItem).IsDir);
      //Did we just copy a directory?
      if TMyTreeNode(DraggedItem).IsDir then
      begin
       //Add all the items within
       Image.FileExists(GetFilePath(Dst)+Image.DirSep+DraggedItem.Text,ref);
       entry:=ref mod $10000;  //Bottom 16 bits - entry reference
       dir  :=ref div $10000;  //Top 16 bits - directory reference
       //Get the pointer to the parent directory
       dir:=Image.Disc[dir].Entries[entry].DirRef;
       HasChanged:=True;
       //Now recursively add the contents of the newly created directory
       AddDirectoryToTree(NewNode,dir,dir);
      end;
      //If we moved, we'll need to remove the old node too
      if not copymode then
      begin
       DraggedItem.Delete;
       ResetFileFields;
      end;
     end;
     if index<0 then
     begin
      //For some reason the operation failed to write the data these are the error
      //codes returned when writing a file, as it uses the same function)
      if index=-1  then ReportError('Could not load "'+DraggedItem.Text+'"');
      if index=-2  then ReportError('Could not add file - image full');
      if index=-3  then ReportError('File already exists in directory');
      if index=-4  then ReportError('Catalogue full');
      if index=-5  then ReportError('Could not write file - error unknown');
      if index=-6  then ReportError('Destination directory does not exist');
      If index=-7  then ReportError('Could not write file - map full');
      if index=-8  then ReportError('Could not write file - nothing to write');
      if index=-9  then ReportError('Could not extend parent directory');
      if index=-10 then ReportError('Cannot move or copy to the same directory');
      if index=-11 then ReportError('Could not find source file');
      if index=-12 then ReportError('Not possible on this system');
     end;
    end;
   end;
  end;
  //Clear any selections
  if DraggedItem<>nil then
  begin
   DirList.ClearSelection;
   //But make sure ours is still selected
   DraggedItem.Selected:=True;
   //Flush the memory of what is being dragged
   DraggedItem:=nil;
  end;              
 end;
  //Reset the flags
  MouseIsDown:=False;
  IsDragging:=False;
end;

{------------------------------------------------------------------------------}
//Create a new blank image
{------------------------------------------------------------------------------}
procedure TMainForm.btn_NewImageClick(Sender: TObject);
var
 minor,tracks: Byte;
begin
 if QueryUnsaved then
 begin
  //Show the form, modally
  NewImageForm.ShowModal;
  //If Create was clicked, then create a new image
  if NewImageForm.ModalResult=mrOK then
  begin
   //Get the sub-format
   minor:=$F;
   case NewImageForm.MainFormat.ItemIndex of
    0: minor:=NewImageForm.DFS.ItemIndex;
    1: minor:=NewImageForm.ADFS.ItemIndex;
    2: minor:=NewImageForm.C64.ItemIndex;
    3: minor:=NewImageForm.Spectrum.ItemIndex;
    4: minor:=NewImageForm.Amiga.ItemIndex;
    5: minor:=$0;
   end;
   //Number of tracks (DFS only)
   tracks:=0; //Default
   if NewImageForm.MainFormat.ItemIndex=0 then
    tracks:=NewImageForm.DFSTracks.ItemIndex;
   //Now create the image
   if Image.Format(NewImageForm.MainFormat.ItemIndex,minor,tracks) then
   begin
    CloseAllHexDumps;
    HasChanged:=True;
    ShowNewImage(Image.Filename);  //This updates the status bar
   end
   else
    ReportError('Failed to create image');
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Attribute has been changed
{------------------------------------------------------------------------------}
procedure TMainForm.AttributeChangeClick(Sender: TObject);
var
 att,filepath: String;
begin
 if not DoNotUpdate then
 begin
  att:='';
   //Attributes - DFS and UEF
   if(Image.FormatNumber>>4=diAcornDFS)
   or(Image.FormatNumber>>4=diAcornUEF)then
    if cb_DFS_l.Checked then att:=att+'L';
   //Attributes - ADFS
   if Image.FormatNumber>>4=diAcornADFS then
   begin
    if cb_ADFS_ownw.Checked then att:=att+'W';
    if cb_ADFS_ownr.Checked then att:=att+'R';
    if cb_ADFS_ownl.Checked then att:=att+'L';
    if cb_ADFS_owne.Checked then att:=att+'E';
    if cb_ADFS_pubw.Checked then att:=att+'w';
    if cb_ADFS_pubr.Checked then att:=att+'r';
    if cb_ADFS_pube.Checked then att:=att+'e';
    if cb_ADFS_pubp.Checked then att:=att+'P';
   end;
   //Attributes - Commodore 64
   if Image.FormatNumber>>4=diCommodore then
   begin
    if cb_C64_c.Checked then att:=att+'C';
    if cb_C64_l.Checked then att:=att+'L';
   end;
   if TMyTreeNode(DirList.Selected).IsDir then att:=att+'D';
   //Get the file path
   filepath:=GetFilePath(DirList.Selected);
   //Update the attributes for the file
   if Image.UpdateAttributes(filepath,att,DirList.Selected.Index) then
   begin
    HasChanged:=True;
    //Update the status bar
    UpdateImageInfo
   end
   else
   begin
    //If unsuccessful, revert back.
    DoNotUpdate:=True;
    TCheckbox(Sender).Checked:=not TCheckbox(Sender).Checked;
    DoNotUpdate:=False;
   end;
 end;
end;

{------------------------------------------------------------------------------}
//Get full file path from selected node
{------------------------------------------------------------------------------}
function TMainForm.GetFilePath(Node: TTreeNode): String;
begin
 Result:='';
 //Make sure that the node exists
 if Node<>nil then
 begin
  //Start with the text in the node (i.e the filename)
  Result:=Node.Text;
  //If it has a parent, then add this and move up a level, until we reach the root
  while Node.Parent<>nil do
  begin
   Node:=Node.Parent;
   Result:=Node.Text+Image.DirSep+Result;
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Delete file
{------------------------------------------------------------------------------}
procedure TMainForm.DeleteFile1Click(Sender: TObject);
var
 R  : Boolean;
begin
 //Result of the confirmation - assumed Yes for now
 R:=True;
 //For mulitple deletes, ensure that the user really wants to
 if DirList.SelectionCount>1 then
  R:=AskConfirm('Delete '+IntToStr(DirList.SelectionCount)+' files?'
               ,'Yes','No','')=mrOK;
 //If user does, or single file, continue
 if R then DeleteFile(True);
end;

{------------------------------------------------------------------------------}
//Delete selected files
{------------------------------------------------------------------------------}
procedure TMainForm.DeleteFile(confirm: Boolean);
var
 i,j     : Integer;
 R       : Boolean;
 filepath: String;
begin
 //Go through all the selections (or the only one)
 for i:=0 to DirList.SelectionCount-1 do
 begin
  //Get the full path to the file
  filepath:=GetFilePath(DirList.Selections[i]);
  //If singular, check if the user wants to
  if(DirList.SelectionCount=1)and(confirm)then
   R:=AskConfirm('Delete '+filepath+'?','Yes','No','')=mrOK
  else R:=True;
  //If so, then delete
  if R then
   if Image.DeleteFile(filepath) then
   begin
    HasChanged:=True;
    //Update the status bar
    UpdateImageInfo;
    //Now update the node and filedetails panel
    DirList.Selections[i].Delete;
    ResetFileFields;
    //Close any open HexDumps
    if Length(HexDump)>0 then
     for j:=0 to Length(HexDump)-1 do
     begin
      //Remove the window
      if HexDump[j].Caption=filepath then
      begin
       //We can just hide the window, and change the title.
       HexDump[j].Caption:='Deleted';
       //When the image is closed, it'll get freed up anyway.
       HexDump[j].Hide;
      end;
      //Remove the menu item
      if HexDumpMenu.Count>j then
       if HexDumpMenu.Items[j].Caption=filepath then
        HexDumpMenu.Items[j].Free;
     end;
   end;
 end;
end;

{------------------------------------------------------------------------------}
//User has double clicked on the DirList box
{------------------------------------------------------------------------------}
procedure TMainForm.DirListDblClick(Sender: TObject);
var
 Node    : TTreeNode;
 index,
 i,
 entry,
 dir     : Integer;
 filename: String;
 buffer  : TDIByteArray;
 menuitem: TMenuItem;
begin
 //Reset the drag/drop flags
 imgCopy.Visible:=False;
 MouseIsDown:=False;
 IsDragging:=False;
 if ObjectDrag<>nil then ObjectDrag.Free;
 ObjectDrag:=nil;
 //Get the selected Node (currently only a single node can be selected)
 Node:=DirList.Selected;
 //Only act on it if there is one selected
 if Node<>nil then
  //If it is not a directory, then proceed to open it, otherwise the system
  //will just expand the node
  if not (TMyTreeNode(Node).IsDir) then
  begin
   //Get the entry and dir references
   entry:=Node.Index;
   dir:=-1;
   //If the node does not have a parent, then the dir ref is the one contained
   //in the extra info. Otherwise is -1
   if Node.Parent<>nil then
   begin
    dir  :=TMyTreeNode(Node).ParentDir;
    //Get the full filename with path
    filename:=Image.Disc[dir].Entries[entry].Parent+
              Image.DirSep+
              Image.Disc[dir].Entries[entry].Filename;
    //Load the file
    if Image.ExtractFile(filename,buffer,entry) then
    begin
     //Check it is not already open
     i:=0;
     index:=-1;
     while(i<Length(HexDump))and(index=-1)do
     begin
      if HexDump[i].Caption=filename then index:=i;
      inc(i);
     end;
     if index=-1 then
     begin
      //Get the current number of windows open
      index:=Length(HexDump);    //this will also point to the last index
      SetLength(HexDump,index+1);//now that we've increased it by one
      //Create a new instance of the form
      HexDump[index]:=THexDumpForm.Create(nil);
      //Put the name in the caption
      HexDump[index].Caption:=filename;
      //Switch to the Hex Dump tab (this can change, depending on the contents)
      HexDump[index].PageControl.ActivePage:=HexDump[index].HexDump;
      //Add to the menu
      menuitem:=TMenuItem.Create(HexDumpMenu);
      menuitem.Caption:=filename;
      menuitem.OnClick:=@HexDumpSubItemClick;
      HexDumpMenu.Add(menuitem);
     end;
     //Move the file across
     SetLength(HexDump[index].buffer,Length(buffer));
     for i:=0 to Length(buffer)-1 do
      HexDump[index].buffer[i]:=buffer[i];
     //And show it
     HexDump[index].Show;
     //Is it a BASIC file, or a text viewable file?
     if(Image.Disc[dir].Entries[entry].ShortFileType='FFB')
     or(Image.Disc[dir].Entries[entry].ShortFileType='FFF')
     or(Image.Disc[dir].Entries[entry].ShortFileType='FEB')
     or(HexDump[index].IsBasicFile)then
      HexDump[index].DecodeBasicFile;
     //Is in an image we can show?
     HexDump[index].DisplayImage; //If not, it won't show it
     //Is it a sprite file?
     if Image.Disc[dir].Entries[entry].ShortFileType='FF9' then
      HexDump[index].DisplaySpriteFile;
    end;
   end;
  end;
end;

{------------------------------------------------------------------------------}
//User has clicked on a hexdump menu item
{------------------------------------------------------------------------------}
procedure TMainForm.HexDumpSubItemClick(Sender: TObject);
var
 i: Integer;
begin
 i:=0;
 //Find and open the appropriate hex dump window
 for i:=0 to Length(HexDump)-1 do
  if HexDump[i].Caption=TMenuItem(Sender).Caption then HexDump[i].Show;
end;

{------------------------------------------------------------------------------}
//User has double clicked on the DirList box
{------------------------------------------------------------------------------}
procedure TMainForm.CloseAllHexDumps;
begin
 //Close all the HexDump forms dynamically created
 while Length(HexDump)>0 do
 begin
  HexDump[Length(HexDump)-1].Free;
  SetLength(HexDump,Length(HexDump)-1);
 end;
 //Remove all the menu items
 while HexDumpMenu.Count>0 do
  HexDumpMenu.Items[0].Free;
end;

{------------------------------------------------------------------------------}
//Rename menu item has been clicked
{------------------------------------------------------------------------------}
procedure TMainForm.RenameFile1Click(Sender: TObject);
begin
 //Make sure we're not dealing with the root
 if DirList.Selected.Parent<>nil then
 begin
  //Save the path and name before they get edited
  PathBeforeEdit:=GetFilePath(DirList.Selected);
  NameBeforeEdit:=DirList.Selected.Text;
  //Set the node to edit mode
  DirList.Selected.EditText;
 end;
end;

{------------------------------------------------------------------------------}
//Can we rename this node or not?
{------------------------------------------------------------------------------}
procedure TMainForm.DirListEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
 //Set the node to edit mode, if not the root
 AllowEdit:=Node.Parent<>nil;
 //Save the path and name before they get edited
 PathBeforeEdit:=GetFilePath(Node);
 NameBeforeEdit:=Node.Text;
end;

{------------------------------------------------------------------------------}
//Rename file/directory
{------------------------------------------------------------------------------}
procedure TMainForm.DirListEditingEnd(Sender: TObject; Node: TTreeNode;
 Cancel: Boolean);
var
 newfilename,
 fileparent  : String;
 newindex,i  : Integer;
 NewNode     : TTreeNode;
begin
 newfilename:=Node.Text;
 if (not Cancel)and(NameBeforeEdit<>newfilename) then
 begin
  //Rename the file
  newindex:=Image.RenameFile(PathBeforeEdit,newfilename,Node.Index);
  if newindex<0 then
  begin
   //Revert if it cannot be renamed
   Node.Text:=NameBeforeEdit;
   //And report error
   if newindex=-1 then ReportError('Failed to rename file/directory');
   if newindex=-2 then ReportError('File/directory does not exist');
   if newindex=-3 then ReportError('Cannot rename file/directory - "'+newfilename+'" already exists');
   if newindex=-4 then ReportError('Could not extend parent directory - image full');
   if newindex=-5 then ReportError('Could not extend parent directory - no free fragments');
   if newindex=-6 then ReportError('Renaming unsupported in this format');
  end
  else
  begin
   HasChanged:=True;
   //Update the status bar
   UpdateImageInfo;
   //Otherwise change the text on the tree and the file details panel
   Node.Text:=newfilename;
   //We will need to delete the entry, then re-add it, as it may have moved as
   //a result of being renamed.
   if newindex<>Node.Index then
   begin
    //Move up
    while newindex<Node.Index do
    begin
     NewNode:=Node.GetPrevSibling;
     if NewNode<>nil then Node.MoveTo(NewNode,naInsert);
    end;
    //Move down
    while newindex>Node.Index do
    begin
     NewNode:=Node.GetNextSibling;
     if NewNode<>nil then NewNode.MoveTo(Node,naInsert);
    end;
   end;
   //Update the directory list
   DirListChange(Sender,Node);
   //Update hex dump window, if it is open
   if Length(HexDump)>0 then
   begin
    //Get the parent details
    fileparent:=LeftStr(PathBeforeEdit,Length(PathBeforeEdit)-Length(NameBeforeEdit));
    for i:=0 to Length(HexDump)-1 do
    begin
     //Just change the title
     if HexDump[i].Caption=PathBeforeEdit then
      HexDump[i].Caption:=fileparent+newfilename;
     //Need to update the menu item too
     if HexDumpMenu.Count>i then
      if HexDumpMenu.Items[i].Caption=PathBeforeEdit then
       HexDumpMenu.Items[i].Caption:=fileparent+newfilename;
    end;
   end;
  end;
 end;
 //Reset the tracking variables
 PathBeforeEdit:='';
 NameBeforeEdit:='';
end;

{------------------------------------------------------------------------------}
//Reset the label fields for file info
{------------------------------------------------------------------------------}
procedure TMainForm.ResetFileFields;
begin
 //Make sure that we don't fire off the tick box OnChange event
 DoNotUpdate         :=True;
 //Blank the entries
 lb_FileName.Caption :='';
 lb_filetype.Caption :='';
 lb_loadaddr.Caption :='';
 lb_execaddr.Caption :='';
 lb_length.Caption   :='';
 lb_timestamp.Caption:='';
 lb_parent.Caption   :='';
 lb_title.Caption    :='';
 lb_location.Caption :='';
 lb_CRC32.Caption    :='';
 //Enable/Disable the Directory Title editing
 lb_title.Visible    :=True;
 ed_title.Visible    :=False;
 ed_title.Enabled    :=False;
 //Blank the Filetype bitmap
 img_FileType.Picture.Bitmap:=nil;
 //Disable the access tick boxes
 ADFSAttrPanel.Visible:=False;
 DFSAttrPanel.Visible :=False;
 C64AttrPanel.Visible :=False;
 //And untick them
 cb_ADFS_ownw.Checked:=False;
 cb_ADFS_ownr.Checked:=False;
 cb_ADFS_ownl.Checked:=False;
 cb_ADFS_owne.Checked:=False;
 cb_ADFS_pubw.Checked:=False;
 cb_ADFS_pubr.Checked:=False;
 cb_ADFS_pube.Checked:=False;
 cb_ADFS_pubp.Checked:=False;
 cb_DFS_l.Checked    :=False;
 cb_C64_l.Checked    :=False;
 cb_C64_c.Checked    :=False;
 //Allow the OnChange to fire again
 DoNotUpdate         :=False;
 //Hide all the labels
 ArrangeFileDetails;
end;

{------------------------------------------------------------------------------}
//Converts a number into a string with trailing 'Bytes', 'KB', etc.
{------------------------------------------------------------------------------}
function TMainForm.ConvertToKMG(size: Int64): String;
var
 new_size_int : Int64; //Int64 will allow for huge disc sizes
 new_size_dec,
 level,
 multiplier   : Integer;
const
 sizes: array[1..6] of String = ('Bytes','KB','MB','GB','TB','EB');
begin
 //Default is in bytes
 Result:=IntToStr(size)+' '+sizes[1];
 //How far through the array
 level:=0;
 //Current multiplier - KB is 1024 bytes, but MB is 1000 KB...odd, I know!
 multiplier:=1024;
 //We just do this as the first thing we do is divide
 new_size_int:=size*multiplier;
 repeat //we could use a While Do loop and avoid the pre-multiplying above
  inc(level); //Next level, or entry into the array
  //There are 1000KB per MB, 1000MB per GB, etc. but 1024 bytes per KB, etc.
  if level>2 then multiplier:=1000;
  //Work out the remainder, as a decimal to three decimal places
  new_size_dec:=Round(((new_size_int mod multiplier)/multiplier)*1000);
  //Reduce the size to the next smaller multiple
  new_size_int:=new_size_int div multiplier;
  //and repeat until we are smaller than the current multiple, or out of choices
 until (new_size_int<multiplier) or (level=High(sizes));
 //If level is valid
 if level<=High(sizes) then
 begin
  //First the major, whole number, portion
  Result:=IntToStr(new_size_int);
  //Then the decimal portion, but only if there is one
  if new_size_dec>0 then
  begin
   //Add an extra zero, which will be removed, and pad to the left with zeros
   Result:=Result+'.'+RightStr('000'+IntToStr(new_size_dec),3)+'0';
   //Now remove all the surplus zeros
   repeat
    Result:=Copy(Result,1,Length(Result)-1);
   until Result[Length(Result)]<>'0';
  end;
  //And finally return the result, with the unit
  Result:=Result+' '+sizes[level];
 end;
end;

{------------------------------------------------------------------------------}
//Converts Int64 to string, adding in the thousand separator ','
{------------------------------------------------------------------------------}
function TMainForm.IntToStrComma(size: Int64): String;
begin
 Result:=Format('%.0n',[1.0*size]);
end;

{------------------------------------------------------------------------------}
//Custom redraw event for the status bar
{------------------------------------------------------------------------------}
procedure TMainForm.ImageDetailsDrawPanel(StatusBar: TStatusBar;
 Panel: TStatusPanel; const Rect: TRect);
var
 png    : Byte;
 imgRect: TRect;
begin
 //Set up the rectangle for the image - giving it 2px border
 imgRect.Top:=Rect.Top+3;
 imgRect.Left:=Rect.Left+3;
 imgRect.Height:=Rect.Height-6;
 imgRect.Width:=imgRect.Height;
 //First panel - we want to put the 'not saved' indicator here
 if(Panel.Index=0)and(HasChanged)then
  icons.StretchDraw(StatusBar.Canvas,changedicon,imgRect);
 //Second panel - needs a logo
 if(Panel.Index=1)and(Panel.Text<>'')then
 begin
  png:=0;
  case Image.FormatNumber>>4 of
   diAcornDFS : png:=bbclogo;       //BBC Micro logo for DFS
   diAcornADFS:
   begin
    if(Image.FormatNumber mod $10<>3)
    and(Image.MapType=0) then png:=acornlogo; //Acorn logo for 8 bit ADFS
    if(Image.FormatNumber mod $10=3)
    or(Image.MapType=1) then png:=riscoslogo; //RISC OS logo for 32 bit ADFS
   end;
   diCommodore: png:=commodorelogo; //Commodore logo
   diSinclair : png:=sinclairlogo;  //Sinclair logo
   diAmiga    : png:=amigalogo;     //Amiga logo
   diAcornUEF : png:=acornlogo;     //Acorn logo for CFS
   diMMFS     : png:=bbclogo;       //BBC Micro logo for MMFS
  end;
  Rect.Height:=Rect.Height-2;
  //Draw the icon
  if png<>0 then
   icons.StretchDraw(StatusBar.Canvas,png,imgRect);
  //And display the text
  StatusBar.Canvas.TextRect(Rect,
                            Rect.Left+imgRect.Width+5,
                            Rect.Top+1,
                            Panel.Text,
                            StatusBar.Canvas.TextStyle);
 end;
 if Panel.Index>1 then
  StatusBar.Canvas.TextRect(Rect,
                            Rect.Left+2,
                            Rect.Top+1,
                            Panel.Text,
                            StatusBar.Canvas.TextStyle);
end;

{------------------------------------------------------------------------------}
//Validate a filename for Windows
{------------------------------------------------------------------------------}
procedure TMainForm.ValidateFilename(var f: String);
var
 i: Integer;
begin
 for i:=1 to Length(f) do
  if (f[i]='\')
  or (f[i]='/')
  or (f[i]=':')
  or (f[i]='*')
  or (f[i]='?')
  or (f[i]='"')
  or (f[i]='<')
  or (f[i]='>')
  or (f[i]='|') then
   f[i]:=' ';
end;

{------------------------------------------------------------------------------}
//Report an error to the user
{------------------------------------------------------------------------------}
procedure TMainForm.ReportError(error: String);
begin
 if ErrorReporting then
  //If we are in command line mode, then do not use the GUI
  if ParamCount>0 then
   WriteLn(error)
  else //Otherwise, display a nice window on the screen
   CustomDialogue.ShowError(error,'');
   //QuestionDlg('Error',error,mtError,[mbOK],0);
end;

{------------------------------------------------------------------------------}
//Ask the user for confirmation
{------------------------------------------------------------------------------}
function TMainForm.AskConfirm(confim,okbtn,cancelbtn,ignorebtn: String): TModalResult;
begin
 if ErrorReporting then
  //If we are in command line mode, then do not use the GUI
{  if ParamCount>0 then
   WriteLn(confirm)
  else //Otherwise, display a nice window on the screen}
   CustomDialogue.ShowConfirm(confim,okbtn,cancelbtn,ignorebtn);
 Result:=CustomDialogue.ModalResult;
end;

{------------------------------------------------------------------------------}
//Show information to the user
{------------------------------------------------------------------------------}
procedure TMainForm.ShowInfo(info: String);
begin
 if ErrorReporting then
  //If we are in command line mode, then do not use the GUI
  if ParamCount>0 then
   WriteLn(info)
  else //Otherwise, display a nice window on the screen
   CustomDialogue.ShowInfo(info,'');
end;

{------------------------------------------------------------------------------}
//Update the progress text
{------------------------------------------------------------------------------}
procedure TMainForm.UpdateProgress(Fupdate: String);
begin
 ProgressForm.UpdateProgress.Caption:=Fupdate;
 Application.ProcessMessages;
end;

{------------------------------------------------------------------------------}
//Texture the form/components
{------------------------------------------------------------------------------}
procedure TMainForm.TileCanvas(c: TCanvas);
var
 rc: TRect;
begin
 rc:=Rect(0,0,c.Width,c.Height);
 TileCanvas(c,rc);
end;
procedure TMainForm.TileCanvas(c: TCanvas;rc: TRect);
var
 b: TBrush;
begin
 b:=Tbrush.Create;
 b.Bitmap:=TextureTile.Picture.Bitmap;
 c.Brush :=b;
 c.FillRect(rc);
 b.Free;
end;

end.
