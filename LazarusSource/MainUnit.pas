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
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, DiscImage,
  Global, DiscImageUtils, ExtCtrls, Buttons, ComCtrls, Menus, DateUtils,
  ImgList, StrUtils, Clipbrd, HexDumpUnit, Spark, FPImage, IntfGraphics,
  ActnList, GraphType, DateTimePicker, Types;

type
 //We need a custom TTreeNode, as we want to tag on some extra information
 //Will need to set the OnCreateNodeClass event in the TTreeView
 TMyTreeNode = class(TTreeNode)
  private
   FDirRef,
   FParentDir : Integer;
   FIsDir     : Boolean;
  public
   property ParentDir: Integer read FParentDir write FParentDir;//Parent directory reference
   property IsDir    : Boolean read FIsDir write FIsDir;        //Is it a directory
   property DirRef   : Integer read FDirRef write FDirRef;      //Reference into TDiscImage.Disc
 end;

 //Form definition

  { TMainForm }
type
  TMainForm = class(TForm)
   AFSAttrPanel: TPanel;
   CancelDragDrop: TAction;
   cb_AFS_ownl: TCheckBox;
   cb_AFS_ownr: TCheckBox;
   cb_AFS_ownw: TCheckBox;
   cb_AFS_pubr: TCheckBox;
   cb_AFS_pubw: TCheckBox;
   DuplicateFile1: TMenuItem;
   IyonixTextureTile: TImage;
   DeletePartition1: TMenuItem;
   AddPasswordFile1: TMenuItem;
   menuAddPasswordFile: TMenuItem;
   menuEditPasswordFile: TMenuItem;
   EditPasswordFile1: TMenuItem;
   menuSavePartition: TMenuItem;
   menuDeletePartition: TMenuItem;
   AFSOAAttributeLabel: TLabel;
   PartitionMenu: TMenuItem;
   AFSPubAttributeLabel: TLabel;
   SavePartition1: TMenuItem;
   ROPiTextureTile: TImage;
   RO3TextureTile: TImage;
   menuDuplicateFile: TMenuItem;
   menuOptions: TMenuItem;
   PasteFromClipboard: TAction;
   CopyToClipboard: TAction;
   KeyboardShortcuts: TActionList;
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
   ed_timestamp: TDateTimePicker;
   ed_loadaddr: TEdit;
   ed_execaddr: TEdit;
   ed_title: TEdit;
   FilenameLabel: TLabel;
   icons: TImageList;
   FileImages: TImageList;
   NoTile: TImage;
   RO4TextureTile: TImage;
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
   btn_Settings: TToolButton;
   btn_DuplicateFile: TToolButton;
   ToolButton1: TToolButton;
   btn_AddPassword: TToolButton;
   btn_EditPassword: TToolButton;
   btn_DeletePartition: TToolButton;
   btn_SavePartition: TToolButton;
   ToolSplitter: TSplitter;
   TimeStampPanel: TPanel;
   ParentPanel: TPanel;
   RO5TextureTile: TImage;
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
   procedure btn_SettingsClick(Sender: TObject);
   procedure btn_SplitDFSClick(Sender: TObject);
   procedure DuplicateFile1Click(Sender: TObject);
   procedure ed_timestampEditingDone(Sender: TObject);
   procedure HexDumpSubItemClick(Sender: TObject);
   procedure lb_execaddrClick(Sender: TObject);
   procedure lb_loadaddrClick(Sender: TObject);
   procedure lb_timestampClick(Sender: TObject);
   procedure lb_titleClick(Sender: TObject);
   procedure sb_FileTypeClick(Sender: TObject);
   procedure FileTypeClick(Sender: TObject);
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
   procedure CancelDragDropExecute(Sender: TObject);
   //Events - Form
   procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
   procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
   procedure FormShow(Sender: TObject);
   procedure FormCreate(Sender: TObject);
   procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
   procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
   procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
   //Events - Other
   procedure ed_execaddrEditingDone(Sender: TObject);
   procedure ed_titleEditingDone(Sender: TObject);
   procedure DirListCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
   procedure DirListEditingEnd(Sender: TObject; Node: TTreeNode;
    Cancel: Boolean);
   procedure DirListGetImageIndex(Sender: TObject; Node: TTreeNode);
   procedure DirListChange(Sender: TObject; Node: TTreeNode);
   procedure DirListEditing(Sender: TObject; Node: TTreeNode;
     var AllowEdit: Boolean);
   procedure DirListExit(Sender: TObject);
   procedure DirListCustomDraw(Sender: TCustomTreeView; const ARect: TRect;
    var DefaultDraw: Boolean);
   procedure DirListCustomDrawArrow(Sender: TCustomTreeView;
    const ARect: TRect; ACollapsed: Boolean);
   procedure DirListCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
    State: TCustomDrawState; var DefaultDraw: Boolean);
   procedure ImageDetailsDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
    const Rect: TRect);
   procedure FileInfoPanelPaint(Sender: TObject);
   procedure FileInfoPanelResize(Sender: TObject);
   procedure ed_execaddrKeyPress(Sender: TObject; var Key: char);
   procedure FileTypeKeyPress(Sender: TObject; var Key: char);
   procedure DirListContextPopup(Sender: TObject; MousePos: TPoint;
    var Handled: Boolean);
   procedure CopyToClipboardExecute(Sender: TObject);
   procedure PasteFromClipboardExecute(Sender: TObject);
   //Misc
   function CreateDirectory(dirname,attr: String): TTreeNode;
   procedure ImportFiles(NewImage: TDiscImage);
   procedure SwapLabelEdit(editcont: TEdit;labelcont: TLabel;dir,hex: Boolean);
   function QueryUnsaved: Boolean;
   function GetFilePath(Node: TTreeNode): String;
   procedure DeleteFile(confirm: Boolean);
   procedure GetImageIndex(Node: TTreeNode;ImageToUse: TDiscImage);
   procedure DisableControls;
   procedure ParseCommandLine(cmd: String);
   procedure ResetFileFields;
   procedure ExtractFiles(ShowDialogue: Boolean);
   function GetImageFilename(dir,entry: Integer): String;
   function GetWindowsFilename(dir,entry: Integer): String;
   procedure DownLoadFile(dir,entry: Integer; path: String;filename: String='');
   procedure CreateINFFile(dir,entry: Integer; path: String;filename: String='');
   procedure DownLoadDirectory(dir,entry: Integer; path: String);
   procedure OpenImage(filename: String);
   procedure AddDirectoryToTree(CurrDir: TTreeNode; dir: Integer;
                                    ImageToUse:TDiscImage;var highdir: Integer);
   procedure ShowNewImage(title: String);
   procedure AddImageToTree(Tree: TTreeView;ImageToUse: TDiscImage);
   function ConvertToKMG(size: Int64): String;
   function IntToStrComma(size: Int64): String;
   procedure ValidateFilename(var f: String);
   procedure SelectNode(filename: String;casesens:Boolean=True);
   procedure CloseAllHexDumps;
   function AddFileToTree(ParentNode: TTreeNode;importfilename: String;
      index:Integer;dir:Boolean;Tree:TTreeView{;ImageToUse:TDiscImage}):TTreeNode;
   procedure AddDirectoryToImage(dirname: String);
   procedure AddSparkToImage(filename: String);
   procedure ShowErrorLog;
   function AddFileToImage(filename: String):Integer;
   function AddFileToImage(filename: String;filedetails: TDirEntry;
           buffer:TDIByteArray=nil;ignoreerror:Boolean=False):Integer; overload;
   function AddFileErrorToText(error: Integer):String;
   procedure UpdateImageInfo(partition: Cardinal=0);
   procedure ArrangeFileDetails;
   procedure ReportError(error: String);
   function AskConfirm(confim,okbtn,cancelbtn,ignorebtn: String): TModalResult;
   procedure ShowInfo(info: String);
   function GetCopyMode(Shift: TShiftState): Boolean;
   function GetNodeAt(Y: Integer): TTreeNode;
   procedure UpdateProgress(Fupdate: String);
   procedure TileCanvas(c: TCanvas);
   procedure TileCanvas(c: TCanvas;rc: TRect); overload;
   function GetTextureTile(Ltile:Integer=-1): TBitmap;
   procedure CreateFileTypeDialogue;
   procedure DoCopyMove(copymode: Boolean);
   procedure WriteToDebug(line: String);
  private
   var
    //To keep track of renames
    PathBeforeEdit,
    NameBeforeEdit:String;
    //Stop the checkbox OnClick from firing when just changing the values
    DoNotUpdate   :Boolean;
    //Has the image changed since last saved?
    HasChanged    :Boolean;
    //Item being dragged on Directory List, and the destination
    Dst,
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
    //Texture type
    TextureType   :Byte;
    //ADFS L Interleaved
    ADFSInterleave:Byte;
    //Treat Spark as Filing System
    SparkIsFS     :Boolean;
    //Importing bypass GUI threshold
    bypassGUIThres:Cardinal;
    //Create INF File?
    DoCreateINF   :Boolean;
    //Hide Commodore DEL files?
    DoHideDEL     :Boolean;
    //Filetype Dialogue Form
    FTDialogue    :TForm;
    //Filetype buttons on dialogue form
    FTButtons     :array of TSpeedButton;
    //Dummy button for dialogue form
    FTDummyBtn    :TSpeedButton;
    //Custom filetype on dialogue form
    FTEdit        :TEdit;
    //Keep a track of which buttons are pressed on the form
    FormShiftState:TShiftState;
    //Produce a log file for debugging
    Fdebug        :Boolean;
    //Allow DFS images with zero number of sectors
    FDFSZeroSecs  :Boolean;
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
    sparklogo     = 7;
    //Time and Date format
    TimeDateFormat = 'hh:nn:ss dd mmm yyyy';
  public
   //The image
   Image: TDiscImage;
   //Debug log filename
   debuglogfile  :String;
   const
    //Application Title
    ApplicationTitle   = 'Disc Image Manager';
    ApplicationVersion = '1.33';
    //Current platform and architecture (compile time directive)
    {$IFDEF Darwin}
    platform = 'macOS';            //Apple Mac OS X
    {$ENDIF}
    {$IFDEF Windows}
    platform = 'Windows';          //Microsoft Windows
    {$ENDIF}
    {$IFDEF Linux}
    platform = 'Linux';            //Linux
    {$ENDIF}
    {$IFNDEF Darwin}{$IFNDEF Windows}{$IFNDEF Linux}
    platform = 'OS?';              //Unknown OS
    {$ENDIF}{$ENDIF}{$ENDIF}
    {$IFNDEF CPUARM}
     {$IFDEF CPU32}
     arch = '32 bit';               //32 bit CPU
     {$ENDIF}
     {$IFDEF CPU64}
     arch = '64 bit';               //64 bit CPU
     {$ENDIF}
    {$ENDIF}
    {$IFDEF CPUARM}
     {$IFDEF CPU32}
     arch = '32 bit ARM';          //32 bit ARM CPU
     {$ENDIF}
     {$IFDEF CPU64}
     arch = '64 bit ARM';          //64 bit ARM CPU
     {$ENDIF}
    {$ENDIF}
    {$IFNDEF CPU32}{$IFNDEF CPU64}
     arch = 'CPU?';                //Unknown CPU
    {$ENDIF}{$ENDIF}
   procedure AfterConstruction; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  AboutUnit,NewImageUnit,ImageDetailUnit,ProgressUnit,SplitDFSUnit,SearchUnit,
  CustomDialogueUnit,ErrorLogUnit,SettingsUnit,ImportSelectorUnit;

{------------------------------------------------------------------------------}
//Rescale the form
{------------------------------------------------------------------------------}
procedure TMainForm.AfterConstruction;
var
 i: Integer;
begin
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
 if Image.FormatNumber>>4=diAcornADFS then
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
 Index,
 error      : Integer;
 filedetails: TDirEntry;
 ParentDir,
 temp       : String;
 buffer     : TDIByteArray;
 ok,
 bypassGUI  : Boolean;
begin
 bypassGUI:=False; //Bypass the GUI part, to speed it up
 //Reset the error list
 ErrorLogForm.ErrorLog.Clear;
 ErrorReporting:=False;
 //Make sure the file exists
 if FileExists(filename) then
 begin
  //Open up as a spark archive
  SparkFile:=TSpark.Create(filename);
  //And make sure it is valid
  if SparkFile.IsSpark then
  begin
   //Get, or set, the selected node
   if(DirList.SelectionCount=0)OR(DirList.SelectionCount>1)then
   begin
    DirList.ClearSelection;
    DirList.Items[0].Selected:=True;
   end;
   ParentDir:=GetImageFilename(TMyTreeNode(DirList.Selected).ParentDir,
                                           DirList.Selected.Index);
   //And make sure that there are some entries
   if Length(SparkFile.FileList)>0 then
   begin
    //Is the current open image suitable
    ok:=True;
    if((SparkFile.MaxDirEnt>47)and(Image.DirectoryType=diADFSOldDir))//Old dir
    or((SparkFile.MaxDirEnt>77)and(Image.DirectoryType=diADFSNewDir))//New dir
    or(Image.FormatNumber>>4<>diAcornADFS)                //Acorn ADFS
    or(SparkFile.UncompressedSize>Image.FreeSpace(0))then //Not enough space
     ok:=AskConfirm('The current open image is not suitable for this archive. '
                   +'Would you like to continue?','Yes','No','')=mrOK;
    if ok then
    begin
     //Show the progress form
     ProgressForm.Show;
     //Bypass the GUI if over a certain number of files
     if Length(SparkFile.FileList)>bypassGUIThres then bypassGUI:=True;
     //Indicate we will be doing some updating
     Image.BeginUpdate;
     //Counter into the file list
     index:=0;
     //OK flag - so we can stop on an error
     ok:=True;
     //Iterate through them all
     while(index<Length(SparkFile.FileList))and(ok)do
     begin
      //Update the progress
      if SparkFile.FileList[index].Parent<>'' then
       temp:=SparkFile.FileList[index].Parent+Image.DirSep
      else
       temp:='';
      UpdateProgress('Adding '+ParentDir+Image.DirSep+temp
                    +SparkFile.FileList[index].Filename
                    +' ('
                    +IntToStr(Round((index/Length(SparkFile.FileList))*100))
                    +'%)');
      //Select the parent node (if not bypassing the GUI)
      if not bypassGUI then
       if SparkFile.FileList[Index].Parent<>'' then
        SelectNode(ParentDir+Image.DirSep+SparkFile.FileList[Index].Parent)
       else
        SelectNode(ParentDir);
      //Update the attributes
      filedetails.Attributes:=GetAttributes(
                                IntToHex(SparkFile.FileList[Index].Attributes,2),
                                Image.FormatNumber>>4);
      //Assign the parent directory (if bypassing the GUI)
      if bypassGUI then
      begin
       if SparkFile.FileList[index].Parent<>'' then
        temp:=Image.DirSep+SparkFile.FileList[index].Parent
       else
        temp:='';
       temp:=ParentDir+temp;
       filedetails.Parent:=temp;
      end;
      //Error number
      error:=0;
      //Add a directory
      if SparkFile.FileList[Index].Directory then
      begin
       filedetails.Attributes:='D'+filedetails.Attributes;
       if bypassGUI then
        error:=Image.CreateDirectory(SparkFile.FileList[index].Filename,
                                     temp,
                                     filedetails.Attributes)
       else
        CreateDirectory(SparkFile.FileList[Index].Filename,
                        filedetails.Attributes); //Does not return any errors
      end;
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
        //Add the file - we'll skip any errors, but record them later
        if bypassGUI then
         error:=Image.WriteFile(filedetails,buffer)
        else
         error:=AddFileToImage('',filedetails,buffer,True);
       end;
      end;
      //Has an error occured? Then record it
      if error<0 then
       ErrorLogForm.ErrorLog.Lines.Add('Error while adding file "'
                                      +filedetails.Parent+Image.DirSep
                                      +SparkFile.FileList[Index].Filename
                                      +'": '
                                      +AddFileErrorToText(-error));
      if ok then inc(index);
     end;
     //End the updating
     Image.EndUpdate;
    end;
   end;
  end;
  //Free the Spark instance
  SparkFile.Free;
  //Hide the progress form
  ProgressForm.Hide;
  //And read in the entire disc again, if bypassing the GUI
  if bypassGUI then ShowNewImage(Image.Filename);
 end
 else ErrorLogForm.ErrorLog.Lines.Add('Could not find file "'+filename+'"');
 //Show the error log
 ShowErrorLog;
 ErrorReporting:=True;
end;

{------------------------------------------------------------------------------}
//Show the error log
{------------------------------------------------------------------------------}
procedure TMainForm.ShowErrorLog;
begin
 //Only show the log if there are some errors
 if ErrorLogForm.ErrorLog.Lines.Count>0 then
 begin
  //Show the form
  ErrorLogForm.Show;
  //Position the form
  ErrorLogForm.Top:=Top+Height;
  ErrorLogForm.Left:=Left;
  //Resize the form
  ErrorLogForm.Width:=Width;
  ErrorLogForm.Height:=Round(64*Screen.PixelsPerInch/ErrorLogForm.DesignTimePPI);
  //If it is off the screen, put it on the screen
  if ErrorLogForm.Top>Screen.DesktopHeight then ErrorLogForm.Top:=0;
 end;
end;

{------------------------------------------------------------------------------}
//Add a file to an image
{------------------------------------------------------------------------------}
function TMainForm.AddFileToImage(filename: String):Integer;
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
                     buffer:TDIByteArray=nil;ignoreerror:Boolean=False):Integer;
var
  NewFile        : TDirEntry;
  index          : Integer;
  side,ref       : Cardinal;
  i              : Byte;
  importfilename,
  inffile,
  execaddr,
  loadaddr,
  attr1,
  attributes,
  filetype,p     : String;
  fields         : array of String;
  chr            : Char;
  F              : TFileStream;
  ok             : Boolean;
  Node           : TTreeNode;
begin
 Result:=-5;
 //If there is nothing in the buffer
 if(Length(buffer)=0)and(FileExists(filename))then
 begin
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
 if Length(buffer)>0 then //Only try and add if there is some data
 begin
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
     //ADFS, AFS, DFS & CFS only stuff
     if((Image.FormatNumber>>4=diAcornDFS)
      or(Image.FormatNumber>>4=diAcornADFS)
      or(Image.FormatNumber>>4=diAcornUEF)
      or(Image.FormatNumber>>4=diAcornFS))
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
     if filedetails.Filename<>''   then importfilename:=filedetails.Filename;
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
     attributes:=attributes+GetAttributes(attr1,Image.FormatNumber>>4);
     if importfilename='' then importfilename:='NewFile';
     //Validate the filename (ADFS, AFS, DFS & CFS only)
     if(Image.FormatNumber>>4=diAcornDFS)
     or(Image.FormatNumber>>4=diAcornADFS)
     or(Image.FormatNumber>>4=diAcornUEF)
     or(Image.FormatNumber>>4=diAcornFS)then
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
       or(Image.FormatNumber>>4=diAcornADFS)
       or(Image.FormatNumber>>4=diAcornFS))
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
     if(Image.FormatNumber>>4=diAcornADFS) //Need the selected directory for ADFS
     or(Image.FormatNumber>>4=diAcornFS)then//And Acorn FS
      if(DirList.Selected.Text='$')
      or(DirList.Selected.Text='AFS$')then NewFile.Parent:=DirList.Selected.Text
      else
       NewFile.Parent    :=GetImageFilename(TMyTreeNode(DirList.Selected).ParentDir,
                                            DirList.Selected.Index);
     if Image.FormatNumber>>4=diAcornDFS then //We'll set up a parent for DFS
      NewFile.Parent:=':'+IntToStr(side*2)+'.$';
     //Set the length - the actual length overrides everything else
     NewFile.Length:=Length(buffer);
     //Does the file already exist?
     ok:=True;
     if Image.FileExists(NewFile.Parent+Image.DirSep+NewFile.Filename,ref) then
     begin
      ok:=AskConfirm('"'+NewFile.Filename+'" already exists in the directory "'
                    +NewFile.Parent+'". Overwrite?','Yes','No','')=mrOK;
      if ok then //Delete the original
      begin
       //First save the selected Node
       Node:=DirList.Selected;
       //Select our node
       SelectNode(NewFile.Parent+Image.DirSep+NewFile.Filename,False);
       //Delete the file
       DeleteFile(False);
       //Reselect our node
       DirList.ClearSelection;
       Node.Selected:=True;
       ok:=True;
      end;
     end;
     //Write the File
     if ok then
     begin
      Result:=Image.WriteFile(NewFile,buffer);
      //Function returns pointer to next item (or parent if no children)
      if Result>-1 then //File added OK
      begin
       HasChanged:=True;
       AddFileToTree(DirList.Selected,NewFile.Filename,Result,False,DirList{,Image});
       UpdateImageInfo(side);
      end
      else
      if not ignoreerror then //For some reason the operation failed to write the data
      begin
       //Prepare the filename, including the parent
       p:=NewFile.Parent;
       if p<>'' then p:=p+Image.DirSep;
       //Report the error
       ReportError('Error when adding file "'+p+NewFile.Filename
                  +'": '+AddFileErrorToText(-Result));
      end;
     end else Result:=-3;
    end;
   end
   else ReportError('"'+DirList.Selected.Text+'" is not a directory')
  else
   if DirList.SelectionCount=0 then
    ReportError('No destination directory selected')
   else
    ReportError('Cannot add to multiple selection');
 end else Result:=-8;
end;

{------------------------------------------------------------------------------}
//Convert an error number (from adding a file) to error text
{------------------------------------------------------------------------------}
function TMainForm.AddFileErrorToText(error: Integer):String;
begin
 Result:='';
 if error<0 then error:=-error;
 case error of
  1: Result:='Could not load file';
  2: Result:='Image full';
  3: Result:='File already exists in image';
  4: Result:='Catalogue full';
  5: Result:='Error unknown';
  6: Result:='Destination directory does not exist';
  7: Result:='Map full';
  8: Result:='Nothing to write';
  9: Result:='Could not extend directory';
 end;
end;

{------------------------------------------------------------------------------}
//Add a file or directory to the TTreeView, under ParentNode
{------------------------------------------------------------------------------}
function TMainForm.AddFileToTree(ParentNode: TTreeNode;importfilename: String;
   index: Integer;dir: Boolean;Tree:TTreeView{;ImageToUse:TDiscImage}): TTreeNode;
begin
 Result:=nil;
 if(ParentNode=nil)or(index<0)then exit;
 RemoveTopBit(importfilename);
 //Now add the entry to the Directory List
 if ParentNode.HasChildren then
  //Insert it before the one specified
  if index<ParentNode.Count then
   Result:=Tree.Items.Insert(ParentNode.Items[index],importfilename)
  else //Unless this is the last one
   Result:=Tree.Items.AddChild(ParentNode,importfilename)
 else
  //Is the first child, so just add it
  Result:=Tree.Items.AddChildFirst(ParentNode,importfilename);
 if Result<>nil then
 begin
  //Set the reference to the node's parent
  TMyTreeNode(Result).ParentDir:=TMyTreeNode(ParentNode).DirRef;
  //Set/reset the directory flag
  TMyTreeNode(Result).IsDir:=dir;
  //If this is not a directory, it will have no reference
  if not dir then TMyTreeNode(Result).DirRef:=-1;
  Tree.Repaint;
 end;
end;

{------------------------------------------------------------------------------}
//About box
{------------------------------------------------------------------------------}
procedure TMainForm.btn_AboutClick(Sender: TObject);
begin
 //Update the Application Title
 AboutForm.lb_Title.Caption:=ApplicationTitle;
 //Update the Application Version
 AboutForm.lb_Version.Caption:='Version '
                              +ApplicationVersion+' '
                              +platform+' ('+arch+')';
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
 Nodes,
 Roots      : array of TTreeNode;
 entry,
 dir,s      : Integer;
 saver,
 showsave,
 selectroot : Boolean;
begin
 //Set up the save dialogue box
 SaveImage.DefaultExt:='';
 SaveImage.Filter:='';
 SaveImage.FilterIndex:=1;
 SaveImage.Title:='Extract File(s)/Directory(s)';
 selectroot:=False;
 showsave:=not ShowDialogue; //Indicates whether the dialogue has been shown
 if DirList.SelectionCount>0 then
 begin
  SetLength(Nodes,0);
  SetLength(Roots,0);
  //Save all the selected nodes that do not have parents
  for s:=0 to DirList.SelectionCount-1 do
   if DirList.Selections[s].Parent<>nil then
   begin
    SetLength(Nodes,Length(Nodes)+1);
    Nodes[Length(Nodes)-1]:=DirList.Selections[s];
   end;
  //Make a note of all the selected roots
  for s:=0 to DirList.SelectionCount-1 do
   if DirList.Selections[s].Parent=nil then
   begin
    SetLength(Roots,Length(Roots)+1);
    Roots[Length(Roots)-1]:=DirList.Selections[s];
   end;
  //Now for any root that is selected, select all its children
  if Length(Roots)>0 then
  begin
   selectroot:=True;
   //First deselect everything
   DirList.ClearSelection;
   for s:=0 to Length(Roots)-1 do
    //Does it have children?
    if Roots[s].HasChildren then
    begin
     //Go through them, starting at the first child
     Node:=Roots[s].GetFirstChild;
     //Continue until we run out of children
     while Node<>nil do
     begin
      //Select it
      Node.Selected:=True;
      //And get the next one, if there is one
      Node:=Node.GetNextSibling;
     end;
    end;
   //Now reselect everything that was selected and not a root
   if Length(Nodes)>0 then
    for s:=0 to Length(Nodes)-1 do
     Nodes[s].Selected:=True;
  end;
 end;
 //Only continue if something is selected
 if DirList.SelectionCount>0 then
 begin
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
       if Image.Title(Image.Disc[dir].Partition)='' then
        SaveImage.FileName:=DirList.Items[0].Text
       else
        SaveImage.FileName:=Image.Title(Image.Disc[dir].Partition);
      end
      else
       SaveImage.FileName:=GetWindowsFilename(dir,entry);
      //Get the result
      saver:=SaveImage.Execute;
      //User clicked on Cancel, so exit
      if not saver then exit;
      if(saver)and(selectroot)then //Root was selected, so create the directory
      begin
       CreateDir(SaveImage.FileName);
       SaveImage.Filename:=SaveImage.Filename+PathDelim+'root';
      end;
     end;
     if s>0 then saver:=True;
     //Download a single file
     if saver then
      //Do not download if the parent is selected, as this will get downloaded anyway
      if not DirList.Selections[s].Parent.Selected then
       if DirList.SelectionCount>1 then //If multi-selection, just use the path
        DownLoadFile(dir,entry,ExtractFilePath(SaveImage.FileName))
       else //otherwise we'll use the filename specified
        DownLoadFile(dir,entry,ExtractFilePath(SaveImage.FileName)
                              ,ExtractFileName(SaveImage.FileName));
    end;
   end;
  end;
  //Now reselect everything as it was before
  DirList.ClearSelection;
  if Length(Nodes)>0 then
   for s:=0 to Length(Nodes)-1 do
    Nodes[s].Selected:=True;
  if Length(Roots)>0 then
   for s:=0 to Length(Roots)-1 do
    Roots[s].Selected:=True;
 end;
end;

{------------------------------------------------------------------------------}
//Create an Image filename
{------------------------------------------------------------------------------}
function TMainForm.GetImageFilename(dir,entry: Integer): String;
begin
 Result:='';
 if(dir>=0)and(dir<Length(Image.Disc))then
  if Length(Image.Disc[dir].Entries)=0 then
   Result:=Image.Disc[dir].Directory
  else
   Result:=Image.GetParent(dir)+Image.DirSep
          +Image.Disc[dir].Entries[entry].Filename;
end;

{------------------------------------------------------------------------------}
//Create a Windows filename
{------------------------------------------------------------------------------}
function TMainForm.GetWindowsFilename(dir,entry: Integer): String;
var
 extsep: Char;
begin
 Result:='';
 if(dir>=0)and(dir<Length(Image.Disc))then
  if(entry>=0)and(entry<=Length(Image.Disc[dir].Entries))then
  begin
   extsep:=#0;
   //Get the filename
   Result:={Image.GetParent(dir)+Image.DirSep
          +}Image.Disc[dir].Entries[entry].Filename;
   //Convert BBC chars to PC
   BBCtoWin(Result);
   //Replace any non-valid characters
   ValidateFilename(Result);
   //Add the filetype to the end, if any (but not directories)
   if (Image.Disc[dir].Entries[entry].ShortFileType<>'')
   and(Image.Disc[dir].Entries[entry].DirRef=-1) then
   begin
    if(Image.FormatNumber>>4=diAcornDFS)
    or(Image.FormatNumber>>4=diAcornADFS)
    or(Image.FormatNumber>>4=diAcornFS)then extsep:=','; //DFS, ADFS and AFS
    if(Image.FormatNumber>>4=diCommodore)                //Commodore
    or(Image.FormatNumber>>4=diAmiga)then extsep:='.';   //Amiga
    Result:=Result+extsep+Image.Disc[dir].Entries[entry].ShortFileType;
   end;
  end;
end;

{------------------------------------------------------------------------------}
//Download a file
{------------------------------------------------------------------------------}
procedure TMainForm.DownLoadFile(dir,entry: Integer;path: String;filename: String='');
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
  if filename='' then //If a filename has not been supplied, generate one
   windowsfilename:=GetWindowsFilename(dir,entry)
  else                //Otherwise use the supplied filename
   windowsfilename:=filename;
  if Image.ExtractFile(imagefilename,buffer,entry) then
  begin
   //Save the buffer to the file
   try
    F:=TFileStream.Create(path+windowsfilename,fmCreate OR fmShareDenyNone);
    F.Position:=0;
    F.Write(buffer[0],Length(buffer));
    F.Free;
    CreateINFFile(dir,entry,path,filename);
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
procedure TMainForm.CreateINFFile(dir,entry: Integer; path: String;filename: String='');
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
 if(dir>=0)and(dir<Length(Image.Disc))then
  if(entry>=0)and(entry<Length(Image.Disc[dir].Entries))then
  begin
   //Length of the hex numbers
   hexlen:=8;
   //6 for DFS (after discussion on Stardot forum)
   if Image.FormatNumber>>4=diAcornDFS then hexlen:=6;
   if DoCreateInf then
   begin
    imagefilename:=Image.Disc[dir].Entries[entry].Filename;
    if filename='' then //If no filename has been supplied, generate one
     windowsfilename:=GetWindowsFilename(dir,entry)
    else                //Otherwise just use the supplied name
     windowsfilename:=filename;
    //Add the root, if DFS and no directory specifier
    if(Image.FormatNumber>>4=diAcornDFS)and(imagefilename[2]<>'.')then
     imagefilename:=RightStr(Image.GetParent(dir),1)+'.'+imagefilename;
    //Put quotes round the filename if it contains a space
    if Pos(' ',imagefilename)>0 then imagefilename:='"'+imagefilename+'"';
    //Create the string
    inffile:=PadRight(LeftStr(imagefilename,12),12)+' '
            +IntToHex(Image.Disc[dir].Entries[entry].LoadAddr,hexlen)+' '
            +IntToHex(Image.Disc[dir].Entries[entry].ExecAddr,hexlen)+' '
            +IntToHex(Image.Disc[dir].Entries[entry].Length,hexlen);
    //Create the attributes
    attributes:=$00;
    if(Image.FormatNumber>>4=diAcornDFS)
    or(Image.FormatNumber>>4=diAcornUEF)then //DFS and CFS
     if Image.Disc[dir].Entries[entry].Attributes='L' then attributes:=$08;
    if(Image.FormatNumber>>4=diAcornADFS)
    or(Image.FormatNumber>>4=diAcornFS)then //ADFS and AFS
     for t:=0 to 7 do
      if Pos(adfsattr[t+1],Image.Disc[dir].Entries[entry].Attributes)>0 then
       inc(attributes,1<<t);
    inffile:=inffile+' '+IntToHex(attributes,2)
            +' CRC32='+Image.GetFileCRC(Image.GetParent(dir)+Image.DirSep+
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
var
 dir,
 entry:Cardinal;
begin
 //Show a progress message
 ProgressForm.Show;
 //Process the messages to close the file dialogue box
 Application.ProcessMessages;
 //Close any open hex dump windows
 CloseAllHexDumps;
 Image.ProgressIndicator:=@UpdateProgress;
 //Update the interleave when loading
 Image.InterleaveMethod:=ADFSInterleave;
 //Treat Sparks as a filing system
 Image.SparkAsFS:=SparkIsFS;
 //Allow DFS to have zero number of sectors
 Image.AllowDFSZeroSectors:=FDFSZeroSecs;
 //Load the image and create the catalogue
 if Image.LoadFromFile(filename) then
 begin
  HasChanged:=False;
  //Write debugging information
  if Length(Image.Disc)>0 then
  begin
   WriteToDebug('Image '+filename+' opened');
   for dir:=0 to Length(Image.Disc)-1 do
   begin
    WriteToDebug(IntToStr(dir)+': '+Image.Disc[dir].Directory);
    if Length(Image.Disc[dir].Entries)>0 then
     for entry:=0 to Length(Image.Disc[dir].Entries)-1 do
      WriteToDebug(IntToStr(dir)+','+IntToStr(entry)+': '
          +Image.Disc[dir].Entries[entry].Filename+' ('
          +IntToStr(Image.Disc[dir].Entries[entry].DirRef)+')');
   end;
  end;
  //Update the display
  ShowNewImage(Image.Filename);
 end
 else
  ReportError('"'+ExtractFilename(filename)
                 +'" has not been recognised as a valid disc image that '
                 +ApplicationTitle+' can open.');
 //Close the progress message
 ProgressForm.Hide;
end;

{------------------------------------------------------------------------------}
//Copy CRC32 to clipboard
{------------------------------------------------------------------------------}
procedure TMainForm.sb_ClipboardClick(Sender: TObject);
var
 nowtime,
 starttime: TDateTime;
 labcont  : TLabel;
begin
 if Sender is TLabel then
 begin
  //Get the calling control - this is so we can service multiple labels with one method
  labcont:=TLabel(Sender);
  //Briefly change to blue
  labcont.Font.Color:=$FF0000;
  //Copy to clipboard
  Clipboard.AsText:=labcont.Caption;
  //Create a delay timer
  starttime:=Round(Time*100000);
  //TDateTime is a Double, with the time being the fraction part
  //So multiplying by 100000 gets the number of seconds
  nowtime:=starttime;
  repeat
   Application.ProcessMessages;
   nowtime:=Round(Time*100000);
  until nowtime-starttime>=1;
  //Change back to default
  labcont.Font.Color:=$000000;
 end;
end;

{------------------------------------------------------------------------------}
//Adds a directory to the TreeView - is called recursively to drill down the tree
{------------------------------------------------------------------------------}
procedure TMainForm.AddDirectoryToTree(CurrDir:TTreeNode;dir:Integer;
                                  ImageToUse:TDiscImage;var highdir:Integer);
var
 entry: Integer;
 Node: TTreeNode;
 Tree: TTreeView;
begin
 Tree:=TTreeView(CurrDir.Owner.Owner);
 //Make a note of the dir ref, it is the highest
 if dir>highdir then highdir:=dir;
 //Set the 'IsDir' flag to true, as this is a directory
 TMyTreeNode(CurrDir).IsDir:=True;
 TMyTreeNode(CurrDir).DirRef:=dir;
 //Iterate though all the entries
 for entry:=0 to Length(ImageToUse.Disc[dir].Entries)-1 do
 begin
  //Adding new nodes for each one
  Node:=AddFileToTree(CurrDir,ImageToUse.Disc[dir].Entries[entry].Filename,
                      entry,false,Tree{,ImageToUse});
  //If it is, indeed, a direcotry, the dir ref will point to the sub-dir
  if ImageToUse.Disc[dir].Entries[entry].DirRef>=0 then
   //and we'll recursively call ourself to add these entries
   AddDirectoryToTree(Node,ImageToUse.Disc[dir].Entries[entry].DirRef,
                      ImageToUse,highdir);
 end;
 if ImageToUse=Image then UpdateImageInfo(ImageToUse.Disc[dir].Partition);
end;

{------------------------------------------------------------------------------}
//Reset the display for a new/loaded image
{------------------------------------------------------------------------------}
procedure TMainForm.ShowNewImage(title: String);
begin
 //Clear all the labels, and enable/disable the buttons
 ResetFileFields;
 //Clear the search fields
 SearchForm.ResetSearchFields;
 //Change the application title (what appears on SHIFT+TAB, etc.)
 Caption:=ApplicationTitle;
 if title<>'' then Caption:=Caption+' - '+ExtractFileName(title);
 //Populate the directory view
 AddImageToTree(DirList,Image);
 //Populate the info box
 UpdateImageInfo(0);
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
 if Image.FormatNumber>>4=diAcornADFS then
 begin
  btn_FixADFS.Enabled:=True;
  menuFixADFS.Enabled:=True;
 end;
 //Enable the directory view
 DirList.Enabled:=True;
 //And select the first item
 if DirList.Items.Count>0 then
 begin
  DirList.Items[0].Selected:=True;
  DirList.SetFocus;
 end;
end;

{------------------------------------------------------------------------------}
//Populate a directory tree with an image
{------------------------------------------------------------------------------}
procedure TMainForm.AddImageToTree(Tree: TTreeView;ImageToUse: TDiscImage);
var
 //Used as a marker to make sure all directories are displayed.
 //Some double sided discs have each side as separate discs
 highdir    : Integer;
begin
 //Clear the tree view, prior to populating it
 Tree.Items.Clear;
 //Set the highdir to zero - which will be root to start with
 highdir:=0;
 //Then add the directories, if there is at least one
 if Length(ImageToUse.Disc)>0 then
 begin
  //Start by adding the root (could be more than one root, particularly on
  //double sided discs)
  repeat
   //This will initiate the recursion through the directory structure, per side
   AddDirectoryToTree(Tree.Items.Add(nil,ImageToUse.Disc[highdir].Directory),
                      highdir,ImageToUse,highdir);
   //Finished on this directory structure, so increase the highdir
   inc(highdir);
   //and continue until we have everything on the disc. This will, in effect,
   //add the second root for double sided discs.
  until highdir=Length(ImageToUse.Disc);
  TMyTreeNode(Tree.Items[0]).ParentDir:=-1;
  //Expand the top level of the tree (but not MMB)
  if ImageToUse.FormatNumber>>4<>diMMFS then Tree.TopItem.Expand(False);
  //And the root for the other side of the disc
  if ImageToUse.DoubleSided then
  begin
   //First, we need to find it
   repeat
    inc(highdir);
    //If there is one, of course - but it must be a directory
   until(highdir>=Tree.Items.Count) or (TMyTreeNode(Tree.Items[highdir-1]).IsDir);
   if highdir>Tree.Items.Count then
    highdir:=Tree.Items.Count;
   //Found? then expand it
   if TMyTreeNode(Tree.Items[highdir-1]).IsDir then
    Tree.Items[highdir-1].Expand(False);
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Update the Image information display
{------------------------------------------------------------------------------}
procedure TMainForm.UpdateImageInfo(partition: Cardinal=0);
var
 i: Integer;
 title: String;
begin
 //Only if there is a valid image
 if Image.FormatNumber<>diInvalidImg then
 begin
  //Image Format
  ImageDetails.Panels[1].Text:=Image.FormatString;
  //Disc name
  title:=Image.Title(partition);
  RemoveTopBit(title);//Ensure top bit not set
  ImageDetails.Panels[2].Text:=title;
  //Disc size
  ImageDetails.Panels[3].Text:=ConvertToKMG(Image.DiscSize(partition))
                           +' ('+IntToStrComma(Image.DiscSize(partition))+' Bytes)';
  //Free space
  ImageDetails.Panels[4].Text:=ConvertToKMG(Image.FreeSpace(partition))
                           +' ('+IntToStrComma(Image.FreeSpace(partition))+' Bytes)';
  //Double sided or not (DFS only)
  if Image.FormatNumber>>4=diAcornDFS then
   if Image.DoubleSided then
    ImageDetails.Panels[5].Text:='Double Sided'
   else
    ImageDetails.Panels[5].Text:='Single Sided'
  else
    ImageDetails.Panels[5].Text:='';
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
 //ADFS and SparkFS
 if(Image.FormatNumber>>4=diAcornADFS)
 or(Image.FormatNumber>>4=diSpark)then
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
 //Acorn FS
 if Image.FormatNumber>>4=diAcornFS then
 begin
  //Make it visible
  if cbpos>0 then AFSAttrPanel.Visible:=True;
  //Position it below the CRC32 section
  AFSAttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
  //Position the ticks box inside - Owner Access
  AFSOAAttributeLabel.Top:=0;
  AFSOAAttributeLabel.Left:=(AFSAttrPanel.Width-AFSOAAttributeLabel.Width)div 2;
  cb_AFS_ownw.Top:=AFSOAAttributeLabel.Top+AFSOAAttributeLabel.Height;
  cb_AFS_ownr.Top:=AFSOAAttributeLabel.Top+AFSOAAttributeLabel.Height;
  cb_AFS_ownl.Top:=AFSOAAttributeLabel.Top+AFSOAAttributeLabel.Height;
  cbpos:=AFSAttrPanel.Width div 4; //Equally space them
  cb_AFS_ownw.Left:=cbpos*0;
  cb_AFS_ownr.Left:=cbpos*1;
  cb_AFS_ownl.Left:=cbpos*2;
  //Position the ticks box inside - Public Access
  AFSPubAttributeLabel.Top:=cb_AFS_ownw.Top+cb_AFS_ownw.Height;
  AFSPubAttributeLabel.Left:=(AFSAttrPanel.Width-AFSPubAttributeLabel.Width)div 2;
  cb_AFS_pubw.Top:=AFSPubAttributeLabel.Top+AFSPubAttributeLabel.Height;
  cb_AFS_pubr.Top:=AFSPubAttributeLabel.Top+AFSPubAttributeLabel.Height;
  cbpos:=AFSAttrPanel.Width div 2; //Equally space them
  cb_AFS_pubw.Left:=cbpos*0;
  cb_AFS_pubr.Left:=cbpos*1;
  //And change the panel height to accomodate
  AFSAttrPanel.Height:=cb_AFS_pubw.Top+cb_AFS_pubw.Height;
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
 dr,
 ft       : Integer;
 filename,
 filetype,
 location,
 title,
 temp     : String;
 multiple : Char;
 R        : TRect;
 afspart  : Boolean;
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
 btn_download.Enabled     :=DirList.SelectionCount>0;
 ExtractFile1.Enabled     :=DirList.SelectionCount>0;
 menuExtractFile.Enabled  :=DirList.SelectionCount>0;
 DeleteFile1.Enabled      :=DirList.SelectionCount>0;
 btn_Delete.Enabled       :=DirList.SelectionCount>0;
 menuDeleteFile.Enabled   :=DirList.SelectionCount>0;
 HexDump1.Enabled         :=DirList.SelectionCount=1;
 menuHexDump.Enabled      :=DirList.SelectionCount=1;
 btn_HexDump.Enabled      :=DirList.SelectionCount=1;
 DuplicateFile1.Enabled   :=DirList.SelectionCount=1;
 menuDuplicateFile.Enabled:=DirList.SelectionCount=1;
 btn_DuplicateFile.Enabled:=DirList.SelectionCount=1;
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
 if(Node<>nil)and(DirList.SelectionCount=1)then
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
  //Enable the create directory button
  if(Image.FormatNumber>>4=diAcornADFS)
  OR(Image.FormatNumber>>4=diAmiga)
  or(Image.FormatNumber>>4=diAcornFS)then //ADFS, Amiga and Acorn FS
  begin
   NewDirectory1.Enabled   :=True;
   btn_NewDirectory.Enabled:=True;
   menuNewDir.Enabled      :=True;
  end;
  //Get the entry and dir references
  entry:=Node.Index;
  dir:=-1;
  dr:=TMyTreeNode(Node).DirRef;
  afspart:=False;//AFS Partition?
  if dr>=0 then afspart:=Image.Disc[dr].AFSPartition;
  //Clear the filename variable
  filename:='';
  //If the node does not have a parent, then the dir ref is the one contained
  //in the extra info. Otherwise is -1
  if Node.Parent<>nil then
   dir  :=TMyTreeNode(Node.Parent).DirRef;
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
   //ADFS and SparkFS
   if(Image.FormatNumber>>4=diAcornADFS)
   or(Image.FormatNumber>>4=diSpark)then
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
   //Acorn FS
   if Image.FormatNumber>>4=diAcornFS then
   begin
    //Tick/untick them
    cb_AFS_ownw.Checked:=Pos('W',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_AFS_ownr.Checked:=Pos('R',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_AFS_ownl.Checked:=Pos('L',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_AFS_pubw.Checked:=Pos('w',Image.Disc[dir].Entries[entry].Attributes)>0;
    cb_AFS_pubr.Checked:=Pos('r',Image.Disc[dir].Entries[entry].Attributes)>0;
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
   DeleteFile1.Enabled      :=False;
   btn_Delete.Enabled       :=False;
   menuDeleteFile.Enabled   :=False;
   RenameFile1.Enabled      :=False;
   btn_Rename.Enabled       :=False;
   menuRenameFile.Enabled   :=False;
   DuplicateFile1.Enabled   :=False;
   menuDuplicateFile.Enabled:=False;
   btn_DuplicateFile.Enabled:=False;
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
    filename:=Image.Disc[dr].Directory;
    filetype:='Root Directory';
    title:=Image.Disc[dr].Title;
    RemoveTopBit(title);
    lb_title.Caption:=title; //Title
    ed_title.Enabled:=True; //Can be edited
    //Report if directory is broken and include the error code
    if Image.Disc[dr].Broken then
     filetype:=filetype+' (BROKEN - 0x'
                       +IntToHex(Image.Disc[dr].ErrorCode,2)+')';
   end;
   //Can't edit a directory's filetype
   img_Filetype.Hint:='';
   lb_FileType.Hint :='';
  end;
  if(not TMyTreeNode(Node).IsDir)//Can only add files to a directory
  or((afspart)and(Image.FormatNumber>>4=diAcornADFS))then//And not to ADFS section of hybrids
  begin
   AddFile1.Enabled        :=False;
   btn_AddFiles.Enabled    :=False;
   menuAddFile.Enabled     :=False;
   NewDirectory1.Enabled   :=False;
   btn_NewDirectory.Enabled:=False;
   menuNewDir.Enabled      :=False;
   //Can edit a file's filetype
   img_Filetype.Hint:='Click to edit';
   lb_FileType.Hint :='Click to edit';
  end;
  //Filename
  RemoveTopBit(filename);
  if filename='' then filename:='unnamed';
  lb_FileName.Caption:=filename;
  //Filetype Image
  ft:=Node.ImageIndex;
  if (ft=directory) or (ft=directory_o) then
  begin
   if filename[1]='!' then //or application
    ft:=appicon
   else
    ft:=directory;
  end;
  //Paint the picture onto it
  R.Top:=0;
  R.Left:=0;
  R.Width:=img_FileType.Width;
  R.Height:=img_FileType.Height;
  //Draw the texture over it (probably don't need this)
  img_FileType.Canvas.Draw(0,0,GetTextureTile);
  FileImages.StretchDraw(img_FileType.Canvas,ft,R);
  img_FileType.Tag:=ft; //To keep track of which image it is
  //Filetype text - only show for certain systems
  if(Image.FormatNumber>>4=diAcornADFS) //ADFS
  or(Image.FormatNumber>>4=diCommodore) //C64
  or(Image.FormatNumber>>4=diAmiga)     //AmigaDOS
  or(Image.FormatNumber>>4=diSpark)     //Spark
  or(Image.FormatNumber>>4=diAcornFS)then //Acorn FS
   lb_FileType.Caption:=filetype;
  location:=''; //Default location string
  if dir>=0 then
  begin
   temp:=Image.GetParent(dir);
   //Status bar
   UpdateImageInfo(Image.Disc[dir].Entries[entry].Side);
   //CRC32
   lb_CRC32.Caption:=Image.GetFileCRC(temp+Image.DirSep+filename,entry);
   //Parent
   RemoveTopBit(temp);
   lb_parent.Caption:=temp;
   //Timestamp - ADFS, Spark and FileStore only
   if  (Image.Disc[dir].Entries[entry].TimeStamp>0)
   and((Image.FormatNumber>>4=diAcornADFS)
   or  (Image.FormatNumber>>4=diSpark)
   or  (Image.FormatNumber>>4=diAcornFS))then
    lb_timestamp.Caption:=FormatDateTime(TimeDateFormat,
                                       Image.Disc[dir].Entries[entry].TimeStamp);
   if(Image.Disc[dir].Entries[entry].TimeStamp=0)
   or(Image.FormatNumber>>4=diAcornFS)then
    if Image.Disc[dir].Entries[entry].DirRef=-1 then
    begin
     //Load address
     lb_loadaddr.Caption:='0x'+IntToHex(Image.Disc[dir].Entries[entry].LoadAddr,8);
     ed_loadaddr.Enabled:=True; //Allow editing
     //Execution address
     lb_execaddr.Caption:='0x'+IntToHex(Image.Disc[dir].Entries[entry].ExecAddr,8);
     ed_execaddr.Enabled:=True; //Allow editing
    end;
   //Length
   lb_length.Caption:=ConvertToKMG(Image.Disc[dir].Entries[entry].Length)+
                   ' (0x'+IntToHex(Image.Disc[dir].Entries[entry].Length,8)+')';
   //Location of object - varies between formats
   //ADFS Old map and Acorn FS - Sector is an offset
   if(Image.MapType=diADFSOldMap)
   or(Image.FormatNumber>>4=diAcornFS)then
    location:='Sector offset: 0x'
             +IntToHex(Image.Disc[dir].Entries[entry].Sector,8)+' ';
   //ADFS New map - Sector is an indirect address (fragment and sector)
   if Image.MapType=diADFSNewMap then
    location:='Indirect address: 0x'
             +IntToHex(Image.Disc[dir].Entries[entry].Sector,8)+' ';
   //Commodore formats - Sector and Track
   if Image.FormatNumber>>4=diCommodore then
    location:='Track ' +IntToStr(Image.Disc[dir].Entries[entry].Track)+' ';
   //All other formats - Sector
   if(Image.FormatNumber>>4=diAcornDFS)
   or(Image.FormatNumber>>4=diAmiga) then
    location:=location+'Sector '
             +IntToStr(Image.Disc[dir].Entries[entry].Sector)+' ';
   //DFS - indicates which side also
   if Image.FormatNumber>>4=diAcornDFS then
    location:=location+'Side '  +IntToStr(Image.Disc[dir].Entries[entry].Side);
   //CFS - indicates offset to starting block
   if Image.FormatNumber>>4=diAcornUEF then
    location:='Starting Block 0x'
             +IntToHex(Image.Disc[dir].Entries[entry].Sector,8);
  end;
  if dir=-1 then
  begin
   //Status bar
   UpdateImageInfo(Image.Disc[dr].Partition);
   //Location of root - varies between formats
   location:='';
   //ADFS Old map and Acorn File Server - Sector is an offset
   if(Image.MapType=diADFSOldMap)
   or(Image.FormatNumber>>4=diAcornFS)
   or(Image.Disc[dr].AFSPartition)then
    location:='Sector Offset: 0x'+IntToHex(Image.Disc[dr].Sector,8);
   //ADFS New map - Sector is an indirect address (fragment and sector)
   if Image.MapType=diADFSNewMap then
    location:='Indirect address: 0x'+IntToHex(Image.Disc[dr].Sector,8);
  end;
  //Update the location label
  lb_location.Caption:=location;
  //And arrange the file details pane
  ArrangeFileDetails;
 end;
end;

{------------------------------------------------------------------------------}
//Called when the TreeView is updated, and it wants to know which icon to use
{------------------------------------------------------------------------------}
procedure TMainForm.DirListGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
 GetImageIndex(Node,Image);
end;

{------------------------------------------------------------------------------}
//Called when the TreeView is updated, and it wants to know which icon to use
{------------------------------------------------------------------------------}
procedure TMainForm.GetImageIndex(Node: TTreeNode;ImageToUse: TDiscImage);
var
 ft,i,dir,entry: Integer;
 filetype      : String;
begin
 //The directory and entry references, as always
 dir  :=TMyTreeNode(Node).ParentDir;
 entry:=Node.Index;
 //Are we ADFS?
 if((ImageToUse.FormatNumber>>4=diAcornADFS)
  or(ImageToUse.FormatNumber>>4=diSpark)
  or(ImageToUse.FormatNumber>>4=diAcornFS))
 and(Length(ImageToUse.Disc)>0)then
 begin
  //Default is a file with load and exec address
  ft:=loadexec;
  //If it is not a directory
  if not TMyTreeNode(Node).IsDir then
  begin
   //And has a timestamp
   if ImageToUse.Disc[dir].Entries[entry].TimeStamp>0 then
   begin
    //Then we become an unknown filetype, until found that is
    ft:=unknown;
    //Start of search - the filetype constant array
    i:=Low(FileTypes)-1;
    //The filetype of the file
    filetype:=ImageToUse.Disc[dir].Entries[entry].ShortFiletype;
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
  if ImageToUse.FormatNumber>>4=diCommodore then
  begin
   //Default is a PRG file
   ft:=prgfile;
   //Directory has a different icon
   if not TMyTreeNode(Node).IsDir then
   begin
    //Get the appropriate filetype - no array for this as there is only 5 of them
    if ImageToUse.Disc[dir].Entries[entry].ShortFileType='SEQ' then ft:=seqfile;
    if ImageToUse.Disc[dir].Entries[entry].ShortFileType='USR' then ft:=usrfile;
    if ImageToUse.Disc[dir].Entries[entry].ShortFileType='REL' then ft:=relfile;
    if ImageToUse.Disc[dir].Entries[entry].ShortFileType='DEL' then ft:=delfile;
    if ImageToUse.Disc[dir].Entries[entry].ShortFileType='CBM' then ft:=cbmfile;
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
  if(ImageToUse.FormatNumber>>4=diAcornADFS)
  or(ImageToUse.FormatNumber>>4=diSpark)then //ADFS and Spark only
   if(ImageToUse.DirectoryType=diADFSNewDir)
   OR(ImageToUse.DirectoryType=diADFSBigDir)then //New or Big
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
  if ImageToUse.FormatNumber>>4=diMMFS then
  begin
   ft:=mmbdisc;
   if ImageToUse.Disc[Node.Index].Locked then ft:=mmbdisclock;
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
 Width:=866;
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
 //Create the dialogue boxes
 CreateFileTypeDialogue;
 //Create the copy and paste shortcuts
 CopyToClipboard.ShortCut   :=$4000 OR ord('C'); //Ctrl+C (WindowsLinux)
 PasteFromClipboard.ShortCut:=$4000 OR ord('V'); //Ctrl+V (Windows/Linux)
 {$IFDEF Darwin}
 CopyToClipboard.ShortCut   :=$1000 OR ord('C'); //Meta+C (Mac)
 PasteFromClipboard.ShortCut:=$1000 OR ord('V'); //Meta+V (Mac)
 {$ENDIF}
 //Meta/Cmd = $1000, Shift = $2000, Ctrl = $4000, Alt = $8000
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
 side         : Integer;
 harddrivesize: Cardinal;
 dirtype      : Byte;
 r,newmap     : Boolean;
 option,
 param,
 param2       : String;
 Dir          : TSearchRec;
 Files        : TSearchResults;
 F            : TFileStream;
 fields       : TStringArray;
 filedetails  : TDirEntry;
const DiscFormats = //Accepted format strings
 'DFSS    DFSS40  DFSD    DFSD40  WDFSS   WDFSS40 WDFSD   WDFSD40 ADFSS   ADFSM   '+
 'ADFSL   ADFSD   ADFSE   ADFSE+  ADFSF   ADFSF+  C1541   C1571   C1581   AMIGADD '+
 'AMIGAHD CFS';
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
  //Turn debugging on or off +++++++++++++++++++++++++++++++++++++++++++++++++++
  if (option='--debug') or (option='-db') then
  begin
   if UpperCase(param)='ON' then Fdebug:=True;
   if UpperCase(param)='OFF' then Fdebug:=False;
  end;
  //Open command +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (option='--insert') or (option='-i') then
   OpenImage(param);
  //New Image command ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (option='--new') or (option='-n') then
  begin
   if UpperCase(param)='ADFSHDD' then //Create ADFS HDD
   begin
    //Minimum length for second parameter is 3
    if Length(param2)>3 then
    begin
     newmap:=False; //Default
     if UpperCase(param2[1])='N' then newmap:=True;
     dirtype:=0; //Default
     if UpperCase(param2[2])='N' then dirtype:=1;//New dir
     if UpperCase(param2[2])='B' then dirtype:=2;//Big dir
     if(newmap)and(dirtype=0)then dirtype:=1; //Can't have old dir on new map
     if(not newmap)and(dirtype=2)then dirtype:=1;//Can't have big dir on old map
     if UpperCase(RightStr(param2,1))='M' then //in MB
      harddrivesize:=StrToIntDef(Copy(param2,3,Length(param2)-3),20)*1024*1024
     else                                                             //in bytes
      harddrivesize:=StrToIntDef(Copy(param2,3),20*1024*1024);
     if harddrivesize<20*1024*1024 then harddrivesize:=20*1024*1024;//20MB min
     if harddrivesize>1000*1024*1024 then harddrivesize:=1000*1024*1024;//1000MB max
     if(not newmap)and(harddrivesize>512*1024*1024)then
      harddrivesize:=512*1024*1024;//512MB max for old map
     //OK, now create it
     if Image.FormatHDD(diAcornADFS,harddrivesize,newmap,dirtype) then
     begin
      HasChanged:=True;
      ShowNewImage(Image.Filename);
     end;
    end;
   end;
   if LeftStr(UpperCase(param),4)='AFSL' then //Create AFS
    if StrToIntDef(param2,0)>0 then //Need a second parameter
    begin
     //Get the image size
     harddrivesize:=StrToIntDef(param2,0);
     //Has it been specified in Megabytes?
     if UpperCase(RightStr(param2,1))='M' then harddrivesize:=harddrivesize*1024;
     //Get the AFS level
     dirtype:=StrToIntDef(RightStr(param,1),0);
     //Is the specified image size big enough
     if(dirtype=2)and(harddrivesize<400)then harddrivesize:=400;
     if(dirtype=3)and(harddrivesize<640)then harddrivesize:=640;
     //But not too big
     if harddrivesize>512*1024 then harddrivesize:=512*1024;
     //Create it
     if Image.FormatHDD(diAcornFS,harddrivesize*1024,False,dirtype) then
     begin
      HasChanged:=True;
      ShowNewImage(Image.Filename);
     end;
    end;
   if Pos(UpperCase(param),DiscFormats)>0 then //Create other
   begin
    index:=(Pos(UpperCase(param),DiscFormats) DIV 8)+1;
    if(index>=Low(DiscNumber))and(index<=High(DiscNumber))then
     //Create new image
     if Image.FormatFDD(DiscNumber[index] DIV $100,
                       (DiscNumber[index] DIV $10)MOD $10,
                        DiscNumber[index] MOD $10) then
     begin
      HasChanged:=True;
      ShowNewImage(Image.Filename);
     end;
   end;
  end;
  //Commands that require at least one parameter and an image ------------------
  if Image.FormatNumber<>diInvalidImg then
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
    if param2='' then SaveImage.FileName:=PathDelim
    else SaveImage.FileName:=param2+PathDelim;
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
  if Image.FormatNumber<>diInvalidImg then
  begin
   if param='' then param:=Image.Filename;
   Image.SaveToFile(param,UpperCase(param2)='TRUE');
   Caption:=ApplicationTitle+' - '+ExtractFileName(Image.Filename);
   HasChanged:=False;
   //Update the status bar
   UpdateImageInfo;
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
 //Reset the form shift state
 FormShiftState:=[];
 //Texture style - get from the registry
 TextureType:=GetRegValI('Texture',1);
 //ADFS L Interleaved type - get from the registry
 ADFSInterleave:=GetRegValI('ADFS_L_Interleave',0);
 //Treat Spark as FS?
 SparkIsFS:=GetRegValB('Spark_Is_FS',True);
 //Threshold of when to bypass the GUI (during import) - get from registry
 bypassGUIThres:={GetRegValI('bypass_GUI_Threshold',}100;//);
 //Create INF Files?
 DoCreateINF:=GetRegValB('CreateINF',True);
 //Hide Commodore DEL files
 DoHideDEL:=GetRegValB('Hide_CDR_DEL',False);
 //Allow DFS images with zero sectors
 FDFSZeroSecs:=GetRegValB('DFS_Zero_Sectors',True);
 //Produce log files for debugging
 Fdebug:=GetRegValB('Debug_Mode',False);
 debuglogfile:=GetTempDir+'DIM_LogFile.txt';
 //Write some debugging info
 WriteToDebug('Application Started.');
 WriteToDebug('Version '+ApplicationVersion+' '+platform+' ('+arch+')');
 WriteToDebug('Screen DPI: '+IntToStr(Screen.PixelsPerInch));
end;

{------------------------------------------------------------------------------}
//Select a node
{------------------------------------------------------------------------------}
procedure TMainForm.SelectNode(filename: String;casesens:Boolean=True);
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
  if(dirname=filename)and(casesens)then found:=i;
  if(LowerCase(dirname)=LowerCase(filename))and(not casesens)then found:=i;
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
 filename,
 pathname : String;
 index    : Integer;
begin
 //Remove the existing part of the original filename
 filename:=ExtractFileName(Image.Filename);
 pathname:=ExtractFilePath(Image.Filename);
 ext:=ExtractFileExt(filename);
 filename:=LeftStr(filename,Length(filename)-Length(ext));
 SaveImage.Title:='Save Image As';
 //Populate the filter part of the dialogue
 index:=0;
 SaveImage.Filter:=Image.SaveFilter(index);
 if index=0 then index:=1;
 SaveImage.FilterIndex:=index;
 //Populate the filename part of the dialogue
 SaveImage.FileName:=filename+'.'+Image.FormatExt;
 SaveImage.DefaultExt:='.'+Image.FormatExt;
 //Point it at the same directory from whence it was opened
 SaveImage.InitialDir:=pathname;
 //Show the dialogue
 If SaveImage.Execute then
 begin
  //Save the image
  Image.SaveToFile(SaveImage.FileName);
  Caption:=ApplicationTitle+' - '+ExtractFileName(Image.Filename);
  HasChanged:=False;
  //Update the status bar
  UpdateImageInfo;
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
begin
 //Create a new DiscImage instance
 NewImage:=TDiscImage.Create;
 NewImage.InterleaveMethod:=ADFSInterleave;
 NewImage.SparkAsFS:=SparkIsFS;
 NewImage.AllowDFSZeroSectors:=FDFSZeroSecs;
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
   if NewImage.FormatNumber<>diInvalidImg then //Is an image
   begin
    open:=open OR $01;
    //Get the detected format string
    fmt:=NewImage.FormatString;
   end;
   //Is there something loaded?
   if Image.Filename<>'' then open:=open OR $02; //Something is open
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
    if (Image.DirectoryType>0)
    and(Image.FormatNumber>>4=diAcornADFS)
    and(not SparkIsFS)then //And Spark is not being treated as a filing system
    begin
     //See if it is a spark archive
     SparkFile:=TSpark.Create(FileName);
     sparksize:=SparkFile.UncompressedSize;
     isspark:=SparkFile.IsSpark;
     SparkFile.Free;
     //Is it a valid Spark Archive?
     if(isspark)and(sparksize<Image.FreeSpace(0))then
     begin //Ask user if they would like it uncompressed
      if AskConfirm('This has been detected as a Spark archive. '
                   +'Would you like to uncompress and add the contents?',
                    'Yes',
                    'No, just add',
                    '')=mrOK then
      begin
       open:=$04; //Yes, so reset the open flag so it doesn't add it
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
   if open=$00 then //Can't load
    ReportError('"'+ExtractFilename(FileName)
                   +'" has not been recognised as a valid disc image that '
                   +ApplicationTitle+' can open.');
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
 index,
 MaxDirEnt,
 NumFiles  : Integer;
 buffer    : TDIByteArray;
 ok        : Boolean;
begin
 //Show a progress message
 ProgressForm.Show;
 //Process the messages to close the file dialogue box
 Application.ProcessMessages;
 //Import the contents
 NewImage.ReadImage;//First, read in the catalogue
 ProgressForm.Hide;
 Application.ProcessMessages;
 //Show the contents in the import selector box
 ImportSelectorForm.Show; //The TTreeView needs to be visible
 //Copy the DiscImage across
 ImportSelectorForm.FImage:=NewImage;;
 //Add the disc image to the tree
 AddImageToTree(ImportSelectorForm.ImportDirList,NewImage);
 //Tick them all (just by ticking the first one)
 ImportSelectorForm.TickNode(ImportSelectorForm.ImportDirList.Items[0],True);
 //Now we need to hide the form so we can show it again, but modally.
 ImportSelectorForm.Hide;
 //And, finally, modally
 if ImportSelectorForm.ShowModal=mrOK then //And start the import
 begin
  //Work out the Maximum Directory Entries, now the user has chosen which files
  MaxDirEnt:=0;
  //We also need the total number of files selected
  NumFiles:=0;
  if Length(NewImage.Disc)>0 then
   for dir:=0 to Length(NewImage.Disc)-1 do
   begin
    index:=0; //Use this as a per-directory counter
    if Length(NewImage.Disc[dir].Entries)>0 then
     for entry:=0 to Length(NewImage.Disc[dir].Entries)-1 do
      if ImportSelectorForm.IsNodeTicked(dir,entry) then
      begin
       inc(index);
       {if NewImage.Disc[dir].Entries[entry].DirRef<>-1 then }inc(NumFiles);
      end;
    //Update the Maximum Directory Entries, per directory
    if index>MaxDirEnt then MaxDirEnt:=index;
   end;
  //Check the open image is suitable for the incoming selected files
  ok:=True;
  //If it is not, ask the user if they wish to continue
  if  ((Image.FormatNumber>>4=diAcornADFS)                     //Check ADFS
  and(((MaxDirEnt>47)and(Image.DirectoryType=diADFSOldDir))
    or((MaxDirEnt>77)and(Image.DirectoryType=diADFSNewDir))))
  or  ((Image.FormatNumber>>4=diAcornDFS)and(NumFiles>31))     //Check DFS
  or  ((Image.FormatNumber>>4=diCommodore)and(NumFiles>144))   //Check Commodore
  then ok:=AskConfirm(
           'The current open image is not suitable for this archive. '
          +'Would you like to continue regardless?',
          'Yes','No','')=mrOK;
  //If all OK, then continue
  if ok then
  begin
   //We don't need progress update from the class as we'll produce our own
   NewImage.ProgressIndicator:=nil;
   //Show the progress form again
   ProgressForm.Show;
   Application.ProcessMessages;
   //Turn error reporting off
   ErrorReporting:=False;
   //Clear the log
   ErrorLogForm.ErrorLog.Clear;
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
   curformat:=Image.FormatNumber>>4;   //Format of the current open image
   newformat:=NewImage.FormatNumber>>4;//Format of the importing image
   //Go through each directory
   if Length(NewImage.Disc)>0 then
    for dir:=0 to Length(NewImage.Disc)-1 do
     if Length(NewImage.Disc[dir].Entries)>0 then
      for entry:=0 to Length(NewImage.Disc[dir].Entries)-1 do
       if ImportSelectorForm.IsNodeTicked(dir,entry) then //Only those that are selected
       begin
        newentry:=NewImage.Disc[dir].Entries[entry];
        //Set the parent, as this may be different
        newentry.Parent:=NewImage.GetParent(dir);
        UpdateProgress('Importing '+newentry.Parent+NewImage.DirSep+
                                    newentry.Filename);
        //Validate the filename, as it could be different across file systems
        if newformat<>diAcornDFS then WinToBBC(newentry.Filename); //Not DFS
        //DFS and C64 don't have directories, so the parent is the selected node
        if(newformat=diCommodore)or(newformat=diAcornDFS)then
         newentry.Parent:=rootname;
        //If going to DFS or C64
        if(curformat=diAcornDFS)or(curformat=diCommodore)then
        begin
         //If coming from ADFS, AFS or Amiga, and is inside a directory,
         //add the first letter of this.
         if((newformat=diAcornADFS)or(newformat=diAmiga)or(newformat=diAcornFS))
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
        if (newformat=diAcornDFS)and(newentry.Filename[2]='.')
        and((curformat=diAcornADFS)or(curformat=diAmiga)) then
        begin
         //Then create the directory
         SelectNode(rootname);
         //This will fail if already created, but we've suppressed errors
         CreateDirectory(newentry.Filename[1],'DLR');
         newentry.Parent:=rootname+Image.DirSep+newentry.Filename[1];
         newentry.Filename:=Copy(newentry.Filename,3,Length(newentry.Filename));
        end;
        //Going to ADFS, from another system, ensure it has 'WR' attributes
        if(curformat=diAcornADFS)and(curformat<>newformat)then
        begin
         if Pos('W',newentry.Attributes)=0 then
          newentry.Attributes:=newentry.Attributes+'W';
         if Pos('R',newentry.Attributes)=0 then
          newentry.Attributes:=newentry.Attributes+'R';
        end;
        //Select the parent directory
        SelectNode(newentry.Parent);
        if DirList.SelectionCount=0 then
         ReportError('Cannot find directory "'+newentry.Parent
                     +'" when adding "'+newentry.Filename+'"')
        else
        begin
         //Is it a directory we're adding? ADFS and Amiga only
         if(newentry.DirRef>=0)and((curformat=diAcornADFS)or(curformat=diAmiga))then
          if newentry.Filename<>'$' then //Create the directory
           CreateDirectory(newentry.Filename,'DLR');
         //Is it a file
         if newentry.DirRef=-1 then
          //Read the file in
          if NewImage.ExtractFile(NewImage.GetParent(dir)
                                 +NewImage.DirSep
                                 +NewImage.Disc[dir].Entries[entry].Filename,
                                  buffer,entry) then
          begin
           //Write it out to the current image
           index:=Image.WriteFile(newentry,buffer);
           //Then add it to the tree, if successful
           if index>=0 then
            AddFileToTree(DirList.Selected,newentry.Filename,index,False,
                          DirList{,Image});
          end;
        end;
       end;
   UpdateImageInfo;
   //Turn error reporting on
   ErrorReporting:=True;
   //Show the log
   ShowErrorLog;
   //Hide the progress message
   ProgressForm.Hide;
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Edit the filetypes
{------------------------------------------------------------------------------}
procedure TMainForm.sb_FileTypeClick(Sender: TObject);
var
 ft,i,
 dir,
 entry: Integer;
begin
 //ADFS non directories only
 if (Image.FormatNumber>>4=diAcornADFS)
 and(not TMyTreeNode(DirList.Selected).IsDir)then
 begin
  //Get the references
  entry:=DirList.Selected.Index;
  dir:=TMyTreeNode(DirList.Selected).ParentDir;
  //And ensure they are valid
  if dir<Length(Image.Disc) then
   if entry<Length(Image.Disc[dir].Entries)then
   begin
    //Get the current filetype
    ft:=StrToIntDef('$'+Image.Disc[dir].Entries[entry].ShortFileType,0);
    //Reset all the buttons to up
    FTDummyBtn.Down:=True;
    //Now set the one for our filetype, if it is there, to down
    for i:=0 to Length(FTButtons)-1 do
     if FTButtons[i].Tag=ft then FTButtons[i].Down:=True;
    //Set the custom filetype to this filetype
    FTEdit.Text:=IntToHex(ft,3);
    //Show the dialogue modally
    FTDialogue.ShowModal;
   end;
 end;
end;

{------------------------------------------------------------------------------}
//User has selected a filetype
{------------------------------------------------------------------------------}
procedure TMainForm.FileTypeClick(Sender: TObject);
var
 ft,
 dir,
 entry: Integer;
begin
 //Default filetype
 ft:=-1;
 //Get the selected filetype
 if Sender is TSpeedButton then ft:=TSpeedButton(Sender).Tag;
 //Hide the dialogue by setting it's ModalResult (doesn't matter what to)
 FTDialogue.ModalResult:=mrOK;
 //If a filetype was selected, then update it
 if ft<>-1 then
 begin
  //Get the references
  entry:=DirList.Selected.Index;
  dir:=TMyTreeNode(DirList.Selected).ParentDir;
  //And ensure they are valid
  if dir<Length(Image.Disc) then
   if entry<Length(Image.Disc[dir].Entries)then
    //Change the filetype
    if Image.ChangeFileType(Image.GetParent(dir)
                           +Image.DirSep
                           +Image.Disc[dir].Entries[entry].Filename
                           ,IntToHex(ft,3))then
    begin
     //If all went OK, update the display
     DirListChange(Sender,DirList.Selected);
     //And mark as changed
     HasChanged:=True;
    end;
 end;
end;

{------------------------------------------------------------------------------}
//Swap the label for an edit - user clicked on the label
{------------------------------------------------------------------------------}
procedure TMainForm.SwapLabelEdit(editcont:TEdit;labelcont:TLabel;dir,hex:Boolean);
var
 edittext: String;
begin
 //If there is only one selection
 if DirList.SelectionCount=1 then
  //And it is a directory
  if TMyTreeNode(DirList.Selected).IsDir=dir then
   //And the editor is enabled
   if editcont.Enabled then
   begin //Then show it
    editcont.Visible  :=True;
    edittext:=labelcont.Caption;
    //For hex number, remove the intial '0x'
    if(hex)and(LeftStr(edittext,2)='0x')then edittext:=Copy(edittext,3);
    editcont.Text     :=edittext;
    //Put the cursor in the control
    editcont.SetFocus;
    //And hide the original label
    labelcont.Visible :=False;
   end;
end;

{------------------------------------------------------------------------------}
//User has clicked on directory title
{------------------------------------------------------------------------------}
procedure TMainForm.lb_titleClick(Sender: TObject);
begin
 SwapLabelEdit(ed_title,lb_title,True,False);
end;

{------------------------------------------------------------------------------}
//User has clicked on execution address
{------------------------------------------------------------------------------}
procedure TMainForm.lb_execaddrClick(Sender: TObject);
begin
 SwapLabelEdit(ed_execaddr,lb_execaddr,False,True);
end;

{------------------------------------------------------------------------------}
//User has clicked on load address
{------------------------------------------------------------------------------}
procedure TMainForm.lb_loadaddrClick(Sender: TObject);
begin
 SwapLabelEdit(ed_loadaddr,lb_loadaddr,False,True);
end;

{------------------------------------------------------------------------------}
//User has clicked on the timestamp label
{------------------------------------------------------------------------------}
procedure TMainForm.lb_timestampClick(Sender: TObject);
var
 dir,
 entry: Integer;
begin
 //ADFS and AFS only
 if(Image.FormatNumber>>4=diAcornADFS)
 or(Image.FormatNumber>>4=diAcornFS)then
 begin
  //Get the references
  entry:=DirList.Selected.Index;
  dir:=TMyTreeNode(DirList.Selected).ParentDir;
  //And ensure they are valid
  if dir<Length(Image.Disc) then
   if entry<Length(Image.Disc[dir].Entries)then
   begin
    //Ensure that the control has the same time/date as the label
    ed_timestamp.DateTime:=Image.Disc[dir].Entries[entry].TimeStamp;
    //Re-position the control (as this does not span the panel)
    ed_timestamp.Top:=lb_timestamp.Top-4;
    ed_timestamp.Left:=Round((lb_timestamp.Width-ed_timestamp.Width)/2);
    //Swap the control and label round
    lb_timestamp.Visible:=False;
    ed_timestamp.Visible:=True;
   end;
 end;
end;

{------------------------------------------------------------------------------}
//User has finished editing the time/date
{------------------------------------------------------------------------------}
procedure TMainForm.ed_timestampEditingDone(Sender: TObject);
var
 newtimedate: TDateTime;
 dir,
 entry      : Integer;
begin
 ed_timestamp.Visible:=False;
 lb_timestamp.Visible:=True;
 //Get the adjusted time
 newtimedate:=ed_timestamp.DateTime;
 //Get the references
 entry:=DirList.Selected.Index;
 dir:=TMyTreeNode(DirList.Selected).ParentDir;
 //And ensure they are valid
 if dir<Length(Image.Disc) then
  if entry<Length(Image.Disc[dir].Entries)then
   if newtimedate<>Image.Disc[dir].Entries[entry].TimeStamp then //And different
    if Image.TimeStampFile(Image.GetParent(dir)
                          +Image.DirSep
                          +Image.Disc[dir].Entries[entry].Filename
                          ,newtimedate) then //So send to the class
    begin
     //Display the new details
     lb_timestamp.Caption:=FormatDateTime(TimeDateFormat,
                                       Image.Disc[dir].Entries[entry].TimeStamp);
     HasChanged:=True;
    end;
end;

{------------------------------------------------------------------------------}
//Application is closing
{------------------------------------------------------------------------------}
procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 WriteToDebug('Application Closed');
end;

{------------------------------------------------------------------------------}
//Make a note of the shift state of the form
{------------------------------------------------------------------------------}
procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
 Shift: TShiftState);
begin
 FormShiftState:=Shift;
end;

{------------------------------------------------------------------------------}
//The shift state has been released.
{------------------------------------------------------------------------------}
procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
 );
begin
 FormShiftState:=[];
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
//User has done editing Execution or Load Address
{------------------------------------------------------------------------------}
procedure TMainForm.ed_execaddrEditingDone(Sender: TObject);
var
 filename,
 newaddr,
 oldaddr  : String;
 dir,
 entry    : Integer;
 ok       : Boolean;
begin
 //Get the filename of the file being edited
 filename:=lb_Filename.Caption;
 //Add the path, if there is one
 if lb_Parent.Caption<>'' then
  filename:=lb_Parent.Caption+Image.DirSep+filename;
 //Get the old address
 oldaddr:='';
 newaddr:='';
 if Sender is TEdit then
 begin
  //Address to change to
  newaddr:='0x'+TEdit(Sender).Text;
  oldaddr:=newaddr;
  //Address to change from
  if TEdit(Sender)=ed_execaddr then oldaddr:=lb_execaddr.Caption;
  if TEdit(Sender)=ed_loadaddr then oldaddr:=lb_loadaddr.Caption;
  //Get the references
  entry:=DirList.Selected.Index;
  dir:=TMyTreeNode(DirList.Selected).ParentDir;
  if dir<Length(Image.Disc) then
   if entry<Length(Image.Disc[dir].Entries) then
   begin
    //Then send to be changed
    if newaddr<>oldaddr then //Only if something has changed
    begin
     ok:=False;
     //Send to the class
     if TEdit(Sender)=ed_execaddr then
      ok:=Image.UpdateExecAddr(filename,StrToIntDef('$'+Copy(newaddr,3),0));
     if TEdit(Sender)=ed_loadaddr then
      ok:=Image.UpdateLoadAddr(filename,StrToIntDef('$'+Copy(newaddr,3),0));
     if ok then
     begin
      //If success, then change the text
      lb_execaddr.Caption:='0x'+IntToHex(Image.Disc[dir].Entries[entry].ExecAddr,8);
      lb_loadaddr.Caption:='0x'+IntToHex(Image.Disc[dir].Entries[entry].LoadAddr,8);
      HasChanged:=True;
     end;
    end;
   end;
 end;
 //Hide the edit control and show the label
 lb_execaddr.Visible:=True;
 ed_execaddr.Visible:=False;
 lb_loadaddr.Visible:=True;
 ed_loadaddr.Visible:=False;
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
 skip      : Byte;
 img       : TLazIntfImage;
 function ConvertColour(col: TColor):TFPColor;
 begin
  //Convert from TColor to TFPColor
  Result.Alpha:=$FF;
  Result.Blue :=(col>>16)AND$FF;
  Result.Green:=(col>> 8)AND$FF;
  Result.Red  := col     AND$FF;
  //TFPColor is 16 bit per Result
  Result.Alpha:=Result.Alpha or Result.Alpha<<8;
  Result.Red  :=Result.Red or Result.Red<<8;
  Result.Green:=Result.Green or Result.Green<<8;
  Result.Blue :=Result.Blue or Result.Blue<<8;
 end;
begin
 Cursor:=crHourglass;
 //Add the editable controls to arrays - makes it easier later on
 titles[0] :=ImageDetailForm.edDiscTitle0;
 titles[1] :=ImageDetailForm.edDiscTitle1;
 boots[0]  :=ImageDetailForm.cbBootOption0;
 boots[1]  :=ImageDetailForm.cbBootOption1;
 bootlbs[0]:=ImageDetailForm.lbBootOption0;
 bootlbs[1]:=ImageDetailForm.lbBootOption1;
 //CRC32
 ImageDetailForm.lbCRC32.Caption    :=Image.CRC32;
 //Format
 ImageDetailForm.lbImgFormat.Caption:=Image.FormatString;
 //Map type
 ImageDetailForm.lbMap.Caption      :=Image.MapTypeString;
 ImageDetailForm.MapPanel.Visible   :=ImageDetailForm.lbMap.Caption<>'';
 //Directory type
 ImageDetailForm.lbDirType.Caption  :=Image.DirectoryTypeString;
 ImageDetailForm.DirPanel.Visible   :=ImageDetailForm.lbDirType.Caption<>'';
 //Interleave type
 ImageDetailForm.lbInterleave.Caption:=Image.InterleaveInUse;
 ImageDetailForm.InterleavePanel.Visible:=ImageDetailForm.lbInterleave.Caption<>'';
 //Arrange the panels
 with ImageDetailForm do
 begin
  pnSide0.Top        :=0;
  pnSide1.Top        :=pnSide0.Top+pnSide0.Height;
  FormatPanel.Top    :=pnSide1.Top+pnSide1.Height;
  MapPanel.Top       :=FormatPanel.Top+FormatPanel.Height;
  DirPanel.Top       :=MapPanel.Top+MapPanel.Height;
  InterleavePanel.Top:=DirPanel.Top+DirPanel.Height;
  CRCPanel.Top       :=InterleavePanel.Top+InterleavePanel.Height;
 end;
 //Logo
 t:=ImageDetailForm.DirPanel.Top+ImageDetailForm.DirPanel.Height;
 s:=ImageDetailForm.OKBtnBack.Top-t;
 //Centralise vertical
 ImageDetailForm.AcornLogo.Top    :=t+((s-ImageDetailForm.AcornLogo.Height)div 2);
 ImageDetailForm.CommodoreLogo.Top:=t+((s-ImageDetailForm.AcornLogo.Height)div 2);
 ImageDetailForm.AmigaLogo.Top    :=t+((s-ImageDetailForm.AcornLogo.Height)div 2);
 ImageDetailForm.SinclairLogo.Top :=t+((s-ImageDetailForm.AcornLogo.Height)div 2);
 //Centralise horizontal
 s:=ImageDetailForm.Legend.Width;
 ImageDetailForm.AcornLogo.Left    :=(s-ImageDetailForm.AcornLogo.Width)div 2;
 ImageDetailForm.CommodoreLogo.Left:=(s-ImageDetailForm.AcornLogo.Width)div 2;
 ImageDetailForm.AmigaLogo.Left    :=(s-ImageDetailForm.AcornLogo.Width)div 2;
 ImageDetailForm.SinclairLogo.Left :=(s-ImageDetailForm.AcornLogo.Width)div 2;
 //Hide them all
 ImageDetailForm.AcornLogo.Visible    :=False;
 ImageDetailForm.CommodoreLogo.Visible:=False;
 ImageDetailForm.AmigaLogo.Visible    :=False;
 ImageDetailForm.SinclairLogo.Visible :=False;
 //Now display the appropriate one
 case Image.FormatNumber>>4 of
  diAcornDFS,
  diAcornADFS,
  diAcornUEF,
  diAcornFS   : ImageDetailForm.AcornLogo.Visible    :=True;
  diCommodore : ImageDetailForm.CommodoreLogo.Visible:=True;
  diAmiga     : ImageDetailForm.AmigaLogo.Visible    :=True;
  diSinclair  : ImageDetailForm.SinclairLogo.Visible :=True;
 end;
 //Should we label the top box?
 if Image.DoubleSided then
  ImageDetailForm.pnSide0.Caption:='Side 0'
 else
  ImageDetailForm.pnSide0.Caption:='';
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
   //Work out what tracks to skip (images over 100000 pixels high will crash)
   skip:=(Length(Image.FreeSpaceMap[0])div 100000)+1;
   //Set the graphic size
   t:=Length(Image.FreeSpaceMap[side]);
   s:=Length(Image.FreeSpaceMap[side,0]);
   FSM[side].Height:=t div skip;
   FSM[side].Width:=s;
   //Initiate the canvas 
   img:=TLazIntfImage.Create(0,0,[riqfRGB, riqfAlpha]);
   img.SetSize(FSM[side].Width,FSM[side].Height);
   //Colour in the free space
   img.FillPixels(ConvertColour(ImageDetailForm.colFree.Brush.Color));
   //Now draw all the sectors in tracks
   for t:=0 to Length(Image.FreeSpaceMap[side])-1 do
    if t mod skip=0 then
    for s:=0 to Length(Image.FreeSpaceMap[side,t])-1 do
     if Image.FreeSpaceMap[side,t,s]>$FC then
     begin
      //Other colours
      if Image.FreeSpaceMap[side,t,s]=$FF then
       col:=ImageDetailForm.colFile.Brush.Color;      //Unknown/Files
      if Image.FreeSpaceMap[side,t,s]=$FE then
       col:=ImageDetailForm.colSystem.Brush.Color;    //System
      if Image.FreeSpaceMap[side,t,s]=$FD then
       col:=ImageDetailForm.colDir.Brush.Color;       //Directories
      //Set the colour
      img.Colors[s,t div skip]:=ConvertColour(col);
     end;
   //Copy the graphic to the display
   FSM[side].Picture.PNG.PixelFormat:=pf32bit;
   FSM[side].Picture.PNG.LoadFromIntfImage(img);
   //Stretch the image
   FSM[side].Stretch:=True;
   //And resize to fit the window
   FSM[side].Height:=ImageDetailForm.ClientHeight-24;
   FSM[side].Width:=200;
   //Free the graphic container
   img.Free;
   //Set the label size
   FSMlabel[side].Height:=20;
   FSMlabel[side].Width:=200;
   FSMlabel[side].Alignment:=taCenter;
   FSMlabel[side].AutoSize:=False;
   //And fill in the details
   FSMlabel[side].Font.Style:=[fsBold];
   FSMlabel[side].Caption:='Free Space Map';
   if Image.DoubleSided then
    FSMlabel[side].Caption:=FSMlabel[side].Caption+' Side '+IntToStr(side*2);
   //Disc Title
   title:=Image.Title(side);
   //Remove top bit - this can cause havoc with Mac OS
   RemoveTopBit(title);
   //Set the edit box
   titles[side].Text:=title;
   titles[0].Enabled:=not Image.AFSPresent;
   //Limit the length
   if Image.FormatNumber>>4=diAcornDFS  then titles[side].MaxLength:=12; //DFS
   if Image.FormatNumber>>4=diAcornADFS then titles[side].MaxLength:=10; //ADFS
   if Image.FormatNumber>>4=diAcornFS   then titles[side].MaxLength:=10; //AFS
   //Boot Option
   boots[side].Visible  :=True;
   if Length(Image.BootOpt)>side then
    boots[side].ItemIndex:=Image.BootOpt[side]
   else
    boots[side].Visible:=False;
   bootlbs[side].Visible:=boots[side].Visible;
  end;
  //Is this an ADFS/AFS Hybrid?
  if(Image.FormatNumber>>4=diAcornADFS)and(Image.AFSPresent)then
  begin
   FSMlabel[0].Caption:=FSMlabel[0].Caption+' ADFS Partition';
   FSMlabel[1].Caption:=FSMlabel[1].Caption+' Acorn FS Partition';
  end;
  //Change the dialogue box width
  ImageDetailForm.ClientWidth:=(Length(Image.FreeSpaceMap)*204)
                              +4+ImageDetailForm.Legend.Width;
  //Show/Hide the second detail panel
  ImageDetailForm.pnSide1.Visible:=numsides>1;
  //Show the window, modally
  Cursor:=crDefault;
  if ImageDetailForm.ShowModal=mrOK then
  begin
   for side:=0 to numsides-1 do
   begin
    //Update Disc Title
    if Image.UpdateDiscTitle(titles[side].Text,side)      then HasChanged:=True;
    //Update Boot Option
    if Image.UpdateBootOption(boots[side].ItemIndex,side) then HasChanged:=True;
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
 //Refresh the display (as some directories might not have got read first time round)
 ShowNewImage(Image.Filename);
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
 //Populate the save dialogue box
 SaveImage.DefaultExt:='.csv';
 SaveImage.Filter:='CSV File|*.csv';
 SaveImage.FilterIndex:=1;
 SaveImage.Title:='Save CSV of image contents';
 //Add the csv extension
 SaveImage.Filename:=filename+'.csv';
 //Show the dialogue box
 if SaveImage.Execute then
 begin
  hexlen:=8;
  if Image.FormatNumber>>4=diAcornDFS then hexlen:=6;
  //Show a progress message
  ProgressForm.Show;
  //Process the messages to close the file dialogue box
  Application.ProcessMessages;
  //Open a new file
  F:=TFileStream.Create(SaveImage.FileName,fmCreate OR fmShareDenyNone);
  //Write the image details
  WriteLine(F,'"'+Image.Filename+'","'+Image.CRC32+'"');
  //Write the headers
  WriteLine(F,'"Parent","Filename","Load Address","Execution Address","Length","Attributes","CRC32"');
  //Go through each directory
  for dir:=0 to Length(Image.Disc)-1 do
   //And each entry in that directory
   for entry:=0 to Length(Image.Disc[dir].Entries)-1 do
    //write out each entry
    WriteLine(F,'"'+Image.GetParent(dir)+'","'
                   +Image.Disc[dir].Entries[entry].Filename+'","'
                   +IntToHex(Image.Disc[dir].Entries[entry].LoadAddr,hexlen)+'","'
                   +IntToHex(Image.Disc[dir].Entries[entry].ExecAddr,hexlen)+'","'
                   +IntToHex(Image.Disc[dir].Entries[entry].Length,hexlen)+'","'
                   +Image.Disc[dir].Entries[entry].Attributes+'","'
                   +Image.GetFileCRC(Image.GetParent(dir)
                                    +Image.DirSep
                                    +Image.Disc[dir].Entries[entry].Filename)+'"');
  //Finally free up the file stream
  F.Free;
  //Close the progress window
  ProgressForm.Hide;
 end;
end;

{------------------------------------------------------------------------------}
//Open the preferences window
{------------------------------------------------------------------------------}
procedure TMainForm.btn_SettingsClick(Sender: TObject);
begin
 //Set the preferences - Texture
 SettingsForm.NoTile.Checked    :=False;
 SettingsForm.TileRO5.Checked   :=False;
 SettingsForm.TileRO4.Checked   :=False;
 SettingsForm.TileRO3.Checked   :=False;
 SettingsForm.TileIyonix.Checked:=False;
 SettingsForm.TileROPi.Checked  :=False;
 case TextureType of
  0: SettingsForm.NoTile.Checked    :=True;
  1: SettingsForm.TileRO5.Checked   :=True;
  2: SettingsForm.TileRO4.Checked   :=True;
  3: SettingsForm.TileRO3.Checked   :=True;
  4: SettingsForm.TileIyonix.Checked:=True;
  5: SettingsForm.TileROPi.Checked  :=True;
 end;
 //ADFS Interleaving
 SettingsForm.InterleaveGroup.ItemIndex:=ADFSInterleave;
 //Miscellaneous
 SettingsForm.CreateINF.Checked:=DoCreateInf;
 SettingsForm.WriteDebug.Checked:=Fdebug;
 SettingsForm.AllowDFSZeroSecs.Checked:=FDFSZeroSecs;
 //Show the form, modally
 SettingsForm.ShowModal;
 if SettingsForm.ModalResult=mrOK then
 begin
  //Get the preferences
  if SettingsForm.NoTile.Checked     then TextureType:=0;
  if SettingsForm.TileRO5.Checked    then TextureType:=1;
  if SettingsForm.TileRO4.Checked    then TextureType:=2;
  if SettingsForm.TileRO3.Checked    then TextureType:=3;
  if SettingsForm.TileIyonix.Checked then TextureType:=4;
  if SettingsForm.TileROPi.Checked   then TextureType:=5;
  ADFSInterleave:=SettingsForm.InterleaveGroup.ItemIndex;
  DoCreateInf   :=SettingsForm.CreateINF.Checked;
  Fdebug        :=SettingsForm.WriteDebug.Checked;
  FDFSZeroSecs  :=SettingsForm.AllowDFSZeroSecs.Checked;
  //Save the settings
  SetRegValI('Texture',TextureType);
  SetRegValI('ADFS_L_Interleave',ADFSInterleave);
  SetRegValB('CreateINF',DoCreateINF);
  SetRegValB('Debug_Mode',Fdebug);
  SetRegValB('DFS_Zero_Sectors',FDFSZeroSecs);
  //Change the tile under the filetype
  if DirList.SelectionCount=1 then DirListChange(Sender,DirList.Selected);
  //Repaint the main form
  Repaint;
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
//The context menu has been requested, so cancel any drag drop operations
{------------------------------------------------------------------------------}
procedure TMainForm.DirListContextPopup(Sender: TObject; MousePos: TPoint;
 var Handled: Boolean);
begin
 //This actually pops up, on a Mac, when CTRL is pressed to add to the selection
 if ssCtrl in FormShiftState then Handled:=True; //Stops the context menu popping up
end;

{------------------------------------------------------------------------------}
//Duplicate File
{------------------------------------------------------------------------------}
procedure TMainForm.DuplicateFile1Click(Sender: TObject);
begin
 if DirList.SelectionCount=1 then
 begin
  //Get the selected file
  DraggedItem:=DirList.Selected;
  //And its parent
  Dst:=DraggedItem.Parent;
  //And duplicate
  DoCopyMove(True);
 end;
end;

{------------------------------------------------------------------------------}
//Copy to clipboard
{------------------------------------------------------------------------------}
procedure TMainForm.CopyToClipboardExecute(Sender: TObject);
begin
 //ShowMessage('Copy');
end;

{------------------------------------------------------------------------------}
//Paste From Clipboard
{------------------------------------------------------------------------------}
procedure TMainForm.PasteFromClipboardExecute(Sender: TObject);
begin
 //ShowMessage('Paste');
end;

{------------------------------------------------------------------------------}
//Ensure we have a hex number being entered
{------------------------------------------------------------------------------}
procedure TMainForm.ed_execaddrKeyPress(Sender: TObject; var Key: char);
begin
 //Verify it is in hex, delete, backspace or Enter
 if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
 and(Key<>chr(127))AND(Key<>chr(8))AND(Key<>chr(13))then Key:=#0;
end;

{------------------------------------------------------------------------------}
//Custom filetype keypress
{------------------------------------------------------------------------------}
procedure TMainForm.FileTypeKeyPress(Sender: TObject; var Key: char);
var
 ft   : String;
 dir,
 entry: Integer;
begin
 //Check for enter
 if Key=chr(13) then
 begin
  ft:=FTEdit.Text;
  //Hide the dialogue by setting it's ModalResult (doesn't matter what to)
  FTDialogue.ModalResult:=mrOK;
  //Get the references
  entry:=DirList.Selected.Index;
  dir:=TMyTreeNode(DirList.Selected).ParentDir;
  //And ensure they are valid
  if dir<Length(Image.Disc) then
   if entry<Length(Image.Disc[dir].Entries)then
    //Change the filetype
    if Image.ChangeFileType(Image.GetParent(dir)
                           +Image.DirSep
                           +Image.Disc[dir].Entries[entry].Filename
                           ,ft)then
    begin
     //If all went OK, update the display
     DirListChange(Sender,DirList.Selected);
     //And mark as changed
     HasChanged:=True;
    end;
 end;
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
 if Sender is TScrollBox then
  TileCanvas(TScrollBox(Sender).Canvas); //For a TForm
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
 if DirList.SelectionCount=0 then
  ReportError('Cannot find parent when adding directory "'+dirname+'"')
 else
 begin
  if DirList.Selections[0].Text<>'$' then //If not the root, get the parent directory
   parentdir:=GetImageFilename(TMyTreeNode(DirList.Selected).ParentDir,
                                           DirList.Selected.Index);
  //Add it
  index:=Image.CreateDirectory(dirname,parentdir,attr);
  //Function returns pointer to next item (or parent if no children)
  if index>-1 then //Directory added OK
  begin
   //Mark as changed
   HasChanged:=True;
   //Create the node as a file
   Node:=AddFileToTree(DirList.Selected,dirname,index,True,DirList{,Image});
   //Update the directory reference and the directory flag
   TMyTreeNode(Node).DirRef:=Length(Image.Disc)-1;
   TMyTreeNode(Node).IsDir:=True;
   //Update the image
   UpdateImageInfo;
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
    if index=-4 then ReportError('Catalogue full when adding "'+dirname+'"');
    if index=-5 then ReportError('Could not write directory "'+dirname+'" - error unknown');
    if index=-6 then ReportError('Destination directory does not exist');
    If index=-7 then ReportError('Could not write directory "'+dirname+'" - map full');
    if index=-8 then ReportError('Could not write directory "'+dirname+'" - nothing to write');
    if index=-9 then ReportError('Could not extend parent directory when adding "'+dirname+'"');
   end;
 end;
end;

{------------------------------------------------------------------------------}
//During a drag, check if the user has pressed a key modifier
{------------------------------------------------------------------------------}
procedure TMainForm.DirListKeyDown(Sender: TObject; var Key: Word;
 Shift: TShiftState);
begin
 if(MouseIsDown)and(IsDragging)then ImgCopy.Visible:=GetCopyMode(Shift);
 //Take a note of the shift state
 FormShiftState:=Shift;
end;

{------------------------------------------------------------------------------}
//During a drag, check if the user has released a key modifier
{------------------------------------------------------------------------------}
procedure TMainForm.DirListKeyUp(Sender: TObject; var Key: Word;
 Shift: TShiftState);
begin
 if(MouseIsDown)and(IsDragging)then ImgCopy.Visible:=True;
 //Forget the shift state
 FormShiftState:=[];
end;

{------------------------------------------------------------------------------}
//User has initiated a drag operation on the directory list
{------------------------------------------------------------------------------}
procedure TMainForm.DirListMouseDown(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
var
 dir,entry: Cardinal;
begin
 if(ssLeft in Shift)and(not(ssCtrl in Shift))then //Only if left button is clicked
 begin
  //Remember the node being dragged
  DraggedItem:=DirList.GetNodeAt(X,Y);
  //Does the file exist?
  if DraggedItem<>nil then
   if Image.FileExists(GetFilePath(DraggedItem),dir,entry) then
    if(dir<>$FFFF)and(entry<>$FFFF)then //Make sure it is not the root
    begin
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
 copymode  : Boolean;
 pt        : TPoint;
 Node      : TTreeNode;
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
   Dst:=GetNodeAt(Y);//Node under the cursor
   if (DraggedItem<>nil)AND(Dst<>nil)
   AND((DraggedItem<>Dst)or(Image.FormatNumber>>4=diAcornUEF))then
   begin
    //If it is not a directory, and not CFS, then get the parent
    if(not TMyTreeNode(Dst).IsDir)and(Image.FormatNumber>>4<>diAcornUEF)then
     Dst:=Dst.Parent;
    //Clear any selections and then highlight the original item, unless it is CFS
    DirList.ClearSelection;
    if Image.FormatNumber>>4<>diAcornUEF then DraggedItem.Selected:=True;
    //Only allow copying if it is a different parent and not within itself
    if(DraggedItem<>Dst)or(Image.FormatNumber>>4=diAcornUEF)then//Or UEF
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
    B.TransparentColor:=clFuchsia; //This is treated as transparent
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
   ObjectDrag.Left:=X-(DirList.ImagesWidth+4);
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
    Node:=GetNodeAt(Y);
    //If we find a node, and we're not in a delay.
    if(Node<>nil)and(not progsleep)then
    begin
     //Are we at the top, then get the previous node (if any)
     if Y<threshold then
      Node:=Node.GetPrevVisible
     else//Otherwise, get the next (if any)
      Node:=Node.GetNextVisible;
     //If we have a node, scroll to make it visible
     if Node<>nil then Node.MakeVisible;
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
 //If the destination is the same as the source, copy only (not UEF)
 if(DraggedItem<>nil)and(Dst<>nil)then
  if(DraggedItem.Parent=Dst)and(Image.FormatNumber>>4<>diAcornUEF)then
   Result:=True;
end;

{------------------------------------------------------------------------------}
//User has dropped an item over the directory list
{------------------------------------------------------------------------------}
procedure TMainForm.DirListMouseUp(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
var
 Xmovement,
 Ymovement : Integer;
 copymode  : Boolean;
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
   //Are we over another node? Then get the destination
   Dst:=GetNodeAt(Y);
   //Do the copy or move operation
   DoCopyMove(copymode);
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
//Do the Copy or Move operation
{------------------------------------------------------------------------------}
procedure TMainForm.DoCopyMove(copymode: Boolean);
var
 newfn  : String;
 i,
 index,
 dir,
 entry  : Integer;
 ref    : Cardinal;
 NewNode: TTreeNode;
begin
 //Are we dragging something, over something and is not the same as the source?
 if (DraggedItem<>nil)AND(Dst<>nil)
 AND((DraggedItem<>Dst)or(Image.FormatNumber>>4=diAcornUEF))then
 begin
  //If it is not a directory, or CFS format, then get the parent
  if (not TMyTreeNode(Dst).IsDir)
  and(Image.FormatNumber>>4<>diAcornUEF)then Dst:=Dst.Parent;
  //Only allow moving/copying if it is not within itself
  if(DraggedItem<>Dst)or(Image.FormatNumber>>4=diAcornUEF)then
  begin
   //Take a copy of the filename
   newfn:=DraggedItem.Text;
   //Do the copy/move
   if Image.FormatNumber>>4<>diAcornUEF then //Everything but UEF
   begin
    if copymode then //copy - here we rename the file if the destination already
     if Image.ValidateFilename(GetFilePath(Dst),newfn) then //has one the same
      index:=Image.CopyFile(GetFilePath(DraggedItem),GetFilePath(Dst),newfn);
    if not copymode then //move
     index:=Image.MoveFile(GetFilePath(DraggedItem),GetFilePath(Dst));
   end;
   if Image.FormatNumber>>4=diAcornUEF then //UEF only
   begin
    if copymode then //copy - Acorn UEF
     index:=Image.CopyFile(DraggedItem.AbsoluteIndex-1,Dst.AbsoluteIndex-1);
    if not copymode then //move - Acorn UEF
     index:=Image.MoveFile(DraggedItem.AbsoluteIndex-1,Dst.AbsoluteIndex-1);
   end;
   //Update the display
   if index>=0 then
   begin
    //index is only a reliable indicator of a success, so we need the file's
    //new reference
    if(Image.FileExists(GetFilePath(Dst)+Image.DirSep+newfn,ref))
    or(Image.FormatNumber>>4=diAcornUEF)then
    begin
     if Image.FormatNumber>>4<>diAcornUEF then
     begin
      entry:=ref mod $10000;  //Bottom 16 bits - entry reference
      dir  :=ref div $10000;  //Top 16 bits - directory reference
     end
     else
     begin //References work differently with UEF files
      entry:=index;
      dir  :=0;
      //This time we need the destination's parent
      if Dst.Parent<>nil then //Unless it is the root
      begin
       Dst:=Dst.Parent;
       //And we'll need to move the index up by one to compensate for the root
       inc(index);
      end;
     end;
     //Mark as changed
     HasChanged:=True;
     if not copymode then //If moving
     begin
      //Update any open hexdumps
      if Length(HexDump)>0 then
       for i:=0 to Length(HexDump)-1 do
       begin
        //Update the window title
        if HexDump[i].Caption=GetFilePath(DraggedItem) then
         HexDump[i].Caption:=GetFilePath(Dst)+Image.DirSep
                            +Image.Disc[dir].Entries[entry].Filename;
        //Update the menu item
        if HexDumpMenu.Count>i then
         if HexDumpMenu.Items[i].Caption=GetFilePath(DraggedItem) then
          HexDumpMenu.Items[i].Caption:=GetFilePath(Dst)+Image.DirSep
                              +Image.Disc[dir].Entries[entry].Filename;
       end;
     end;
     NewNode:=AddFileToTree(Dst,Image.Disc[dir].Entries[entry].Filename,
                            index,TMyTreeNode(DraggedItem).IsDir,DirList{,Image});
     //Did we just copy a directory?
     if TMyTreeNode(DraggedItem).IsDir then
     begin
      //Add all the items within
      //Get the pointer to the parent directory
      dir:=Image.Disc[dir].Entries[entry].DirRef;
      HasChanged:=True;
      //Now recursively add the contents of the newly created directory
      AddDirectoryToTree(NewNode,dir,Image,dir);
     end;
     //If we moved, we'll need to remove the old node too
     if not copymode then
     begin
      DraggedItem.Delete;
      DraggedItem:=nil;
      ResetFileFields;
     end;
     UpdateImageInfo;
    end
    else
     ReportError('Could not write file "'+DraggedItem.Text+'" - error unknown');
   end;
   if index<0 then
   begin
    //For some reason the operation failed to write the data these are the error
    //codes returned when writing a file, as it uses the same function)
    if index=-1  then ReportError('Could not load "'+DraggedItem.Text+'"');
    if index=-2  then ReportError('Could not add file "'+DraggedItem.Text+'" - image full');
    if index=-3  then ReportError('File "'+DraggedItem.Text+'" already exists in directory');
    if index=-4  then ReportError('Catalogue full when writing "'+DraggedItem.Text+'"');
    if index=-5  then ReportError('Could not write file "'+DraggedItem.Text+'" - error unknown');
    if index=-6  then ReportError('Destination directory does not exist when writing "'+DraggedItem.Text+'"');
    If index=-7  then ReportError('Could not write file "'+DraggedItem.Text+'" - map full');
    if index=-8  then ReportError('Could not write file "'+DraggedItem.Text+'" - nothing to write');
    if index=-9  then ReportError('Could not extend parent directory when writing "'+DraggedItem.Text+'"');
    if index=-10 then ReportError('Cannot move "'+DraggedItem.Text+'" to the same directory');
    if index=-11 then ReportError('Could not find source file "'+DraggedItem.Text+'"');
    if index=-12 then ReportError('Not possible on this system');
   end;
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Cancel drag drop
{------------------------------------------------------------------------------}
procedure TMainForm.CancelDragDropExecute(Sender: TObject);
begin
 //Clear the flags
 IsDragging:=False;
 MouseIsDown:=False;
 //Empty the containers
 Dst:=nil;
 DraggedItem:=nil;
 //Remove the copied control
 if ObjectDrag<>nil then ObjectDrag.Free;
 ObjectDrag:=nil;
 //And hide the 'copy' plus icon
 imgCopy.Visible:=False;
end;

{------------------------------------------------------------------------------}
//Create a new blank image
{------------------------------------------------------------------------------}
procedure TMainForm.btn_NewImageClick(Sender: TObject);
var
 minor,tracks: Byte;
 ok: Boolean;
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
    7: minor:=NewImageForm.AFS.ItemIndex;
   end;
   //Number of tracks (DFS only)
   tracks:=0; //Default
   if NewImageForm.MainFormat.ItemIndex=0 then
    tracks:=NewImageForm.DFSTracks.ItemIndex;
   //Now create the image
   ok:=False;
   //ADFS Hard Drive
   if(NewImageForm.MainFormat.ItemIndex=1)and(minor=8)then
    ok:=Image.FormatHDD(diAcornADFS,
                        NewImageForm.harddrivesize,
                        NewImageForm.newmap,
                        NewImageForm.dirtype)
   else //AFS
    if NewImageForm.MainFormat.ItemIndex=7 then
    begin
     ok:=Image.FormatHDD(diAcornFS,
                         NewImageForm.AFSImageSize.Position*10*1024,
                         False,
                         NewImageForm.AFS.ItemIndex+2);
     if(ok)and(NewImageForm.cb_AFScreatepword.Checked)then
      //Create blank password file for AFS
      if not Image.CreateAFSPassword(NewImageForm.AFS.ItemIndex+2,nil)then //If fails, report an error
       ReportError('Failed to create a password file');
    end
    else //Floppy Drive
     ok:=Image.FormatFDD(NewImageForm.MainFormat.ItemIndex,minor,tracks);
   if ok then
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
   //Attributes - ADFS
   if Image.FormatNumber>>4=diAcornFS then
   begin
    if cb_AFS_ownw.Checked then att:=att+'W';
    if cb_AFS_ownr.Checked then att:=att+'R';
    if cb_AFS_ownl.Checked then att:=att+'L';
    if cb_AFS_pubw.Checked then att:=att+'w';
    if cb_AFS_pubr.Checked then att:=att+'r';
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
    UpdateImageInfo;
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
 j,
 nodes   : Integer;
 R       : Boolean;
 filepath: String;
begin
 if DirList.SelectionCount>0 then
 begin
  //Take a note of the number of selections
  nodes:=DirList.SelectionCount;
  //Go through all the selections (or the only one)
  while DirList.SelectionCount>0 do
  begin
   //Get the full path to the file
   filepath:=GetFilePath(DirList.Selections[0]);
   //If singular, check if the user wants to
   if(nodes=1)and(confirm)then
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
     DirList.Selections[0].Delete;
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
    filename:=Image.GetParent(dir)+Image.DirSep+
              Image.Disc[dir].Entries[entry].Filename;
    //Load the file
    if Image.ExtractFile(filename,buffer,entry) then
    begin
     RemoveTopBit(filename);
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
     or(HexDump[index].IsBasicFile)
     or(HexDump[index].IsTextFile)then HexDump[index].DecodeBasicFile;
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
//Editing control has lost focus
{------------------------------------------------------------------------------}
procedure TMainForm.DirListExit(Sender: TObject);
begin
 //Make sure there is something selected
 if DirList.SelectionCount=1 then
  //Is it being edited?
  if DirList.Selected.EditText then
  begin
   //Change the name back to what it was
   DirList.Selected.Text:=NameBeforeEdit;
   //And cancel the editing
   DirList.Selected.EndEdit(True);
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
 //Enable/Disable the Load Address editing
 lb_loadaddr.Visible :=True;
 ed_loadaddr.Visible :=False;
 ed_loadaddr.Enabled :=False;
 //Enable/Disable the Execution Address editing
 lb_execaddr.Visible :=True;
 ed_execaddr.Visible :=False;
 ed_execaddr.Enabled :=False;
 //Blank the Filetype bitmap
 img_FileType.Picture.Bitmap:=nil;
 //Disable the access tick boxes
 ADFSAttrPanel.Visible:=False;
 AFSAttrPanel.Visible :=False;
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
// panwid : Integer;
begin
 //Set up the rectangle for the image - giving it 2px border
 imgRect.Top:=Rect.Top+3;
 imgRect.Left:=Rect.Left+3;
 imgRect.Height:=Rect.Height-6;
 imgRect.Width:=imgRect.Height;
{ panwid:=StatusBar.Canvas.TextWidth(Panel.Text)+4;
 if panwid<imgRect.Width then panwid:=imgRect.Width;
 Rect.Width:=panwid;}
 //First panel - we want to put the 'not saved' indicator here
 if(Panel.Index=0)and(HasChanged)then
  icons.StretchDraw(StatusBar.Canvas,changedicon,imgRect);
 //Second panel - needs a logo
 if(Panel.Index=1)and(Panel.Text<>'')then
 begin
{  inc(panwid,imgRect.Width+5);
  Rect.Width:=panwid;}
  png:=0;
  case Image.FormatNumber>>4 of
   diAcornDFS : png:=bbclogo;       //BBC Micro logo for DFS
   diAcornADFS:
   begin
    if(Image.FormatNumber mod $10<>3)
    and(Image.MapType=diADFSOldMap) then
     png:=acornlogo; //Acorn logo for 8 bit ADFS
    if(Image.FormatNumber mod $10=3)
    or(Image.MapType=diADFSNewMap) then
     png:=riscoslogo; //RISC OS logo for 32 bit ADFS
   end;
   diCommodore: png:=commodorelogo; //Commodore logo
   diSinclair : png:=sinclairlogo;  //Sinclair logo
   diAmiga    : png:=amigalogo;     //Amiga logo
   diAcornUEF : png:=acornlogo;     //Acorn logo for CFS
   diMMFS     : png:=bbclogo;       //BBC Micro logo for MMFS
   diSpark    : png:=sparklogo;     //!SparkFS logo for Spark
   diAcornFS  : png:=bbclogo;       //BBC Micro logo for Acorn FS
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
// Panel.Width:=panwid;
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
  if ParamCount>0 then //If we are in command line mode, then do not use the GUI
   WriteLn(error)
  else //Otherwise, display a nice window on the screen
   CustomDialogue.ShowError(error,'')
 else
  ErrorLogForm.ErrorLog.Lines.Add(error);
end;

{------------------------------------------------------------------------------}
//Ask the user for confirmation
{------------------------------------------------------------------------------}
function TMainForm.AskConfirm(confim,okbtn,cancelbtn,ignorebtn: String): TModalResult;
begin
 Result:=mrOK; //Default
 if ErrorReporting then
 begin
  CustomDialogue.ShowConfirm(confim,okbtn,cancelbtn,ignorebtn);
  Result:=CustomDialogue.ModalResult;
 end;
end;

{------------------------------------------------------------------------------}
//Show information to the user
{------------------------------------------------------------------------------}
procedure TMainForm.ShowInfo(info: String);
begin
 if ErrorReporting then
  if ParamCount>0 then //If we are in command line mode, then do not use the GUI
   WriteLn(info)
  else //Otherwise, display a nice window on the screen
   CustomDialogue.ShowInfo(info,'');
end;

{------------------------------------------------------------------------------}
//Update the progress text
{------------------------------------------------------------------------------}
procedure TMainForm.UpdateProgress(Fupdate: String);
begin
 WriteToDebug(Fupdate);
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
 b:=TBrush.Create;
 b.Bitmap:=GetTextureTile;
 c.Brush :=b;
 c.FillRect(rc);
 b.Free;
end;

{------------------------------------------------------------------------------}
//Return the current texture tile as a bitmap
{------------------------------------------------------------------------------}
function TMainForm.GetTextureTile(Ltile:Integer=-1): TBitmap;
begin
 Result:=nil; //None
 if Ltile<0 then Ltile:=TextureType;
 case Ltile of
  0: Result:=NoTile.Picture.Bitmap;           //No tile
  1: Result:=RO5TextureTile.Picture.Bitmap;   //RISC OS 5 style
  2: Result:=RO4TextureTile.Picture.Bitmap;   //RISC OS 4 style
  3: Result:=RO3TextureTile.Picture.Bitmap;   //RISC OS 3 style
  4: Result:=IyonixTextureTile.Picture.Bitmap;//Iyonix style
  5: Result:=ROPiTextureTile.Picture.Bitmap;  //RISC OS Pi style
 end;
end;

{------------------------------------------------------------------------------}
//Create the ADFS Filetype Dialogue box
{------------------------------------------------------------------------------}
procedure TMainForm.CreateFileTypeDialogue;
var
 i: Integer;
 sc: TScrollBox;
begin
 //Create the filetype pop-up
 FTDialogue:=TForm.Create(nil);
 FTDialogue.Parent:=nil;
 FTDialogue.BorderIcons:=[biSystemMenu];
 FTDialogue.BorderStyle:=bsDialog;
 FTDialogue.BorderWidth:=0;
 FTDialogue.Caption:='RISC OS Filetypes';
 FTDialogue.Color:=$ECECEC;
 FTDialogue.Visible:=False;
 FTDialogue.Width:=Round((64*5)*(Screen.PixelsPerInch/DesignTimePPI));
 FTDialogue.Position:=poMainFormCenter;
 //Scrollbox
 sc:=TScrollbox.Create(FTDialogue);
 sc.Parent:=FTDialogue;
 sc.Align:=alClient;
 sc.BorderStyle:=bsSingle;
 sc.OnPaint:=@FileInfoPanelPaint;
 //Controls
 SetLength(FTButtons,riscoshigh-2);
 for i:=3 to riscoshigh do
 begin
  FTButtons[i-3]:=TSpeedButton.Create(sc);
  FTButtons[i-3].Parent:=sc;
  FTButtons[i-3].Width:=Round(64*(Screen.PixelsPerInch/DesignTimePPI));
  FTButtons[i-3].Height:=Round(64*(Screen.PixelsPerInch/DesignTimePPI));
  FTButtons[i-3].Caption:=Image.GetFileTypeFromNumber(StrToInt('$'+FileTypes[i]));
  FTButtons[i-3].Tag:=StrToInt('$'+FileTypes[i]);
  FTButtons[i-3].Left:=((i-3)mod 5)*FTButtons[i-3].Width;
  FTButtons[i-3].Top :=((i-3)div 5)*FTButtons[i-3].Height;
  FTButtons[i-3].Flat:=True;
  FTButtons[i-3].GroupIndex:=1;
  FTButtons[i-3].Images:=FileImages;
  FTButtons[i-3].ImageWidth:=Round(32*(Screen.PixelsPerInch/DesignTimePPI));
  FTButtons[i-3].Layout:=blGlyphTop;
  FTButtons[i-3].ImageIndex:=i;
  FTButtons[i-3].OnClick:=@FileTypeClick;
 end;
 //Create a dummy button, for no selection
 FTDummyBtn:=TSpeedButton.Create(sc);
 FTDummyBtn.Parent:=sc;
 FTDummyBtn.GroupIndex:=1;
 FTDummyBtn.Visible:=False;
 //Custom filetype
 FTEdit:=TEdit.Create(sc);
 FTEdit.Parent:=sc;
 FTEdit.Width:=Round(64*(Screen.PixelsPerInch/DesignTimePPI));
 FTEdit.Text:='';
 FTEdit.Font.Name:='Courier New';
 FTEdit.Alignment:=taCenter;
 i:=Round(64*(Screen.PixelsPerInch/DesignTimePPI));
 FTEdit.Top:=(((riscoshigh-2)div 5)*i)+Round((i-FTEdit.Height)/2);
 FTEdit.Left:=((riscoshigh-2)mod 5)*FTEdit.Width;
 FTEdit.OnKeyPress:=@FileTypeKeyPress;
end;

{------------------------------------------------------------------------------}
//Write to the debug file
{------------------------------------------------------------------------------}
procedure TMainForm.WriteToDebug(line: String);
var
 F : TFileStream;
begin
 if Fdebug then
 begin
  if FileExists(debuglogfile) then
   F:=TFileStream.Create(debuglogfile,fmOpenWrite)
  else
   F:=TFileStream.Create(debuglogfile,fmCreate);
  F.Position:=F.Size;
  WriteLine(F,FormatDateTime(TimeDateFormat,Now)+': '+line);
  F.Free;
 end;
end;

end.
