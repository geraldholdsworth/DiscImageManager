unit MainUnit;

{
Disc Image Manager written by Gerald Holdsworth
Started out as a demo for the TDiscImage class but blossomed into a full-blown
application, ported across from Delphi to Lazarus and hence then compiled on
macOS and Linux in addition to the original Windows version. The underlying
class has also grown from being just a reader to also a writer.
Extra 'gimmicks' have been added over time, to utilise the code in the
underlying class.

Copyright ©2018-2025 Gerald Holdsworth gerald@hollypops.co.uk

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

DSK Image modules written by Damien Guard
}

{$MODE objFPC}{$H+}

interface

uses
  SysUtils,Classes,Graphics,Controls,Forms,Dialogs,StdCtrls,DiscImage,Global,
  ExtCtrls,Buttons,ComCtrls,Menus,DateUtils,ImgList,StrUtils,Clipbrd,HexDumpUnit,
  FPImage,IntfGraphics,ActnList,GraphType,DateTimePicker,Types,RFSDetailUnit,
  GJHCustomComponents,fpjson,DiscImageHelper;

type
 //We need a custom TTreeNode, as we want to tag on some extra information
 //Will need to set the OnCreateNodeClass event in the TTreeView
 TMyTreeNode = class(TTreeNode)
  private
   FPart      : Cardinal;
   FImage,
   FDirRef,
   FParentDir : Integer;
   FIsDir,
   FBeenRead,
   FBroken,
   FIsDOSPart : Boolean;
   FParentName: String;
  public
   property ParentDir : Integer  read FParentDir  write FParentDir; //Parent directory reference
   property IsDir     : Boolean  read FIsDir      write FIsDir;     //Is it a directory
   property DirRef    : Integer  read FDirRef     write FDirRef;    //Reference into TDiscImage.Disc
   property BeenRead  : Boolean  read FBeenRead   write FBeenRead;  //Has the directory been read in
   property Broken    : Boolean  read FBroken     write FBroken;    //Is the ADFS directory broken?
   property Image     : Integer  read FImage      write FImage;     //Index into the TDiscImages array
   property IsDOSPart : Boolean  read FIsDOSPart  write FIsDOSPart; //This is the DOS Partition file
   property ParentName: String   read FParentName write FParentName;//Fully qualified name of the parent
   property Partition : Cardinal read FPart       write FPart;      //Which partition?
 end;
 //Form definition - TMainForm
 TMainForm = class(TForm)
  DeleteAFile: TAction;
  AmigaAttrPanel: TPanel;
  AFSAttrPanel: TPanel;
  DOSAttributeLabel: TLabel;
  DOSAttrPanel: TPanel;
  SinclairAttributeLabel: TLabel;
  SinclairAttrPanel: TPanel;
  ISOAttrPanel: TPanel;
  ISOAttributeLabel: TLabel;
  CancelDragDrop: TAction;
  Help: TMemo;
  menuFixADFS: TMenuItem;
  menuDefrag: TMenuItem;
  menuChangeInterleave: TMenuItem;
  menuMultiCSV: TMenuItem;
  menuShowReport: TMenuItem;
  OAAttrLabelAmiga: TLabel;
  OthAttrLabelAmiga: TLabel;
  MiscAttrLabelAmiga: TLabel;
  PubAttrLabelAmiga: TLabel;
  HoverTimer: TTimer;
  ImageToolBarPage: TTabSheet;
  FIlesToolBarPage: TTabSheet;
  PartitionToolBarPage: TTabSheet;
  btn_FixADFS: TToolButton;
  btn_ChangeInterleave: TToolButton;
  btn_Defrag: TToolButton;
  btn_Settings: TToolButton;
  btn_About: TToolButton;
  btn_MultiCSV: TToolButton;
  ToolsToolBar: TToolBar;
  ToolsToolBarPage: TTabSheet;
  ImageToolBar: TToolBar;
  FilesToolBar: TToolBar;
  PartitionToolBar: TToolBar;
  menuImage: TMenuItem;
  menuFiles: TMenuItem;
  menuTools: TMenuItem;
  menuPartition: TMenuItem;
  ToolBarContainer: TPageControl;
  btn_NewImage: TToolButton;
  btn_OpenImage: TToolButton;
  btn_CloseImage: TToolButton;
  btn_SaveImage: TToolButton;
  btn_SaveAsCSV: TToolButton;
  btn_ImageDetails: TToolButton;
  btn_FileSearch: TToolButton;
  btn_ShowReport: TToolButton;
  btn_download: TToolButton;
  btn_AddFiles: TToolButton;
  btn_Rename: TToolButton;
  btn_HexDump: TToolButton;
  btn_NewDirectory: TToolButton;
  btn_Delete: TToolButton;
  btn_DuplicateFile: TToolButton;
  btn_AddPassword: TToolButton;
  btn_EditPassword: TToolButton;
  btn_SavePartition: TToolButton;
  btn_DeletePartition: TToolButton;
  btn_AddPartition: TToolButton;
  DuplicateFile1: TMenuItem;
  IyonixTextureTile: TImage;
  menuAddPasswordFile: TMenuItem;
  menuEditPasswordFile: TMenuItem;
  menuAddPartition: TMenuItem;
  menuViewFileDetails: TMenuItem;
  menuViewStatus: TMenuItem;
  menuViewToolBar: TMenuItem;
  TextureTiles: TPanel;
  ViewMenu: TMenuItem;
  menuSavePartition: TMenuItem;
  menuDeletePartition: TMenuItem;
  AFSOAAttributeLabel: TLabel;
  PartitionMenu: TMenuItem;
  AFSPubAttributeLabel: TLabel;
  ROPiTextureTile: TImage;
  RO3TextureTile: TImage;
  menuDuplicateFile: TMenuItem;
  menuOptions: TMenuItem;
  PasteFromClipboard: TAction;
  CopyToClipboard: TAction;
  KeyboardShortcuts: TActionList;
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
  menuSaveAsCSV: TMenuItem;
  menuRenameFile: TMenuItem;
  menuNewDir: TMenuItem;
  menuDeleteFile: TMenuItem;
  menuAbout: TMenuItem;
  FilesMenu: TMenuItem;
  ToolsMenu: TMenuItem;
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
  //Dynamically created tickboxes
  cb_C64_c, //Commodore 64
  cb_C64_l,
  cb_DFS_l, //Acorn DFS
  cb_ADFS_pubp, //Acorn ADFS
  cb_ADFS_ownw,
  cb_ADFS_ownr,
  cb_ADFS_ownl,
  cb_ADFS_owne,
  cb_ADFS_pubr,
  cb_ADFS_pubw,
  cb_ADFS_pube,
  cb_AFS_ownl, //Acorn FS
  cb_AFS_ownr,
  cb_AFS_ownw,
  cb_AFS_pubr,
  cb_AFS_pubw,
  cb_DOS_system, //DOS
  cb_DOS_read,
  cb_DOS_hidden,
  cb_DOS_archive,
  cb_Sinclair_system, //Sinclair
  cb_Sinclair_readonly,
  cb_Sinclair_archive,
  cb_ISO_hidden,
  cb_ISO_associated,
  cb_Amiga_othd, //Amiga
  cb_Amiga_arch,
  cb_Amiga_othe,
  cb_Amiga_pure,
  cb_Amiga_hold,
  cb_Amiga_scri,
  cb_Amiga_ownr,
  cb_Amiga_othr,
  cb_Amiga_ownw,
  cb_Amiga_owne,
  cb_Amiga_ownd,
  cb_Amiga_othw,
  cb_Amiga_pubw,
  cb_Amiga_pubr,
  cb_Amiga_pubd,
  cb_Amiga_pube: TRISCOSTickBox;
  //Events - mouse clicks
  procedure btn_AddPartitionClick(Sender: TObject);
  procedure btn_AddPasswordClick(Sender: TObject);
  procedure btn_ChangeInterleaveClick(Sender: TObject);
  procedure btn_CloseImageClick(Sender: TObject);
  procedure btn_DefragClick(Sender: TObject);
  procedure btn_DeletePartitionClick(Sender: TObject);
  procedure btn_EditPasswordClick(Sender: TObject);
  procedure btn_FileSearchClick(Sender: TObject);
  procedure btn_FixADFSClick(Sender: TObject);
  procedure btn_ImageDetailsClick(Sender: TObject);
  procedure btn_MultiCSVClick(Sender: TObject);
  procedure btn_NewDirectoryClick(Sender: TObject);
  procedure btn_SaveAsCSVClick(Sender: TObject);
  procedure btn_SavePartitionClick(Sender: TObject);
  procedure btn_SettingsClick(Sender: TObject);
  procedure btn_ShowReportClick(Sender: TObject);
  procedure DuplicateFile1Click(Sender: TObject);
  procedure ed_timestampEditingDone(Sender: TObject);
  procedure HexDumpSubItemClick(Sender: TObject);
  procedure lb_execaddrClick(Sender: TObject);
  procedure lb_loadaddrClick(Sender: TObject);
  procedure lb_timestampClick(Sender: TObject);
  procedure lb_titleClick(Sender: TObject);
  procedure ShowHideToolbar(Sender: TObject);
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
  procedure ToolBarContainerChange(Sender: TObject);
  procedure HoverTimerTimer(Sender: TObject);
  //Misc
  function AddDirectoryToImage(dirname: String): Boolean;
  procedure AddDirectoryToTree(CurrDir: TTreeNode; dir: Integer;
                                   ImageToUse:TDiscImage;var highdir: Integer);
  function AddFileErrorToText(error: Integer):String;
  function AddFileToImage(filename: String):Integer;
  function AddFileToImage(filename: String;filedetails: TDirEntry;
          buffer:TDIByteArray=nil;ignoreerror:Boolean=False):Integer; overload;
  function AddFileToTree(ParentNode: TTreeNode;importfilename: String;
     index:Integer;dir:Boolean;Tree:TTreeView;IsDOSPart:Boolean):TTreeNode;
  procedure AddImageToTree(Tree: TTreeView;ImageToUse: TDiscImage);
  procedure AddSparkToImage(filename: String);
  procedure ArrangeFileDetails;
  function AskConfirm(confim: String; Buttons: array of String): TModalResult;
  procedure CloseAllHexDumps;
  function ConvertToKMG(size: Int64): String;
  function CreateButton(Lparent: TControl; Lcaption: String;LDef: Boolean;
                        LLeft,LTop: Integer; LModal: TModalResult): TRISCOSButton;
  function CreateDirectory(dirname,attr: String): TTreeNode;
  procedure CreateFileTypeDialogue;
  procedure CreateNewImage;
  procedure Defrag(side: Byte=0);
  function DeleteFile(confirm: Boolean): Boolean;
  procedure DisableControls;
  procedure DoCopyMove(copymode: Boolean);
  procedure DownLoadDirectory(dir,entry: Integer; path: String);
  procedure DownLoadFile(dir,entry: Integer; path: String;filename: String='');
  procedure ExtractFiles(ShowDialogue: Boolean);
  function FindNode(filename: String;casesens:Boolean=True): TTreeNode;
  function FindPartitionRoot(filepath: String): Integer;
  function GetCopyMode(Shift: TShiftState): Boolean;
  function GetFilePath(Node: TTreeNode): String;
  function GetFileTypeGraphic(filetype: String;offset: Integer;
                                    const filetypes: array of String): Integer;
  function GetImageFilename(dir,entry: Integer): String;
  function GetImageIndex(Node: TTreeNode;ImageToUse: TDiscImage): Integer;
  procedure GetJSONString(json: TJSONObject; search: String; var output: String);
  procedure SetImageIndex(Node: TTreeNode;ImageToUse: TDiscImage);
  function GetNodeAt(Y: Integer): TTreeNode;
  function GetTextureTile(Ltile:Integer=-1): TBitmap;
  function ImportFiles(NewImage: TDiscImage;Dialogue: Boolean=True;
                                                Errors: Boolean=True): Integer;
  function IntToStrComma(size: Int64): String;
  procedure OpenImage(filename: String);
  function QueryUnsaved: Boolean;
  procedure ReadInDirectory(Node: TTreeNode);
  procedure ReportError(error: String);
  procedure ResetFileFields;
  procedure SaveAsCSV(filename: String='');
  procedure SaveAsCSV(FileNames: TStrings; filename: String=''); overload;
  procedure SaveConfigSettings;
  procedure SetNativeControls;
  procedure Scaling;
  procedure SelectNode(filename: String;casesens:Boolean=True);
  procedure ShowErrorLog;
  procedure ShowInfo(info: String);
  procedure ShowNewImage(title: String);
  procedure SwapLabelEdit(editcont: TEdit;labelcont: TLabel;dir,hex: Boolean);
  procedure TileCanvas(c: TCanvas);
  procedure TileCanvas(c: TCanvas;rc: TRect); overload;
  procedure UpdateImageInfo(partition: Cardinal=0);
  procedure UpdateProgress(Fupdate: String);
  procedure WriteToDebug(line: String);
 private
  var
   //To keep track of renames
   PathBeforeEdit,
   NameBeforeEdit      :String;
   //Stop the checkbox OnClick from firing when just changing the values
   DoNotUpdate         :Boolean;
   //Item being dragged on Directory List, and the destination
   Dst,
   DraggedItem         :TTreeNode;
   //To keep track of if we are dragging and if the mouse button is down
   IsDragging,
   MouseIsDown         :Boolean;
   //To remember where we started with the drag operation
   CursorPos           :TPoint;
   //The mouse 'cursor' while we are dragging
   ObjectDrag          :TImage;
   //Keep track of which hex dump windows are open
   HexDump             :array of THexDumpForm;
   //Reporting of errors
   ErrorReporting      :Boolean;
   //Delay flag
   progsleep           :Boolean;
   //Texture type
   TextureType         :Byte;
   //ADFS L Interleaved
   ADFSInterleave      :Byte;
   //Treat Spark as Filing System
   SparkIsFS           :Boolean;
   //Importing bypass GUI threshold
   bypassGUIThres      :Cardinal;
   //Create INF File?
   DoCreateINF         :Boolean;
   //Add implied attributes for DFS/CFS/RFS
   AddImpliedAttributes:Boolean;
   //Hide Commodore DEL files?
   DoHideDEL           :Boolean;
   //Filetype Dialogue Form
   FTDialogue          :TForm;
   //Filetype buttons on dialogue form
   FTButtons           :array of TSpeedButton;
   //Dummy button for dialogue form
   FTDummyBtn          :TSpeedButton;
   //Custom filetype on dialogue form
   FTEdit              :TEdit;
   //Keep a track of which buttons are pressed on the form
   FormShiftState      :TShiftState;
   //Produce a log file for debugging
   Fdebug              :Boolean;
   //Allow DFS images with zero number of sectors
   FDFSZeroSecs        :Boolean;
   //Check for files going beyond the disc edge on DFS
   FDFSBeyondEdge      :Boolean;
   //Check for blank filenames
   FDFSAllowBlank      :Boolean;
   //Compress UEF files
   FUEFCompress        :Boolean;
   //View options (what is visible)
   ViewOptions         :Cardinal;
   //Scan sub-directories on opening
   FScanSubDirs        :Boolean;
   //Open DOS Partitions on ADFS
   FOpenDOS            :Boolean;
   //Create *.dsc files with ADFS hard drives
   FCreateDSC          :Boolean;
   AppIsClosing        :Boolean;
   //Currently selected directory, when in console mode
   Fcurrdir            :Integer;
  const
   //These point to certain icons used when no filetype is found, or non-ADFS
   //The numbers are indexes into the TImageList component 'FileImages'.
   appicon     =   0; //RISC OS Application
   directory   =   1; //Generic directory
   directory_o =   2; //Generic open directory
   loadexec    =   3; //RISC OS Load/Exec
//    RObrokenfile=   4; //RISC OS broken file
   ROunknown   =   5; //Unknown RISC OS file
   //RISC OS Filetypes - used to locate the appropriate icon in the ImageList
   RISCOSFileTypes: array[6..143] of String =
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
   //To add more filetypes, increase the array, then ensure that the numbers
   //matche. Finally, make sure they are in the correct order in the
   //ImageList components: FileImages
   RISCOSApplications: array[144..176] of String =
                             ('!alarm','!boot','!chars','!closeup','!configure',
                              '!dpingscan','!draw','!edit','!flasher','!fontprint',
                              '!fonts','!help','!hform','!hopper','!maestro',
                              '!netsurf','!omni','!paint','!printedit',
                              '!printers','!puzzle','!resetboot','!routines',
                              '!scicalc','!serialdev','!showscrap','!sparkfs',
                              '!sparkfs1','!sparkfs2','!sparkfs3','!squash',
                              '!system','!t1tofont');
   //Commodore 64 filetypes
   C64unknown  = 177; //Commodore 64 unknown
   C64FileTypes: array[178..181] of String =
                             ('DEL','PRG','SEQ','USR');//Need REL and CBM
   //DOS filetypes
   unknown     = 182; //DOS unknown and generic for everything else
   DOSFileTypes: array[183..199] of String =
                             ('APP','BAT','BIN','CFG','CMD','COM','DOC','EXE',
                              'FNT','HLP','ICN','IMG','INF','INI','PAT','SYS',
                              'TXT');
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
   //Others
   mmbdisc     = 202; //MMFS Disc with image
   mmbdisclock = 201; //MMFS Locked disc
   mmbdiscempt = 200; //MMFS Empty slot
   //Icons for status bar - index into TImageList 'icons'
   changedicon   =  0;
   acornlogo     =  1;
   amigalogo     =  2;
   bbclogo       =  3;
   commodorelogo =  4;
   riscoslogo    =  5;
   sinclairlogo  =  6;
   sparklogo     =  7;
   bbcmasterlogo =  8;
   msdoslogo     =  9;
   romfslogo     = 10;
   //Time and Date format
   TimeDateFormat = 'hh:nn:ss dd mmm yyyy';
 public
  //The image
  Image         :TDiscImage;
//  Images        :TDiscImages;
  //Has the image changed since last saved?
  HasChanged    :Boolean;
  //Debug log filename
  debuglogfile,
  //What are we running on?
  platform,
  arch          :String;
  //Is the GUI open?
  Fguiopen      :Boolean;
  //Window styling
  Fstyling      :Byte;
  //Registry
  DIMReg        :TGJHRegistry;
  const
   //DPI that the application was designed in
   DesignedDPI        = 96;
   //Application Title
   ApplicationTitle   = 'Disc Image Manager';
   ApplicationVersion = '1.49.2';
   //Current platform and architecture (compile time directive)
   TargetOS  = {$I %FPCTARGETOS%};
   TargetCPU = {$I %FPCTARGETCPU%};
   //Styling
   NativeStyle = 0;
   RISCOSStyle = 1;
 end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  AboutUnit,NewImageUnit,ImageDetailUnit,ProgressUnit,SearchUnit,
  CustomDialogueUnit,ErrorLogUnit,SettingsUnit,ImportSelectorUnit,
  PWordEditorUnit,AFSPartitionUnit,ChangeInterleaveUnit,CSVPrefUnit,
  ImageReportUnit;

{-------------------------------------------------------------------------------
Add a new file to the disc image
-------------------------------------------------------------------------------}
procedure TMainForm.AddFile1Click(Sender: TObject);
var
  i    : Integer=0;
  files: array of String=nil;
begin
 WriteToDebug('MainForm.AddFile1Click');
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
//Get a string from a JSON
{------------------------------------------------------------------------------}
procedure TMainForm.GetJSONString(json: TJSONObject; search: String;
                                                            var output: String);
begin
 //If the search string is not found, the output variable goes unchanged
 if json.FindPath(search)<>nil then output:=json.FindPath(search).AsString;
end;

{------------------------------------------------------------------------------}
//Add a directory to an image
{------------------------------------------------------------------------------}
function TMainForm.AddDirectoryToImage(dirname: String): Boolean;
var
 OriginalNode : TTreeNode=nil;
 NewNode      : TTreeNode=nil;
 Lparent      : String='';
 inffile      : String='';
 attr         : String='';
 dirtitle     : String='';
 disctitle    : String='';
 importname   : String='';
 temp         : String='';
 F            : TFileStream=nil;
 fields       : TJSONObject=nil;//array of String=nil;
 Dir          : TSearchRec;
 Lcurrdir     : Integer=0;
 thisdir      : Integer=0;
begin
 Result:=False;
 WriteToDebug('MainForm.AddDirectoryToImage('+dirname+')');
 if Fguiopen then
 begin
  //Need to make this compatible with AmigaDOS too. *****************************
  Image.ProgressIndicator:=nil;
  ProgressForm.Show;
  //First, if there is no selection, make one, or if multiple, select the root
  if(DirList.SelectionCount=0)OR(DirList.SelectionCount>1)then
  begin
   DirList.ClearSelection;
   DirList.Items[0].Selected:=True;
  end;
  OriginalNode:=DirList.Selected;
  thisdir:=TMyTreeNode(OriginalNode).DirRef;
 end
 else
 begin
  thisdir:=Fcurrdir;
  Lcurrdir:=Fcurrdir; //Save the current directory pointer
 end;
 //If ADFS, create the directory, then select it
 if Image.MajorFormatNumber=diAcornADFS then
 begin
  importname:=ExtractFilename(dirname);
  attr      :='DLR';
  disctitle :='';
  dirtitle  :='';
  //Is there an inf file?
  if FileExists(dirname+'.inf') then
  begin
   inffile:='';
   //Read in the first line
   try
    F:=TFileStream.Create(dirname+'.inf',fmOpenRead OR fmShareDenyNone);
    F.Position:=0;
    ReadLine(F,inffile);
    fields:=TJSONObject.Create;
    ParseInf(fields,inffile);
    //Then extract the fields
    GetJSONString(fields,'Filename',importname);
    temp:='';
    GetJSONString(fields,'OPT',temp);
    if not temp.IsEmpty then
     Image.UpdateBootOption(StrToIntDef(temp,0)
                           ,Image.Disc[thisdir].Partition);
    GetJSONString(fields,'DIRTITLE',dirtitle);
    if importname='$' then
     GetJSONString(fields,'TITLE',disctitle)
    else
     GetJSONString(fields,'TITLE',dirtitle);
    GetJSONString(fields,'Access',attr);
    fields.Free;
    //Convert the attributes from hex to letters, if necessary
    attr:=GetAttributes(attr,Image.MajorFormatNumber);
    //Remove any quotes from the titles
    dirtitle :=dirtitle.DeQuotedString('"');
    disctitle:=disctitle.DeQuotedString('"');
    //Update the disc title, only on the root
    if(not disctitle.IsEmpty)and(importname='$')then
     Image.UpdateDiscTitle(LeftStr(disctitle,10)
                       ,Image.Disc[thisdir].Partition);
   except
    on E: Exception do
     ReportError('Failed to read inf file "'+dirname+'": '+E.Message);
   end;
   F.Free;
  end;
  //Convert a Windows filename to a BBC filename
  if Fguiopen then
   if(TMyTreeNode(OriginalNode).Parent=nil)
   and(importname=OriginalNode.Text)then else WinToBBC(importname)
  else
   if(Image.Disc[thisdir].Parent<0)
   and(importname=Image.Disc[thisdir].Directory) then else WinToBBC(importname);
  //Remove spaces for non-big directories, and ensure is 10 chars or less
  if Image.DirectoryType<>2 then
  begin
   importname:=ReplaceStr(importname,' ','_');
   importname:=LeftStr(importname,10);
  end;
  //Create the directory
  NewNode:=nil;
  if Fguiopen then
   if importname<>'$' then
    NewNode:=CreateDirectory(importname,attr)
   else
    NewNode:=OriginalNode
  else
   if importname<>'$' then
   begin
    Lparent:=Image.GetParent(thisdir);
    thisdir:=Image.CreateDirectory(importname,Lparent,attr);
    if thisdir>=0 then thisdir:=Image.Disc[Fcurrdir].Entries[thisdir].DirRef;
   end
   else
    thisdir:=Fcurrdir;
  if(NewNode<>nil)or((not Fguiopen)and(thisdir>=0))then //Success
  begin
   Result:=True;
   if Fguiopen then
   begin
    //And select it
    DirList.ClearSelection;
    NewNode.Selected:=True;
    thisdir:=TMyTreeNode(NewNode).DirRef;
   end;
   //Update the directory title
   if not dirtitle.IsEmpty then
    if(thisdir>=0)
    and(thisdir<Length(Image.Disc))then
    begin
     //if TMyTreeNode(NewNode).ParentDir>=0 then
     if Image.Disc[thisdir].Parent>=0 then
      disctitle:=Image.GetParent(thisdir)
     else disctitle:=importname;
     Image.RetitleDirectory(disctitle,dirtitle);
    end;
   //Now we import everything inside this
   FindFirst(dirname+pathdelim+'*',faDirectory,Dir);
   repeat
    if(Dir.Name<>'.')and(Dir.Name<>'..')then
    begin
     if(Dir.Attr AND faDirectory)=faDirectory then
     begin //Add any sub-directories
      UpdateProgress('Adding '+Dir.Name);
      Result:=(AddDirectoryToImage(dirname+pathdelim+Dir.Name))and(Result);
     end
     else
     begin //Add any files
      if not Fguiopen then Fcurrdir:=thisdir;
      if LowerCase(RightStr(Dir.Name,4))<>'.inf' then
       Result:=(AddFileToImage(dirname+pathdelim+Dir.Name)>=0)and(Result);
     end;
    end;
   until FindNext(Dir)<>0;
   FindClose(Dir);
  end;
 end;
 //Revert to the original selection
 if Fguiopen then
 begin
  DirList.ClearSelection;
  OriginalNode.Selected:=True;
 end
 else Fcurrdir:=Lcurrdir; //Restore the current directory pointer
end;

{------------------------------------------------------------------------------}
//Add a spark archive to an image
{------------------------------------------------------------------------------}
procedure TMainForm.AddSparkToImage(filename: String);
var
 SparkFile  : TSpark=nil;
 Index      : Integer=0;
 error      : Integer=0;
 filedetails: TDirEntry=();
 ParentDir  : String='';
 temp       : String='';
 buffer     : TDIByteArray=nil;
 ok         : Boolean=False;
 bypassGUI  : Boolean=False;
begin
 ResetDirEntry(filedetails);
 WriteToDebug('MainForm.AddSparkToImage('+filename+')');
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
    or(Image.MajorFormatNumber<>diAcornADFS)                //Acorn ADFS
    or(SparkFile.UncompressedSize>Image.FreeSpace(0))then //Not enough space
     ok:=AskConfirm('The current open image is not suitable for this archive. '
                   +'Would you like to continue?',['Yes','No'])=mrOK;
    if ok then
    begin
     //Show the progress form
     Image.ProgressIndicator:=@UpdateProgress;
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
      if not SparkFile.FileList[index].Parent.IsEmpty then
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
       if not SparkFile.FileList[Index].Parent.IsEmpty then
        SelectNode(ParentDir+Image.DirSep+SparkFile.FileList[Index].Parent)
       else
        SelectNode(ParentDir);
      //Update the attributes
      filedetails.Attributes:=GetAttributes(
                                IntToHex(SparkFile.FileList[Index].Attributes,2),
                                Image.MajorFormatNumber);
      //Assign the parent directory (if bypassing the GUI)
      if bypassGUI then
      begin
       if not SparkFile.FileList[index].Parent.IsEmpty then
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
       if not SparkFile.FileList[Index].Filename.IsEmpty then //We need a filename
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
  ErrorLogForm.Height:=Round(64*Screen.PixelsPerInch/DesignedDPI);
  //If it is off the screen, put it on the screen
  if ErrorLogForm.Top>Screen.DesktopHeight then ErrorLogForm.Top:=0;
 end;
end;

{------------------------------------------------------------------------------}
//Add a file to an image
{------------------------------------------------------------------------------}
function TMainForm.AddFileToImage(filename: String):Integer;
var
 filedetails: TDirEntry=();
begin
 WriteToDebug('MainForm.AddFileToImage('+filename+')');
 ResetDirEntry(filedetails);
 Result:=AddFileToImage(filename,filedetails);
end;
function TMainForm.AddFileToImage(filename:String;filedetails: TDirEntry;
                     buffer:TDIByteArray=nil;ignoreerror:Boolean=False):Integer;
var
  NewFile        : TDirEntry=();
  side           : Cardinal=0;
  ref            : Cardinal=0;
  i              : Byte=0;
  importfilename : String='';
  inffile        : String='';
  execaddr       : String='';
  loadaddr       : String='';
  attr1          : String='';
  attributes     : String='';
  filetype       : String='';
  p              : String='';
  temp           : String='';
  timestamp      : TDateTime;
  fields         : TJSONOBject=nil;//array of String=nil;
  F              : TFileStream=nil;
  ok             : Boolean=False;
  Node           : TTreeNode=nil;
 //This does the adding of the file
 function AddFile: Boolean;
 var
  index          : Integer=0;
 begin
  Result:=False;
  WriteToDebug('MainForm.AddFile');
  //Find out which side of a DFS disc it is
  if Fguiopen then
   if (Image.DoubleSided)//FormatNumber mod 2=1)
   and(Image.MajorFormatNumber=diAcornDFS)then //Only for DFS double sided
   //As with DFS we can only Add with the root selected, the index will be the side
    side:=DirList.Selected.Index
   else
   //Not double sided or DFS, so assume side 0
    side:=0
  else
   side:=Image.Disc[Fcurrdir].Partition;
  //Extract the filename
  if filedetails.Filename.IsEmpty then
   importfilename:=ExtractFileName(filename)
  else
   importfilename:=ExtractFileName(filedetails.Filename);
  //If we still have no filename, make one up
  if importfilename.IsEmpty then importfilename:='NewFile';
  //Reject any *.inf files, unless this is DOS
  if(LowerCase(RightStr(importfilename,4))<>'.inf')
  or(Image.MajorFormatNumber=diDOSPlus)then
  begin
   //Initialise the strings
   execaddr :='00000000';
   loadaddr :='00000000';
   attr1    :='';
   filetype :='';
   timestamp:=0;
   //Does the filename contain the filetype?
   if ((Pos(',',importfilename)>0)
   or  (Pos('.',importfilename)>0))
   and((Image.MajorFormatNumber=diAcornADFS)      //ADFS
   or  (Image.MajorFormatNumber=diCommodore)      //Commodore
   or  (Image.MajorFormatNumber=diSpark))then     //!Spark
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
    //ADFS and Spark
    if(Image.MajorFormatNumber=diAcornADFS)
    or(Image.MajorFormatNumber=diSpark)then
    begin
     filetype:=IntToHex(StrToIntDef('$'+filetype,0),3);
     if filetype='000' then filetype:='';//None, so reset
    end;
   end;
   //ADFS, AFS, DFS, Spark & CFS only stuff
   if((Image.MajorFormatNumber=diAcornDFS)
    or(Image.MajorFormatNumber=diAcornADFS)
    or(Image.MajorFormatNumber=diAcornUEF)
    or(Image.MajorFormatNumber=diAcornRFS)
    or(Image.MajorFormatNumber=diSpark)
    or(Image.MajorFormatNumber=diAcornFS))
   and(not filename.IsEmpty)then
   begin
    //Is there an inf file?
    if FileExists(filename+'.inf') then
    begin
     inffile:='';
     //Read in the first line
     try
      F:=TFileStream.Create(filename+'.inf',fmOpenRead OR fmShareDenyNone);
      F.Position:=0;
      ReadLine(F,inffile);
      fields:=TJSONObject.Create;
      ParseInf(fields,inffile);
      //Then extract the fields
      GetJSONString(fields,'Filename',importfilename);
      GetJSONString(fields,'Load Address',loadaddr);
      GetJSONString(fields,'Execution Address',execaddr);
      GetJSONString(fields,'Access',attr1);
      temp:='';
      GetJSONString(fields,'DATETIME',temp);
      timestamp:=EncodeDate(StrToIntDef(Copy(temp,10,4),2023),
                            StrToIntDef(Copy(temp,14,2),4),
                            StrToIntDef(Copy(temp,16,2),9))
                +EncodeTime(StrToIntDef(Copy(temp,18,2),0),
                            StrToIntDef(Copy(temp,20,2),0),
                            StrToIntDef(Copy(temp,22,2),0),0);
      //Full time and date overrides the AFS word
      temp:='';
      GetJSONString(fields,'Modification Date',temp);
      timestamp:=AFSToDateTime(StrToIntDef('$'+temp,0));//0 will be passed}
      fields.Free;
     except
      on E: Exception do
       ReportError('Failed to read inf file "'+filename+'.inf" : '+E.Message);
     end;
     F.Free;
    end;
   end;
   //Initialise the TDirArray
   ResetDirEntry(NewFile);
   //Supplied attributes override anything else
   if not filedetails.Filename.IsEmpty   then importfilename:=filedetails.Filename;
   if not filedetails.Attributes.IsEmpty then attr1:=filedetails.Attributes;
   if filedetails.LoadAddr  <>0  then loadaddr:=IntToHex(filedetails.LoadAddr,8);
   if filedetails.ExecAddr  <>0  then execaddr:=IntToHex(filedetails.ExecAddr,8);
   if filedetails.TimeStamp <>0  then timestamp:=filedetails.TimeStamp;
   //Decode the attributes
   attributes:=''; //Default
   if attr1.IsEmpty then
   begin
    if(Image.MajorFormatNumber=diAcornADFS)
    or(Image.MajorFormatNumber=diSpark)    then attributes:='WR';//Default for ADFS and Spark
    if Image.MajorFormatNumber=diCommodore then attributes:='C' ;//Default for Commodore
   end;
   attributes:=attributes+GetAttributes(attr1,Image.MajorFormatNumber);
   if importfilename.IsEmpty then importfilename:='NewFile';
   //Validate the filename (ADFS, AFS, DFS, Spark & CFS only)
   if(Image.MajorFormatNumber=diAcornDFS)
   or(Image.MajorFormatNumber=diAcornADFS)
   or(Image.MajorFormatNumber=diAcornUEF)
   or(Image.MajorFormatNumber=diAcornRFS)
   or(Image.MajorFormatNumber=diSpark)
   or(Image.MajorFormatNumber=diAcornFS)then
   begin
    //Remove any extraenous specifiers
    while (importfilename[4]=Image.DirSep) do
     importfilename:=RightStr(importfilename,Length(importfilename)-2);
    //If root, remove the directory specifier
    if(importfilename[2]=Image.DirSep)and(importfilename[1]='$')then
     importfilename:=RightStr(importfilename,Length(importfilename)-2);
    //Convert a Windows filename to a BBC filename
    WinToBBC(importfilename);
    //Check to make sure that a DFS directory hasn't been changed
    if((Image.MajorFormatNumber=diAcornDFS)
     or(Image.MajorFormatNumber=diAcornADFS)
     or(Image.MajorFormatNumber=diSpark)
     or(Image.MajorFormatNumber=diAcornFS))
    and(importfilename[2]='/')then
     importfilename[2]:=Image.DirSep;
    //Remove any spaces, unless it is a big directory
    if Image.DirectoryType<>diADFSBigDir then
     importfilename:=ReplaceStr(importfilename,' ','_');
   end;
   //Setup the record
   NewFile.Filename     :=importfilename;
   NewFile.ExecAddr     :=StrToInt('$'+execaddr);
   NewFile.LoadAddr     :=StrToInt('$'+loadaddr);
   NewFile.TimeStamp    :=timestamp;
   NewFile.Side         :=side;
   NewFile.Attributes   :=attributes;
   NewFile.DirRef       :=-1; //Not a directory
   NewFile.ShortFileType:=filetype;
   //Set the parent
   if Fguiopen then
   begin
    if(Image.MajorFormatNumber=diAcornADFS) //Need the selected directory for ADFS
    or(Image.MajorFormatNumber=diSpark)     //And Spark
    or(Image.MajorFormatNumber=diAcornFS)   //And Acorn FS
    or(Image.MajorFormatNumber=diAmiga)     //And Amiga
    or(Image.MajorFormatNumber=diDOSPlus)then//And DOS Plus
     if(DirList.Selected.Text='$')
     or(DirList.Selected.Text='AFS$')
     or(DirList.Selected.Text='DF0:')
     or(DirList.Selected.Text='A:')
     or(DirList.Selected.Text='C:')then NewFile.Parent:=DirList.Selected.Text
     else
      NewFile.Parent    :=GetImageFilename(TMyTreeNode(DirList.Selected).ParentDir,
                                           DirList.Selected.Index);
    if Image.MajorFormatNumber=diAcornDFS then //We'll set up a parent for DFS
     NewFile.Parent:=':'+IntToStr(side*2)+'.$';
   end
   else NewFile.Parent:=Image.GetParent(Fcurrdir);
   //Set the length - the actual length overrides everything else
   NewFile.Length:=Length(buffer);
   //Does the file already exist?
   Result:=True;
   if Fguiopen then
    if (Image.MajorFormatNumber<>diAcornUEF)
    and(Image.MajorFormatNumber<>diAcornRFS)then
     if Image.FileExists(NewFile.Parent+Image.DirSep+NewFile.Filename,ref) then
     begin
      Result:=AskConfirm('"'+NewFile.Filename+'" already exists in the directory "'
                    +NewFile.Parent+'". Overwrite?',['Yes','No'])=mrOK;
      if Result then //Delete the original
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
       Result:=True;
      end;
     end;
  end;
 end;
//Main function code starts here
begin
 Result:=-5;
 WriteToDebug('MainForm.AddFileToImage('+filename+',TDirEntry)');
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
  except
   on E: Exception do
    ReportError('Failed to read file "'+filename+'": '+E.Message);
  end;
  F.Free;
 end;
 if Length(buffer)>0 then //Only try and add if there is some data
 begin
  ok:=False;
  if Fguiopen then
  begin
   //First, if there is no selection, make one, or if multiple, select the root
   if(DirList.SelectionCount=0)OR(DirList.SelectionCount>1)then
   begin
    DirList.ClearSelection;
    DirList.Items[0].Selected:=True;
   end;
   //Make sure that there is a destination selected - should be after last command
   if DirList.SelectionCount=1 then
   begin
    //If the selection is not a directory
    if not TMyTreeNode(DirList.Selected).IsDir then
    begin
     //Remember the selection
     Node:=DirList.Selected;
     //Unselect everything
     DirList.ClearSelection;
     //Now select the parent, which should be a directory
     Node.Parent.Selected:=True;
    end;
    //Make sure that the selection is a directory
    if TMyTreeNode(DirList.Selected).IsDir then
    begin
     //Make sure the directory has been read in
     if not TMyTreeNode(DirList.Selected).BeenRead then
      ReadInDirectory(DirList.Selected);
     ok:=True;
    end
    else ReportError('"'+DirList.Selected.Text+'" is not a directory');
   end
   else
    if DirList.SelectionCount=0 then
     ReportError('No destination directory selected')
    else
     ReportError('Cannot add to multiple selection');
  end else ok:=True;
  if ok then
  begin
   ok:=AddFile;
   //Write the File
   if ok then
   begin
    Result:=Image.WriteFile(NewFile,buffer);
    if Fguiopen then
     //Function returns pointer to next item (or parent if no children)
     if Result>-1 then //File added OK
     begin
      if Image.MajorFormatNumber<>diSpark then HasChanged:=True;
      AddFileToTree(DirList.Selected,NewFile.Filename,Result,False,DirList,False);
      UpdateImageInfo(side);
     end
     else
     if not ignoreerror then //For some reason the operation failed to write the data
     begin
      //Prepare the filename, including the parent
      p:=NewFile.Parent;
      if not p.IsEmpty then p:=p+Image.DirSep;
      //Report the error
      ReportError('Error when adding file "'+p+NewFile.Filename
                 +'": '+AddFileErrorToText(-Result));
     end;
   end else Result:=-3;
  end;
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
   index: Integer;dir: Boolean;Tree:TTreeView;IsDOSPart:Boolean): TTreeNode;
begin
 Result:=nil;
 WriteToDebug('MainForm.AddFileToTree(TTreeNode,'
              +importfilename+','+IntToStr(index)+',...)');
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
  //Set the DOS Partition flag
  TMyTreeNode(Result).IsDOSPart:=IsDOSPart;
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
 Node       : TTreeNode=nil;
 Nodes      : array of TTreeNode=nil;
 Roots      : array of TTreeNode=nil;
 entry      : Integer=0;
 dir        : Integer=0;
 s          : Integer=0;
 saver      : Boolean=False;
 showsave   : Boolean=False;
 selectroot : Boolean=False;
begin
 WriteToDebug('MainForm.ExtractFiles');
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
    Insert(DirList.Selections[s],Nodes,Length(Nodes));
  //Make a note of all the selected roots
  for s:=0 to DirList.SelectionCount-1 do
   if DirList.Selections[s].Parent=nil then 
    Insert(DirList.Selections[s],Nodes,Length(Nodes));
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
    if Image.FileExists(GetFilePath(Node),dir,entry) then
    begin
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
        if Image.Title(Image.Disc[dir].Partition).IsEmpty then
         SaveImage.FileName:=DirList.Items[0].Text
        else
         SaveImage.FileName:=Image.Title(Image.Disc[dir].Partition);
       end
       else
        SaveImage.FileName:=Image.GetWindowsFilename(dir,entry);
       //Get the result
       saver:=SaveImage.Execute;
       //User clicked on Cancel, so exit
       if not saver then exit;
       if(saver)and(selectroot)then //Root was selected, so create the directory
       begin
        CreateDir(SaveImage.FileName);
        Image.CreateRootInf(SaveImage.FileName,Image.Disc[dir].Partition);
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
 WriteToDebug('MainForm.GetImageFilename('+IntToStr(dir)+','
              +IntToStr(entry)+')');
 Result:='';
 if(dir>=0)and(dir<Length(Image.Disc))then
  if(Length(Image.Disc[dir].Entries)=0)or(entry=-1)then
   Result:=Image.Disc[dir].Directory
  else
   Result:=Image.GetParent(dir)+Image.GetDirSep(Image.Disc[dir].Partition)
          +Image.Disc[dir].Entries[entry].Filename;
end;

{------------------------------------------------------------------------------}
//Download a file
{------------------------------------------------------------------------------}
procedure TMainForm.DownLoadFile(dir,entry: Integer;path: String;filename: String='');
var
 F              : TFileStream=nil;
 buffer         : TDIByteArray=nil;
 imagefilename  : String='';
 windowsfilename: String='';
begin
 WriteToDebug('MainForm.DownloadFile('+IntToStr(dir)+','+IntToStr(entry)+','
              +path+','+filename+')');
 // Ensure path ends in a directory separator
 if Length(path)>0 then
  if path[Length(path)]<>PathDelim then path:=path+PathDelim;
 //Object is a file, so download it
 if Image.Disc[dir].Entries[entry].DirRef=-1 then
 begin
  //Get the full path and filename
  imagefilename  :=GetImageFilename(dir,entry);
  if filename.IsEmpty then //If a filename has not been supplied, generate one
   windowsfilename:=Image.GetWindowsFilename(dir,entry)
  else                //Otherwise use the supplied filename
   windowsfilename:=filename;
  if Image.ExtractFile(imagefilename,buffer,entry) then
  begin
   //Save the buffer to the file
   try
    F:=TFileStream.Create(path+windowsfilename,fmCreate OR fmShareDenyNone);
    F.Position:=0;
    F.Write(buffer[0],Length(buffer));
    if DoCreateInf then Image.CreateINFFile(dir,entry,path,filename);
    if not Fguiopen then WriteLn('Success.');
   except
    //Could not create file
    on E: Exception do
     ReportError('Failed to open file stream "'+path+windowsfilename
                +'": '+E.Message);
   end;
   F.Free;
  end
  //Happens if the file could not be located
  else
   ReportError('Could not download file "'+imagefilename+'"');
 end
 else DownLoadDirectory(dir,entry,path+windowsfilename);
end;

{------------------------------------------------------------------------------}
//Download an entire directory
{------------------------------------------------------------------------------}
procedure TMainForm.DownLoadDirectory(dir,entry: Integer;path: String);
var
 imagefilename  : String='';
 windowsfilename: String='';
 ref            : Cardinal=0;
 c              : Integer=0;
 s              : Integer=0;
 Node           : TTreeNode=nil;
 ok             : Boolean=False;
begin
 WriteToDebug('MainForm.DownloadDirectory('+IntToStr(dir)+','+IntToStr(entry)
              +','+path+')');
 // Ensure path ends in a directory separator
 if Length(path)>0 then
  if path[Length(path)]<>PathDelim then path:=path+PathDelim;
 //Get the full path and filename
 imagefilename:=GetImageFilename(dir,entry);
 ok:=False;
 //Find the correct node
 if Fguiopen then
 begin
  Node:=FindNode(imagefilename);
  ok:=Node<>nil;
 end else
 begin
  ok:=True;
  WriteLn();
 end;
 if ok then
 begin
  //Need to ensure that the directory has been read in
  if Fguiopen then
   if not TMyTreeNode(Node).BeenRead then
    ReadInDirectory(Node);
  //Convert to Windows filename
  windowsfilename:=Image.GetWindowsFilename(dir,entry);
  if Image.FileExists(imagefilename,ref) then
  begin
   //Create the directory
   if not DirectoryExists(path+windowsfilename) then
   begin
    CreateDir(path+windowsfilename);
    if DoCreateInf then Image.CreateINFFile(dir,entry,path);
   end;
   //Navigate into the directory
   s:=Image.Disc[dir].Entries[entry].DirRef;
   //Iterate through the entries
   for c:=0 to Length(Image.Disc[s].Entries)-1 do
   begin
    if not Fguiopen then
     Write('Extracting '
           +Image.Disc[s].Entries[c].Parent
           +Image.GetDirSep(Image.Disc[s].Partition)
           +Image.Disc[s].Entries[c].Filename+' ');
    DownLoadFile(s,c,path+windowsfilename);
   end;
  end
  //Happens if the file could not be located
  else
   ReportError('Could not locate directory "'+imagefilename+'"');
 end;
end;

{------------------------------------------------------------------------------}
//User has clicked on the button to open a new image
{------------------------------------------------------------------------------}
procedure TMainForm.btn_OpenImageClick(Sender: TObject);
begin
 WriteToDebug('MainForm.btn_OpenImageClick');
 OpenImageFile.Options:=[ofEnableSizing,ofViewDetail];
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
 dir   : Cardinal=0;
 entry : Cardinal=0;
begin
 WriteToDebug('MainForm.OpenImage('+filename+')');
 //Show a progress message
 Image.ProgressIndicator:=@UpdateProgress;
 ProgressForm.Show;
 //Process the messages to close the file dialogue box
 Application.ProcessMessages;
 //Close any open hex dump windows
 CloseAllHexDumps;
 Image.ProgressIndicator   :=@UpdateProgress;
 //Update the interleave when loading
 Image.InterleaveMethod    :=ADFSInterleave;
 //Treat Sparks as a filing system
 Image.SparkAsFS           :=SparkIsFS;
 //Allow DFS to have zero number of sectors
 Image.AllowDFSZeroSectors :=FDFSZeroSecs;
 //Check for files going over the disc edge on DFS
 Image.DFSBeyondEdge       :=FDFSBeyondEdge;
 //Check for blank filenames in DFS
 Image.DFSAllowBlanks      :=FDFSAllowBlank;
 //Scan sub directories
 Image.ScanSubDirs         :=FScanSubDirs;
 //Open DOS Partitions
 Image.OpenDOSPartitions   :=FOpenDOS;
 //Create *.dsc files with ADFS hard drives
 Image.CreateDSC           :=FCreateDSC;
 //Add implied attributes to DFS/CFS/RFS
 Image.AddImpliedAttributes:=AddImpliedAttributes;
 //Load the image and create the catalogue
 if Image.LoadFromFile(filename) then
 begin
  HasChanged:=False;
  //Write debugging information
  if Length(Image.Disc)>0 then
  begin
   WriteToDebug('MainForm.OpenImage: Image '+filename+' opened');
   for dir:=0 to Length(Image.Disc)-1 do
   begin
    WriteToDebug('MainForm.OpenImage: '
                 +IntToStr(dir)+': '+Image.Disc[dir].Directory);
    if Length(Image.Disc[dir].Entries)>0 then
     for entry:=0 to Length(Image.Disc[dir].Entries)-1 do
      WriteToDebug('MainForm.OpenImage: '
          +IntToStr(dir)+','+IntToStr(entry)+': '
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
 WriteToDebug('MainForm.sb_ClipboardClick');
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
 entry : Integer=0;
 Node  : TTreeNode=nil;
 Tree  : TTreeView=nil;
begin
 WriteToDebug('MainForm.AddDirectoryToTree(TTreeNode,'+IntToStr(dir)
              +',TDiscImage,'+IntToStr(highdir)+')');
 if ImageToUse.Disc[dir].Deleted then exit;
 Tree:=TTreeView(CurrDir.Owner.Owner);
 //Make a note of the dir ref, it is the highest
 if dir>highdir then highdir:=dir;
 //Set the 'IsDir' flag to true, as this is a directory
 TMyTreeNode(CurrDir).IsDir    :=True;
 TMyTreeNode(CurrDir).DirRef   :=dir;
 TMyTreeNode(CurrDir).BeenRead :=ImageToUse.Disc[dir].BeenRead;
 TMyTreeNode(CurrDir).Broken   :=ImageToUse.Disc[dir].Broken;
 TMyTreeNode(CurrDir).IsDOSPart:=False;
 TMyTreeNode(CurrDir).Partition:=ImageToUse.Disc[dir].Partition;
 //Iterate though all the entries
 for entry:=0 to Length(ImageToUse.Disc[dir].Entries)-1 do
 begin
  //Adding new nodes for each one
  Node:=AddFileToTree(CurrDir,ImageToUse.Disc[dir].Entries[entry].Filename,
                      entry,false,Tree,
                      ImageToUse.Disc[dir].Entries[entry].isDOSPart);
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
 WriteToDebug('MainForm.ShowNewImage('+title+')');
 //Clear all the labels, and enable/disable the buttons
 ResetFileFields;
 //Clear the search fields
 SearchForm.ResetSearchFields;
 //Change the application title (what appears on SHIFT+TAB, etc.)
 Caption:=ApplicationTitle;
 if not title.IsEmpty then Caption:=Caption+' - '+ExtractFileName(title);
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
 btn_ShowReport.Enabled:=True;
 menuShowReport.Enabled:=True;
 if(Length(Image.FreeSpaceMap)>0)
 or(Image.MajorFormatNumber=diAcornRFS)then
 begin
  btn_ImageDetails.Enabled:=True;
  menuImageDetails.Enabled:=True;
 end;
 if Image.MajorFormatNumber=diAcornADFS then
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
 highdir    : Integer=0;
begin
 WriteToDebug('MainForm.AddImageToTree');
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
   //add the roots for multi-partition images.
  until highdir=Length(ImageToUse.Disc);
  TMyTreeNode(Tree.Items[0]).ParentDir:=-1;
  //Expand the top level of the tree (but not MMB)
  if ImageToUse.MajorFormatNumber<>diMMFS then Tree.TopItem.Expand(False);
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
 i    : Integer=0;
 title: String='';
begin
 WriteToDebug('MainForm.UpdateImageInfo('+IntToStr(partition)+')');
 //Only if there is a valid image
 if Image.FormatNumber<>diInvalidImg then
 begin
  //Image Format
  ImageDetails.Panels[1].Text:=Image.FormatString;
  //Disc name
  title:=Image.Title(partition);
  if title.IsEmpty then title:=' ';
  RemoveTopBit(title);//Ensure top bit not set
  ImageDetails.Panels[2].Text:=title;
  if(lb_title.Visible)and(DirTitleLabel.Caption='Disc Title')then
   lb_title.Caption:=title;
  //Disc size
  ImageDetails.Panels[3].Text:=ConvertToKMG(Image.DiscSize(partition))
                           +' ('+IntToStrComma(Image.DiscSize(partition))+' Bytes)';
  //Free space
  ImageDetails.Panels[4].Text:=ConvertToKMG(Image.FreeSpace(partition))
                           +' ('+IntToStrComma(Image.FreeSpace(partition))+' Bytes)';
  //Double sided or not (DFS only)
  if Image.MajorFormatNumber=diAcornDFS then
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
  //Interleave (ADFS/DFS only)
  ImageDetails.Panels[8].Text:=Image.InterleaveInUse;
 end
 else //Reset all to blank
  for i:=1 to 8 do
   ImageDetails.Panels[i].Text:='';
 //Update the status bar
 ImageDetails.Repaint;
end;

{------------------------------------------------------------------------------}
//Re-arrange, and show/hide, the various elements in the File Details panel
{------------------------------------------------------------------------------}
procedure TMainForm.ArrangeFileDetails;
var
 cbpos : Integer=0;
 dir   : Integer=0;
 attr  : Boolean=False;
 afs   : Boolean=False;
 dos   : Boolean=False;
 procedure ArrangeComponent(c,p: TControl;l: TLabel);
 begin
  c.Visible:=l.Caption<>'';
  if c.Visible then inc(cbpos);
  c.Height :=l.Top+l.Height;
  c.Top    :=p.Top+p.Height;
 end;
 function EquallySpace(num: Byte): Integer;
 begin
  Result:=FileInfoPanel.Width div num;
 end;
begin
 WriteToDebug('MainForm.ArrangeFileDetails');
 //If there is just the one item selected
 if DirList.SelectionCount=1 then
 begin
  //And it is not the root
  if DirList.Selections[0].Parent<>nil then
  begin
   //Set the flag to show the attributes
   attr:=True;
   //And make a note of the AFS and DOS flags, for ADFS
   dir:=TMyTreeNode(DirList.Selections[0].Parent).DirRef; //Directory reference of the parent
   if(dir>=0)and(dir<Length(Image.Disc))then
   begin
    afs:=Image.Disc[dir].AFSPartition;
    dos:=Image.Disc[dir].DOSPartition;
   end;
  end;
 end;
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
 //Make the appropriate attribute panel visible
 if(cbpos>0)and(attr)then
 begin
  //DFS and UEF
  if(Image.MajorFormatNumber=diAcornDFS)
  or(Image.MajorFormatNumber=diAcornUEF)
  or(Image.MajorFormatNumber=diAcornRFS)then
  begin
   //Make it visible
   DFSAttrPanel.Visible:=True;
   //Position it below the CRC32 section
   DFSAttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
   //Position the tick box inside
   DFSAttributeLabel.Top:=0;
   DFSAttributeLabel.Left:=(DFSAttrPanel.Width-DFSAttributeLabel.Width)div 2;
   cb_DFS_l.Top:=DFSAttributeLabel.Top+DFSAttributeLabel.Height;
   cb_DFS_l.Left:=(FileInfoPanel.Width-cb_DFS_l.Width)div 2;
   //And change the panel height to accomodate
   DFSAttrPanel.Height:=cb_DFS_l.Top+cb_DFS_l.Height;
  end;
  //ADFS and SparkFS
  if((Image.MajorFormatNumber=diAcornADFS)
  or (Image.MajorFormatNumber=diSpark)
  or (Image.ISOFormatNumber=diAcornADFS))
  and(not afs)and(not dos)then
  begin
   //Make it visible
   ADFSAttrPanel.Visible:=True;
   //Position it below the CRC32 section
   ADFSAttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
   //Position the ticks box inside - Owner Access
   OAAttributeLabel.Top:=0;
   OAAttributeLabel.Left:=(ADFSAttrPanel.Width-OAAttributeLabel.Width)div 2;
   cb_ADFS_ownw.Top:=OAAttributeLabel.Top+OAAttributeLabel.Height;
   cb_ADFS_ownr.Top:=cb_ADFS_ownw.Top;
   cb_ADFS_ownl.Top:=cb_ADFS_ownw.Top;
   cb_ADFS_owne.Top:=cb_ADFS_ownw.Top;
   cbpos:=EquallySpace(4); //Equally space them
   cb_ADFS_ownw.Left:=cbpos*0;
   cb_ADFS_ownr.Left:=cbpos*1;
   cb_ADFS_ownl.Left:=cbpos*2;
   cb_ADFS_owne.Left:=cbpos*3;
   //Position the ticks box inside - Public Access
   PubAttributeLabel.Top:=cb_ADFS_ownw.Top+cb_ADFS_ownw.Height;
   PubAttributeLabel.Left:=(ADFSAttrPanel.Width-PubAttributeLabel.Width)div 2;
   cb_ADFS_pubw.Top:=PubAttributeLabel.Top+PubAttributeLabel.Height;
   cb_ADFS_pubr.Top:=cb_ADFS_pubw.Top;
   cb_ADFS_pube.Top:=cb_ADFS_pubw.Top;
   cb_ADFS_pubp.Top:=cb_ADFS_pubw.Top;
   cb_ADFS_pubw.Left:=cbpos*0;
   cb_ADFS_pubr.Left:=cbpos*1;
   cb_ADFS_pube.Left:=cbpos*2;
   cb_ADFS_pubp.Left:=cbpos*3;
   //And change the panel height to accomodate
   ADFSAttrPanel.Height:=cb_ADFS_pubw.Top+cb_ADFS_pubw.Height;
   //Show/hide those not applicable for new/old directories
   if Image.MajorFormatNumber<>diISO then
   begin
    cb_ADFS_owne.Visible:=Image.MinorFormatNumber<3;
    cb_ADFS_pube.Visible:=cb_ADFS_owne.Visible;
    cb_ADFS_pubp.Visible:=cb_ADFS_owne.Visible;
   end;
  end;
  //Acorn FS
  if(Image.MajorFormatNumber=diAcornFS)
  or((Image.MajorFormatNumber=diAcornADFS)and(afs))then
  begin
   //Make it visible
   AFSAttrPanel.Visible:=True;
   //Position it below the CRC32 section
   AFSAttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
   //Position the ticks box inside - Owner Access
   AFSOAAttributeLabel.Top:=0;
   AFSOAAttributeLabel.Left:=(AFSAttrPanel.Width-AFSOAAttributeLabel.Width)div 2;
   cb_AFS_ownw.Top:=AFSOAAttributeLabel.Top+AFSOAAttributeLabel.Height;
   cb_AFS_ownr.Top:=cb_AFS_ownw.Top;
   cb_AFS_ownl.Top:=cb_AFS_ownw.Top;
   cbpos:=EquallySpace(3); //Equally space them
   cb_AFS_ownw.Left:=cbpos*0;
   cb_AFS_ownr.Left:=cbpos*1;
   cb_AFS_ownl.Left:=cbpos*2;
   //Position the ticks box inside - Public Access
   AFSPubAttributeLabel.Top:=cb_AFS_ownw.Top+cb_AFS_ownw.Height;
   AFSPubAttributeLabel.Left:=(AFSAttrPanel.Width-AFSPubAttributeLabel.Width)div 2;
   cb_AFS_pubw.Top:=AFSPubAttributeLabel.Top+AFSPubAttributeLabel.Height;
   cb_AFS_pubr.Top:=cb_AFS_pubw.Top;
   cbpos:=EquallySpace(2); //Equally space them
   cb_AFS_pubw.Left:=cbpos*0;
   cb_AFS_pubr.Left:=cbpos*1;
   //And change the panel height to accomodate
   AFSAttrPanel.Height:=cb_AFS_pubw.Top+cb_AFS_pubw.Height;
  end;
  //DOS Plus
  if(Image.MajorFormatNumber=diDOSPlus)
  or((Image.MajorFormatNumber=diAcornADFS)and(dos))then
  begin
   //Make it visible
   DOSAttrPanel.Visible:=True;
   //Position it below the CRC32 section
   DOSAttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
   //Position the ticks box inside
   DOSAttributeLabel.Top:=0;
   DOSAttributeLabel.Left:=(DOSAttrPanel.Width-DOSAttributeLabel.Width)div 2;
   cb_DOS_hidden.Top :=DOSAttributeLabel.Top+DOSAttributeLabel.Height;
   cb_DOS_read.Top   :=cb_DOS_hidden.Top;
   cb_DOS_system.Top :=cb_DOS_hidden.Top;
   cb_DOS_archive.Top:=cb_DOS_hidden.Top;
   cbpos:=EquallySpace(4); //Equally space them
   cb_DOS_hidden.Left :=cbpos*0;
   cb_DOS_read.Left   :=cbpos*1;
   cb_DOS_system.Left :=cbpos*2;
   cb_DOS_archive.Left:=cbpos*3;
   //And change the panel height to accomodate
   DOSAttrPanel.Height:=cb_DOS_hidden.Top+cb_DOS_hidden.Height;
  end;
  //Sinclair/Amstrad
  if Image.MajorFormatNumber=diSinclair then
  begin
   //Make it visible
   SinclairAttrPanel.Visible:=True;
   //Position it below the CRC32 section
   SinclairAttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
   //Position the ticks box inside
   SinclairAttributeLabel.Top:=0;
   SinclairAttributeLabel.Left:=(SinclairAttrPanel.Width-SinclairAttributeLabel.Width)div 2;
   cb_Sinclair_readonly.Top :=SinclairAttributeLabel.Top+SinclairAttributeLabel.Height;
   cb_Sinclair_system.Top   :=cb_Sinclair_readonly.Top;
   cb_Sinclair_archive.Top  :=cb_Sinclair_readonly.Top;
   cbpos:=EquallySpace(3); //Equally space them
   cb_Sinclair_readonly.Left :=cbpos*0;
   cb_Sinclair_system.Left   :=cbpos*1;
   cb_Sinclair_archive.Left  :=cbpos*2;
   //And change the panel height to accomodate
   SinclairAttrPanel.Height:=cb_Sinclair_readonly.Top+cb_Sinclair_readonly.Height;
  end;
  //Commodore 64
  if Image.MajorFormatNumber=diCommodore then
  begin
   //Make it visible
   C64AttrPanel.Visible:=True;
   //Position it below the CRC32 section
   C64AttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
   //Position the ticks box inside
   C64AttributeLabel.Top:=0;
   C64AttributeLabel.Left:=(C64AttrPanel.Width-C64AttributeLabel.Width)div 2;
   cb_C64_c.Top:=C64AttributeLabel.Top+C64AttributeLabel.Height;
   cb_C64_l.Top:=cb_C64_c.Top;
   //Equally space the boxes
   cbpos:=EquallySpace(2);
   cb_C64_c.Left:=cbpos*0;
   cb_C64_l.Left:=cbpos*1;
   //And change the panel height to accomodate
   C64AttrPanel.Height:=cb_C64_l.Top+cb_C64_l.Height;
  end;
  //Commodore Amiga
  if(Image.MajorFormatNumber=diAmiga)
  or(Image.ISOFormatNumber=diAmiga)then
  begin
   //Make it visible
   AmigaAttrPanel.Visible:=True;
   //Position it below the CRC32 section
   AmigaAttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
   //Position the ticks box inside - Owner Access
   OAAttrLabelAmiga.Top:=0;
   OAAttrLabelAmiga.Left:=(AmigaAttrPanel.Width-OAAttrLabelAmiga.Width)div 2;
   cb_Amiga_ownd.Top:=OAAttrLabelAmiga.Top+OAAttrLabelAmiga.Height;
   cb_Amiga_owne.Top:=cb_Amiga_ownd.Top;
   cb_Amiga_ownw.Top:=cb_Amiga_ownd.Top;
   cb_Amiga_ownr.Top:=cb_Amiga_ownd.Top;
   cbpos:=EquallySpace(4); //Equally space them
   cb_Amiga_ownd.Left:=cbpos*0;
   cb_Amiga_owne.Left:=cbpos*1;
   cb_Amiga_ownw.Left:=cbpos*2;
   cb_Amiga_ownr.Left:=cbpos*3;
   //Position the ticks box inside - Public Access
   PubAttrLabelAmiga.Top:=cb_Amiga_ownd.Top+cb_Amiga_ownd.Height;
   PubAttrLabelAmiga.Left:=(AmigaAttrPanel.Width-PubAttrLabelAmiga.Width)div 2;
   cb_Amiga_pubd.Top:=PubAttrLabelAmiga.Top+PubAttrLabelAmiga.Height;
   cb_Amiga_pube.Top:=cb_Amiga_pubd.Top;
   cb_Amiga_pubw.Top:=cb_Amiga_pubd.Top;
   cb_Amiga_pubr.Top:=cb_Amiga_pubd.Top;
   cbpos:=EquallySpace(4); //Equally space them
   cb_Amiga_pubd.Left:=cbpos*0;
   cb_Amiga_pube.Left:=cbpos*1;
   cb_Amiga_pubw.Left:=cbpos*2;
   cb_Amiga_pubr.Left:=cbpos*3;
   //Position the ticks box inside - Other access
   OthAttrLabelAmiga.Top:=cb_Amiga_pubd.Top+cb_Amiga_pubd.Height;
   OthAttrLabelAmiga.Left:=(AmigaAttrPanel.Width-OthAttrLabelAmiga.Width)div 2;
   cb_Amiga_othd.Top:=OthAttrLabelAmiga.Top+OthAttrLabelAmiga.Height;
   cb_Amiga_othe.Top:=cb_Amiga_othd.Top;
   cb_Amiga_othw.Top:=cb_Amiga_othd.Top;
   cb_Amiga_othr.Top:=cb_Amiga_othd.Top;
   cbpos:=EquallySpace(4); //Equally space them
   cb_Amiga_othd.Left:=cbpos*0;
   cb_Amiga_othe.Left:=cbpos*1;
   cb_Amiga_othw.Left:=cbpos*2;
   cb_Amiga_othr.Left:=cbpos*3;
   //Position the ticks box inside - Misc
   MiscAttrLabelAmiga.Top:=cb_Amiga_othd.Top+cb_Amiga_othd.Height;
   MiscAttrLabelAmiga.Left:=(AmigaAttrPanel.Width-MiscAttrLabelAmiga.Width)div 2;
   cb_Amiga_hold.Top:=MiscAttrLabelAmiga.Top+MiscAttrLabelAmiga.Height;
   cb_Amiga_scri.Top:=cb_Amiga_hold.Top;
   cb_Amiga_pure.Top:=cb_Amiga_hold.Top;
   cb_Amiga_arch.Top:=cb_Amiga_hold.Top;
   cbpos:=EquallySpace(4); //Equally space them
   cb_Amiga_hold.Left:=cbpos*0;
   cb_Amiga_scri.Left:=cbpos*1;
   cb_Amiga_pure.Left:=cbpos*2;
   cb_Amiga_arch.Left:=cbpos*3;
   //And change the panel height to accomodate
   AmigaAttrPanel.Height:=cb_Amiga_hold.Top+cb_Amiga_hold.Height;
  end;
  //ISO
  if Image.MajorFormatNumber=diISO then
  begin
   //Make it visible
   ISOAttrPanel.Visible:=True;
   //Position it below the CRC32 section, or one of the others already showing
   if ADFSAttrPanel.Visible then
    ISOAttrPanel.Top:=ADFSAttrPanel.Top+ADFSAttrPanel.Height;
   if AmigaAttrPanel.Visible then
    ISOAttrPanel.Top:=AmigaAttrPanel.Top+AmigaAttrPanel.Height;
   if(not ADFSAttrPanel.Visible)and(not AmigaAttrPanel.Visible)then
    ISOAttrPanel.Top:=CRC32Panel.Top+CRC32Panel.Height;
   //Position the ticks box inside
   ISOAttributeLabel.Top:=0;
   ISOAttributeLabel.Left:=(ISOAttrPanel.Width-ISOAttributeLabel.Width)div 2;
   cb_ISO_hidden.Top    :=ISOAttributeLabel.Top+ISOAttributeLabel.Height;
   cb_ISO_associated.Top:=cb_ISO_hidden.Top;
   cbpos:=EquallySpace(2); //Equally space them
   cb_ISO_hidden.Left    :=cbpos*0;
   cb_ISO_associated.Left:=cbpos*1;
   //And change the panel height to accomodate
   ISOAttrPanel.Height:=cb_ISO_hidden.Top+cb_ISO_hidden.Height;
  end;
 end;
 FileInfoPanel.Repaint;
end;

{------------------------------------------------------------------------------}
//find the root name of the selected partition
{------------------------------------------------------------------------------}
function TMainForm.FindPartitionRoot(filepath: String): Integer;
begin
 //By default, use the main root
 Result:=0;
 WriteToDebug('MainForm.FindPartitionRoot('+filepath+')');
 if Length(Image.Disc)>1 then //Definately another root present
 begin
  //Then extract the root part
  if(Pos('.',filepath)>0)and(Image.MajorFormatNumber<>diAcornDFS)then
   filepath:=LeftStr(filepath,Pos('.',filepath)-1);
  if(Pos('.',filepath)>3)and(Image.MajorFormatNumber=diAcornDFS)then
   filepath:=LeftStr(filepath,Pos('.',filepath,3)-1);
  //Then look to find the AFS root
  Result:=0;
  while(Image.Disc[Result].Directory<>filepath)and(Result<Length(Image.Disc)-1)do
   inc(Result);
  //Did we find the root?
  if Image.Disc[Result].Directory<>filepath then Result:=-1; //Then return -1
 end;
end;

{------------------------------------------------------------------------------}
//This is called when the selection changes on the TreeView
{------------------------------------------------------------------------------}
procedure TMainForm.DirListChange(Sender: TObject; Node: TTreeNode);
var
 entry    : Integer=0;
 dir      : Integer=0;
 dr       : Integer=0;
 rt       : Integer=0;
 ft       : Integer=0;
 ptr      : Cardinal=0;
 filename : String='';
 filetype : String='';
 location : String='';
 title    : String='';
 temp     : String='';
 multiple : Char=' ';
 R        : TRect;
 afspart  : Boolean=False;
 dospart  : Boolean=False;
begin
 WriteToDebug('MainForm.DirListChange');
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
 ExtractFile1.Enabled     :=btn_download.Enabled;
 menuExtractFile.Enabled  :=btn_download.Enabled;    
 btn_Delete.Enabled       :=DirList.SelectionCount>0;
 DeleteFile1.Enabled      :=btn_Delete.Enabled;
 menuDeleteFile.Enabled   :=btn_Delete.Enabled;
 DeleteAFile.Enabled      :=btn_Delete.Enabled;
 btn_HexDump.Enabled      :=DirList.SelectionCount=1;
 HexDump1.Enabled         :=btn_HexDump.Enabled;
 menuHexDump.Enabled      :=btn_HexDump.Enabled;     
 btn_DuplicateFile.Enabled:=DirList.SelectionCount=1;
 DuplicateFile1.Enabled   :=btn_DuplicateFile.Enabled;
 menuDuplicateFile.Enabled:=btn_DuplicateFile.Enabled;
 //Delete and Save Partition
 afspart:=((Image.MajorFormatNumber=diAcornDFS)and(Image.DoubleSided)) //DFS DS
        or((Image.MajorFormatNumber=diAcornADFS)and(Image.AFSPresent)) //ADFS/AFS
        or((Image.MajorFormatNumber=diAcornADFS)and(Image.DOSPresent));//ADFS/DOS
 //Show/Hide partition options
 btn_DeletePartition.Enabled:=(afspart)and(DirList.SelectionCount=1);
 menuDeletePartition.Enabled:=btn_DeletePartition.Enabled;
 btn_SavePartition.Enabled  :=(afspart)and(DirList.SelectionCount=1);
 menuSavePartition.Enabled  :=btn_SavePartition.Enabled;
 //Check for 8 bit ADFS or single sided DFS
 btn_AddPartition.Enabled   :=((Image.MajorFormatNumber=diAcornADFS)
                           and(Image.DirectoryType=diADFSOldDir)
                           and(Image.MapType=diADFSOldMap)
                           and(not Image.AFSPresent)
                           and(not Image.DOSPresent))
                            or((Image.MajorFormatNumber=diAcornDFS)
                           and(not Image.DoubleSided));
 menuAddPartition.Enabled   :=btn_AddPartition.Enabled;
 //Defrag (Compact) button
 btn_defrag.Enabled:=Length(Image.Disc)>0;
 menudefrag.Enabled:=btn_defrag.Enabled;
 //Change Interleave
 btn_ChangeInterleave.Enabled:=(Image.FormatNumber=diAcornADFS<<4+2) //ADFS L
                             or(Image.FormatNumber=diAcornADFS<<4+$E)//ADFS Hybrid
                             or(Image.MajorFormatNumber=diAcornFS)     //Acorn FS
                             or((Image.MajorFormatNumber=diAcornADFS)  //Acorn ADFS
                             and(Image.InterleaveMethod>0));         //with non-auto interleave
 menuChangeInterleave.Enabled:=btn_ChangeInterleave.Enabled;
 //Change the captions
 temp:='Partition';
 if Image.MajorFormatNumber=diAcornDFS then temp:='Side';
 btn_DeletePartition.Hint   :='Delete '+temp;
 menuDeletePartition.Caption:='&Delete '+temp;
 btn_SavePartition.Hint     :='Save '+temp+' As';
 menuSavePartition.Caption  :='&Save '+temp+' As...';
 //Disable the Add Files and Rename menu
 AddFile1.Enabled            :=False;
 btn_AddFiles.Enabled        :=False;
 menuAddFile.Enabled         :=False;
 RenameFile1.Enabled         :=False;
 btn_Rename.Enabled          :=False;
 menuRenameFile.Enabled      :=False;
 NewDirectory1.Enabled       :=False;
 btn_NewDirectory.Enabled    :=False;
 menuNewDir.Enabled          :=False;
 btn_AddPassword.Enabled     :=False;
 menuAddPasswordFile.Enabled :=False;
 btn_EditPassword.Enabled    :=False;
 menuEditPasswordFile.Enabled:=False;
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
  if Image.DirectoryCapable then //ADFS, Amiga, Acorn FS, Spark and DOS Plus
  begin
   NewDirectory1.Enabled   :=True;
   btn_NewDirectory.Enabled:=True;
   menuNewDir.Enabled      :=True;
  end;
  //Get the entry and dir references
  if Image.FileExists(GetFilePath(Node),dir,entry) then
  begin
   dir:=-1;
   dr:=TMyTreeNode(Node).DirRef;
   //Clear the filename variable
   filename:='';
   //If the node does not have a parent, then the dir ref is the one contained
   //in the extra info. Otherwise is -1
   if Node.Parent<>nil then
    dir  :=TMyTreeNode(Node.Parent).DirRef;
   afspart:=False;//AFS or DOS Partition?
   dospart:=False;
   if dir>=0 then
   begin
    afspart:=Image.Disc[dir].AFSPartition;
    dospart:=Image.Disc[dir].DOSPartition;
   end
   else
    if dr>=0 then
    begin
     afspart:=Image.Disc[dr].AFSPartition;
     dospart:=Image.Disc[dr].DOSPartition;
    end;
   //Enable the add/edit password buttons
   if(Image.MajorFormatNumber=diAcornFS)
   or((Image.MajorFormatNumber=diAcornADFS)and(Image.AFSPresent))then
   begin
    rt:=FindPartitionRoot(GetFilePath(DirList.Selections[0]));
    //ADFS...find the AFS parent
    if(Image.MajorFormatNumber=diAcornADFS)and(rt=0)then rt:=-1;
    if rt>=0 then
    begin
     //Does the password file exist on the root?
     if Image.FileExists(Image.Disc[rt].Directory
                        +Image.GetDirSep(Image.Disc[rt].Partition)
                        +'Passwords',ptr) then
     begin
      //Yes, so enable the edit button
      btn_EditPassword.Enabled:=True;
      menuEditPasswordFile.Enabled:=True;
     end
     else
     begin
      //No, so enable the add button
      btn_AddPassword.Enabled :=True;
      menuAddPasswordFile.Enabled:=True;
     end;
    end;
   end;
   //Then, get the filename and filetype of the file...not root directory
   if(dir>=0)and(entry>=0)then
   begin
    filename:=Image.Disc[dir].Entries[entry].Filename;
    //Attributes
    DoNotUpdate   :=True; //Make sure the event doesn't fire
    //Copy the attribute string to a temporary store
    temp:=Image.Disc[dir].Entries[entry].Attributes;
    //First three characters are ISO specific, on ISO format
    if Image.MajorFormatNumber=diISO then temp:=Copy(temp,4);
    //DFS and UEF
    if(Image.MajorFormatNumber=diAcornDFS)
    or(Image.MajorFormatNumber=diAcornUEF)
    or(Image.MajorFormatNumber=diAcornRFS)then
     //Tick/untick it
     cb_DFS_l.Ticked            :=Pos('L',temp)>0;
    //ADFS, SparkFS and ISO
    if((Image.MajorFormatNumber=diAcornADFS)
    or (Image.MajorFormatNumber=diSpark)
    or (Image.ISOFormatNumber=diAcornADFS))
    and(not afspart)and(not dospart)then
    begin
     //Tick/untick them
     cb_ADFS_ownw.Ticked        :=Pos('W',temp)>0;
     cb_ADFS_ownr.Ticked        :=Pos('R',temp)>0;
     cb_ADFS_ownl.Ticked        :=Pos('L',temp)>0;
     cb_ADFS_owne.Ticked        :=Pos('E',temp)>0;
     cb_ADFS_pubw.Ticked        :=Pos('w',temp)>0;
     cb_ADFS_pubr.Ticked        :=Pos('r',temp)>0;
     cb_ADFS_pube.Ticked        :=Pos('e',temp)>0;
     cb_ADFS_pubp.Ticked        :=Pos('P',temp)>0;
    end;
    //Acorn FS
    if(Image.MajorFormatNumber=diAcornFS)
    or((Image.MajorFormatNumber=diAcornADFS)
    and(afspart))then
    begin
     //Tick/untick them
     cb_AFS_ownw.Ticked         :=Pos('W',temp)>0;
     cb_AFS_ownr.Ticked         :=Pos('R',temp)>0;
     cb_AFS_ownl.Ticked         :=Pos('L',temp)>0;
     cb_AFS_pubw.Ticked         :=Pos('w',temp)>0;
     cb_AFS_pubr.Ticked         :=Pos('r',temp)>0;
    end;
    //DOS Plus
    if(Image.MajorFormatNumber=diDOSPlus)
    or((Image.MajorFormatNumber=diAcornADFS)
    and(dospart))then
    begin
     //Tick/untick them
     cb_DOS_hidden.Ticked       :=Pos('H',temp)>0;
     cb_DOS_read.Ticked         :=Pos('R',temp)>0;
     cb_DOS_system.Ticked       :=Pos('S',temp)>0;
     cb_DOS_archive.Ticked      :=Pos('A',temp)>0;
    end;
    //Sinclair
    if Image.MajorFormatNumber=diSinclair then
    begin
     //Tick/untick them
     cb_Sinclair_readonly.Ticked:=Pos('R',temp)>0;
     cb_Sinclair_system.Ticked  :=Pos('S',temp)>0;
     cb_Sinclair_archive.Ticked :=Pos('A',temp)>0;
    end;
    //Commodore 64
    if Image.MajorFormatNumber=diCommodore then
    begin
     //Tick/untick them
     cb_C64_l.Ticked            :=Pos('L',temp)>0;
     cb_C64_c.Ticked            :=Pos('C',temp)>0;
    end;
    //Amiga
    if(Image.MajorFormatNumber=diAmiga)
    or(Image.ISOFormatNumber=diAmiga)then
    begin
     //Tick/untick them
     cb_Amiga_ownw.Ticked       :=Pos('W',temp)=0;
     cb_Amiga_ownr.Ticked       :=Pos('R',temp)=0;
     cb_Amiga_ownd.Ticked       :=Pos('D',temp)=0;
     cb_Amiga_owne.Ticked       :=Pos('E',temp)=0;
     cb_Amiga_pubw.Ticked       :=Pos('w',temp)>0;
     cb_Amiga_pubr.Ticked       :=Pos('r',temp)>0;
     cb_Amiga_pubd.Ticked       :=Pos('d',temp)=0;
     cb_Amiga_pube.Ticked       :=Pos('e',temp)>0;
     cb_Amiga_othw.Ticked       :=Pos('i',temp)>0;
     cb_Amiga_othr.Ticked       :=Pos('a',temp)>0;
     cb_Amiga_othd.Ticked       :=Pos('l',temp)=0;
     cb_Amiga_othe.Ticked       :=Pos('x',temp)>0;
     cb_Amiga_arch.Ticked       :=Pos('A',temp)>0;
     cb_Amiga_pure.Ticked       :=Pos('P',temp)>0;
     cb_Amiga_scri.Ticked       :=Pos('S',temp)>0;
     cb_Amiga_hold.Ticked       :=Pos('H',temp)>0;
    end;
    //ISO
    if Image.MajorFormatNumber=diISO then
    begin
     temp:=LeftStr(Image.Disc[dir].Entries[entry].Attributes,3);
     //Tick/untick them
     cb_ISO_hidden.Ticked       :=Pos('H',temp)>0;
     cb_ISO_associated.Ticked   :=Pos('A',temp)>0;
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
    DeleteAFile.Enabled      :=False;
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
     if filename.IsEmpty then
      filename:=Image.Disc[dir].Directory;
     //Pick up from the Node if it is an application or not
     if(Node.ImageIndex=appicon)
     or((Node.ImageIndex>=Low(RISCOSApplications))
     and(Node.ImageIndex<=High(RISCOSApplications)))then
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
      if Image.MajorFormatNumber<>diISO then if title.IsEmpty then title:=' ';
      DirTitleLabel.Caption:='Directory Title';
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
     if Image.MajorFormatNumber<>diISO then if title.IsEmpty then title:=' ';
     if Image.MajorFormatNumber=diAcornADFS then
      DirTitleLabel.Caption:='Directory Title'
     else
      DirTitleLabel.Caption:='Disc Name';//Also see UpdateImageInfo
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
   if not TMyTreeNode(Node).IsDir then //Can only add files to a directory
   begin
    AddFile1.Enabled        :=False;
    btn_AddFiles.Enabled    :=False;
    menuAddFile.Enabled     :=False;
    NewDirectory1.Enabled   :=False;
    btn_NewDirectory.Enabled:=False;
    menuNewDir.Enabled      :=False;
    //Filetype hints
    if Image.MajorFormatNumber=diDOSPlus then
    begin //Can't edit
     img_Filetype.Hint:='';
     lb_FileType.Hint :='';
    end
    else
    begin //Can edit
     img_Filetype.Hint:='Click to edit';
     lb_FileType.Hint :='Click to edit';
    end;
   end;
   //Filename
   RemoveTopBit(filename);
   if filename.IsEmpty then filename:='unnamed';
   lb_FileName.Caption:=ReplaceStr(filename,'&','&&');
   //Filetype Image
   ft:=Node.ImageIndex;
   if(Image.MajorFormatNumber=diAcornADFS)
   or(Image.ISOFormatNumber=diAcornADFS)then
    if(ft=directory)or(ft=directory_o)then
     if filename[1]='!' then ft:=appicon;// else ft:=directory;
   //Paint the picture onto it
   R.Top:=0;
   R.Left:=0;
   R.Width:=img_FileType.Width;
   R.Height:=img_FileType.Height;
   //Draw the texture over it
   TileCanvas(img_FileType.Canvas,R);
   //Draw the filetype icon
   FileImages.StretchDraw(img_FileType.Canvas,ft,R);
   img_FileType.Tag:=ft; //To keep track of which image it is
   //Filetype text - only show for certain systems
   if(Image.MajorFormatNumber=diAcornADFS) //ADFS
   or(Image.MajorFormatNumber=diCommodore) //C64
   or(Image.MajorFormatNumber=diAmiga)     //AmigaDOS
   or(Image.MajorFormatNumber=diSpark)     //Spark
   or(Image.MajorFormatNumber=diAcornFS)   //Acorn FS
   or(Image.MajorFormatNumber=diDOSPlus)   //DOS Plya
   or(Image.ISOFormatNumber=diAcornADFS)then//ISO (ADFS)
    lb_FileType.Caption:=filetype;
   location:=''; //Default location string
   if dir>=0 then
   begin
    temp:=Image.GetParent(dir);
    //Status bar
    UpdateImageInfo(Image.Disc[dir].Entries[entry].Side);
    //CRC32
    if Image.Disc[dir].Entries[entry].DirRef=-1 then
     lb_CRC32.Caption:=Image.GetFileCRC(temp
                                       +Image.GetDirSep(Image.Disc[dir].Partition)
                                       +filename,entry);
    //Parent
    RemoveTopBit(temp);
    lb_parent.Caption:=temp;
    //Timestamp - ADFS, Spark, FileStore, Amiga and DOS only
    if  (Image.Disc[dir].Entries[entry].TimeStamp>0)
    and((Image.MajorFormatNumber=diAcornADFS)
    or  (Image.MajorFormatNumber=diSpark)
    or  (Image.MajorFormatNumber=diAcornFS)
    or  (Image.MajorFormatNumber=diAmiga)
    or  (Image.MajorFormatNumber=diDOSPlus)
    or  (Image.MajorFormatNumber=diISO))then
     lb_timestamp.Caption:=FormatDateTime(TimeDateFormat,
                                        Image.Disc[dir].Entries[entry].TimeStamp);
    if(Image.Disc[dir].Entries[entry].TimeStamp=0)
    or(Image.MajorFormatNumber=diAcornFS)then
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
    if Image.Disc[dir].Entries[entry].DirRef=-1 then
     lb_length.Caption:=ConvertToKMG(Image.Disc[dir].Entries[entry].Length)+
                     ' (0x'+IntToHex(Image.Disc[dir].Entries[entry].Length,8)+')'
    else //Number of entries in a directory
    begin
     ptr:=Length(Image.Disc[Image.Disc[dir].Entries[entry].DirRef].Entries);
     lb_length.Caption:=IntToStr(ptr)+' item';
     if ptr<>1 then lb_length.Caption:=lb_length.Caption+'s';
    end;
    //Location of object - varies between formats
    //ADFS Old map and Acorn FS - Sector is an offset
    if(Image.MapType=diADFSOldMap)
    or(Image.MajorFormatNumber=diAcornFS)then
     location:='Sector offset: 0x'
              +IntToHex(Image.Disc[dir].Entries[entry].Sector,8)+' ';
    //ADFS New map - Sector is an indirect address (fragment and sector)
    if Image.MapType=diADFSNewMap then
     location:='Indirect address: 0x'
              +IntToHex(Image.Disc[dir].Entries[entry].Sector,8)+' ';
    //DOS Plus - Sector is the starting cluster
    if(Image.MajorFormatNumber=diDOSPlus)
    or(Image.Disc[dir].DOSPartition)then
     location:='Starting Cluster: 0x'
              +IntToHex(Image.Disc[dir].Entries[entry].Sector,4);
    //Commodore formats - Sector and Track
    if Image.MajorFormatNumber=diCommodore then
     location:='Track ' +IntToStr(Image.Disc[dir].Entries[entry].Track)+' ';
    //DFS and Amiga - Sector
    if(Image.MajorFormatNumber=diAcornDFS)
    or(Image.MajorFormatNumber=diAmiga) then
     location:=location+'Sector '
              +IntToStr(Image.Disc[dir].Entries[entry].Sector)+' ';
    //DFS - indicates which side also
    if Image.MajorFormatNumber=diAcornDFS then
     location:=location+'Side '  +IntToStr(Image.Disc[dir].Entries[entry].Side);
    //CFS - indicates offset to starting block
    if(Image.MajorFormatNumber=diAcornUEF)
    or(Image.MajorFormatNumber=diAcornRFS)then
     location:='Starting Block 0x'
              +IntToHex(Image.Disc[dir].Entries[entry].Sector,8);
    //Sinclair - indicates Side, Track and Sector
    if Image.MajorFormatNumber=diSinclair then
     location:= 'Side '  +IntToStr(Image.Disc[dir].Entries[entry].Side)
              +' Track ' +IntToStr(Image.Disc[dir].Entries[entry].Track)
              +' Sector '+IntToStr(Image.Disc[dir].Entries[entry].Sector);
    //ISO - Block number
    if Image.MajorFormatNumber=diISO then
     location:='Block 0x'+IntToHex(Image.Disc[dir].Entries[entry].Sector,8);
   end;
   if dir=-1 then //Root directory
   begin
    //Status bar
    UpdateImageInfo(Image.Disc[dr].Partition);
    //Location of root - varies between formats
    location:='';
    //ADFS Old map and Acorn File Server - Sector is an offset
    if(Image.MapType=diADFSOldMap)
    or(Image.MajorFormatNumber=diAcornFS)
    or((Image.MajorFormatNumber=diDOSPlus)and(Image.MapType<>diFAT32))
    or(Image.Disc[dr].AFSPartition)
    or(Image.Disc[dr].DOSPartition)then
     location:='Sector Offset: 0x'+IntToHex(Image.Disc[dr].Sector,8);
    if(Image.MajorFormatNumber=diDOSPlus)and(Image.MapType=diFAT32)then
     location:='Starting Cluster: 0x'+IntToHex(Image.Disc[dr].Sector,8);
    //Number of entries in a directory
    ptr:=Length(Image.Disc[dr].Entries);
    lb_length.Caption:=IntToStr(ptr)+' item';
    if ptr<>1 then lb_length.Caption:=lb_length.Caption+'s';
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
end;

{------------------------------------------------------------------------------}
//Called when the TreeView is updated, and it wants to know which icon to use
{------------------------------------------------------------------------------}
procedure TMainForm.DirListGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
 WriteToDebug('MainForm.DirListGetImageIndex');
 SetImageIndex(Node,Image);
end;

{------------------------------------------------------------------------------}
//Called when the TreeView is updated, and it wants to know which icon to use
{------------------------------------------------------------------------------}
function TMainForm.GetImageIndex(Node: TTreeNode;ImageToUse: TDiscImage): Integer;
var
 ft       : Integer=0;
 i        : Integer=0;
 dir      : Integer=0;
 entry    : Integer=0;
 filetype : String='';
 afspart  : Boolean=False;
 dospart  : Boolean=False;
begin
 Result:=unknown;
 if AppIsClosing then exit;
 if(not Assigned(ImageToUse))or(ImageToUse=nil)then exit;
 if(not Assigned(ImageToUse.Disc))or(ImageToUse.Disc=nil)then exit;
 if Node=nil then exit;
 filetype:='';
 WriteToDebug('MainForm.GetImageIndex('+Node.Text+')');
 //The directory and entry references, as always
 if Image.FileExists(GetFilePath(Node),dir,entry) then
 begin
  dir  :=TMyTreeNode(Node).ParentDir;
  //AFS or DOS Partition?
  afspart:=False;
  dospart:=False;
  WriteToDebug('MainForm.GetImageIndex: Dir: '+IntToStr(dir));
  WriteToDebug('MainForm.GetImageIndex: Entry: '+IntToStr(entry));
  if(dir>=0)and(dir<Length(ImageToUse.Disc))then
  begin
   afspart:=ImageToUse.Disc[dir].AFSPartition;
   dospart:=ImageToUse.Disc[dir].DOSPartition;
   //Get the filetype to look for
   if(entry>=0)and(entry<Length(ImageToUse.Disc[dir].Entries))then
    filetype:=ImageToUse.Disc[dir].Entries[entry].ShortFiletype;
  end;
  if afspart then WriteToDebug('MainForm.GetImageIndex: AFS Partition present');
  if dospart then WriteToDebug('MainForm.GetImageIndex: DOS Partition present');
  WriteToDebug('MainForm.GetImageIndex: Looking for filetype "'+filetype+'"');
  ft:=unknown; //Default icon to use
  //Directory has a different icon
  if not TMyTreeNode(Node).IsDir then
  begin
   WriteToDebug('MainForm.GetImageIndex: Non-directory');
   //Are we ADFS, SparkFS, AFS or DOS?
   if((ImageToUse.MajorFormatNumber=diAcornADFS)
    or(ImageToUse.MajorFormatNumber=diSpark)
    or(ImageToUse.MajorFormatNumber=diAcornFS)
    or(ImageToUse.ISOFormatNumber=diAcornADFS))
   and(Length(ImageToUse.Disc)>0)and(not dospart)then
   begin
    //Default
    ft:=ROunknown; //Default icon
    //Make sure we are in range
    if dir<Length(ImageToUse.Disc)then
     if entry<Length(ImageToUse.Disc[dir].Entries)then
     begin
      //If it has a Load and/or Exec address
      if(ImageToUse.Disc[dir].Entries[entry].LoadAddr<>0)
      or(ImageToUse.Disc[dir].Entries[entry].ExecAddr<>0)then
       ft:=loadexec;
      //If it has a timestamp
      if(ImageToUse.Disc[dir].Entries[entry].TimeStamp>0)
      and(not afspart)and(not dospart)then
      begin
       i:=GetFileTypeGraphic(filetype,Low(RISCOSFileTypes),RISCOSFileTypes);
       if i<>unknown then ft:=i;
      end;
     end;
   end;
   //Is it a Commodore format?
   if(ImageToUse.MajorFormatNumber=diCommodore)
   and(Length(ImageToUse.Disc)>0)then
   begin
    //Default is a PRG file
    ft:=C64unknown;
    i:=GetFileTypeGraphic(filetype,Low(C64FileTypes),C64FileTypes);
    if i<>unknown then ft:=i;
   end;
   //DOS Plus (DOS)
   if((ImageToUse.MajorFormatNumber=diDOSPlus)
   or((ImageToUse.MajorFormatNumber=diAcornADFS)and(dospart)))
   and(Length(ImageToUse.Disc)>0)then
   begin
    i:=GetFileTypeGraphic(filetype,Low(DOSFileTypes),DOSFileTypes);
    if i<>unknown then ft:=i;
   end;
  end;
  //Diectory icons
  if TMyTreeNode(Node).IsDir then
  begin
   WriteToDebug('MainForm.GetImageIndex: Directory');
   //Different icon if it is expanded, i.e. open
   if Node.Expanded then
    ft:=directory_o
   else
    //to a closed one
    ft:=directory;
   //If RISC OS, and an application
   if(not dospart)and(not afspart)then
    if(ImageToUse.MajorFormatNumber=diAcornADFS)
    or(ImageToUse.MajorFormatNumber=diSpark)
    or(ImageToUse.ISOFormatNumber=diAcornADFS)then //ADFS, Spark and ISO only
     if(ImageToUse.DirectoryType=diADFSNewDir)
     OR(ImageToUse.DirectoryType=diADFSBigDir)
     or(ImageToUse.MajorFormatNumber=diISO)then //New or Big (or ISO)
      if Node.Text[1]='!' then
      begin
       ft:=appicon; //Default icon for application
       i:=GetFileTypeGraphic(Node.Text,Low(RISCOSApplications),RISCOSApplications);
       if i<>unknown then ft:=i;
      end;
   //If MMB
   if ImageToUse.MajorFormatNumber=diMMFS then
   begin
    ft:=mmbdisc;
    if ImageToUse.Disc[Node.Index].Locked then ft:=mmbdisclock;
    if RightStr(Node.Text,5)='empty' then ft:=mmbdiscempt;
   end;
  end;
  Result:=ft;
 end;
end;

{------------------------------------------------------------------------------}
//Called when the TreeView is updated, and it wants to know which icon to use
{------------------------------------------------------------------------------}
procedure TMainForm.SetImageIndex(Node: TTreeNode;ImageToUse: TDiscImage);
var
 ft: Integer=0;
begin
 WriteToDebug('MainForm.SetImageIndex');
 //Tell the system what the ImageList reference is
 ft:=GetImageIndex(Node,ImageToUse);
 Node.ImageIndex:=ft;
 //And ensure it stays selected
 Node.SelectedIndex:=Node.ImageIndex;
 WriteToDebug('MainForm.SetImageIndex: Image index '+IntToStr(ft));
end;

{------------------------------------------------------------------------------}
//Get a filetype graphic from a filetype string
{------------------------------------------------------------------------------}
function TMainForm.GetFileTypeGraphic(filetype: String;offset: Integer;
                                      const filetypes: array of String): Integer;
var
 i: Integer=0;
begin
 Result:=unknown;
 WriteToDebug('MainForm.GetFileTypeGraphic('+filetype+','+IntToStr(offset)+',...)');
 //Start of search - the filetype constant array
 i:=Low(filetypes)-1;
 //Just iterate through until we find a match, or get to the end
 repeat
  inc(i)
 until(UpperCase(filetype)=UpperCase(filetypes[i]))or(i=High(filetypes));
 //Do we have a match? Make a note
 if UpperCase(filetype)=UpperCase(filetypes[i])then Result:=i+offset;
end;

{------------------------------------------------------------------------------}
//Initialise Form
{------------------------------------------------------------------------------}
procedure TMainForm.FormShow(Sender: TObject);
begin
 Fguiopen:=True;
 Scaling;
 //Initial width and height of form
 Width:=751;
 Height:=515;
 //Enable or disable buttons
 DisableControls;
 //Reset the file details panel
 ResetFileFields;
 //Clear the search fields
 SearchForm.ResetSearchFields;
 //Clear the status bar
 UpdateImageInfo;
 //Show/Hide the various elements
 //File Details
 menuViewFileDetails.Checked:=ViewOptions AND $01=$01;
 FileInfoPanel.Visible:=menuViewFileDetails.Checked;
 ToolSplitter.Visible:=FileInfoPanel.Visible;
 //Image Details
 menuViewStatus.Checked:=ViewOptions AND $02=$02;
 ImageDetails.Visible:=menuViewStatus.Checked;
 //Image Toolbar
 menuImage.Checked:=ViewOptions AND $10=$10;
 ImageToolBarPage.TabVisible:=menuImage.Checked;
 //Files Toolbar
 menuFiles.Checked:=ViewOptions AND $20=$20;
 FilesToolBarPage.TabVisible:=menuFiles.Checked;
 //Partition Toolbar
 menuPartition.Checked:=ViewOptions AND $40=$40;
 PartitionToolBarPage.TabVisible:=menuPartition.Checked;
 //Tools Toolbar
 menuTools.Checked:=ViewOptions AND $80=$80;
 ToolsToolBarPage.TabVisible:=menuTools.Checked;
 //Reset the tracking variables
 PathBeforeEdit           :='';
 NameBeforeEdit           :='';
 //Reset the delay timer
 progsleep                :=False;
 if not Image.Filename.IsEmpty then ShowNewImage(Image.Filename);
 //Create the dialogue boxes
 CreateFileTypeDialogue;
 //Create the copy and paste shortcuts
 if TargetOS='Darwin' then
 begin
  CopyToClipboard.ShortCut   :=$1000 OR ord('C'); //Meta+C (Mac)
  PasteFromClipboard.ShortCut:=$1000 OR ord('V'); //Meta+V (Mac)
 end else
 begin
  CopyToClipboard.ShortCut   :=$4000 OR ord('C'); //Ctrl+C (WindowsLinux)
  PasteFromClipboard.ShortCut:=$4000 OR ord('V'); //Ctrl+V (Windows/Linux)
 end;
 //Meta/Cmd = $1000, Shift = $2000, Ctrl = $4000, Alt = $8000
end;

{------------------------------------------------------------------------------}
//Disable/Enable all the controls for initialisation
{------------------------------------------------------------------------------}
procedure TMainForm.DisableControls;
begin
 WriteToDebug('MainForm.DisableControls');
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
 btn_ShowReport.Enabled   :=False;
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
 menuShowReport.Enabled   :=False;
 //Shortcuts
 DeleteAFile.Enabled      :=False;
 //Close the search window
 SearchForm.Close;
 //Disable the directory view
 DirList.Enabled          :=False;
 //Reset the changed variable
// HasChanged               :=False;
end;

{------------------------------------------------------------------------------}
//Rescale all the components
{------------------------------------------------------------------------------}
procedure TMainForm.Scaling;
var
 ppi  : Integer=0;
 ratio: Real=0;
begin
 WriteToDebug('MainForm.Scaling');
 //Scaling for large DPI
 ppi:=Screen.PixelsPerInch;//Can use TMonitor.PixelsPerInch to scale to a big monitor
 ratio:=ppi/DesignedDPI;
 //Form Size
 Width:=Round(745*ratio);
 Height:=Round(599*ratio);
 //Status Bar
 //ImageDetails.Height            :=Round( 18*ratio);
 ImageDetails.Panels[0].Width   :=ImageDetails.Height;//Round( 20*ratio);
 ImageDetails.Panels[1].Width   :=Round(175*ratio);
 ImageDetails.Panels[2].Width   :=Round(150*ratio);
 ImageDetails.Panels[3].Width   :=Round(200*ratio);
 ImageDetails.Panels[4].Width   :=Round(200*ratio);
 ImageDetails.Panels[5].Width   :=Round(100*ratio);
 ImageDetails.Panels[6].Width   :=Round(100*ratio);
 ImageDetails.Panels[7].Width   :=Round(130*ratio);
 ImageDetails.Panels[8].Width   :=Round(130*ratio);
 ImageDetails.Panels[9].Width   :=Round( 50*ratio);
 //Directory Listing
 DirList.ImagesWidth            :=Round( 17*ratio);
 //Toolbars
 ImageToolBar.ImagesWidth       :=Round( 32*ratio);
 ImageToolBar.ButtonHeight      :=Round( 32*ratio);
 ImageToolBar.ButtonWidth       :=Round( 32*ratio);
 FilesToolBar.ImagesWidth       :=Round( 32*ratio);
 FilesToolBar.ButtonHeight      :=Round( 32*ratio);
 FilesToolBar.ButtonWidth       :=Round( 32*ratio);
 PartitionToolBar.ImagesWidth   :=Round( 32*ratio);
 PartitionToolBar.ButtonHeight  :=Round( 32*ratio);
 PartitionToolBar.ButtonWidth   :=Round( 32*ratio);
 ToolsToolBar.ImagesWidth       :=Round( 32*ratio);
 ToolsToolBar.ButtonHeight      :=Round( 32*ratio);
 ToolsToolBar.ButtonWidth       :=Round( 32*ratio);
 //File Info Panel
 FileInfoPanel.Width            :=Round(355*ratio);
end;

{------------------------------------------------------------------------------}
//This is called when the form is created - i.e. when the application is created
{------------------------------------------------------------------------------}
procedure TMainForm.FormCreate(Sender: TObject);
 function CreateTickBox(LCaption: String; LParent: TPanel): TRISCOSTickBox;
 begin
  Result:=TRISCOSTickBox.Create(LParent as TComponent);
  Result.Parent:=LParent as TWinControl;
  Result.Visible:=True;
  Result.Font.Name:='Courier New';
  Result.Font.Style:=[fsBold];
  Result.Caption:=LCaption;
  Result.OnClick:=@AttributeChangeClick;
 end;
begin
 AppIsClosing:=False;
 Fguiopen:=False;
 //Default Toolbar
 ToolBarContainer.ActivePage:=ImageToolBarPage;
 //Create the attribute panel tick boxes (ADFS)
 cb_ADFS_ownw        :=CreateTickBox('Write'     ,ADFSAttrPanel);
 cb_ADFS_ownr        :=CreateTickBox('Read'      ,ADFSAttrPanel);
 cb_ADFS_ownl        :=CreateTickBox('Locked'    ,ADFSAttrPanel);
 cb_ADFS_owne        :=CreateTickBox('Execute'   ,ADFSAttrPanel);
 cb_ADFS_pubw        :=CreateTickBox('Write'     ,ADFSAttrPanel);
 cb_ADFS_pubr        :=CreateTickBox('Read'      ,ADFSAttrPanel);
 cb_ADFS_pubp        :=CreateTickBox('Private'   ,ADFSAttrPanel);
 cb_ADFS_pube        :=CreateTickBox('Execute'   ,ADFSAttrPanel);
 //Create the attribute panel tick boxes (DFS)
 cb_DFS_l            :=CreateTickBox('Locked'    ,DFSAttrPanel);
 //Create the attribute panel tick boxes (C64)
 cb_C64_c            :=CreateTickBox('Closed'    ,C64AttrPanel);
 cb_C64_l            :=CreateTickBox('Locked'    ,C64AttrPanel);
 //Create the attribute panel tick boxes (AFS)
 cb_AFS_ownw         :=CreateTickBox('Write'     ,AFSAttrPanel);
 cb_AFS_ownr         :=CreateTickBox('Read'      ,AFSAttrPanel);
 cb_AFS_ownl         :=CreateTickBox('Locked'    ,AFSAttrPanel);
 cb_AFS_pubr         :=CreateTickBox('Read'      ,AFSAttrPanel);
 cb_AFS_pubw         :=CreateTickBox('Write'     ,AFSAttrPanel);
 //Create the attribute panel tick boxes (DOS)
 cb_DOS_hidden       :=CreateTickBox('Hidden'    ,DOSAttrPanel);
 cb_DOS_read         :=CreateTickBox('Read'      ,DOSAttrPanel);
 cb_DOS_system       :=CreateTickBox('System'    ,DOSAttrPanel);
 cb_DOS_archive      :=CreateTickBox('Archive'   ,DOSAttrPanel);
 //Create the attribute panel tick boxes (Sinclair)
 cb_Sinclair_readonly:=CreateTickBox('Read Only' ,SinclairAttrPanel);
 cb_Sinclair_system  :=CreateTickBox('System'    ,SinclairAttrPanel);
 cb_Sinclair_archive :=CreateTickBox('Archive'   ,SinclairAttrPanel);
 //Create the attribute panel tick boxes (Amiga)
 cb_Amiga_ownw       :=CreateTickBox('Write'     ,AmigaAttrPanel);
 cb_Amiga_ownr       :=CreateTickBox('Read'      ,AmigaAttrPanel);
 cb_Amiga_ownd       :=CreateTickBox('Delete'    ,AmigaAttrPanel);
 cb_Amiga_owne       :=CreateTickBox('Execute'   ,AmigaAttrPanel);
 cb_Amiga_pubw       :=CreateTickBox('Write'     ,AmigaAttrPanel);
 cb_Amiga_pubr       :=CreateTickBox('Read'      ,AmigaAttrPanel);
 cb_Amiga_pubd       :=CreateTickBox('Delete'    ,AmigaAttrPanel);
 cb_Amiga_pube       :=CreateTickBox('Execute'   ,AmigaAttrPanel);
 cb_Amiga_othw       :=CreateTickBox('Write'     ,AmigaAttrPanel);
 cb_Amiga_othr       :=CreateTickBox('Read'      ,AmigaAttrPanel);
 cb_Amiga_othd       :=CreateTickBox('Delete'    ,AmigaAttrPanel);
 cb_Amiga_othe       :=CreateTickBox('Execute'   ,AmigaAttrPanel);
 cb_Amiga_hold       :=CreateTickBox('Hold'      ,AmigaAttrPanel);
 cb_Amiga_scri       :=CreateTickBox('Script'    ,AmigaAttrPanel);
 cb_Amiga_pure       :=CreateTickBox('Pure'      ,AmigaAttrPanel);
 cb_Amiga_arch       :=CreateTickBox('Archived'  ,AmigaAttrPanel);
 //Create the attribute panel tick boxes (ISO)
 cb_ISO_hidden       :=CreateTickBox('Hidden'    ,ISOAttrPanel);
 cb_ISO_associated   :=CreateTickBox('Associated',ISOAttrPanel);
 //Platform Details
 platform:='OS?';
 arch:='CPU?';
 //Platform details - OS
 if TargetOS='Darwin'   then {%H-}platform:= 'macOS';   //Apple Mac OS X
 if(TargetOS='Win64')
 or(TargetOS='Win32')   then {%H-}platform:= 'Windows'; //Microsoft Windows
 if TargetOS='Linux'    then {%H-}platform:= 'Linux';   //Linux
 //Platform details - CPU
 if TargetCPU='aarch64' then {%H-}arch    := 'ARM 64 bit';
 if TargetCPU='arm'     then {%H-}arch    := 'ARM 32 bit';
 if TargetCPU='i386'    then {%H-}arch    := 'Intel 32 bit';
 if TargetCPU='x86_64'  then {%H-}arch    := 'Intel 64 bit';
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
 //Initiate the Registry
 DIMReg:=TGJHRegistry.Create('\Software\GJH Software\Disc Image Manager');
 //Texture style - get from the registry
 TextureType               :=DIMReg.GetRegValI('Texture',1);
 //Window style - get from the registry
 Fstyling                  :=DIMReg.GetRegValI('WindowStyle',RISCOSStyle);
 SetNativeControls; //And set the controls on this form
 //ADFS L Interleaved type - get from the registry
 ADFSInterleave            :=DIMReg.GetRegValI('ADFS_L_Interleave',0);
 Image.InterleaveMethod    :=ADFSInterleave;
 //Treat Spark as FS?
 SparkIsFS                 :=DIMReg.GetRegValB('Spark_Is_FS',True);
 Image.SparkAsFS           :=SparkIsFS;
 //Threshold of when to bypass the GUI (during import) - get from registry
 bypassGUIThres            :={DIMReg.GetRegValI('bypass_GUI_Threshold',}100;//);
 //Create INF Files?
 DoCreateINF               :=DIMReg.GetRegValB('CreateINF',True);
 //Add Implied Attributes for DFS/CFS/RFS
 AddImpliedAttributes      :=DIMReg.GetRegValB('AddImpliedAttributes',True);
 Image.AddImpliedAttributes:=AddImpliedAttributes;
 //Hide Commodore DEL files
 DoHideDEL                 :=DIMReg.GetRegValB('Hide_CDR_DEL',False);
 //Allow DFS images with zero sectors
 FDFSZeroSecs              :=DIMReg.GetRegValB('DFS_Zero_Sectors',False);
 Image.AllowDFSZeroSectors :=FDFSZeroSecs;
 //Check for files going over the DFS disc edge
 FDFSBeyondEdge            :=DIMReg.GetRegValB('DFS_Beyond_Edge',False);
 Image.DFSBeyondEdge       :=FDFSBeyondEdge;
 //Check for blank filenames in DFS
 FDFSAllowBlank            :=DIMReg.GetRegValB('DFS_Allow_Blanks',False);
 Image.DFSAllowBlanks      :=FDFSAllowBlank;
 //Compress UEF Files on save
 FUEFCompress              :=DIMReg.GetRegValB('UEF_Compress',True);
 //Scan all sub directories on opening
 FScanSubDirs              :=DIMReg.GetRegValB('Scan_SubDirs',True);
 Image.ScanSubDirs         :=FScanSubDirs;
 //Open DOS Partitions on ADFS
 FOpenDOS                  :=DIMReg.GetRegValB('Open_DOS',True);
 Image.OpenDOSPartitions   :=FOpenDOS;
 //Create *.dsc files with ADFS Hard Drives
 FCreateDSC                :=DIMReg.GetRegValB('Create_DSC',False);
 Image.CreateDSC           :=FCreateDSC;
 //View menu options
 ViewOptions               :=DIMReg.GetRegValI('View_Options',$FFFF);
 //Toolbar order - this doesn't work currently
{ ToolBarContainer.Bands.Items[0]:=DIMReg.GetRegValS('ToolBar0','ImageToolBar');
 ToolBarContainer.Bands.Items[1].Text:=DIMReg.GetRegValS('ToolBar1','FilesToolBar');
 ToolBarContainer.Bands.Items[2].Text:=DIMReg.GetRegValS('ToolBar2','PartitionToolBar');
 ToolBarContainer.Bands.Items[3].Text:=DIMReg.GetRegValS('ToolBar3','ToolsToolBar');}
 //Produce log files for debugging
 Fdebug                    :=DIMReg.GetRegValB('Debug_Mode',False);
 debuglogfile              :=GetTempDir+'DIM_LogFile.txt';
 //Write some debugging info
 WriteToDebug('Application Started.');
 WriteToDebug('Version '+ApplicationVersion+' '+platform+' ('+arch+')');
 WriteToDebug('Screen DPI: '+IntToStr(Screen.PixelsPerInch));
 WriteToDebug('If you are experiencing any issues, please email this to '+
              'gerald@hollypops.co.uk with a description of your issue.');
 //Reset the changed variable
 HasChanged:=False;
end;

{------------------------------------------------------------------------------}
//Set the custom controls to either Native OS or RISC OS style
{------------------------------------------------------------------------------}
procedure TMainForm.SetNativeControls;
begin
 cb_C64_c.NativeOS            :=Fstyling=NativeStyle;
 cb_C64_l.NativeOS            :=Fstyling=NativeStyle;
 cb_DFS_l.NativeOS            :=Fstyling=NativeStyle;
 cb_ADFS_pubp.NativeOS        :=Fstyling=NativeStyle;
 cb_ADFS_ownw.NativeOS        :=Fstyling=NativeStyle;
 cb_ADFS_ownr.NativeOS        :=Fstyling=NativeStyle;
 cb_ADFS_ownl.NativeOS        :=Fstyling=NativeStyle;
 cb_ADFS_owne.NativeOS        :=Fstyling=NativeStyle;
 cb_ADFS_pubr.NativeOS        :=Fstyling=NativeStyle;
 cb_ADFS_pubw.NativeOS        :=Fstyling=NativeStyle;
 cb_ADFS_pube.NativeOS        :=Fstyling=NativeStyle;
 cb_AFS_ownl.NativeOS         :=Fstyling=NativeStyle;
 cb_AFS_ownr.NativeOS         :=Fstyling=NativeStyle;
 cb_AFS_ownw.NativeOS         :=Fstyling=NativeStyle;
 cb_AFS_pubr.NativeOS         :=Fstyling=NativeStyle;
 cb_AFS_pubw.NativeOS         :=Fstyling=NativeStyle;
 cb_DOS_system.NativeOS       :=Fstyling=NativeStyle;
 cb_DOS_read.NativeOS         :=Fstyling=NativeStyle;
 cb_DOS_hidden.NativeOS       :=Fstyling=NativeStyle;
 cb_DOS_archive.NativeOS      :=Fstyling=NativeStyle;
 cb_Sinclair_system.NativeOS  :=Fstyling=NativeStyle;
 cb_Sinclair_readonly.NativeOS:=Fstyling=NativeStyle;
 cb_Sinclair_archive.NativeOS :=Fstyling=NativeStyle;
 cb_ISO_hidden.NativeOS       :=Fstyling=NativeStyle;
 cb_ISO_associated.NativeOS   :=Fstyling=NativeStyle;
 cb_Amiga_othd.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_arch.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_othe.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_pure.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_hold.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_scri.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_ownr.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_othr.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_ownw.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_owne.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_ownd.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_othw.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_pubw.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_pubr.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_pubd.NativeOS       :=Fstyling=NativeStyle;
 cb_Amiga_pube.NativeOS       :=Fstyling=NativeStyle;
end;

{------------------------------------------------------------------------------}
//Select a node
{------------------------------------------------------------------------------}
procedure TMainForm.SelectNode(filename: String;casesens:Boolean=True);
var
 Node   : TTreeNode=nil;
begin
 WriteToDebug('MainForm.SelectNode('+filename+')');
 //Unselect everything
 DirList.ClearSelection;
 //Find the node
 Node:=FindNode(filename,casesens);
 //Did it find anything, then select it
 if Node<>nil then Node.Selected:=True;
 //We'll need to give the tree focus, so it shows up
 DirList.SetFocus;
end;

{------------------------------------------------------------------------------}
//Find a node
{------------------------------------------------------------------------------}
function TMainForm.FindNode(filename: String;casesens:Boolean=True): TTreeNode;
var
 i      : Integer=0;
 found  : Integer=-1;
 dirname: String='';
 Node   : TTreeNode=nil;
 Ldirsep: Char=' ';
begin
 WriteToDebug('MainForm.FindNode('+filename+')');
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
   if TMyTreeNode(Node).ParentDir>=0 then
    Ldirsep:=Image.GetDirSep(Image.Disc[TMyTreeNode(Node).ParentDir].Partition)
   else
    Ldirsep:=Image.DirSep;
   Node:=Node.Parent;
   dirname:=Node.Text+Ldirsep+dirname;
  end;
  //If it matches, then take a note
  if(dirname=filename)and(casesens)then found:=i;
  if(LowerCase(dirname)=LowerCase(filename))and(not casesens)then found:=i;
  inc(i);
 end;
 if found>=0 then //and select it
  Result:=DirList.Items[found]
 else
  Result:=nil;
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
 ext      : String='';
 filename : String='';
 pathname : String='';
 index    : Integer=0;
begin
 WriteToDebug('MainForm.btn_SaveImageClick');
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
  Image.SaveToFile(SaveImage.FileName,FUEFCompress);
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
 WriteToDebug('MainForm.QueryUnsaved');
 if HasChanged then
 begin
  WriteToDebug('Close unsaved file requested');
  Result:=AskConfirm('You have unsaved changes. Do you wish to continue?',
                            ['Yes','No'])=mrOK;
 end;
end;

{------------------------------------------------------------------------------}
//Application is asking whether it can close
{------------------------------------------------------------------------------}
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 WriteToDebug('Application close requested');
 CanClose:=QueryUnsaved;
 AppIsClosing:=CanClose;
end;

{------------------------------------------------------------------------------}
//Open an image, add a new file, or import another image to the disc image
{------------------------------------------------------------------------------}
procedure TMainForm.FormDropFiles(Sender: TObject;
 const FileNames: array of String);
var
 msg       : String='';
 fmt       : String='';
 FileName  : String='';
 NewImage  : TDiscImage=nil;
 open      : Byte=0;
 SparkFile : TSpark=nil;
 sparksize : Cardinal=0;
 isspark   : Boolean=False;
 confirm   : TModalResult=mrCancel;
 ListOfFile: array of String=nil;
begin
 WriteToDebug('MainForm.FormDropFiles');
 //Create a new DiscImage instance
 NewImage:=TDiscImage.Create;
 NewImage.InterleaveMethod    :=ADFSInterleave;
 NewImage.SparkAsFS           :=SparkIsFS;
 NewImage.AllowDFSZeroSectors :=FDFSZeroSecs;
 NewImage.DFSBeyondEdge       :=FDFSBeyondEdge;
 NewImage.DFSAllowBlanks      :=FDFSAllowBlank;
 NewImage.ScanSubDirs         :=True;
 NewImage.CreateDSC           :=FCreateDSC;
 NewImage.AddImpliedAttributes:=AddImpliedAttributes;
 //Extract any *.inf files, except for DOS
 SetLength(ListOfFile,0);
 for FileName in FileNames do
  if(LowerCase(RightStr(FileName,4))<>'.inf')
  or(Image.MajorFormatNumber=diDOSPlus)then
   Insert(FileName,ListOfFile,Length(ListOfFile));
 //And then iterate through them and add them
 for FileName in ListOfFile do
 begin
  //If it is not a directory
  if not DirectoryExists(Filename) then
  begin
   //Open or add ($00=ignore, $01=open, $02=add, $03=ask)
   open:=$00;
   if Length(ListOfFile)=1 then //If more than one, then just add, otherwise explore other options
   begin
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
    if not Image.Filename.IsEmpty then open:=open OR $02; //Something is open
    if((NewImage.FormatNumber=diAcornDFS<<4+0)
    or (NewImage.FormatNumber=diAcornDFS<<4+2))
    and((Image.FormatNumber=diAcornDFS<<4+0)
    or  (Image.FormatNumber=diAcornDFS<<4+2))then //Loading a DFS SS while a SS is open
     open:=$04; //Might want to convert an SS to a DS
    //Go through the different states
    if open=$04 then //Convert DFS SS to DFS DS
    begin
     msg:='The new image is a '+NewImage.FormatString+', '
         +'and the open image is a '+Image.FormatString+'.'#13#10;
     msg:=msg+'Would you like to open this as an image, '
             +'or convert the open one by adding this as a second side/partition?';
     confirm:=AskConfirm(msg,['Open','Cancel','Convert']);
     if confirm=mrOK     then open:=$01; //Open
     if confirm=mrCancel then open:=$05; //Cancel
     if confirm=mrIgnore then open:=$04; //Convert
    end;
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
     confirm:=AskConfirm(msg,['Open','Add','Import']);
     if confirm=mrOK     then open:=$01;
     if confirm=mrCancel then open:=$02;
     if confirm=mrIgnore then open:=$03;
    end;
    if open=$02 then //Add file
    begin
     //Is this 32bit ADFS?
     if (Image.DirectoryType>0)
     and(Image.MajorFormatNumber=diAcornADFS)
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
                     ['Yes','No, just add'])=mrOK then
       begin
        open:=$04; //Yes, so reset the open flag so it doesn't add it
        AddSparkToImage(FileName);
       end;
      end;
     end;
     if open=$02 then //No, then just add it
      AddFileToImage(FileName);
    end;
   end
   else
   begin
    open:=$02;
    AddFileToImage(FileName);
   end;
   if open=$01 then //Open image
    if QueryUnsaved then OpenImage(FileName);
   if open=$03 then //Import contents
    ImportFiles(NewImage);
   if open=$04 then //Convert DFS SS into DFS DS or AFS L3 partition onto ADFS
    if Image.AddPartition(FileName) then
    begin
     //Update the changed flag
     HasChanged:=True;
     //And the directory display
     ShowNewImage(Image.Filename);
     UpdateImageInfo;
    end else ReportError('Failed to add a side/partition to the current image.');
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
 ProgressForm.Hide;
end;

{------------------------------------------------------------------------------}
//Import an image's contents into the current image
{------------------------------------------------------------------------------}
function TMainForm.ImportFiles(NewImage: TDiscImage;Dialogue: Boolean=True;
                                                 Errors: Boolean=True): Integer;
var
 Node      : TTreeNode=nil;
 newentry  : TDirEntry=();
// oldroot   : String='$';
 rootname  : String='$';
 method    : String='Moving';
 temp      : String='';
 tempattr  : String='';
 curformat : Byte=0;
 newformat : Byte=0;
 side      : Byte=0;
 sidecount : Byte=0;
 dir       : Integer=0;
 dr        : Integer=0;
 entry     : Integer=0;
 index     : Integer=0;
 MaxDirEnt : Integer=0;
 NumFiles  : Integer=0;
 DirEntry  : Cardinal=0;
 buffer    : TDIByteArray=nil;
 ok        : Boolean=True;
begin
 Result:=0;
 ResetDirEntry(newentry);
 if Fguiopen then WriteToDebug('MainForm.ImportFiles');
 //Read in the catalogue of the new image, if there is none
 if Length(NewImage.Disc)=0 then
 begin
  //Show a progress message
  if Fguiopen then
  begin
   Image.ProgressIndicator:=@UpdateProgress;
   ProgressForm.Show;
   //Process the messages to close the file dialogue box
   Application.ProcessMessages;
  end;
  //Read the catalogue
  NewImage.ReadImage;
  if Fguiopen then
  begin
   ProgressForm.Hide;
   Application.ProcessMessages;
  end;
 end;
 ok:=True;
 if Fguiopen then
 begin
  //Show the contents in the import selector box
  ImportSelectorForm.Show; //The TTreeView needs to be visible
  //Copy the DiscImage across
  ImportSelectorForm.FImage:=NewImage;
  //Add the disc image to the tree
  AddImageToTree(ImportSelectorForm.ImportDirList,NewImage);
  //Which side to tick
  side:=0;
  if DirList.SelectionCount>0 then
  begin
   entry:=DirList.Selections[0].Index;
   if DirList.Selections[0].Parent<>nil then
    dir:=TMyTreeNode(DirList.Selections[0].Parent).DirRef
   else
    dir:=-1;
   dr:=TMyTreeNode(DirList.Selections[0]).DirRef;
   if dir>=0 then side:=Image.Disc[dir].Entries[entry].Side
   else side:=Image.Disc[dr].Partition;
  end;
  //Tick them all (just by ticking the roots...i.e. those without parents)
  if ImportSelectorForm.ImportDirList.Items.Count>0 then
  begin
   sidecount:=0;
   for index:=0 to ImportSelectorForm.ImportDirList.Items.Count-1 do
    if ImportSelectorForm.ImportDirList.Items[index].Parent=nil then
    begin
     if sidecount=side then //Only tick the root on the first side/partition
     begin
      ImportSelectorForm.TickNode(ImportSelectorForm.ImportDirList.Items[index],
                                  True);
      ImportSelectorForm.ImportDirList.Items[index].Expand(False);
     end
     else                   //And untick the other side/partition
     begin
      ImportSelectorForm.TickNode(ImportSelectorForm.ImportDirList.Items[index],
                                  False);
      ImportSelectorForm.ImportDirList.Items[index].Collapse(True);
     end;
     inc(sidecount);
    end;
  end;
  //Now we need to hide the form so we can show it again, but modally.
  ImportSelectorForm.Hide;
  //And, finally, modally
  if Dialogue then ok:=ImportSelectorForm.ShowModal=mrOK;
 end;
 if ok then //And start the import
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
     begin
      //When this is not open in a GUI, assume everything is ticked
      //This function is used when defragging an image
      ok:=not Fguiopen;
      if Fguiopen then
       if ImportSelectorForm.IsNodeTicked(dir,entry) then ok:=True;
      if ok then
      begin
       inc(index);
       inc(NumFiles);
      end;
     end;
    //Update the Maximum Directory Entries, per directory
    if index>MaxDirEnt then MaxDirEnt:=index;
   end;
  //Check the open image is suitable for the incoming selected files
  ok:=True;
  //If it is not, ask the user if they wish to continue
  if  ((Image.MajorFormatNumber=diAcornADFS)                     //Check ADFS
  and(((MaxDirEnt>47)and(Image.DirectoryType=diADFSOldDir))
    or((MaxDirEnt>77)and(Image.DirectoryType=diADFSNewDir))))
  or  ((Image.MajorFormatNumber=diAcornDFS)and(NumFiles>31))     //Check DFS
  or  ((Image.MajorFormatNumber=diCommodore)and(NumFiles>144))   //Check Commodore
  then
   if(Dialogue)and(Fguiopen)then ok:=AskConfirm(
           'The current open image is not suitable for this archive. '
          +'Would you like to continue regardless?',
          ['Yes','No'])=mrOK;
  //If all OK, then continue
  if ok then
  begin
   rootname:=Image.Disc[0].Directory;
   if Fguiopen then
   begin
    //We don't need progress update from the class as we'll produce our own
    NewImage.ProgressIndicator:=nil;
    Image.ProgressIndicator:=@UpdateProgress;
    //Show the progress form again
    ProgressForm.Show;
    Application.ProcessMessages;
    //Importing or moving (this is also used by defrag)
    method:='Importing';
    if not Dialogue then method:='Moving';
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
    end;
   end;
   curformat:=Image.MajorFormatNumber;   //Format of the current open image
   newformat:=NewImage.MajorFormatNumber;//Format of the importing image
//   oldroot  :=NewImage.RootName;
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
        UpdateProgress(method+' '+newentry.Parent
                      +NewImage.GetDirSep(NewImage.Disc[dir].Partition)
                      +newentry.Filename);
        //Validate the filename, as it could be different across file systems
        if(newformat<>diAcornDFS)and(newformat<>curformat)then
         WinToBBC(newentry.Filename);//Unless the systems are the same, but not DFS
        //DFS and C64 don't have directories, so the parent is the selected node
        if(newformat=diCommodore)or(newformat=diAcornDFS)then
         newentry.Parent:=rootname;
        //If going to DFS or C64
        if(curformat=diAcornDFS)or(curformat=diCommodore)then
        begin
         //If coming from ADFS, AFS or Amiga, and is inside a directory,
         //add the first letter of this.
         //if((newformat=diAcornADFS)or(newformat=diAmiga)or(newformat=diAcornFS))
         if(NewImage.DirectoryCapable)
         and(newentry.Parent<>NewImage.Disc[0].Directory)then
         begin
          index:=Length(newentry.Parent);
          while(newentry.Parent[index]<>NewImage.GetDirSep(NewImage.Disc[index].Partition))
            and(index>1)do dec(index);
          if index=Length(newentry.Parent) then index:=0;
          newentry.Filename:=newentry.Parent[index+1]+'.'+newentry.Filename;
         end;
         //Make the root the parent
         newentry.Parent:=rootname;
        end;
        //Coming from DFS and first letter holds the directory?
        //And going to directory capable system?
        if (newformat=diAcornDFS)and(newentry.Filename[2]='.')
        and(Image.DirectoryCapable) then
        begin
         //Then create the directory
         if Fguiopen then SelectNode(rootname);//First, get the root
         //Get the temporary filename
         temp:=newentry.Filename[1];
         //Does it exist already?
         if not Image.FileExists(rootname+'.'+temp,DirEntry) then
         begin
          //New attributes
          tempattr:='LR';
          //Create it
          if Fguiopen then CreateDirectory(temp,tempattr)
          else Image.CreateDirectory(temp,rootname,tempattr);
         end;
         //Move the Parent
         newentry.Parent:=rootname+Image.DirSep+temp;
         //And rename the file
         newentry.Filename:=Copy(newentry.Filename,3,Length(newentry.Filename));
        end;
        //Going to ADFS or !Spark, from another system, ensure it has 'WR' attributes
        if ((curformat= diAcornADFS) or(curformat= diSpark))
        and((newformat<>diAcornADFS)and(newformat<>diSpark))then
        begin
         if Pos('W',newentry.Attributes)=0 then
          newentry.Attributes:=newentry.Attributes+'W';
         if Pos('R',newentry.Attributes)=0 then
          newentry.Attributes:=newentry.Attributes+'R';
        end;
        //Convert the parent name to the new path system
        newentry.Parent:=StringReplace(newentry.Parent
                                      ,NewImage.DirSep
                                      ,Image.DirSep
                                      ,[rfReplaceAll,rfIgnoreCase]);
        newentry.Parent:=StringReplace(newentry.Parent
                                      ,NewImage.RootName
                                      ,rootname
                                      ,[rfReplaceAll,rfIgnoreCase]);
        ok:=True;
        if Fguiopen then
        begin
         //Select the parent directory
         SelectNode(newentry.Parent);
         if DirList.SelectionCount=0 then
         begin
          if Errors then
           ReportError('Cannot find directory "'+newentry.Parent
                       +'" when adding "'+newentry.Filename+'"');
          inc(Result);
          ok:=False;
         end;
        end;
        if ok then
        begin
         //Make sure it has been read in
         if Fguiopen then
          if not TMyTreeNode(DirList.Selected).BeenRead then
           ReadInDirectory(DirList.Selected);
         //Is it a directory we're adding?
         if(newentry.DirRef>=0)and(Image.DirectoryCapable)then
          if newentry.Filename<>rootname then //Create the directory
          begin
           newentry.Attributes:='DLR';
           if Fguiopen then
            CreateDirectory(newentry.Filename,newentry.Attributes)
           else
            Image.CreateDirectory(newentry.Filename,
                                  newentry.Parent,
                                  newentry.Attributes);
          end;
         //Is it a file
         if newentry.DirRef=-1 then
         begin
          //Read the file in
          if NewImage.ExtractFile(NewImage.GetParent(dir)
                               +NewImage.GetDirSep(NewImage.Disc[dir].Partition)
                               +NewImage.Disc[dir].Entries[entry].Filename,
                                buffer,entry) then
          begin
           //Write it out to the current image
           index:=Image.WriteFile(newentry,buffer);
           //Then add it to the tree, if successful
           if index>=0 then
            AddFileToTree(DirList.Selected,newentry.Filename,index,False,
                          DirList,False)
           else //Failed to write the file
           begin
            if Errors then
             ReportError('Failed when '+method+' '+newentry.Parent+Image.DirSep
                                                  +newentry.Filename
                                                  +' : '
                                                  +AddFileErrorToText(-index));
            inc(Result);
           end;
          end
          else //Failed to read the file
          begin
           if Errors then
            ReportError('Failed to read '+NewImage.GetParent(dir)
                               +NewImage.GetDirSep(NewImage.Disc[dir].Partition)
                               +NewImage.Disc[dir].Entries[entry].Filename);
           inc(Result);
          end;
         end;
        end;
       end;
   if Fguiopen then
   begin
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
 //Clear the Import Selector Form List, otherwise this could cause issues later
 if Fguiopen then ImportSelectorForm.ImportDirList.Items.Clear;
end;

{------------------------------------------------------------------------------}
//Edit the filetypes
{------------------------------------------------------------------------------}
procedure TMainForm.sb_FileTypeClick(Sender: TObject);
var
 ft    : Integer=0;
 i     : Integer=0;
 dir   : Integer=0;
 entry : Integer=0;
begin
 WriteToDebug('MainForm.sb_FileTypeClick');
 //ADFS or Spark non directories only
 if((Image.MajorFormatNumber=diAcornADFS)
  or(Image.MajorFormatNumber=diSpark))
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
    if Length(FTButtons)>0 then
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
 ft   : Integer=-1;
 dir  : Integer=0;
 entry: Integer=0;
begin
 WriteToDebug('MainForm.FileTypeClick');
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
                           +Image.GetDirSep(Image.Disc[dir].Partition)
                           +Image.Disc[dir].Entries[entry].Filename
                           ,IntToHex(ft,3))then
    begin
     //If all went OK, update the display
     DirListChange(Sender,DirList.Selected);
     //And mark as changed
     if Image.MajorFormatNumber<>diSpark then HasChanged:=True;
    end;
 end;
end;

{------------------------------------------------------------------------------}
//Swap the label for an edit - user clicked on the label
{------------------------------------------------------------------------------}
procedure TMainForm.SwapLabelEdit(editcont:TEdit;labelcont:TLabel;dir,hex:Boolean);
var
 edittext: String='';
begin
 WriteToDebug('MainForm.SwapLabelEdit');
 //If there is only one selection
 if DirList.SelectionCount=1 then
  //And it is a directory
  if TMyTreeNode(DirList.Selected).IsDir=dir then
   //And the editor is enabled
   if editcont.Enabled then
   begin //Then show it
    editcont.Visible  :=True;
    edittext:=labelcont.Caption;
    if edittext=' ' then edittext:='';
    //For hex number, remove the intial '0x'
    if(hex)and(LeftStr(edittext,2)='0x')then edittext:=Copy(edittext,3);
    editcont.Text     :=edittext;
    //Put the cursor in the control
    editcont.SetFocus;
    //And hide the original label
    labelcont.Visible :=False;
    //Disable the keyboard shortcuts
    DeleteAFile.Enabled:=False;
   end;
end;

{------------------------------------------------------------------------------}
//User has clicked on directory title
{------------------------------------------------------------------------------}
procedure TMainForm.lb_titleClick(Sender: TObject);
begin
 WriteToDebug('MainForm.lb_titleClick');
 SwapLabelEdit(ed_title,lb_title,True,False);
end;

{------------------------------------------------------------------------------}
//Show/Hide a Toolbar
{------------------------------------------------------------------------------}
procedure TMainForm.ShowHideToolbar(Sender: TObject);
var
 Item: Integer=-1;
begin
 WriteToDebug('MainForm.ShowHideToolbar');
 //Find the menu item
 if TMenuItem(Sender)=menuViewFileDetails then Item:=0;
 if TMenuItem(Sender)=menuViewStatus      then Item:=1;
 if TMenuItem(Sender)=menuImage           then Item:=4;
 if TMenuItem(Sender)=menuFiles           then Item:=5;
 if TMenuItem(Sender)=menuPartition       then Item:=6;
 if TMenuItem(Sender)=menuTools           then Item:=7;
 //Tick/Untick it
 TMenuItem(Sender).Checked:=not TMenuItem(Sender).Checked;
 //Make the appropriate element visible or not
 case Item of
  0      : FileInfoPanel.Visible                   :=TMenuItem(Sender).Checked;
  1      : ImageDetails.Visible                    :=TMenuItem(Sender).Checked;
  4,5,6,7: ToolBarContainer.Page[Item-4].TabVisible:=TMenuItem(Sender).Checked;
 end;
 //And the tool splitter, if the file info panel is visible or not
 ToolSplitter.Visible:=FileInfoPanel.Visible;
 //Set the registry option
 if Item<>-1 then
 begin
  //First, clear the bit
  ViewOptions:=ViewOptions AND($FFFF-(1<<Item));
  //Then set it, if needed
  if TMenuItem(Sender).Checked then ViewOptions:=ViewOptions OR(1<<Item);
  //And finally write to the registry
  DIMReg.SetRegValI('View_Options',ViewOptions);
 end;
end;

{------------------------------------------------------------------------------}
//User has clicked on execution address
{------------------------------------------------------------------------------}
procedure TMainForm.lb_execaddrClick(Sender: TObject);
begin
 WriteToDebug('MainForm.lb_execaddrClick');
 SwapLabelEdit(ed_execaddr,lb_execaddr,False,True);
end;

{------------------------------------------------------------------------------}
//User has clicked on load address
{------------------------------------------------------------------------------}
procedure TMainForm.lb_loadaddrClick(Sender: TObject);
begin
 WriteToDebug('MainForm.lb_loadaddrClick');
 SwapLabelEdit(ed_loadaddr,lb_loadaddr,False,True);
end;

{------------------------------------------------------------------------------}
//User has clicked on the timestamp label
{------------------------------------------------------------------------------}
procedure TMainForm.lb_timestampClick(Sender: TObject);
var
 dir  : Integer=0;
 entry: Integer=0;
begin
 WriteToDebug('MainForm.lb_timestampClick');
 //ADFS and AFS only
 if(Image.MajorFormatNumber=diAcornADFS)
 or(Image.MajorFormatNumber=diAcornFS)
 or(Image.MajorFormatNumber=diDOSPlus)
 or(Image.MajorFormatNumber=diAmiga)
 or(Image.MajorFormatNumber=diSpark)then
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
 dir        : Integer=0;
 entry      : Integer=0;
begin
 WriteToDebug('MainForm.ed_timestampEditingDone');
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
                          +Image.GetDirSep(Image.Disc[dir].Partition)
                          +Image.Disc[dir].Entries[entry].Filename
                          ,newtimedate) then //So send to the class
    begin
     //Display the new details
     lb_timestamp.Caption:=FormatDateTime(TimeDateFormat,
                                       Image.Disc[dir].Entries[entry].TimeStamp);
     if Image.MajorFormatNumber<>diSpark then
     begin
      HasChanged:=True;
      UpdateImageInfo;
     end;
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
 filename : String='';
 newtitle : String='';
begin
 WriteToDebug('MainForm.ed_titleEditingDone');
 //Re-enable the keyboard shortcuts
 DeleteAFile.Enabled:=True;
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
   UpdateImageInfo; //And update the status bar, if need be
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
 filename : String='';
 newaddr  : String='';
 oldaddr  : String='';
 dir      : Integer=0;
 entry    : Integer=0;
 ok       : Boolean=False;
begin
 WriteToDebug('MainForm.ed_execaddrEditingDone');
 //Re-enable the keyboard shortcuts
 DeleteAFile.Enabled:=True;
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
      lb_execaddr.Caption:='0x'
                          +IntToHex(Image.Disc[dir].Entries[entry].ExecAddr,8);
      lb_loadaddr.Caption:='0x'
                          +IntToHex(Image.Disc[dir].Entries[entry].LoadAddr,8);
      if Image.MajorFormatNumber<>diSpark then HasChanged:=True;
     end;
    end;
   end;
  //Hide the edit control and show the label
  TEdit(Sender).Visible:=False;
  if TEdit(Sender)=ed_execaddr then lb_execaddr.Visible:=True;
  if TEdit(Sender)=ed_loadaddr then lb_loadaddr.Visible:=True;
 end;
end;

{------------------------------------------------------------------------------}
//Image Detail Display
{------------------------------------------------------------------------------}
procedure TMainForm.btn_ImageDetailsClick(Sender: TObject);
var
 t         : Integer=0;
 s         : Integer=0;
 side      : Integer=0;
 col       : TColor=0;
 FSM       : array of TImage=nil;
 FSMlabel  : array of TLabel=nil;
 titles    : array[0..1] of TEdit;
 boots     : array[0..1] of TComboBox;
 bootlbs   : array[0..1] of TLabel;
 partstr   : String='';
 title     : String='';
 numsides  : Byte=0;
 skip      : Byte=0;
 img       : TLazIntfImage=nil;
 function ConvertColour(col: TColor):TFPColor;
 begin
  //Convert from TColor to TFPColor
  Result.Alpha:=$FF;
  Result.Blue :=(col>>16)AND$FF;
  Result.Green:=(col>> 8)AND$FF;
  Result.Red  := col     AND$FF;
  //TFPColor is 16 bit per Result
  Result.Alpha:=Result.Alpha or Result.Alpha<<8;
  Result.Red  :=Result.Red   or Result.Red  <<8;
  Result.Green:=Result.Green or Result.Green<<8;
  Result.Blue :=Result.Blue  or Result.Blue <<8;
 end;
begin
 WriteToDebug('MainForm.btn_ImageDetailsClick');
 //ROM FS image is open
 if Image.MajorFormatNumber=diAcornRFS then
 begin
  RFSDetailForm.ROMFSTitle.Text         :=Image.Title(0);
  RFSDetailForm.ROMFSVersion.Text       :=Image.VersionString;
  RFSDetailForm.ROMFSBinVersAdj.Position:=Image.VersionNumber;
  RFSDetailForm.ROMFSBinVers.Caption    :=IntToHex(Image.VersionNumber,2);
  RFSDetailForm.ROMFSCopy.Text          :=Image.Copyright;
  //Show the form, modally and respond if user clicks OK
  if RFSDetailForm.ShowModal=mrOK then
  begin
   //Set the modified flag while updating the image
   HasChanged:=(Image.VersionNumber<>RFSDetailForm.ROMFSBinVersAdj.Position)
             or(Image.UpdateDiscTitle(RFSDetailForm.ROMFSTitle.Text,0))
             or(Image.UpdateVersionString(RFSDetailForm.ROMFSVersion.Text))
             or(Image.UpdateCopyright(RFSDetailForm.ROMFSCopy.Text));
   //Update the version number
   Image.VersionNumber:=RFSDetailForm.ROMFSBinVersAdj.Position;
  end;
 end
 else //Every other valid format is open
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
  if Image.MajorFormatNumber<>diDOSPlus then
   ImageDetailForm.MapLabel.Caption:='Map Type'
  else
   ImageDetailForm.MapLabel.Caption:='FAT Type';
  ImageDetailForm.MapPanel.Visible   :=ImageDetailForm.lbMap.Caption<>'';
  //Directory type
  ImageDetailForm.lbDirType.Caption  :=Image.DirectoryTypeString;
  ImageDetailForm.DirPanel.Visible   :=ImageDetailForm.lbDirType.Caption<>'';
  //Interleave type
  if ImageDetailForm.cbInterleave.Items.Count=0 then //Populate the dropdown
   for t:=0 to Image.GetNumberOfInterleaves-1 do
    ImageDetailForm.cbInterleave.Items.Add(Image.GetInterleaveString(t));
  ImageDetailForm.lbInterleave.Caption:=Image.InterleaveInUse;
  ImageDetailForm.cbInterleave.ItemIndex:=Image.InterleaveInUseNum-1;
  ImageDetailForm.InterleavePanel.Visible:=
                                ImageDetailForm.lbInterleave.Caption<>'unknown';
{  if ImageDetailForm.InterleavePanel.Visible then //If this is visible
  begin
   if HasChanged then //Make the appropriate control visible
    ImageDetailForm.lbInterleave.Visible:=True
   else
    ImageDetailForm.lbInterleave.Visible:=False;
   //Can't change the interleave if the image has been modified
   ImageDetailForm.cbInterleave.Visible:=not ImageDetailForm.lbInterleave.Visible;
  end;}
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
  s:=ImageDetailForm.OKButton.Top-t;
  //Centralise vertical
  ImageDetailForm.AcornLogo.Top    :=t+((s-ImageDetailForm.AcornLogo.Height)div 2);
  ImageDetailForm.CommodoreLogo.Top:=ImageDetailForm.AcornLogo.Top;
  ImageDetailForm.AmigaLogo.Top    :=ImageDetailForm.AcornLogo.Top;
  ImageDetailForm.SinclairLogo.Top :=ImageDetailForm.AcornLogo.Top;
  ImageDetailForm.BBCMasterLogo.Top:=ImageDetailForm.AcornLogo.Top;
  ImageDetailForm.MicrosoftLogo.Top:=ImageDetailForm.AcornLogo.Top;
  //Centralise horizontal
  s:=ImageDetailForm.Legend.Width;
  ImageDetailForm.AcornLogo.Left    :=(s-ImageDetailForm.AcornLogo.Width)div 2;
  ImageDetailForm.CommodoreLogo.Left:=ImageDetailForm.AcornLogo.Left;
  ImageDetailForm.AmigaLogo.Left    :=ImageDetailForm.AcornLogo.Left;
  ImageDetailForm.SinclairLogo.Left :=ImageDetailForm.AcornLogo.Left;
  ImageDetailForm.BBCMasterLogo.Left:=ImageDetailForm.AcornLogo.Left;
  ImageDetailForm.MicrosoftLogo.Left:=ImageDetailForm.AcornLogo.Left;
  //Hide them all
  ImageDetailForm.AcornLogo.Visible    :=False;
  ImageDetailForm.CommodoreLogo.Visible:=False;
  ImageDetailForm.AmigaLogo.Visible    :=False;
  ImageDetailForm.SinclairLogo.Visible :=False;
  ImageDetailForm.BBCMasterLogo.Visible:=False;
  ImageDetailForm.MicrosoftLogo.Visible:=False;
  //Now display the appropriate one
  case Image.MajorFormatNumber of
   diAcornDFS,
   diAcornADFS,
   diAcornUEF,
   diAcornRFS,
   diAcornFS   : ImageDetailForm.AcornLogo.Visible    :=True;
   diCommodore : ImageDetailForm.CommodoreLogo.Visible:=True;
   diAmiga     : ImageDetailForm.AmigaLogo.Visible    :=True;
   diSinclair  : ImageDetailForm.SinclairLogo.Visible :=True;
   diDOSPlus   :
    if Image.MinorFormatNumber<>0 then
     ImageDetailForm.MicrosoftLogo.Visible:=True
    else
     ImageDetailForm.BBCMasterLogo.Visible:=True;
  end;
  //Should we label the top box?
  if Image.DoubleSided then
   ImageDetailForm.pnSide0Caption.Caption:='  Side 0 Details'
  else
   ImageDetailForm.pnSide0Caption.Caption:='  Side Details';
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
    //Style it
    FSMlabel[side].Color:=$777777;
    FSMlabel[side].Font.Color:=$FFFFFF;
    FSMlabel[side].Font.Style:=[fsBold];
    //We'll stretch it later
    FSM[side].Stretch:=False;
    //Work out what tracks to skip (images over 100000 pixels high will crash)
    skip:=(Length(Image.FreeSpaceMap[0])div 100000)+1;
    //Set the graphic size
    t:=Length(Image.FreeSpaceMap[side]);
    if t>0 then s:=Length(Image.FreeSpaceMap[side,0]) else s:=0;
    FSM[side].Height:=t div skip;
    FSM[side].Width:=s;
    //Initiate the canvas
    img:=TLazIntfImage.Create(0,0,[riqfRGB, riqfAlpha]);
    img.SetSize(FSM[side].Width,FSM[side].Height);
    //Colour in the free space
    img.FillPixels(ConvertColour(ImageDetailForm.colUnformatted.Brush.Color));
    //Now draw all the sectors in tracks
    if Length(Image.FreeSpaceMap[side])>0 then
     for t:=0 to Length(Image.FreeSpaceMap[side])-1 do
      if(t mod skip=0)and(Length(Image.FreeSpaceMap[side,t])>0)then
       for s:=0 to Length(Image.FreeSpaceMap[side,t])-1 do
       begin
        col:=ImageDetailForm.colFree.Brush.Color;
        //Other colours
        if Image.FreeSpaceMap[side,t,s]=diFSMUnformat then
         col:=ImageDetailForm.colUnformatted.Brush.Color;      //Unformatted
        if Image.FreeSpaceMap[side,t,s]=diFSMUsed then
         col:=ImageDetailForm.colFile.Brush.Color;      //Unknown/Files
        if Image.FreeSpaceMap[side,t,s]=diFSMSystem then
         col:=ImageDetailForm.colSystem.Brush.Color;    //System
        if Image.FreeSpaceMap[side,t,s]=diFSMDir then
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
    titles[0].Enabled:=(not Image.AFSPresent)
                    and(Image.MajorFormatNumber<>diSinclair);
    //Limit the length
    if Image.MajorFormatNumber=diAcornDFS  then titles[side].MaxLength:=12; //DFS
    if Image.MajorFormatNumber=diAcornADFS then
    begin
     if(Image.DOSPresent)and(side=1)then
      titles[side].MaxLength:=11  //DOS Partition in an ADFS disc
     else
      titles[side].MaxLength:=10; //ADFS
    end;
    if Image.MajorFormatNumber=diAcornFS   then titles[side].MaxLength:=10; //AFS
    if Image.MajorFormatNumber=diDOSPlus   then titles[side].MaxLength:=11; //DOS
    //Boot Option
    boots[side].Visible  :=Image.MajorFormatNumber<>diSinclair;
    if Length(Image.BootOpt)>side then
     boots[side].ItemIndex:=Image.BootOpt[side]
    else
     boots[side].Visible:=False;
    bootlbs[side].Visible:=boots[side].Visible;
   end;
   //Is this an ADFS Hybrid?
   if(Image.MajorFormatNumber=diAcornADFS)
   and((Image.AFSPresent)or(Image.DOSPresent))then
   begin
    partstr:='ADFS Partition';
    FSMlabel[0].Caption:=FSMlabel[0].Caption+' '+partstr;
    ImageDetailForm.pnSide0Caption.Caption:='  '+partstr+' Details';
    //AFS
    if Image.AFSPresent then partstr:='Acorn FS Partition';
    //DOS
    if Image.DOSPresent then partstr:='DOS Partition';
    FSMlabel[1].Caption:=FSMlabel[1].Caption+' '+partstr;
    ImageDetailForm.pnSide1Caption.Caption:='  '+partstr+' Details';
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
    //Update the interleave
    if(ImageDetailForm.cbInterleave.ItemIndex<>Image.InterleaveInUseNum-1)
    and(ImageDetailForm.cbInterleave.Visible)then
    begin
     //Remember the previous settings
     title:=Image.Filename;
     t:=ADFSInterleave;
     //Close the image
     btn_CloseImageClick(Sender);
     //Change the interleave
     ADFSInterleave:=ImageDetailForm.cbInterleave.ItemIndex+1;
     //Re-open
     OpenImage(title);
     //Change the interleave back
     ADFSInterleave:=t;
    end;
    //Still a valid image?
    if Image.FormatNumber<>diInvalidImg then
     for side:=0 to numsides-1 do
     begin
      //Update Disc Title
      if Image.UpdateDiscTitle(titles[side].Text,side)      then
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
end;

{-------------------------------------------------------------------------------
Open multiple images and output as CSV
-------------------------------------------------------------------------------}
procedure TMainForm.btn_MultiCSVClick(Sender: TObject);
begin
 WriteToDebug('MainForm.btn_MultiCSVClick');
 OpenImageFile.Options:=[ofAllowMultiSelect,ofEnableSizing,ofViewDetail];
 if OpenImageFile.Execute then
  SaveAsCSV(OpenImageFile.Files);
end;

{------------------------------------------------------------------------------}
//Close currently open image
{------------------------------------------------------------------------------}
procedure TMainForm.btn_CloseImageClick(Sender: TObject);
begin
 WriteToDebug('MainForm.btn_CloseImageClick');
 if QueryUnsaved then
 begin
  CloseAllHexDumps;
  Image.Close;
  ShowNewImage('');
  UpdateImageInfo;
  DisableControls;
  HasChanged:=False;
  ImageDetails.Invalidate;
 end;
end;

{------------------------------------------------------------------------------}
//Delete the current partition
{------------------------------------------------------------------------------}
procedure TMainForm.btn_DeletePartitionClick(Sender: TObject);
var
 side: Integer=0;
begin
 WriteToDebug('MainForm.btn_DeletePartitionClick');
 //Get the root reference
 side:=FindPartitionRoot(GetFilePath(DirList.Selections[0]));
 //if valid, find the partition/side
 if(side>=0)and(side<Length(Image.Disc))then
 begin
  side:=Image.Disc[side].Partition;
  //Now delete it
  if Image.SeparatePartition(side) then
  begin
   //Update the changed flag
   HasChanged:=True;
   //And the display
   ShowNewImage(Image.Filename);
   UpdateImageInfo;
  end;
 end else ReportError('Invalid Partition Selected');
end;

{------------------------------------------------------------------------------}
//Save the current partition
{------------------------------------------------------------------------------}
procedure TMainForm.btn_SavePartitionClick(Sender: TObject);
var
 side        : Integer=0;
 index       : Integer=0;
 targetformat: Integer=-1;
 exts        : array of String=nil;
begin
 WriteToDebug('MainForm.btn_SavePartitionClick');
 //Get the root reference
 side:=FindPartitionRoot(GetFilePath(DirList.Selections[0]));
 //if valid, find the partition/side
 if(side>=0)and(side<Length(Image.Disc))then
 begin
  side:=Image.Disc[side].Partition;
  //Open the Save As dialogue box
  SaveImage.Title:='Save Partition As';
  //DS DFS Image, so target is SS DFS
  if(Image.MajorFormatNumber=diAcornDFS)
  and(Image.DoubleSided)then targetformat:=Image.FormatNumber-1;
  //ADFS/AFS Hybrid, with AFS partition selected, so target will be AFS Level 3
  if(Image.MajorFormatNumber=diAcornADFS)
  and(Image.AFSPresent)
  and(side<>0)then targetformat:=diAcornFS<<4+2;
  //ADFS/DOS Hybrid, with DOS partition selected, so target will be DOS Plus
  if(Image.MajorFormatNumber=diAcornADFS)
  and(Image.DOSPresent)
  and(side<>0)then targetformat:=diDOSPlus<<4;
  //ADFS/AFS Hybrid, with ADFS partition selected, so target will be ADFS Hard Disc
  if(Image.MajorFormatNumber=diAcornADFS)
  and(side=0)then targetformat:=diAcornADFS<<4+$F;
  //Populate the filter part of the dialogue
  index:=0;
  SaveImage.Filter:=Image.SaveFilter(index,targetformat);
  if index=0 then index:=1;
  SaveImage.FilterIndex:=index;
  //Populate the filename part of the dialogue
  exts:=SaveImage.Filter.Split('|');
  SaveImage.FileName:='Untitled.'+Copy(exts[(index*2)-1],3);
  SaveImage.DefaultExt:='.'+Copy(exts[(index*2)-1],3);
  //Show the dialogue
  if SaveImage.Execute then
  begin
   //Show a progress message
   Image.ProgressIndicator:=@UpdateProgress;
   ProgressForm.Show;
   //Process the messages to close the file dialogue box
   Application.ProcessMessages;
   Image.ProgressIndicator:=@UpdateProgress;
   //Separate the partitions
   Image.SeparatePartition(side,SaveImage.FileName);
   //Close the progress message
   ProgressForm.Hide;
  end;
 end else ReportError('Invalid partition selected');
end;

{------------------------------------------------------------------------------}
//Creates a new password file
{------------------------------------------------------------------------------}
procedure TMainForm.btn_AddPasswordClick(Sender: TObject);
var
 index   : Integer=0;
begin
 WriteToDebug('MainForm.btn_AddPasswordFileClick');
 index:=Image.CreatePasswordFile(nil);
 if index>=0 then
 begin
  HasChanged:=True;
  AddFileToTree(DirList.Selected,'Passwords',index,False,DirList,False);
  UpdateImageInfo;
 end
 else
  ReportError('Failed to create Passwords file');
end;

{------------------------------------------------------------------------------}
//Change the interleave of an image
{------------------------------------------------------------------------------}
procedure TMainForm.btn_ChangeInterleaveClick(Sender: TObject);
begin
 WriteToDebug('MainForm.btn_ChangeInterleaveClick');
 //Setup the Change Interleave dialogue
 ChangeInterleaveForm.lb_Current.Caption:=Image.InterleaveInUse;
 ChangeInterleaveForm.cb_NewMethod.ItemIndex:=Image.InterleaveInUseNum-1;
 //Open the dialogue to select Interleave method
 ChangeInterleaveForm.ShowModal;
 //Change the Interleave
 if ChangeInterleaveForm.ModalResult=mrOK then
  if Image.ChangeInterleaveMethod(ChangeInterleaveForm.cb_NewMethod.ItemIndex+1)
   then HasChanged:=True;
end;

{------------------------------------------------------------------------------}
//Defrags (Compacts) the image (button click)
{------------------------------------------------------------------------------}
procedure TMainForm.btn_DefragClick(Sender: TObject);
var
 side      : Byte=0;
 dir       : Integer=0;
 dr        : Integer=0;
 entry     : Integer=0;
begin
 WriteToDebug('MainForm.btn_DefragClick');
 if Length(Image.Disc)>0 then
 begin
  ProgressForm.Show;
  Application.ProcessMessages;
  UpdateProgress('Preparing to defrag');
  //Determine which side/partition
  side:=0;
  if DirList.SelectionCount>0 then
  begin
   entry:=DirList.Selections[0].Index;
   if DirList.Selections[0].Parent<>nil then
    dir:=TMyTreeNode(DirList.Selections[0].Parent).DirRef
   else
    dir:=-1;
   dr:=TMyTreeNode(DirList.Selections[0]).DirRef;
   if dir>=0 then side:=Image.Disc[dir].Entries[entry].Side
   else side:=Image.Disc[dr].Partition;
  end;
  //Call the defrag procedure
  Defrag(side);
  ProgressForm.Hide;
 end;
end;

{------------------------------------------------------------------------------}
//Defrags (Compacts) the image
{------------------------------------------------------------------------------}
procedure TMainForm.Defrag(side: Byte=0);
var
 NewImage  : TDiscImage=nil;
 oldscan   : Boolean=False;
 olddos    : Boolean=False;
 ok        : Boolean=False;
 sidecount : Integer=0;
 index     : Integer=0;
 root      : Integer=0;
 filename  : String='';
begin
 if Fguiopen then WriteToDebug('MainForm.Defrag('+IntToStr(side)+')');
 //Can only defrag if there are objects and free space
 if(Length(Image.Disc)>0){and(Image.FreeSpace(side)>0)}then
 begin
  //Display progress form
  Image.ProgressIndicator:=nil;
  if Fguiopen then ProgressForm.Show;
  UpdateProgress('Preparing to defrag');
  //Create a new image, with the same shape as the original
  oldscan:=Image.ScanSubDirs;
  olddos :=Image.OpenDOSPartitions;
  Image.ScanSubDirs:=True;
  Image.OpenDOSPartitions:=False;
  NewImage:=TDiscImage.Create(Image);
  Image.ScanSubDirs:=oldscan;
  Image.OpenDOSPartitions:=olddos;
  root:=0; //Default pointer
  //We need to work out the root references
  if Length(Image.Disc)>0 then
  begin
   sidecount:=0;
   for index:=0 to Length(Image.Disc)-1 do
    if Image.Disc[index].Parent=-1 then
    begin
     if sidecount=side then root:=index;
     inc(sidecount);
    end;
  end;
  sidecount:=Length(Image.Disc[root].Entries);
  NewImage.ProgressIndicator:=nil;
  //Delete all the existing objects in the root
  Image.BeginUpdate;
  ok:=True;
  while(Length(Image.Disc[root].Entries)>0)and(ok)do
  begin
   if Fguiopen then
   begin
    ProgressForm.Show;
    Application.ProcessMessages;
   end;
   filename:=Image.Disc[root].Entries[0].Parent
            +Image.GetDirSep(Image.Disc[root].Partition)
            +Image.Disc[root].Entries[0].Filename;
   UpdateProgress('Preparing '+filename);
   if Fguiopen then
   begin
    SelectNode(filename);
    ok:=DeleteFile(False);
   end
   else
    ok:=Image.DeleteFile(filename);
  end;
  Image.EndUpdate;
  ok:=Length(Image.Disc[root].Entries)=0;
  if Fguiopen then
  begin
   ProgressForm.Show;
   Application.ProcessMessages;
   Image.ProgressIndicator:=@UpdateProgress;
  end;
  //Deleted all the files OK, so import again
  if ok then
  begin
   if Fguiopen then SelectNode(Image.Disc[root].Directory);
   //Import the contents of the current image into it
   ok:=ImportFiles(NewImage,False,False)=0;
   if ok then HasChanged:=True;//Update the changed flag
  end;
  //Didn't create a new image, so revert
  if not ok then
  begin
   Image.Free;
   Image:=TDiscImage.Create(NewImage);
   ReportError('Failed to defrag');
  end;
  if Fguiopen then
  begin
   //Update the directory display, whether it failed or not
   ShowNewImage(Image.Filename);
   UpdateImageInfo;
  end;
  //Free up resources and hide the progress form
  NewImage.Free;
  if Fguiopen then ProgressForm.Hide;
  Image.ProgressIndicator:=nil;
 end
 else //Report an error
 begin
  if Length(Image.Disc)=0 then //No objects to move
   ReportError('No objects in catalogue');
  if Image.FreeSpace(side)=0 then //No free space
   ReportError('No free space to perform a defrag');
 end;
end;

{------------------------------------------------------------------------------}
//Adds a new AFS or DOS partition to an ADFS image, or a second side to a DFS SS
{------------------------------------------------------------------------------}
procedure TMainForm.btn_AddPartitionClick(Sender: TObject);
var
 partsize : Integer=0;
 parttype : Integer=0;
 part     : String='';
 success  : Boolean=False;
begin
 WriteToDebug('MainForm.btn_AddPartitionClick');
 //ADFS
 if Image.MajorFormatNumber=diAcornADFS then
 begin
  //Set up the form
  AFSPartitionForm.Caption                   :='Add a new partition';
  AFSPartitionForm.PartitionSize.Visible     :=True;
  AFSPartitionForm.PartitionSizeLabel.Visible:=True;
  AFSPartitionForm.rad_typeAFS.Visible       :=True;
  AFSPartitionForm.rad_typeDOS.Visible       :=True;
  AFSPartitionForm.rad_type40T.Visible       :=False;
  AFSPartitionForm.rad_type80T.Visible       :=False;
  AFSPartitionForm.PartitionSize.Min:=9; //Minimum partition size
  AFSPartitionForm.maxAFSSize:=Image.GetMaxLength div $100;
  if AFSPartitionForm.maxAFSSize>$7F000 then //   <----- TO BE REMOVED
   AFSPartitionForm.maxAFSSize:=$7F000; //Max AFS is, currently, 127MB
  AFSPartitionForm.maxDOSSize:=Image.GetMaxLength div $100;
  if AFSPartitionForm.maxDOSSize>$1F4000 then
   AFSPartitionForm.maxDOSSize:=$1F4000; //Max DOS FAT12 is 500MB
  AFSPartitionForm.PartitionSize.Max     :=AFSPartitionForm.maxAFSSize; //Max partition size
  AFSPartitionForm.PartitionSize.Position:=AFSPartitionForm.maxAFSSize; //Current size
  AFSPartitionForm.PartitionSizeChange(Sender); //Update the label
  AFSPartitionForm.rad_typeAFS.Ticked        :=True; //Set to Acorn FS by default
  AFSPartitionForm.FromFileButton.Enabled    :=False;
  AFSPartitionForm.fromFile                  :=False;
  //Display the form
  if AFSPartitionForm.ShowModal=mrOK then //If OK was clicked, then continue
  begin
   //Get the specifications
   partsize:=AFSPartitionForm.PartitionSize.Position*$100;
   parttype:=-1;
   if AFSPartitionForm.rad_typeAFS.Ticked then parttype:=0;
   if AFSPartitionForm.rad_typeDOS.Ticked then parttype:=1;
   //Send to the class
   if Image.AddPartition(partsize,parttype) then
   begin
    //If successful, mark as changed
    HasChanged:=True;
    //Update the display
    ShowNewImage(Image.Filename);
    UpdateImageInfo;
   end
   else
   begin
    //If unsuccessful then report an error
    if parttype=0 then part:='Acorn File Server'
    else part:='DOS Plus';
    ReportError('Failed to create '+part+' partition');
   end;
  end;
 end;
 //DFS Single sided
 if Image.MajorFormatNumber=diAcornDFS then
 begin
  //Set up the form
  AFSPartitionForm.Caption                   :='Add a new or existing side';
  AFSPartitionForm.PartitionSize.Visible     :=False;
  AFSPartitionForm.PartitionSizeLabel.Visible:=False;
  AFSPartitionForm.rad_typeAFS.Visible       :=False;
  AFSPartitionForm.rad_typeDOS.Visible       :=False;
  AFSPartitionForm.rad_type40T.Visible       :=True;
  AFSPartitionForm.rad_type80T.Visible       :=True;
  AFSPartitionForm.rad_type40T.Ticked        :=True; //Set to 40T by default
  AFSPartitionForm.FromFileButton.Enabled    :=True;
  AFSPartitionForm.fromFile                  :=False;
  //Display the form
  if AFSPartitionForm.ShowModal=mrOK then //If OK was clicked, then continue
  begin
   success:=False;
   //Add an existing file
   if AFSPartitionForm.fromFile then
    success:=Image.AddPartition(AFSPartitionForm.OpenDFSFile.Filename)
   else //Create a new, blank, side
    if AFSPartitionForm.rad_type40T.Ticked then success:=Image.AddPartition(40)
    else success:=Image.AddPartition(80);
   //Success? then update the display, otherwise report an error
   if success then
   begin
    //Update the changed flag
    HasChanged:=True;
    //And the directory display
    ShowNewImage(Image.Filename);
    UpdateImageInfo;
   end else ReportError('Failed to add a side to the current image.');
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Opens the Password Editor form
{------------------------------------------------------------------------------}
procedure TMainForm.btn_EditPasswordClick(Sender: TObject);
begin
 WriteToDebug('MainForm.btn_EditPasswordClick');
 //Display them on the form
 PWordEditorForm.UserAccounts:=Image.ReadPasswordFile;
 //Show the form modally
 PWordEditorForm.ShowModal;
 //If user clicked OK
 if PWordEditorForm.ModalResult=mrOK then
 begin
  if Image.CreatePasswordFile(PWordEditorForm.UserAccounts)>=0 then
   HasChanged:=True;
 end;
end;

{------------------------------------------------------------------------------}
//Opens the file search window
{------------------------------------------------------------------------------}
procedure TMainForm.btn_FileSearchClick(Sender: TObject);
begin
 WriteToDebug('MainForm.btn_FileSearchClick');
 SearchForm.Show;
 SearchForm.Repaint;
end;

{------------------------------------------------------------------------------}
//Fix the ADFS Broken Directories
{------------------------------------------------------------------------------}
procedure TMainForm.btn_FixADFSClick(Sender: TObject);
var
 c: Boolean=False;
begin
 WriteToDebug('MainForm.btn_FixADFSClick');
 //Show a progress message
 Image.ProgressIndicator:=@UpdateProgress;
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
 dirname   : String='';
 parentdir : String='';
 x         : Integer=0;
 ref       : Cardinal=0;
begin
 WriteToDebug('MainForm.btn_NewDirectoryClick');
 //Assume the root, for now
 parentdir:='$';
 if DirList.Selections[0].Parent<>nil then //If not the root, get the parent directory
  parentdir:=GetImageFilename(TMyTreeNode(DirList.Selections[0]).ParentDir,
                                          DirList.Selections[0].Index)
 else
  parentdir:=GetImageFilename(TMyTreeNode(DirList.Selections[0]).DirRef,-1);
 //Default name
 dirname:='NewDir';
 //Make sure it doesn't exist
 x:=0;
 while Image.FileExists(parentdir+Image.DirSep+dirname+IntToStr(x),ref) do
  inc(x);
 dirname:=dirname+IntToStr(x);
 //Create the directory
 if Image.MajorFormatNumber<>diAmiga then
  CreateDirectory(dirname,'DLR')
 else
  CreateDirectory(dirname,''); //Create with no protection flags for Amiga
end;

{-------------------------------------------------------------------------------
Save all the current image's file details to a CSV file
-------------------------------------------------------------------------------}
procedure TMainForm.btn_SaveAsCSVClick(Sender: TObject);
begin
 WriteToDebug('MainForm.btn_SaveAsCSVClick');
 SaveAsCSV;
end;

{-------------------------------------------------------------------------------
Output an image's file details to a CSV file
-------------------------------------------------------------------------------}
procedure TMainForm.SaveAsCSV(filename: String='');
var
 FileNames: TStringList=nil;
 ok       : Boolean=False;
begin
 WriteToDebug('MainForm.SaveAsCSV('+filename+')');
 //Add the current image's filename to the stringlist as the only entry
 FileNames:=TStringList.Create;
 FileNames.Add(Image.Filename);
 //No filename specified, so get the current image's one
 if filename.IsEmpty then
  filename:=LeftStr(Image.Filename,
                    Length(Image.Filename)-Length(ExtractFileExt(Image.Filename)))
           +'.csv';
 //Display the save dialogue box (GUI only)
 if Fguiopen then
 begin
  //Populate the save dialogue box
  SaveImage.DefaultExt:='.csv';
  SaveImage.Filter:='CSV File|*.csv';
  SaveImage.FilterIndex:=1;
  SaveImage.Title:='Save CSV of image contents';
  //Add the csv extension
  SaveImage.Filename:=filename;
  ok:=SaveImage.Execute;
  if ok then filename:=SaveImage.FileName;
 end else ok:=True;
 //Finally, execute the other procedure with the values calculated
 if ok then SaveAsCSV(Filenames,filename);
 //Free up the stringlist
 FileNames.Free;
end;
procedure TMainForm.SaveAsCSV(FileNames: TStrings; filename: String='');
var
 ok       : Boolean=False;
 F        : TFileStream=nil;
 dir      : Integer=0;
 entry    : Integer=0;
 currfile : String='';
 line     : String='';
 hexlen   : Byte=0;
 report   : TStringList=nil;
 LImage   : TDiscImage=nil;
begin
 WriteToDebug('MainForm.SaveAsCSV(TStrings,'+filename+')');
 //More than one filename given, then blank off the filename
 if FileNames.Count>1 then filename:='';
 //No filenames given, then quit
 if FileNames.Count=0 then exit;
 //Get the last used settings from the registry
 CSVPrefForm.cb_IncDir.Ticked     :=DIMReg.GetRegValB('CSVIncDir'     ,False);
 CSVPrefForm.cb_IncFilename.Ticked:=DIMReg.GetRegValB('CSVIncFilename',True);
 CSVPrefForm.cb_IncReport.Ticked  :=DIMReg.GetRegValB('CSVIncReport'  ,True);
 CSVPrefForm.cb_Parent.Ticked     :=DIMReg.GetRegValB('CSVParent'     ,True);
 CSVPrefForm.cb_Filename.Ticked   :=DIMReg.GetRegValB('CSVFilename'   ,True);
 CSVPrefForm.cb_LoadAddr.Ticked   :=DIMReg.GetRegValB('CSVLoadAddr'   ,True);
 CSVPrefForm.cb_ExecAddr.Ticked   :=DIMReg.GetRegValB('CSVExecAddr'   ,True);
 CSVPrefForm.cb_Length.Ticked     :=DIMReg.GetRegValB('CSVLength'     ,True);
 CSVPrefForm.cb_Attributes.Ticked :=DIMReg.GetRegValB('CSVAttributes' ,True);
 CSVPrefForm.cb_Address.Ticked    :=DIMReg.GetRegValB('CSVAddress'    ,False);
 CSVPrefForm.cb_CRC32.Ticked      :=DIMReg.GetRegValB('CSVCRC32'      ,True); 
 CSVPrefForm.cb_MD5.Ticked        :=DIMReg.GetRegValB('CSVMD5'        ,False);
 if Fguiopen then
 begin
  //Ask user what they want in the CSV file
  CSVPrefForm.ShowModal;
  ok:=CSVPrefForm.ModalResult=mrOK;
 end else ok:=True;
 if ok then //Unless they clicked Cancel
 begin
  //Save the settings to the registry
  if Fguiopen then
  begin
   DIMReg.SetRegValB('CSVIncDir'     ,CSVPrefForm.cb_IncDir.Ticked);
   DIMReg.SetRegValB('CSVIncFilename',CSVPrefForm.cb_IncFilename.Ticked);
   DIMReg.SetRegValB('CSVIncReport'  ,CSVPrefForm.cb_IncReport.Ticked);
   DIMReg.SetRegValB('CSVParent'     ,CSVPrefForm.cb_Parent.Ticked);
   DIMReg.SetRegValB('CSVFilename'   ,CSVPrefForm.cb_Filename.Ticked);
   DIMReg.SetRegValB('CSVLoadAddr'   ,CSVPrefForm.cb_LoadAddr.Ticked);
   DIMReg.SetRegValB('CSVExecAddr'   ,CSVPrefForm.cb_ExecAddr.Ticked);
   DIMReg.SetRegValB('CSVLength'     ,CSVPrefForm.cb_Length.Ticked);
   DIMReg.SetRegValB('CSVAttributes' ,CSVPrefForm.cb_Attributes.Ticked);
   DIMReg.SetRegValB('CSVAddress'    ,CSVPrefForm.cb_Address.Ticked);
   DIMReg.SetRegValB('CSVCRC32'      ,CSVPrefForm.cb_CRC32.Ticked);
   DIMReg.SetRegValB('CSVMD5'        ,CSVPrefForm.cb_MD5.Ticked);
  end;
  //Remove the existing part of the original filename
  for currfile in FileNames do
   if FileExists(currfile) then
   begin
    ok:=True;
    //Is this the current file? If not calculate a new filename
    if currfile<>Image.Filename then
    begin
     filename:=LeftStr(currfile,
                       Length(currfile)-Length(ExtractFileExt(currfile)))
              +'.csv';
     //And create the DiscImage object
     LImage:=TDiscimage.Create;
     ok:=LImage.LoadFromFile(currfile);
    end else LImage:=TDiscImage.Create(Image); //Clone the current image
    WriteLn(ok);
    if ok then
    begin
     hexlen:=8;
     if LImage.MajorFormatNumber=diAcornDFS then hexlen:=6;
     //Show a progress message
     if Fguiopen then
     begin
      LImage.ProgressIndicator:=@UpdateProgress;
      ProgressForm.Show;
      //Process the messages to close the file dialogue box
      Application.ProcessMessages;
     end;
     //Open a new file
     try
      F:=TFileStream.Create(filename,fmCreate OR fmShareDenyNone);
      //Write the image details
      if CSVPrefForm.cb_IncFilename.Ticked then
       WriteLine(F,LImage.Filename.QuotedString('"')+',"0x'+LImage.CRC32+'"');
      //Write the report
      if CSVPrefForm.cb_IncReport.Ticked then
      begin
       report:=LImage.ImageReport(True);
       if report.Count>0 then
        for dir:=0 to report.Count-1 do
         WriteLine(F,report[dir]);
      end;
      //Write the headers
      line:='';
      if CSVPrefForm.cb_Parent.Ticked     then line:=line+'"Parent",';
      if CSVPrefForm.cb_Filename.Ticked   then line:=line+'"Filename",';
      if CSVPrefForm.cb_LoadAddr.Ticked   then line:=line+'"Load Address",';
      if CSVPrefForm.cb_ExecAddr.Ticked   then line:=line+'"Execution Address",';
      if CSVPrefForm.cb_Length.Ticked     then line:=line+'"Length",';
      if CSVPrefForm.cb_Attributes.Ticked then line:=line+'"Attributes",';
      if CSVPrefForm.cb_Address.Ticked    then line:=line+'"Address",';
      if CSVPrefForm.cb_CRC32.Ticked      then line:=line+'"CRC32",';
      if CSVPrefForm.cb_MD5.Ticked        then line:=line+'"MD-5",';
      line:=LeftStr(line,Length(line)-1);
      WriteLine(F,line);
      //Go through each directory
      for dir:=0 to Length(LImage.Disc)-1 do
       //And each entry in that directory
       for entry:=0 to Length(LImage.Disc[dir].Entries)-1 do
        //write out each entry
        if(LImage.Disc[dir].Entries[entry].DirRef=-1) //Exclude directories?
        or(CSVPrefForm.cb_IncDir.Ticked)then        //Or include them?
        begin
         line:='';
         if CSVPrefForm.cb_Parent.Ticked     then
          line:=line+LImage.GetParent(dir).QuotedString('"')+',';
         if CSVPrefForm.cb_Filename.Ticked   then
          line:=line+
                LImage.Disc[dir].Entries[entry].Filename.QuotedString('"')+',';
         if CSVPrefForm.cb_LoadAddr.Ticked   then
          line:=line+'"0x'
               +IntToHex(LImage.Disc[dir].Entries[entry].LoadAddr,hexlen)+'",';
         if CSVPrefForm.cb_ExecAddr.Ticked   then
          line:=line+'"0x'
               +IntToHex(LImage.Disc[dir].Entries[entry].ExecAddr,hexlen)+'",';
         if CSVPrefForm.cb_Length.Ticked     then
          line:=line+'"0x'
               +IntToHex(LImage.Disc[dir].Entries[entry].Length,hexlen)+'",';
         if CSVPrefForm.cb_Attributes.Ticked then
          line:=line+'"'+LImage.Disc[dir].Entries[entry].Attributes+'",';
         if CSVPrefForm.cb_Address.Ticked    then
          line:=line+'"0x'
               +IntToHex(LImage.Disc[dir].Entries[entry].Sector,hexlen)+'",';
         if CSVPrefForm.cb_CRC32.Ticked      then
          line:=line+'"0x'+LImage.GetFileCRC(LImage.GetParent(dir)
                                +LImage.GetDirSep(LImage.Disc[dir].Partition)
                                +LImage.Disc[dir].Entries[entry].Filename)+'",';
         if CSVPrefForm.cb_MD5.Ticked        then
          line:=line+'"0x'+LImage.GetFileMD5(LImage.GetParent(dir)
                                +LImage.GetDirSep(LImage.Disc[dir].Partition)
                                +LImage.Disc[dir].Entries[entry].Filename)+'",';
         line:=LeftStr(line,Length(line)-1);
         WriteLine(F,line);
        end;
      //Write a footer
      WriteLine(F,'""');
      WriteLine(F,'"'+ApplicationTitle+' v'+ApplicationVersion+'",'
                 +'"by Gerald J Holdsworth",'
                 +'"gerald@geraldholdsworth.co.uk"');
     except
      on E: Exception do ReportError('Failed to write file stream "'+filename
                                     +'": '+E.Message);
     end;
     //Finally free up the file stream
     F.Free;
     //Close the progress window
     if Fguiopen then ProgressForm.Hide
     else WriteLn('CSV output for '+filename+' complete.');
     LImage.Free;
     filename:='';
    end;
   end;
 end;
end;

{------------------------------------------------------------------------------}
//Open the preferences window
{------------------------------------------------------------------------------}
procedure TMainForm.btn_SettingsClick(Sender: TObject);
begin
 WriteToDebug('MainForm.btn_SettingsClick');
 //Set the preferences - Texture
 SettingsForm.NoTile.Ticked    :=False;
 SettingsForm.TileRO5.Ticked   :=False;
 SettingsForm.TileRO4.Ticked   :=False;
 SettingsForm.TileRO3.Ticked   :=False;
 SettingsForm.TileIyonix.Ticked:=False;
 SettingsForm.TileROPi.Ticked  :=False;
 case TextureType of
  0: SettingsForm.NoTile.Ticked    :=True;
  1: SettingsForm.TileRO5.Ticked   :=True;
  2: SettingsForm.TileRO4.Ticked   :=True;
  3: SettingsForm.TileRO3.Ticked   :=True;
  4: SettingsForm.TileIyonix.Ticked:=True;
  5: SettingsForm.TileROPi.Ticked  :=True;
 end;
 //Window styling
 SettingsForm.styleNative.Ticked:=False;
 SettingsForm.styleRISCOS.Ticked:=False;
 case Fstyling of
  NativeStyle: SettingsForm.styleNative.Ticked:=True;
  RISCOSStyle: SettingsForm.styleRISCOS.Ticked:=True;
 end;
 //ADFS Interleaving
 case ADFSInterleave of
  0: SettingsForm.ilAuto.Ticked    :=True;
  1: SettingsForm.ilSeq.Ticked     :=True;
  2: SettingsForm.ilInter.Ticked   :=True;
  3: SettingsForm.ilMPX.Ticked     :=True;
 end;
 //Miscellaneous
 SettingsForm.CreateINF.Ticked             :=DoCreateInf;
 SettingsForm.ImpliedAttr.Ticked           :=AddImpliedAttributes;
 SettingsForm.WriteDebug.Ticked            :=Fdebug;
 SettingsForm.AllowDFSZeroSecs.Ticked      :=FDFSZeroSecs;
 SettingsForm.DFSBeyondEdge.Ticked         :=FDFSBeyondEdge;
 SettingsForm.AllowDFSBlankFilenames.Ticked:=FDFSAllowBlank;
 SettingsForm.CompressUEF.Ticked           :=FUEFCompress;
 SettingsForm.ScanSubDirs.Ticked           :=FScanSubDirs;
 SettingsForm.OpenDOS.Ticked               :=FOpenDOS;
 SettingsForm.CreateDSC.Ticked             :=FCreateDSC;
 //Show the form, modally
 SettingsForm.ShowModal;
 if SettingsForm.ModalResult=mrOK then
 begin
  //Get the preferences
  if SettingsForm.NoTile.Ticked      then TextureType   :=0;
  if SettingsForm.TileRO5.Ticked     then TextureType   :=1;
  if SettingsForm.TileRO4.Ticked     then TextureType   :=2;
  if SettingsForm.TileRO3.Ticked     then TextureType   :=3;
  if SettingsForm.TileIyonix.Ticked  then TextureType   :=4;
  if SettingsForm.TileROPi.Ticked    then TextureType   :=5;
  if SettingsForm.styleNative.Ticked then Fstyling      :=NativeStyle;
  if SettingsForm.styleRISCOS.Ticked then Fstyling      :=RISCOSStyle;
  if SettingsForm.ilAuto.Ticked      then ADFSInterleave:=0;
  if SettingsForm.ilSeq.Ticked       then ADFSInterleave:=1;
  if SettingsForm.ilInter.Ticked     then ADFSInterleave:=2;
  if SettingsForm.ilMPX.Ticked       then ADFSInterleave:=3;
  DoCreateInf         :=SettingsForm.CreateINF.Ticked;
  AddImpliedAttributes:=SettingsForm.ImpliedAttr.Ticked;
  Fdebug              :=SettingsForm.WriteDebug.Ticked;
  FDFSZeroSecs        :=SettingsForm.AllowDFSZeroSecs.Ticked;
  FDFSBeyondEdge      :=SettingsForm.DFSBeyondEdge.Ticked;
  FDFSAllowBlank      :=SettingsForm.AllowDFSBlankFilenames.Ticked;
  FUEFCompress        :=SettingsForm.CompressUEF.Ticked;
  FScanSubDirs        :=SettingsForm.ScanSubDirs.Ticked;
  FOpenDOS            :=SettingsForm.OpenDOS.Ticked;
  FCreateDSC          :=SettingsForm.CreateDSC.Ticked;
  //Save the settings
  SaveConfigSettings;
  //Change the control style, if necessary
  SetNativeControls;
  //Change the tile under the filetype
  if DirList.SelectionCount=1 then DirListChange(Sender,DirList.Selected);
  //Repaint the main form
  Repaint;
 end;
end;

{------------------------------------------------------------------------------}
//Produces a report on the image
{------------------------------------------------------------------------------}
procedure TMainForm.btn_ShowReportClick(Sender: TObject);
 procedure AddLine(line: String);
 begin
  if Fguiopen then ImageReportForm.Report.Lines.Add(line)
  else WriteLn(line);
 end;
var
 pcent     : Integer=0;
 lpcent    : Integer=0;
 dir       : Integer=0;
 entry     : Integer=0;
 errorcount: Integer=0;
 Lreport   : TStringList;
begin
 WriteToDebug('MainForm.btn_ShowReportClick');
 //Get the report
 Lreport:=Image.ImageReport(False);
 //Display it
 if Fguiopen then
 begin
  MainForm.Cursor:=crHourGlass;
  ProgressForm.Show;
  Application.ProcessMessages;
  UpdateProgress('Preparing report');
  ImageReportForm.Report.Clear;
  ImageReportForm.Report.Lines:=Lreport;
 end
 else
  if Lreport.Count>0 then
   for pcent:=0 to Lreport.Count-1 do AddLine(Lreport[pcent]);
 //Add file details
 AddLine('');
 AddLine('File report');
 AddLine('===========');
 if not Image.ScanSubDirs then
  AddLine('Please note that not all directories may have been read in');
 errorcount:=0;
 lpcent:=-1;
 if Length(Image.Disc)>0 then
  for dir:=0 to Length(Image.Disc)-1 do
  begin
   //Progress display
   if Fguiopen then
   begin
    pcent:=Round(((dir+1)/Length(Image.Disc))*100);
    if lpcent<>pcent then
     UpdateProgress('Preparing report...'+IntToStr(pcent)+'%');
    lpcent:=lpcent;
   end;
   //Broken directory
   if Image.Disc[dir].Broken then
   begin
    AddLine(Image.GetParent(dir)+' is broken');
    inc(errorcount);
   end;
   //Check the files for errors
   if Length(Image.Disc[dir].Entries)>0 then
    for entry:=0 to Length(Image.Disc[dir].Entries)-1 do
     if Image.GetFileCRC(Image.GetParent(dir)
                        +Image.GetDirSep(Image.Disc[dir].Partition)
                        +Image.Disc[dir].Entries[entry].Filename,entry)='error'
                        then
     begin
      AddLine(Image.GetParent(dir)
                 +Image.GetDirSep(Image.Disc[dir].Partition)
                 +Image.Disc[dir].Entries[entry].Filename+' could not be read');
      inc(errorcount);
     end;
  end;
 AddLine(IntToStr(errorcount)+' error(s) found');
 //Add a footer
 AddLine(StringOfChar('_',80));
 AddLine(ApplicationTitle+' v'+ApplicationVersion);
 AddLine('by Gerald J Holdsworth');
 AddLine('gerald@geraldholdsworth.co.uk');
 //Show the form
 if Fguiopen then
 begin
  ProgressForm.Hide;
  Application.ProcessMessages;
  MainForm.Cursor:=crDefault;
  ImageReportForm.ShowModal;
 end;
end;

{------------------------------------------------------------------------------}
//Saves the configuration settings to the registry
{------------------------------------------------------------------------------}
procedure TMainForm.SaveConfigSettings;
begin
 WriteToDebug('MainForm.SaveConfigSettings');
 DIMReg.SetRegValI('Texture',             TextureType);
 DIMReg.SetRegValI('WindowStyle',         Fstyling);
 DIMReg.SetRegValI('ADFS_L_Interleave',   ADFSInterleave);
 DIMReg.SetRegValB('CreateINF',           DoCreateINF);
 DIMReg.GetRegValB('AddImpliedAttributes',AddImpliedAttributes);
 DIMReg.SetRegValB('Debug_Mode',          Fdebug);
 DIMReg.SetRegValB('DFS_Zero_Sectors',    FDFSZeroSecs);
 DIMReg.SetRegValB('DFS_Beyond_Edge',     FDFSBeyondEdge);
 DIMReg.SetRegValB('DFS_Allow_Blanks',    FDFSAllowBlank);
 DIMReg.SetRegValB('Scan_SubDirs',        FScanSubDirs);
 DIMReg.SetRegValB('Open_DOS',            FOpenDOS);
 DIMReg.SetRegValB('Create_DSC',          FCreateDSC);
 DIMReg.SetRegValB('UEF_Compress',        FUEFCompress);
end;

{------------------------------------------------------------------------------}
//The context menu has been requested, so cancel any drag drop operations
{------------------------------------------------------------------------------}
procedure TMainForm.DirListContextPopup(Sender: TObject; MousePos: TPoint;
 var Handled: Boolean);
begin
 WriteToDebug('MainForm.DirListContextPopup');
 //This actually pops up, on a Mac, when CTRL is pressed to add to the selection
 if ssCtrl in FormShiftState then Handled:=True; //Stops the context menu popping up
end;

{------------------------------------------------------------------------------}
//Duplicate File
{------------------------------------------------------------------------------}
procedure TMainForm.DuplicateFile1Click(Sender: TObject);
begin
 WriteToDebug('MainForm.DuplicateFileClick');
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
 ft   : String='';
 dir  : Integer=0;
 entry: Integer=0;
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
                           +Image.GetDirSep(Image.Disc[dir].Partition)
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
{ if DirList.Items.Count>0 then
  DirList.ImagesWidth:=DirList.Items[0].Height-3;}
  DefaultDraw:=True;
end;

{------------------------------------------------------------------------------}
//Paint the Directory Tree - The state arrow to the left
{------------------------------------------------------------------------------}
procedure TMainForm.DirListCustomDrawArrow(Sender: TCustomTreeView;
 const ARect: TRect; ACollapsed: Boolean);
var
 Index: Integer=0;
 TV   : TTreeView=nil;
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
 indent   : Integer=0;
 arrowin  : Integer=0;
 arrowsize: Integer=0;
 imgidx   : Integer=0;
 TV       : TTreeView=nil;
begin
 WriteToDebug('MainForm.DirListCustomDrawItem');
 if Sender is TTreeView then
 begin
  TV:=TTreeView(Sender);
  //Default font style
  TV.Font.Style:=[fsBold];
  TV.Font.Color:=$000000;
  //If it is a directory that hasn't been read in yet
  if(TMyTreeNode(Node).IsDir)
  and(not TMyTreeNode(Node).BeenRead)
  and(not TMyTreeNode(Node).Broken)then TV.Font.Style:=[fsBold,fsItalic];
  //Only concerned if it is selected, or a directory not read in, or broken
{  if(cdsSelected in State)
  or(TMyTreeNode(Node).IsDOSPart)
  or(((not TMyTreeNode(Node).BeenRead)
  or(TMyTreeNode(Node).Broken))and(TMyTreeNode(Node).IsDir))then
  begin}
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
     if TargetOS='Darwin' then dec(arrowsize); //For some reason macOS size is 1px smaller
     //Centralise it
     arrowin:=(NodeRect.Height-arrowsize)div 2;
     //Adjust the lefthand position to accomodate the arrow
     NodeRect.Left:=NodeRect.Left+indent+arrowin-NodeRect.Height;
     //And the top
     NodeRect.Top:=NodeRect.Top+arrowin;
     //Set the size
     NodeRect.Width:=arrowsize;
     NodeRect.Height:=arrowsize;
     //And call the OnCustomDrawArrow procedure
     DirListCustomDrawArrow(Sender,NodeRect,not Node.Expanded);
    end;
    //Draw the Image
    NodeRect:=Node.DisplayRect(False); //Image boundary rectangle
    NodeRect.Left:=NodeRect.Left+indent; //Left pos
    NodeRect.Top:=NodeRect.Top+2;        //Top pos
    NodeRect.Width:=TV.ImagesWidth;      //Width
    NodeRect.Height:=NodeRect.Width;     //Height
    //Get the correct image
    imgidx:=Node.ImageIndex;
    WriteToDebug('MainForm.DirListCustomDrawItem: imgidx='+IntToStr(imgidx));
    if imgidx=-1 then imgidx:=GetImageIndex(Node,Image);
    //And draw it
    TImageList(TV.Images).StretchDraw(TV.Canvas,imgidx,NodeRect);
    //Write out the text
    NodeRect:=Node.DisplayRect(False); //Text boundary rectangle
    NodeRect.Left:=NodeRect.Left+indent+TV.ImagesWidth+4; //Left pos
    NodeRect.Top:=NodeRect.Top+2;                         //Top pos
    //Change the colour - directory not been read in
    if(not TMyTreeNode(Node).BeenRead)
    and(TMyTreeNode(Node).IsDir)
    and(not TMyTreeNode(Node).Broken)then Font.Color:=$FF0000;
    //Change the colour - file is the DOS Partition
    if TMyTreeNode(Node).isDOSPart   then Font.Color:=$007700;
    //Change the colour - directory broken
    if(TMyTreeNode(Node).Broken)
    and(TMyTreeNode(Node).IsDir)     then Font.Color:=$0000FF;
    //Change the colour - selected item
    if cdsSelected in State then
    begin
     //Selected items
     Brush.Style:=bsSolid; //Solid background
     Brush.Color:=TV.SelectionColor; //Background
     //Paint the background
     FillRect(NodeRect.Left,NodeRect.Top,
              NodeRect.Left+TextWidth(Node.Text),NodeRect.Bottom);
     Font.Color:=TV.SelectionFontColor; //Foreground
    end else Brush.Style:=bsClear; //Clear background for everything else
    //Finally, write out the text
    TextOut(NodeRect.Left,NodeRect.Top,Node.Text);
   end;
//  end;
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
 if Sender is TPanel      then TileCanvas(TPanel(Sender).Canvas);
 if Sender is TToolBar    then TileCanvas(TToolBar(Sender).Canvas);
 if Sender is TForm       then TileCanvas(TForm(Sender).Canvas);
 if Sender is TScrollBox  then TileCanvas(TScrollBox(Sender).Canvas);
 if Sender is TControlBar then TileCanvas(TControlBar(Sender).Canvas);
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
 parentdir: String='$';
 index    : Integer=0;
 Node     : TTreeNode=nil;
// dir,entry:Cardinal;
begin
 Result:=nil;
 WriteToDebug('MainForm.CreateDirectory('+dirname+','+attr+')');
 if DirList.SelectionCount=0 then
  ReportError('Cannot find parent when adding directory "'+dirname+'"')
 else
 begin
  if DirList.Selections[0].Parent<>nil then //If not the root, get the parent directory
   parentdir:=GetImageFilename(TMyTreeNode(DirList.Selections[0]).ParentDir,
                                           DirList.Selections[0].Index)
  else
   parentdir:=GetImageFilename(TMyTreeNode(DirList.Selections[0]).DirRef,-1);
  //Ensure that the directory has been read in
  if not TMyTreeNode(DirList.Selections[0]).BeenRead then
   ReadInDirectory(DirList.Selections[0]);
  //Add it
  index:=Image.CreateDirectory(dirname,parentdir,attr);
  //Function returns pointer to next item (or parent if no children)
  if index>-1 then //Directory added OK
  begin
   //Mark as changed
   if Image.MajorFormatNumber<>diSpark then HasChanged:=True;
   //Create the node as a file
   Node:=AddFileToTree(DirList.Selected,dirname,index,True,DirList,False);
   //Update the directory reference and the directory flag
   TMyTreeNode(Node).DirRef:=Length(Image.Disc)-1;
   TMyTreeNode(Node).IsDir:=True;
   TMyTreeNode(Node).BeenRead:=True; //It'll be empty anyway
   TMyTreeNode(Node).Broken:=False;
   TMyTreeNode(Node).IsDOSPart:=False;
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
    if index=-1 then
     ReportError('Cannot create a directory on this platform');
    if index=-2 then
     ReportError('Could not add directory - image full');
    if index=-3 then
     ReportError('Directory "'+dirname+'" already exists in image');
    if index=-4 then
     ReportError('Catalogue full when adding "'+dirname+'"');
    if index=-5 then
     ReportError('Could not write directory "'+dirname+'" - error unknown');
    if index=-6 then
     ReportError('Destination directory does not exist');
    If index=-7 then
     ReportError('Could not write directory "'+dirname+'" - map full');
    if index=-8 then
     ReportError('Could not write directory "'+dirname+'" - nothing to write');
    if index=-9 then
     ReportError('Could not extend parent directory when adding "'+dirname+'"');
    if index<-9 then
     ReportError('Could not create directory "'+dirname+'" - unknown error');
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
 dir  : Cardinal=0;
 entry: Cardinal=0;
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
 xpos: Integer=0;
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
 Xmovement : Integer=0;
 Ymovement : Integer=0;
 H         : Integer=0;
 threshold : Integer=0;
 R,IR      : TRect;
 B         : TBitmap=nil;
 copymode  : Boolean=False;
 pt        : TPoint;
 Node      : TTreeNode=nil;
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
   AND((DraggedItem<>Dst)
     or(Image.MajorFormatNumber=diAcornUEF)
     or(Image.MajorFormatNumber=diAcornRFS))then
   begin
    //If it is not a directory, and not CFS, then get the parent
    if (not TMyTreeNode(Dst).IsDir)
    and(Image.MajorFormatNumber<>diAcornUEF)
    and(Image.MajorFormatNumber<>diAcornRFS)then
     Dst:=Dst.Parent;
    //Clear any selections and then highlight the original item, unless it is CFS
    DirList.ClearSelection;
    if (Image.MajorFormatNumber<>diAcornUEF)
    and(Image.MajorFormatNumber<>diAcornRFS)then DraggedItem.Selected:=True;
    //Only allow copying if it is a different parent and not within itself
    if(DraggedItem<>Dst)
    or(Image.MajorFormatNumber=diAcornUEF)
    or(Image.MajorFormatNumber=diAcornRFS)then//Or UEF
    begin
     //Highlight it
     Dst.Selected:=True;
     //Reset the timer
     HoverTimer.Enabled:=False;
     //And start it off again, so that there is a delay before expanding the node
     HoverTimer.Enabled:=True;
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
//User has hovered over item, so expand
{------------------------------------------------------------------------------}
procedure TMainForm.HoverTimerTimer(Sender: TObject);
begin
 if Dst<>nil then
  if Dst.Selected then
   Dst.Expand(False);
 HoverTimer.Enabled:=False;
end;

{------------------------------------------------------------------------------}
//Determine whether to copy or move
{------------------------------------------------------------------------------}
function TMainForm.GetCopyMode(Shift: TShiftState): Boolean;
begin
 //Default result
 Result:=True;
 //Look at the key modifiers
 if TargetOS='Darwin' then   //For the Mac
 begin
  if ssMeta in Shift then Result:=False; //Move
  if ssAlt in Shift  then Result:=True;  //Copy
 end else
 {%H-}begin //For Windows or Linux
  if ssShift in Shift then Result:=False; //Move
  if ssCtrl in Shift  then Result:=True;  //Copy
 end;
 //If the destination is the same as the source, copy only (not UEF)
 if(DraggedItem<>nil)and(Dst<>nil)then
  if (DraggedItem.Parent=Dst)
  and(Image.MajorFormatNumber<>diAcornUEF)
  and(Image.MajorFormatNumber<>diAcornRFS)then
   Result:=True;
end;

{------------------------------------------------------------------------------}
//User has dropped an item over the directory list
{------------------------------------------------------------------------------}
procedure TMainForm.DirListMouseUp(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
var
 Xmovement : Integer=0;
 Ymovement : Integer=0;
 copymode  : Boolean=False;
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
 newfn  : String='';
 i      : Integer=0;
 index  : Integer=-1;
 dir    : Integer=0;
 entry  : Integer=0;
 ref    : Cardinal=0;
 NewNode: TTreeNode=nil;
begin
 WriteToDebug('MainForm.DoCopyMove');
 //Are we dragging something, over something and is not the same as the source?
 if (DraggedItem<>nil)AND(Dst<>nil)
 AND((DraggedItem<>Dst)
   or(Image.MajorFormatNumber=diAcornUEF)
   or(Image.MajorFormatNumber=diAcornRFS))then
 begin
  //If it is not a directory, or CFS format, then get the parent
  if (not TMyTreeNode(Dst).IsDir)
  and(Image.MajorFormatNumber<>diAcornUEF)
  and(Image.MajorFormatNumber<>diAcornRFS)then Dst:=Dst.Parent;
  //Only allow moving/copying if it is not within itself
  if(DraggedItem<>Dst)
  or(Image.MajorFormatNumber=diAcornUEF)
  or(Image.MajorFormatNumber=diAcornRFS)then
  begin
   //Read in the destination, if necessary
   if(TMyTreeNode(Dst).IsDir)and(not TMyTreeNode(Dst).BeenRead)then
    ReadInDirectory(Dst);
   //Take a copy of the filename
   newfn:=DraggedItem.Text;
   //Do the copy/move
   if (Image.MajorFormatNumber<>diAcornUEF)
   and(Image.MajorFormatNumber<>diAcornRFS)then //Everything but UEF
   begin
    if copymode then //copy - here we rename the file if the destination already
     if Image.ValidateFilename(GetFilePath(Dst),newfn) then //has one the same
      index:=Image.CopyFile(GetFilePath(DraggedItem),GetFilePath(Dst),newfn);
    if not copymode then //move
     index:=Image.MoveFile(GetFilePath(DraggedItem),GetFilePath(Dst));
   end;
   if(Image.MajorFormatNumber=diAcornUEF)
   or(Image.MajorFormatNumber=diAcornRFS)then //UEF only
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
    ref:=0;
    if(Image.FileExists(GetFilePath(Dst)+Image.DirSep+newfn,ref))
    or(Image.MajorFormatNumber=diAcornUEF)
    or(Image.MajorFormatNumber=diAcornRFS)then
    begin
     if (Image.MajorFormatNumber<>diAcornUEF)
     and(Image.MajorFormatNumber<>diAcornRFS)then
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
     if Image.MajorFormatNumber<>diSpark then HasChanged:=True;
     if not copymode then //If moving
     begin
      //Update any open hexdumps
      if Length(HexDump)>0 then
       for i:=0 to Length(HexDump)-1 do
       begin
        //Update the window title
        if HexDump[i].Caption=GetFilePath(DraggedItem) then
         HexDump[i].Caption:=GetFilePath(Dst)
                            +Image.GetDirSep(Image.Disc[dir].Partition)
                            +Image.Disc[dir].Entries[entry].Filename;
        //Update the menu item
        if HexDumpMenu.Count>i then
         if HexDumpMenu.Items[i].Caption=GetFilePath(DraggedItem) then
          HexDumpMenu.Items[i].Caption:=GetFilePath(Dst)
                              +Image.GetDirSep(Image.Disc[dir].Partition)
                              +Image.Disc[dir].Entries[entry].Filename;
       end;
     end;
     NewNode:=AddFileToTree(Dst,Image.Disc[dir].Entries[entry].Filename,
                            index,TMyTreeNode(DraggedItem).IsDir,DirList,False);
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
    if index=-1  then
     ReportError('Could not load "'+DraggedItem.Text+'"');
    if index=-2  then
     ReportError('Could not add file "'+DraggedItem.Text+'" - image full');
    if index=-3  then
     ReportError('File "'+DraggedItem.Text+'" already exists in directory');
    if index=-4  then
     ReportError('Catalogue full when writing "'+DraggedItem.Text+'"');
    if index=-5  then
     ReportError('Could not write file "'+DraggedItem.Text+'" - error unknown');
    if index=-6  then
     ReportError('Destination directory does not exist when writing "'
                 +DraggedItem.Text+'"');
    If index=-7  then
     ReportError('Could not write file "'+DraggedItem.Text+'" - map full');
    if index=-8  then
     ReportError('Could not write file "'+DraggedItem.Text
                 +'" - nothing to write');
    if index=-9  then
     ReportError('Could not extend parent directory when writing "'
                 +DraggedItem.Text+'"');
    if index=-10 then
     ReportError('Cannot move "'+DraggedItem.Text+'" to the same directory');
    if index=-11 then
     ReportError('Could not find source file "'+DraggedItem.Text+'"');
    if index=-12 then
     ReportError('Not possible on this system');
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
//Create a new blank image - button clicked
{------------------------------------------------------------------------------}
procedure TMainForm.btn_NewImageClick(Sender: TObject);
begin
 WriteToDebug('MainForm.btn_NewImageClick');
 CreateNewImage;
end;

{------------------------------------------------------------------------------}
//Create a new blank image or set default image format
{------------------------------------------------------------------------------}
procedure TMainForm.CreateNewImage;
 function SelectMinor(LOptions: array of TRISCOSRadioBox): Byte;
 var Index: Byte=0;
 begin
  Result:=0;
  for Index:=0 to Length(LOptions)-1 do
   if LOptions[index].Ticked then Result:=Index;
 end;
var
 major    : Word=$FFF;
 binvers  : Byte=0;
 minor    : Byte=$F;
 tracks   : Byte=0;
 ok       : Boolean=False;
 hdd      : Boolean=False;
 index    : Integer=0;
 title    : String='';
 version  : String='';
 copyr    : String='';
 filename : String='';
begin
 ok:=True;
 if QueryUnsaved then
 begin
  //Show the form, modally
  NewImageForm.ShowModal;
  //If OK was clicked (and it is valid), then create a new image
  if(NewImageForm.ModalResult=mrOK)and(NewImageForm.ok)then
  begin
   CloseAllHexDumps; //From this point, all loaded data will be dumped
   //DFS
   if NewImageForm.SystemOptions[0].Ticked then
   begin
    major:=diAcornDFS;
    minor:=SelectMinor(NewImageForm.DFSOptions);
    tracks:=SelectMinor(NewImageForm.DFSTOptions);
   end;
   //ADFS
   if NewImageForm.SystemOptions[1].Ticked then
   begin
    major:=diAcornADFS;
    minor:=SelectMinor(NewImageForm.ADFSOptions);
   end;
   //C64
   if NewImageForm.SystemOptions[2].Ticked then
   begin
    major:=diCommodore;
    minor:=SelectMinor(NewImageForm.C64Options);
   end;
   //Spectrum
   if NewImageForm.SystemOptions[3].Ticked then
   begin
    major:=diSinclair;
    minor:=SelectMinor(NewImageForm.SpecOptions);
   end;
   //Amiga
   if NewImageForm.SystemOptions[4].Ticked then
   begin
    major:=diAmiga;
    minor:=SelectMinor(NewImageForm.AmigaOptions);
   end;
   //CFS
   if NewImageForm.SystemOptions[5].Ticked then
   begin
    major:=diAcornUEF;
    minor:=$0;
   end;
   //Spark
   if NewImageForm.SystemOptions[6].Ticked then
   begin
    major:=diSpark;
    minor:=$0;
    SaveImage.Title:='Create New !Spark Image As';
    //Populate the filter part of the dialogue
    index:=0;
    SaveImage.Filter:=Image.SaveFilter(index,diSpark<<4);
    if index=0 then index:=1;
    SaveImage.FilterIndex:=index;
    //Populate the filename part of the dialogue
    SaveImage.FileName:='Untitled.zip';
    SaveImage.DefaultExt:='.zip';
    //Show the dialogue and set the filename
    if SaveImage.Execute then filename:=SaveImage.FileName else exit;
   end;
   //AFS
   if NewImageForm.SystemOptions[7].Ticked then
   begin
    major:=diAcornFS;
    minor:=SelectMinor(NewImageForm.AFSOptions);
   end;
   //DOS
   if NewImageForm.SystemOptions[8].Ticked then
   begin
    major:=diDOSPlus;
    minor:=SelectMinor(NewImageForm.DOSOptions);
   end;
   //ROM FS
   if NewImageForm.SystemOptions[9].Ticked then
   begin
    major:=diAcornRFS;
    minor:=$0;
    title  :=NewImageForm.ROMFSTitle.Text;
    version:=NewImageForm.ROMFSVersion.Text;
    binvers:=NewImageForm.ROMFSBinVersAdj.Position;
    copyr  :=NewImageForm.ROMFSCopy.Text;
   end;
   //Create the new image
   //Show the progress indicator
   Image.ProgressIndicator:=@UpdateProgress;
   ProgressForm.Show;
   Application.ProcessMessages;
   //ADFS Hard Drive
   if(major=diAcornADFS)and(minor=8)then
   begin
    ok:=Image.FormatHDD(diAcornADFS,
                        NewImageForm.harddrivesize,
                        NewImageForm.ide,
                        NewImageForm.newmap,
                        NewImageForm.dirtype,
                        NewImageForm.addheader);
    hdd:=True;
   end;
   //AFS HardDrive
   if major=diAcornFS then
   begin
    //Create the format
    ok:=Image.FormatHDD(diAcornFS,
                        NewImageForm.AFSImageSize.Position*10*1024,
                        minor+2);
    if(ok)and(NewImageForm.cb_AFScreatepword.Ticked)then
     //Create blank password file for AFS
     if Image.CreatePasswordFile(nil)<0 then //If fails, report an error
      ReportError('Failed to create a password file');
    hdd:=True;
   end;
   //DOS Hard Drive
   if(major=diDOSPlus)and(minor=6)then
   begin
    ok:=Image.FormatHDD(major,NewImageForm.harddrivesize,NewImageForm.fat);
    hdd:=True;
   end;
   //Amiga Hard Drive
   if(major=diAmiga)and(minor=2)then
   begin
    ok:=Image.FormatHDD(major,NewImageForm.harddrivesize);
    hdd:=True;
   end;
   //Floppy Drive
   if not hdd then
    case major of
     diAcornDFS,
     diAcornADFS,
     diCommodore,
     diSinclair,
     diAmiga,
     diDOSPlus   : ok:=Image.FormatFDD(major,minor,tracks);
     diSpark     : ok:=Image.FormatFDD(major,filename);
     diAcornUEF  : ok:=Image.FormatFDD(major);
     diAcornRFS  : ok:=Image.FormatFDD(major,title,version,copyr,binvers);
    end;
   //All OK, so load the new image
   if ok then
   begin
    if major<>diSpark then HasChanged:=True;
    ShowNewImage(Image.Filename);  //This updates the status bar
   end
   else
    ReportError('Failed to create image');
   ProgressForm.Hide;
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Attribute has been changed
{------------------------------------------------------------------------------}
procedure TMainForm.AttributeChangeClick(Sender: TObject);
var
 att     : String='';
 filepath: String='';
 dir     : Integer=0;
 afs     : Boolean=False;
 dos     : Boolean=False;
begin
 if not DoNotUpdate then
 begin
  WriteToDebug('MainForm.AttributeChangeClick');
  //Determine if this is on an AFS or DOS partition
  if DirList.SelectionCount=1 then //Assuming only one item selected
  begin
   //And it is not the root
   if DirList.Selected.Parent<>nil then
   begin
    //Make a note of the AFS and DOS flags, for ADFS
    dir:=TMyTreeNode(DirList.Selected.Parent).DirRef; //Directory reference of the parent
    if(dir>=0)and(dir<Length(Image.Disc))then
    begin
     afs:=Image.Disc[dir].AFSPartition;
     dos:=Image.Disc[dir].DOSPartition;
    end;
   end;
  end;
  att:='';
  //Attributes - DFS and UEF
  if(Image.MajorFormatNumber=diAcornDFS)
  or(Image.MajorFormatNumber=diAcornUEF)
  or(Image.MajorFormatNumber=diAcornRFS)then
   if cb_DFS_l.Ticked then att:=att+'L';
  //Attributes - ADFS
  if((Image.MajorFormatNumber=diAcornADFS)
  or(Image.MajorFormatNumber=diSpark))
  and(not afs)and(not dos)then
  begin
   if cb_ADFS_ownw.Ticked then att:=att+'W';
   if cb_ADFS_ownr.Ticked then att:=att+'R';
   if cb_ADFS_ownl.Ticked then att:=att+'L';
   if cb_ADFS_owne.Ticked then att:=att+'E';
   if cb_ADFS_pubw.Ticked then att:=att+'w';
   if cb_ADFS_pubr.Ticked then att:=att+'r';
   if cb_ADFS_pube.Ticked then att:=att+'e';
   if cb_ADFS_pubp.Ticked then att:=att+'P';
  end;
  //Attributes - AFS
  if(Image.MajorFormatNumber=diAcornFS)
  or((Image.MajorFormatNumber=diAcornADFS)and(afs))then
  begin
   if cb_AFS_ownw.Ticked then att:=att+'W';
   if cb_AFS_ownr.Ticked then att:=att+'R';
   if cb_AFS_ownl.Ticked then att:=att+'L';
   if cb_AFS_pubw.Ticked then att:=att+'w';
   if cb_AFS_pubr.Ticked then att:=att+'r';
  end;
  //Attributes - Commodore 64
  if Image.MajorFormatNumber=diCommodore then
  begin
   if cb_C64_c.Ticked then att:=att+'C';
   if cb_C64_l.Ticked then att:=att+'L';
  end;
  //Attributes - DOS Plus
  if(Image.MajorFormatNumber=diDOSPlus)
  or((Image.MajorFormatNumber=diAcornADFS)and(dos))then
  begin
   if cb_DOS_hidden.Ticked  then att:=att+'H';
   if cb_DOS_read.Ticked    then att:=att+'R';
   if cb_DOS_system.Ticked  then att:=att+'S';
   if cb_DOS_archive.Ticked then att:=att+'A';
  end;
  //Attributes - Amiga
  if Image.MajorFormatNumber=diAmiga then
  begin
   if not cb_Amiga_ownd.Ticked  then att:=att+'D';
   if not cb_Amiga_owne.Ticked  then att:=att+'E';
   if not cb_Amiga_ownw.Ticked  then att:=att+'W';
   if not cb_Amiga_ownr.Ticked  then att:=att+'R';
   if cb_Amiga_arch.Ticked  then att:=att+'A';
   if cb_Amiga_pure.Ticked  then att:=att+'P';
   if cb_Amiga_scri.Ticked  then att:=att+'S';
   if cb_Amiga_hold.Ticked  then att:=att+'H';
   if not cb_Amiga_pubd.Ticked  then att:=att+'d';
   if cb_Amiga_pube.Ticked  then att:=att+'e';
   if cb_Amiga_pubw.Ticked  then att:=att+'w';
   if cb_Amiga_pubr.Ticked  then att:=att+'r';
   if not cb_Amiga_othd.Ticked  then att:=att+'l';
   if cb_Amiga_othe.Ticked  then att:=att+'x';
   if cb_Amiga_othw.Ticked  then att:=att+'i';
   if cb_Amiga_othr.Ticked  then att:=att+'a';
  end;
  //Add the directory attribute
  if TMyTreeNode(DirList.Selected).IsDir then
   if Image.MajorFormatNumber<>diAmiga then att:=att+'D' else att:=att+'F';
  //Get the file path
  filepath:=GetFilePath(DirList.Selected);
  //Update the attributes for the file
  if Image.UpdateAttributes(filepath,att,DirList.Selected.Index) then
  begin
   if Image.MajorFormatNumber<>diSpark then HasChanged:=True;
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
 WriteToDebug('MainForm.GetFilePath');
 //Make sure that the node exists
 if Node<>nil then
 begin
  //Start with the text in the node (i.e the filename)
  Result:=Node.Text;
  //If it has a parent, then add this and move up a level, until we reach the root
  while Node.Parent<>nil do
  begin
   Node:=Node.Parent;
   Result:=Node.Text+Image.GetDirSep(TMyTreeNode(Node).Partition)+Result;
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Delete file
{------------------------------------------------------------------------------}
procedure TMainForm.DeleteFile1Click(Sender: TObject);
var
 R  : Boolean=False;
begin
 WriteToDebug('MainForm.DeleteFileClick');
 //Result of the confirmation - assumed Yes for now
 R:=True;
 //For mulitple deletes, ensure that the user really wants to
 if DirList.SelectionCount>1 then
  R:=AskConfirm('Delete '+IntToStr(DirList.SelectionCount)+' files?'
               ,['Yes','No'])=mrOK;
 //If user does, or single file, continue
 if R then DeleteFile(True);
end;

{------------------------------------------------------------------------------}
//Delete selected files
{------------------------------------------------------------------------------}
function TMainForm.DeleteFile(confirm: Boolean): Boolean;
var
 LDirRef : Integer=0;
 j       : Integer=0;
 nodes   : Integer=0;
 LIsDir  : Boolean=False;
 R       : Boolean=False;
 ok      : Boolean=False;
 filepath: String='';
begin
 WriteToDebug('MainForm.DeleteFile');
 Result:=False;
 if DirList.SelectionCount>0 then
 begin
  //Take a note of the number of selections
  nodes:=DirList.SelectionCount;
  //Go through all the selections (or the only one)
  ok:=True;
  WriteToDebug('MainForm.DeleteFile: DirList.SelectionCount = '
               +IntToStr(DirList.SelectionCount));
  while(DirList.SelectionCount>0)and(ok)do
  begin
   //Get the full path to the file
   filepath:=GetFilePath(DirList.Selections[0]);
   WriteToDebug('MainForm.DeleteFile: filepath='+filepath);
   //If singular, check if the user wants to
   if(nodes=1)and(confirm)then
    R:=AskConfirm('Delete '+filepath+'?',['Yes','No'])=mrOK
   else R:=True;
   //If so, then delete
   if R then
   begin
    if nodes>1 then
    begin
     ProgressForm.Show;
     Application.ProcessMessages;
    end;
    //Make a note if we are deleting a directory
    LIsDir:=TMyTreeNode(DirList.Selections[0]).IsDir;
    if LIsDir then
     LDirRef:=TMyTreeNode(DirList.Selections[0]).DirRef
    else
     LDirRef:=-1;
    //Perform the deletion
    if (Image.MajorFormatNumber<>diAcornUEF)
    and(Image.MajorFormatNumber<>diAcornRFS)then
     ok:=Image.DeleteFile(filepath)
    else
     ok:=Image.DeleteFile(DirList.Selections[0].Index);
    if ok then
    begin
     WriteToDebug('MainForm.DeleteFile: File deletion OK');
     //Update the directory references
     if LIsDir then
     begin
      for j:=0 to DirList.Items.Count-1 do
      begin
       if TMyTreeNode(DirList.Items[j]).DirRef>LDirRef then
        TMyTreeNode(DirList.Items[j]).DirRef:=
                                         TMyTreeNode(DirList.Items[j]).DirRef-1;
       if TMyTreeNode(DirList.Items[j]).ParentDir>LDirRef then
        TMyTreeNode(DirList.Items[j]).ParentDir:=
                                      TMyTreeNode(DirList.Items[j]).ParentDir-1;
      end;
     end;
     if Image.MajorFormatNumber<>diSpark then HasChanged:=True;
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
    end else WriteToDebug('MainForm.DeleteFile: File deletion failed');
    if nodes>1 then ProgressForm.Hide;
   end else ok:=False;
  end;
 end;
 Result:=ok;
end;

{------------------------------------------------------------------------------}
//User has double clicked on the DirList box
{------------------------------------------------------------------------------}
procedure TMainForm.DirListDblClick(Sender: TObject);
var
 Node    : TTreeNode=nil;
 index   : Integer=0;
 i       : Integer=0;
 entry   : Integer=0;
 dir     : Integer=0;
 filename: String='';
 buffer  : TDIByteArray=nil;
 menuitem: TMenuItem=nil;
begin
 WriteToDebug('MainForm.DirListDblClick');
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
   //entry:=Node.Index;
   if Image.FileExists(GetFilePath(Node),dir,entry) then
   begin
    dir:=-1;
    //If the node does not have a parent, then the dir ref is the one contained
    //in the extra info. Otherwise is -1
    if Node.Parent<>nil then
    begin
     dir  :=TMyTreeNode(Node).ParentDir;
     //Get the full filename with path
     filename:=Image.GetParent(dir)+
               Image.GetDirSep(Image.Disc[dir].Partition)+
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
  end
  else //User double clicked on a directory
  begin
   ReadInDirectory(Node);
   //Need to open the directory
   Node.Expand(False);
  end;
end;

{------------------------------------------------------------------------------}
//Read a directory onto the tree
{------------------------------------------------------------------------------}
procedure TMainForm.ReadInDirectory(Node: TTreeNode);
var
 dir     : Integer=0;
 entry   : Integer=0;
 index   : Integer=0;
 filename: String='';
begin
 WriteToDebug('MainForm.ReadInDirectory');
 if not TMyTreeNode(Node).BeenRead then //If it hasn't been read
 begin
  if Image.FileExists(GetFilePath(Node),dir,entry) then
  begin
   dir:=-1;
   //dir variable, as above
   if Node.Parent<>nil then //We will only act on this if not the root
   begin
    dir:=TMyTreeNode(Node).ParentDir;
    //Get the full filename with path
    filename:=Image.GetParent(dir)+
              Image.GetDirSep(Image.Disc[dir].Partition)+
              Image.Disc[dir].Entries[entry].Filename;
    //And read in the directory
    index:=Image.ReadDirectory(filename);
    if index<>-1 then
    begin
     //Add the entire directory contents
     AddDirectoryToTree(Node,index,Image,index);
     //Mark this directory as having been read
     TMyTreeNode(Node).BeenRead:=True;
     TMyTreeNode(Node).Broken:=
                       Image.Disc[Image.Disc[dir].Entries[entry].DirRef].Broken;
    end;
   end;
  end;
 end;
end;

{------------------------------------------------------------------------------}
//User has clicked on a hexdump menu item
{------------------------------------------------------------------------------}
procedure TMainForm.HexDumpSubItemClick(Sender: TObject);
var
 i: Integer=0;
begin
 WriteToDebug('MainForm.HexDumpSubItemClick');
 //Find and open the appropriate hex dump window
 for i:=0 to Length(HexDump)-1 do
  if HexDump[i].Caption=TMenuItem(Sender).Caption then HexDump[i].Show;
end;

{------------------------------------------------------------------------------}
//User has double clicked on the DirList box
{------------------------------------------------------------------------------}
procedure TMainForm.CloseAllHexDumps;
begin
 WriteToDebug('MainForm.CloseAllHexDumps');
 //Close all the HexDump forms dynamically created
 while Length(HexDump)>0 do
 begin
  HexDump[Length(HexDump)-1].Free;
  SetLength(HexDump,Length(HexDump)-1);
  //There is a Delete method which can be used
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
 WriteToDebug('MainForm.RenameFile1Click');
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
 WriteToDebug('MainForm.DirListExit');
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
var
 ctrl: TControl=nil;
 pt  : TPoint;
begin
 WriteToDebug('MainForm.DirListEditing');
 //Do not allow an edit at this stage
 AllowEdit:=False;
 //Disable the keyboard shortcuts
 DeleteAFile.Enabled:=False;
 //Get the control under the mouse cursor
 pt:=ScreenToClient(Mouse.CursorPos);
 ctrl:=ControlAtPos(pt,[capfRecursive,capfAllowWinControls]);
 //Make sure it is indeed over the directory tree
 if Assigned(ctrl) then
  if ctrl.Name=DirList.Name then
  begin
   //Set the node to edit mode, if not the root
   AllowEdit:=Node.Parent<>nil;
   //Save the path and name before they get edited
   PathBeforeEdit:=GetFilePath(Node);
   NameBeforeEdit:=Node.Text;
  end;
end;

{------------------------------------------------------------------------------}
//Rename file/directory
{------------------------------------------------------------------------------}
procedure TMainForm.DirListEditingEnd(Sender: TObject; Node: TTreeNode;
 Cancel: Boolean);
var
 newfilename : String='';
 fileparent  : String='';
 newindex    : Integer=0;
 i           : Integer=0;
 NewNode     : TTreeNode=nil;
begin
 if AppIsClosing then exit;
 WriteToDebug('MainForm.DirListEditingEnd');
 //Get the new filename
 newfilename:=Node.Text;
 //Re-enable the keyboard shortcuts
 DeleteAFile.Enabled:=True;
 //Should we be renaming?
 if(not Cancel)and(NameBeforeEdit<>newfilename)then
 begin
  //Rename the file
  if(Image.MajorFormatNumber=diAcornUEF)
  or(Image.MajorFormatNumber=diAcornRFS)then
   newindex:=Image.RenameFile(Node.Index,newfilename)
  else
   newindex:=Image.RenameFile(PathBeforeEdit,newfilename);
  if newindex<0 then
  begin
   //Revert if it cannot be renamed
   Node.Text:=NameBeforeEdit;
   //And report error
   if newindex=-1 then
    ReportError('Failed to rename file/directory');
   if newindex=-2 then
    ReportError('File/directory does not exist');
   if newindex=-3 then
    ReportError('Cannot rename file/directory - "'+newfilename+'" already exists');
   if newindex=-4 then
    ReportError('Could not extend parent directory - image full');
   if newindex=-5 then
    ReportError('Could not extend parent directory - no free fragments');
   if newindex=-6 then
    ReportError('Renaming unsupported in this format');
  end
  else
  begin
   if Image.MajorFormatNumber<>diSpark then HasChanged:=True;
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
    fileparent:=LeftStr(PathBeforeEdit,
                        Length(PathBeforeEdit)-Length(NameBeforeEdit));
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
 WriteToDebug('MainForm.ResetFileFields');
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
 ADFSAttrPanel.Visible    :=False;
 AFSAttrPanel.Visible     :=False;
 DFSAttrPanel.Visible     :=False;
 C64AttrPanel.Visible     :=False;
 DOSAttrPanel.Visible     :=False;
 SinclairAttrPanel.Visible:=False;
 AmigaAttrPanel.Visible   :=False;
 ISOAttrPanel.Visible     :=False;
 //And untick them
 cb_ADFS_ownw.Ticked        :=False;
 cb_ADFS_ownr.Ticked        :=False;
 cb_ADFS_ownl.Ticked        :=False;
 cb_ADFS_owne.Ticked        :=False;
 cb_ADFS_pubw.Ticked        :=False;
 cb_ADFS_pubr.Ticked        :=False;
 cb_ADFS_pube.Ticked        :=False;
 cb_ADFS_pubp.Ticked        :=False;
 cb_DFS_l.Ticked            :=False;
 cb_C64_l.Ticked            :=False;
 cb_C64_c.Ticked            :=False;
 cb_AFS_ownl.Ticked         :=False;
 cb_AFS_ownr.Ticked         :=False;
 cb_AFS_ownw.Ticked         :=False;
 cb_AFS_pubr.Ticked         :=False;
 cb_AFS_pubw.Ticked         :=False;
 cb_DOS_system.Ticked       :=False;
 cb_DOS_read.Ticked         :=False;
 cb_DOS_hidden.Ticked       :=False;
 cb_DOS_archive.Ticked      :=False;
 cb_Sinclair_system.Ticked  :=False;
 cb_Sinclair_readonly.Ticked:=False;
 cb_Sinclair_archive.Ticked :=False;
 cb_Amiga_othd.Ticked       :=False;
 cb_Amiga_arch.Ticked       :=False;
 cb_Amiga_othe.Ticked       :=False;
 cb_Amiga_pure.Ticked       :=False;
 cb_Amiga_hold.Ticked       :=False;
 cb_Amiga_scri.Ticked       :=False;
 cb_Amiga_ownr.Ticked       :=False;
 cb_Amiga_othr.Ticked       :=False;
 cb_Amiga_ownw.Ticked       :=False;
 cb_Amiga_owne.Ticked       :=False;
 cb_Amiga_ownd.Ticked       :=False;
 cb_Amiga_othw.Ticked       :=False;
 cb_Amiga_pubw.Ticked       :=False;
 cb_Amiga_pubr.Ticked       :=False;
 cb_Amiga_pubd.Ticked       :=False;
 cb_Amiga_pube.Ticked       :=False;
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
 new_size_int : Int64=0; //Int64 will allow for huge disc sizes
 new_size_dec : Integer=0;
 level        : Integer=0;
 multiplier   : Integer=0;
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
//Order of the ToolBarContainer has changed
{------------------------------------------------------------------------------}
procedure TMainForm.ToolBarContainerChange(Sender: TObject);
{var
 index: Integer;}
begin
{ for index:=0 to ToolBarContainer.Bands.Count-1 do
  DIMReg.SetRegValS('ToolBar'+IntToStr(index)
            ,ToolBarContainer.Bands[index].Control.Name);}
end;

{------------------------------------------------------------------------------}
//Custom redraw event for the status bar
{------------------------------------------------------------------------------}
procedure TMainForm.ImageDetailsDrawPanel(StatusBar: TStatusBar;
 Panel: TStatusPanel; const Rect: TRect);
var
 png    : Byte=0;
 imgRect: TRect;
 Ltext  : String='';
// panwid : Integer;
begin
 //Set up the rectangle for the image - giving it 2px border
 imgRect.Top:=Rect.Top+3;
 imgRect.Left:=Rect.Left+3;
 imgRect.Height:=Rect.Height-6;
 imgRect.Width:=imgRect.Height;
 StatusBar.Canvas.Font.Color:=$000000;
 //First panel - we want to put the 'not saved' indicator here
 if(Panel.Index=0)and(HasChanged)then
  icons.StretchDraw(StatusBar.Canvas,changedicon,imgRect);
 //Second panel - needs a logo
 if(Panel.Index=1)and(Panel.Text<>'')then
 begin
{  inc(panwid,imgRect.Width+5);
  Rect.Width:=panwid;}
  png:=0;
  case Image.MajorFormatNumber of
   diAcornDFS : png:=bbclogo;       //BBC Micro logo for DFS
   diAcornADFS:
   begin
    if (Image.MinorFormatNumber<>3)
    and(Image.MapType=diADFSOldMap) then
     png:=acornlogo;                //Acorn logo for 8 bit ADFS
    if(Image.MinorFormatNumber=3)
    or(Image.MapType=diADFSNewMap) then
     png:=riscoslogo;               //RISC OS logo for 32 bit ADFS
   end;
   diCommodore: png:=commodorelogo; //Commodore logo
   diSinclair : png:=sinclairlogo;  //Sinclair logo
   diAmiga    : png:=amigalogo;     //Amiga logo
   diAcornUEF : png:=acornlogo;     //Acorn logo for CFS
   diMMFS     : png:=bbclogo;       //BBC Micro logo for MMFS
   diSpark    : png:=sparklogo;     //!SparkFS logo for Spark
   diAcornFS  : png:=bbclogo;       //BBC Micro logo for Acorn FS
   diAcornRFS : png:=romfslogo;     //Chip symbol for Acorn ROM FS
   diDOSPlus  :
    if Image.MinorFormatNumber<>0 then
     png:=msdoslogo                 //MS DOS logo for DOS
    else
     png:=bbcmasterlogo;            //BBC Master logo for DOS Plus
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
 //Panels 3 to 8
 if(Panel.Index>1)and(Panel.Index<8)then
  StatusBar.Canvas.TextRect(Rect,
                            Rect.Left+2,
                            Rect.Top+1,
                            Panel.Text,
                            StatusBar.Canvas.TextStyle);
 //Panel 9 - interleave type. If 'unknown', we want it blank
 if Panel.Index=8 then
 begin
  Ltext:=Panel.Text;
  if Ltext='unknown' then Ltext:='';
  StatusBar.Canvas.TextRect(Rect,
                            Rect.Left+2,
                            Rect.Top+1,
                            Ltext,
                            StatusBar.Canvas.TextStyle);
 end
// Panel.Width:=panwid;
end;

{------------------------------------------------------------------------------}
//Report an error to the user
{------------------------------------------------------------------------------}
procedure TMainForm.ReportError(error: String);
begin
 //Remove the top bit, if present
 RemoveTopBit(error);
 if Fguiopen then
 begin
  WriteToDebug('MainForm.ReportError('+error+')');
  if ErrorReporting then
   if Fstyling=RISCOSStyle then
    CustomDialogue.ShowError(error,'')
   else
    MessageDlg(error,mtError,[mbOK],0)
  else
   ErrorLogForm.ErrorLog.Lines.Add(error);
 end
 else if ErrorReporting then WriteLn(cmdRed+error+cmdNormal);
end;

{------------------------------------------------------------------------------}
//Ask the user for confirmation
{------------------------------------------------------------------------------}
function TMainForm.AskConfirm(confim: String; Buttons: array of String): TModalResult;
begin
 Result:=mrOK; //Default
 WriteToDebug('MainForm.AskConfirm('+confim+',array of String)');
 if ErrorReporting then
  if Fstyling=RISCOSStyle then
  begin
   CustomDialogue.ShowConfirm(confim,Buttons);
   Result:=CustomDialogue.ModalResult;
  end
  else
  begin
   if Length(Buttons)=1 then
    Result:=QuestionDlg('Confirmation',
                        confim,
                        mtConfirmation,
                        [mrOK,Buttons[0]],
                        0);
   if Length(Buttons)=2 then
    Result:=QuestionDlg('Confirmation',
                        confim,
                        mtConfirmation,
                        [mrOK,Buttons[0],
                         mrCancel,Buttons[1]],
                        0);
   if Length(Buttons)=3 then
    Result:=QuestionDlg('Confirmation',
                        confim,
                        mtConfirmation,
                        [mrOK,Buttons[0],
                         mrCancel,Buttons[1],
                         mrIgnore,Buttons[2]],
                        0);
   if Length(Buttons)=4 then
    Result:=QuestionDlg('Confirmation',
                        confim,
                        mtConfirmation,
                        [mrOK,Buttons[0],
                         mrCancel,Buttons[1],
                         mrIgnore,Buttons[2],
                         mrAbort,Buttons[3]],
                        0);
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
   if Fstyling=RISCOSStyle then
    CustomDialogue.ShowInfo(info,'')
   else
    MessageDlg(info,mtInformation,[mbOK],0);
end;

{------------------------------------------------------------------------------}
//Update the progress text
{------------------------------------------------------------------------------}
procedure TMainForm.UpdateProgress(Fupdate: String);
begin
 WriteToDebug('MainForm.UpdateProgress('+Fupdate+')');
 if Fguiopen then
 begin
  ProgressForm.UpdateProgress.Caption:=Fupdate;
  Application.ProcessMessages;
 end
 else WriteLn(Fupdate);
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
 if Fstyling=RISCOSStyle then
 begin
  b:=TBrush.Create;
  b.Bitmap:=GetTextureTile;
  c.Brush :=b;
  c.FillRect(rc);
  b.Free;
 end;
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
 i  : Integer=0;
 x  : Integer=0;
 sc : TScrollBox=nil;
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
 FTDialogue.Width:=Round((64*5)*(Screen.PixelsPerInch/DesignedDPI));
 FTDialogue.Position:=poMainFormCenter;
 //Scrollbox
 sc:=TScrollbox.Create(FTDialogue);
 sc.Parent:=FTDialogue;
 sc.Align:=alClient;
 sc.BorderStyle:=bsSingle;
 sc.OnPaint:=@FileInfoPanelPaint;
 //Controls
 SetLength(FTButtons,High(RISCOSFileTypes)-Low(RISCOSFileTypes));
 i:=Length(FTButtons);
 x:=Low(RISCOSFileTypes);
 for i:=x to High(RISCOSFileTypes) do
 begin
  FTButtons[i-x]:=TSpeedButton.Create(sc);
  FTButtons[i-x].Parent:=sc;
  FTButtons[i-x].Width:=Round(64*(Screen.PixelsPerInch/DesignedDPI));
  FTButtons[i-x].Height:=Round(64*(Screen.PixelsPerInch/DesignedDPI));
  FTButtons[i-x].Caption:=Image.GetFileType(StrToInt('$'+RISCOSFileTypes[i]));
  FTButtons[i-x].Tag:=StrToInt('$'+RISCOSFileTypes[i]);
  FTButtons[i-x].Left:=((i-x)mod 5)*FTButtons[i-x].Width;
  FTButtons[i-x].Top :=((i-x)div 5)*FTButtons[i-x].Height;
  FTButtons[i-x].Flat:=True;
  FTButtons[i-x].GroupIndex:=1;
  FTButtons[i-x].Images:=FileImages;
  FTButtons[i-x].ImageWidth:=Round(32*(Screen.PixelsPerInch/DesignedDPI));
  FTButtons[i-x].Layout:=blGlyphTop;
  FTButtons[i-x].ImageIndex:=i;
  FTButtons[i-x].OnClick:=@FileTypeClick;
 end;
 //Create a dummy button, for no selection
 FTDummyBtn:=TSpeedButton.Create(sc);
 FTDummyBtn.Parent:=sc;
 FTDummyBtn.GroupIndex:=1;
 FTDummyBtn.Visible:=False;
 //Custom filetype
 FTEdit:=TEdit.Create(sc);
 FTEdit.Parent:=sc;
 FTEdit.Width:=Round(64*(Screen.PixelsPerInch/DesignedDPI));
 FTEdit.Text:='';
 FTEdit.Font.Name:='Courier New';
 FTEdit.Alignment:=taCenter;
 i:=Round(64*(Screen.PixelsPerInch/DesignedDPI));
 FTEdit.Top:=(((Length(FTButtons)+1)div 5)*i)+Round((i-FTEdit.Height)/2);
 FTEdit.Left:=((Length(FTButtons)+1)mod 5)*FTEdit.Width;
 FTEdit.OnKeyPress:=@FileTypeKeyPress;
end;

{------------------------------------------------------------------------------}
//Write to the debug file
{------------------------------------------------------------------------------}
procedure TMainForm.WriteToDebug(line: String);
var
 F : TFileStream=nil;
begin
 if Fdebug then
 begin
  try
   if FileExists(debuglogfile) then
    F:=TFileStream.Create(debuglogfile,fmOpenWrite or fmShareDenyNone)
   else
    F:=TFileStream.Create(debuglogfile,fmCreate or fmShareDenyNone);
   F.Position:=F.Size;
   WriteLine(F,FormatDateTime(TimeDateFormat,Now)+': '+line);
  except
   on E: Exception do ShowMessage('Failed to write logfile "'+debuglogfile
                                  +'": '+E.Message);
  end;
  F.Free;
 end;
end;

{------------------------------------------------------------------------------}
//Create a RISC OS button
{------------------------------------------------------------------------------}
function TMainForm.CreateButton(Lparent: TControl; Lcaption: String;LDef: Boolean;
                         LLeft,LTop: Integer; LModal: TModalResult): TRISCOSButton;
begin
 Result:=TRISCOSButton.Create(Lparent as TControl);
 Result.Parent:=Lparent as TWinControl;
 Result.Default:=LDef;
 Result.Caption:=Lcaption;
 Result.Left:=LLeft;
 Result.Top:=LTop;
 Result.ModalResult:=LModal;
 Result.Font.Color:=clBlack;
end;

end.
