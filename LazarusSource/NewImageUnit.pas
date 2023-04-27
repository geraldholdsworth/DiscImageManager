unit NewImageUnit;

{
Copyright (C) 2018-2023 Gerald Holdsworth gerald@hollypops.co.uk

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,ExtCtrls,StdCtrls,ComCtrls,
 Math,GJHCustomComponents,DiscImage;

type
 { TNewImageForm }

 TNewImageForm = class(TForm)
  ADFSLabel: TLabel;
  AFSSize: TPanel;
  AFSSizeLabel: TLabel;
  DOS: TPanel;
  AFS: TPanel;
  DOSLabel: TLabel;
  AFSLabel: TLabel;
  C64Label: TLabel;
  AmigaLabel: TLabel;
  Amiga: TPanel;
  AFSImageSizeLabel: TLabel;
  SpectrumLabel: TLabel;
  Spectrum: TPanel;
  DFSTracksLabel: TLabel;
  DFS: TPanel;
  DFSTracks: TPanel;
  ADFS: TPanel;
  C64: TPanel;
  SystemLabel: TLabel;
  MainFormatPanel: TPanel;
  DFSLabel: TLabel;
  AFSImageSize: TGJHSlider;
  cb_AFScreatepword: TGJHTickBox;
  btn_OK,
  btn_Cancel: TGJHButton;
  procedure AFSClick(Sender: TObject);
  procedure AFSImageSizeChange(Sender: TObject);
  procedure btn_OKClick(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure MainFormatClick(Sender: TObject);
 private
  const
   FFormats: array[0..8] of String=       ('Disc Filing System (DFS)',
                                           'Advanced Disc Filing System (ADFS)',
                                           'Commodore 64/128',
                                           'Sinclair Spectrum +3/Amstrad',
                                           'Commodore Amiga',
                                           'Cassette Filing System (CFS)',
                                           '!Spark archive',
                                           'Acorn File Server (AFS0)',
                                           'DOS Plus/DOS');
   DFSFormats: array[0..3] of String=     ('Acorn single sided',
                                           'Acorn double sided',
                                           'Watford single sided',
                                           'Watford double sided');
   DFSTrackStrings: array[0..1] of String=('40 Track','80 Track');
   ADFSFormats: array[0..8] of String=    ('S (160K, Old Map, Old Directory)',
                                           'M (320K, Old Map, Old Directory)',
                                           'L (640K, Old Map, Old Directory)',
                                           'D (800K, Old Map, New Directory)',
                                           'E (800K, New Map, New Directory)',
                                           'E+ (800K, New Map, Big Directory)',
                                           'F (1.6M, New Map, New Directory)',
                                           'F+ (1.6M, New Map, Big Directory)',
                                           'Hard Disc');
   C64Formats: array[0..2] of String=     ('1541','1571','1581');
   SpectrumFormats: array[0..1] of String=('DSK','Extended DSK');
   AmigaFormats: array[0..2] of String=   ('AmigaDOS DD',
                                           'AmigaDOS HD',
                                           'AmigaDOS Hard Disc');
   DOSFormats: array[0..6] of String=     ('640KB ADFS/DOS Plus',
                                           '800KB DOS Plus',
                                           '360KB',
                                           '720KB',
                                           '1.44MB',
                                           '2.88MB',
                                           'DOS Hard Drive');
   AFSFormats: array[0..2] of String=     ('Level 2',
                                           'Level 3 (<1988)',
                                           'Level 3 (>1988)');
 public
  harddrivesize : Cardinal;
  newmap,
  addheader,
  ide           : Boolean;
  dirtype,
  fat           : Byte;
  SystemOptions,
  DFSOptions,
  DFSTOptions,
  ADFSOptions,
  C64Options,
  SpecOptions,
  AmigaOptions,
  DOSOptions,
  AFSOptions    : array of TGJHRadioBox;
 end;

var
 NewImageForm: TNewImageForm;

implementation

{$R *.lfm}

uses MainUnit,HardDriveUnit;

{ TNewImageForm }

{-------------------------------------------------------------------------------
Options have changed
-------------------------------------------------------------------------------}
procedure TNewImageForm.MainFormatClick(Sender: TObject);
begin
 //Show or hide the appropriate secondary panels
 DFS.Visible          :=SystemOptions[0].Ticked;
 ADFS.Visible         :=SystemOptions[1].Ticked;
 C64.Visible          :=SystemOptions[2].Ticked;
 Spectrum.Visible     :=SystemOptions[3].Ticked;
 Amiga.Visible        :=SystemOptions[4].Ticked;
 AFS.Visible          :=SystemOptions[7].Ticked;
 DOS.Visible          :=SystemOptions[8].Ticked;
 //And tertiary panels
 DFSTracks.Visible :=DFS.Visible;
 AFSSize.Visible   :=AFS.Visible;
 //Currently, only certain types of format can be created
 btn_OK.Enabled:=(SystemOptions[0].Ticked) //DFS
               OR(SystemOptions[1].Ticked) //ADFS
               OR(SystemOptions[2].Ticked) //C64
               OR(SystemOptions[4].Ticked) //Amiga
               OR(SystemOptions[5].Ticked) //CFS
               OR(SystemOptions[6].Ticked) //Spark
               OR(SystemOptions[7].Ticked) //AFS
               OR(SystemOptions[8].Ticked);//DOS
end;

{-------------------------------------------------------------------------------
Form is being displayed
-------------------------------------------------------------------------------}
procedure TNewImageForm.FormShow(Sender: TObject);
begin
 //Reset to the default options
 SystemOptions[0].Ticked:=True;
 DFSOptions[0].Ticked   :=True;
 DFSTOptions[0].Ticked  :=True;
 ADFSOptions[0].Ticked  :=True;
 C64Options[0].Ticked   :=True;
 SpecOptions[0].Ticked  :=True;
 AmigaOptions[0].Ticked :=True;
 AFSOptions[0].Ticked   :=True;
 DOSOptions[0].Ticked   :=True;
 cb_AFScreatepword.Ticked:=False;
 AFSImageSize.Position:=AFSImageSize.Min;
 AFSClick(Sender);
 AFSImageSizeChange(Sender);
 //Hide all the sub options, except for the first one
 DFS.Visible         :=True;
 DFSTracks.Visible   :=True;
 ADFS.Visible        :=False;
 C64.Visible         :=False;
 Spectrum.Visible    :=False;
 Amiga.Visible       :=False;
 AFS.Visible         :=False;
 AFSSize.Visible     :=False;
 DOS.Visible         :=False;
 //Enable the create button
 btn_OK.Enabled      :=True;
end;

{-------------------------------------------------------------------------------
User has clicked on create
-------------------------------------------------------------------------------}
procedure TNewImageForm.btn_OKClick(Sender: TObject);
var
 ok: Boolean;
begin
 ok:=True;
 //Are we creating a hard drive?
 if((SystemOptions[1].Ticked)AND(ADFSOptions[8].Ticked))     //ADFS
 or((SystemOptions[8].Ticked)AND(DOSOptions[6].Ticked))      //DOS
 or((SystemOptions[4].Ticked)AND(AmigaOptions[2].Ticked))then//Amiga
 begin
  //Then we need to open the additional dialogue to configure this
  HardDriveForm.ADFSHDD :=SystemOptions[1].Ticked; //Set to ADFS
  HardDriveForm.DOSHDD  :=SystemOptions[8].Ticked; //Set to DOS
  HardDriveForm.AmigaHDD:=SystemOptions[4].Ticked; //Set to Amiga
  HardDriveForm.ShowModal;
  ok:=HardDriveForm.ModalResult=mrOK;
  if ok then
  begin
   //Selected hard drive size in MB
   harddrivesize:=HardDriveForm.CapacitySlider.Position*1024*1024;
   addheader:=HardDriveForm.cb_AddHeader.Ticked;
   ide:=HardDriveForm.cb_IDE.Ticked;
   if SystemOptions[1].Ticked then //ADFS Specific
   begin
    //New or old map
    newmap:=HardDriveForm.cb_NewMap.Ticked;
    //Directory type
    dirtype:=diADFSOldDir;
    if HardDriveForm.rb_NewDir.Ticked then dirtype:=diADFSNewDir;
    if HardDriveForm.rb_BigDir.Ticked then dirtype:=diADFSBigDir;
   end;
   if SystemOptions[4].Ticked then //Amiga Specific
   begin
    {fat:=diFAT12;
    if HardDriveForm.rb_FAT16.Checked then fat:=diFAT16;
    if HardDriveForm.rb_FAT32.Checked then fat:=diFAT32;}
   end;
   if SystemOptions[8].Ticked then //DOS Specific
   begin
    fat:=diFAT12;
    if HardDriveForm.rb_FAT16.Ticked then fat:=diFAT16;
    if HardDriveForm.rb_FAT32.Ticked then fat:=diFAT32;
   end;
  end;
 end;
 //Return to the calling form
 if ok then btn_OK.ModalResult:=mrOK else btn_OK.ModalResult:=mrNone;
end;

{-------------------------------------------------------------------------------
Form creation
-------------------------------------------------------------------------------}
procedure TNewImageForm.FormCreate(Sender: TObject);
var
 Index,
 LWid: Integer;
 ratio: Real;
procedure CreateRadioBoxes(var LLabel: TLabel; var LPanel: TPanel;
          var LOptions: array of TGJHRadioBox;var LStrings: array of String);
var
 LH,
 Index: Integer;
begin
 //Keep track of the Y position
 LH:=LLabel.Top+LLabel.Height+4;
 //Create each control
 for Index:=0 to Length(LStrings)-1 do
 begin
  LOptions[Index]:=TGJHRadioBox.Create(LPanel as TControl);
  LOptions[Index].Parent:=LPanel as TWinControl;
  LOptions[Index].Left:=8;
  LOptions[Index].Top:=LH;
  LOptions[Index].Caption:=LStrings[Index];
  LOptions[Index].Tag:=Index;
  inc(LH,LOptions[Index].Height+4);
  if LOptions[Index].Width>LWid then LWid:=LOptions[Index].Width;
 end;
 //Align the panel
 LPanel.Height:=LH-LLabel.Top;
end;
procedure RepositionPanel(LPanel: TPanel);
begin
 LPanel.Width:=LWid;
 LPanel.Top:=0;
 LPanel.Left:=LWid;
end;
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 LWid:=0;//Find the widest string
 //System Format ---------------------------------------------------------------
 //Set up the control array
 SetLength(SystemOptions,Length(FFormats));
 //Create the controls
 CreateRadioBoxes(SystemLabel,MainFormatPanel,SystemOptions,FFormats);
 //Set the OnChange
 for Index:=0 to Length(FFormats)-1 do
  SystemOptions[Index].OnChange:=@MainFormatClick;
 //DFS Format ------------------------------------------------------------------
 //Set up the control array
 SetLength(DFSOptions,Length(DFSFormats));
 //Create the controls
 CreateRadioBoxes(DFSLabel,DFS,DFSOptions,DFSFormats);
 //DFS Tracks ------------------------------------------------------------------
 //Set up the control array
 SetLength(DFSTOptions,Length(DFSTrackStrings));
 //Create the controls
 CreateRadioBoxes(DFSTracksLabel,DFSTracks,DFSTOptions,DFSTrackStrings);
 //ADFS Format -----------------------------------------------------------------
 //Set up the control array
 SetLength(ADFSOptions,Length(ADFSFormats));
 //Create the controls
 CreateRadioBoxes(ADFSLabel,ADFS,ADFSOptions,ADFSFormats);
 //C64 Format ------------------------------------------------------------------
 //Set up the control array
 SetLength(C64Options,Length(C64Formats));
 //Create the controls
 CreateRadioBoxes(C64Label,C64,C64Options,C64Formats);
 //Spectrum Format -------------------------------------------------------------
 //Set up the control array
 SetLength(SpecOptions,Length(SpectrumFormats));
 //Create the controls
 CreateRadioBoxes(SpectrumLabel,Spectrum,SpecOptions,SpectrumFormats);
 //Amiga Format ----------------------------------------------------------------
 //Set up the control array
 SetLength(AmigaOptions,Length(AmigaFormats));
 //Create the controls
 CreateRadioBoxes(AmigaLabel,Amiga,AmigaOptions,AmigaFormats);        
 //DOS Format ------------------------------------------------------------------
 //Set up the control array
 SetLength(DOSOptions,Length(DOSFormats));
 //Create the controls
 CreateRadioBoxes(DOSLabel,DOS,DOSOptions,DOSFormats);
 //AFS Format ------------------------------------------------------------------
 //Set up the control array
 SetLength(AFSOptions,Length(AFSFormats));
 //Create the controls
 CreateRadioBoxes(AFSLabel,AFS,AFSOptions,AFSFormats);
 //Set the OnChange
 for Index:=0 to Length(AFSFormats)-1 do
  AFSOptions[Index].OnChange:=@AFSClick;
 //Create the sizing controls
 AFSImageSize:=TGJHSlider.Create(AFSSize as TControl);
 AFSImageSize.Parent:=AFSSize as TWinControl;
 AFSImageSize.Visible:=True;
 AFSImageSize.Top:=AFSImageSizeLabel.Top+AFSImageSizeLabel.Height;
 AFSImageSize.Align:=alTop;
 AFSImageSize.Height:=Round(30*ratio);
 AFSImageSize.Min:=40;
 AFSImageSize.Max:=52428;
 AFSImageSize.Position:=40;
 AFSImageSize.OnChange:=@AFSImageSizeChange;
 AFSImageSize.Orientation:=csHorizontal;
 AFSImageSize.Pointers:=False;
 AFSImageSize.Outline:=csOutInner;
 cb_AFScreatepword:=TGJHTickBox.Create(AFSSize as TControl);
 cb_AFScreatepword.Parent:=AFSSize as TWinControl;
 cb_AFScreatepword.Visible:=True;
 cb_AFScreatepword.Caption:='Create password file';
 cb_AFScreatepword.Left:=Round(8*ratio);
 cb_AFScreatepword.Top:=AFSImageSize.Top+AFSImageSize.Height+Round(4*ratio);
 AFSSize.Height:=cb_AFScreatepword.Top+cb_AFScreatepword.Height;
 //Adjust the panel widths and left positions ----------------------------------
 inc(LWid,8);
 MainFormatPanel.Width:=LWid;
 MainFormatPanel.Top:=0;
 MainFormatPanel.Left:=0;
 RepositionPanel(DFS);
 DFSTracks.Width:=LWid;
 DFSTracks.Top:=DFS.Top+DFS.Height;
 DFSTracks.Left:=LWid;
 RepositionPanel(ADFS);
 RepositionPanel(C64);
 RepositionPanel(Spectrum);
 RepositionPanel(Amiga);
 RepositionPanel(DOS);
 RepositionPanel(AFS);
 AFSSize.Top:=AFS.Top+AFS.Height;
 AFSSize.Left:=LWid;
 AFSSize.Width:=LWid;
 //Align the buttons -----------------------------------------------------------
 btn_Cancel:=MainForm.CreateButton(NewImageForm as TControl,'Cancel',False,0,
                      MainFormatPanel.Top+MainFormatPanel.Height+Round(8*ratio),
                      mrCancel);
 btn_OK:=MainForm.CreateButton(NewImageForm as TControl,'Create',True,0,
                               btn_Cancel.Top-Round(4*ratio),mrNone);
 btn_OK.Left:=(LWid*2)-btn_OK.Width-Round(8*ratio);
 btn_OK.OnClick:=@btn_OKClick;
 btn_Cancel.Left:=btn_OK.Left-Round(4*ratio)-btn_Cancel.Width;
 //Adjust the form size
 Height:=btn_OK.Top+btn_OK.Height+Round(8*ratio);
 Width:=LWid*2;
end;

{-------------------------------------------------------------------------------
The AFS capacity slider is changing
-------------------------------------------------------------------------------}
procedure TNewImageForm.AFSImageSizeChange(Sender: TObject);
begin
 if AFSImageSize.Position<=409 then
  AFSImageSizeLabel.Caption:=IntToStr(AFSImageSize.Position*10)+'KB'
 else
 begin
  AFSImageSize.Position:=Round((Ceil((AFSImageSize.Position*10)/1024)<<10)/10);
  AFSImageSizeLabel.Caption:=IntToStr(Ceil((AFSImageSize.Position*10)/1024))+'MB';
 end;
end;

{-------------------------------------------------------------------------------
The AFS Level has changed, change the minimum size
-------------------------------------------------------------------------------}
procedure TNewImageForm.AFSClick(Sender: TObject);
begin
 if AFSOptions[0].Ticked then
 begin
  AFSImageSize.Min:=40;  //Level 2 minimum size 400K
  AFSImageSize.Max:=102; //Level 2 maximum size is 1023K (1MB)
 end;
 if(AFSOptions[1].Ticked)or(AFSOptions[2].Ticked)then
 begin
  AFSImageSize.Min:=64;    //Level 3 minimum size 640K
  AFSImageSize.Max:=13107; //Level 3 temporary max is ~128MB
  //AFSImageSize.Max:=52428; //Level 3 maximum size is 524280K (~512MB)
 end;
 AFSImageSizeChange(Sender);
end;

{-------------------------------------------------------------------------------
Texturise the form
-------------------------------------------------------------------------------}
procedure TNewImageForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

end.
