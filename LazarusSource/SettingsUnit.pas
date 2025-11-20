unit SettingsUnit;

{
Copyright (C) 2018-2025 Gerald Holdsworth gerald@hollypops.co.uk

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,StdCtrls,ExtCtrls,
 GJHCustomComponents;

type

 { TSettingsForm }

 TSettingsForm = class(TForm)
  Border2: TImage;
  Border3: TImage;
  Border4: TImage;
  Border5: TImage;
  DFSPanel: TPanel;
  DFSPanelLabel: TLabel;
  Border1: TImage;
  InterleavePanel: TPanel;
  InterleaveLabel: TLabel;
  MiscLabel: TLabel;
  MiscPanel: TPanel;
  TexturePanelLabel: TLabel;
  TexturePanel: TPanel;
  TilePictureIyonix: TImage;
  TilePictureROPi: TImage;
  TilePictureRO5: TImage;
  TilePictureRO4: TImage;
  TilePictureRO3: TImage;
  styleType: TPanel;
  NoTile,
  TileRO3,
  TileRO4,
  TileRO5,
  TileIyonix,
  TileROPi,
  ilAuto,
  ilSeq,
  ilInter,
  ilMPX,
  styleNative,
  styleRISCOS: TRISCOSRadioBox;
  DFSBeyondEdge,
  AllowDFSZeroSecs,
  AllowDFSBlankFilenames,
  CreateINF,
  ImpliedAttr,
  WriteDebug,
  CompressUEF,
  ScanSubDirs,
  OpenDOS,
  CreateDSC: TRISCOSTickBox;
  CancelButton,
  OKButton: TRISCOSButton;
  procedure FormCreate(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure TilePictureRO5Click(Sender: TObject);
 private

 public

 end;

var
 SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TSettingsForm }

{------------------------------------------------------------------------------}
//Show the form - set it up
{------------------------------------------------------------------------------}
procedure TSettingsForm.FormShow(Sender: TObject);
begin
 //Style the buttons
 OKButton.NativeOS              :=MainForm.Fstyling=MainForm.NativeStyle;
 CancelButton.NativeOS          :=MainForm.Fstyling=MainForm.NativeStyle;
 NoTile.NativeOS                :=MainForm.Fstyling=MainForm.NativeStyle;
 TileRO3.NativeOS               :=MainForm.Fstyling=MainForm.NativeStyle;
 TileRO4.NativeOS               :=MainForm.Fstyling=MainForm.NativeStyle;
 TileRO5.NativeOS               :=MainForm.Fstyling=MainForm.NativeStyle;
 TileIyonix.NativeOS            :=MainForm.Fstyling=MainForm.NativeStyle;
 TileROPi.NativeOS              :=MainForm.Fstyling=MainForm.NativeStyle;
 ilAuto.NativeOS                :=MainForm.Fstyling=MainForm.NativeStyle;
 ilSeq.NativeOS                 :=MainForm.Fstyling=MainForm.NativeStyle;
 ilInter.NativeOS               :=MainForm.Fstyling=MainForm.NativeStyle;
 ilMPX.NativeOS                 :=MainForm.Fstyling=MainForm.NativeStyle;
 styleNative.NativeOS           :=MainForm.Fstyling=MainForm.NativeStyle;
 styleRISCOS.NativeOS           :=MainForm.Fstyling=MainForm.NativeStyle;
 DFSBeyondEdge.NativeOS         :=MainForm.Fstyling=MainForm.NativeStyle;
 AllowDFSZeroSecs.NativeOS      :=MainForm.Fstyling=MainForm.NativeStyle;
 AllowDFSBlankFilenames.NativeOS:=MainForm.Fstyling=MainForm.NativeStyle;
 CreateINF.NativeOS             :=MainForm.Fstyling=MainForm.NativeStyle;
 ImpliedAttr.NativeOS           :=MainForm.Fstyling=MainForm.NativeStyle;
 WriteDebug.NativeOS            :=MainForm.Fstyling=MainForm.NativeStyle;
 CompressUEF.NativeOS           :=MainForm.Fstyling=MainForm.NativeStyle;
 ScanSubDirs.NativeOS           :=MainForm.Fstyling=MainForm.NativeStyle;
 OpenDOS.NativeOS               :=MainForm.Fstyling=MainForm.NativeStyle;
 CreateDSC.NativeOS             :=MainForm.Fstyling=MainForm.NativeStyle;
 //Re-align the buttons
 CancelButton.Top     :=OKButton.Top+(OKButton.Height-CancelButton.Height)div 2;
 TilePictureRO5.Picture.Bitmap   :=MainForm.GetTextureTile(1);
 TilePictureRO4.Picture.Bitmap   :=MainForm.GetTextureTile(2);
 TilePictureRO3.Picture.Bitmap   :=MainForm.GetTextureTile(3);
 TilePictureIyonix.Picture.Bitmap:=MainForm.GetTextureTile(4);
 TilePictureROPi.Picture.Bitmap  :=MainForm.GetTextureTile(5);
end;

{------------------------------------------------------------------------------}
//Paint the form with the texture
{------------------------------------------------------------------------------}
procedure TSettingsForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

{------------------------------------------------------------------------------}
//Create the form
{------------------------------------------------------------------------------}
procedure TSettingsForm.FormCreate(Sender: TObject);
 function CreateRadioBox(text: String;prt: TPanel): TRISCOSRadioBox;
 begin
  Result:=TRISCOSRadioBox.Create(prt as TControl);
  Result.Parent:=prt as TWinControl;
  Result.Visible:=True;
  Result.Caption:=text;
  Result.Font.Color:=clBlack;
 end;
 function CreateTickBox(text: String;prt: TPanel): TRISCOSTickBox;
 begin
  Result:=TRISCOSTickBox.Create(prt as TControl);
  Result.Parent:=prt as TWinControl;
  Result.Visible:=True;
  Result.Caption:=text;
  Result.Font.Color:=clBlack;
 end;
var
 LRefWid: Integer=0;
 ratio  : Real=0;
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 //Create the texture radio boxes, and move the tiles around
 NoTile:=CreateRadioBox('No Tile',TexturePanel);
 NoTile.Top:=TexturePanelLabel.Top+TexturePanelLabel.Height+Round(4*ratio);
 NoTile.Left:=TilePictureRO3.Left;
 TileRO3:=CreateRadioBox('RISC OS 3',TexturePanel);
 TileRO4:=CreateRadioBox('RISC OS 4',TexturePanel);
 TileRO5:=CreateRadioBox('RISC OS 5',TexturePanel);
 TileIyonix:=CreateRadioBox('Iyonix',TexturePanel);
 TileROPi:=CreateRadioBox('RISC OS Pi',TexturePanel);
 //Move and resize the tiles - borders first
 LRefWid:=TileROPi.Width+Round(32*ratio);
 Border1.Top:=NoTile.Top+NoTile.Height+Round(4*ratio);
 Border1.Width:=LRefWid;
 Border1.Height:=LRefWid;
 Border1.Left:=NoTile.Left;
 Border2.Top:=Border1.Top;
 Border2.Width:=LRefWid;
 Border2.Height:=LRefWid;
 Border2.Left:=Border1.Left+Border1.Width+Round(4*ratio);
 Border3.Top:=Border1.Top;
 Border3.Width:=LRefWid;
 Border3.Height:=LRefWid;
 Border3.Left:=Border2.Left+Border2.Width+Round(4*ratio);
 Border4.Top:=Border1.Top;
 Border4.Width:=LRefWid;
 Border4.Height:=LRefWid;
 Border4.Left:=Border3.Left+Border3.Width+Round(4*ratio);
 Border5.Top:=Border1.Top;
 Border5.Width:=LRefWid;
 Border5.Height:=LRefWid;
 Border5.Left:=Border4.Left+Border4.Width+Round(4*ratio);
 TilePictureRO3.Top:=Border1.Top;
 TilePictureRO3.Left:=Border1.Left;
 TilePictureRO3.Width:=LRefWid;
 TilePictureRO3.Height:=LRefWid;
 TilePictureRO4.Top:=Border2.Top;
 TilePictureRO4.Left:=Border2.Left;
 TilePictureRO4.Width:=LRefWid;
 TilePictureRO4.Height:=LRefWid;
 TilePictureRO5.Top:=Border3.Top;
 TilePictureRO5.Left:=Border3.Left;
 TilePictureRO5.Width:=LRefWid;
 TilePictureRO5.Height:=LRefWid;
 TilePictureIyonix.Top:=Border4.Top;
 TilePictureIyonix.Left:=Border4.Left;
 TilePictureIyonix.Width:=LRefWid;
 TilePictureIyonix.Height:=LRefWid;
 TilePictureROPi.Top:=Border5.Top;
 TilePictureROPi.Left:=Border5.Left;
 TilePictureROPi.Width:=LRefWid;
 TilePictureROPi.Height:=LRefWid;
 //Move the radio boxes around
 TileRO3.Top:=TilePictureRO3.Top+TilePictureRO3.Height+Round(4*ratio);
 TileRO3.Left:=TilePictureRO3.Left;
 TileRO4.Top:=TilePictureRO4.Top+TilePictureRO4.Height+Round(4*ratio);
 TileRO4.Left:=TilePictureRO4.Left;
 TileRO5.Top:=TilePictureRO5.Top+TilePictureRO5.Height+Round(4*ratio);
 TileRO5.Left:=TilePictureRO5.Left;
 TileIyonix.Top:=TilePictureIyonix.Top+TilePictureIyonix.Height+Round(4*ratio);
 TileIyonix.Left:=TilePictureIyonix.Left;
 TileROPi.Top:=TilePictureROPi.Top+TilePictureROPi.Height+Round(4*ratio);
 TileROPi.Left:=TilePictureROPi.Left;
 TexturePanel.Height:=TileROPi.Top+TileROPi.Height+Round(8*ratio);
 //Resize the form and panels to accomodate the above
 TexturePanel.ClientWidth:=LRefWid+TileROPi.Left+Round(4*ratio);
 SettingsForm.Width:=TexturePanel.Width;
 styleType.Width:=TexturePanel.Width;
 DFSPanel.Width:=SettingsForm.Width div 2;
 DFSPanel.Left:=SettingsForm.Width div 2;
 InterleavePanel.Left:=0;
 InterleavePanel.Width:=SettingsForm.Width div 2;
 MiscPanel.Width:=TexturePanel.Width;
 //Style selection
 styleNative:=CreateRadioBox('Native OS',styleType);
 styleRISCOS:=CreateRadioBox('RISC OS',styleType);
 styleNative.Top:=0;
 styleNative.Left:=Round(4*ratio);
 styleRISCOS.Top:=0;
 styleRISCOS.Left:=(styleType.Width div 2)+Round(4*ratio);
 styleType.Height:=styleNative.Height+Round(8*ratio);
 //Create the DFS panel tick boxes
 DFSBeyondEdge:=CreateTickBox('Allow files goes over disc edge',DFSPanel);
 DFSBeyondEdge.Top:=DFSPanelLabel.Top+DFSPanelLabel.Height+Round(4*ratio);
 DFSBeyondEdge.Left:=Round(4*ratio);
 AllowDFSZeroSecs:=CreateTickBox('Allow zero sectors',DFSPanel);
 AllowDFSZeroSecs.Top:=DFSBeyondEdge.Top+DFSBeyondEdge.Height+Round(4*ratio);
 AllowDFSZeroSecs.Left:=Round(4*ratio);
 AllowDFSBlankFilenames:=CreateTickBox('Allow blank filenames',DFSPanel);
 AllowDFSBlankFilenames.Top:=AllowDFSZeroSecs.Top+AllowDFSZeroSecs.Height+Round(4*ratio);
 AllowDFSBlankFilenames.Left:=Round(4*ratio);
 DFSPanel.Height:=AllowDFSBlankFilenames.Top+AllowDFSBlankFilenames.Height+Round(8*ratio);
 //Create the Interleave panel radio boxes
 ilAuto:=CreateRadioBox('Automatic (recommended)',InterleavePanel);
 ilAuto.Left:=Round(4*ratio);
 ilAuto.Top:=InterleaveLabel.Top+InterleaveLabel.Height+Round(4*ratio);
 ilSeq:=CreateRadioBox('Sequential (0,1,2,...80,81,82,...)',InterleavePanel);
 ilSeq.Left:=Round(4*ratio);
 ilSeq.Top:=ilAuto.Top+ilAuto.Height+Round(4*ratio);
 ilInter:=CreateRadioBox('Interleaved (0,80,1,81,2,82,...)',InterleavePanel);
 ilInter.Left:=Round(4*ratio);
 ilInter.Top:=ilSeq.Top+ilSeq.Height+Round(4*ratio);
 ilMPX:=CreateRadioBox('Multiplexed (0,2,4,...1,3,5,...)',InterleavePanel);
 ilMPX.Left:=Round(4*ratio);
 ilMPX.Top:=ilInter.Top+ilInter.Height+Round(4*ratio);
 InterleavePanel.Height:=ilMPX.Top+ilMPX.Height+Round(8*ratio);
 //Create the Misc panel tick boxes
 CreateINF:=CreateTickBox('Create *.inf files on download',MiscPanel);
 CreateINF.Top:=MiscLabel.Top+MiscLabel.Height+Round(4*ratio);
 CreateINF.Left:=Round(4*ratio);
 ImpliedAttr:=CreateTickBox('Add Implied Attributes (DFS/CFS/RFS)',MiscPanel);
 ImpliedAttr.Top:=CreateINF.Top+CreateINF.Height+Round(4*ratio);
 ImpliedAttr.Left:=Round(4*ratio);
 WriteDebug:=CreateTickBox('Write debugging information (slow)',MiscPanel);
 WriteDebug.Top:=ImpliedAttr.Top+ImpliedAttr.Height+Round(4*ratio);
 WriteDebug.Left:=Round(4*ratio);
 CompressUEF:=CreateTickBox('Compress UEF images',MiscPanel);
 CompressUEF.Top:=WriteDebug.Top+WriteDebug.Height+Round(4*ratio);
 CompressUEF.Left:=Round(4*ratio);
 ScanSubDirs:=CreateTickBox('Scan sub-directories on open',MiscPanel);
 ScanSubDirs.Top:=CreateINF.Top;
 ScanSubDirs.Left:=(MiscPanel.Width div 2)+Round(4*ratio);
 OpenDOS:=CreateTickBox('Open DOS partitions on ADFS',MiscPanel);
 OpenDOS.Top:=ScanSubDirs.Top+ScanSubDirs.Height+Round(4*ratio);
 OpenDOS.Left:=ScanSubDirs.Left;
 CreateDSC:=CreateTickBox('Create *.dsc files with new ADFS HDD',MiscPanel);
 CreateDSC.Top:=OpenDOS.Top+OpenDOS.Height+Round(4*ratio);
 CreateDSC.Left:=OpenDOS.Left;
 MiscPanel.Height:=CompressUEF.Top+CompressUEF.Height+Round(8*ratio);
 //Move the panels up/down
 styleType.Top:=TexturePanel.Top+TexturePanel.Height;
 DFSPanel.Top:=styleType.Top+styleType.Height;
 InterleavePanel.Top:=DFSPanel.Top;
 MiscPanel.Top:=InterleavePanel.Top+InterleavePanel.Height;
 //Create the buttons
 OKButton:=MainForm.CreateButton(SettingsForm as TControl,
                                 'OK',True,0,
                                 MiscPanel.Top+MiscPanel.Height+Round(4*ratio),
                                 mrOK);
 OKButton.Left:=Width-OKButton.Width-Round(8*ratio);
 CancelButton:=MainForm.CreateButton(SettingsForm as TControl,
                                     'Cancel',False,0,
                                     OKButton.Top+Round(4*ratio),
                                     mrCancel);
 CancelButton.Left:=OKButton.Left-CancelButton.Width-Round(8*ratio);
 //Resize the form
 SettingsForm.Height:=OKButton.Top+OKButton.Height+Round(8*ratio);
end;

{------------------------------------------------------------------------------}
//User has clicked on a picture rather than the radio button
{------------------------------------------------------------------------------}
procedure TSettingsForm.TilePictureRO5Click(Sender: TObject);
begin
 if Sender is TImage then
 begin
  if(TImage(Sender)=TilePictureRO3)
  or(TImage(Sender)=Border1)          then TileRO3.Ticked:=True;
  if(TImage(Sender)=TilePictureRO4)
  or(TImage(Sender)=Border2)          then TileRO4.Ticked:=True;
  if(TImage(Sender)=TilePictureRO5)
  or(TImage(Sender)=Border3)          then TileRO5.Ticked:=True;
  if(TImage(Sender)=TilePictureIyonix)
  or(TImage(Sender)=Border4)          then TileIyonix.Ticked:=True;
  if(TImage(Sender)=TilePictureROPi)
  or(TImage(Sender)=Border5)          then TileROPi.Ticked:=True;
 end;
end;

end.

