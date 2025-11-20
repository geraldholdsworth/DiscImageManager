unit ImageDetailUnit;

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,ExtCtrls,StdCtrls,
 GJHCustomComponents;

type

 { TImageDetailForm }

 TImageDetailForm = class(TForm)
  cbInterleave: TComboBox;
  pnSide1: TPanel;
  pnSide0Caption: TLabel;
  MicrosoftLogo: TImage;
  cbBootOption0: TComboBox;
  cbBootOption1: TComboBox;
  DirLabel: TLabel;
  DirPanel: TPanel;
  AcornLogo: TImage;
  AmigaLogo: TImage;
  CommodoreLogo: TImage;
  lbInterleave: TLabel;
  InterleaveLabel: TLabel;
  InterleavePanel: TPanel;
  pnSide0: TPanel;
  pnSide1Caption: TLabel;
  SinclairLogo: TImage;
  lbDirType: TLabel;
  MapLabel: TLabel;
  MapPanel: TPanel;
  FormatLabel: TLabel;
  FormatPanel: TPanel;
  edDiscTitle0: TEdit;
  edDiscTitle1: TEdit;
  DiscName0Label: TLabel;
  CRC32Label: TLabel;
  lbCRC32: TLabel;
  lbBootOption0: TLabel;
  DiscName1Label: TLabel;
  lbBootOption1: TLabel;
  CRCPanel: TPanel;
  lbMap: TLabel;
  lbImgFormat: TLabel;
  FreeLabel: TLabel;
  BBCMasterLogo: TImage;
  SystemLabel: TLabel;
  DirectoryLabel: TLabel;
  UnformattedLabel: TLabel;
  FileLabel: TLabel;
  LegendLabel: TLabel;
  Legend: TPanel;
  colFree: TShape;
  colSystem: TShape;
  colDir: TShape;
  colFile: TShape;
  colUnformatted: TShape;
  OKButton,
  CancelButton: TRISCOSButton;
  procedure FormCreate(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure LegendPaint(Sender: TObject);
 private

 public

 end;

var
 ImageDetailForm: TImageDetailForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TImageDetailForm }

{-------------------------------------------------------------------------------
Paint the form
-------------------------------------------------------------------------------}
procedure TImageDetailForm.LegendPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

{-------------------------------------------------------------------------------
Create the form
-------------------------------------------------------------------------------}
procedure TImageDetailForm.FormCreate(Sender: TObject);
var
 ratio: Real=0;
begin
 ratio             :=PixelsPerInch/DesignTimePPI;
 OKButton          :=MainForm.CreateButton(Legend as TControl,
                                           'Update',
                                           True,
                                           0,
                                           0,
                                           mrOK);
 CancelButton      :=MainForm.CreateButton(Legend as TControl,
                                           'Cancel',
                                           False,
                                           0,
                                           0,
                                           mrCancel);
 OKButton.Width    :=(Legend.ClientWidth div 2)-Round(16*ratio);
 CancelButton.Width:=OKButton.Width-Round(8*ratio);
 OKButton.Top      :=LegendLabel.Top-Round(8*ratio)-OKButton.Height;
 OKButton.Left     :=Legend.ClientWidth-Round(8*ratio)-OKButton.Width;
 CancelButton.Top  :=OKButton.Top+Round(4*ratio);
 CancelButton.Left :=Round(8*ratio);
end;

{-------------------------------------------------------------------------------
Show the form
-------------------------------------------------------------------------------}
procedure TImageDetailForm.FormShow(Sender: TObject);
begin
 OKButton.NativeOS    :=MainForm.Fstyling=MainForm.NativeStyle;
 CancelButton.NativeOS:=MainForm.Fstyling=MainForm.NativeStyle;
 //Re-align the buttons
 CancelButton.Top     :=OKButton.Top+(OKButton.Height-CancelButton.Height)div 2;
end;

end.
