unit ImageDetailUnit;

{
Copyright (C) 2018-2024 Gerald Holdsworth gerald@hollypops.co.uk

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
  FileLabel: TLabel;
  LegendLabel: TLabel;
  Legend: TPanel;
  colFree: TShape;
  colSystem: TShape;
  colDir: TShape;
  colFile: TShape;
  btn_OK,
  btn_Cancel: TRISCOSButton;
  procedure FormCreate(Sender: TObject);
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

procedure TImageDetailForm.LegendPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

procedure TImageDetailForm.FormCreate(Sender: TObject);
var
 ratio: Real;
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 btn_OK:=MainForm.CreateButton(Legend as TControl,'Update',True,0,0,mrOK);
 btn_Cancel:=MainForm.CreateButton(Legend as TControl,'Cancel',False,0,0,mrCancel);
 btn_OK.Width:=(Legend.ClientWidth div 2)-Round(16*ratio);
 btn_Cancel.Width:=btn_OK.Width-Round(8*ratio);
 btn_OK.Top:=LegendLabel.Top-Round(8*ratio)-btn_OK.Height;
 btn_OK.Left:=Legend.ClientWidth-Round(8*ratio)-btn_OK.Width;
 btn_Cancel.Top:=btn_OK.Top+Round(4*ratio);
 btn_Cancel.Left:=Round(8*ratio);
end;

end.
