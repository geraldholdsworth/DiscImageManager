unit ImageDetailUnit;

{
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

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
 Buttons;

type

 { TImageDetailForm }

 TImageDetailForm = class(TForm)
  btn_Cancel: TBitBtn;
  btn_OK: TBitBtn;
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
  OKBtnBack: TPanel;
  pnSide0: TGroupBox;
  FreeLabel: TLabel;
  BBCMasterLogo: TImage;
  SystemLabel: TLabel;
  DirectoryLabel: TLabel;
  FileLabel: TLabel;
  Label5: TLabel;
  Legend: TPanel;
  colFree: TShape;
  colSystem: TShape;
  colDir: TShape;
  colFile: TShape;
  pnSide1: TGroupBox;
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

end.
