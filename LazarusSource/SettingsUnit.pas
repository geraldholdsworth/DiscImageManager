unit SettingsUnit;

{
Copyright (C) 2018-2022 Gerald Holdsworth gerald@hollypops.co.uk

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
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
 Buttons{, Spin};

type

 { TSettingsForm }

 TSettingsForm = class(TForm)
  AllowDFSZeroSecs: TCheckBox;
  AllowDFSBlankFilenames: TCheckBox;
  CancelButton: TBitBtn;
  ScanSubDirs: TCheckBox;
  DFSBeyondEdge: TCheckBox;
  DFSGroup: TGroupBox;
  WriteDebug: TCheckBox;
  CreateINF: TCheckBox;
  MiscGroup: TGroupBox;
  InterleaveGroup: TRadioGroup;
  NoTile: TRadioButton;
  TextureGroup: TGroupBox;
  OKBtnBack: TPanel;
  OKButton: TBitBtn;
  TileROPi: TRadioButton;
  TilePictureIyonix: TImage;
  TilePictureROPi: TImage;
  TileRO3: TRadioButton;
  TilePictureRO5: TImage;
  TilePictureRO4: TImage;
  TileRO5: TRadioButton;
  TileRO4: TRadioButton;
  TilePictureRO3: TImage;
  TileIyonix: TRadioButton;
  CompressUEF: TCheckBox;
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
//User has clicked on a picture rather than the radio button
{------------------------------------------------------------------------------}
procedure TSettingsForm.TilePictureRO5Click(Sender: TObject);
begin
 if Sender is TImage then
 begin
  if TImage(Sender)=TilePictureRO3 then TileRO3.Checked:=True;
  if TImage(Sender)=TilePictureRO4 then TileRO4.Checked:=True;
  if TImage(Sender)=TilePictureRO5 then TileRO5.Checked:=True;
  if TImage(Sender)=TilePictureIyonix then TileIyonix.Checked:=True;
  if TImage(Sender)=TilePictureROPi then TileROPi.Checked:=True;
 end;
end;

end.

