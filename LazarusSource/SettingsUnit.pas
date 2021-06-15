unit SettingsUnit;

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
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
 Buttons{, Spin};

type

 { TSettingsForm }

 TSettingsForm = class(TForm)
  CancelButton: TBitBtn;
  CreateINF: TCheckBox;
  MiscGroup: TGroupBox;
  InterleaveGroup: TRadioGroup;
  NoTile: TRadioButton;
  TextureGroup: TGroupBox;
  OKBtnBack: TPanel;
  OKButton: TBitBtn;
  TilePicture1: TImage;
  TilePicture2: TImage;
  Tile1: TRadioButton;
  Tile2: TRadioButton;
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure TilePicture1Click(Sender: TObject);
  procedure TilePicture2Click(Sender: TObject);
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
 TilePicture1.Picture:=MainForm.RO5TextureTile.Picture;
 TilePicture2.Picture:=MainForm.RO4TextureTile.Picture;
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
procedure TSettingsForm.TilePicture1Click(Sender: TObject);
begin
 Tile1.Checked:=True;
end;

{------------------------------------------------------------------------------}
//User has clicked on a picture rather than the radio button
{------------------------------------------------------------------------------}
procedure TSettingsForm.TilePicture2Click(Sender: TObject);
begin
 Tile2.Checked:=True;
end;

end.

