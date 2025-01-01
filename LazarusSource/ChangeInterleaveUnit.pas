unit ChangeInterleaveUnit;

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,GJHCustomComponents,ExtCtrls,
 StdCtrls;

type

 { TChangeInterleaveForm }

 TChangeInterleaveForm = class(TForm)
  cb_NewMethod: TComboBox;
  CurrentLabel: TLabel;
  NewLabel: TLabel;
  lb_Current: TLabel;
  OKButton,
  CancelButton: TRISCOSButton;
  procedure FormCreate(Sender: TObject);
  procedure FormPaint(Sender: TObject);
 private

 public

 end;

var
 ChangeInterleaveForm: TChangeInterleaveForm;

implementation

uses MainUnit;

{$R *.lfm}

{ TChangeInterleaveForm }

{------------------------------------------------------------------------------}
//Tile the form
{------------------------------------------------------------------------------}
procedure TChangeInterleaveForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

{------------------------------------------------------------------------------}
//Create the form
{------------------------------------------------------------------------------}
procedure TChangeInterleaveForm.FormCreate(Sender: TObject);
var
 ratio  : Real=0;
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 //Create the buttons
 CancelButton:=MainForm.CreateButton(ChangeInterleaveForm as TControl,
                                     'Cancel',False,Round(8*ratio),
                           cb_NewMethod.Top+cb_NewMethod.Height+Round(16*ratio),
                           mrCancel);
 OKButton:=MainForm.CreateButton(ChangeInterleaveForm as TControl,'OK',True,
                           CancelButton.Left+CancelButton.Width+Round(8*ratio),
                           cb_NewMethod.Top+cb_NewMethod.Height+Round(12*ratio),
                           mrOK);
 //Re-adjust the form size
 Height:=OKButton.Top+OKButton.Height+Round(8*ratio);
 Width:=OKButton.Left+OKButton.Width+Round(8*ratio);
end;

end.

