unit HardDriveUnit;

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,Buttons,ExtCtrls,Math,
 ComCtrls, StdCtrls;

type

 { THardDriveForm }

 THardDriveForm = class(TForm)
  cb_NewMap: TCheckBox;
  Image1: TImage;
  CapacityHeaderLabel: TLabel;
  CapacityLabel: TLabel;
  DirectoryLabel: TLabel;
  OKBtnBack: TPanel;
  OKButton: TBitBtn;
  CancelButton: TBitBtn;
  CapacitySlider: TTrackBar;
  rb_BigDir: TRadioButton;
  rb_NewDir: TRadioButton;
  rb_OldDir: TRadioButton;
  procedure cb_NewMapChange(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure CapacitySliderChange(Sender: TObject);
  procedure FormShow(Sender: TObject);
 private
  const
   OldMapLimit=512*1024*1024; //Old map drive limit = 512MB
   Multiplier =63*16*512;     //Capacity multiplier = sectors*heads*secsize
   MB         =1024*1024;     //MegaByte
 public

 end;

var
 HardDriveForm: THardDriveForm;

implementation

{$R *.lfm}

uses MainUnit;

{ THardDriveForm }

{-------------------------------------------------------------------------------
Texturise the form
-------------------------------------------------------------------------------}
procedure THardDriveForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

{-------------------------------------------------------------------------------
Change of the new map tick box
-------------------------------------------------------------------------------}
procedure THardDriveForm.cb_NewMapChange(Sender: TObject);
begin
 //Enable/disable the appropriate radio boxes
 rb_BigDir.Enabled:=cb_NewMap.Checked;
 rb_OldDir.Enabled:=not cb_NewMap.Checked;
 //If Old map is selected
 if not cb_NewMap.Checked then
 begin
  //Ensure the hard drive capactiy is <512MB
  if CapacitySlider.Position*10*Multiplier>OldMapLimit then
   CapacitySlider.Position:=OldMapLimit div(Multiplier*10);
  //And Big Dir is not selected
  if rb_BigDir.Checked then rb_NewDir.Checked:=True;
 end;
 //If New map is selected, ensure Old Dir is not
 if(cb_NewMap.Checked)and(rb_OldDir.Checked)then rb_NewDir.Checked:=True;
end;

{-------------------------------------------------------------------------------
Hard drive capacity is changing
-------------------------------------------------------------------------------}
procedure THardDriveForm.CapacitySliderChange(Sender: TObject);
begin
 //Update the label
 CapacityLabel.Caption:=
       IntToStr(Ceil((CapacitySlider.Position*10*Multiplier)/MB))+'MB';
 //If it goes over 512MB, ensure it is a New map
 if CapacitySlider.Position*10*Multiplier>OldMapLimit then
  cb_NewMap.Checked:=True;
end;

{-------------------------------------------------------------------------------
Form is showing
-------------------------------------------------------------------------------}
procedure THardDriveForm.FormShow(Sender: TObject);
begin
 //Set capacity to 40MB
 CapacitySlider.Position:=8;
 CapacitySliderChange(Sender);
 //Set directory type to 'Old'
 rb_OldDir.Checked:=True;
 rb_NewDir.Checked:=False;
 rb_BigDir.Checked:=False;
 //Enable/Disable the appropriate radio boxes
 rb_OldDir.Enabled:=True;
 rb_BigDir.Enabled:=False;
 cb_NewMap.Checked:=False;
end;

end.