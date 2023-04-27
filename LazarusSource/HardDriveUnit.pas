unit HardDriveUnit;

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,ExtCtrls,ComCtrls,StdCtrls,
 GJHCustomComponents;

type

 { THardDriveForm }

 THardDriveForm = class(TForm)
  HDDImage: TImage;
  CapacityHeaderLabel: TLabel;
  DirectoryLabel: TLabel;
  ADFSControls: TPanel;
  DOSControls: TPanel;
  CapacitySlider: TGJHSlider;
  cb_NewMap,
  cb_AddHeader,
  cb_IDE: TGJHTickBox;
  rb_FAT12,
  rb_FAT16,
  rb_FAT32,
  rb_BigDir,
  rb_NewDir,
  rb_OldDir: TGJHRadioBox;
  OKButton,
  CancelButton: TGJHButton;
  procedure cb_NewMapChange(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure CapacitySliderChange(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure rb_FAT12Change(Sender: TObject);
  procedure rb_FAT16Change(Sender: TObject);
  procedure rb_FAT32Change(Sender: TObject);
 private
  const
   OldMapLimit=512*1024*1024; //Old map drive limit = 512MB
   MB         =1024*1024;     //MegaByte
 public
  ADFSHDD,
  DOSHDD,
  AmigaHDD: Boolean;
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
 rb_BigDir.Enabled:=cb_NewMap.Ticked;
 rb_OldDir.Enabled:=not cb_NewMap.Ticked;
 cb_IDE.Enabled:=cb_NewMap.Ticked;
 //If Old map is selected
 if not cb_NewMap.Ticked then
 begin
  //Ensure the hard drive capactiy is <512MB
  if CapacitySlider.Position*MB>OldMapLimit then
   CapacitySlider.Position:=OldMapLimit div MB;
  //And Big Dir is not selected
  if rb_BigDir.Ticked then rb_NewDir.Ticked:=True;
 end;
 //If New map is selected, ensure Old Dir is not
 if(cb_NewMap.Ticked)and(rb_OldDir.Ticked)then rb_NewDir.Ticked:=True;
end;

{-------------------------------------------------------------------------------
Form creation
-------------------------------------------------------------------------------}
procedure THardDriveForm.FormCreate(Sender: TObject);
function CreateTickBox(text: String; LPanel: TPanel): TGJHTickBox;
begin
 Result:=TGJHTickBox.Create(LPanel as TControl);
 Result.Parent:=LPanel as TWinControl;
 Result.Visible:=True;
 Result.Caption:=text;
 Result.Top:=0;
end;
function CreateRadioBox(text: String; LPanel: TPanel): TGJHRadioBox;
begin
 Result:=TGJHRadioBox.Create(LPanel as TControl);
 Result.Parent:=LPanel as TWinControl;
 Result.Visible:=True;
 Result.Caption:=text;
 Result.Top:=0;
end;
var ratio: Real;
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 //Create the slider
 CapacitySlider:=TGJHSlider.Create(HardDriveForm as TControl);
 CapacitySlider.Parent:=HardDriveForm as TWinControl;
 CapacitySlider.Visible:=True;
 CapacitySlider.Min:=4;
 CapacitySlider.Max:=203;
 CapacitySlider.Position:=4;
 CapacitySlider.Outline:=csOutInner;
 CapacitySlider.Height:=Round(30*ratio);
 CapacitySlider.Orientation:=csHorizontal;
 CapacitySlider.ShowValue:=True;
 CapacitySlider.Pointers:=False;
 CapacitySlider.Top:=CapacityHeaderLabel.Top+CapacityHeaderLabel.Height;
 CapacitySlider.Left:=HDDImage.Left+HDDImage.Width;
 CapacitySlider.Suffix:='MB';
 CapacitySlider.OnChange:=@CapacitySliderChange;
 //Create the ADFS Tick boxes
 cb_NewMap:=CreateTickBox('New Map',ADFSControls);
 cb_NewMap.Left:=0;
 cb_NewMap.OnChange:=@cb_NewMapChange;
 cb_IDE:=CreateTickBox('IDE',ADFSControls);
 cb_IDE.Left:=cb_NewMap.Left+cb_NewMap.Width+4;
 cb_AddHeader:=CreateTickBox('Emulator',ADFSControls);
 cb_AddHeader.Left:=cb_IDE.Left+cb_IDE.Width+4;
 //Move the label
 DirectoryLabel.Left:=cb_AddHeader.Left+cb_AddHeader.Width+Round(12*ratio);
 //Create the ADFS Radio boxes
 rb_OldDir:=CreateRadioBox('Old',ADFSControls);
 rb_OldDir.Left:=DirectoryLabel.Left+DirectoryLabel.Width+Round(4*ratio);
 rb_NewDir:=CreateRadioBox('New',ADFSControls);
 rb_NewDir.Left:=rb_OldDir.Left+rb_OldDir.Width+Round(4*ratio);
 rb_BigDir:=CreateRadioBox('Big',ADFSControls);
 rb_BigDir.Left:=rb_NewDir.Left+rb_NewDir.Width+Round(4*ratio);
 ADFSControls.Height:=rb_BigDir.Top+rb_BigDir.Height+Round(8*ratio);
 //Change the widths
 ADFSControls.Width:=rb_BigDir.Left+rb_BigDir.Width+Round(8*ratio);
 Width:=ADFSControls.Left+ADFSControls.Width;
 CapacitySlider.Width:=Width-CapacitySlider.Left;
 //Create the DOS Tick boxes
 rb_FAT12:=CreateRadioBox('FAT12',DOSControls);
 rb_FAT12.OnChange:=@rb_FAT12Change;
 rb_FAT12.Left:=0;
 rb_FAT16:=CreateRadioBox('FAT16',DOSControls);
 rb_FAT16.OnChange:=@rb_FAT16Change;
 rb_FAT16.Left:=rb_FAT12.Left+rb_FAT12.Width+Round(4*ratio);
 rb_FAT32:=CreateRadioBox('FAT32',DOSControls);
 rb_FAT32.OnChange:=@rb_FAT32Change;
 rb_FAT32.Left:=rb_FAT16.Left+rb_FAT16.Width+Round(4*ratio);
 //Create the buttons
 OKButton:=MainForm.CreateButton(HardDriveForm as TControl,'Create',True,0,
                            ADFSControls.Height+ADFSControls.Top+Round(4*ratio),
                                 mrOK);
 OKButton.Left:=Width-OKButton.Width-Round(8*ratio);
 CancelButton:=MainForm.CreateButton(HardDriveForm as TControl,'Cancel',False,0,
                                     OKButton.Top+Round(4*ratio),mrCancel);
 CancelButton.Left:=OKButton.Left-Round(8*ratio)-CancelButton.Width;
 //Adjust the form height
 Height:=OKButton.Top+OKButton.Height+Round(8*ratio);
end;

{-------------------------------------------------------------------------------
Hard drive capacity is changing
-------------------------------------------------------------------------------}
procedure THardDriveForm.CapacitySliderChange(Sender: TObject);
begin
 if ADFSHDD then
  if CapacitySlider.Position*MB>OldMapLimit then
   cb_NewMap.Ticked:=True;
end;

{-------------------------------------------------------------------------------
Form is showing
-------------------------------------------------------------------------------}
procedure THardDriveForm.FormShow(Sender: TObject);
begin
 if ADFSHDD then
 begin
  //Set capacity to 40MB
  CapacitySlider.Position:=40;
  CapacitySlider.Min:=20;  //Minimum 20MB
  CapacitySlider.Max:=1024;//Maximum 1GB
  CapacitySliderChange(Sender);
  ADFSControls.Visible:=True;
  DOSControls.Visible:=False;
  Caption:='Create ADFS Hard Drive';
 end;
 //Set directory type to 'Old'
 rb_OldDir.Ticked:=True;
 rb_NewDir.Ticked:=False;
 rb_BigDir.Ticked:=False;
 //Enable/Disable the appropriate radio boxes
 rb_OldDir.Enabled:=True;
 rb_BigDir.Enabled:=False;
 //Other options
 cb_NewMap.Ticked:=False;
 cb_AddHeader.Ticked:=False;
 cb_IDE.Ticked:=True;
 cb_IDE.Enabled:=cb_NewMap.Ticked;
 if AmigaHDD then
 begin
  //Set capacity to 40MB
  CapacitySlider.Position:=40;
  //Set max to 1024MB
  CapacitySlider.Max:=1024;//Maximum 500MB
  CapacitySlider.Min:=20;  //Minimum 20MB
  CapacitySliderChange(Sender);
  ADFSControls.Visible:=False;
  DOSControls.Visible:=False;
  Caption:='Create Amiga Hard Drive';
 end;
 if DOSHDD then
 begin
  //Set capacity to 40MB
  CapacitySlider.Position:=40;
  //Set max to 500MB for FAT12
  CapacitySlider.Max:=500;//Maximum 500MB
  CapacitySlider.Min:=20; //Minimum 20MB
  CapacitySliderChange(Sender);
  ADFSControls.Visible:=False;
  DOSControls.Visible:=True;
  Caption:='Create DOS Hard Drive';
 end;
 rb_FAT12.Ticked:=True;
 rb_FAT16.Ticked:=False;
 rb_FAT32.Ticked:=False;
end;

{-------------------------------------------------------------------------------
FAT12 has been selected
-------------------------------------------------------------------------------}
procedure THardDriveForm.rb_FAT12Change(Sender: TObject);
begin
 //Set max to 500MB for FAT12
 CapacitySlider.Max:=500;//Max 500MB
 CapacitySlider.Min:=20; //Min 20MB
 CapacitySliderChange(Sender);
end;

{-------------------------------------------------------------------------------
FAT16 has been selected
-------------------------------------------------------------------------------}
procedure THardDriveForm.rb_FAT16Change(Sender: TObject);
begin
 //Set max to 1000MB for FAT16
 CapacitySlider.Max:=1000;//Max 1000MB
 CapacitySlider.Min:=20;  //Min 20MB
 CapacitySliderChange(Sender);
end;

{-------------------------------------------------------------------------------
FAT32 has been selected
-------------------------------------------------------------------------------}
procedure THardDriveForm.rb_FAT32Change(Sender: TObject);
begin
 //Set max to 1024MB for FAT32
 CapacitySlider.Max:=1024;//Max 1GB
 CapacitySlider.Min:=33;  //Min 33MB
 CapacitySliderChange(Sender);
end;

end.
