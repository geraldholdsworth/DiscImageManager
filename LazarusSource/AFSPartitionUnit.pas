unit AFSPartitionUnit;

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,ComCtrls,StdCtrls,ExtCtrls,
 GJHCustomComponents;

type

 { TAFSPartitionForm }

 TAFSPartitionForm = class(TForm)
  OpenDFSFile: TOpenDialog;
  PartitionSizeLabel: TLabel;
  PartitionSize: TRISCOSSlider;
  rad_type40T,
  rad_type80T,
  rad_typeAFS,
  rad_typeDOS: TRISCOSRadioBox;
  FromFileButton,
  OKButton,
  CancelButton: TRISCOSButton;
  procedure FormCreate(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure PartitionSizeChange(Sender: TObject);
  procedure rad_typeClick(Sender: TObject);
  procedure FromFileClick(Sender: TObject);
 private

 public
  maxAFSSize,
  maxDOSSize : Cardinal;
  fromFile   : Boolean;
 end;

var
 AFSPartitionForm: TAFSPartitionForm;

implementation

uses MainUnit;

{$R *.lfm}

{ TAFSPartitionForm }

{------------------------------------------------------------------------------}
//Update the size label
{------------------------------------------------------------------------------}
procedure TAFSPartitionForm.PartitionSizeChange(Sender: TObject);
begin
 PartitionSizeLabel.Caption:=FloatToStr((PartitionSize.Position*$100)/1024)+'KB';
end;

{------------------------------------------------------------------------------}
//Ensure we don't create anything bigger than the FS can handle
{------------------------------------------------------------------------------}
procedure TAFSPartitionForm.rad_typeClick(Sender: TObject);
begin
 if rad_typeAFS.Ticked then PartitionSize.Max:=maxAFSSize;
 if rad_typeDOS.Ticked then PartitionSize.Max:=maxDOSSize;
end;

{------------------------------------------------------------------------------}
//Tile the form
{------------------------------------------------------------------------------}
procedure TAFSPartitionForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

{------------------------------------------------------------------------------}
//Show the form - set it up
{------------------------------------------------------------------------------}
procedure TAFSPartitionForm.FormShow(Sender: TObject);
begin
 //Style the buttons
 FromFileButton.NativeOS:=MainForm.Fstyling=MainForm.NativeStyle;
 OKButton.NativeOS      :=MainForm.Fstyling=MainForm.NativeStyle;
 CancelButton.NativeOS  :=MainForm.Fstyling=MainForm.NativeStyle;
 rad_type40T.NativeOS   :=MainForm.Fstyling=MainForm.NativeStyle;
 rad_type80T.NativeOS   :=MainForm.Fstyling=MainForm.NativeStyle;
 rad_typeAFS.NativeOS   :=MainForm.Fstyling=MainForm.NativeStyle;
 rad_typeDOS.NativeOS   :=MainForm.Fstyling=MainForm.NativeStyle;
 //Re-align the buttons
 CancelButton.Top       :=OKButton.Top+(OKButton.Height-CancelButton.Height)div 2;
 FromFileButton.Top     :=CancelButton.Top;
end;

{------------------------------------------------------------------------------}
//Create the form
{------------------------------------------------------------------------------}
procedure TAFSPartitionForm.FormCreate(Sender: TObject);
 function CreateRadioBox(text: String): TRISCOSRadioBox;
 begin
  Result:=TRISCOSRadioBox.Create(AFSPartitionForm as TControl);
  Result.Parent:=AFSPartitionForm as TWinControl;
  Result.Top:=PartitionSize.Top+PartitionSize.Height;
  Result.Visible:=True;
  Result.Caption:=text;
  Result.OnClick:=@rad_typeClick;
  Result.Font.Color:=clBlack;
 end;
var
 ratio  : Real=0;
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 //Create the slider
 PartitionSize:=TRISCOSSlider.Create(AFSPartitionForm as TControl);
 PartitionSize.Parent:=AFSPartitionForm as TWinControl;
 PartitionSize.Top:=PartitionSizeLabel.Height;
 PartitionSize.Align:=alTop;
 PartitionSize.Orientation:=csHorizontal;
 PartitionSize.Height:=Round(30*ratio);
 PartitionSize.Min:=0;
 PartitionSize.Max:=10;
 PartitionSize.Pointers:=False;
 PartitionSize.Outline:=csOutInner;
 PartitionSize.OnChange:=@PartitionSizeChange;
 PartitionSize.Font.Color:=clBlack;
 //Create the radio boxes
 rad_typeAFS:=CreateRadioBox('Acorn File Server');
 rad_typeDOS:=CreateRadioBox('DOS Plus');
 rad_type40T:=CreateRadioBox('40 Track');
 rad_type80T:=CreateRadioBox('80 Track');
 //Create the buttons
 FromFileButton:=MainForm.CreateButton(AFSPartitionForm as TControl,
                                       'From File...',False,Round(8*ratio),
                              rad_typeAFS.Top+rad_typeAFS.Height+Round(8*ratio),
                                       mrNone);
 FromFileButton.Enabled:=False;
 FromFileButton.OnClick:=@FromFileClick;
 CancelButton:=MainForm.CreateButton(AFSPartitionForm as TControl,'Cancel',
                                     False,
                        FromFileButton.Left+FromFileButton.Width+Round(8*ratio),
                                     FromFileButton.Top,mrCancel);
 OKButton:=MainForm.CreateButton(AFSPartitionForm as TControl,'OK',True,
                            CancelButton.Left+CancelButton.Width+Round(8*ratio),
                                 FromFileButton.Top-Round(4*ratio),mrOK);
 Width:=OKButton.Left+OKButton.Width+8;
 //Adjust the radio button positions
 rad_typeAFS.Left:=Round(8*ratio);
 rad_typeDOS.Left:=Width div 2;
 rad_type40T.Left:=rad_typeAFS.Left;
 rad_type80T.Left:=rad_typeDOS.Left;
 //Adjust the form height
 Height:=OKButton.Top+OKButton.Height+Round(8*ratio);
end;

{------------------------------------------------------------------------------}
//User clicked on 'From File'
{------------------------------------------------------------------------------}
procedure TAFSPartitionForm.FromFileClick(Sender: TObject);
begin
 FromFileButton.ModalResult:=mrNone;
 //Currently only for adding DFS image
 if OpenDFSFile.Execute then
 begin
  fromFile:=True;
  FromFileButton.ModalResult:=mrOK;
 end;
end;

end.

