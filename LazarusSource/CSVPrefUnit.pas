unit CSVPrefUnit;

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

{$mode ObjFPC}{$H+}

interface

uses
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,StdCtrls,ExtCtrls,
 GJHCustomComponents;

type

 { TCSVPrefForm }

 TCSVPrefForm = class(TForm)
  ColumnsLabel: TLabel;
  OptionsLabel: TLabel;
  cb_IncDir,
  cb_Parent,
  cb_Filename,
  cb_LoadAddr,
  cb_ExecAddr,
  cb_Length,
  cb_Attributes,
  cb_Address,
  cb_CRC32,
  cb_MD5,
  cb_IncFilename,
  cb_IncReport   : TRISCOSTickBox;
  OKButton,
  CancelButton   : TRISCOSButton;
  procedure FormCreate(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
 private

 public

 end;

var
 CSVPrefForm: TCSVPrefForm;

implementation

uses MainUnit;

{$R *.lfm}

{ TCSVPrefForm }

{------------------------------------------------------------------------------}
//Tile the form
{------------------------------------------------------------------------------}
procedure TCSVPrefForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

{------------------------------------------------------------------------------}
//Show the form - set it up
{------------------------------------------------------------------------------}
procedure TCSVPrefForm.FormShow(Sender: TObject);
begin
 //Style the buttons
 OKButton.NativeOS      :=MainForm.Fstyling=MainForm.NativeStyle;
 CancelButton.NativeOS  :=MainForm.Fstyling=MainForm.NativeStyle;
 cb_IncDir.NativeOS     :=MainForm.Fstyling=MainForm.NativeStyle;
 cb_Parent.NativeOS     :=MainForm.Fstyling=MainForm.NativeStyle;
 cb_Filename.NativeOS   :=MainForm.Fstyling=MainForm.NativeStyle;
 cb_LoadAddr.NativeOS   :=MainForm.Fstyling=MainForm.NativeStyle;
 cb_ExecAddr.NativeOS   :=MainForm.Fstyling=MainForm.NativeStyle;
 cb_Length.NativeOS     :=MainForm.Fstyling=MainForm.NativeStyle;
 cb_Attributes.NativeOS :=MainForm.Fstyling=MainForm.NativeStyle;
 cb_Address.NativeOS    :=MainForm.Fstyling=MainForm.NativeStyle;
 cb_CRC32.NativeOS      :=MainForm.Fstyling=MainForm.NativeStyle;
 cb_MD5.NativeOS        :=MainForm.Fstyling=MainForm.NativeStyle;
 cb_IncFilename.NativeOS:=MainForm.Fstyling=MainForm.NativeStyle;
 cb_IncReport.NativeOS  :=MainForm.Fstyling=MainForm.NativeStyle;
 //Re-align the buttons
 CancelButton.Top     :=OKButton.Top+(OKButton.Height-CancelButton.Height)div 2;
end;

{------------------------------------------------------------------------------}
//Create the form
{------------------------------------------------------------------------------}
procedure TCSVPrefForm.FormCreate(Sender: TObject);
var
 pos  : Integer=0;
 wid  : Integer=0;
 ratio: Real=0;
 function CreateTickBox(Ltitle: String; Ltop: Integer): TRISCOSTickBox;
 begin
  Result:=TRISCOSTickBox.Create(CSVPrefForm as TComponent);
  Result.Parent:=CSVPrefForm as TWinControl;
  Result.Left:=Round(8*ratio);
  Result.Visible:=True;
  Result.Top:=Ltop;
  Result.Caption:=Ltitle;
  Result.Font.Color:=clBlack;
 end;
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 OptionsLabel.Top:=0;
 OptionsLabel.Left:=0;
 //Create each tickbox and adjust the position, as it could be scaled
 pos:=OptionsLabel.Top+OptionsLabel.Height+Round(4*ratio);//This is the anchor for the rest
 cb_IncDir     :=CreateTickBox('Include Directories',pos);
 inc(pos,cb_IncDir.Height+Round(4*ratio));
 cb_IncFilename:=CreateTickBox('Include Filename',pos);
 inc(pos,cb_IncFilename.Height+Round(4*ratio));
 cb_IncReport  :=CreateTickBox('Include Image Report',pos);
 inc(pos,cb_IncReport.Height+Round(4*ratio));
 ColumnsLabel.Top:=pos; //Need to reposition the labels too
 ColumnsLabel.Left:=0;
 inc(pos,ColumnsLabel.Height+Round(4*ratio));
 cb_Parent     :=CreateTickBox('Parent',pos);
 inc(pos,cb_Parent.Height+Round(4*ratio));
 cb_Filename   :=CreateTickBox('Filename',pos);
 inc(pos,cb_Filename.Height+Round(4*ratio));
 cb_LoadAddr   :=CreateTickBox('Load Address',pos);
 inc(pos,cb_LoadAddr.Height+Round(4*ratio));
 cb_ExecAddr   :=CreateTickBox('Execution Address',pos);
 inc(pos,cb_ExecAddr.Height+Round(4*ratio));
 cb_Length     :=CreateTickBox('Length',pos);
 inc(pos,cb_Length.Height+Round(4*ratio));
 cb_Attributes :=CreateTickBox('Attributes',pos);
 inc(pos,cb_Attributes.Height+Round(4*ratio));
 cb_Address    :=CreateTickBox('Address',pos);
 inc(pos,cb_Address.Height+Round(4*ratio));
 cb_CRC32      :=CreateTickBox('CRC-32',pos);
 inc(pos,cb_CRC32.Height+Round(4*ratio));
 cb_MD5        :=CreateTickBox('MD-5',pos);
 inc(pos,cb_MD5.Height+Round(4*ratio));
 //Create the buttons
 CancelButton:=MainForm.CreateButton(CSVPrefForm as TControl,'Cancel',False,
                                     Round(8*ratio),pos+Round(4*ratio),mrCancel);
 OKButton:=MainForm.CreateButton(CSVPrefForm as TControl,'OK',True,
                            CancelButton.Left+CancelButton.Width+Round(8*ratio),
                            pos,mrOK);
 //Re-adjust the form height
 Height:=pos+OKButton.Height+Round(8*ratio);
 //Work out the widest control
 pos:=OptionsLabel.Left;
 for wid:=0 to ComponentCount-1 do
  if TControl(Components[wid]).Width+TControl(Components[wid]).Left+Round(8*ratio)>pos then
   pos:=TControl(Components[wid]).Width+TControl(Components[wid]).Left+Round(8*ratio);
 //Adjust the form width
 Width:=pos;
 OptionsLabel.AutoSize:=False;
 OptionsLabel.Width:=pos;
 ColumnsLabel.AutoSize:=False;
 ColumnsLabel.Width:=pos;
end;

end.

