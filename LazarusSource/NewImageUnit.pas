unit NewImageUnit;

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
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
 StdCtrls, ComCtrls, DiscImageUtils,Math;

type
 { TNewImageForm }

 TNewImageForm = class(TForm)
  DOS: TRadioGroup;
  btn_Cancel: TBitBtn;
  AFS: TRadioGroup;
  AFSSize: TGroupBox;
  AFSImageSizeLabel: TLabel;
  cb_AFScreatepword: TCheckBox;
  MainFormat: TRadioGroup;
  DFS: TRadioGroup;
  ADFS: TRadioGroup;
  C64: TRadioGroup;
  Amiga: TRadioGroup;
  DFSTracks: TRadioGroup;
  OKBtnBack: TPanel;
  btn_OK: TBitBtn;
  Spectrum: TRadioGroup;
  AFSImageSize: TTrackBar;
  procedure AFSClick(Sender: TObject);
  procedure AFSImageSizeChange(Sender: TObject);
  procedure btn_OKClick(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure MainFormatClick(Sender: TObject);
 private

 public
  harddrivesize : Cardinal;
  newmap        : Boolean;
  dirtype,
  fat           : Byte;
 end;

var
 NewImageForm: TNewImageForm;

implementation

{$R *.lfm}

uses MainUnit,HardDriveUnit;

{ TNewImageForm }

{-------------------------------------------------------------------------------
Options have changed
-------------------------------------------------------------------------------}
procedure TNewImageForm.MainFormatClick(Sender: TObject);
begin
 //First, hide all the sub options
 DFS.Visible          :=False;
 ADFS.Visible         :=False;
 C64.Visible          :=False;
 Spectrum.Visible     :=False;
 Amiga.Visible        :=False;
 AFS.Visible          :=False;
 DOS.Visible          :=False;
 //Now enable the appropriate one, based on the main option
 case MainFormat.ItemIndex of
  0: DFS.Visible      :=True;
  1: ADFS.Visible     :=True;
  2: C64.Visible      :=True;
  3: Spectrum.Visible :=True;
  4: Amiga.Visible    :=True;
  7: AFS.Visible      :=True;
  8: DOS.Visible      :=True;
 end;
 DFSTracks.Visible :=DFS.Visible;
 AFSSize.Visible   :=AFS.Visible;
 //Currently, only certain types of format can be created
 btn_OK.Enabled:=(MainFormat.ItemIndex=0) //DFS
               OR(MainFormat.ItemIndex=1) //ADFS
               OR(MainFormat.ItemIndex=2) //C64
               OR(MainFormat.ItemIndex=5) //CFS
               OR(MainFormat.ItemIndex=7) //AFS
               OR(MainFormat.ItemIndex=8);//DOS
end;

{-------------------------------------------------------------------------------
Form is being displayed
-------------------------------------------------------------------------------}
procedure TNewImageForm.FormShow(Sender: TObject);
begin
 //Reset to the default options
 MainFormat.ItemIndex:=0;
 DFS.ItemIndex       :=0;
 DFSTracks.ItemIndex :=1;
 ADFS.ItemIndex      :=0;
 C64.ItemIndex       :=0;
 Spectrum.ItemIndex  :=0;
 Amiga.ItemIndex     :=0;
 AFS.ItemIndex       :=0;
 DOS.ItemIndex       :=0;
 cb_AFScreatepword.Checked:=False;
 AFSImageSize.Position:=AFSImageSize.Min;
 AFSClick(Sender);
 AFSImageSizeChange(Sender);
 //Hide all the sub options, except for the first one
 DFS.Visible         :=True;
 DFSTracks.Visible   :=True;
 ADFS.Visible        :=False;
 C64.Visible         :=False;
 Spectrum.Visible    :=False;
 Amiga.Visible       :=False;
 AFS.Visible         :=False;
 AFSSize.Visible     :=False;
 DOS.Visible         :=False;
 //Enable the create button
 btn_OK.Enabled      :=True;
end;

{-------------------------------------------------------------------------------
User has clicked on create
-------------------------------------------------------------------------------}
procedure TNewImageForm.btn_OKClick(Sender: TObject);
var
 ok: Boolean;
begin
 ok:=True;
 //Are we creating a hard drive?
 if((MainFormat.ItemIndex=1)AND(ADFS.ItemIndex=8))    //ADFS
 or((MainFormat.ItemIndex=8)AND(DOS.ItemIndex=6))then //DOS
 begin
  //Then we need to open the additional dialogue to configure this
  HardDriveForm.ADFSHDD:=MainFormat.ItemIndex=1; //Set to ADFS or DOS
  HardDriveForm.ShowModal;
  ok:=HardDriveForm.ModalResult=mrOK;
  if ok then
  begin
   //Selected hard drive size in MB
   harddrivesize:=HardDriveForm.CapacitySlider.Position*1024*1024;
   if MainFormat.ItemIndex=1 then //ADFS Specific
   begin
    //New or old map
    newmap:=HardDriveForm.cb_NewMap.Checked;
    //Directory type
    dirtype:=diADFSOldDir;
    if HardDriveForm.rb_NewDir.Checked then dirtype:=diADFSNewDir;
    if HardDriveForm.rb_BigDir.Checked then dirtype:=diADFSBigDir;
   end
   else                           //DOS Specific
   begin
    fat:=diFAT12;
    if HardDriveForm.rb_FAT16.Checked then fat:=diFAT16;
    if HardDriveForm.rb_FAT32.Checked then fat:=diFAT32;
   end;
  end;
 end;
 //Return to the calling form
 if ok then ModalResult:=mrOK;
end;

{-------------------------------------------------------------------------------
The AFS capacity slider is changing
-------------------------------------------------------------------------------}
procedure TNewImageForm.AFSImageSizeChange(Sender: TObject);
begin
 if AFSImageSize.Position<=409 then
  AFSImageSizeLabel.Caption:=IntToStr(AFSImageSize.Position*10)+'KB'
 else
 begin
  AFSImageSize.Position:=Round((Ceil((AFSImageSize.Position*10)/1024)*1024)/10);
  AFSImageSizeLabel.Caption:=IntToStr(Ceil((AFSImageSize.Position*10)/1024))+'MB';
 end;
 //Label is not updating on Windows
end;

{-------------------------------------------------------------------------------
The AFS Level has changed, change the minimum size
-------------------------------------------------------------------------------}
procedure TNewImageForm.AFSClick(Sender: TObject);
begin
 if AFS.ItemIndex=0 then
 begin
  AFSImageSize.Min:=40;  //Level 2 minimum size 400K
  AFSImageSize.Max:=102; //Level 2 maximum size is 1023K (1MB)
 end;
 if(AFS.ItemIndex=1)or(AFS.ItemIndex=2)then
 begin
  AFSImageSize.Min:=64;    //Level 3 minimum size 640K
  AFSImageSize.Max:=13107; //Level 3 temporary max is ~128MB
  //AFSImageSize.Max:=52428; //Level 3 maximum size is 524280K (~512MB)
 end;
 AFSImageSizeChange(Sender);
end;

{-------------------------------------------------------------------------------
Texturise the form
-------------------------------------------------------------------------------}
procedure TNewImageForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

end.
