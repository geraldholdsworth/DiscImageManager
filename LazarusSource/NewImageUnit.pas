unit NewImageUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,ExtCtrls,Buttons;

type

 { TNewImageForm }

 TNewImageForm = class(TForm)
  MainFormat: TRadioGroup;
  DFS: TRadioGroup;
  ADFS: TRadioGroup;
  C64: TRadioGroup;
  Amiga: TRadioGroup;
  DFSTracks: TRadioGroup;
  Spectrum: TRadioGroup;
  btn_OK: TSpeedButton;
  btn_Cancel: TSpeedButton;
  procedure ADFSClick(Sender: TObject);
  procedure btn_CancelClick(Sender: TObject);
  procedure btn_OKClick(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure MainFormatClick(Sender: TObject);
 private

 public

 end;

var
 NewImageForm: TNewImageForm;

implementation

{$R *.lfm}

{ TNewImageForm }

//Options have changed
procedure TNewImageForm.MainFormatClick(Sender: TObject);
begin
 //First, hide all the sub options
 DFS.Visible          :=False;
 ADFS.Visible         :=False;
 C64.Visible          :=False;
 Spectrum.Visible     :=False;
 Amiga.Visible        :=False;
 //Now enable the appropriate one, based on the main option
 case MainFormat.ItemIndex of
  0: DFS.Visible      :=True;
  1: ADFS.Visible     :=True;
  2: C64.Visible      :=True;
  3: Spectrum.Visible :=True;
  4: Amiga.Visible    :=True;
 end;
 DFSTracks.Visible:=DFS.Visible;
 //Currently, only certain types of format can be created
 btn_OK.Enabled:=(MainFormat.ItemIndex=0) //DFS
               OR(MainFormat.ItemIndex=1) //ADFS
               OR(MainFormat.ItemIndex=2) //C64
               OR(MainFormat.ItemIndex=5);//CFS
end;

//Form is being displayed
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
 //Hide all the sub options, except for the first one
 DFS.Visible         :=True;
 DFSTracks.Visible   :=True;
 ADFS.Visible        :=False;
 C64.Visible         :=False;
 Spectrum.Visible    :=False;
 Amiga.Visible       :=False;
 //Enable the create button
 btn_OK.Enabled      :=True;
end;

//User has clicked on cancel
procedure TNewImageForm.btn_CancelClick(Sender: TObject);
begin
 ModalResult:=mrCancel;
end;

//Select type of ADFS disc
procedure TNewImageForm.ADFSClick(Sender: TObject);
begin
 //Currently, only certain types of format can be created
 btn_OK.Enabled:=ADFS.ItemIndex<8; //Not hard drive yet
end;

//User has clicked on create
procedure TNewImageForm.btn_OKClick(Sender: TObject);
begin
 ModalResult:=mrOK;
end;

end.
