unit ImageDetailUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
 Buttons;

type

 { TImageDetailForm }

 TImageDetailForm = class(TForm)
  btn_Cancel: TSpeedButton;
  cbBootOption0: TComboBox;
  cbBootOption1: TComboBox;
  edDiscTitle0: TEdit;
  edDiscTitle1: TEdit;
  Label6: TLabel;
  Label7: TLabel;
  lbCRC32: TLabel;
  lbBootOption0: TLabel;
  Label8: TLabel;
  lbBootOption1: TLabel;
  pnSide0: TGroupBox;
  Label1: TLabel;
  Label2: TLabel;
  Label3: TLabel;
  Label4: TLabel;
  Label5: TLabel;
  Legend: TPanel;
  colFree: TShape;
  colSystem: TShape;
  colDir: TShape;
  colFile: TShape;
  pnSide1: TGroupBox;
  btn_OK: TSpeedButton;
  procedure btn_CancelClick(Sender: TObject);
  procedure btn_OKClick(Sender: TObject);
 private

 public

 end;

var
 ImageDetailForm: TImageDetailForm;

implementation

{$R *.lfm}

{ TImageDetailForm }

procedure TImageDetailForm.btn_OKClick(Sender: TObject);
begin
 ModalResult:=mrOK;
end;

procedure TImageDetailForm.btn_CancelClick(Sender: TObject);
begin
 ModalResult:=mrCancel;
end;

end.
