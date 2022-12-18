unit CSVPrefUnit;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
 ExtCtrls;

type

 { TCSVPrefForm }

 TCSVPrefForm = class(TForm)
  CancelButton: TBitBtn;
  cb_IncDir: TCheckBox;
  cb_Parent: TCheckBox;
  cb_Filename: TCheckBox;
  cb_LoadAddr: TCheckBox;
  cb_ExecAddr: TCheckBox;
  cb_Length: TCheckBox;
  cb_Attributes: TCheckBox;
  cb_Address: TCheckBox;
  cb_CRC32: TCheckBox;
  cb_IncFilename: TCheckBox;
  cb_IncReport: TCheckBox;
  Label1: TLabel;
  Label2: TLabel;
  OKBtnBack: TPanel;
  OKButton: TBitBtn;
  procedure FormPaint(Sender: TObject);
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

end.

