unit ChangeInterleaveUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
 StdCtrls;

type

 { TChangeInterleaveForm }

 TChangeInterleaveForm = class(TForm)
  CancelButton: TBitBtn;
  cb_NewMethod: TComboBox;
  CurrentLabel: TLabel;
  NewLabel: TLabel;
  lb_Current: TLabel;
  OKBtnBack: TPanel;
  OKButton: TBitBtn;
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

end.

