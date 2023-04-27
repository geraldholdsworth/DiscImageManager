unit ChangeInterleaveUnit;

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
  CancelButton: TGJHButton;
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
 ratio  : Real;
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

