unit ProgressUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,StdCtrls,ExtCtrls;

type

 { TProgressForm }

 TProgressForm = class(TForm)
  UpdateProgress: TLabel;
  Panel1: TPanel;
  procedure FormShow(Sender: TObject);
 private

 public

 end;

var
 ProgressForm: TProgressForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TProgressForm }

procedure TProgressForm.FormShow(Sender: TObject);
begin
 Top:=MainForm.Top;
 Left:=MainForm.Left;
 Width:=MainForm.Width;
 Height:=MainForm.Height+MainForm.ImageDetails.Height;
 UpdateProgress.Caption:='';
end;

end.
