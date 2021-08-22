unit AFSPartitionUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
 Buttons, ExtCtrls;

type

 { TAFSPartitionForm }

 TAFSPartitionForm = class(TForm)
  CancelButton: TBitBtn;
  OKBtnBack: TPanel;
  OKButton: TBitBtn;
  PartitionSizeLabel: TLabel;
  PartitionSize: TTrackBar;
  procedure FormResize(Sender: TObject);
  procedure PartitionSizeChange(Sender: TObject);
 private

 public

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
//Tile the form
{------------------------------------------------------------------------------}
procedure TAFSPartitionForm.FormResize(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

end.

