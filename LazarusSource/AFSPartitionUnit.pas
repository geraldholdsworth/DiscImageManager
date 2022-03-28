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
  FromFileButton: TBitBtn;
  OKBtnBack: TPanel;
  OKButton: TBitBtn;
  PartitionSizeLabel: TLabel;
  PartitionSize: TTrackBar;
  rad_type: TRadioGroup;
  procedure FormPaint(Sender: TObject);
  procedure PartitionSizeChange(Sender: TObject);
  procedure rad_typeClick(Sender: TObject);
 private

 public
  maxAFSSize,
  maxDOSSize : Cardinal;
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
 if rad_type.ItemIndex=0 then PartitionSize.Max:=maxAFSSize;
 if rad_type.ItemIndex=1 then PartitionSize.Max:=maxDOSSize;
end;

{------------------------------------------------------------------------------}
//Tile the form
{------------------------------------------------------------------------------}
procedure TAFSPartitionForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

end.

