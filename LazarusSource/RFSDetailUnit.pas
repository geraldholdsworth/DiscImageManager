unit RFSDetailUnit;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,ExtCtrls,ComCtrls,StdCtrls,
 GJHCustomComponents;

type

 { TRFSDetailForm }

 TRFSDetailForm = class(TForm)
  ROMFS: TPanel;
  ROMFSLabel: TLabel;
  ROMFSTitle: TLabeledEdit;
  ROMFSVersion: TLabeledEdit;
  ROMFSBinVersAdj: TUpDown;
  ROMFSBinVersLabel: TLabel;
  ROMFSBinVers: TLabel;
  ROMFSCopy: TLabeledEdit;
  btn_OK,
  btn_Cancel: TGJHButton;
  procedure FormCreate(Sender: TObject);
  procedure ROMFSBinVersAdjClick(Sender: TObject; Button: TUDBtnType);
  procedure FormPaint(Sender: TObject);
 private

 public

 end;

var
 RFSDetailForm: TRFSDetailForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TRFSDetailForm }

{-------------------------------------------------------------------------------
The ROM FS binary version is changing
-------------------------------------------------------------------------------}
procedure TRFSDetailForm.ROMFSBinVersAdjClick(Sender: TObject;
 Button: TUDBtnType);
begin
 //Update the display
 ROMFSBinVers.Caption:=IntToHex(ROMFSBinVersAdj.Position,2);
end;

{-------------------------------------------------------------------------------
Form creation
-------------------------------------------------------------------------------}
procedure TRFSDetailForm.FormCreate(Sender: TObject);
var
 ratio: Real;
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 ROMFSTitle.Width:=ROMFS.Width-Round(12*ratio)-ROMFSTitle.Left;
 ROMFSVersion.Width:=ROMFSTitle.Width;
 ROMFSVersion.Top:=ROMFSTitle.Top+ROMFSTitle.Height+Round(8*ratio);
 ROMFSBinVersLabel.Top:=ROMFSVersion.Top+ROMFSVersion.Height+Round(8*ratio);
 ROMFSBinVers.Left:=ROMFSBinVersLabel.Width+ROMFSBinVersLabel.Left+Round(8*ratio);
 ROMFSBinVers.Top:=ROMFSBinVersLabel.Top;
 ROMFSBinVersAdj.Left:=ROMFSBinVers.Left+ROMFSBinVers.Width+Round(8*ratio);
 ROMFSBinVersAdj.Height:=ROMFSBinVers.Height;
 ROMFSBinVersAdj.Top:=ROMFSBinVersLabel.Top;
 ROMFSCopy.Width:=ROMFSTitle.Width;
 ROMFSCopy.Top:=ROMFSBinVers.Top+ROMFSBinVers.Height+Round(8*ratio);
 //Align the buttons -----------------------------------------------------------
 btn_Cancel:=MainForm.CreateButton(RFSDetailForm as TControl,'Cancel',False,0,
                      ROMFS.Top+ROMFS.Height+Round(8*ratio),
                      mrCancel);
 btn_OK:=MainForm.CreateButton(RFSDetailForm as TControl,'OK',True,0,
                               btn_Cancel.Top-Round(4*ratio),mrOK);
 btn_OK.Left:=Width-btn_OK.Width-Round(8*ratio);
 btn_Cancel.Left:=btn_OK.Left-Round(4*ratio)-btn_Cancel.Width;
 //Adjust the form size
 Height:=btn_OK.Top+btn_OK.Height+Round(8*ratio);
 Width:=ROMFS.Width;
end;

{-------------------------------------------------------------------------------
Texturise the form
-------------------------------------------------------------------------------}
procedure TRFSDetailForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

end.

