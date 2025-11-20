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
  OKButton,
  CancelButton: TRISCOSButton;
  procedure FormCreate(Sender: TObject);
  procedure FormShow(Sender: TObject);
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
 ratio: Real=0;
begin
 ratio                 :=PixelsPerInch/DesignTimePPI;
 ROMFSTitle.Width      :=ROMFS.Width-Round(12*ratio)-ROMFSTitle.Left;
 ROMFSVersion.Width    :=ROMFSTitle.Width;
 ROMFSVersion.Top      :=ROMFSTitle.Top+ROMFSTitle.Height+Round(8*ratio);
 ROMFSBinVersLabel.Top :=ROMFSVersion.Top+ROMFSVersion.Height+Round(8*ratio);
 ROMFSBinVers.Left     :=ROMFSBinVersLabel.Width+ROMFSBinVersLabel.Left+Round(8*ratio);
 ROMFSBinVers.Top      :=ROMFSBinVersLabel.Top;
 ROMFSBinVersAdj.Left  :=ROMFSBinVers.Left+ROMFSBinVers.Width+Round(8*ratio);
 ROMFSBinVersAdj.Height:=ROMFSBinVers.Height;
 ROMFSBinVersAdj.Top   :=ROMFSBinVersLabel.Top;
 ROMFSCopy.Width       :=ROMFSTitle.Width;
 ROMFSCopy.Top         :=ROMFSBinVers.Top+ROMFSBinVers.Height+Round(8*ratio);
 //Align the buttons -----------------------------------------------------------
 CancelButton     :=MainForm.CreateButton(RFSDetailForm as TControl,
                                          'Cancel',
                                          False,
                                          0,
                                          ROMFS.Top+ROMFS.Height+Round(8*ratio),
                                          mrCancel);
 OKButton         :=MainForm.CreateButton(RFSDetailForm as TControl,
                                          'OK',
                                          True,
                                          0,
                                          CancelButton.Top-Round(4*ratio),
                                          mrOK);
 OKButton.Left    :=Width-OKButton.Width-Round(8*ratio);
 CancelButton.Left:=OKButton.Left-Round(4*ratio)-CancelButton.Width;
 //Adjust the form size
 Height           :=OKButton.Top+OKButton.Height+Round(8*ratio);
 Width            :=ROMFS.Width;
end;

{------------------------------------------------------------------------------}
//Show the form - set it up
{------------------------------------------------------------------------------}
procedure TRFSDetailForm.FormShow(Sender: TObject);
begin
 OKButton.NativeOS    :=MainForm.Fstyling=MainForm.NativeStyle;
 CancelButton.NativeOS:=MainForm.Fstyling=MainForm.NativeStyle;
 //Re-align the buttons
 CancelButton.Top     :=OKButton.Top+(OKButton.Height-CancelButton.Height)div 2;
end;

{-------------------------------------------------------------------------------
Texturise the form
-------------------------------------------------------------------------------}
procedure TRFSDetailForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

end.

