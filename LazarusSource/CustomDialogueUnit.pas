unit CustomDialogueUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
 Buttons;

type

 { TCustomDialogue }

 TCustomDialogue = class(TForm)
  IgnoreButton: TBitBtn;
  CancelButton: TBitBtn;
  OKButton: TBitBtn;
  MainBevel: TBevel;
  ErrorImg: TImage;
  MessageLabel: TLabel;
  MessagePanel: TPanel;
  OKBtnBack: TPanel;
  QuestionImg: TImage;
  InfoImg: TImage;
  procedure FormPaint(Sender: TObject);
  procedure ShowError(msg,BtnTxt: String);
  procedure ShowConfirm(msg,OKBtnTxt,CancelBtnTxt,IgnoreBtnTxt: String);
  procedure ShowInfo(msg,BtnTxt: String);
  procedure ShowDialogue(msg,OKBtnTxt,CancelBtnTxt,IgnoreBtnTxt: String;style: Byte);
 private

 public

 end;

var
 CustomDialogue: TCustomDialogue;

implementation

{$R *.lfm}

uses MainUnit;

{ TCustomDialogue }

{------------------------------------------------------------------------------}
//Tile the window
{------------------------------------------------------------------------------}
procedure TCustomDialogue.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

{------------------------------------------------------------------------------}
//Show an error message
{------------------------------------------------------------------------------}
procedure TCustomDialogue.ShowError(msg,BtnTxt: String);
begin
 ShowDialogue(msg,BtnTxt,'','',0);
end;

{------------------------------------------------------------------------------}
//Ask the user for a confirmation
{------------------------------------------------------------------------------}
procedure TCustomDialogue.ShowConfirm(msg,OKBtnTxt,CancelBtnTxt,IgnoreBtnTxt: String);
begin
 ShowDialogue(msg,OKBtnTxt,CancelBtnTxt,IgnoreBtnTxt,2);
end;

{------------------------------------------------------------------------------}
//Show information
{------------------------------------------------------------------------------}
procedure TCustomDialogue.ShowInfo(msg,BtnTxt: String);
begin
 ShowDialogue(msg,BtnTxt,'','',1);
end;

{------------------------------------------------------------------------------}
//General display procedure
{------------------------------------------------------------------------------}
procedure TCustomDialogue.ShowDialogue(msg,OKBtnTxt,CancelBtnTxt,IgnoreBtnTxt: String
                                                                  ;style: Byte);
begin
 if style>2 then exit;
 //We need to briefly show the form so the controls resize
 Show;
 //Hide all the graphics
 ErrorImg.Visible   :=False;
 InfoImg.Visible    :=False;
 QuestionImg.Visible:=False;
 //Now show the appropriate one
 case style of
  0: ErrorImg.Visible   :=True;
  1: InfoImg.Visible    :=True;
  2: QuestionImg.Visible:=True;
 end;
 //Display the message
 MessageLabel.Caption:=msg;
 //And reposition it
 MessageLabel.Left:=(MessagePanel.Width-MessageLabel.Width)div 2;
 MessageLabel.Top:=(MessagePanel.Height-MessageLabel.Height)div 2;
 //Label the default button
 if OKBtnTxt='' then OKBtnTxt:='&OK';
 OKButton.Caption:=OKBtnTxt;
 //Label the cancel button, and then show or hide it
 if CancelBtnTxt='' then CancelButton.Visible:=False
 else
 begin
   CancelButton.Visible:=True;
   CancelButton.Caption:=CancelBtnTxt;
 end;
 //Label the ignore button, and then show or hide it
 if IgnoreBtnTxt='' then IgnoreButton.Visible:=False
 else
 begin
   IgnoreButton.Visible:=True;
   IgnoreButton.Caption:=IgnoreBtnTxt;
 end;
 //Change the title
 case style of
  0: Caption:='Error';
  1: Caption:='Information';
  2: Caption:='Confirmation';
 end;
 //Now hide it before we show it as a modal
 Hide;
 ShowModal;
end;

end.

