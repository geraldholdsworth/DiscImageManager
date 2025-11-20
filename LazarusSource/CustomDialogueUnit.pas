unit CustomDialogueUnit;

{
Copyright (C) 2018-2025 Gerald Holdsworth gerald@hollypops.co.uk

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public Licence as published by the Free
Software Foundation; either version 3 of the Licence, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public Licence for more
details.

A copy of the GNU General Public Licence is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}

interface

uses
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,ExtCtrls,StdCtrls,
 GJHCustomComponents;

type

 { TCustomDialogue }

 TCustomDialogue = class(TForm)
  MainBevel: TBevel;
  ErrorImg: TImage;
  MessageLabel: TLabel;
  MessagePanel: TPanel;
  QuestionImg: TImage;
  InfoImg: TImage;
  IgnoreButton,
  AbortButton,
  CancelButton,
  OKButton: TRISCOSButton;
  procedure FormCreate(Sender: TObject);
  procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure ShowError(msg,BtnTxt: String);
  procedure ShowConfirm(msg: String; Buttons: array of String);
  procedure ShowInfo(msg,BtnTxt: String);
  procedure ShowDialogue(msg: String; Buttons: array of String; style: Byte);
 private

 public

 end;

var
 CustomDialogue: TCustomDialogue;

implementation

{$R *.lfm}

uses MainUnit;

{ TCustomDialogue }

{-------------------------------------------------------------------------------
Tile the window
-------------------------------------------------------------------------------}
procedure TCustomDialogue.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

{------------------------------------------------------------------------------}
//Show the form - set it up
{------------------------------------------------------------------------------}
procedure TCustomDialogue.FormShow(Sender: TObject);
begin
 //Style the buttons
 IgnoreButton.NativeOS:=MainForm.Fstyling=MainForm.NativeStyle;
 AbortButton.NativeOS :=MainForm.Fstyling=MainForm.NativeStyle;
 OKButton.NativeOS    :=MainForm.Fstyling=MainForm.NativeStyle;
 CancelButton.NativeOS:=MainForm.Fstyling=MainForm.NativeStyle;
 //Re-align the buttons
 CancelButton.Top     :=OKButton.Top+(OKButton.Height-CancelButton.Height)div 2;
 AbortButton.Top      :=CancelButton.Top;
 IgnoreButton.Top     :=CancelButton.Top;
end;

{-------------------------------------------------------------------------------
Create the buttons and move things around for scaling
-------------------------------------------------------------------------------}
procedure TCustomDialogue.FormCreate(Sender: TObject);
var
 ratio  : Real=0;
 TopBut : Integer=0;
 WinWid : Integer=0;
const
 Pad    = 8; //Padding
 NoBtns = 4; //Number of buttons
 BtnWid = 92;//Width of buttons (OK button is bigger, so we use this value)
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 //Set width: ( buttons width + gap between ) * number of buttons + gap at edge
 Width             :=Round(((BtnWid+Pad)*NoBtns+Pad)*ratio);
 MainBevel.Width   :=Width;
 MessagePanel.Width:=Width-MessagePanel.Left-Round(Pad*ratio);
 //Create the buttons
 WinWid:=Width-Round(Pad*ratio); //Width of the window, minus the padding
 TopBut:=MainBevel.Top+MainBevel.Height+Round((4+Pad)*ratio);//Top of the buttons
 //'Abort' Button
 AbortButton      :=MainForm.CreateButton(CustomDialogue as TControl,'Abort',
                                          False,0,TopBut,mrAbort);
 AbortButton.Left :=(WinWid div NoBtns)*0+Round(Pad*ratio); //Far left
 //'Ignore' Button
 IgnoreButton     :=MainForm.CreateButton(CustomDialogue as TControl,'Ignore',
                                          False,0,TopBut,mrIgnore);
 IgnoreButton.Left:=(WinWid div NoBtns)*1+Round(Pad*ratio); //Left of middle
 //'Cancel' Button
 CancelButton     :=MainForm.CreateButton(CustomDialogue as TControl,'Cancel',
                                          False,0,TopBut,mrCancel);
 CancelButton.Left:=(WinWid div NoBtns)*2+Round(Pad*ratio); //Right of middle
 //'OK' Button - This is 8px higher and wider
 OKButton         :=MainForm.CreateButton(CustomDialogue as TControl,'OK',
                                          True, 0,TopBut-Round(4*ratio),mrOK);
 OKButton.Left    :=(WinWid div NoBtns)*3+Round(Pad*ratio); //Far right
 //Adjust the form height
 Height           :=OKButton.Top+OKButton.Height+Round(Pad*ratio);
end;

{-------------------------------------------------------------------------------
User has pressed a key
-------------------------------------------------------------------------------}
procedure TCustomDialogue.FormKeyDown(Sender:TObject;var Key:Word;
 Shift:TShiftState);
begin
 if(Shift=[ssShift])and(Key=$0D)and(AbortButton.Visible) then AbortButton.Click;  //Shift + Return
 if(Shift=[ssShift])and(Key=$1B)and(IgnoreButton.Visible)then IgnoreButton.Click; //Shift + Escape
 if(Shift=[])       and(Key=$0D)                         then OKButton.Click;     //Return
 if(Shift=[])       and(Key=$1B)and(CancelButton.Visible)then CancelButton.Click; //Escape
end;

{-------------------------------------------------------------------------------
Show an error message
-------------------------------------------------------------------------------}
procedure TCustomDialogue.ShowError(msg,BtnTxt: String);
begin
 ShowDialogue(msg,[BtnTxt],0);
end;

{-------------------------------------------------------------------------------
Ask the user for a confirmation
-------------------------------------------------------------------------------}
procedure TCustomDialogue.ShowConfirm(msg: String; Buttons: array of String);
begin
 ShowDialogue(msg,Buttons,2);
end;

{-------------------------------------------------------------------------------
Show information
-------------------------------------------------------------------------------}
procedure TCustomDialogue.ShowInfo(msg,BtnTxt: String);
begin
 ShowDialogue(msg,[BtnTxt],1);
end;

{-------------------------------------------------------------------------------
General display procedure
-------------------------------------------------------------------------------}
procedure TCustomDialogue.ShowDialogue(msg: String; Buttons: array of String; style: Byte);
var
 OKBtnTxt     : String='';
 CancelBtnTxt : String='';
 IgnoreBtnTxt : String='';
 AbortBtnTxt  : String='';
begin
// OKBtnTxt:='';
 if style>2 then exit;
 //Get the button text strings
 if Length(Buttons)>0 then OKBtnTxt    :=Buttons[0];
 if Length(Buttons)>1 then CancelBtnTxt:=Buttons[1] else CancelBtnTxt:='';
 if Length(Buttons)>2 then IgnoreBtnTxt:=Buttons[2] else IgnoreBtnTxt:='';
 if Length(Buttons)>3 then AbortBtnTxt :=Buttons[3] else AbortBtnTxt :='';
 //Supplying an empty Buttons field will produce all four buttons, labelled
 //with the default text
 if Length(Buttons)=0 then
 begin
  OKBtnTxt    :='OK';
  CancelBtnTxt:='Cancel';
  IgnoreBtnTxt:='Ignore';
  AbortBtnTxt :='Abort';
 end;
 if OKBtnTxt='' then OKBtnTxt:='OK';
 //We need to briefly show the form so the controls resize
 Show;
 //Hide all the graphics
 ErrorImg.Visible    :=style=0;
 InfoImg.Visible     :=style=1;
 QuestionImg.Visible :=style=2;
 MessageLabel.Caption:='';
 MessageLabel.Constraints.MaxWidth :=MessagePanel.ClientWidth;
 MessageLabel.Constraints.MaxHeight:=MessagePanel.ClientHeight;
 //Display the message
 MessageLabel.Caption:=msg;
 //And reposition it
 MessageLabel.Left   :=(MessagePanel.Width -MessageLabel.Width )div 2;
 MessageLabel.Top    :=(MessagePanel.Height-MessageLabel.Height)div 2;
 //Label the default button - this will always show
 OKButton.Caption    :=OKBtnTxt;
 //Label the cancel button and show or hide it
 CancelButton.Visible:=CancelBtnTxt<>'';
 CancelButton.Caption:=CancelBtnTxt;
 //Label the ignore button and show or hide it
 IgnoreButton.Visible:=IgnoreBtnTxt<>'';
 IgnoreButton.Caption:=IgnoreBtnTxt;
 //Label the abort button and show or hide it
 AbortButton.Visible :=AbortBtnTxt<>'';
 AbortButton.Caption :=AbortBtnTxt;
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
