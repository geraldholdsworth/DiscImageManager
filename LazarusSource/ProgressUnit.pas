unit ProgressUnit;

{
Copyright (C) 2018-2023 Gerald Holdsworth gerald@hollypops.co.uk

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,StdCtrls,ExtCtrls;

type

 { TProgressForm }

 TProgressForm = class(TForm)
  WaitLabel: TLabel;
  UpdateProgress: TLabel;
  MainPanel: TPanel;
  procedure FormPaint(Sender: TObject);
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
 //Arrange the window so it covers the main form
 Top:=MainForm.Top-8;
 Left:=MainForm.Left-8;
 Width:=MainForm.Width+16;
 Height:=MainForm.Height+MainForm.ImageDetails.Height+16;
 //Don't need a title
 UpdateProgress.Caption:='';
 //Re-position the 'Waiting' label
 WaitLabel.Left:=(Width-WaitLabel.Width)div 2;
 WaitLabel.Top:=(Height-WaitLabel.Height)div 4;
 //Re-position the 'Progress' label
 UpdateProgress.Left:=0;
 UpdateProgress.Width:=Width;
 UpdateProgress.Top:=WaitLabel.Top+WaitLabel.Height+16;
end;

procedure TProgressForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

end.
