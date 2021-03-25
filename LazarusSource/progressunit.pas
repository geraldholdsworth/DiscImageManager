unit ProgressUnit;

{
Copyright (C) 2018-2021 Gerald Holdsworth gerald@hollypops.co.uk

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
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
