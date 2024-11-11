unit AboutUnit;

{
Copyright (C) 2018-2024 Gerald Holdsworth gerald@hollypops.co.uk

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

{$MODE objFPC}{$H+}

interface

uses
  SysUtils,Variants,Classes,Controls,Forms,Dialogs,ExtCtrls,StdCtrls,Clipbrd;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CreditsPanel: TPanel;
    IconImage: TImage;
    AckLabel: TLabel;
    GitHubWebsiteLabel: TLabel;
    LicenceLabel: TLabel;
    lb_Title: TLabel;
    WrittenByLabel: TLabel;
    lb_Version: TLabel;
    GHWebsiteLabel: TLabel;
    GraphicsLabel: TLabel;
    AdditionsByLabel: TLabel;
    procedure CreditsPanelPaint(Sender: TObject);
    procedure IconImageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses
  MainUnit;

{ TAboutForm }

procedure TAboutForm.CreditsPanelPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

procedure TAboutForm.IconImageClick(Sender: TObject);
begin
 Clipboard.AsText:=MainForm.debuglogfile;
 ShowMessage('Log file path and filename: '
            +#10#13+MainForm.debuglogfile+'.'
            +#10#13+'This has been copied to your clipboard.');
end;

end.
