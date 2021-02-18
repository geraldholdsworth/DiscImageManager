program DiscImageManager;

{$MODE objFPC}{$H+}

uses
  Forms, Interfaces,
  MainUnit in 'MainUnit.pas',
  DiscImage in 'DiscImage.pas',
  AboutUnit in 'AboutUnit.pas',
  NewImageUnit in 'NewImageUnit.pas',
  DiscImageUtils, Global,
  ProgressUnit in 'ProgressUnit.pas',
  ImageDetailUnit in 'ImageDetailUnit.pas',
  HexDumpUnit in 'HexDumpUnit.pas',
  SplitDFSUnit in 'SplitDFSUnit.pas';

{$R *.res}

begin
 Application.Scaled:=True;
 Application.Title:='Disc Image Manager';
 Application.Initialize;
 Application.CreateForm(TMainForm, MainForm);
 Application.CreateForm(TAboutForm, AboutForm);
 Application.CreateForm(TNewImageForm, NewImageForm);
 Application.CreateForm(TProgressForm, ProgressForm);
 Application.CreateForm(TImageDetailForm, ImageDetailForm);
 Application.CreateForm(TSplitDFSForm, SplitDFSForm);
 Application.Run;
end.
