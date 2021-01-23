program DiscImageManager;

{$MODE objFPC}{$H+}

uses
  Forms, Interfaces,
  MainUnit in 'MainUnit.pas',
  DiscImage in 'DiscImage.pas',
  AboutUnit in 'AboutUnit.pas',
  NewImageUnit in 'NewImageUnit.pas',
  ImageDetailUnit in 'ImageDetailUnit.pas',
  DiscImageUtils;

{$R *.res}

begin
 Application.Scaled:=True;
 Application.Title:='Disc Image Manager';
 Application.Initialize;
 Application.CreateForm(TMainForm, MainForm);
 Application.CreateForm(TAboutForm, AboutForm);
 Application.CreateForm(TNewImageForm, NewImageForm);
 Application.CreateForm(TImageDetailForm, ImageDetailForm);
 Application.Run;
end.
