program DiscImageReader;

{$MODE objFPC}{$H+}

uses
  Forms, Interfaces,
  MainUnit in 'MainUnit.pas' {MainForm},
  DiscImage in 'DiscImage.pas',
  AboutUnit in 'AboutUnit.pas' {AboutForm};

{$R *.res}

begin
  //RequireDerivedFormResource:=True;
  Application.Scaled:=True;
 Application.Title:='Disc Image Manager';
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.
