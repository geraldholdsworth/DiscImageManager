program DiscImageManager;

{$MODE objFPC}{$H+}

uses
  Forms,Interfaces,DiscImageUtils,Global,Spark,
  MainUnit in 'MainUnit.pas',
  DiscImage in 'DiscImage.pas',
  AboutUnit in 'AboutUnit.pas',
  NewImageUnit in 'NewImageUnit.pas',
  ProgressUnit in 'ProgressUnit.pas',
  ImageDetailUnit in 'ImageDetailUnit.pas',
  HexDumpUnit in 'HexDumpUnit.pas',
  SplitDFSUnit in 'SplitDFSUnit.pas',
  SearchUnit in 'SearchUnit.pas',
  CustomDialogueUnit in 'CustomDialogueUnit.pas';

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
 Application.CreateForm(TSearchForm, SearchForm);
 Application.CreateForm(TCustomDialogue, CustomDialogue);
 Application.Run;
end.
