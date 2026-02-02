program DiscImageManager;

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

{$MODE objFPC}{$H+}

uses
  Forms,Interfaces,
  MainUnit in 'MainUnit.pas',
  DiscImage in 'DiscImage.pas',
  AboutUnit in 'AboutUnit.pas',
  NewImageUnit in 'NewImageUnit.pas',
  ProgressUnit in 'ProgressUnit.pas',
  ImageDetailUnit in 'ImageDetailUnit.pas',
  //HexDumpUnit in 'HexDumpUnit.pas',
  SearchUnit in 'SearchUnit.pas',
  CustomDialogueUnit in 'CustomDialogueUnit.pas',
  HardDriveUnit in 'HardDriveUnit.pas',
  ErrorLogUnit in 'ErrorLogUnit.pas',
  SettingsUnit in 'SettinsUnit.pas',
  ImportSelectorUnit in 'ImportSelectorUnit.pas',
  PWordEditorUnit in 'PWordEditorUnit.pas',
  AFSPartitionUnit in 'AFSParitionUnit.pas',
  ChangeInterleaveUnit in 'ChangeInterleaveUnit.pas',
  CSVPrefUnit in 'CSVPrefUnit.pas',
  ImageReportUnit in 'ImageReportUnit.pas', RFSDetailUnit, DiscImageHelper;

{$R *.res}

{-------------------------------------------------------------------------------
Main program execution starts here
-------------------------------------------------------------------------------}
begin
 //Create GUI application
 RequireDerivedFormResource:=True;
 Application.Scaled:=True;
 Application.Title:='Disc Image Manager';
 Application.Initialize;
 Application.CreateForm(TMainForm, MainForm);
 Application.CreateForm(TAboutForm, AboutForm);
 Application.CreateForm(TNewImageForm, NewImageForm);
 Application.CreateForm(TProgressForm, ProgressForm);
 Application.CreateForm(TImageDetailForm, ImageDetailForm);
 Application.CreateForm(TSearchForm, SearchForm);
 Application.CreateForm(TCustomDialogue, CustomDialogue);
 Application.CreateForm(THardDriveForm, HardDriveForm);
 Application.CreateForm(TErrorLogForm, ErrorLogForm);
 Application.CreateForm(TSettingsForm, SettingsForm);
 Application.CreateForm(TImportSelectorForm, ImportSelectorForm);
 Application.CreateForm(TPwordEditorForm, PwordEditorForm);
 Application.CreateForm(TAFSPartitionForm, AFSPartitionForm);
 Application.CreateForm(TChangeInterleaveForm, ChangeInterleaveForm);
 Application.CreateForm(TCSVPrefForm, CSVPrefForm);
 Application.CreateForm(TImageReportForm, ImageReportForm);
 Application.CreateForm(TRFSDetailForm, RFSDetailForm);
 Application.Run;
end.
