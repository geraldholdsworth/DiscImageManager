unit SearchUnit;

{
Copyright (C) 2018-2021 Gerald Holdsworth gerald@hollypops.co.uk

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,ExtCtrls,StdCtrls,Buttons,
 DiscImageUtils;

type

 { TSearchForm }

 TSearchForm = class(TForm)
  ed_filenamesearch: TEdit;
  ed_filetypesearch: TEdit;
  lb_searchresults: TListBox;
  SearchEntryPanel: TPanel;
  SearchFilenameLabel: TLabel;
  SearchFiletypeLabel: TLabel;
  searchresultscount: TLabel;
  sb_SearchButton: TSpeedButton;
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure sb_searchClick(Sender: TObject);
  procedure ed_filenamesearchKeyPress(Sender: TObject; var Key: char);
  procedure lb_searchresultsClick(Sender: TObject);
  procedure ResetSearchFields;
 private

 public

 end;

var
 SearchForm: TSearchForm;

implementation

{$R *.lfm}

uses MainUnit;

{------------------------------------------------------------------------------}
//Search for files
{------------------------------------------------------------------------------}
procedure TSearchForm.sb_searchClick(Sender: TObject);
var
 search : TDirEntry;
 results: TSearchResults;
 i      : Integer;
 line   : String;
begin
 ResetDirEntry(search);
 SetLength(results,0);
 //Get the search criteria
 search.Filename:=ed_filenamesearch.Text;
 //Filetype
 line:=IntToHex(StrToIntDef('$'+UpperCase(ed_filetypesearch.Text),0),3);
 if line=UpperCase(ed_filetypesearch.Text) then
  search.ShortFiletype:=ed_filetypesearch.Text //Hex number specified?
 else
  search.Filetype:=ed_filetypesearch.Text; //Or full filetype string?
{ //Validate that the length entered is a hex number or zero if not
 search.Length:=StrToIntDef('$'+ed_lengthsearch.Text,0);}
 //Search for files
 results:=MainForm.Image.FileSearch(search);
 //Clear the results
 lb_searchresults.Clear;
 //Now populate, if there is anything to add to it
 if Length(results)>0 then
  for i:=0 to Length(results)-1 do
  begin
   //Create the text
   line:=results[i].Parent+MainForm.Image.DirSep+results[i].Filename;
   //Remove any top bit set characters
   RemoveTopBit(line);
   //And list the result
   lb_searchresults.Items.Add(line);
  end;
 //And report how many results
 searchresultscount.Caption:='Number of results found: '+IntToStr(lb_searchresults.Count);
end;

{------------------------------------------------------------------------------}
//Tile the form
{------------------------------------------------------------------------------}
procedure TSearchForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

{------------------------------------------------------------------------------}
//Re-arrange the controls
{------------------------------------------------------------------------------}
procedure TSearchForm.FormShow(Sender: TObject);
begin
 ed_filenamesearch.Left:=(SearchFilenameLabel.Left+SearchFilenameLabel.Width)+4;
 ed_filenamesearch.Width:=(SearchEntryPanel.Width-ed_filenamesearch.Left)-4;
 SearchFiletypeLabel.Left:=(ed_filenamesearch.Left-4)-SearchFiletypeLabel.Width;
 ed_filetypesearch.Left:=ed_filenamesearch.Left;
 sb_SearchButton.Top:=ed_filetypesearch.Top;
 sb_SearchButton.Left:=(SearchEntryPanel.Width-sb_SearchButton.Width)-4;
end;

{------------------------------------------------------------------------------}
//User has pressed return on a search edit field
{------------------------------------------------------------------------------}
procedure TSearchForm.ed_filenamesearchKeyPress(Sender: TObject; var Key: char);
begin
 if Key=#13 then sb_SearchClick(Sender);
end;

{------------------------------------------------------------------------------}
//Highlight the file in the tree
{------------------------------------------------------------------------------}
procedure TSearchForm.lb_searchresultsClick(Sender: TObject);
begin
 //Is there one selected?
 if lb_searchresults.ItemIndex>=0 then
  MainForm.SelectNode(lb_searchresults.Items[lb_searchresults.ItemIndex]);
end;

{------------------------------------------------------------------------------}
//Clear the search edit boxes
{------------------------------------------------------------------------------}
procedure TSearchForm.ResetSearchFields;
begin
 lb_searchresults.Clear;
 ed_filenamesearch.Text:='';
 //ed_lengthsearch.Text:='';
 searchresultscount.Caption:='Number of results found: '+IntToStr(lb_searchresults.Count);
end;

end.

