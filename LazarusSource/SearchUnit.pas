unit SearchUnit;

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,ExtCtrls,StdCtrls,DiscImage,
 GJHCustomComponents;

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
  sb_SearchButton: TRISCOSButton;
  procedure FormCreate(Sender: TObject);
  procedure FormPaint(Sender: TObject);
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
 results: TSearchResults=nil;
 i      : Integer=0;
 line   : String='';
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
 //Search for files
 results:=MainForm.Image.FileSearch(search);
 //Clear the results
 lb_searchresults.Clear;
 //Now populate, if there is anything to add to it
 if Length(results)>0 then
  for i:=0 to Length(results)-1 do
  begin
   //Create the text
   line:=results[i].Parent
        +MainForm.Image.GetDirSep(results[i].Side)+results[i].Filename;
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
//Create the form
{------------------------------------------------------------------------------}
procedure TSearchForm.FormCreate(Sender: TObject);
var
 ratio: Real=0;
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 //Move the fields to account for scaling
 ed_filenamesearch.Left:=SearchFilenameLabel.Left
                        +SearchFilenameLabel.Width
                        +Round(4*ratio);
 ed_filenamesearch.Width:=SearchEntryPanel.Width
                         -ed_filenamesearch.Left
                         -Round(4*ratio);
 SearchFiletypeLabel.Left:=ed_filenamesearch.Left
                          -Round(4*ratio)
                          -SearchFiletypeLabel.Width;
 ed_filetypesearch.Left:=ed_filenamesearch.Left;
 //Create the button
 sb_SearchButton:=MainForm.CreateButton(SearchEntryPanel as TControl,'Search',
                                        False,0,0,mrNone);
 //And position it
 sb_SearchButton.Top:=ed_filetypesearch.Top;
 sb_SearchButton.Left:=SearchEntryPanel.Width
                      -sb_SearchButton.Width
                      -Round(4*ratio);
 sb_SearchButton.OnClick:=@sb_searchClick;
 //Adjust the panel height to account for scaling
 SearchEntryPanel.Height:=sb_SearchButton.Top
                        +sb_SearchButton.Height
                        +Round(4*ratio)
                        +searchresultscount.Height;
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
 searchresultscount.Caption:='Number of results found: '+IntToStr(lb_searchresults.Count);
end;

end.

