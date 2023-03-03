unit PWordEditorUnit;

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
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
 GJHCustomComponents, DiscImageUtils, Buttons;

type

 { TPwordEditorForm }

 TPwordEditorForm = class(TForm)
  AccountsPanel: TPanel;
  HeaderPanel: TPanel;
  AccountsScroll: TScrollBox;
  ControlsPanel: TPanel;
  Label1: TLabel;
  Label2: TLabel;
  Label3: TLabel;
  Label4: TLabel;
  Label5: TLabel;
  btnAdd: TSpeedButton;
  UsernamesHdr: TPanel;
  PasswordsHdr: TPanel;
  SystemHdr: TPanel;
  LockedHdr: TPanel;
  BootOptionHdr: TPanel;
  OKButton,
  CancelButton: TGJHButton;
  procedure btnAddClick(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure FormHide(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  function CreateNewEntry: Integer;
  function CreateEditField(column: TPanel;maxlen,len: Integer): TEdit;
  function CreateTickBox(column: TPanel;len: Integer): TGJHTickBox;
  function CreateDropDown(column: TPanel;len: Integer): TComboBox;
  procedure OKButtonClick(Sender: TObject);
 private
  Usernames: array of TEdit;
  Passwords: array of TEdit;
  System   : array of TGJHTickBox;
  Locked   : array of TGJHTickBox;
  BootOpts : array of TComboBox;
  FreeSpc  : array of TEdit;
 public
  UserAccounts: TUserAccounts;
 end;

var
 PwordEditorForm: TPwordEditorForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TPwordEditorForm }

{------------------------------------------------------------------------------}
//Set the form up prior to opening it
{------------------------------------------------------------------------------}
procedure TPwordEditorForm.FormShow(Sender: TObject);
var
 index,
 ctrl  : Integer;
begin
 //Is there anything to show?
 if Length(UserAccounts)>0 then
  for index:=0 to Length(UserAccounts)-1 do
  begin
   //Create a new entry and make a note of the reference
   ctrl:=CreateNewEntry;
   //Fill in the fields
   Usernames[ctrl].Text    :=UserAccounts[index].Username;
   Passwords[ctrl].Text    :=UserAccounts[index].Password;
   System[ctrl].Ticked     :=UserAccounts[index].System;
   Locked[ctrl].Ticked     :=UserAccounts[index].Locked;
   BootOpts[ctrl].ItemIndex:=UserAccounts[index].BootOption;
   FreeSpc[ctrl].Text      :=IntToHex(UserAccounts[index].FreeSpace,8);
  end;
end;

{------------------------------------------------------------------------------}
//Hide the form and delete all the dynamic components
{------------------------------------------------------------------------------}
procedure TPwordEditorForm.FormHide(Sender: TObject);
var
 len: Integer;
begin
 //Continue while there are some components left
 while Length(Usernames)>0 do
 begin
  //Take a note of the reference of the last one
  len:=Length(Usernames)-1;
  //Free up the components
  Usernames[len].Free;
  Passwords[len].Free;
  System[len].Free;
  Locked[len].Free;
  BootOpts[len].Free;
  FreeSpc[len].Free;
  //Reduce the array lengths
  SetLength(Usernames,len);
  SetLength(Passwords,len);
  SetLength(System,len);
  SetLength(Locked,len);
  SetLength(BootOpts,len);
  SetLength(FreeSpc,len);
 end;
 btnAdd.Top:=0;
end;

{------------------------------------------------------------------------------}
//Texturise the form
{------------------------------------------------------------------------------}
procedure TPwordEditorForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

{------------------------------------------------------------------------------}
//Adds a new entry
{------------------------------------------------------------------------------}
procedure TPwordEditorForm.btnAddClick(Sender: TObject);
begin
 CreateNewEntry;
end;

{------------------------------------------------------------------------------}
//Create the form
{------------------------------------------------------------------------------}
procedure TPwordEditorForm.FormCreate(Sender: TObject);
var
 ratio  : Real;
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 OKButton:=MainForm.CreateButton(ControlsPanel as TControl,'OK',True,0,
                                 Round(8*ratio),mrOK);
 OKButton.Left:=ControlsPanel.ClientWidth-OKButton.Width-Round(8*ratio);
 OKButton.OnClick:=@OKButtonClick;
 CancelButton:=MainForm.CreateButton(ControlsPanel as TControl,'Cancel',False,
                                     0,Round(12*ratio),mrCancel);
 CancelButton.Left:=OKButton.Left-Round(8*ratio)-CancelButton.Width;
 ControlsPanel.Height:=OKButton.Top+OKButton.Height+Round(8*ratio);
end;

{------------------------------------------------------------------------------}
//Create a new entry, including creating the components
{------------------------------------------------------------------------------}
function TPwordEditorForm.CreateNewEntry: Integer;
begin
 //Take a note of the reference for the last one
 Result:=Length(Usernames);
 //Username
 SetLength(Usernames,Result+1);
 Usernames[Result]:=CreateEditField(UsernamesHdr,20,Result);
 //Password
 SetLength(Passwords,Result+1);
 Passwords[Result]:=CreateEditField(PasswordsHdr,10,Result);
 //Is it a system account?
 SetLength(System,Result+1);
 System[Result]:=CreateTickBox(SystemHdr,Result);
 //Is it locked?
 SetLength(Locked,Result+1);
 Locked[Result]:=CreateTickBox(LockedHdr,Result);
 //Boot option
 SetLength(BootOpts,Result+1);
 BootOpts[Result]:=CreateDropDown(BootOptionHdr,Result);
 //Free space
 SetLength(FreeSpc,Result+1);
 FreeSpc[Result]:=CreateEditField(UsernamesHdr,8,Result);
 FreeSpc[Result].Visible:=False; //This is hidden
 //Move the add button
 btnAdd.Top:=Usernames[Result].Top+Usernames[Result].Height;
end;

{------------------------------------------------------------------------------}
//Create a TEdit
{------------------------------------------------------------------------------}
function TPwordEditorForm.CreateEditField(column: TPanel;maxlen,len: Integer): TEdit;
begin
 Result:=TEdit.Create(AccountsScroll);
 Result.Parent:=AccountsScroll;
 Result.Left:=column.Left;
 Result.Width:=column.Width;
 Result.Height:=22;
 Result.Top:=(2+column.Height)*len;
 Result.MaxLength:=maxlen;
end;

{------------------------------------------------------------------------------}
//Create a TCheckBox
{------------------------------------------------------------------------------}
function TPwordEditorForm.CreateTickBox(column: TPanel;len: Integer): TGJHTickBox;
begin
 Result:=TGJHTickBox.Create(AccountsScroll);
 Result.Parent:=AccountsScroll;
 Result.Left:=column.Left+(column.Width-18)div 2;
 Result.Top:=2+(2+column.Height)*len;
 Result.Caption:='';
end;

{------------------------------------------------------------------------------}
//Create a TComboBox
{------------------------------------------------------------------------------}
function TPwordEditorForm.CreateDropDown(column: TPanel;len: Integer): TComboBox;
begin
 Result:=TComboBox.Create(AccountsScroll);
 Result.Parent:=AccountsScroll;
 Result.Left:=column.Left;
 Result.Top:=(2+column.Height)*len;
 Result.Width:=column.Width;
 Result.Items.AddCommaText('None,Load,Run,Execute');
 Result.ItemIndex:=0;
 Result.Style:=csDropDownList;
end;

{------------------------------------------------------------------------------}
//Gather the entries
{------------------------------------------------------------------------------}
procedure TPwordEditorForm.OKButtonClick(Sender: TObject);
var
 index: Integer;
begin
 //Reset the accounts to none
 SetLength(UserAccounts,0);
 //If there are any fields, go through them
 if Length(Usernames)>0 then
  for index:=0 to Length(Usernames)-1 do
   if Usernames[index].Text<>'' then //If the username is blank, skip it
   begin
    //Increase the accounts array by one
    SetLength(UserAccounts,Length(UserAccounts)+1);
    //Populate it
    UserAccounts[Length(UserAccounts)-1].Username  :=Usernames[index].Text;
    UserAccounts[Length(UserAccounts)-1].Password  :=Passwords[index].Text;
    UserAccounts[Length(UserAccounts)-1].System    :=System[index].Ticked;
    UserAccounts[Length(UserAccounts)-1].Locked    :=Locked[index].Ticked;
    UserAccounts[Length(UserAccounts)-1].BootOption:=BootOpts[index].ItemIndex;
    UserAccounts[Length(UserAccounts)-1].FreeSpace :=StrToIntDef('$'+FreeSpc[index].Text,0);
   end;
end;

end.
