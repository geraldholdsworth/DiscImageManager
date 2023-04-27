unit ImportSelectorUnit;

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,ComCtrls,ExtCtrls,LCLIntf,
 GJHCustomComponents,DiscImage;

type

 { TImportSelectorForm }

 TImportSelectorForm = class(TForm)
  TickIcons: TImageList;
  ImageList2: TImageList;
  ButtonPanel: TPanel;
  ImportDirList: TTreeView;
  OKButton,
  CancelButton: TGJHButton;
  procedure FormCreate(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure ImportDirListCreateNodeClass(Sender: TCustomTreeView;
   var NodeClass: TTreeNodeClass);
  procedure ImportDirListCustomDraw(Sender: TCustomTreeView;
   const ARect: TRect; var DefaultDraw: Boolean);
  procedure ImportDirListGetImageIndex(Sender: TObject; Node: TTreeNode);
  procedure ButtonPanelResize(Sender: TObject);
  procedure TickNode(Node: TTreeNode; Ticked: Boolean);
  procedure ToggleTreeViewTickBoxes(Node: TTreeNode);
  function IsNodeTicked(ANode:TTreeNode): Boolean;
  function IsNodeTicked(dir,entry: Integer): Boolean; overload;
  procedure TreeView1Click(Sender: TObject);
 private
   ratio  : Real;
  const
   //Tick, Untick or Indeterminate graphics
   ImgIndexTicked  = 0;
   ImgIndexUnTicked= 1;
   ImgIndexInter   = 2;
 public
  //Copy of the image that is being imported (used to get the correct icon)
  FImage: TDiscImage;
 end;

var
 ImportSelectorForm: TImportSelectorForm;

implementation

{$R *.lfm}

uses MainUnit;

{------------------------------------------------------------------------------}
//Tick, or untick, the node. Affects any children and parents too.
{------------------------------------------------------------------------------}
procedure TImportSelectorForm.TickNode(Node: TTreeNode; Ticked: Boolean);
var
 Child: TTreeNode;
 ParentStatus: Integer;
begin
 //Ensure that the node actually exists
 if Assigned(Node) then
 begin
  //Set it
  if Ticked     then Node.StateIndex:=ImgIndexTicked;
  //Unset it
  if not Ticked then Node.StateIndex:=ImgIndexUnTicked;
  //Now we'll set/unset any children and grandchildren
  Child:=Node.GetFirstChild; //The first child
  //Continue until there are no more
  while Child<>nil do
  begin
   //Copy the parent's ticked status
   TickNode(Child,Ticked);
   //And move onto the next child
   Child:=Node.GetNextChild(Child);
  end;
  //Now we'll go upwards and do the parents and grandparents
  while Node.Parent<>nil do //While there is a parent
  begin
   //Make a note of this node's status
   ParentStatus:=Node.StateIndex;
   //Get the parent's first child, which could be this one
   Child:=Node.Parent.GetFirstChild;
   //And go through all the parent's children
   while Child<>nil do
   begin
    //If different to what we have noted then change the noted status
    if Child.StateIndex<>ParentStatus then ParentStatus:=ImgIndexInter;
    //And next child
    Child:=Node.Parent.GetNextChild(Child);
   end;
   //Assign the noted status to the parent
   Node.Parent.StateIndex:=ParentStatus;
   //And move up a level
   Node:=Node.Parent;
  end;
 end;
end;

{------------------------------------------------------------------------------}
//This just creates our custom TTreeNode
{------------------------------------------------------------------------------}
procedure TImportSelectorForm.ImportDirListCreateNodeClass(
 Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
 NodeClass:=TMyTreeNode;
end;

{------------------------------------------------------------------------------}
//Tile the tree
{------------------------------------------------------------------------------}
procedure TImportSelectorForm.ImportDirListCustomDraw(Sender: TCustomTreeView;
 const ARect: TRect; var DefaultDraw: Boolean);
begin
 //Tile the tree
 MainForm.TileCanvas(Sender.Canvas,ARect);
end;

{------------------------------------------------------------------------------}
//Update the icons
{------------------------------------------------------------------------------}
procedure TImportSelectorForm.ImportDirListGetImageIndex(Sender: TObject;
 Node: TTreeNode);
begin
 MainForm.WriteToDebug('Selector Form -> ImportDirList');
 if Visible then MainForm.WriteToDebug('Selector Form Visible')
 else MainForm.WriteToDebug('Selector Form not Visible');
 MainForm.GetImageIndex(Node,FImage);
end;

{------------------------------------------------------------------------------}
//The button panel is being resized
{------------------------------------------------------------------------------}
procedure TImportSelectorForm.ButtonPanelResize(Sender: TObject);
begin
 if ButtonPanel.ClientWidth<OKButton.Width+CancelButton.Width+3*Round(8*ratio)then
  ButtonPanel.ClientWidth:=OKButton.Width+CancelButton.Width+3*Round(8*ratio);
 if ButtonPanel.ClientHeight<OKButton.Height+2*Round(8*ratio)then
  ButtonPanel.ClientHeight:=OKButton.Height+2*Round(8*ratio);
 OKButton.Left:=ButtonPanel.ClientWidth-OKButton.Width-Round(8*ratio);
 CancelButton.Left:=OKButton.Left-CancelButton.Width-Round(8*ratio);
end;

{------------------------------------------------------------------------------}
//Tile the form
{------------------------------------------------------------------------------}
procedure TImportSelectorForm.FormPaint(Sender: TObject);
begin
 MainForm.FileInfoPanelPaint(Sender);
end;

{------------------------------------------------------------------------------}
//Create the form
{------------------------------------------------------------------------------}
procedure TImportSelectorForm.FormCreate(Sender: TObject);
begin
 ratio:=PixelsPerInch/DesignTimePPI;
 //Create the buttons
 OKButton:=MainForm.CreateButton(ButtonPanel as TControl,'OK',True,0,0,mrOK);
 CancelButton:=MainForm.CreateButton(ButtonPanel as TControl,'Cancel',False,0,
                                     Round(4*ratio),mrCancel);
 //Set the default minimum sizes
 Constraints.MinHeight:=Round(456*ratio);
 Constraints.MinWidth :=Round(400*ratio);
end;

{------------------------------------------------------------------------------}
//Form is just being shown
{------------------------------------------------------------------------------}
procedure TImportSelectorForm.FormShow(Sender: TObject);
begin
 //Change the form size to its default
 Height:=Constraints.MinHeight;
 Width :=Constraints.MinWidth;
end;

{------------------------------------------------------------------------------}
//Toggle ticked/unticked
{------------------------------------------------------------------------------}
procedure TImportSelectorForm.ToggleTreeViewTickBoxes(Node: TTreeNode);
begin
 //Check that the node exists
 if Assigned(Node) then
 begin
  //If unticked or in an inderminate state, then set as ticked
  if(Node.StateIndex=ImgIndexUnTicked)or(Node.StateIndex=ImgIndexInter)then
   TickNode(Node,True)
  else //If ticked, then untick
  if Node.StateIndex=ImgIndexTicked then
   TickNode(Node,False);
 end;
end;

{------------------------------------------------------------------------------}
//Is the node ticked?
{------------------------------------------------------------------------------}
function TImportSelectorForm.IsNodeTicked(ANode:TTreeNode): Boolean;
begin
 Result:=False;
 If Assigned(ANode) then
  Result:=ANode.StateIndex<>ImgIndexUnTicked;
end;
function TImportSelectorForm.IsNodeTicked(dir,entry: Integer): Boolean;
var
 index: Integer;
 Node : TTreeNode;
begin
 //Counter into all the nodes
 index:=0;
 //Setup the Node to eventually query
 Node:=nil;
 //Go through the nodes until we either find it, or run out of nodes
 while(Node=nil)and(index<ImportDirList.Items.Count)do
 begin
  //Check each node
  if(TMyTreeNode(ImportDirList.Items[index]).ParentDir=dir)
  and(ImportDirList.Items[index].Index=entry)
  and(ImportDirList.Items[index].Level>0)then
   Node:=ImportDirList.Items[index];
  //Move onto the next
  inc(index);
 end;
 //Query this node for a result (a nil Node will return False)
 Result:=IsNodeTicked(Node);
end;

{------------------------------------------------------------------------------}
//User has clicked on a tickbox on one of the items
{------------------------------------------------------------------------------}
procedure TImportSelectorForm.TreeView1Click(Sender: TObject);
var
 P   : TPoint;
 Node: TTreeNode;
 ht  : THitTests;
begin
 //Set the TPoint to something, so the IDE doesn't whinge
 P.X:=0;
 //Get the position of the mouse
 GetCursorPos(P);
 //And convert it so it is local to the tree view
 P:=ImportDirList.ScreenToClient(P);
 //Where abouts has the mouse been clicked on?
 ht:=ImportDirList.GetHitTestInfoAt(P.X,P.Y);
 //State Icon? That's what we need.
 if(htOnStateIcon in ht)then
 begin
  //Get the node under the moust
  Node:=ImportDirList.GetNodeAt(P.X,P.Y);
  //Toggle the state
  ToggleTreeViewTickBoxes(Node);
 end;
 //It will leave it selected, so un-select
 ImportDirList.ClearSelection;
end;

end.
