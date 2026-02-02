unit HexDumpUnit;

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
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,Grids,ExtCtrls,Buttons,
 StdCtrls,ComCtrls,IpHtml,Global,StrUtils,SpriteFile,DiscImage,Types;

type

 { THexDumpForm }

 THexDumpForm = class(TForm)
  btnMoveDown: TSpeedButton;
  btnMoveDownLine: TSpeedButton;
  btnMoveToBottom: TSpeedButton;
  btnMoveToTop: TSpeedButton;
  btnMoveUp: TSpeedButton;
  btnMoveUpLine: TSpeedButton;
  btnSaveText: TSpeedButton;
  btnSaveBasic: TSpeedButton;
  ButtonImages: TImageList;
  edFontSize: TEdit;
  edJump: TEdit;
  edXOR: TEdit;
  HexDumpDisplay: TStringGrid;
  ImageDisplay: TImage;
  BasicOutput: TIpHtmlPanel;
  JumpToLabel: TLabel;
  BasicPanel: TPanel;
  lbFontSize: TLabel;
  TextOutput: TMemo;
  PageControl: TPageControl;
  HexDump: TTabSheet;
  BasicViewer: TTabSheet;
  ImagePanel: TPanel;
  SpritePanel: TPanel;
  SpriteOutput: TScrollBox;
  SpriteViewer: TTabSheet;
  ImageViewer: TTabSheet;
  TextViewer: TTabSheet;
  udFontSize: TUpDown;
  XORLabel: TLabel;
  NavImages: TImageList;
  ToolPanel: TPanel;
  ScrollPanel: TPanel;
  pbProgress: TProgressBar;
  SaveFile: TSaveDialog;
  ScrollBar: TScrollBar;
  procedure btnMoveDownClick(Sender: TObject);
  procedure btnMoveDownLineClick(Sender: TObject);
  procedure btnMoveToBottomClick(Sender: TObject);
  procedure btnMoveToTopClick(Sender: TObject);
  procedure btnMoveUpClick(Sender: TObject);
  procedure btnMoveUpLineClick(Sender: TObject);
  procedure btnSaveBasicClick(Sender: TObject);
  procedure edFontSizeChange(Sender: TObject);
  procedure edXORKeyPress(Sender: TObject; var Key: char);
  procedure FormCreate(Sender: TObject);
  procedure HexDumpDisplayMouseWheel(Sender: TObject; Shift: TShiftState;
   WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  procedure ImagePanelPaint(Sender: TObject);
  procedure PageControlChange(Sender: TObject);
  procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
   var ScrollPos: Integer);
  procedure btnSaveTextClick(Sender: TObject);
  procedure DisplayHex(start: Cardinal);
  procedure edJumpKeyPress(Sender: TObject; var Key: char);
  procedure FormResize(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure ResetApplication;
  procedure HexDumpDisplayGetCellHint(Sender: TObject; ACol, ARow: Integer;
   var HintText: String);
  procedure HexDumpDisplayKeyPress(Sender: TObject; var Key: char);
  procedure HexDumpDisplayPrepareCanvas(sender: TObject; aCol, aRow: Integer;
   aState: TGridDrawState);
  procedure HexDumpDisplaySelectCell(Sender: TObject; aCol, aRow: Integer;
   var CanSelect: Boolean);
  procedure HexDumpDisplaySetEditText(Sender: TObject; ACol, ARow: Integer;
   const Value: String);
  procedure HexDumpDisplayValidateEntry(sender: TObject; aCol, aRow: Integer;
   const OldValue: String; var NewValue: String);
  function IsBasicFile: Boolean;
  function IsTextFile: Boolean;
  procedure DecodeBasicFile;
  procedure DisplayImage;
  procedure DisplaySpriteFile;
  procedure SpriteOutputResize(Sender: TObject);
 private
  basiclength     : Cardinal;
  numsprites,
  spritew,
  spriteh,
  formwidth,
  formheight      : Integer;
  BasicTxtOutput  : TStringList;
 public
  buffer          : TDIByteArray;
 end;

var
 HexDumpForm: THexDumpForm;

implementation

{$R *.lfm}

uses MainUnit;

{ THexDumpForm }

{-------------------------------------------------------------------------------
 Procedure to run when the application first runs
-------------------------------------------------------------------------------}
procedure THexDumpForm.FormShow(Sender: TObject);
var
 c: Integer=0;
begin
 PageControlChange(Sender);
 //Set up the String Grid
 HexDumpDisplay.FixedCols:=1;
 HexDumpDisplay.FixedRows:=1;
 HexDumpDisplay.RowCount :=1;
 HexDumpDisplay.Font.Size:=Round(8*Screen.PixelsPerInch/DesignTimePPI);
 //Header
 HexDumpDisplay.ColWidths[0] :=HexDumpDisplay.Canvas.GetTextWidth('00000000000');
 HexDumpDisplay.Cells[0,0]   :='Address';
 HexDumpDisplay.ColWidths[17]:=HexDumpDisplay.Canvas.GetTextWidth('XXXXXXXXXXXXXXXXX');
 HexDumpDisplay.Cells[17,0]  :='ASCII';
 for c:=1 to 16 do
 begin
  HexDumpDisplay.ColWidths[c]:=HexDumpDisplay.Canvas.GetTextWidth('000');
  HexDumpDisplay.Cells[c,0]  :=IntToHex(c-1,2);
 end;
 formwidth:=HexDumpDisplay.ColWidths[0]
           +16*HexDumpDisplay.ColWidths[1]
           +HexDumpDisplay.ColWidths[17];
 HexDumpDisplay.Width:=formwidth;
 inc(formwidth,Round(40*Screen.PixelsPerInch/DesignTimePPI));
 ResetApplication;
 //Set up the form
 Constraints.MaxWidth:=formwidth;
 Constraints.MinWidth:=formwidth;
 Width:=formwidth;
 //Show the hex display
 DisplayHex(0);
 //Setup the scrollbar
 ScrollBar.Max:=Length(buffer);
 ScrollBar.Min:=0;
 ScrollBar.Position:=0;
 ScrollBar.Enabled:=True;
 //BASIC Font Size controls
 BasicOutput.DefaultFontSize:=Round(10*Screen.PixelsPerInch/DesignTimePPI);
 udFontSize.Position:=BasicOutput.DefaultFontSize;
 lbFontSize.Left:=btnSaveBasic.Left+btnSaveBasic.Width+4;
 edFontSize.Left:=lbFontSize.Left+lbFontSize.Width;
 edFontSize.Top:=(BasicPanel.Height-edFontSize.Height)div 2;
 lbFontSize.Top:=(BasicPanel.Height-lbFontSize.Height)div 2;
end;

{-------------------------------------------------------------------------------
 Reset the application
-------------------------------------------------------------------------------}
procedure THexDumpForm.ResetApplication;
begin
 //Disable the controls
 btnMoveUp.Enabled      :=True;
 btnMoveDown.Enabled    :=True;
 btnMoveUpLine.Enabled  :=True;
 btnMoveDownLine.Enabled:=True;
 btnMoveToTop.Enabled   :=True;
 btnMoveToBottom.Enabled:=True;
 edJump.Enabled         :=True;
 edXOR.Enabled          :=True;
 btnSaveText.Enabled    :=True;
 ScrollBar.Enabled      :=True;
 //Empty the grid
 HexDumpDisplay.RowCount:=1;
end;

{-------------------------------------------------------------------------------
 Procedure to run when the user presses a key while in the XOR key box
-------------------------------------------------------------------------------}
procedure THexDumpForm.edXORKeyPress(Sender: TObject; var Key: char);
begin
 //13 = CR...i.e. Enter key
 if Ord(Key)=13 then //Action the result
 begin
  //Validate the entry
  edXOR.Text:=IntToHex(StrToIntDef('$'+edXOR.Text,0),2);
  //Remove focus from the control
  HexDumpDisplay.SetFocus;
  //Refresh the grid
  DisplayHex(StrToIntDef('$'+HexDumpDisplay.Cells[0,1],0));
 end
 else //Ensure it is a number or A to F (i.e. Hex), or Delete/Backspace
  if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
  and (Key<>chr(127)) AND (Key<>chr(8)) then Key:=#0; //If not, invalidate
end;

{-------------------------------------------------------------------------------
 Tile the various components
-------------------------------------------------------------------------------}
procedure THexDumpForm.FormCreate(Sender: TObject);
begin
 formwidth:=Width;
 formheight:=Height;
 BasicTxtOutput:=TStringList.Create;
end;

{-------------------------------------------------------------------------------
 User is using the scroll wheel
-------------------------------------------------------------------------------}
procedure THexDumpForm.HexDumpDisplayMouseWheel(Sender: TObject;
 Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean
 );
var
 Lscroll : Integer=0;
 i       : Integer=0;
begin
 //Work out by how much
 Lscroll:=WheelDelta div 240;
 //If it is <0 then it is down, otherwise up - we just need the amount
 if Lscroll<0 then Lscroll:=-Lscroll;
 //If we are scrolling anything
 if lscroll>0 then
  //Scroll line by line
  for i:=0 to Lscroll-1 do
  begin
   if WheelDelta<0 then btnMoveDownLineClick(Sender); //Down
   if WheelDelta>0 then btnMoveUpLineClick(Sender);   //Up
  end;
 //Tell the OS not to do anything, as we have handled it ourselves
 Handled:=True;
end;

{-------------------------------------------------------------------------------
 Tile the various components
-------------------------------------------------------------------------------}
procedure THexDumpForm.ImagePanelPaint(Sender: TObject);
begin
 if Sender is TPanel then
  MainForm.TileCanvas(TPanel(Sender).Canvas); //for a TPanel
end;

{-------------------------------------------------------------------------------
 The active page is changing, so allow a resize
-------------------------------------------------------------------------------}
procedure THexDumpForm.PageControlChange(Sender: TObject);
begin
 //If swtiching to Hex Dump or Sprite Viewer, restrict the size
 if(PageControl.ActivePage=HexDump)
 or(PageControl.ActivePage=SpriteViewer)then
 begin
  Constraints.MaxWidth:=formwidth;
  Constraints.MinWidth:=formwidth;
  Constraints.MinHeight:=Round(290*Screen.PixelsPerInch/DesignTimePPI);
  //Change the size, if not already changed
  Width:=Round(635*Screen.PixelsPerInch/DesignTimePPI);
  if Height<Round(290*Screen.PixelsPerInch/DesignTimePPI) then
   Height:=Round(290*Screen.PixelsPerInch/DesignTimePPI);
 end
 else
 begin
  //Unrestrict the size
  Constraints.MaxWidth:=0;
  Constraints.MinWidth:=0;
  Constraints.MinHeight:=0;
  //And restore the former size
  Width:=formwidth;
  Height:=formheight;
 end;
end;

{-------------------------------------------------------------------------------
 User has pressed a key on the Jump edit box
-------------------------------------------------------------------------------}
procedure THexDumpForm.edJumpKeyPress(Sender: TObject; var Key: char);
begin
 //13 = CR...i.e. Enter key
 if Ord(Key)=13 then //Action the result
 begin
  //Update the grid with a valid entry
  DisplayHex(StrToIntDef('$'+edJump.Text,0));
  //Update the edit box with a valid entry
  edJump.Text:=IntToHex(StrToIntDef('$'+edJump.Text,0),10);
  //Remove focus
  HexDumpDisplay.SetFocus;
 end
 else //Ensure it is a number or A to F (i.e. Hex), or Delete/Backspace
  if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
  and (Key<>chr(127)) AND (Key<>chr(8)) then Key:=#0; //If not, invalidate
end;

{-------------------------------------------------------------------------------
 Procedure to run when the scroll bar moves
-------------------------------------------------------------------------------}
procedure THexDumpForm.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
 var ScrollPos: Integer);
begin
 //Just move the display on to the position dictated by the scroll bar
 DisplayHex((ScrollPos div $10)*$10);
end;

{-------------------------------------------------------------------------------
 User has clicked on the Save As Text File button
-------------------------------------------------------------------------------}
procedure THexDumpForm.btnSaveTextClick(Sender: TObject);
var
 line  : String='';
 F     : TFileStream=nil;
 p     : Byte=0;
 len   : Byte=0;
 i     : Integer=0;
 pos   : Integer=0;
begin
 //Adapt the filename
 line:=Caption;
 BBCToWin(line);
 //Remove any dots
 for i:=1 to Length(line) do if line[i]='.' then line[i]:='-';
 SaveFile.Filename:=line+'-dump.txt';
 //And open the dialogue box
 if SaveFile.Execute then
 begin
  //Show the progress bar
  pbProgress.Visible:=True;
  pbProgress.Position:=0;
  //Create a new file (overwrite one if already exists)
  F:=TFileStream.Create(SaveFile.Filename,fmCreate);
  //Set to start of file
  F.Position:=0;
  //Write out the header
  WriteLine(F,MainForm.ApplicationTitle+' V'+MainForm.ApplicationVersion);
  WriteLine(F,'https://www.geraldholdsworth.co.uk https://github.com/geraldholdsworth/DiscImageManager');
  WriteLine(F,'');
  WriteLine(F,'Filename      : '+Caption);
  WriteLine(F,'Total Filesize: '+IntToStr(Length(buffer))
                   +' (0x'+IntToHex(Length(buffer),10)+') bytes');
  WriteLine(F,'');
  line:='Address     00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F  ASCII';
  WriteLine(F,line);
  //Now the data
  pos:=0;//Start of the data
  repeat
   //Start the line off with the address, in hex, 10 digits long
   line:=IntToHex((pos div $10)*$10,10)+'  ';
   //Set the amount of data to read to 16 bytes
   len:=$10;
   //If this will take us over the total size, then adjust accordingly
   if pos+len>Length(buffer) then
    len:=Length(buffer)-pos;
   //If there is something to read, then read it
   if len>0 then
   begin
    //Turn each byte into hex and output
    for p:=0 to len-1 do
    begin
     line:=line+IntToHex(buffer[p+pos],2)+' ';
     if p=$07 then line:=line+' '; //Split in the middle
    end;
    //Extra space to separate from the characters
    line:=PadRight(line,62);
    //Now the characters
    for p:=0 to len-1 do
     if (buffer[p+pos]>31) AND (buffer[p+pos]<127) then
      line:=line+chr(buffer[p+pos]) //Printable
     else
      line:=line+'.'; //Not printable
    //Write out the complete line
    WriteLine(F,line);
   end;
   //Update the progress bar
   pbProgress.Position:=Round((pos/Length(buffer))*100);
   Application.ProcessMessages;
   //Continue until no more data
   inc(pos,len);
  until pos=Length(buffer);
  //Close the file and exit
  F.Free;
  //Hide the progress bar
  pbProgress.Visible:=False;
 end;
end;

{-------------------------------------------------------------------------------
 User has clicked on the Save As Text File button on the BASIC viewer tab
-------------------------------------------------------------------------------}
procedure THexDumpForm.btnSaveBasicClick(Sender: TObject);
var
 line  : String='';
 i     : Integer=0;
begin
 //Adapt the filename
 line:=Caption;
 BBCToWin(line);
 //Remove any dots
 for i:=1 to Length(line) do if line[i]='.' then line[i]:='-';
 SaveFile.Filename:=line+'-basic.txt';
 //And open the dialogue box to save the file
 if SaveFile.Execute then BasicTxtOutput.SaveToFile(SaveFile.Filename);
end;

{-------------------------------------------------------------------------------
 Populates the grid, starting at address 'start'
-------------------------------------------------------------------------------}
procedure THexDumpForm.DisplayHex(start: Cardinal);
var
 rows : Cardinal=0;
 line : Cardinal=0;
 ch   : Byte=0;
 len  : Byte=0;
 key  : Byte=0;
 chars: String='';
begin
 //How many rows are visible on the form?
 rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight)-1;
 if rows=0 then exit; //None, then leave
 //Get the XOR key
 key:=StrToIntDef('$'+edXOR.Text,0);
 //Will this take us beyond the end of the file?
 if start+(rows*$10)>Length(buffer) then
 begin
  //Then adjust accordingly
  if Length(buffer)-(rows*$10)>=0 then
  begin
   start:=Length(buffer)-(rows*$10);
   //start will be reset to a $10 boundary, so need to move it on if there is more
   if start mod $10>0 then inc(start,$10);
  end
  else
  begin //If the entire file is smaller than the space available, adjust
   start:=0;
   rows:=Length(buffer) div $10;
   if Length(buffer) mod $10>0 then inc(rows);
  end;
 end;
 //Ensure the start address is on a 16-byte boundary
 start:=(start div $10)*$10;
 //And the scroll bar
 ScrollBar.Position:=start;
 //Make sure there are the appropriate number of rows
 HexDumpDisplay.RowCount:=rows+1; //+1 is the header
 //Start at line 0
 line:=0;
 repeat
  //We will be reading in 16 bytes at a time
  if start+$10<Length(buffer) then len:=$10
  else len:=Length(buffer)-start; //Unless we run out of bytes
  //Clear the character column string
  chars:='';
  //Display the address in the first column - to 10 digits
  if line+1<HexDumpDisplay.RowCount then
   HexDumpDisplay.Cells[0,line+1]:=IntToHex((start div $10)*$10,10);
  //Go through the data just read in
  for ch:=0 to len-1 do
  begin
   //Display each one as hex - colours are dealt with elsewhere
   if(ch+1<HexDumpDisplay.ColCount)and(line+1<HexDumpDisplay.RowCount)then
    HexDumpDisplay.Cells[ch+1,line+1]:=IntToHex(buffer[start+ch]XOR key,2);
   //Add add to the character column
   if(buffer[start+ch]XOR key>31)AND(buffer[start+ch]XOR key<127)then
    chars:=chars+Chr(buffer[start+ch]XOR key) //Printable
   else
    chars:=chars+'.';                   //Not printable
  end;
  //Are there more cells than data?
  if len<$10 then
   for ch:=len to $0F do
    if(ch+1<HexDumpDisplay.ColCount)and(line+1<HexDumpDisplay.RowCount)then
     HexDumpDisplay.Cells[ch+1,line+1]:=' ';
  //Display the characters in the final coluumn
  if line+1<HexDumpDisplay.RowCount then
   HexDumpDisplay.Cells[17,line+1]:=chars;
  //And move onto the next line
  inc(line);
  inc(start,len);
 until (start-$10>=Length(buffer)-1) //Continue until the end of the file
 or (line+1=HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight); //Or form
end;

{-------------------------------------------------------------------------------
 Navigation buttons - move up a page
-------------------------------------------------------------------------------}
procedure THexDumpForm.btnMoveUpClick(Sender: TObject);
var
 s   : Cardinal=0;
 rows: Cardinal=0;
begin
 //Get the current top position
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 //And the number of rows available
 rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight)-2;
 //Move towards the beginning of the file (or start if close enough)
 if s-rows<0 then s:=0 else dec(s,rows);
 //Update the display
 DisplayHex(s*$10);
end;

{-------------------------------------------------------------------------------
 Navigation buttons - move up a line
-------------------------------------------------------------------------------}
procedure THexDumpForm.btnMoveUpLineClick(Sender: TObject);
var
 s: Cardinal=0;
begin
 //Get the current top position
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 //Can we move up?
 if s>0 then dec(s);
 //Update the display
 DisplayHex(s*$10);
end;

{-------------------------------------------------------------------------------
 Navigation buttons - move down a page
-------------------------------------------------------------------------------}
procedure THexDumpForm.btnMoveDownClick(Sender: TObject);
var
 s   : Cardinal=0;
 rows: Cardinal=0;
begin
 //Get the current top position
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 //And the number of rows available
 rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight)-2;
 //And we'll move down towards the end of the file
 inc(s,rows);
 //Update the display
 DisplayHex(s*$10);
end;

{-------------------------------------------------------------------------------
 Navigation buttons - move down a line
-------------------------------------------------------------------------------}
procedure THexDumpForm.btnMoveDownLineClick(Sender: TObject);
var
 s: Cardinal=0;
begin
 //Get the current top position
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 //Move down one line
 inc(s);
 //Update the display
 DisplayHex(s*$10);
end;

{-------------------------------------------------------------------------------
 Navigation buttons - move to the end of the file
-------------------------------------------------------------------------------}
procedure THexDumpForm.btnMoveToBottomClick(Sender: TObject);
begin
 //Just update the display with the filesize
 DisplayHex(Length(buffer));
end;

{-------------------------------------------------------------------------------
 Navigation buttons - move to the beginning of the file
-------------------------------------------------------------------------------}
procedure THexDumpForm.btnMoveToTopClick(Sender: TObject);
begin
 //Just update the display with zero
 DisplayHex(0);
end;

{-------------------------------------------------------------------------------
 User is resizing the form
-------------------------------------------------------------------------------}
procedure THexDumpForm.FormResize(Sender: TObject);
var
 s   : Cardinal=0;
 rows: Cardinal=0;
begin
 //Have we got any rows displayed?
 if HexDumpDisplay.RowCount>1 then
 begin
  //Then find out how many we can now display
  rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight);
  //Has it changed?
  if HexDumpDisplay.RowCount<>rows then
  begin
   //Then update the grid with the new number of rows
   s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1]);
   DisplayHex(s);
  end;
 end;
 //Remember the size
 if (PageControl.ActivePage<>HexDump)
 and(PageControl.ActivePage<>SpriteViewer)then
  formwidth:=Width;
 formheight:=Height;
end;

{-------------------------------------------------------------------------------
 This doesn't actually appear to do anything
-------------------------------------------------------------------------------}
procedure THexDumpForm.HexDumpDisplayGetCellHint(Sender: TObject; ACol,
 ARow: Integer; var HintText: String);
var
 s: Byte=0;
begin
 s:=StrToIntDef('$'+HexDumpDisplay.Cells[aCol,aRow],0);
 if (s>31) and (s<127) then HintText:=chr(s)
 else HintText:='';
end;

{-------------------------------------------------------------------------------
 User has pressed a key while editing a value on the grid
-------------------------------------------------------------------------------}
procedure THexDumpForm.HexDumpDisplayKeyPress(Sender: TObject; var Key: char);
begin
 //Verify it is in hex, delete, backspace or Enter
 if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
 and (Key<>chr(127)) AND (Key<>chr(8)) AND (Key<>chr(13)) then Key:=#0;
end;

{-------------------------------------------------------------------------------
 This recolours the individual cells
-------------------------------------------------------------------------------}
procedure THexDumpForm.HexDumpDisplayPrepareCanvas(sender: TObject; aCol,
 aRow: Integer; aState: TGridDrawState);
var
 s: Byte=0;
begin
 //Default font colour is black, if everything else fails
 HexDumpDisplay.Font.Color:=$000000;
 //We're only colouring below the header row
 if aRow>0 then
  //And the hex cells
  if (aCol>0) and (aCol<17) then
  begin
   //Get the value in the cell
   s:=StrToIntDef('$'+HexDumpDisplay.Cells[aCol,aRow],0);
   //Default font colour is Blue
   HexDumpDisplay.Font.Color:=$FF0000;
   //If it is zero, then it is Red
   if s=0 then HexDumpDisplay.Font.Color:=$0000FF;
   //If it is printable, then it is Green (darkish shade)
   if (s>31) and (s<127) then HexDumpDisplay.Font.Color:=$00AA00;
   //No styles are being applied
   HexDumpDisplay.Font.Style:=[];
  end
  else
  begin
   //Colour the character column to match the header and address
   HexDumpDisplay.Font.Style:=[fsBold];
   HexDumpDisplay.Canvas.Brush.Color:=HexDumpDisplay.FixedColor;
  end;
end;

{-------------------------------------------------------------------------------
 User is trying to edit a cell
-------------------------------------------------------------------------------}
procedure THexDumpForm.HexDumpDisplaySelectCell(Sender: TObject; aCol,
 aRow: Integer; var CanSelect: Boolean);
begin
 //We can only edit cells 1 to 16, and not the character column
 if aCol=17 then CanSelect:=False
 else CanSelect:=True;
end;

{-------------------------------------------------------------------------------
 This fires everytime a change is made
-------------------------------------------------------------------------------}
procedure THexDumpForm.HexDumpDisplaySetEditText(Sender: TObject; ACol,
 ARow: Integer; const Value: String);
begin
 //We need to make sure that we don't have more than 2 character length
 If Length(value)>2 then
  HexDumpDisplay.Cells[ACol,ARow]:=LeftStr(value,2);
end;

{-------------------------------------------------------------------------------
 User has finished editing the cell, so now validate
-------------------------------------------------------------------------------}
procedure THexDumpForm.HexDumpDisplayValidateEntry(sender: TObject; aCol,
 aRow: Integer; const OldValue: String; var NewValue: String);
var
 p: Cardinal=0;
 c: String='';
begin
 //We can only edit cells 1 to 16
 if aCol<17 then
 begin
  //Ensure it is a valid hex number, not more that 2 digits
  NewValue:=IntToHex(StrToIntDef('$'+NewValue,0),2);
  //Get the character display text
  c:=HexDumpDisplay.Cells[17,aRow];
  //And update to the new value
  if (StrToInt('$'+HexDumpDisplay.Cells[aCol,aRow])>31)
  and(StrToInt('$'+HexDumpDisplay.Cells[aCol,aRow])<127) then
   c[aCol]:=chr(StrToInt('$'+HexDumpDisplay.Cells[aCol,aRow])) //Printable
  else
   c[aCol]:='.';                                               //Not printable
  //And update the cell
  HexDumpDisplay.Cells[17,aRow]:=c;
  //Work out the position within the file
  p:=StrToIntDef('$'+HexDumpDisplay.Cells[0,aRow],0)+(aCol-1);
  //Move to it
  buffer[p]:=StrToIntDef('$'+NewValue,0);
 end
 else //Otherwise, change back to what it was before
  NewValue:=OldValue;
end;

{-------------------------------------------------------------------------------
 Analysis a file to see if it is a BASIC file or not
-------------------------------------------------------------------------------}
function THexDumpForm.IsBasicFile: Boolean;
var
 ptr: Integer=0;
begin
 //It should start with 0x0D, then two bytes later should have a pointer to the
 //next 0x0D, all the way to the end of the file.
 Result:=False;
 if buffer[0]=$0D then
 begin
  Result:=True;
  ptr:=0;
  // $0D is followed by two byte line number, then the line length
  while(ptr+3<basiclength)and(Result)do
  begin
   // $FF marks the end of file, which doesn't always happen at the end
   if(buffer[ptr+1]=$FF)and(buffer[ptr+3]<5) then
    basiclength:=ptr+1 //So we truncate the file
   else
   begin
    //Move onto the next pointer
    inc(ptr,buffer[ptr+3]);
    if ptr<Length(buffer) then
     if buffer[ptr]<>$0D then
      Result:=False;
   end;
  end;
 end;
end;

{-------------------------------------------------------------------------------
 Analysis a file to see if it is a Text file or not
-------------------------------------------------------------------------------}
function THexDumpForm.IsTextFile: Boolean;
var
 ptr: Integer=0;
begin
 //We will just see if all the characters are between 32 and 126. Can also
 //permit 10 (LF), 13 (CR) and 9 (HT).
 Result:=True;
 for ptr:=0 to Length(buffer)-1 do
  if((buffer[ptr]<32)and(buffer[ptr]<>10)and(buffer[ptr]<>13)and(buffer[ptr]<>9))
  or(buffer[ptr]>126)then Result:=False;
end;

{-------------------------------------------------------------------------------
 Decodes a BBC BASIC file, or displays as text
-------------------------------------------------------------------------------}
procedure THexDumpForm.DecodeBasicFile;
var
 ptr     : Integer=0;
 linenum : Integer=0;
 linelen : Byte=0;
 lineptr : Byte=0;
 c       : Byte=0;
 cn      : Byte=0;
 t       : Byte=0;
 basicver: Byte=0;
 tmp     : String='';
 basictxt: String='';
 linetxt : String='';
 detok   : Boolean=False;
 rem     : Boolean=False;
 isbasic : Boolean=False;
 fs      : TStringStream=nil;
const
 // $80 onwards, single token per keyword
 tokens: array[0..127] of String = (
  'AND'   ,'DIV'    ,'EOR'     ,'MOD'    ,'OR'       ,'ERROR' ,'LINE'    ,'OFF',
  'STEP'  ,'SPC'    ,'TAB('    ,'ELSE'   ,'THEN'     ,'line'  ,'OPENIN'  ,'PTR',
  'PAGE'  ,'TIME'   ,'LOMEM'   ,'HIMEM'  ,'ABS'      ,'ACS'   ,'ADVAL'   ,'ASC',
  'ASN'   ,'ATN'    ,'BGET'    ,'COS'    ,'COUNT'    ,'DEG'   ,'ERL'     ,'ERR',
  'EVAL'  ,'EXP'    ,'EXT'     ,'FALSE'  ,'FN'       ,'GET'   ,'INKEY'   ,'INSTR(',
  'INT'   ,'LEN'    ,'LN'      ,'LOG'    ,'NOT'      ,'OPENUP','OPENOUT' ,'PI',
  'POINT(','POS'    ,'RAD'     ,'RND'    ,'SGN'      ,'SIN'   ,'SQR'     ,'TAN',
  'TO'    ,'TRUE'   ,'USR'     ,'VAL'    ,'VPOS'     ,'CHR$'  ,'GET$'    ,'INKEY$',
  'LEFT$(','MID$('  ,'RIGHT$(' ,'STR$'   ,'STRING$(' ,'EOF'   ,'SUM'     ,'WHILE',
  'CASE'  ,'WHEN'   ,'OF'      ,'ENDCASE','OTHERWISE','ENDIF' ,'ENDWHILE','PTR',
  'PAGE'  ,'TIME'   ,'LOMEM'   ,'HIMEM'  ,'SOUND'    ,'BPUT'  ,'CALL'    ,'CHAIN',
  'CLEAR' ,'CLOSE'  ,'CLG'     ,'CLS'    ,'DATA'     ,'DEF'   ,'DIM'     ,'DRAW',
  'END'   ,'ENDPROC','ENVELOPE','FOR'    ,'GOSUB'    ,'GOTO'  ,'GCOL'    ,'IF',
  'INPUT' ,'LET'    ,'LOCAL'   ,'MODE'   ,'MOVE'     ,'NEXT'  ,'ON'      ,'VDU',
  'PLOT'  ,'PRINT'  ,'PROC'    ,'READ'   ,'REM'      ,'REPEAT','REPORT'  ,'RESTORE',
  'RETURN','RUN'    ,'STOP'    ,'COLOUR' ,'TRACE'    ,'UNTIL' ,'WIDTH'   ,'OSCLI');
 //Extended tokens, $C6 then $8E onwards
 exttokens1: array[0..1] of String = ('SUM', 'BEAT');
 //Extended tokens, $C7 then $8E onwards
 exttokens2: array[0..17] of String = (
  'APPEND','AUTO'    ,'CRUNCH'  ,'DELET','EDIT' ,'HELP',
  'LIST'  ,'LOAD'    ,'LVAR'    ,'NEW'  ,'OLD'  ,'RENUMBER',
  'SAVE'  ,'TEXTLOAD','TEXTSAVE','TWIN' ,'TWINO','INSTALL');
 //Extended tokens, $C8 then $8E onwards
 exttokens3: array[0..21] of String = (
  'CASE' ,'CIRCLE','FILL'  ,'ORIGIN','PSET'   ,'RECT'   ,'SWAP','WHILE',
  'WAIT' ,'MOUSE' ,'QUIT'  ,'SYS'   ,'INSTALL','LIBRARY','TINT','ELLIPSE',
  'BEATS','TEMPO' ,'VOICES','VOICE' ,'STEREO' ,'OVERLAY');
 keywordstyle = 'style="color:#FFFF00;font-weight: bold"';
 linenumstyle = 'style="color:#00FF00"';
 quotestyle   = 'style="color:#00FFFF;font-style: italic"';
begin
 basiclength:=Length(buffer);
 //First we'll analyse the data to see if it is a BBC BASIC file
 isbasic:=IsBasicFile;
 //Our pointer into the file
 ptr:=0;
 //Is it a BBC BASIC file?
 if isbasic then
 begin
  //Clear the output container and write the headers
  BasicTxtOutput.Clear;
  fs:=TStringStream.Create('<html><head><title>Basic Listing</title></head>');
  fs.WriteString('<body style="background-color:#0000FF;color:#FFFFFF">');
  //BBC BASIC version
  basicver:=1;
  //Continue until the end of the file
  while ptr+3<basiclength do
  begin
   //Read in the line
   if buffer[ptr]=$0D then
   begin
    //Line number
    linenum:=buffer[ptr+2]+buffer[ptr+1]<<8;
    linetxt:='<span '+linenumstyle+'>'
            +StringReplace(PadLeft(IntToStr(linenum),5),' ','&nbsp;',[rfReplaceAll])
            +'</span>&nbsp;';
    basictxt:=PadLeft(IntToStr(linenum),5);
    //Line length
    linelen:=buffer[ptr+3];
    //Move our line pointer one
    lineptr:=4;
    //Whether to detokenise or not (i.e. within quotes or not)
    detok:=True;
    //Has a REM been issued?
    rem:=False;
    //While we are within bounds
    while lineptr<linelen do
    begin
     //Get the next character
     c:=buffer[ptr+lineptr];
     //And move on
     inc(lineptr);
     //Is it a token?
     if(c>$7F)and(detok)then
     begin
      //Is token a REM?
      if c=$F4 then
      begin
       detok:=False;
       rem:=True;
      end;
      //Set the BASIC version
      if(c=$AD)or(c=$FF)then basicver:=2;
      if(c=$CA)or(c=$CB)or(c=$CD)or(c=$CE)then basicver:=5;
      tmp:='';
      //Normal token (BASIC I,II,III and IV)
      if(c<$C6)or(c>$C8)then
      begin
       if c-$80<=High(tokens) then
        if c-$80<>$D then
         tmp:=tokens[c-$80]
        else
        begin //Line number
         tmp:=IntToStr(((buffer[ptr+lineptr  ]XOR$54)AND$30)<< 2
                     OR((buffer[ptr+lineptr  ]XOR$54)AND$03)<<14
                     OR (buffer[ptr+lineptr+1]       AND$3F)
                     OR (buffer[ptr+lineptr+2]       AND$3F)<< 8);
         inc(lineptr,3);
        end;
       linetxt:=linetxt+'<span '+keywordstyle+'>'+tmp+'</span>';
       basictxt:=basictxt+tmp;
      end
      else //Extended tokens (BASIC V)
      begin
       basicver:=5;
       //Extended token number
       t:=buffer[ptr+lineptr];
       //Move on
       inc(lineptr);
       //Decode the token
       if t>$8D then
       begin
        if c=$C6 then
         if t-$8E<=High(exttokens1)then tmp:=exttokens1[t-$8E];
        if c=$C7 then
         if t-$8E<=High(exttokens2)then tmp:=exttokens2[t-$8E];
        if c=$C8 then
         if t-$8E<=High(exttokens3)then tmp:=exttokens3[t-$8E];
        linetxt:=linetxt+'<span '+keywordstyle+'>'+tmp+'</span>';
        basictxt:=basictxt+tmp;
       end;
      end;
      //Reset c
      c:=0;
     end;
     //We can get control characters in BBC BASIC, but macOS can't deal with them
     if c>31 then
     begin
      if not rem then if(c=34)AND(detok)then
       linetxt:=linetxt+'<span '+quotestyle+'>';
      if(c<>32)and(c<>38)and(c<>60)and(c<>62)then
       linetxt:=linetxt+Chr(c AND$7F);
      if c=32 then linetxt:=linetxt+'&nbsp;';
      if c=38 then linetxt:=linetxt+'&amp;';
      if c=60 then linetxt:=linetxt+'&lt;';
      if c=62 then linetxt:=linetxt+'&gt;';
      if not rem then if(c=34)and(not detok)then linetxt:=linetxt+'</span>';
      basictxt:=basictxt+Chr(c AND$7F);
      //Do not detokenise within quotes
      if(c=34)and(not rem)then detok:=not detok;
     end;
    end;
    //Add the complete line to the output container
    fs.WriteString(linetxt+'<br>');
    BasicTxtOutput.Add(basictxt);
    //And move onto the next line
    inc(ptr,linelen);
   end;
  end;
  //Display the minimum compatible BASIC version
  linetxt:='';
  case basicver of
   1: linetxt:=' I';
   2: linetxt:=' II';
   3: linetxt:=' III';
   4: linetxt:=' IV';
   5: linetxt:=' V';
  end;
  BasicViewer.Caption:='BBC BASIC'+linetxt;
  //Change the colour
  BasicOutput.Color:=$FF0000;
  BasicOutput.Font.Color:=$FFFFFF;
  //Finish off the HTML
  fs.WriteString('</body></html>');
  //Now upload the document to the display
  fs.Position:=0;
  BasicOutput.SetHtmlFromStream(fs);
  fs.Free;
  //Make the tab visible
  BasicViewer.TabVisible:=True;
  //And switch to it
  PageControl.ActivePage:=BasicViewer;
  PageControlChange(nil);
 end
 else //Display as text file, if it is a text file
 if IsTextFile then
 begin
  //Clear the container
  TextOutput.Clear;
  linetxt:='';
  while ptr<Length(buffer) do
  begin
   //Read the character in
   c:=buffer[ptr];
   //Move on
   inc(ptr);
   //Read the next character, if not at the end
   if ptr<Length(buffer) then cn:=buffer[ptr+1] else cn:=0;
   //Can't deal with control characters on macOS
   if(c>31)and(c<127)then linetxt:=linetxt+chr(c);
   //New line
   if((c=$0A)and(cn<>$0D))
   or((c=$0D)and(cn<>$0A))then
   begin
    TextOutput.Lines.Add(linetxt);
    linetxt:='';
   end;
  end;
  //At the end, anything left then push to the output container
  if linetxt<>'' then
   TextOutput.Lines.Add(linetxt);
  //Move the cursor to the beginning
  TextOutput.SelStart:=0;
  TextOutput.SelLength:=0;
  //Make the tab visible
  TextViewer.TabVisible:=True;
  //And switch to it
  PageControl.ActivePage:=TextViewer;
  PageControlChange(nil);
 end;
end;

{-------------------------------------------------------------------------------
 Displays a compatible image
-------------------------------------------------------------------------------}
procedure THexDumpForm.DisplayImage;
var
 size : Integer=0;
 j    : Integer=0;
 png  : Boolean=False;
 bmp  : Boolean=False;
 jpg  : Boolean=False;
 ms   : TMemoryStream=nil;
 const
  pngsig: array[0..$F] of Byte=($89,$50,$4E,$47
                               ,$0D,$0A,$1A,$0A
                               ,$00,$00,$00,$0D
                               ,$49,$48,$44,$52);
begin
 //We will first make sure it is an image we can deal with
 //We need to know the size of the file
 size:=Length(buffer);
 //Bitmaps:
 //The first two bytes should be 'BM', and the next four should be the filesize
 //which will match what we got before
 bmp:=(buffer[0]=ord('B')) and (buffer[1]=ord('M'))
  and (buffer[2]+buffer[3]*$100+buffer[4]*$10000+buffer[5]*$1000000=size);
 //PNG:
 //First sixteen bytes will be: 89 50 4E 47 0D 0A 1A 0A 00 00 00 0D 49 48 44 52
 png:=True;
 for j:=0 to 15 do
  if buffer[j]<>pngsig[j] then png:=False;
 //JPEG:
 //Starts FF 80
 jpg:=(buffer[0]=$FF)and(buffer[1]=$D8);
 //If valid image, load into the image display
 if(png)or(bmp)or(jpg)then
 begin
  //Load the contents of the buffer into the image
  ms:=TMemoryStream.Create;
  ms.Write(buffer[0],Length(buffer));
  ms.Position:=0;
  if png then ImageDisplay.Picture.PNG.LoadFromStream(ms);
  if bmp then ImageDisplay.Picture.Bitmap.LoadFromStream(ms);
  if jpg then ImageDisplay.Picture.JPEG.LoadFromStream(ms);
  //Squash it if bigger than available space
  if(ImageDisplay.Width>ImageViewer.ClientWidth)
  or(ImageDisplay.Height>ImageViewer.ClientHeight)then
  begin
   ImageDisplay.Stretch:=True;
   ImageDisplay.Proportional:=True;
  end;
  //Finish up
  ms.Free;
  //And make the tab visible
  ImageViewer.TabVisible:=True;
  //And switch to it
  PageControl.ActivePage:=ImageViewer;
  PageControlChange(nil);
 end;
end;

{-------------------------------------------------------------------------------
 Displays a sprite file
-------------------------------------------------------------------------------}
procedure THexDumpForm.DisplaySpriteFile;
var
 ms    : TMemoryStream=nil;
 sp    : TSpriteFile=nil;
 sprite: TSprite;
 e     : Byte=0;
 img   : array of TImage=nil;
 lbl   : array of TLabel=nil;
 sn    : Integer=0;
 x     : Integer=0;
 y     : Integer=0;
 size  : Integer=0;
begin
 //Width of each sprite, including name
 spritew:=Round(70*Screen.PixelsPerInch/DesignTimePPI);
 //Height of each sprite, including name
 spriteh:=Round(64*Screen.PixelsPerInch/DesignTimePPI);
 //Actual width and height of the sprite image
 size:=Round(32*Screen.PixelsPerInch/DesignTimePPI);
 //Create the memory stream which we will use to transfer the file
 ms:=TMemoryStream.Create;
 //Transfer the data from the array to the stream
 ms.Write(buffer[0],Length(buffer));
 ms.Position:=0;
 //Create the sprite file
 sp:=TSpriteFile.Create;
 //And transfer the data from the array to the sprite file to read it
 e:=sp.LoadSpriteFileFromStream(ms);
 //If it is a valid sprite file, and there are some sprites, then show them
 if(e=0)and(sp.SpriteCount>0)then
 begin
  numsprites:=sp.SpriteCount;
  //Containers for the images and labels
  SetLength(img,sp.SpriteCount);
  SetLength(lbl,sp.SpriteCount);
  //Set and position the panel height
  SpriteOutputResize(nil);
  //Iterate through each one
  for sn:=0 to sp.SpriteCount-1 do
  begin
   //Calculate the top and left of each container
   x:=(((sn*spritew)mod(SpritePanel.ClientWidth-spritew))div spritew)*spritew;
   y:=((sn*spritew)div(SpritePanel.ClientWidth-spritew))*spriteh;
   //Create the image display
   img[sn]:=TImage.Create(SpritePanel);
   img[sn].Parent:=SpritePanel;
   //Position it
   img[sn].Left:=4+x+((spritew-size)div 2);
   img[sn].Top :=4+y;
   //Re-size it
   img[sn].Width:=size;
   img[sn].Height:=size;
   //Squash/stretch it
   img[sn].Stretch:=True;
   img[sn].Proportional:=True;
   img[sn].Center:=True;
   //Read the sprite in
   sprite:=sp.ReadSprite(sn);
   //And display it
   img[sn].Picture.PNG.Assign(sprite.PNG);
   //Create the label for the name
   lbl[sn]:=TLabel.Create(SpritePanel);
   lbl[sn].Parent:=SpritePanel;
   //Ensure it doesn't re-size itself
   lbl[sn].AutoSize:=False;
   //Centre the text
   lbl[sn].Alignment:=taCenter;
   //Write the text
   lbl[sn].Caption:=sprite.Name;
   //And make it max width (so it gets centred)
   lbl[sn].Width:=spritew;
   //Fill the bottom part of the container
   lbl[sn].Height:=spriteh-(4+size+4);
   //Position it
   lbl[sn].Left:=4+x;
   lbl[sn].Top:=4+y+36;
  end;
  //Make the tab visible, so we can see them
  SpriteViewer.TabVisible:=True;
  //And switch to it
  PageControl.ActivePage:=SpriteViewer;
  PageControlChange(nil);
 end;
 //Free up the memory stream and sprite file
 ms.Free;
 sp.Free;
end;

{-------------------------------------------------------------------------------
 The window is getting resized
-------------------------------------------------------------------------------}
procedure THexDumpForm.SpriteOutputResize(Sender: TObject);
begin
 SpritePanel.Top:=0;
 SpritePanel.Left:=0;
 SpritePanel.Width:=SpriteOutput.ClientWidth;
 SpritePanel.Height:=((numsprites*spritew)div(SpritePanel.ClientWidth-spritew))*spriteh;
 SpritePanel.Height:=SpritePanel.Height+spriteh+4;
 if SpriteOutput.ClientHeight>SpritePanel.Height then
  SpritePanel.Height:=SpriteOutput.ClientHeight;
end;

{-------------------------------------------------------------------------------
 Font size is being changed on the BASIC output
-------------------------------------------------------------------------------}
procedure THexDumpForm.edFontSizeChange(Sender: TObject);
begin
 udFontSize.Position:=StrToIntDef(edFontSize.Text,8);
 BasicOutput.DefaultFontSize:=udFontSize.Position;
 DecodeBasicFile;
end;

end.
