unit HexDumpUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,Grids,ExtCtrls,Buttons,
 StdCtrls,ComCtrls,DiscImageUtils,Global;

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
  ButtonImages: TImageList;
  edJump: TEdit;
  edXOR: TEdit;
  HexDumpDisplay: TStringGrid;
  Label1: TLabel;
  Label2: TLabel;
  NavImages: TImageList;
  Panel1: TPanel;
  Panel2: TPanel;
  pbProgress: TProgressBar;
  SaveFile: TSaveDialog;
  ScrollBar1: TScrollBar;
  procedure btnMoveDownClick(Sender: TObject);
  procedure btnMoveDownLineClick(Sender: TObject);
  procedure btnMoveToBottomClick(Sender: TObject);
  procedure btnMoveToTopClick(Sender: TObject);
  procedure btnMoveUpClick(Sender: TObject);
  procedure btnMoveUpLineClick(Sender: TObject);
  procedure edXORKeyPress(Sender: TObject; var Key: char);
  procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
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
 private

 public
  buffer          : TDIByteArray;

 end;

var
 HexDumpForm: THexDumpForm;

implementation

{$R *.lfm}

uses MainUnit;

{ THexDumpForm }

{                                                                              }
{ Procedure to run when the application first runs                             }
{                                                                              }
procedure THexDumpForm.FormShow(Sender: TObject);
var
 c: Integer;
begin
 //Set up the String Grid
 HexDumpDisplay.FixedCols:=1;
 HexDumpDisplay.FixedRows:=1;
 HexDumpDisplay.RowCount :=1;
 //Header
 HexDumpDisplay.ColWidths[0] :=80;
 HexDumpDisplay.Cells[0,0]   :='Address';
 HexDumpDisplay.ColWidths[17]:=120;
 HexDumpDisplay.Cells[17,0]  :='ASCII';
 for c:=1 to 16 do
 begin
  HexDumpDisplay.ColWidths[c]:=25;
  HexDumpDisplay.Cells[c,0]  :=IntToHex(c-1,2);
 end;
 ResetApplication;
 //Set up the form
 Width:=635;
 //Show the hex display
 DisplayHex(0);
 //Setup the scrollbar
 ScrollBar1.Max:=Length(buffer);
 ScrollBar1.Min:=0;
 ScrollBar1.Position:=0;
 ScrollBar1.Enabled:=True;
end;

{                                                                              }
{ Reset the application                                                        }
{                                                                              }
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
 ScrollBar1.Enabled     :=True;
 //Empty the grid
 HexDumpDisplay.RowCount:=1;
end;

{                                                                              }
{ Procedure to run when the user presses a key while in the XOR key box        }
{                                                                              }
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

{                                                                              }
{ User has pressed a key on the Jump edit box                                  }
{                                                                              }
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

{                                                                              }
{ Procedure to run when the scroll bar moves                                   }
{                                                                              }
procedure THexDumpForm.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
 var ScrollPos: Integer);
begin
 //Just move the display on to the position dictated by the scroll bar
 DisplayHex((ScrollPos div $10)*$10);
end;

{                                                                              }
{ User has clicked on the Save As Text File button                             }
{                                                                              }
procedure THexDumpForm.btnSaveTextClick(Sender: TObject);
var
 line  : String;
 F     : TFileStream;
 p,len : Byte;
 i,pos : Integer;
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
  WriteLine(F,'Address     00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F  ASCII');
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
    line:=line+' ';
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

{                                                                              }
{ Populates the grid, starting at address 'start'                              }
{                                                                              }
procedure THexDumpForm.DisplayHex(start: Cardinal);
var
 rows,
 line  : Cardinal;
 ch,
 len,
 key   : Byte;
 chars : String;
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
 ScrollBar1.Position:=start;
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
  HexDumpDisplay.Cells[0,line+1]:=IntToHex((start div $10)*$10,10);
  //Go through the data just read in
  for ch:=0 to len-1 do
  begin
   //Display each one as hex - colours are dealt with elsewhere
   HexDumpDisplay.Cells[ch+1,line+1]:=IntToHex(buffer[start+ch]XOR key,2);
   //Add add to the character column
   if (buffer[start+ch]XOR key>31) AND (buffer[start+ch]XOR key<127) then
    chars:=chars+Chr(buffer[start+ch]XOR key) //Printable
   else
    chars:=chars+'.';                   //Not printable
  end;
  //Are there more cells than data?
  if len<$10 then
   for ch:=len to $0F do
    HexDumpDisplay.Cells[ch+1,line+1]:='';
  //Display the characters in the final coluumn
  HexDumpDisplay.Cells[17,line+1]:=chars;
  //And move onto the next line
  inc(line);
  inc(start,len);
 until (start>=Length(buffer)-1) //Continue until the end of the file
 or (line+1=HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight); //Or form
end;

{                                                                              }
{ Navigation buttons - move up a page                                          }
{                                                                              }
procedure THexDumpForm.btnMoveUpClick(Sender: TObject);
var
 s,rows: Cardinal;
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

{                                                                              }
{ Navigation buttons - move up a line                                          }
{                                                                              }
procedure THexDumpForm.btnMoveUpLineClick(Sender: TObject);
var
 s: Cardinal;
begin
 //Get the current top position
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 //Can we move up?
 if s>0 then dec(s);
 //Update the display
 DisplayHex(s*$10);
end;

{                                                                              }
{ Navigation buttons - move down a page                                        }
{                                                                              }
procedure THexDumpForm.btnMoveDownClick(Sender: TObject);
var
 s,rows: Cardinal;
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

{                                                                              }
{ Navigation buttons - move down a line                                        }
{                                                                              }
procedure THexDumpForm.btnMoveDownLineClick(Sender: TObject);
var
 s: Cardinal;
begin
 //Get the current top position
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 //Move down one line
 inc(s);
 //Update the display
 DisplayHex(s*$10);
end;

{                                                                              }
{ Navigation buttons - move to the end of the file                             }
{                                                                              }
procedure THexDumpForm.btnMoveToBottomClick(Sender: TObject);
begin
 //Just update the display with the filesize
 DisplayHex(Length(buffer));
end;

{                                                                              }
{ Navigation buttons - move to the beginning of the file                       }
{                                                                              }
procedure THexDumpForm.btnMoveToTopClick(Sender: TObject);
begin
 //Just update the display with zero
 DisplayHex(0);
end;

{                                                                              }
{ User is resizing the form                                                    }
{                                                                              }
procedure THexDumpForm.FormResize(Sender: TObject);
var
 s,rows: Cardinal;
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
end;

{                                                                              }
{ This doesn't actually appear to do anything                                  }
{                                                                              }
procedure THexDumpForm.HexDumpDisplayGetCellHint(Sender: TObject; ACol,
 ARow: Integer; var HintText: String);
var
 s: Byte;
begin
 s:=StrToIntDef('$'+HexDumpDisplay.Cells[aCol,aRow],0);
 if (s>31) and (s<127) then HintText:=chr(s)
 else HintText:='';
end;

{                                                                              }
{ User has pressed a key while editing a value on the grid                     }
{                                                                              }
procedure THexDumpForm.HexDumpDisplayKeyPress(Sender: TObject; var Key: char);
begin
 //Verify it is in hex, delete, backspace or Enter
 if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
 and (Key<>chr(127)) AND (Key<>chr(8)) AND (Key<>chr(13)) then Key:=#0;
end;

{                                                                              }
{ This recolours the individual cells                                          }
{                                                                              }
procedure THexDumpForm.HexDumpDisplayPrepareCanvas(sender: TObject; aCol,
 aRow: Integer; aState: TGridDrawState);
var
 s: Byte;
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

{                                                                              }
{ User is trying to edit a cell                                                }
{                                                                              }
procedure THexDumpForm.HexDumpDisplaySelectCell(Sender: TObject; aCol,
 aRow: Integer; var CanSelect: Boolean);
begin
 //We can only edit cells 1 to 16, and not the character column
 if aCol=17 then CanSelect:=False
 else CanSelect:=True;
end;

{                                                                              }
{ This fires everytime a change is made                                        }
{                                                                              }
procedure THexDumpForm.HexDumpDisplaySetEditText(Sender: TObject; ACol,
 ARow: Integer; const Value: String);
begin
 //We need to make sure that we don't have more than 2 character length
 If Length(value)>2 then
  HexDumpDisplay.Cells[ACol,ARow]:=LeftStr(value,2);
end;

{                                                                              }
{ User has finished editing the cell, so now validate                          }
{                                                                              }
procedure THexDumpForm.HexDumpDisplayValidateEntry(sender: TObject; aCol,
 aRow: Integer; const OldValue: String; var NewValue: String);
var
 p: Cardinal;
 c: String;
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

end.
