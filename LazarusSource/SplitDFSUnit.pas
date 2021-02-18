unit SplitDFSUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes,SysUtils,Forms,Controls,Graphics,Dialogs,ComCtrls,EditBtn,Buttons,
 StdCtrls,DiscImageUtils,DiscImage;

type

 { TSplitDFSForm }

 TSplitDFSForm = class(TForm)
  DFSPages: TPageControl;
  GroupBox1: TGroupBox;
  GroupBox2: TGroupBox;
  GroupBox3: TGroupBox;
  GroupBox7: TGroupBox;
  GroupBox8: TGroupBox;
  GroupBox9: TGroupBox;
  Buttons: TImageList;
  lbDestSSD2: TLabel;
  lbSourceSSD0: TLabel;
  lbSourceSSD2: TLabel;
  lbSourceDSD: TLabel;
  lbDestSSD0: TLabel;
  lbDestDSD: TLabel;
  OpenDialog: TOpenDialog;
  SaveDialog: TSaveDialog;
  sbCancel2: TSpeedButton;
  sbConfirmSplit: TSpeedButton;
  sbCancel: TSpeedButton;
  sbConfirmCombine: TSpeedButton;
  sbLoadSourceDSD: TSpeedButton;
  sbSaveDestDSD: TSpeedButton;
  sbSaveDestSSD0: TSpeedButton;
  sbSaveDestSSD2: TSpeedButton;
  sbLoadSourceSSD0: TSpeedButton;
  sbLoadSourceSSD2: TSpeedButton;
  split: TTabSheet;
  combine: TTabSheet;
  procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  function GetAbsAddr(address: Cardinal;side: Byte): Cardinal;
  procedure FormShow(Sender: TObject);
  procedure sbCancelClick(Sender: TObject);
  procedure sbConfirmCombineClick(Sender: TObject);
  procedure sbConfirmSplitClick(Sender: TObject);
  procedure sbLoadSourceSSD0Click(Sender: TObject);
  procedure sbSaveDestDSDClick(Sender: TObject);
  procedure sbSaveDestSSD0Click(Sender: TObject);
  procedure sbLoadSourceDSDClick(Sender: TObject);
  function IsImageValid(filename: String;dsd: Boolean): Boolean;
 private

 public

 end;

var
 SplitDFSForm: TSplitDFSForm;

implementation

{$R *.lfm}

{ TSplitDFSForm }

{------------------------------------------------------------------------------}
//Convert an address and side into a offset into the image
{------------------------------------------------------------------------------}
function TSplitDFSForm.GetAbsAddr(address: Cardinal;side: Byte): Cardinal;
var
 sector,
 offset : Cardinal;
begin
 //Taken directly from the DiscImage class
 sector:=address DIV $100; //Sectors are $100 in size, and we need to know the sector
 offset:=address MOD $100; //Offset within the sector
 //Annoyingly, it is the tracks which are interleaved, not the sectors.
 //On Acorn DFS discs, there are 10 sectors per track
 Result:=(((sector MOD 10)+(20*(sector DIV 10))+(10*side))*$100)+offset;
end;

{------------------------------------------------------------------------------}
//User has drop a file on the form
{------------------------------------------------------------------------------}
procedure TSplitDFSForm.FormDropFiles(Sender: TObject;
 const FileNames: array of String);
begin
 //If the active page is the splitter, then open it there
 if DFSPages.ActivePage=split then
 begin
  //Is it a double sided DFS?
  if IsImageValid(Filenames[0],True) then
  begin
   //Set the source filename
   lbSourceDSD.Caption:=FileNames[0];
   //And the destination filenames
   lbDestSSD0.Caption:=LeftStr(lbSourceDSD.Caption,Length(lbSourceDSD.Caption)-4)
                      +'-DFSSide0.ssd';
   lbDestSSD2.Caption:=LeftStr(lbSourceDSD.Caption,Length(lbSourceDSD.Caption)-4)
                      +'-DFSSide2.ssd';
  end;
  //Enable/Disable the confirm button
  sbConfirmSplit.Enabled:=(lbSourceDSD.Caption<>'')and
                          (lbDestSSD0.Caption<>'')and
                          (lbDestSSD2.Caption<>'');
 end;
 //If the active page is the combiner, then open it there
 if DFSPages.ActivePage=combine then
 begin
  //Is it a single sided DFS?
  if IsImageValid(Filenames[0],False) then
   //Have we already got one in the first side?
   if lbSourceSSD0.Caption='' then //No
   begin
    lbSourceSSD0.Caption:=FileNames[0]; //Update the labels
    //Is there a second?
    if Length(FileNames)>1 then
     //Is it a single sided DFS?
     if IsImageValid(Filenames[1],False) then
      lbSourceSSD2.Caption:=FileNames[1];
   end
   else //If first slot is already taken, put it in the second slot
    lbSourceSSD2.Caption:=FileNames[0]; //Update the labels
  //Enable/Disable the confirm button
  sbConfirmCombine.Enabled:=(lbDestDSD.Caption<>'')and
                            (lbSourceSSD0.Caption<>'')and
                            (lbSourceSSD2.Caption<>'');
 end;
end;

{------------------------------------------------------------------------------}
//Open a source DSD to split
{------------------------------------------------------------------------------}
procedure TSplitDFSForm.sbLoadSourceDSDClick(Sender: TObject);
begin
 //Set up the open dialogue
 OpenDialog.Filename:='';
 OpenDialog.Filter:='Double Sided DFS|*.dsd';
 OpenDialog.DefaultExt:='dsd';
 //Show the dialogue box
 If(OpenDialog.Execute)and(IsImageValid(OpenDialog.Filename,True)) then
 begin
  //Set the source filename
  lbSourceDSD.Caption:=OpenDialog.Filename;
  //And the destination filenames
  lbDestSSD0.Caption:=LeftStr(lbSourceDSD.Caption,Length(lbSourceDSD.Caption)-4)
                     +'-DFSSide0.ssd';
  lbDestSSD2.Caption:=LeftStr(lbSourceDSD.Caption,Length(lbSourceDSD.Caption)-4)
                     +'-DFSSide2.ssd';
 end;
 //Enable/Disable the confirm button
 sbConfirmSplit.Enabled:=(lbSourceDSD.Caption<>'')and
                         (lbDestSSD0.Caption<>'')and
                         (lbDestSSD2.Caption<>'');
end;

{------------------------------------------------------------------------------}
//Select a destination SSD for split
{------------------------------------------------------------------------------}
procedure TSplitDFSForm.sbSaveDestSSD0Click(Sender: TObject);
var
 side: Byte;
begin
 side:=0;
 //Work out which side
 if (Sender=sbSaveDestSSD0) then side:=0;
 if (Sender=sbSaveDestSSD2) then side:=2;
 //Set the save dialogue filename
 if side=0 then SaveDialog.Filename:=lbDestSSD0.Caption;
 if side=2 then SaveDialog.Filename:=lbDestSSD2.Caption;
 //And set up the rest of the dialogue
 SaveDialog.Filter:='Single Sided DFS|*.ssd';
 SaveDialog.DefaultExt:='ssd';
 //Show the dialogue
 if SaveDialog.Execute then
 begin
  //Show the filename in the label
  if side=0 then lbDestSSD0.Caption:=SaveDialog.Filename;
  if side=2 then lbDestSSD2.Caption:=SaveDialog.Filename;
 end;
 //Enable/Disable the confirm button
 sbConfirmSplit.Enabled:=(lbSourceDSD.Caption<>'')and
                         (lbDestSSD0.Caption<>'')and
                         (lbDestSSD2.Caption<>'');
end;

{------------------------------------------------------------------------------}
//Form is being shown
{------------------------------------------------------------------------------}
procedure TSplitDFSForm.FormShow(Sender: TObject);
begin
 //Clear all the labels
 lbSourceDSD.Caption     :='';
 lbDestSSD0.Caption      :='';
 lbDestSSD2.Caption      :='';
 lbSourceSSD0.Caption    :='';
 lbSourceSSD2.Caption    :='';
 lbDestDSD.Caption       :='';
 //Set the active page
 DFSPages.ActivePage     :=split;
 //And disable the confirm buttons
 sbConfirmSplit.Enabled  :=False;
 sbConfirmCombine.Enabled:=False;
end;

{------------------------------------------------------------------------------}
//Cancel clicked
{------------------------------------------------------------------------------}
procedure TSplitDFSForm.sbCancelClick(Sender: TObject);
begin
 ModalResult:=mrCancel;
end;

{------------------------------------------------------------------------------}
//Confirm clicked on combine
{------------------------------------------------------------------------------}
procedure TSplitDFSForm.sbConfirmCombineClick(Sender: TObject);
var
 sc        : array[0..1] of TFileStream;
 dst       : TFileStream;
 buffer    : TDIByteArray;
 address,
 i         : Cardinal;
 sidesize  : array[0..1] of Cardinal;
 side      : Byte;
begin
 //Set up the buffer
 SetLength(buffer,$A00);
 try
  //Open the three streams (one destination and two sources)
  dst:=TFileStream.Create(lbDestDSD.Caption,fmCreate or fmShareDenyNone);
  sc[0]:=TFileStream.Create(lbSourceSSD0.Caption,fmOpenRead OR fmShareDenyNone);
  sc[1]:=TFileStream.Create(lbSourceSSD2.Caption,fmOpenRead OR fmShareDenyNone);
  //Read each side and output
  for side:=0 to 1 do
  begin
   //How big is the side?
   sc[side].Position:=$106;
   i:=sc[side].Read(buffer[0],2);
   sidesize[side]:=(buffer[1]+(buffer[0]AND$3)*$100)*$100;
   //Read the side, in 10 sector chunks
   address:=$0;
   while address<sidesize[side] do
   begin
    //Clear the buffer
    for i:=0 to Length(buffer)-1 do buffer[i]:=0;
    //Read from the source
    //We make sure we don't read over the end
    //We'll still write all of buffer out, which is why we cleared it
    if address<sc[side].Size then
    begin
     //Position within the file
     sc[side].Position:=address;
     //And read
     i:=sc[side].Read(buffer[0],Length(buffer));
    end;
    //Write to the destination
    dst.Position:=GetAbsAddr(address,side);
    dst.Write(buffer[0],Length(buffer));
    //Move onto the next block
    inc(address,Length(buffer));
   end;
  end;
  //Close the three streams
  dst.Free;
  sc[0].Free;
  sc[1].Free;
  //All OK, so close with OK
  ModalResult:=mrOK;
 except
  //An error occurred, so feedback an abort
  ModalResult:=mrAbort;
 end;
end;

{------------------------------------------------------------------------------}
//Confirm clicked on Split
{------------------------------------------------------------------------------}
procedure TSplitDFSForm.sbConfirmSplitClick(Sender: TObject);
var
 src       : TFileStream;
 ds        : array[0..1] of TFileStream;
 buffer    : TDIByteArray;
 address,
 i         : Cardinal;
 sidesize  : array[0..1] of Cardinal;
 side      : Byte;
begin
 //Set up the buffer
 SetLength(buffer,$A00);
 try
  //Open the three streams (one source and two destinations)
  src:=TFileStream.Create(lbSourceDSD.Caption,fmOpenRead OR fmShareDenyNone);
  ds[0]:=TFileStream.Create(lbDestSSD0.Caption,fmCreate or fmShareDenyNone);
  ds[1]:=TFileStream.Create(lbDestSSD2.Caption,fmCreate or fmShareDenyNone);
  //Read each side and output
  for side:=0 to 1 do
  begin
   //How big is the side?
   src.Position:=GetAbsAddr($106,side);
   i:=src.Read(buffer[0],2);
   sidesize[side]:=(buffer[1]+(buffer[0]AND$3)*$100)*$100;
   //Read the side, in 10 sector chunks
   address:=$0;
   while address<sidesize[side] do
   begin
    //Clear the buffer
    for i:=0 to Length(buffer)-1 do buffer[i]:=0;
    //Read from the source
    //We make sure we don't read over the end
    //We'll still write all of buffer out, which is why we cleared it
    if GetAbsAddr(address,side)<src.Size then
    begin
     //Set the position within the file
     src.Position:=GetAbsAddr(address,side);
     //And read
     i:=src.Read(buffer[0],Length(buffer));
    end;
    //Write to the destination
    ds[side].Position:=address;
    ds[side].Write(buffer[0],Length(buffer));
    //And move the pointer on
    inc(address,Length(buffer));
   end;
  end;
  //Close the three streams
  src.Free;
  ds[0].Free;
  ds[1].Free;
  //All OK, so close with OK
  ModalResult:=mrOK;
 except
  //An error occurred, so feedback an abort
  ModalResult:=mrAbort;
 end;
end;

{------------------------------------------------------------------------------}
//Load a source SSD to combine
{------------------------------------------------------------------------------}
procedure TSplitDFSForm.sbLoadSourceSSD0Click(Sender: TObject);
var
 side: Byte;
begin
 side:=0;
 //Work out which side
 if (Sender=sbLoadSourceSSD0) then side:=0;
 if (Sender=sbLoadSourceSSD2) then side:=2;
 //Setup the dialogue box
 OpenDialog.Filename:='';
 OpenDialog.Filter:='Single Sided DFS|*.ssd';
 OpenDialog.DefaultExt:='ssd';
 //Show the dialogue box
 if(OpenDialog.Execute)and(IsImageValid(OpenDialog.Filename,False)) then
 begin
  //Update the label
  if side=0 then lbSourceSSD0.Caption:=OpenDialog.Filename;
  if side=2 then lbSourceSSD2.Caption:=OpenDialog.Filename;
 end;
 //Enable/Disable the confirm button
 sbConfirmCombine.Enabled:=(lbDestDSD.Caption<>'')and
                           (lbSourceSSD0.Caption<>'')and
                           (lbSourceSSD2.Caption<>'');
end;

{------------------------------------------------------------------------------}
//Save a destination DSD to combine
{------------------------------------------------------------------------------}
procedure TSplitDFSForm.sbSaveDestDSDClick(Sender: TObject);
begin
 //Set up the diaglogue box
 SaveDialog.Filename:=LeftStr(lbSourceSSD0.Caption,Length(lbSourceSSD0.Caption)-4)
                     +'.dsd';
 SaveDialog.Filter:='Double Sided DFS|*.dsd';
 SaveDialog.DefaultExt:='dsd';
 //Show it
 If SaveDialog.Execute then //Update the label
  lbDestDSD.Caption:=SaveDialog.Filename;
 //Enable/Disable the confirm button
 sbConfirmCombine.Enabled:=(lbDestDSD.Caption<>'')and
                           (lbSourceSSD0.Caption<>'')and
                           (lbSourceSSD2.Caption<>'');
end;

{------------------------------------------------------------------------------}
//Confirm if a supplied image is valid or not
{------------------------------------------------------------------------------}
function TSplitDFSForm.IsImageValid(filename: String;dsd: Boolean): Boolean;
var
 TestImage: TDiscImage;
begin
 TestImage:=TDiscImage.Create;
 TestImage.LoadFromFile(filename,false);
 Result:=(TestImage.FormatNumber shr 4=0)and(TestImage.DoubleSided=dsd);
 TestImage.Free;
end;

end.
