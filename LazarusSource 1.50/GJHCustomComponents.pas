unit GJHCustomComponents;

{
GJH Custom Components V1.10
Copyright (C) 2024-2025 Gerald Holdsworth gerald@hollypops.co.uk

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

{$mode ObjFPC}{$H+}
{$WARN 2005 off : Comment level $1 found}
interface

uses
 Classes,SysUtils,Graphics,ExtCtrls,Controls,StrUtils,Math,Forms,Buttons,StdCtrls
 {{$IFNDEF Darwin}}, Registry//{$ENDIF}
// {$IFDEF Darwin}, MacOSAll, CFPreferences{$ENDIF}
 ;

{$M+}

//Global constants
const
 GJHVersion             = '1.10';
 cmColBlack             = #$81#$00#$00#$00;
 cmColRed               = #$81#$00#$00#$FF;
 cmColGreen             = #$81#$00#$FF#$00;
 cmColDGreen            = #$81#$00#$77#$00;
 cmColYellow            = #$81#$00#$FF#$FF;
 cmColBlue              = #$81#$FF#$00#$00;
 cmColCyan              = #$81#$FF#$00#$FF;
 cmColMagenta           = #$81#$FF#$FF#$00;
 cmColWhite             = #$81#$FF#$FF#$FF;
 cmResetCol             = #$82;
 cmHighBlack            = #$83#$00#$00#$00;
 cmHighRed              = #$83#$00#$00#$FF;
 cmHighGreen            = #$83#$00#$FF#$00;
 cmHighYellow           = #$83#$00#$FF#$FF;
 cmHighBlue             = #$83#$FF#$00#$00;
 cmHighCyan             = #$83#$FF#$00#$FF;
 cmHighMagenta          = #$83#$FF#$FF#$00;
 cmHighWhite            = #$83#$FF#$FF#$FF;
 cmResetHigh            = #$84;
 cmBold                 = #$85#$01;
 cmItalic               = #$85#$02;
 cmBoldItalic           = #$85#$03;
 cmStrike               = #$85#$04;
 cmBoldStrike           = #$85#$05;
 cmItalicStrike         = #$85#$06;
 cmBoldItalicStrike     = #$85#$07;
 cmUnder                = #$85#$08;
 cmBoldUnder            = #$85#$09;
 cmItalicUnder          = #$85#$0A;
 cmBoldItalicUnder      = #$85#$0B;
 cmStrikeUnder          = #$85#$0C;
 cmBoldStrikeUnder      = #$85#$0D;
 cmItalicStrikeUnder    = #$85#$0E;
 cmBoldItalicStrikeUnder= #$85#$0F;
 cmResetStyle           = #$86;

//RISC OS style control parent class - declarations ++++++++++++++++++++++++++++
type
 TRISCOSControl = class(TGraphicControl)
 private
  FOnChange : TNotifyEvent;
  FNative   : Boolean;
  procedure ForceRedraw;
 published
  property OnChange: TNotifyEvent read FOnChange  write FOnChange;
 end;

//RISC OS style tick boxes - declarations ++++++++++++++++++++++++++++++++++++++
type
 TRISCOSTickBoxes = class(TRISCOSControl)
 private
  FExclusive,
  FOnlyMouse,
  FTicked    : Boolean;
  FGroup     : Integer;
  FOn,
  FOff       : TPortableNetworkGraphic;
  FColour    : TColor;
  FNativeBox : TObject;
  function CreateTickBox(aOwner: TObject): TCheckBox;
  function CreateRadioBox(aOwner: TObject): TRadioButton;
  procedure SetCaption(const LCaption: String);
  function GetCaption: String;
  procedure SetTicked(const LTicked: Boolean);
  function GetTicked: Boolean;
  procedure SetTop(aValue: Integer);
  function GetTop: Integer;
  procedure SetLeft(aValue: Integer);
  function GetLeft: Integer;
  procedure SetNative(aValue: Boolean);
  procedure UnsetOthers;
 public
  destructor Destroy; override;
  procedure Paint; override;
  procedure Click; override;
 published
  //Methods
  constructor Create(AOwner: TComponent); override;
  //Properties
  property Caption  : String  read GetCaption write SetCaption;
  property Colour   : TColor  read FColour    write FColour     default clNone;
  property OnlyMouse: Boolean read FOnlyMouse write FOnlyMouse  default False;
  property NativeOS : Boolean read FNative    write SetNative   default False;
  property Ticked   : Boolean read GetTicked  write SetTicked   default False;
  property Top      : Integer read GetTop     write SetTop;
  property Left     : Integer read GetLeft    write SetLeft;
 end;

//Tick box - declarations ++++++++++++++++++++++++++++++++++++++++++++++++++++++
type
 TRISCOSTickBox = class(TRISCOSTickBoxes)
 private
  procedure DoClick(Sender: TObject);
  const
{$INCLUDE 'TickBoxGraphics.pas'}
 published
  constructor Create(AOwner: TComponent); override;
 public
  destructor Destroy; override;
end;

//Radio box - declarations +++++++++++++++++++++++++++++++++++++++++++++++++++++
type
 TRISCOSRadioBox = class(TRISCOSTickBoxes)
 private
  procedure DoClick(Sender: TObject);
  const
{$INCLUDE 'RadioBoxGraphics.pas'}
 protected
  // Protected Methods
 published
  constructor Create(AOwner: TComponent); override;
  property Group : Integer read FGroup write FGroup default 0;
 public
  destructor Destroy; override;
end;

//Coloured Slider - declarations +++++++++++++++++++++++++++++++++++++++++++++++
type TFaderColour=(csRed,csGreen,csBlue,csYellow,csBlack,csWhite);
type TOrientation=(csHorizontal,csVertical);
type TOuterBorder=(csOutNone,csOutInner,csOutOuter,csOutBoth);
type TRISCOSSlider = class(TRISCOSControl)
 private
  FBackColour,
  FGradColour,
  FColour      : TColor;
  F3DBorder,
  FTransparent,
  FMouseIsDown,
  FShowValue,
  FHexValue,
  FGradient,
  FPointers,
  FFaders,
  FFaderGrads,
  FFillSlider  : Boolean;
  FOutline     : TOuterBorder;
  FFaderSize,
  FPosition,
  FMax,
  FMin,
  FBorderSize,
  FStep        : Integer;
  FValueDiv    : Byte;
  FSuffix,
  FCaption     : String;
  FOrient      : TOrientation;
  FFaderColour : TFaderColour;
  procedure SetPosition(const LPosition: Integer);
  procedure SetStep(const LStep: Integer);
  procedure SetColour(const LColour: TColor);
  procedure SetMax(const LMax: Integer);
  procedure SetMin(const LMin: Integer);
  procedure SetShowValue(const LShowValue: Boolean);
  procedure SetHexValue(const LHexValue: Boolean);
  procedure SetOrient(const LOrient: TOrientation);
  procedure SetGradient(const LGradient: Boolean);
  procedure SetPointers(const LPointers: Boolean);
  procedure SetFaders(const LFaders: Boolean);
  procedure SetFaderGrads(const LFaderGrads: Boolean);
  procedure SetFaderColour(const LFaderColour: TFaderColour);
  procedure SetFillSlider(const LFillSlider: Boolean);
  procedure SetOutline(const LOutline: TOuterBorder);
  procedure SetSuffix(const LSuffix: String);
  procedure SetCaption(const LCaption: String);
  procedure SetBackColour(const LBackColour: TColor);
  procedure SetTransparent(const LTransparent: Boolean);
  procedure Set3DBorder(const L3DBorder: Boolean);
  procedure SetValueDiv(const LValueDiv: Byte);
  procedure SetGradColour(const LGradColour: TColor);
  function GetSliderEnd: Integer;
  function GetValue: String;
  function GetSliderStart: Integer;
 const
{$INCLUDE 'PointerGraphics.pas'}
{$INCLUDE 'FaderGraphics.pas'}
  FGap = 4;
 public
  procedure MouseDown(Button :TMouseButton; Shift :TShiftState; X, Y :Integer); override;
  procedure MouseMove(Shift :TShiftState; X, Y :Integer); override;
  procedure MouseUp(Button :TMouseButton; Shift :TShiftState; X, Y :Integer); override;
  procedure Paint; override;
  destructor Destroy; override;
 published
  //Methods
  constructor Create(AOwner: TComponent); override;
  //Properties
  property OnMouseMove;
  property OnMouseDown;
  property OnMouseUp;
  property OnMouseEnter;
  property OnMouseLeave;
           //Background colour of the control, unless transparent
  property BackColour : TColor       read FBackColour  write SetBackColour  default $FFFFFF;
           //Use a RISC OS style 3D border
  property Border3D   : Boolean      read F3DBorder    write Set3DBorder    default False;
           //Text to print at the top of the control
  property Caption    : string       read FCaption     write SetCaption;
           //Colour of the slider fill
  property Colour     : TColor       read FColour      write SetColour      default $0000FF;
           //Make it a fader, rather than a slider
  property Faders     : Boolean      read FFaders      write SetFaders      default False;
           //Colour of the fader 'knob'
  property FaderColour: TFaderColour read FFaderColour write SetFaderColour default csBlack;
           //Show graduations with a fader
  property FaderGrads : Boolean      read FFaderGrads  write SetFaderGrads  default True;
           //Fill the slider all the way, or just to the position
  property FillSlider : Boolean      read FFillSlider  write SetFillSlider  default False;
           //Colour of the graduations on the fader
  property GradColour : TColor       read FGradColour  write SetGradColour  default $000000;
           //Slider colour is a gradient fill
  property Gradient   : Boolean      read FGradient    write SetGradient    default False;
           //Print the value at the bottom as a hex value
  property HexValue   : Boolean      read FHexValue    write SetHexValue    default False;
           //Maximum value
  property Max        : Integer      read FMax         write SetMax         default 100;
           //Minimum value
  property Min        : Integer      read FMin         write SetMin         default 0;
           //Whether to show native OS slider or RISC OS styled/fader
  property NativeOS   : Boolean      read FNative      write FNative        default False;
           //How to orient the control - faders can only be vertical
  property Orientation: TOrientation read FOrient      write SetOrient      default csVertical;
           //What outlines to print - inside, outside or both
  property Outline    : TOuterBorder read FOutline     write SetOutline     default csOutOuter;
           //Show the pointers? Set this and Faders to true for faders
  property Pointers   : Boolean      read FPointers    write SetPointers    default True;
           //Current position
  property Position   : Integer      read FPosition    write SetPosition    default 0;
           //Print the value at the bottom
  property ShowValue  : Boolean      read FShowValue   write SetShowValue   default False;
           //Step size between values
  property Step       : Integer      read FStep        write SetStep        default 1;
           //Suffix to add after the value
  property Suffix     : string       read FSuffix      write SetSuffix;
           //Background is transparent
  property Transparent: Boolean      read FTransparent write SetTransparent default True;
           //What to divide the value by to get the actual value (printed only)
  property ValueDiv   : Byte         read FValueDiv    write SetValueDiv    default 1;
end;

//RISC OS Buttons - declarations +++++++++++++++++++++++++++++++++++++++++++++++
type TRISCOSButton = class(TCustomSpeedButton)
 private
  FNative,
  FPushed,
  FDefault    : Boolean;
  FModalResult: TModalResult;
 protected
  procedure SetDefault(const LDefault: Boolean);
  procedure SetDimensions;
  procedure SetModalResult(const LModalResult: TModalResult);
  procedure SetNative(const LNative: Boolean);
 public
  procedure MouseDown(Button :TMouseButton; Shift :TShiftState; X, Y :Integer); override;
  procedure MouseUp(Button :TMouseButton; Shift :TShiftState; X, Y :Integer); override;
  procedure Paint; override;
  destructor Destroy; override;
  procedure Click; override;
 published
  //Methods
  constructor Create(AOwner: TComponent); override;
  //Properties
  property OnMouseMove;
  property OnMouseDown;
  property OnMouseUp;
  property OnMouseEnter;
  property OnMouseLeave;
  property Default    : Boolean      read FDefault     write SetDefault     default False;
  property NativeOS   : Boolean      read FNative      write SetNative      default False;
  property ModalResult: TModalResult read FModalResult write SetModalResult default mrNone;
end;

//TColouredMemo class - declarations +++++++++++++++++++++++++++++++++++++++++++
type TColouredMemo = class(TScrollingWinControl)
 private
  type //Extended the TStringList so we can repaint the form when a line is added
   TExtStringList=class(TStringList)
    FColouredMemo : TColouredMemo;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    function Add(const Fmt : string; const Args : Array of const): Integer; overload;
    function AddObject(const Fmt: string; Args : Array of const; AObject: TObject): Integer; overload;
    function AddPair(const AName, AValue: string): TStrings; overload;
    function AddPair(const AName, AValue: string; AObject: TObject): TStrings; overload;
    procedure AddStrings(TheStrings: TStrings); override;
    procedure AddStrings(TheStrings: TStrings; ClearFirst : Boolean); overload;
    procedure AddStrings(const TheStrings: array of string); overload; override;
    procedure AddStrings(const TheStrings: array of string; ClearFirst : Boolean); overload;
    procedure SetStrings(TheStrings: TStrings); override;
    procedure SetStrings(TheStrings: array of string); override; overload;
    Procedure AddText(Const S : String); override;
    procedure AddCommaText(const S: String);
    procedure AddDelimitedText(const S: String; ADelimiter: char; AStrictDelimiter: Boolean);
    procedure AddDelimitedtext(const S: String); overload;
    procedure Append(const S: string);
    procedure Assign(Source: TPersistent); override;
   end;
 var
  FLines    : TExtStringList;
  FLineSpace: Cardinal;
  FIndent   : Cardinal;
  FContent  : TImage;
  FPlainText: TExtStringList;
  FTextWrap : Boolean;
 protected
  procedure Paint; override;
  procedure SetLines(const AValue: TExtStringList);
  procedure SetTextWrap(const AValue: Boolean);
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
 published
  procedure Clear;
  property AutoScroll;
  property Indent    : Cardinal       read FIndent    write FIndent;
  property Lines     : TExtStringList read FLines     write SetLines;
  property LineSpace : Cardinal       read FLineSpace write FLineSpace;
  property PlainText : TExtStringList read FPlainText;
  property TextWrap  : Boolean        read FTextWrap  write SetTextWrap;
end;

//Registry Class - declarations ++++++++++++++++++++++++++++++++++++++++++++++++
type TGJHRegistry = class
 private
//  {$IFNDEF Darwin}
  FRegistry : TRegistry;
//  {$ENDIF}
  FRegKey   : String;
//  {$IFNDEF Darwin}
  procedure OpenReg(key: String);
//  {$ENDIF}
  function ExtractKey(var V: String):String;
{  {$IFDEF Darwin}
  procedure SetMacValue(V,X: String);
  {$ENDIF}}
 published
  //Methods
  constructor Create(LRegKey: String);
  function DoesKeyExist(V: String):Boolean;
//  {$IFNDEF Darwin}
  function DeleteKey(key: String): Boolean;
  procedure GetRegValA(V: String;var D: array of Byte);
  function GetRegValS(V: String;D: String): String;
  function GetRegValS(V: String): String; overload;
  function GetRegValB(V: String;D: Boolean): Boolean;
  function GetRegValB(V: String): Boolean; overload;
  function GetRegValI(V: String;D: Cardinal;CrNew: Boolean=True): Cardinal;
  function GetRegValI(V: String): Cardinal; overload;
{  {$ENDIF}
  {$IFDEF Darwin}
  procedure DeleteKey(key: String);
  function GetRegValB(V: String;D: Boolean): Boolean;
  function GetRegValI(V: String;D: Cardinal): Cardinal;
  {$ENDIF}
  {$IFNDEF Darwin}}
  procedure SetRegValA(V: String;var D: array of Byte);
//  {$ENDIF}
  procedure SetRegValS(V: String;D: String);
  procedure SetRegValB(V: String;D: Boolean);
  procedure SetRegValI(V: String;D: Cardinal);
  //Properties
  property Key : String read FRegKey;
 public
  destructor Destroy; override;
end;

procedure Register;
function ParseCSVLine(Line: String): TStringArray;

//Methods and functions ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

implementation

{-------------------------------------------------------------------------------
Register all the components
-------------------------------------------------------------------------------}
procedure Register;
begin
 RegisterComponents('GJH Custom Components',[TRISCOSTickBox,
                                             TRISCOSRadioBox,
                                             TRISCOSTickBoxes,
                                             TRISCOSSlider,
                                             TRISCOSButton,
                                             TColouredMemo]);
end;

{-------------------------------------------------------------------------------
Parse a CSV line into an array
-------------------------------------------------------------------------------}
function ParseCSVLine(Line: String): TStringArray;
var
 Index: Integer=0;
 PLine: PChar='';
begin
 Result:=nil;
 //If not blank
 if Line<>'' then
 begin
  //Split it into an array
  Result:=Line.Split(',','"');//Split at commas, unless surrounded by quotes
  //Make sure it has at least a field
  if Length(Result)>0 then
  for Index:=0 to Length(Result)-1 do
  begin
   //Get each field
   PLine:=PChar(Result[Index]);
   //And remove any quotes (except those escaped)
   Result[Index]:=AnsiExtractQuotedStr(PLine,'"');
  end;
 end;
end;

//RISC OS Control parent Methods +++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Force a redraw of the control
-------------------------------------------------------------------------------}
procedure TRISCOSControl.ForceRedraw;
begin
 Invalidate;
 Update;
end;

//Tickbox parent Methods +++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the global variables
-------------------------------------------------------------------------------}
constructor TRISCOSTickBoxes.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 //Set the default variables
 FTicked   :=False;
 FColour   :=clNone;
 FOnlyMouse:=False;
 FNative   :=False;
 //Create the on and off graphics
 FOn       :=TPortableNetworkGraphic.Create;
 FOff      :=TPortableNetworkGraphic.Create;
end;

{-------------------------------------------------------------------------------
Class destructor - tidies up afterwards
-------------------------------------------------------------------------------}
destructor TRISCOSTickBoxes.Destroy;
begin
 FOn.Free;
 FOff.Free;
 inherited;
end;

{-------------------------------------------------------------------------------
Paint the control
-------------------------------------------------------------------------------}
procedure TRISCOSTickBoxes.Paint;
var
 Lgf : TPortableNetworkGraphic=nil;
 R   : TRect=();
 Lcol: TColor=0;
begin
 if not FNative then
 begin
  //Create a temporary graphic
  Lgf       :=TPortableNetworkGraphic.Create;
  //Set it's dimensions
  Lgf.Width :=FOn.Width;
  Lgf.Height:=FOn.Height;
  //Colour the background, if one has been specified
  if FColour=clNone then
  begin
   Canvas.Brush.Style:=bsClear;
   Canvas.Pen.Style  :=psClear;
  end
  else
  begin
   Canvas.Brush.Color:=FColour;
   Canvas.Brush.Style:=bsSolid;
   Canvas.Pen.Color  :=FColour;
   Canvas.Pen.Style  :=psSolid;
   Canvas.Rectangle(0,0,Width,Height);
  end;
  //Paint the appropriate graphic
  if FTicked then Lgf.Assign(FOn) else Lgf.Assign(FOff);
  //And paint it onto the control
  R.Top   :=0;
  R.Left  :=0;
  R.Width :=Height;
  R.Height:=Height;
  Canvas.StretchDraw(R,Lgf);
  Lgf.Free;
  //Control enabled?
  Lcol:=Canvas.Font.Color;
  if not Enabled then Canvas.Font.Color:=$8E8E8E;
  //Write the text
  Canvas.TextOut(Height,(Height-Canvas.TextHeight(Caption))div 2,' '+Caption);
  Canvas.Font.Color:=LCol;
 end;
end;

{-------------------------------------------------------------------------------
React to the click
-------------------------------------------------------------------------------}
procedure TRISCOSTickBoxes.Click;
begin
 if not FExclusive then FTicked:=not FTicked else FTicked:=True;
 ForceRedraw;
 UnsetOthers;
 //Fire the OnChange event
 if Assigned(FOnChange) then FOnChange(Self as TObject);
 inherited Click;
end;

{-------------------------------------------------------------------------------
If this is set, unset every other one
-------------------------------------------------------------------------------}
procedure TRISCOSTickBoxes.UnsetOthers;
var
 LParent: TComponent=nil;
 LGroup : Integer=0;
 Index  : Integer=0;
begin
 if(FExclusive)and(FTicked)then
  //Now we need to unset every other radio box control sibling
  if HasParent then
  begin
   //Get the parent control
   LParent:=GetParentComponent;
   //And iterate through it's children
   for Index:=0 to LParent.ComponentCount-1 do
    //Ignore ourself
    if (LParent.Components[Index]<>Self)
    and(LParent.Components[Index].ClassName=ClassName)then //But not other radios
    begin
     if LParent.Components[Index] is TRISCOSRadioBox then
      LGroup:=TRISCOSRadioBox(LParent.Components[Index]).Group
     else LGroup:=FGroup;
     if LGroup=FGroup then //Ignore other groups
      TRISCOSTickBoxes(LParent.Components[Index]).Ticked:=False; //Unset them
    end;
  end;
end;

{-------------------------------------------------------------------------------
Caption has changed, so adjust the dimensions
-------------------------------------------------------------------------------}
procedure TRISCOSTickBoxes.SetCaption(const LCaption: String);
var
 Ltext: String='';
begin
 inherited Caption:=LCaption;
 if FNativeBox is TCheckBox then (FNativeBox as TCheckBox).Caption:=LCaption;
 if FNativeBox is TRadioButton then (FNativeBox as TRadioButton).Caption:=LCaption;
 Ltext   :=' '+Caption;
 Height  :=Canvas.TextHeight(Ltext);
 Width   :=Canvas.TextWidth(Ltext)+Height+4;
 Invalidate;//Force a redraw
 Update;
end;

{-------------------------------------------------------------------------------
Get the caption
-------------------------------------------------------------------------------}
function TRISCOSTickBoxes.GetCaption: String;
begin
 Result:=inherited Caption;
end;

{-------------------------------------------------------------------------------
The ticked state has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSTickBoxes.SetTicked(const LTicked: Boolean);
begin
 if FNativeBox is TCheckBox then (FNativeBox as TCheckBox).Checked:=LTicked;
 if FNativeBox is TRadioButton then (FNativeBox as TRadioButton).Checked:=LTicked;
 FTicked:=LTicked;
 ForceRedraw;
 UnsetOthers;
 //Fire the OnChange event
 if not FOnlyMouse then //Unless we only reacting to a mouse click
  if Assigned(FOnChange) then FOnChange(Self as TObject);
end;

{-------------------------------------------------------------------------------
Get the ticked state
-------------------------------------------------------------------------------}
function TRISCOSTickBoxes.GetTicked: Boolean;
begin
 Result:=FTicked;
end;

{-------------------------------------------------------------------------------
Create a native OS tickbox
-------------------------------------------------------------------------------}
function TRISCOSTickBoxes.CreateTickBox(aOwner: TObject): TCheckBox;
begin
 Result:=TCheckBox.Create(AOwner as TComponent);
 Result.Parent:=AOwner as TWinControl;
 Result.Visible:=FNative;
end;

{-------------------------------------------------------------------------------
Create a native OS radio box
-------------------------------------------------------------------------------}
function TRISCOSTickBoxes.CreateRadioBox(aOwner: TObject): TRadioButton;
begin
 Result:=TRadioButton.Create(AOwner as TComponent);
 Result.Parent:=AOwner as TWinControl;
 Result.Visible:=FNative;
end;

{-------------------------------------------------------------------------------
Set the top co-ordinate of the control
-------------------------------------------------------------------------------}
procedure TRISCOSTickBoxes.SetTop(aValue: Integer);
begin
 if FNativeBox is TCheckBox then (FNativeBox as TCheckBox).Top:=aValue;
 if FNativeBox is TRadioButton then (FNativeBox as TRadioButton).Top:=aValue;
 inherited Top:=aValue;
end;

{-------------------------------------------------------------------------------
Get the top co-ordinate of the control
-------------------------------------------------------------------------------}
function TRISCOSTickBoxes.GetTop: Integer;
begin
 Result:=inherited Top;
end;

{-------------------------------------------------------------------------------
Set the left co-ordinate of the control
-------------------------------------------------------------------------------}
procedure TRISCOSTickBoxes.SetLeft(aValue: Integer);
begin
 if FNativeBox is TCheckBox then (FNativeBox as TCheckBox).Left:=aValue;
 if FNativeBox is TRadioButton then (FNativeBox as TRadioButton).Left:=aValue;
 inherited Left:=aValue;
end;

{-------------------------------------------------------------------------------
Get the left co-ordinate of the control
-------------------------------------------------------------------------------}
function TRISCOSTickBoxes.GetLeft: Integer;
begin
 Result:=inherited Left;
end;

{-------------------------------------------------------------------------------
Set whether the control is native OS or RISC OS styled
-------------------------------------------------------------------------------}
procedure TRISCOSTickBoxes.SetNative(aValue: Boolean);
begin
 if FNativeBox is TCheckBox then (FNativeBox as TCheckBox).Visible:=aValue;
 if FNativeBox is TRadioButton then (FNativeBox as TRadioButton).Visible:=aValue;
 FNative:=aValue;
 ForceRedraw;
end;

//Tickbox Methods ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the local variables
-------------------------------------------------------------------------------}
constructor TRISCOSTickBox.Create(AOwner: TComponent);
var
 Lms: TMemoryStream=nil;
begin
 inherited Create(AOwner);
 FExclusive:=False;
 //Create the tick box graphics and assign them
 Lms:=TMemoryStream.Create;
 Lms.Write(FTickBoxOn[0],Length(FTickBoxOn));
 Lms.Position:=0;
 FOn.LoadFromStream(Lms);
 Lms.Clear;
 Lms.Write(FTickBoxOff[0],Length(FTickBoxOff));
 Lms.Position:=0;
 FOff.LoadFromStream(Lms);
 Lms.Free;
 //Create the native OS tick box
 FNativeBox:=CreateTickBox(AOwner);
 (FNativeBox as TCheckBox).OnClick:=@DoClick;
end;

{-------------------------------------------------------------------------------
We need an OnClick event, locally
-------------------------------------------------------------------------------}
procedure TRISCOSTickBox.DoClick(Sender: TObject);
begin
 inherited Click;
end;

{-------------------------------------------------------------------------------
Free up and tidy up
-------------------------------------------------------------------------------}
destructor TRISCOSTickBox.Destroy;
begin
 inherited;
end;

//Radio box Methods ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the local variables
-------------------------------------------------------------------------------}
constructor TRISCOSRadioBox.Create(AOwner: TComponent);
var
 Lms: TMemoryStream=nil;
begin
 inherited Create(AOwner);
 FExclusive:=True;
 FGroup:=0;
 //Create the radio box graphics and assign them
 Lms:=TMemoryStream.Create;
 Lms.Write(FRadioOn[0],Length(FRadioOn));
 Lms.Position:=0;
 FOn.LoadFromStream(Lms);
 Lms.Clear;
 Lms.Write(FRadioOff[0],Length(FRadioOff));
 Lms.Position:=0;
 FOff.LoadFromStream(Lms);
 Lms.Free;
 //Create the native OS radio box
 FNativeBox:=CreateRadioBox(AOwner);
 (FNativeBox as TRadioButton).OnClick:=@DoClick;
end;

{-------------------------------------------------------------------------------
We need an OnClick event, locally
-------------------------------------------------------------------------------}
procedure TRISCOSRadioBox.DoClick(Sender: TObject);
begin
 inherited Click;
end;

{-------------------------------------------------------------------------------
Class destructor - tidy up
-------------------------------------------------------------------------------}
destructor TRISCOSRadioBox.Destroy;
begin
 inherited;
end;

//Slider methods +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the local variables
-------------------------------------------------------------------------------}
constructor TRISCOSSlider.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 //Default values
 FMin        :=0;
 FMax        :=100;
 FPosition   :=0;
 FBackColour :=clWhite;
 FColour     :=clBlue;
 FCaption    :='';
 FSuffix     :='';
 FShowValue  :=False;
 FStep       :=1;
 FHexValue   :=False;
 FOrient     :=csVertical;
 FGradient   :=False;
 FPointers   :=True;
 FFaders     :=False;
 FFaderGrads :=True;
 FFaderColour:=csBlack;
 FGradColour :=clBlack;
 FFillSlider :=False;
 FOutline    :=csOutOuter;
 FTransparent:=True;
 F3DBorder   :=False;
 FBorderSize :=Round(ScreenInfo.PixelsPerInchX/96)<<1;//Scaled and to the nearest even number
 FValueDiv   :=1;
 Width       :=40;
 Height      :=300;
 FFaderSize  :=0;
end;

{-------------------------------------------------------------------------------
Destructor - tidy up
-------------------------------------------------------------------------------}
destructor TRISCOSSlider.Destroy;
begin
 inherited;
end;

{-------------------------------------------------------------------------------
Paint the control
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.Paint;
var
 LUnit      : Real=0;
 LSliderSize: Integer=0;
 LPosition  : Integer=0;
 LY         : Integer=0;
 Index      : Integer=0;
 LTX        : Integer=0;
 LTY        : Integer=0;
 LX         : Integer=0;
 LH         : Integer=0;
 LCaption   : String='';
 Lms        : TMemoryStream=nil;
 Lpng       : TPortableNetworkGraphic=nil;
 LR         : TRect=();
 procedure GetGraphic(LGraphic: array of Byte);
 begin
  Lms.Clear;
  Lms.Write(LGraphic[0],Length(LGraphic));
  Lms.Position:=0;
  Lpng.LoadFromStream(Lms);
 end;
begin
 inherited Paint;
 FFaderSize :=0;
 //Work out the position (centre of control)
 if FOrient=csVertical then
 begin
  LSliderSize:=(Width div 2)-FGap;
  if(FFaders)and(FPointers)then LSliderSize:=(Width div 2)-FGap*4;
  LX         :=(Width-LSliderSize)div 2;
 end
 else
 begin
  LSliderSize:=(Height div 2)-FGap;
  LX         :=(Height-LSliderSize)div 2;
 end;
 //Work out where the top and bottom of the slider area
 if FOrient=csVertical then
 begin
  LY:=GetSliderStart;
  LH:=GetSliderEnd;
 end
 else
 begin
  LH:=GetSliderStart;
  LY:=GetSliderEnd;
 end;
 //Fader - take account of the fader overhang
 if(FFaders)and(FPointers)then
 begin
  //First we need to know the height of the graphic
  Lms :=TMemoryStream.Create;
  Lpng:=TPortableNetworkGraphic.Create;
  //And the colour
  case FFaderColour of
   csBlack : GetGraphic(FBlackFader);
   csRed   : GetGraphic(FRedFader);
   csGreen : GetGraphic(FGreenFader);
   csBlue  : GetGraphic(FBlueFader);
   csYellow: GetGraphic(FYellowFader);
   csWhite : GetGraphic(FWhiteFader);
  end;
  FFaderSize :=Round(Lpng.Height*(60/Width)) div 4;
  dec(LH,FFaderSize);
  inc(LY,FFaderSize);
 end;
 //Are we displaying the value?
 LCaption:=GetValue;
 //If so, then paint it
 Canvas.Brush.Style:=bsClear;
 if LCaption<>'' then
 begin
  Canvas.Font:=Font;
  if not Enabled then Canvas.Font.Color:=$8E8E8E;
  if FOrient=csVertical then
  begin
   LTX:=(Width-Canvas.GetTextWidth(LCaption))div 2;
   if F3DBorder then LTY:=FBorderSize else LTY:=0;
   Canvas.TextOut(LTX,Height-Canvas.GetTextHeight(LCaption)-LTY,LCaption);
  end
  else
  begin
   LTX:=(Height-Canvas.GetTextHeight(LCaption))div 2;
   Canvas.TextOut(0,LTX,LCaption);
  end;
 end;
 //Are we displaying any caption?
 if FCaption<>'' then
 begin
  Canvas.Font:=Font;
  if not Enabled then Canvas.Font.Color:=$8E8E8E;
  if FOrient=csVertical then
  begin
   LTX:=(Width-Canvas.GetTextWidth(FCaption))div 2;
   if F3DBorder then LTY:=FBorderSize else LTY:=0;
   Canvas.TextOut(LTX,LTY,FCaption);
  end
  else
  begin
   LTX:=(Height-Canvas.GetTextHeight(FCaption))div 2;
   Canvas.TextOut(LY+FGap,LTX,FCaption);
  end;
 end;
 //And paint it
 Canvas.Brush.Style:=bsSolid;
 //Fill in the background
 if not FTransparent then
 begin
  Canvas.Brush.Color:=FBackColour;
  Canvas.Pen.Style  :=psClear;
  if FOrient=csVertical then
   Canvas.Rectangle(LX,LY,LX+LSliderSize,LH)
  else
   Canvas.Rectangle(LY,LX,LH,LX+LSliderSize);
 end;
 //Gradient fill
 if FGradient then
 begin
  Canvas.Pen.Style:=psSolid;
  //Smallest unit for rectangles
  LUnit:=abs(LH-LY)/(FMax-FMin);
  for Index:=1 to FMax-FMin do
   if((Index<=FPosition-FMin)and(not FFillSlider))//Upto the position
   or(FFillSlider)then//Or the entire slider?
   begin
    Canvas.Brush.Color:=Round((FColour     AND$FF)*(Index/(FMax-FMin)))
                     OR Round((FColour>>8  AND$FF)*(Index/(FMax-FMin)))<<8
                     OR Round((FColour>>16 AND$FF)*(Index/(FMax-FMin)))<<16;
    Canvas.Pen.Color:=Canvas.Brush.Color;
    if FOrient=csVertical then
     Canvas.Rectangle(LX,
                      LH-Math.Ceil(LUnit*(Index-1)),
                      LX+LSliderSize,
                      LH-Math.Ceil(LUnit*Index))
    else
     Canvas.Rectangle(LH+Math.Ceil(LUnit*(Index-1)),
                      LX,
                      LH+Math.Ceil(LUnit*Index),
                      LX+LSliderSize);
   end;
 end
 else //Solid fill
 begin
  //Work out where the filler starts and ends
  if not FFillSlider then
   LPosition:=LH-Round(((FPosition-FMin)/(FMax-FMin))*(LH-LY))
  else
   LPosition:=LY;
  Canvas.Brush.Color:=FColour;
  if(FOutline=csOutInner)or(FOutline=csOutBoth)then
  begin
   Canvas.Pen.Style:=psSolid;
   Canvas.Pen.Color:=$000000;
  end
  else
   Canvas.Pen.Style:=psClear;
  //Draw the rectangle
  if FOrient=csVertical then
   Canvas.Rectangle(LX       ,LPosition,LX+LSliderSize,LH)
  else
   Canvas.Rectangle(LPosition,LX       ,LH            ,LX+LSliderSize);
 end;
 //Draw a little sliver of bar if at minimum, and no outer outline
 if (not FFillSlider)
 and(FPosition=FMin)
 and(FOutline<>csOutOuter)
 and(FOutline<>csOutBoth) then
  if FOrient=csVertical then
   Canvas.Rectangle(LX,LH,LX+LSliderSize,LH+1)
  else
   Canvas.Rectangle(LH,LX,LH+1          ,LX+LSliderSize);
 //Draw the outline
 if(FOutline=csOutOuter)or(FOutline=csOutBoth)then
 begin
  Canvas.Pen.Color  :=$000000;
  Canvas.Pen.Style  :=psSolid;
  Canvas.Brush.Style:=bsClear;
  if FOrient=csVertical then
   Canvas.Rectangle(LX,LY,LX+LSliderSize,LH)
  else
   Canvas.Rectangle(LH,LX,LY            ,LX+LSliderSize);
 end;
 //Graduation marks
 if(FFaders)and(FFaderGrads)and(FPointers)then
 begin
  //Set the colour and style
  Canvas.Pen.Color  :=FGradColour;
  Canvas.Pen.Style  :=psSolid;
  //Mark from min to max
  for Index:=FMin to FMax do
  begin
   //Different line size for different gradients
   if Index=0 then Canvas.Pen.Width:=2 else Canvas.Pen.Width:=1;
   //Draw the line
   if(Index mod 10=0)
   or(Index=FMin)
   or(Index=FMax)
   or(Index=0)then
   begin
    //Get the position of the tick mark
    LPosition:=LH-Round(((Index-FMin)/(FMax-FMin))*(LH-LY));
    //Draw a full line for every tenth
    if((Index div FValueDiv)mod 10=0)or(Index=FMin)or(Index=FMax)then
    begin
     Canvas.Line(FGap               ,LPosition,LX-FGap   ,LPosition);
     Canvas.Line(LX+LSliderSize+FGap,LPosition,Width-FGap,LPosition);
    end;
    //Draw a half line for halfway between
    if(Index div FValueDiv)mod 5=0 then
    begin
     Canvas.Line(FGap                 ,LPosition,LX-FGap*3 ,LPosition);
     Canvas.Line(LX+LSliderSize+FGap*3,LPosition,Width-FGap,LPosition);
    end;
   end;
  end;
  Canvas.Pen.Width:=1;
 end;
 //3D Border
 if F3DBorder then
 begin
  //Top
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Style  :=psSolid;
  Canvas.Brush.Color:=$777777;
  Canvas.Pen.Color  :=$777777;
  Canvas.Rectangle(0,0,Width,FBorderSize);
  //Left
  Canvas.Rectangle(0,0,FBorderSize,Height);
  //Bottom
  Canvas.Brush.Color:=$FFFFFF;
  Canvas.Pen.Color  :=$FFFFFF;
  Canvas.Rectangle(FBorderSize>>1,Height-FBorderSize,Width,Height);
  Canvas.Rectangle(0,Height-FBorderSize>>1,Width,Height);
  //Right;
  Canvas.Rectangle(Width-FBorderSize,FBorderSize>>1,Width,Height);
  Canvas.Rectangle(Width-FBorderSize>>1,0,Width,Height);
 end;
 //Draw the pointers
 if FPointers then
 begin
  //This is only calculated when partially filled
  LPosition:=LH-Round(((FPosition-FMin)/(FMax-FMin))*(LH-LY));
  if FFaders then //Fader
  begin
   LR.Left  :=FGap;
   LR.Top   :=LPosition-FFaderSize;
   LR.Right :=Width-FGap;
   LR.Bottom:=LPosition+FFaderSize;
   Canvas.StretchDraw(LR,Lpng);
  end
  else
  begin //Pointers
   //Create the containers
   Lms :=TMemoryStream.Create;
   Lpng:=TPortableNetworkGraphic.Create;
   if FOrient=csVertical then
   begin
    //Left
    GetGraphic(FPointerLeft);
    LR.Left  :=LX+LSliderSize;
    LR.Top   :=LPosition-(Width-LSliderSize)div 4;
    LR.Right :=Width;
    LR.Bottom:=LPosition+(Width-LSliderSize)div 4;
    Canvas.StretchDraw(LR,Lpng);
    //Right
    GetGraphic(FPointerRight);
    LR.Left  :=0;
    LR.Top   :=LPosition-(Width-LSliderSize)div 4;
    LR.Right :=LX;
    LR.Bottom:=LPosition+(Width-LSliderSize)div 4;
    Canvas.StretchDraw(LR,Lpng);
   end
   else
   begin
    //Up
    GetGraphic(FPointerUp);
    LR.Top   :=LX+LSliderSize;
    LR.Left  :=LPosition-(Height-LSliderSize)div 4;
    LR.Bottom:=Height;
    LR.Right :=LPosition+(Height-LSliderSize)div 4;
    Canvas.StretchDraw(LR,Lpng);
    //Down
    GetGraphic(FPointerDown);
    LR.Top   :=0;
    LR.Left  :=LPosition-(Height-LSliderSize)div 4;
    LR.Bottom:=LX;
    LR.Right :=LPosition+(Height-LSliderSize)div 4;
    Canvas.StretchDraw(LR,Lpng);
   end;
   Lpng.Free;
   Lms.Free;
  end;
 end;
end;

{-------------------------------------------------------------------------------
Position and/or step has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetPosition(const LPosition: Integer);
var
 LOldPosition: Integer=0;
begin
 //Ensure that they are valid
 if FStep<1 then FStep:=1;
 if FStep>(FMax-FMin)div 2 then FStep:=(FMax-FMin)div 2;
 //Remember the old position
 LOldPosition:=FPosition;
 //Now change it
 if(LPosition>=FMin)and(LPosition<=FMax)then FPosition:=LPosition;
 FPosition:=(FPosition div FStep)*FStep;
 ForceRedraw;
 //Fire the OnChange event, if it has changed
 if FPosition<>LOldPosition then
  if Assigned(FOnChange) then FOnChange(Self as TObject);
end;

{-------------------------------------------------------------------------------
Step has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetStep(const LStep: Integer);
begin
 FStep:=LStep;
 //This is handled by the previous method
 SetPosition(FPosition);
end;

{-------------------------------------------------------------------------------
The colour has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetColour(const LColour: TColor);
begin
 FColour:=LColour;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
The suffix has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetSuffix(const LSuffix: String);
begin
 FSuffix:=LSuffix;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
The caption has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetCaption(const LCaption: String);
begin
 FCaption:=LCaption;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
The background colour has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetBackColour(const LBackColour: TColor);
begin
 FBackColour:=LBackColour;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
The transparent setting has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetTransparent(const LTransparent: Boolean);
begin
 FTransparent:=LTransparent;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
The 3D Border setting has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.Set3DBorder(const L3DBorder: Boolean);
begin
 F3DBorder:=L3DBorder;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
The value divider setting has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetValueDiv(const LValueDiv: Byte);
begin
 FValueDiv:=LValueDiv;
 if FValueDiv<1 then FValueDiv:=1;
 if FValueDiv>(FMax-FMin)div 2 then FValueDiv:=(FMax-FMin)div 2;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
The max has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetMax(const LMax: Integer);
begin
 //Ensure it is valid
 if LMax>FMin then FMax:=LMax;
 if FPosition>FMax then FPosition:=FMax;
 SetValueDiv(FValueDiv);
 SetPosition(FPosition);
end;

{-------------------------------------------------------------------------------
The min has been changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetMin(const LMin: Integer);
begin
 //Ensure it is valid
 if FMax>LMin then FMin:=LMin;
 if FPosition<FMin then FPosition:=FMin;
 SetValueDiv(FValueDiv);
 SetPosition(FPosition);
end;

{-------------------------------------------------------------------------------
The show value boolean has been toggled
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetShowValue(const LShowValue: Boolean);
begin
 FShowValue:=LShowValue;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
The show as Hex has been toggled
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetHexValue(const LHexValue: Boolean);
begin
 FHexValue:=LHexValue;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
Orientation has changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetOrient(const LOrient: TOrientation);
begin
 FOrient:=LOrient;
 if FFaders then FOrient:=csVertical; //Faders are vertical
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
Gradient has changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetGradient(const LGradient: Boolean);
begin
 FGradient:=LGradient;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
Showing pointers has changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetPointers(const LPointers: Boolean);
begin
 FPointers:=LPointers;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
Pointers are faders
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetFaders(const LFaders: Boolean);
begin
 FFaders:=LFaders;
 if(FFaders)and(FOrient=csHorizontal)then SetOrient(csVertical); //Faders are vertical
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
Print the gradient marks for faders
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetFaderGrads(const LFaderGrads: Boolean);
begin
 FFaderGrads:=LFaderGrads;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
Fader colour has changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetFaderColour(const LFaderColour: TFaderColour);
begin
 FFaderColour:=LFaderColour;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
Fader graduation colour has changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetGradColour(const LGradColour: TColor);
begin
 FGradColour:=LGradColour;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
Fill the slider has changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetFillSlider(const LFillSlider: Boolean);
begin
 FFillSlider:=LFillSlider;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
The outline setting has changed
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.SetOutline(const LOutline: TOuterBorder);
begin
 FOutline:=LOutline;
 ForceRedraw;
end;

{-------------------------------------------------------------------------------
React to the Mouse Down
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.MouseDown(Button: TMouseButton; Shift: TShiftState;
                            X, Y: Integer);
begin
 //Set the flag
 FMouseIsDown:=True;
 //And adjust the slider
 MouseMove(Shift,X,Y);
 inherited MouseDown(Button, Shift, X, Y);
end;

{-------------------------------------------------------------------------------
React to the Mouse Move
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.MouseMove(Shift: TShiftState; X,Y: Integer);
var
 Lposition : Integer=0;
 LH        : Integer=0;
 LY        : Integer=0;
 Lpercent  : Real=0;
begin
 //Only if the mouse button is down
 if FMouseIsDown then
 begin
  //Work out position
  LH:=GetSliderEnd-FFaderSize;
  LY:=GetSliderStart+FFaderSize;
  if((FOrient=csVertical)  and(Y>=LY)and(Y<=LH))
  or((FOrient=csHorizontal)and(X>=LY)and(X<=LH))then
  begin
   if FOrient=csVertical then Lposition:=LH-Y else Lposition:=X-LY;
   Lpercent:=Lposition/(LH-LY);
   SetPosition(Round((FMax-FMin)*Lpercent)+FMin);
   //Fire the OnChange event
   if Assigned(FOnChange) then FOnChange(Self as TObject);
  end;
 end;
 inherited MouseMove(Shift, X, Y);
end;

{-------------------------------------------------------------------------------
React to the Mouse Up
-------------------------------------------------------------------------------}
procedure TRISCOSSlider.MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
 //Clear the flag
 FMouseIsDown:=False;
 inherited MouseDown(Button, Shift, X, Y);
end;

{-------------------------------------------------------------------------------
Get the height of the slider, taking into account the text
-------------------------------------------------------------------------------}
function TRISCOSSlider.GetSliderEnd: Integer;
var
 LCaption : String='';
begin
 //Default, if there is nothing to print
 if FOrient=csVertical then
  Result:=Height-(Width div 4)
 else
  Result:=Width-(Height div 4);
 //Get the value
 LCaption:=GetValue;
 if LCaption<>'' then
 begin
  //Find out how high it will be
  Canvas.Font:=Font;
  if FOrient=csVertical then
  begin
   Result:=Height-Canvas.GetTextHeight(LCaption);
   if F3DBorder then dec(Result,FBorderSize);
  end
  else //Or wide
   Result:=Width-(Canvas.GetTextWidth(FCaption)+FGap);
 end;
end;

{-------------------------------------------------------------------------------
Returns a string representation of the value
-------------------------------------------------------------------------------}
function TRISCOSSlider.GetValue: String;
var
 L: Byte=0;
begin
 Result:='';
 //If we are showing a value
 if FShowValue then
 begin
  if FHexValue then //And it is in hex
  begin 
   //Work out how many digits are required
   L:=1;
   while IntToHex(FMax,L)[1]<>'0' do inc(L);
   Result:='0x'+IntToHex(FPosition,L-1)
  end
  else //Otherwise in decimal
  begin
   //Pad to how much?
   L     :=Length(IntToStr(FMax));
   if FValueDiv>1 then inc(L);
   if FValueDiv=0 then FValueDiv:=1;
   Result:=PadLeft(FloatToStr(FPosition/FValueDiv),L);
  end;
  if Result<>'' then Result:=Result+FSuffix;
 end;
end;

{-------------------------------------------------------------------------------
Work out where the top of the slider is
-------------------------------------------------------------------------------}
function TRISCOSSlider.GetSliderStart: Integer;
var
 LCaption : String='';
 LTemp    : Integer=0;
begin
 //Default - if no text to print
 if FOrient=csVertical then
  Result:=(Width div 4)
 else
  Result:=Height div 4;
 Canvas.Font:=Font;
 if(FCaption<>'')and(FOrient=csVertical)then
 begin
  Result:=Canvas.GetTextHeight(FCaption);
  if F3DBorder then inc(Result,FBorderSize);
 end;
 if FOrient=csHorizontal then
 begin
  //Get the maximum value
  LTemp    :=FPosition;//Remember the previous setting
  FPosition:=FMax;     //Set it to the max
  LCaption :=GetValue; //Get the value
  FPosition:=LTemp;    //Reset it
  Result   :=Canvas.GetTextWidth(LCaption)+FGap;
 end;
end;

//Button Methods +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the global variables
-------------------------------------------------------------------------------}
constructor TRISCOSButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 //Set the default variables
 FDefault:=False;
 FNative :=False;
// FCaption:='';
 FPushed :=False;
 SetDimensions;
 FModalResult:=mrNone;
end;

{-------------------------------------------------------------------------------
Class destructor - tidies up afterwards
-------------------------------------------------------------------------------}
destructor TRISCOSButton.Destroy;
begin
 inherited;
end;

{-------------------------------------------------------------------------------
Paint the control
-------------------------------------------------------------------------------}
procedure TRISCOSButton.Paint;
var
 LX    : Integer=0;
 LY    : Integer=0;
 LCol  : TColor=0;
 Lsize : Integer=0;
begin
 if not FNative then
 begin
  //Border size, 2px scaled and to the nearest even number
  Lsize:=Round(ScreenInfo.PixelsPerInchX/96)<<1;
  //Draw the button
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Style  :=psClear;
  //Outer border, top and left
  if(not FDefault)and(not FPushed)then Canvas.Brush.Color:=$FFFFFF;//Normal button, not pushed
  if(FDefault)and(not Enabled)    then Canvas.Brush.Color:=$BBBBBB;//Default button, disabled
  if((FDefault)and(Enabled))                                       //Default button, enabled
  or((not FDefault)and(Enabled)and(FPushed))then                   //and normal button, pushed, enabled
   Canvas.Brush.Color:=$777777;
  Canvas.Rectangle(0,0,Width,Lsize);
  Canvas.Rectangle(0,0,Lsize,Height);
  //Outer border, bottom and right
  if(not FDefault)and(not FPushed)and(Enabled)then                 //Normal button, not pushed, enabled
   Canvas.Brush.Color:=$777777;
  if(FDefault)                                                     //Default button
  or((not FDefault)and(Enabled)and(FPushed))then                   //and normal button, pushed, enabled
   Canvas.Brush.Color:=$FFFFFF;
  if(not FDefault)and(not Enabled)then Canvas.Brush.Color:=$BBBBBB;//Normal button, disabled
  Canvas.Rectangle(Width-Lsize      ,Lsize             ,Width      ,Height);
  Canvas.Rectangle(Width-Lsize div 2,Lsize div 2       ,Width      ,Lsize);
  Canvas.Rectangle(Lsize div 2      ,Height-Lsize      ,Width      ,Height);
  Canvas.Rectangle(0                ,Height-Lsize div 2,Lsize div 2,Height);
  //Button surface (normal) and gap between inside and outside borders (default)
  if(FDefault)    then Canvas.Brush.Color:=$BBEEEE;
  if(not FDefault)then Canvas.Brush.Color:=$DDDDDD;
  Canvas.Rectangle(Lsize,Lsize,Width-Lsize,Height-Lsize);
  //Default button, inside
  if(FDefault)and(not FPushed)then
  begin
   //Top and left inside border
   Canvas.Brush.Color:=$FFFFFF;
   Canvas.Rectangle(Lsize*2,Lsize*2,Width-Lsize*2,Lsize*3);
   Canvas.Rectangle(Lsize*2,Lsize*2,Lsize*3      ,Height-Lsize*2);
   //Bottom and right inside border
   if Enabled then Canvas.Brush.Color:=$777777 else Canvas.Brush.Color:=$BBBBBB;
   Canvas.Rectangle(Width-Lsize*3            ,Lsize*3                   ,Width-Lsize*2      ,Height-Lsize*2);
   Canvas.Rectangle(Width-Lsize*2-Lsize div 2,Lsize*2+Lsize div 2       ,Width-Lsize*2      ,Lsize*3);
   Canvas.Rectangle(Lsize*2+Lsize div 2      ,Height-Lsize*3            ,Width-Lsize*3      ,Height-Lsize*2);
   Canvas.Rectangle(Lsize*2                  ,Height-Lsize*2-Lsize div 2,Lsize*2+Lsize div 2,Height-Lsize*2);
   //Button surface
   Canvas.Brush.Color:=$DDDDDD;
   Canvas.Rectangle(Lsize*3,Lsize*3,Width-Lsize*3,Height-Lsize*3);
  end;
  //Write the text
  if Caption<>'' then
  begin
   //Remember the current colour
   LCol:=Canvas.Font.Color;
   //Change if disabled
   if not Enabled then Canvas.Font.Color:=$8E8E8E;
   //Find the centred position
   LX:=(Width -Canvas.GetTextWidth( Caption))div 2;
   LY:=(Height-Canvas.GetTextHeight(Caption))div 2;
   //Write with transparent background
   Canvas.Brush.Style:=bsClear;
   Canvas.TextOut(LX,LY,Caption);
   //Change the colour back
   Canvas.Font.Color:=LCol;
  end;
 end
 else inherited Paint;
end;

{-------------------------------------------------------------------------------
React to the mouse down
-------------------------------------------------------------------------------}
procedure TRISCOSButton.MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
 FPushed:=True;
 Invalidate;
 Update;
 inherited MouseDown(Button, Shift, X, Y);
end;

{-------------------------------------------------------------------------------
React to the mouse up
-------------------------------------------------------------------------------}
procedure TRISCOSButton.MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
 FPushed:=False;
 Invalidate;
 Update;
 inherited MouseUp(Button, Shift, X, Y);
end;

{-------------------------------------------------------------------------------
The click procedure
-------------------------------------------------------------------------------}
procedure TRISCOSButton.Click;
var
 Lform: TCustomForm=nil;
begin
 Lform:=GetParentForm(Self);
 if Lform<>nil then Lform.ModalResult:=FModalResult;
 inherited Click;
end;

{-------------------------------------------------------------------------------
The 'default' setting has changed
-------------------------------------------------------------------------------}
procedure TRISCOSButton.SetDefault(const LDefault: Boolean);
begin
 FDefault:=LDefault;
 SetDimensions;
 Invalidate;
 Update;
end;

{-------------------------------------------------------------------------------
Set the dimensions
-------------------------------------------------------------------------------}
procedure TRISCOSButton.SetDimensions;
var
 w,h: Integer;
begin
 if(FDefault)and(not FNative)then w:=92 else w:=84; //Width
 if(FDefault)and(not FNative)then h:=34 else h:=26; //Height
 Width :=Round(w*(ScreenInfo.PixelsPerInchX/96)); //Set width, scaled
 Height:=Round(h*(ScreenInfo.PixelsPerInchX/96)); //Set height, scaled
end;

{-------------------------------------------------------------------------------
The modal result has changed
-------------------------------------------------------------------------------}
procedure TRISCOSButton.SetModalResult(const LModalResult: TModalResult);
var
 Lform: TCustomForm=nil;
begin
 FModalResult:=LModalResult;
 Lform:=GetParentForm(Self);
 if Lform<>nil then
 begin
  Lform.CancelControl :=nil;
  Lform.DefaultControl:=nil;
  if FModalResult=mrOK     then Lform.DefaultControl:=Self;
  if FModalResult=mrCancel then Lform.CancelControl:=Self;
 end;
end;

{-------------------------------------------------------------------------------
User has changed the look of the button
-------------------------------------------------------------------------------}
procedure TRISCOSButton.SetNative(const LNative: Boolean);
begin
 FNative:=LNative;
 SetDimensions;
 Invalidate;
 Update;
end;

//ColouredMemo Methods +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
These bunch of functions and procedures are just inherited from the base class.
They are so that the main component gets repainted when the list is updated.
-------------------------------------------------------------------------------}
function TColouredMemo.TExtStringList.Add(const S: string): Integer;
begin
 Result:=inherited Add(S);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

function TColouredMemo.TExtStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
 Result:=inherited AddObject(S,AObject);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

function TColouredMemo.TExtStringList.Add(const Fmt : string; const Args : Array of const): Integer;
begin
 Result:=inherited Add(Fmt,Args);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

function TColouredMemo.TExtStringList.AddObject(const Fmt: string; Args : Array of const; AObject: TObject): Integer;
begin
 Result:=inherited AddObject(Fmt,Args,AObject);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

function TColouredMemo.TExtStringList.AddPair(const AName, AValue: string): TStrings;
begin
 Result:=inherited AddPair(AName,AValue);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

function TColouredMemo.TExtStringList.AddPair(const AName, AValue: string; AObject: TObject): TStrings;
begin
 Result:=inherited AddPair(AName,AValue,AObject);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddStrings(TheStrings: TStrings);
begin
 inherited AddStrings(TheStrings);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddStrings(TheStrings: TStrings; ClearFirst : Boolean);
begin
 inherited AddStrings(TheStrings,ClearFirst);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddStrings(const TheStrings: array of string);
begin
 inherited AddStrings(TheStrings);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddStrings(const TheStrings: array of string; ClearFirst : Boolean);
begin
 inherited AddStrings(TheStrings,ClearFirst);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.SetStrings(TheStrings: TStrings);
begin
 inherited SetStrings(TheStrings);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.SetStrings(TheStrings: array of string);
begin
 inherited SetStrings(TheStrings);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

Procedure TColouredMemo.TExtStringList.AddText(Const S : String);
begin
 inherited AddText(S);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddCommaText(const S: String);
begin
 inherited AddCommaText(S);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddDelimitedText(const S: String; ADelimiter: char; AStrictDelimiter: Boolean);
begin
 inherited AddDelimitedText(S,ADelimiter,AStrictDelimiter);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddDelimitedtext(const S: String);
begin
 inherited AddDelimitedText(S);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.Append(const S: string);
begin
 inherited Append(S);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.Assign(Source: TPersistent);
begin
 inherited Assign(Source);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

{-------------------------------------------------------------------------------
Constructor method for the coloured memo
-------------------------------------------------------------------------------}
constructor TColouredMemo.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 //Create the line container
 FLines:=TExtStringList.Create;
 FLines.FColouredMemo:=Self;
 //Create the plain text container
 FPlainText:=TExtStringList.Create;
 FPlainText.FColouredMemo:=Self;
 //Create the canvas
 FContent                      :=TImage.Create(Self);
 FContent.Parent               :=Self;
 FContent.Top                  :=0;
 FContent.Left                 :=0;
 FContent.Picture.Bitmap.Width :=ClientWidth;
 FContent.Picture.Bitmap.Height:=ClientHeight;
 FContent.Width                :=ClientWidth;
 FContent.Height               :=ClientHeight;
 FContent.Visible              :=True;
 //Defaults
 FLineSpace:=4;       //Space between lines, in pixels
 FIndent   :=4;       //Indent in from the left, in pixels
 Color     :=$FFFFFF; //Default background colour
 TextWrap  :=False;   //Whether to wrap a line to the next
end;

{-------------------------------------------------------------------------------
Destructor method for the coloured memo
-------------------------------------------------------------------------------}
destructor TColouredMemo.Destroy;
begin
 FLines.Free;
 FPlainText.Free;
 FContent.Free;
 inherited Destroy;
end;

{-------------------------------------------------------------------------------
Repaint method - this is where most of the work is done
-------------------------------------------------------------------------------}
procedure TColouredMemo.Paint;
var
 LLine : Integer=0;
 LIndex: Integer=0;
 XPos  : Integer=0;
 YPos  : Integer=0;
 W     : Integer=0;
 H     : Integer=0;
 LPart : String='';
 LText : String='';
 LTemp : Byte=0;
 LRed  : Byte=0;
 LGreen: Byte=0;
 LBlue : Byte=0;
 //Function to remove control characters
 function RemCont(rcText: String): String;
 var rcIndex: Integer;
 begin
  Result:='';
  if Length(rcText)>1 then
  begin
   rcIndex:=1;
   while rcIndex<=Length(rcText) do
   begin
    //Only ASCII printable characters
    if(ord(rcText[rcIndex])>31)and(ord(rcText[rcIndex])<127)then
     Result:=Result+rcText[rcIndex];
    //Our special styles, so move along a few more characters
    if(ord(rcText[rcIndex])=$81)
    or(ord(rcText[rcIndex])=$83)then inc(rcIndex,3);
    if ord(rcText[rcIndex])=$85 then inc(rcIndex,1);
    //Next character
    inc(rcIndex);
   end;
  end;
 end;
 //Procedure to print the text
 procedure PrintText(ptText: String);
 begin
  FContent.Canvas.FillRect(XPos,YPos,
                           XPos+FContent.Canvas.TextWidth(ptText),
                           YPos+FContent.Canvas.TextHeight(ptText)+FLineSpace);
  FContent.Canvas.TextOut(XPos,YPos,ptText);
  inc(XPos,FContent.Canvas.TextWidth(ptText));
 end;
 //Procedure to wrap the text
 procedure WrapText(wtText: String);
 var
  LStart,
  LLength: Integer;
 begin
  //Only wrap if it is longer than the available width
  if(FTextWrap)and(FContent.Canvas.TextWidth(wtText)+XPos>W)then
  begin
   //Start at the beginning
   LStart:=1;
   //And continue until we are out of characters
   while LStart<Length(wtText) do
   begin
    //Length of remaining string (if LStart+LLength are bigger than the string
    //length, then it'll just go to the end of the string)
    LLength:=Length(wtText);
    //Reduce the length until it'll fit
    while(FContent.Canvas.TextWidth(Copy(wtText,LStart,LLength))+XPos>W)
      and(LLength>1)do dec(LLength);
    //Then print what we can
    PrintText(Copy(wtText,LStart,LLength));
    //And move the starting position along
    inc(LStart,LLength);
    //If there are more characters left, then do a CR and LF
    if LStart<=Length(wtText) then
    begin
     //Move the Y pointer downwards
     inc(YPos,FContent.Canvas.TextHeight(wtText)+FLineSpace);
     //Reset X pointer
     XPos:=FLineSpace;
    end;
   end;
   //The string is only a single character, but still won't fit, so CR LF
   if Length(wtText)=1 then
   begin
    //Move the Y pointer downwards
    inc(YPos,FContent.Canvas.TextHeight(wtText)+FLineSpace);
    //Reset X pointer
    XPos:=FLineSpace;
   end;
   //The starting point is the last character, so print and move along
   if LStart=Length(wtText) then PrintText(Copy(wtText,LStart));
  end
  else PrintText(wtText);//No text wrapping
 end;
begin
 LRed  :=0;
 LGreen:=0;
 LBlue :=0;
 //Set the font
 FContent.Canvas.Font:=Font;
 //Starting size
 W:=ClientWidth;
 H:=FLineSpace;
 //First pass, calculate then set the canvas size
 if FLines.Count>0 then
 begin
  FPlainText.Clear;
  for LLine:=0 to FLines.Count-1 do
  begin
   LText:='';
   if FLines[LLine]='' then LText:=' ' //blank line will give 0 height
   else LText:=RemCont(FLines[LLine]);//Remove control characters
   if FContent.Canvas.TextWidth(LText)+FIndent>W then
   begin
    if not FTextWrap then W:=FContent.Canvas.TextWidth(LText)+FIndent
    //Take account of any text wrapping
    else inc(H,(FContent.Canvas.TextHeight(LText)+FLineSpace)
              *((FContent.Canvas.TextWidth(LText)+FIndent)div W));
   end;
   //Increase the height by a line
   inc(H,FContent.Canvas.TextHeight(LText)+FLineSpace);
   FPlainText.Add(LText);
  end;
 end;
 //Height can't be smaller than the scroll height
 if H<ClientHeight then H:=ClientHeight;
 //Set the canvas size
 FContent.Picture.Bitmap.Width :=W;
 FContent.Picture.Bitmap.Height:=H;
 FContent.Width :=W;
 FContent.Height:=H;
 //Clear the background
 FContent.Canvas.Brush.Color:=Color;
 FContent.Canvas.Brush.Style:=bsSolid;
 FContent.Canvas.Pen.Color  :=Color;
 FContent.Canvas.Pen.Style  :=psSolid;
 FContent.Canvas.Rectangle(0,0,W,H);
 //Are there any lines entered?
 if FLines.Count>0 then
 begin
  //Start at the top
  YPos:=FLineSpace;
  //Work our way through the lines
  for LLine:=0 to FLines.Count-1 do
  begin
   //Get the current line
   LText:=FLines[LLine];
   //If it is empty, put a space in so we get a blank line
   if LText='' then LText:=' ';
   //Indent it
   XPos:=FIndent;
   //Clear the 'part of' string
   LPart:='';
   //Start at the beginning
   LIndex:=1;
   while LIndex<=Length(LText) do
   begin
    //Top bit set? Then this means a change of style or colour
    if(ord(LText[LIndex])and$80)=$80 then
    begin
     //Output what we currently have
     if LPart<>'' then WrapText(LPart);
     //New part of string
     LPart:='';
     case (ord(LText[LIndex])AND$0F) of
      $01,$03: //Change of colour : $81 and $83
      begin
       LTemp:=ord(LText[LIndex])AND$0F;
       //Get the Blue index
       inc(LIndex);
       if LIndex<=Length(LText) then LBlue :=ord(LText[LIndex]);
       //Get the Green index
       inc(LIndex);
       if LIndex<=Length(LText) then LGreen:=ord(LText[LIndex]);
       //Get the Red index
       inc(LIndex);
       if LIndex<=Length(LText) then LRed  :=ord(LText[LIndex]);
       //Set the colour
       if Ltemp=$01 then //Foreground (text)
        FContent.Canvas.Font.Color:=LRed+LGreen<<8+LBlue<<16;
       if Ltemp=$03 then //Background (highlight)
        FContent.Canvas.Brush.Color:=LRed+LGreen<<8+LBlue<<16;
      end;
      $05: //Change of style : $85
      begin
       inc(LIndex);
       if LIndex<=Length(LText) then
       begin
        //Bold
        if(ord(LText[LIndex])AND$01)=$01 then
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style+[fsBold]
        else //No Bold
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style-[fsBold];
        //Italic
        if(ord(LText[LIndex])AND$02)=$02 then
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style+[fsItalic]
        else //No Italic
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style-[fsItalic];
        //Strikeout
        if(ord(LText[LIndex])AND$04)=$04 then
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style+[fsStrikeOut]
        else //No Strikeout
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style-[fsStrikeOut];
        //Underline
        if(ord(LText[LIndex])AND$08)=$08 then
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style+[fsUnderline]
        else //No Underline
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style-[fsUnderline];
       end;
      end;
      $02: FContent.Canvas.Font.Color :=Font.Color;//Reset foreground colour : $82
      $04: FContent.Canvas.Brush.Color:=Color;     //Reset background colour : $84
      $06: FContent.Canvas.Font.Style :=Font.Style;//Reset font style : $86
     end;
    end;
    //Valid ASCII character? add it to the 'part of' string
    if(ord(LText[LIndex])>31)and(ord(LText[LIndex])<127)then
     LPart:=LPart+LText[LIndex];
    //Next character
    inc(LIndex);
   end;
   //Anything left that hasn't been printed?
   if LPart<>'' then WrapText(LPart);
   //Move the Y pointer downwards
   inc(YPos,FContent.Canvas.TextHeight(LText)+FLineSpace);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Clears the text
-------------------------------------------------------------------------------}
procedure TColouredMemo.Clear;
begin
 FLines.Clear;
end;

{-------------------------------------------------------------------------------
Inherits from the base class - unlikely this ever gets called
-------------------------------------------------------------------------------}
procedure TColouredMemo.SetLines(const AValue: TExtStringList);
begin
 if AValue<>nil then FLines.Assign(AValue);
 Invalidate;
end;

{-------------------------------------------------------------------------------
Sets the text wrap flag, and fires off the repaint
-------------------------------------------------------------------------------}
procedure TColouredMemo.SetTextWrap(const AValue: Boolean);
begin
 FTextWrap:=AValue;
 Invalidate;
end;

//Registry Class methods +++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the global variables
-------------------------------------------------------------------------------}
constructor TGJHRegistry.Create(LRegKey: String);
begin
 inherited Create;
 FRegKey:=LRegKey;
end;

{-------------------------------------------------------------------------------
Class destructor
-------------------------------------------------------------------------------}
destructor TGJHRegistry.Destroy;
begin
 inherited;
end;

{-------------------------------------------------------------------------------
Open the registry key
-------------------------------------------------------------------------------}
//{$IFNDEF Darwin}
procedure TGJHRegistry.OpenReg(key: String);
begin
 FRegistry:=TRegistry.Create;
 if key<>'' then key:='\'+key;
 FRegistry.OpenKey(FRegKey+key,true);
end;
//{$ENDIF}

{-------------------------------------------------------------------------------
Function to delete a key from the registry
-------------------------------------------------------------------------------}
{{$IFDEF Darwin}
procedure TGJHRegistry.DeleteKey(key: String); 
{$ENDIF}
{$IFNDEF Darwin}}
function TGJHRegistry.DeleteKey(key: String): Boolean;
var
 x: Boolean=False;
//{$ENDIF}
begin
//{$IFNDEF Darwin}
 x:=True;
 OpenReg(ExtractKey(key));
 if FRegistry.ValueExists(key) then x:=FRegistry.DeleteValue(key);
 FRegistry.Free;
 Result:=x;
{{$ENDIF}
{$IFDEF Darwin}
 SetMacValue(key,NULL{%H-});
{$ENDIF}}
end;

{-------------------------------------------------------------------------------
Function to read a string from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
//{$IFNDEF Darwin}
function TGJHRegistry.GetRegValS(V: String;D: String): String;
var
 X: String='';
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadString(V)
 else begin X:=D;FRegistry.WriteString(V,X);end;
 FRegistry.Free;
 Result:=X;
end;
function TGJHRegistry.GetRegValS(V: String): String;
var
 X: String='';
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadString(V) else X:='';
 FRegistry.Free;
 Result:=X;
end;
//{$ENDIF}

{-------------------------------------------------------------------------------
Function to read an array from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
//{$IFNDEF Darwin}
procedure TGJHRegistry.GetRegValA(V: String;var D: array of Byte);
var
 s: Integer=0;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then
 begin
  s:=FRegistry.GetDataSize(V);
  FRegistry.ReadBinaryData(V,D,s);
 end
 else
 begin
  FRegistry.WriteBinaryData(V,D,SizeOf(D));
 end;
 FRegistry.Free;
end;
//{$ENDIF}

{-------------------------------------------------------------------------------
Function to read an integer from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}  
//{$IFNDEF Darwin}
function TGJHRegistry.GetRegValI(V: String;D: Cardinal;CrNew: Boolean=True): Cardinal;
var
 X: Cardinal=0;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadInteger(V)
 else
 begin
  X:=D;
  if CrNew then FRegistry.WriteInteger(V,X);
 end;
 FRegistry.Free;
 Result:=X;
end;
{The same as above, but doesn't create it}
function TGJHRegistry.GetRegValI(V: String): Cardinal;
begin
 Result:=GetRegValI(V,0,False);
end;
{{$ENDIF}
function TGJHRegistry.GetRegValI(V: String;D: Cardinal): Cardinal;
var
 X: Cardinal;
 IsValid: Boolean;
begin
 IsValid:=False;
 X:=CFPreferencesGetAppIntegerValue(CFStr(PChar(ExtractKey(V)))
                                   ,kCFPreferencesCurrentApplication
                                   ,IsValid);
 if IsValid then Result:=X else Result:=D;
end; }

{-------------------------------------------------------------------------------
Function to read a boolean from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
//{$IFNDEF Darwin}
function TGJHRegistry.GetRegValB(V: String;D: Boolean): Boolean;
var
 X: Boolean=False;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadBool(V)
 else begin X:=D;FRegistry.WriteBool(V,X);end;
 FRegistry.Free;
 Result:=X;
end;
function TGJHRegistry.GetRegValB(V: String): Boolean;
var
 X: Boolean=False;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadBool(V) else X:=False;
 FRegistry.Free;
 Result:=X;
end;
{{$ENDIF}
{$IFDEF Darwin}
function TGJHRegistry.GetRegValB(V: String;D: Boolean): Boolean;
var
 X      : Int64;
 IsValid: Boolean;
begin
 IsValid:=False;
 X:=CFPreferencesGetAppIntegerValue(CFStr(PChar(ExtractKey(V)))
                                   ,kCFPreferencesCurrentApplication
                                   ,IsValid);
 if IsValid then Result:=X<>0 else Result:=D;
end;
{$ENDIF}  }

{-------------------------------------------------------------------------------
Does the specified key exist?
-------------------------------------------------------------------------------}
function TGJHRegistry.DoesKeyExist(V: String):Boolean;
begin
//{$IFNDEF Darwin}
 OpenReg(ExtractKey(V));
 Result:=FRegistry.ValueExists(V);
 FRegistry.Free;
{{$ENDIF}
{$IFDEF Darwin}
 Result:=False;
 CFPreferencesGetAppIntegerValue(CFStr(PChar(ExtractKey(V)))
                                ,kCFPreferencesCurrentApplication
                                ,Result);
{$ENDIF}}
end;

{-------------------------------------------------------------------------------
Function to save a string to the registry
-------------------------------------------------------------------------------}
procedure TGJHRegistry.SetRegValS(V: String;D: String);
begin
//{$IFNDEF Darwin}
 OpenReg(ExtractKey(V));
 FRegistry.WriteString(V,D);
 FRegistry.Free;
{{$ENDIF}
{$IFDEF Darwin}
 SetMacValue(V,D);
{$ENDIF}}
end;

{-------------------------------------------------------------------------------
Function to save an array to the registry
-------------------------------------------------------------------------------}
//{$IFNDEF Darwin}
procedure TGJHRegistry.SetRegValA(V: String;var D: array of Byte);
begin
 OpenReg(ExtractKey(V));
 FRegistry.WriteBinaryData(V,D,SizeOf(D));
 FRegistry.Free;
end;
//{$ENDIF}

{-------------------------------------------------------------------------------
Function to save an integer to the registry
-------------------------------------------------------------------------------}
procedure TGJHRegistry.SetRegValI(V: String;D: Cardinal);
begin
//{$IFNDEF Darwin}
 OpenReg(ExtractKey(V));
 FRegistry.WriteInteger(V,D);
 FRegistry.Free;
{{$ENDIF}
{$IFDEF Darwin}
 SetMacValue(V,IntToStr(D));
{$ENDIF}}
end;

{-------------------------------------------------------------------------------
Function to save a boolean to the registry
-------------------------------------------------------------------------------}
procedure TGJHRegistry.SetRegValB(V: String;D: Boolean);
begin
//{$IFNDEF Darwin}
 OpenReg(ExtractKey(V));
 FRegistry.WriteBool(V,D);
 FRegistry.Free;
{{$ENDIF}
{$IFDEF Darwin}
 if D then SetMacValue(V,'1') else SetMacValue(V,'0');
{$ENDIF}}
end;

{-------------------------------------------------------------------------------
Function to extract key part of string
-------------------------------------------------------------------------------}
function TGJHRegistry.ExtractKey(var V: String):String;
begin
 Result:='';
 if Pos('\',V)>0 then
 begin
  Result:=Copy(V,1,Pos('\',V)-1);
  V:=Copy(V,Pos('\',V)+1);
 end;
end;

{-------------------------------------------------------------------------------
Procedure to write a Mac preference
-------------------------------------------------------------------------------}
{{$IFDEF Darwin}
procedure TGJHRegistry.SetMacValue(V,X: String);
var
 ItemVal : CFPropertyListRef;
begin
 ItemVal:=CFStringCreateWithPascalString(kCFAllocatorDefault
                                        ,X
                                        ,kCFStringEncodingUTF8);
 CFPreferencesSetAppValue(CFStr(PChar(ExtractKey(V))),
                          ItemVal,
                          kCFPreferencesCurrentApplication);
 // write out the preference data
 CFPreferencesAppSynchronize(kCFPreferencesCurrentApplication);
end;
{$ENDIF} }

end.
