unit AboutUnit;

{$MODE Delphi}

interface

uses
  {Windows,}Messages,SysUtils,Variants,Classes,
  Controls,Forms,Dialogs,ExtCtrls,StdCtrls;

type
  TAboutForm = class(TForm)
    CreditsPanel: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label14: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

end.
