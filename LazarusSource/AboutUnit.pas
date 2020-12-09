unit AboutUnit;

{$MODE objFPC}

interface

uses
  SysUtils,Variants,Classes,Controls,Forms,Dialogs,ExtCtrls,StdCtrls;

type
  TAboutForm = class(TForm)
    CreditsPanel: TPanel;
    lb_Title: TLabel;
    Label8: TLabel;
    lb_Version: TLabel;
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
