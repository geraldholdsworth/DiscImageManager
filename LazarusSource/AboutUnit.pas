unit AboutUnit;

//This project is now covered by the GNU GPL v3 licence

{$MODE objFPC}{$H+}

interface

uses
  SysUtils,Variants,Classes,Controls,Forms,Dialogs,ExtCtrls,StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CreditsPanel: TPanel;
    Image1: TImage;
    Label15: TLabel;
    Label16: TLabel;
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
