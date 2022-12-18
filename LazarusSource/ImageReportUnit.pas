unit ImageReportUnit;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

 { TImageReportForm }

 TImageReportForm = class(TForm)
  Report: TMemo;
 private

 public

 end;

var
 ImageReportForm: TImageReportForm;

implementation

{$R *.lfm}

end.

