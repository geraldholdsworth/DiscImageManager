unit ErrorLogUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

 { TErrorLogForm }

 TErrorLogForm = class(TForm)
  ErrorLog: TMemo;
 private

 public

 end;

var
 ErrorLogForm: TErrorLogForm;

implementation

{$R *.lfm}

end.
