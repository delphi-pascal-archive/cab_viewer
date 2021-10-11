program CABDemo;

uses
  Forms,
  FrmMain in 'FrmMain.pas' {Form1},
  CABAPI in 'CABAPI.pas',
  CABFile in 'CABFile.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
