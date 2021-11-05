program CryptoLibManager;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  UC.Delphi.Versions in 'UC.Delphi.Versions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
