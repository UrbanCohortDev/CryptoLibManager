program CryptoLibManager;
uses
  Vcl.Forms,
  UC.Delphi.Versions in 'UC.Delphi.Versions.pas',
  MainForm2 in 'MainForm2.pas' {MainFormNew},
  CryptoLibProcessor in 'CryptoLibProcessor.pas';

{$R *.res}
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainFormNew, MainFormNew);
  Application.Run;
end.
