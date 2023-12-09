program XMPPServer;

uses
  Vcl.Forms,
  uThreadPrimario in 'uThreadPrimario.pas' {Form1},
  uXMLServerParser in 'uXMLServerParser.pas',
  uXMPPServerHelper in 'uXMPPServerHelper.pas',
  XMPPServerSASLCrypt in 'XMPPServerSASLCrypt.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
