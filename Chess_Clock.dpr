program Proj_001_Test;

uses
  Vcl.Forms,
  MainForm in 'form\MainForm.pas' {Form1},
  u_clock_intf in 'clock_tick_rules\u_clock_intf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
