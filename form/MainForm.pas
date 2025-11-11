unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, u_clock_impl, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    var
      pnlLeft: TPanel;
      lblLeftTime: TLabel;
      lblRightTime: TLabel;
      tmrFreshContent: TTimer;
    procedure pnlLeftClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrFreshContentTimer(Sender: TObject);
    var
      pnlRight: TPanel;
    procedure pnlRightClick(Sender: TObject);
  private
    FRightChessClock: TChessClock;
    FLeftChessClock: TChessClock;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);

  procedure ChessClockInit(var AChessClock: TChessClock);
  begin
    AChessClock := TChessClock.Create;
    AChessClock.SetPlusTime(30);
    AChessClock.SetTimeInSecond := 30 * 60;
  end;

begin
  ChessClockInit(FLeftChessClock);
  ChessClockInit(FRightChessClock)
end;

procedure TForm1.pnlLeftClick(Sender: TObject);
begin
  if FLeftChessClock.IsRunning and not FLeftChessClock.IsPausing then
  begin
    FLeftChessClock.Pause;
  end;

  if FRightChessClock.IsRunning and FRightChessClock.IsPausing then
  begin
    FRightChessClock.Resume;
  end
  else
  begin
    FRightChessClock.Start;
  end;
end;

procedure TForm1.pnlRightClick(Sender: TObject);
begin
  if FRightChessClock.IsRunning and not FRightChessClock.IsPausing then
  begin
    FRightChessClock.Pause;
  end;

  if FLeftChessClock.IsRunning and FLeftChessClock.IsPausing then
  begin
    FLeftChessClock.Resume;
  end
  else
  begin
    FLeftChessClock.Start;
  end;
end;

procedure TForm1.tmrFreshContentTimer(Sender: TObject);
  procedure UpdateTimeShow();
  var
    Hours, Minutes, Seconds:Word;
  begin
    FLeftChessClock.GetRemainingTime(Hours, Minutes, Seconds);
    lblLeftTime.Caption := Format('%d:%d:%d',[Hours, Minutes, Seconds]);
    FRightChessClock.GetRemainingTime(Hours, Minutes, Seconds);
    lblRightTime.Caption := Format('%d:%d:%d',[Hours, Minutes, Seconds]);
  end;

begin
  UpdateTimeShow();
end;

end.

