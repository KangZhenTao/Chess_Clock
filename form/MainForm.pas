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
      mmoDebugMsg: TMemo;
    procedure pnlLeftClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrFreshContentTimer(Sender: TObject);
    var
      pnlRight: TPanel;
    procedure pnlRightClick(Sender: TObject);
  private
    procedure ChessClockInit(var AChessClock: TChessClock);
    var
      FRightChessClock: TChessClock;
      FLeftChessClock: TChessClock;
      cnt: Integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ChessClockInit(var AChessClock: TChessClock);
begin
  if not Assigned(AChessClock) then
   AChessClock := TChessClock.Create;
  AChessClock.SetPlusTime(30);
  AChessClock.SetCountdownTime(0, 30, 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ChessClockInit(FLeftChessClock);
  ChessClockInit(FRightChessClock)
end;

procedure TForm1.pnlLeftClick(Sender: TObject);
begin
  if FLeftChessClock.IsRunning then
  begin
    if not FLeftChessClock.IsPausing then
      FLeftChessClock.Pause;
  end;

  if FRightChessClock.IsRunning then
  begin
    if FRightChessClock.IsPausing then
      FRightChessClock.Resume;
  end
  else
  begin
    FRightChessClock.Start;
  end;
end;

procedure TForm1.pnlRightClick(Sender: TObject);
begin
  if FRightChessClock.IsRunning then
  begin
    if not FRightChessClock.IsPausing then
      FRightChessClock.Pause;
  end;

  if FLeftChessClock.IsRunning then
  begin
    if FLeftChessClock.IsPausing then
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
    Hours, Minutes, Seconds: Word;
  begin
    FLeftChessClock.GetRemainingTime(Hours, Minutes, Seconds);
    lblLeftTime.Caption := Format('%d:%d:%d', [Hours, Minutes, Seconds]);
    FRightChessClock.GetRemainingTime(Hours, Minutes, Seconds);
    lblRightTime.Caption := Format('%d:%d:%d', [Hours, Minutes, Seconds]);
  end;

  procedure UpdateDebugMsg();
  begin
    mmoDebugMsg.Lines.Add('Left : ' + FLeftChessClock.ShowDebugMsg());
    mmoDebugMsg.Lines.Add('Right : ' + FRightChessClock.ShowDebugMsg());
  end;

begin
  UpdateTimeShow();
  Inc(cnt);
  if cnt mod 100 = 0 then
  begin

    UpdateDebugMsg();
  end;
end;

end.

