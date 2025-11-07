unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, u_clock_impl;

type
  TForm1 = class(TForm)
    var
      pnlLeft: TPanel;
    procedure pnlLeftClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  if FLeftChessClock.IsRunning then
  begin
    FLeftChessClock.Pause;
  end;

  if FRightChessClock.IsRunning then
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
  if FRightChessClock.IsRunning then
  begin
    FRightChessClock.Pause;
  end;

  if FLeftChessClock.IsRunning then
  begin
    FLeftChessClock.Resume;
  end
  else
  begin
    FLeftChessClock.Start;
  end;
end;

end.

