unit u_clock_impl;

interface

uses
  u_clock_intf, Classes;

type
  TChessClock = class(TInterfacedObject, ICountDownClock)
  private
    FPlusSecondePerStep: Cardinal;
    FSetTimeInSecond: Cardinal;
    FRemainingTimeInSecond: Cardinal;
    procedure Process();
    type InnerThread = class(TThread)

    end;
  published
    property SetTimeInSecond: Cardinal read FSetTimeInSecond write FSetTimeInSecond;
    property RemainingTimeInSecond: Cardinal read FRemainingTimeInSecond write FRemainingTimeInSecond;
  public
    {$REGION 'ICountDownClock'}
    /// <summary>
    /// 设置倒计时时间
    /// </summary>
    /// <param name="Hours">倒计时小时数</param>
    /// <param name="Minutes">倒计时分钟数</param>
    /// <param name="Seconds">倒计时秒数</param>
    /// <returns>错误码，默认为0，无错误</returns>
    function SetCountdownTime(const Hours, Minutes, Seconds: Word): TErrorCode;

    /// <summary>
    /// 重置时钟到初始状态
    /// </summary>
    procedure Reset;

    /// <summary>
    /// 暂停时钟运行
    /// </summary>
    procedure Pause;

    /// <summary>
    /// 继续时钟运行
    /// </summary>
    procedure Resume;

    /// <summary>
    /// 获取剩余时间
    /// </summary>
    /// <param name="Hours">倒计时小时数</param>
    /// <param name="Minutes">倒计时分钟数</param>
    /// <param name="Seconds">倒计时秒数</param>
    /// <returns>错误码，默认为0，无错误</returns>
    function GetRemainingTime(out Hours, Minutes, Seconds: Word): TErrorCode;

    /// <summary>
    /// 检查时钟是否在运行
    /// </summary>
    function IsRunning(out Running: Boolean): TErrorCode;
    {$ENDREGION}

    /// <summary>
    /// 多一步棋所增加时间
    /// </summary>
    /// <param name="Seconds">秒数</param>
    /// <returns>错误码，默认为0，无错误</returns>
    function SetPlusTime(const Seconds: Cardinal): TErrorCode;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TChessClock }

function TChessClock.SetPlusTime(const Seconds: Cardinal): TErrorCode;
begin
  FPlusSecondePerStep := Seconds;
  Result := TErrorCode.ecDummy;
end;

procedure TChessClock.Process;
begin

end;

constructor TChessClock.Create();
begin
  inherited;
  FSetTimeInSecond := 0;
  FRemainingTimeInSecond := 0;
end;

destructor TChessClock.Destroy;
begin

  inherited;
end;

{$REGION 'ICountDownClock'}
function TChessClock.GetRemainingTime(out Hours, Minutes, Seconds: Word): TErrorCode;
begin
  Hours := RemainingTimeInSecond div 3600;
  Minutes :=  (RemainingTimeInSecond mod 3600) div 60;
  Seconds := RemainingTimeInSecond mod 60;
  Result := TErrorCode.ecDummy;
end;

function TChessClock.IsRunning(out Running: Boolean): TErrorCode;
begin

  Result := TErrorCode.ecDummy;
end;

procedure TChessClock.Pause;
begin

end;

procedure TChessClock.Reset;
begin
  RemainingTimeInSecond := SetTimeInSecond
end;

procedure TChessClock.Resume;
begin

end;

function TChessClock.SetCountdownTime(const Hours, Minutes, Seconds: Word): TErrorCode;
begin
  SetTimeInSecond := Hours * 3600 + Minutes * 60 + Seconds;
  Result := TErrorCode.ecDummy;
end;
{$ENDREGION}

end.

