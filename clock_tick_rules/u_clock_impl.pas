unit u_clock_impl;

interface

uses
  u_clock_intf;

type
  TChessClock = class(TInterfacedObject, IClock)
  private
    FRemainingTimeInSecond:Cardinal;
  public

    {$REGION 'IClock'}
    /// <summary>
    /// 设置倒计时时间
    /// </summary>
    /// <param name="Hours">倒计时小时数</param>
    /// <param name="Minutes">倒计时分钟数</param>
    /// <param name="Seconds">倒计时秒数</param>
    /// <returns>错误码，默认为0，无错误</returns>
    function SetCountdownTime(Hours, Minutes, Seconds: Word): TErrorCode;

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
    constructor Create;
    destructor Destroy;override;
  end;

implementation

{ TChessClock }

constructor TChessClock.Create();
begin
  inherited;
  FRemainingTimeInSecond := 60 * 60 * 1;
end;

destructor TChessClock.Destroy;
begin

  inherited;
end;

function TChessClock.GetRemainingTime(out Hours, Minutes,
  Seconds: Word): TErrorCode;
begin
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

end;

procedure TChessClock.Resume;
begin

end;

function TChessClock.SetCountdownTime(Hours, Minutes,
  Seconds: Word): TErrorCode;
begin
  Result := TErrorCode.ecDummy;
end;

end.

