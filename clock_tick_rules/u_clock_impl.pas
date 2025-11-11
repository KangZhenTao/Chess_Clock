unit u_clock_impl;

interface

uses
  u_clock_intf, Classes, SyncObjs;

type
  TChessClock = class(TInterfacedObject, ICountDownClock)
  private
    type
      TInnerThread = class(TThread)
      private
        FOwner: TChessClock;
      protected
        procedure Execute; override;
      public
        constructor Create(AOwner: TChessClock);
      end;
    var
      FPlusSecondePerStep: Cardinal;
      FSetTimeInSecond: Cardinal;
      FRemainingTimeInSecond: Cardinal;
      FIsRunning: Boolean;
      FIsPaused: Boolean;
      FLastUpdateTime: TDateTime;
      FLock: TCriticalSection;
      FInnerThread: TInnerThread;
    procedure Process();
    procedure UpdateRemainingTime;
    function GetCurrentRemainingTime: Cardinal;
  public
    function ShowDebugMsg(): string;

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

    procedure Start;

    procedure Stop;

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
    function IsRunning(): Boolean;

    /// <summary>
    /// 检查时钟是否在运行
    /// </summary>
    function IsPausing(): Boolean;
    {$ENDREGION}

    /// <summary>
    /// 多一步棋所增加时间
    /// </summary>
    /// <param name="Seconds">秒数</param>
    /// <returns>错误码，默认为0，无错误</returns>
    function SetPlusTime(const Seconds: Cardinal): TErrorCode;

    // 额外方法：添加时间（用于加时）
    function AddTime(const Seconds: Cardinal): TErrorCode;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Sysutils, DateUtils;
{ TChessClock }

function TChessClock.SetPlusTime(const Seconds: Cardinal): TErrorCode;
begin
  FLock.Enter;
  try
    FPlusSecondePerStep := Seconds;
    Result := TErrorCode.ecSuccess;
  finally
    FLock.Leave;
  end;
end;

function TChessClock.ShowDebugMsg(): string;
begin
  Result := Format('%s %s %s %s %s %s ',
    [IntToStr(FPlusSecondePerStep),
      IntToStr(FSetTimeInSecond),
      IntToStr(FRemainingTimeInSecond),
      BoolToStr(FIsRunning, True),
      BoolToStr(FIsPaused, True),
      DateToStr(FLastUpdateTime)]);
end;

procedure TChessClock.Start;
begin
  FLock.Enter;
  try
    if not FIsRunning then
    begin
      FIsRunning := True;
      FIsPaused := False;
      FLastUpdateTime := Now;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TChessClock.Stop;
begin
  FLock.Enter;
  try
    FIsRunning := False;
    FIsPaused := False;
  finally
    FLock.Leave;
  end;
end;

procedure TChessClock.UpdateRemainingTime;
var
  CurrentTime: TDateTime;
  ElapsedSeconds: Integer;
begin
  if FIsRunning and not FIsPaused then
  begin
    CurrentTime := Now;
    ElapsedSeconds := SecondsBetween(CurrentTime, FLastUpdateTime);

    if ElapsedSeconds > 0 then
    begin
      if Cardinal(ElapsedSeconds) >= FRemainingTimeInSecond then
      begin
        FRemainingTimeInSecond := 0;
        FIsRunning := False;
      end
      else
      begin
        Dec(FRemainingTimeInSecond, ElapsedSeconds);
      end;

      FLastUpdateTime := CurrentTime;
    end;
  end;
end;

procedure TChessClock.Process;
begin

end;

function TChessClock.AddTime(const Seconds: Cardinal): TErrorCode;
begin
  FLock.Enter;
  try
    Inc(FRemainingTimeInSecond, Seconds);
    Result := TErrorCode.ecSuccess;
  finally
    FLock.Leave;
  end;
end;

constructor TChessClock.Create();
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FSetTimeInSecond := 0;
  FRemainingTimeInSecond := 0;
  FIsRunning := False;
  FIsPaused := False;
  FPlusSecondePerStep := 0;
  FLastUpdateTime := Now;
  FInnerThread := TInnerThread.Create(Self);
end;

destructor TChessClock.Destroy;
begin
  Stop;
  if Assigned(FInnerThread) then
  begin
    FInnerThread.Terminate;
    FInnerThread.WaitFor;
    FreeAndNil(FInnerThread);
  end;
  FreeAndNil(FLock);
  inherited Destroy;
end;

{$REGION 'ICountDownClock'}
function TChessClock.GetCurrentRemainingTime: Cardinal;
begin
  // 如果时钟正在运行且未暂停，需要实时计算剩余时间
  if FIsRunning and not FIsPaused then
  begin
    UpdateRemainingTime;
  end;

  Result := FRemainingTimeInSecond;
end;

function TChessClock.GetRemainingTime(out Hours, Minutes, Seconds: Word): TErrorCode;
var
  CurrentRemaining: Cardinal;
begin
  FLock.Enter;
  try
    CurrentRemaining := GetCurrentRemainingTime;

    Hours := CurrentRemaining div 3600;
    Minutes := (CurrentRemaining mod 3600) div 60;
    Seconds := CurrentRemaining mod 60;
    Result := TErrorCode.ecSuccess;
  finally
    FLock.Leave;
  end;
end;

function TChessClock.IsPausing: Boolean;
begin
  FLock.Enter;
  try
    Result := FIsPaused;
  finally
    FLock.Leave;
  end;
end;

function TChessClock.IsRunning(): Boolean;
begin
  FLock.Enter;
  try
    Result := FIsRunning;
  finally
    FLock.Leave;
  end;
end;

procedure TChessClock.Pause;
begin
  FLock.Enter;
  try
    if FIsRunning and not FIsPaused then
    begin
      UpdateRemainingTime;
      FIsPaused := True;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TChessClock.Reset;
begin
  FLock.Enter;
  try
    FIsRunning := False;
    FIsPaused := False;
    FRemainingTimeInSecond := FSetTimeInSecond;
    FLastUpdateTime := Now;
  finally
    FLock.Leave;
  end;

end;

procedure TChessClock.Resume;
begin
  FLock.Enter;
  try
    if FIsRunning and FIsPaused then
    begin
      FLastUpdateTime := Now;
      FIsPaused := False;
    end;
  finally
    FLock.Leave;
  end;
end;

function TChessClock.SetCountdownTime(const Hours, Minutes, Seconds: Word): TErrorCode;
begin
  FLock.Enter;
  try
    FSetTimeInSecond := Hours * 3600 + Minutes * 60 + Seconds;
    FRemainingTimeInSecond := FSetTimeInSecond;
    Result := TErrorCode.ecSuccess;
  finally
    FLock.Leave;
  end;
end;
{$ENDREGION}

{ TChessClock.TInnerThread }

constructor TChessClock.TInnerThread.Create(AOwner: TChessClock);
begin
  inherited Create(False);
  FOwner := AOwner;
  FreeOnTerminate := False;
end;

procedure TChessClock.TInnerThread.Execute;
begin
  inherited;
  while not Terminated do
  begin
    if FOwner.FIsRunning and not FOwner.FIsPaused then
    begin
      FOwner.UpdateRemainingTime;

      // 检查时间是否用完
      if FOwner.GetCurrentRemainingTime <= 0 then
      begin
        FOwner.FIsRunning := False;
        Break;
      end;
    end;

    // 每100毫秒更新一次
    Sleep(1);
  end;
end;

end.

