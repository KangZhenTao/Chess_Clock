unit u_clock_intf;

interface

type
  TErrorCode = (
    ecSuccess = 0,
    ecInvalidParameter,
    ecNotInitialized,
    // ... 其他错误码
    ecDummy = $7fffffff
  );

  ICountDownClock = interface
    ['{1C132F5A-3FE4-4792-9979-64DE76C40960}']

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
  end;

implementation

end.

