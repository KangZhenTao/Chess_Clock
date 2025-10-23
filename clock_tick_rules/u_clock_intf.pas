unit u_clock_intf;

interface

type
  IClock = interface
    ['{1C132F5A-3FE4-4792-9979-64DE76C40960}']

    // 设置时间
    procedure SetTime(Hour, Minute, Second: Word);

    // 重置时钟到初始状态
    procedure Reset;

    // 暂停时钟运行
    procedure Pause;

    // 继续时钟运行
    procedure Resume;

    // 获取当前时间
    procedure GetTime(out Hour, Minute, Second: Word);

    // 检查时钟是否在运行
    function IsRunning: Boolean;
  end;

implementation

end.
