unit u_clock_intf;

interface

type
  IClock = interface
    ['{1C132F5A-3FE4-4792-9979-64DE76C40960}']

    // ����ʱ��
    procedure SetTime(Hour, Minute, Second: Word);

    // ����ʱ�ӵ���ʼ״̬
    procedure Reset;

    // ��ͣʱ������
    procedure Pause;

    // ����ʱ������
    procedure Resume;

    // ��ȡ��ǰʱ��
    procedure GetTime(out Hour, Minute, Second: Word);

    // ���ʱ���Ƿ�������
    function IsRunning: Boolean;
  end;

implementation

end.
