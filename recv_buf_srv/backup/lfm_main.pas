unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, FileCtrl, StdCtrls,
  ExtCtrls, bc_udpsrv, bc_msgqueue,bc_datetime;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStartSrv: TButton;
    btnSend: TButton;
    btnStopSrv: TButton;
    IdleTimer1: TIdleTimer;
    memSrv: TMemo;
    memcli: TMemo;
    procedure btnStartSrvClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnStopSrvClick(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
  private
    fUdpSrv: TUdpServerDaemon;
    procedure DataInQueue(Sender: TObject);
    procedure CmdEvent(aPeerThread: TUdpWorkerThrd;const aCmd: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnStartSrvClick(Sender: TObject);
begin
  MsgQ.OnDataInQueue:= @DataInQueue;
  fUdpSrv:= TUdpServerDaemon.Create(Handle,@CmdEvent);
end;

procedure TForm1.btnSendClick(Sender: TObject);
begin
//  IdleTimer1Timer(Self);
end;

procedure TForm1.btnStopSrvClick(Sender: TObject);
begin
  fUdpSrv.Terminate;
  fUdpSrv.WaitFor;
end;

procedure TForm1.IdleTimer1Timer(Sender: TObject);
const Fmt = 'Msg: %d, port: %d, time: %s, sparam: %s';
var Msg: TbcMessage;
begin
  if not MsgQ.IsEmpty then begin
    if MsgQ.Peek.Handle = Self.Handle then begin
      Msg:= MsgQ.DeQueue;
      memSrv.Lines.Add(format(Fmt,[Msg.Msg,Msg.WParam,bcIntTimeToStr(Msg.LParam),Msg.SParam]));
      Msg.Free;
    end;
  end;
end;

procedure TForm1.DataInQueue(Sender: TObject);
begin
  if not IdleTimer1.Enabled then IdleTimer1.Enabled:= true;
end;

procedure TForm1.CmdEvent(aPeerThread: TUdpWorkerThrd; const aCmd: string);
begin
  // todo
end;

end.

