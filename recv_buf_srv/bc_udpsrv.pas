

                   {**************************************$
                   $  Unit name : bc_udpsrv.pas           $
                   $  Copyright : (C)2020 cdbc.dk         $
                   $  Programmer: Benny Christensen       $
                   $  Created   : 2020-03-27 /bc          $
                   $  Updated   : 2020-03-27 /bc          $
                   $ ************************************ $
                   $  Purpose   :                         $
                   $  One unit that implements an udp-    $
                   $  server.                             $
                   $**************************************}

unit bc_udpsrv;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,
  blcksock,
  synsock,
  bc_msgqueue,
  bc_datetime;

type
  TUdpWorkerThrd = class; { forward declaration }
  { *** TCmdEvent *** }
  TCmdEvent = procedure(aPeerThread: TUdpWorkerThrd;const aCmd: string) of object;
  { *** TUdpServerDaemon *** }

  { TUdpServerDaemon }

  TUdpServerDaemon = class(TThread)
  private
    fSock: TUDPBlockSocket;
    fHandle: ptrint;
    fOnCmdEvent: TCmdEvent;
  public
    Constructor Create; overload;
    constructor Create(const aHandle: ptrint;aCmdHandler: TCmdEvent); overload;
    Destructor Destroy; override;
    procedure Terminate;
    procedure Execute; override;
    property OnCmdReceived: TCmdEvent read fOnCmdEvent write fOnCmdEvent;
  end;

  { *** TUdpWorkerThrd *** }
  TUdpWorkerThrd = class(TThread)
  private
    fSock:TUDPBlockSocket;
    fSockHandle: TSocket;
    fHandle: THandle;
    fOnCmdEvent: TCmdEvent;
  protected
    procedure DoOnCmdReceived(const aCmd: string);
  public
    Constructor Create(hSock: TSocket;const aHandle: ptrint;aCmdHandler: TCmdEvent);
    procedure Execute; override;
    property Sock: TUDPBlockSocket read fSock write fSock;
    property OnCmdReceived: TCmdEvent read fOnCmdEvent write fOnCmdEvent;
  end;




implementation
var Time: IIsoTime;
{ *** TUdpServerDaemon *** }
constructor TUdpServerDaemon.Create;
begin
  inherited Create(true);
  fSock:= TUDPBlockSocket.create;
  FreeOnTerminate:= true;
  Time.AsTime:= now;
  MsgQ.EnQueue(TbcMessage.Create(0,                                // ahandle
                                 LM_CREATE,                        // amsg
                                 8723,                             // wparam
                                 Time.AsInteger,                   // lparam
                                 'Udp server daemon is created')); // sparam
  Start;
end;

constructor TUdpServerDaemon.Create(const aHandle: ptrint;aCmdHandler: TCmdEvent);
begin
  inherited Create(true);
  fSock:= TUDPBlockSocket.create;
  fHandle:= aHandle;
  fOnCmdEvent:= aCmdHandler;
  FreeOnTerminate:= true;
  Time.AsTime:= now;
  MsgQ.EnQueue(TbcMessage.Create(fHandle,                          // ahandle
                                 LM_CREATE,                        // amsg
                                 8723,                             // wparam
                                 Time.AsInteger,                   // lparam
                                 'Udp server daemon is created')); // sparam
  Start;
end;

destructor TUdpServerDaemon.Destroy;
begin
  fSock.Free;
  Time.AsTime:= now;
  MsgQ.EnQueue(TbcMessage.Create(fHandle,                            // ahandle
                                 LM_DESTROY,                         // amsg
                                 8723,                               // wparam
                                 Time.AsInteger,                     // lparam
                                 'Udp server daemon is destroyed')); // sparam
  inherited Destroy;
end;

procedure TUdpServerDaemon.Terminate;
begin
  // todo notify worker
  inherited Terminate;
end;

procedure TUdpServerDaemon.Execute;
var ClientSockHandle: TSocket;
begin
  with fSock do begin
    CreateSocket; // ? nescecary...
//    SetLinger(true,10000); // only for tcp sockets
    Bind('0.0.0.0','8723');
    if fSock.LastError <> 0 then exit;
    Listen;
    Time.AsTime:= now;
    MsgQ.EnQueue(TbcMessage.Create(fHandle,                            // ahandle
                                   LM_LISTEN,                          // amsg
                                   8723,                               // wparam
                                   Time.AsInteger,                     // lparam
                                   'Udp server daemon is listening')); // sparam
    while not Terminated do try
      if CanRead(1000) then begin
        ClientSockHandle:= Accept;
        if LastError = 0 then begin
          TUdpWorkerThrd.Create(ClientSockHandle,fHandle,fOnCmdEvent);
          Time.AsTime:= now;
          MsgQ.EnQueue(TbcMessage.Create(fHandle,                                 // ahandle
                                         LM_ACCEPT,                               // amsg
                                         8723,                                    // wparam
                                         Time.AsInteger,                          // lparam
                                         'Udp server daemon accepted a client')); // sparam
        end;
      end;

    except on E:Exception do
      ;
    end;
  end;
end;

{ *** TUdpWorkerThrd *** }
procedure TUdpWorkerThrd.DoOnCmdReceived(const aCmd: string);
begin
  if assigned(fOnCmdEvent) then fOnCmdEvent(Self,aCmd);
end;

constructor TUdpWorkerThrd.Create(hSock: TSocket;const aHandle: ptrint;aCmdHandler: TCmdEvent);
begin
  inherited Create(true);
  fSockHandle:= hSock;
  fHandle:= aHandle;
  fOnCmdEvent:= aCmdHandler;
  FreeOnTerminate:= true;
  Time.AsTime:= now;
  MsgQ.EnQueue(TbcMessage.Create(fHandle,                   // ahandle
                                 LM_CREATE,                 // amsg
                                 8723,                      // wparam
                                 Time.AsInteger,            // lparam
                                 'Udp worker is created')); // sparam
  Start;
end;

procedure TUdpWorkerThrd.Execute;
var
  S: string;
begin
  fSock:= TUDPBlockSocket.create;
  try
    fSock.Socket:= fSockHandle;
    fSock.GetSins;
    with fSock do begin
      while not Terminated do try
        S:= RecvPacket(1000);
        if lastError <> 0 then break;
        Time.AsTime:= now;
        MsgQ.EnQueue(TbcMessage.Create(fHandle,                           // ahandle
                                       LM_WORKING,                        // amsg
                                       8723,                              // wparam
                                       Time.AsInteger,                    // lparam
                                       'Udp worker is processing data')); // sparam
        DoOnCmdReceived(S);
        SendString(Time.AsString+' ['+S+']');
        if lastError <> 0 then break;
      except on E:Exception do
        //nothing...
      end;
    end;
  finally fSock.Free; end;
  Time.AsTime:= now;
  MsgQ.EnQueue(TbcMessage.Create(fHandle,                           // ahandle
                                 LM_DONE,                           // amsg
                                 8723,                              // wparam
                                 Time.AsInteger,                    // lparam
                                 'Udp worker is done!'));           // sparam
end;
initialization
  Time:= TIsoTime.Create(now);
finalization
  Time:= nil;
end.

