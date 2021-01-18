 
How To Write A UDP Server?

Try next code on thread, or in any other case:

procedure TSomeThread.Execute;
var
  Sock:TUDPBlockSocket;
  size:integer;
  buf:string;
begin
  Sock:=TUDPBlockSocket.Create;
  try
    sock.bind('0.0.0.0','port');
    if sock.LastError0 then exit;
    while True do
      begin
        if terminated then break;
	buf := sock.RecvPacket(1000);
	if sock.lasterror=0 then
	  begin
  //        do something with data and prepare response data
            sock.SendString(Buf);
          end;
        sleep(1);
      end;
    sock.CloseSocket;
  finally
    sock.free;
  end;
end;
