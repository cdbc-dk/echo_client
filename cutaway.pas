
        0: begin { .list }
             (*
             fCliSocket.SendString(Cmd); { send the .list command to server }
             if ((fCliSocket.LastError = 0) and
                  fCliSocket.CanRead(1000)) then begin
               Received_Data:= fCliSocket.RecvPacket(1000); { read the received data }
               if fCliSocket.LastError = 0 then Memo1.Lines.Text:= Received_Data
               else Memo1.Lines.Add(' * * * ERROR! '+fCliSocket.LastErrorDesc);
             end; *)
             HR_RESULT:= DoListCmd(Cmd);
           end;












