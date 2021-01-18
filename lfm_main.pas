unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  bc_strings, { provides a string class, which implements operations on a string. }
  blcksock;   { provides tcp-communication }

type
  { TfrmTcp }
  TfrmTcp = class(TForm)
    btnSend: TButton;
    edtAdress: TLabeledEdit;
    edtPort: TLabeledEdit;
    edtCommand: TLabeledEdit;
    Memo1: TMemo;
    procedure btnSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function DoListCmd(const aCmd: string): ptrint;
    function DoRecvCmd(const aCmd: string): ptrint;
    function DoSendCmd(const aCmd: string): ptrint;
    function DoInfoCmd(const aCmd: string): ptrint;
    function DoEchoCmd(const aCmd: string): ptrint;
    function DoQuitCmd(const aCmd: string): ptrint;
  protected
    fSrvSocket: TTCPBlockSocket;
    fCliSocket: TTCPBlockSocket;
  public

  end;

var
  frmTcp: TfrmTcp;

implementation

{$R *.lfm}

{ TfrmTcp }

procedure TfrmTcp.btnSendClick(Sender: TObject);
var
  Cmd: string;
  HR_RESULT: ptrint;
begin
  Memo1.Lines.Clear;
  fCliSocket:= TTCPBlockSocket.Create;
  try
    fCliSocket.Bind(edtAdress.Text,edtPort.Text);  // this is nescesary!
    fCliSocket.Connect(edtAdress.Text,edtPort.Text);
    if fCliSocket.CanWrite(1000) then begin
      Cmd:= edtCommand.Text;
      case StringWorkshop.StrCaseLen(Cmd,['.list','.recv','.send','.info','.echo','.quit'],5) of
        0: begin { .list }
             HR_RESULT:= DoListCmd(Cmd);
           end;
        1: begin { .recv }
             HR_RESULT:= DoRecvCmd(Cmd);
           end;
        2: begin { .send }
             HR_RESULT:= DoSendCmd(Cmd);
           end;
        3: begin { .info }
             HR_RESULT:= DoInfoCmd(Cmd);
           end;
        4: begin { .echo }
             HR_RESULT:= DoEchoCmd(Cmd);
           end;
        5: begin { .quit }
             HR_RESULT:= DoQuitCmd(Cmd);
           end;
      end; { case cmd }
//      Sleep(100); // give the echo server a little time to process
//      if fCliSocket.CanRead(1000) then S:= fCliSocket.RecvPacket(1000);
      if fCliSocket.CanRead(1000) then begin
        Memo1.Lines.Add('we can read');
      end;
      Memo1.Lines.Add(edtCommand.Text+' Result = '+HR_RESULT.ToString);
      fCliSocket.CloseSocket;
    end;
  finally FreeAndNil(fCliSocket); end;
end;

procedure TfrmTcp.FormCreate(Sender: TObject);
begin
end;

procedure TfrmTcp.FormDestroy(Sender: TObject);
begin
end;

{ ask the server for a list of commands it undestands }
function TfrmTcp.DoListCmd(const aCmd: string): ptrint;
var
  Received_Data: string;
begin
  Result:= -1;
  fCliSocket.SendString(aCmd); { send the .list command to server }
  if ((fCliSocket.LastError = 0) and
       fCliSocket.CanRead(1000)) then begin
    Received_Data:= fCliSocket.RecvPacket(1000); { read the received data }
    if fCliSocket.LastError = 0 then begin
      Memo1.Lines.Text:= Received_Data;
      Result:= fCliSocket.LastError;
    end else Memo1.Lines.Add(' * * * ERROR! '+fCliSocket.LastErrorDesc);
  end;
end;

{ protocol for sending a stream:
1) send ".recv" command.
2) read "OK" to send from server
3) Send stream to server
4) read "OK" received from server
}
function TfrmTcp.DoRecvCmd(const aCmd: string): ptrint;
var
  Res: string;
  Strm: TMemoryStream;
begin
  Result:= -1;
  Strm:= TMemoryStream.Create;
  try
    Strm.LoadFromFile('/home/bc/share/bom_worksheet.pas');
    Strm.Position:= 0; // Strm.Seek(0,soFromBeginning);
    fCliSocket.SendString(aCmd);            { send the .recv command to server }
    if fCliSocket.LastError <> 0 then exit; { stop on errors }
    Res:= fCliSocket.RecvPacket(1000);      { read the "HandShake" ~ 'OK' }
    if fCliSocket.LastError = 0 then begin  { stop on errors }
      if fCliSocket.CanWrite(1000) then fCliSocket.SendStream(Strm); { send the stream }
      if fCliSocket.LastError = 0 then begin
        Result:= 0;
        if fCliSocket.CanRead(1000) then ;
      end;
    end;
  finally FreeAndNil(Strm); end;
end;

function TfrmTcp.DoSendCmd(const aCmd: string): ptrint;
var
  Strm: TMemoryStream;
begin
  Result:= -1;
  fCliSocket.SendString(aCmd); { send the .send command to server }
  if ((fCliSocket.LastError = 0) and
       fCliSocket.CanRead(1000)) then begin
    Memo1.Lines.Add('we can read');
    Strm:= TMemoryStream.Create;
    try
      Memo1.Lines.Add('stream created');
      Strm.Seek(0,soFromBeginning);
      fCliSocket.ResetLastError;
      fCliSocket.RecvStream(Strm,1000); // 1 second ?!?
      if fCliSocket.LastError = 0 then begin
        Memo1.Lines.Add('no errors');
        Strm.Seek(0,soFromBeginning);
        Strm.SaveToFile('/home/bc/ftp_share/stream.txt');
        Memo1.Lines.Add('File saved to: /home/bc/ftp_share/stream.txt');
        Result:= fCliSocket.LastError;
      end else Memo1.Lines.Add('Last Error: '+fCliSocket.LastErrorDesc);
    finally FreeAndNil(Strm); end;
  end;
end;

function TfrmTcp.DoInfoCmd(const aCmd: string): ptrint;
var
  Received_Data: string;
begin
  Result:= -1;
  fCliSocket.SendString(aCmd); { send the .info command to server }
  if ((fCliSocket.LastError = 0) and
       fCliSocket.CanRead(1000)) then begin
    Received_Data:= fCliSocket.RecvPacket(1000); { read the received data }
    if fCliSocket.LastError = 0 then begin
      Memo1.Lines.Add('Info: '+Received_Data);
      Result:= fCliSocket.LastError;
    end else Memo1.Lines.Add(' * * * ERROR! '+fCliSocket.LastErrorDesc);
  end;
end;

function TfrmTcp.DoEchoCmd(const aCmd: string): ptrint;
var
  Received_Data: string;
begin
  Result:= -1;
  fCliSocket.SendString(aCmd); { send the .echo command to server }
  if ((fCliSocket.LastError = 0) and
       fCliSocket.CanRead(1000)) then begin
    Received_Data:= fCliSocket.RecvPacket(1000); { read the received data }
    if fCliSocket.LastError = 0 then begin
      Memo1.Lines.Add('Echo: '+Received_Data);
      Result:= fCliSocket.LastError;
    end else Memo1.Lines.Add(' * * * ERROR! '+fCliSocket.LastErrorDesc);
  end;
end;

function TfrmTcp.DoQuitCmd(const aCmd: string): ptrint;
var
  Received_Data: string;
begin
  Result:= -1;
  fCliSocket.SendString(aCmd); { send the .quit command to server }
  if ((fCliSocket.LastError = 0) and
       fCliSocket.CanRead(1000)) then begin
    Received_Data:= fCliSocket.RecvPacket(1000); { read the goodbye message }
    if fCliSocket.LastError = 0 then begin
      Memo1.Lines.Add('Quit message: '+Received_Data);
      Result:= fCliSocket.LastError;
    end else Memo1.Lines.Add(' * * * ERROR! '+fCliSocket.LastErrorDesc);
  end;
end;

end.

