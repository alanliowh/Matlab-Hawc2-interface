library TCPServer;

uses
  SysUtils,
  Classes,
  Forms,
  IdTCPServer,
  IdTCPClient,
  Dialogs;

{$R *.res}

const
  nInputs = 40;

type
  ts = array[0..255] of char;
  array_20 = array[1..nInputs] of single;
  Thelper = class
    IdTCPServer: TIdTCPServer;
    procedure IdTCPServer1Execute(AThread: TIdPeerThread);
    procedure IdTCPServer1Connect(AThread: TIdPeerThread);
    procedure IdTCPServer1DisConnect(AThread: TIdPeerThread);
  end;

var
  helper: Thelper;

  server_to_be_send: string = '';
  server_lastread: string = '';

  firsttimecall : boolean = true;

  inputvector_delay, outputvector_delay : array_20;
  time_old : single = -1.0;

procedure Thelper.IdTCPServer1Connect(AThread: TIdPeerThread);
begin
  try
    Write('Connect from '+AThread.Connection.Socket.Binding.IP+':'+IntToStr(AThread.Connection.Socket.Binding.Port));
    WriteLn(' (peer '+AThread.Connection.Socket.Binding.PeerIP+':'+IntToStr(AThread.Connection.Socket.Binding.PeerPort)+')');
  except
  end;
end;

procedure Thelper.IdTCPServer1DisConnect(AThread: TIdPeerThread);
begin
  try
    Write('Disconnect to '+AThread.Connection.Socket.Binding.IP+':'+IntToStr(AThread.Connection.Socket.Binding.Port));
    WriteLn(' (peer '+AThread.Connection.Socket.Binding.PeerIP+':'+IntToStr(AThread.Connection.Socket.Binding.PeerPort)+')');
  except
  end;
end;

procedure Thelper.IdTCPServer1Execute(AThread: TIdPeerThread);
var
  ii : Integer;
begin
  IdTCPServer.OnExecute := nil;
  while true do
  begin
    Application.ProcessMessages();
    if (server_to_be_send<>'') then
    begin
      AThread.Connection.WriteLn(server_to_be_send);
      server_to_be_send := '';
      server_lastread := AThread.Connection.ReadLn('*',5000);
      if server_lastread='' then
        for ii:=1 to nInputs do server_lastread := server_lastread + '0;';
    end;
  end;
end;

Procedure tcplink(na:integer;var inputvector : array_20;
                  nb:integer;var outputvector: array_20);stdcall;
var
  i : Integer;
  st : string;
begin

  // Ensure English locale decimal separator symbol
  DecimalSeparator := '.';

   if firsttimecall then
  begin
    firsttimecall := false;

    helper := Thelper.Create();
    helper.IdTCPServer := TIdTCPServer.Create(nil);
    helper.IdTCPServer.OnExecute := helper.IdTCPServer1Execute;
    helper.IdTCPServer.OnConnect := helper.IdTCPServer1Connect;
    helper.IdTCPServer.OnDisconnect := helper.IdTCPServer1DisConnect;
    helper.IdTCPServer.DefaultPort := 1239;
    try
      helper.IdTCPServer.Active := true;
      if helper.IdTCPServer.Active then
        Writeln('TCP/IP host ready, default port: '+IntToStr(helper.IdTCPServer.DefaultPort));
    except
      Writeln('*** Could not start TCP/IP server ***');
    end;
  end;

  st := IntToStr(nInputs)+';'; for i:=1 to nInputs do st := st + FloatToStr(inputvector[i])+';';
  server_lastread := '';
  server_to_be_send := st;

  while (server_lastread='') do Application.ProcessMessages();

  st := server_lastread;
  server_lastread := '';

  for i:=1 to nInputs do
  begin
    if Length(st) < 1 then
    begin
      outputvector[i] := 0;
    end
    else
    begin
      outputvector[i] := StrToFloat(copy(st,1,AnsiPos(';',st)-1));
      st := copy(st,AnsiPos(';',st)+1,MaxInt);
    end;
  end;
end;

// Only call tcplink when time has changed
Procedure tcplink_delay(na:integer;var inputvector : array_20;
                  nb:integer;var outputvector: array_20);stdcall;
var
  i : Integer;
begin
  if inputvector[1] > time_old then
  begin
    tcplink(na, inputvector_delay, nb, outputvector_delay);
    time_old := inputvector[1];
  end;

  for i :=1 to nInputs do inputvector_delay[i] := inputvector[i];
  for i :=1 to nInputs do outputvector[i] := outputvector_delay[i];

end;

Procedure tcplink_init(var string256:ts; length:integer);stdcall;
var
  init_str : string[255];

  onPos : Integer;
begin
  init_str:=strpas(string256);
  // Crop trailing blanks
  onPos := AnsiPos(' ', init_str);
  SetLength(init_str, onPos-1);

  if firsttimecall then
  begin
    firsttimecall := false;

    helper := Thelper.Create();
    helper.IdTCPServer := TIdTCPServer.Create(nil);
    helper.IdTCPServer.OnExecute := helper.IdTCPServer1Execute;
    helper.IdTCPServer.OnConnect := helper.IdTCPServer1Connect;
    helper.IdTCPServer.OnDisconnect := helper.IdTCPServer1DisConnect;
    helper.IdTCPServer.DefaultPort := StrToInt(init_str);

    try
      helper.IdTCPServer.Active := true;
      if helper.IdTCPServer.Active then
        Writeln('TCP/IP host ready, selected port: '+IntToStr(helper.IdTCPServer.DefaultPort));
    except
      Writeln('*** Could not start TCP/IP server ***');
    end;
  end;

end;

Procedure tcplink_delay_init(var string256:ts; length:integer);stdcall;

begin
  tcplink_init(string256, length);
end;

exports
  tcplink, tcplink_delay, tcplink_init, tcplink_delay_init;

begin
end.
