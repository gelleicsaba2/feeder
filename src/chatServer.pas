program chatServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Classes,
  websocketserver,
  wsmessages,
  wsstream,
  wsutils,
  fileServicesUnit,
  serverSettingsUnit,
  SysUtils,
  ContentMapUnit;

type

  { TSocketHandler }

  TSocketHandler = class(TThreadedWebsocketHandler)
  private
    procedure ConnectionClosed(Sender: TObject);
    procedure MessageReceived(Sender: TObject);
    procedure Process(Sender: TObject; Command: string);
  public
    function Accept(const ARequest: TRequestData;
      const ResponseHeaders: TStrings): boolean; override;
    procedure DoHandleCommunication(ACommunication: TWebsocketCommunicator);
      override;
  end;

  TServiceSocketHandler = class(TThreadedWebsocketHandler)
  private
    procedure ConnectionClosed(Sender: TObject);
    procedure MessageReceived(Sender: TObject);
    procedure Process(Sender: TObject; Cmd: string);
  public
    function Accept(const ARequest: TRequestData;
      const ResponseHeaders: TStrings): boolean; override;
    procedure DoHandleCommunication(ACommunication: TWebsocketCommunicator);
      override;
  end;

  TServiceThread = class(TThread)
    procedure Execute; override;
  end;

var
  socket: TWebSocketServer;
  ServiceThread: TServiceThread;
  Settings: TServerSettings;
  FileServices: TFileServices;
  ContentMap: TContentMap;

  { TSocketHandler }

  function TSocketHandler.Accept(const ARequest: TRequestData;
  const ResponseHeaders: TStrings): boolean;
  begin
    Result := True;
  end;

  procedure TSocketHandler.DoHandleCommunication(
    ACommunication: TWebsocketCommunicator);
  var
    str: string;
  begin
    WriteLn('Connected to ', ACommunication.SocketStream.RemoteAddress.Address);
    ACommunication.OnReceiveMessage := @MessageReceived;
    ACommunication.OnClose := @ConnectionClosed;
    ACommunication.Open;
    while ACommunication.Open do
    begin
      sleep(100);
    end;
    //socket.Stop(True);
  end;

  procedure TSocketHandler.ConnectionClosed(Sender: TObject);
  var
    Comm: TWebsocketCommunicator;
  begin
    Comm := TWebsocketCommunicator(Sender);
    WriteLn('Connection to ', Comm.SocketStream.RemoteAddress.Address, ' closed');
  end;

  procedure TSocketHandler.MessageReceived(Sender: TObject);
  var
    Messages: TWebsocketMessageOwnerList;
    m: TWebsocketMessage;
    Comm: TWebsocketCommunicator;
  begin
    Comm := TWebsocketCommunicator(Sender);
    Messages := TWebsocketMessageOwnerList.Create(True);
    try
      Comm.GetUnprocessedMessages(Messages);
      for m in Messages do
        if m is TWebsocketStringMessage then
        begin
          //WriteLn('Message from ', Comm.SocketStream.RemoteAddress.Address,
          //  ': ', TWebsocketStringMessage(m).Data);
          //(Sender as TWebsocketCommunicator).WriteStringMessage(
          //           'Hello '+TWebsocketStringMessage(m).Data);
          Process(Sender, TWebsocketStringMessage(m).Data);
        end;
    finally
      Messages.Free;
    end;
  end;

  procedure TSocketHandler.Process(Sender: TObject; Command: string);
  var
    Communicator: TWebsocketCommunicator;
    Content: string;
  begin
    Communicator := Sender as TWebsocketCommunicator;
    Content := ContentMap.GetValue(Command);
    if Content <> '' then
    begin
      Communicator.WriteStringMessage(Content);
    end
    else
    begin
      Communicator.WriteStringMessage('ERR:NOT FOUND');
    end;
  end;

  function TServiceSocketHandler.Accept(const ARequest: TRequestData;
  const ResponseHeaders: TStrings): boolean;
  begin
    Result := True;
  end;

  procedure TServiceSocketHandler.DoHandleCommunication(
    ACommunication: TWebsocketCommunicator);
  var
    str: string;
  begin
    WriteLn('Connected to ', ACommunication.SocketStream.RemoteAddress.Address);
    ACommunication.OnReceiveMessage := @MessageReceived;
    ACommunication.OnClose := @ConnectionClosed;
    ACommunication.Open;
    while ACommunication.Open do
    begin
      // no operations
      sleep(100);
    end;
    // socket.Stop(True);
  end;

  procedure TServiceSocketHandler.ConnectionClosed(Sender: TObject);
  var
    Comm: TWebsocketCommunicator;
  begin
    Comm := TWebsocketCommunicator(Sender);
    WriteLn('Service connection to ', Comm.SocketStream.RemoteAddress.Address,
      ' closed');
  end;

  procedure TServiceSocketHandler.MessageReceived(Sender: TObject);
  var
    Messages: TWebsocketMessageOwnerList;
    m: TWebsocketMessage;
    Comm: TWebsocketCommunicator;
  begin
    Comm := TWebsocketCommunicator(Sender);
    Messages := TWebsocketMessageOwnerList.Create(True);
    try
      Comm.GetUnprocessedMessages(Messages);
      for m in Messages do
        if m is TWebsocketStringMessage then
        begin
          WriteLn('Service message from ', Comm.SocketStream.RemoteAddress.Address,
            ': ', TWebsocketStringMessage(m).Data);
          Process(Sender, TWebsocketStringMessage(m).Data);
        end;
    finally
      Messages.Free;
    end;
  end;

  procedure TServiceThread.Execute;
  var
    ServiceSocket: TWebSocketServer;
  begin
    WriteLn('Management server started on port ', Settings.ServicePort);
    ServiceSocket := TWebSocketServer.Create(Settings.ServicePort);
    try
      ServiceSocket.FreeHandlers := True;
      //socket.AcceptingMethod:=samThreadPool;
      ServiceSocket.RegisterHandler('*', '*', TServiceSocketHandler.Create, True, True);
      ServiceSocket.Start;
    finally
      ServiceSocket.Free;
    end;
  end;


  procedure TServiceSocketHandler.Process(Sender: TObject; Cmd: string);
  var
    Communicator: TWebsocketCommunicator;
    Command: string;
    option: string;
    guid: string;
    i: integer;
    keysArray: array of string;
    tmp: string;
    options: array of string;
  begin
    Communicator := Sender as TWebsocketCommunicator;
    Command := Cmd[1];
    option := Cmd.Substring(1, Cmd.Length - 1);
    if (command = 'U') then
    begin
      guid := FileServices.SetFileText(option);
      FileServices.AppendKey(guid);
      Communicator.WriteStringMessage(guid);
      ContentMap.Add(guid, option);
    end
    else if (command = 'l') then
    begin
      keysArray := ContentMap.map.Keys.ToArray();
      if Length(keysArray) > 0 then
      begin
        for i := 0 to Length(keysArray) - 1 do
        begin
          Communicator.WriteStringMessage(IntToStr(i) + '    ' + keysArray[i]);
        end;
      end
      else
      begin
        Communicator.WriteStringMessage('(EMPTY LIST)');
      end;
    end
    else if (command = 's') then
    begin
      tmp := ContentMap.GetValue(option);
      if tmp <> '' then
      begin
        Communicator.WriteStringMessage(tmp);
      end
      else
      begin
        Communicator.WriteStringMessage('ERR:NOT FOUND!');
      end;
    end
    else if (command = 'R') then
    begin
      if ContentMap.Remove(option, FileServices) then
      begin
        Communicator.WriteStringMessage('DONE.');
      end
      else
      begin
        Communicator.WriteStringMessage('ERR:NOT FOUND');
      end;
    end
    else if (command = 'C') then
    begin
      options := option.Split('*');
      if FileServices.ChangeFileText(options[0], options[1]) then
      begin
        ContentMap.map[options[0]] := options[1];
        Communicator.WriteStringMessage('DONE.');
      end
      else
      begin
        Communicator.WriteStringMessage('ERR:NOT FOUND');
      end;
    end
    else if (command = 'c') then
    begin
      keysArray := ContentMap.map.Keys.ToArray();
      Communicator.WriteStringMessage(keysArray[Length(keysArray) - 1]);
    end
    else if (command = '^') then
    begin
      Communicator.WriteStringMessage('DONE.');
      halt();
    end
    else
    begin
      Communicator.WriteStringMessage('ERR:INVALID COMMAND');
    end;
  end;

  procedure CreateEmptyFile(filename: string);
  var
    tmp: TStringList;
  begin
    tmp := TStringList.Create;
    tmp.SaveToFile(filename);
    tmp.Free;
  end;

begin
  Settings := TServerSettings.Create;
  Settings.ReadSettings(ParamStr(0));
  FileServices := TFileServices.Create;
  FileServices.DbPath := ExtractFilePath(ParamStr(0)) + Settings.DbPath;
  if not DirectoryExists(FileServices.DbPath) then
  begin
    CreateDir(FileServices.DbPath);
  end;
  if not FileExists(FileServices.DbPath + 'keys.txt') then
  begin
    CreateEmptyFile(FileServices.DbPath + 'keys.txt');
  end;
  ContentMap := TContentMap.Create;
  ContentMap.Build(FileServices);
  WriteLn('Server started on port ', Settings.Port);
  ServiceThread := TServiceThread.Create(False);
  ServiceThread.Start;
  socket := TWebSocketServer.Create(Settings.Port);
  try
    socket.FreeHandlers := True;
    //socket.AcceptingMethod:=samThreadPool;
    socket.RegisterHandler('*', '*', TSocketHandler.Create, True, True);
    socket.Start;
  finally
    socket.Free;
    FileServices.Free;
    Settings.Free;
    ContentMap.Free;
  end;
end.
