program management;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,{$ENDIF}
  Classes,
  SysUtils,
  websocketsclient,
  wsmessages,
  wsstream,
  wsutils,
  ssockets,
  bufstream,
  mngsettingsunit;

const
  BUFSIZE = 4096;

type

  { TManagement }

  TManagement = class
  private
    FCommunicator: TWebsocketCommunicator;
    procedure ReceiveMessage(Sender: TObject);
    procedure StreamClosed(Sender: TObject);
  public
    ExportFile: string;
    procedure Execute(command: string; option: string);
    constructor Create(ACommunicator: TWebsocketCommunicator);
    function GetFileText(filepath: string): string; virtual;
    procedure ExportToFile(filename: string; Content: string); virtual;
    destructor Destroy; override;
  end;

  { TManagement }

  procedure TManagement.StreamClosed(Sender: TObject);
  begin
    WriteLn('Connection to ', FCommunicator.SocketStream.RemoteAddress.Address,
      ' closed');
  end;

  procedure TManagement.ReceiveMessage(Sender: TObject);
  var
    MsgList: TWebsocketMessageOwnerList;
    m: TWebsocketMessage;
  begin
    MsgList := TWebsocketMessageOwnerList.Create(True);
    try
      FCommunicator.GetUnprocessedMessages(MsgList);
      for m in MsgList do
        if m is TWebsocketStringMessage then
        begin
          if ExportFile = '' then
          begin
            WriteLn(TWebsocketStringMessage(m).Data);
          end
          else
          begin
            ExportToFile(ExportFile, TWebsocketStringMessage(m).Data);
            WriteLn('''' + ExportFile + ''' has been created.');
          end;
        end;
    finally
      MsgList.Free;
      FCommunicator.Close;
    end;
  end;

  procedure TManagement.ExportToFile(filename: string; Content: string);
  var
    list: TStringList;
  begin
    list := TStringList.Create;
    list.Text := Content;
    list.SaveToFile(filename, TEncoding.UTF8);
    list.Free;
  end;

  procedure TManagement.Execute(command: string; option: string);
  var
    str: string;
    sb: TStringBuilder;
    options: array of string;
  begin
    ExportFile := '';
    if FCommunicator.Open then
    begin
      if command = 'upload' then
      begin
        try
          sb := TStringBuilder.Create;
          sb.Append('U');
          sb.Append(GetFileText(option));
          FCommunicator.WriteStringMessage(sb.toString());
        finally
          sb.Free;
        end;
      end
      else if ((command = 'l') or (command = 'list')) and
        ((option = '') or (option = 'guids')) then
      begin
        FCommunicator.WriteStringMessage('l-');
      end
      else if (command = 'show') or (command = 's') then
      begin
        sb := TStringBuilder.Create;
        sb.Append('s');
        sb.Append(option);
        FCommunicator.WriteStringMessage(sb.toString());
        sb.Free();
      end
      else if command = 'remove' then
      begin
        sb := TStringBuilder.Create;
        sb.Append('R');
        sb.Append(option);
        FCommunicator.WriteStringMessage(sb.toString());
        sb.Free();
      end
      else if command = 'change' then
      begin
        options := option.Split('*');
        sb := TStringBuilder.Create;
        sb.Append('C');
        sb.Append(options[0]);
        sb.Append('*');
        sb.Append(GetFileText(options[1]));
        FCommunicator.WriteStringMessage(sb.toString());
        sb.Free();
      end
      else if command = 'last' then
      begin
        FCommunicator.WriteStringMessage('c.');
      end
      else if command = 'export' then
      begin
        options := option.Split('*');
        ExportFile := options[1];
        sb := TStringBuilder.Create;
        sb.Append('s');
        sb.Append(options[0]);
        FCommunicator.WriteStringMessage(sb.toString());
        sb.Free();
      end
      else if command = 'terminate' then
      begin
        FCommunicator.WriteStringMessage('^.');
      end;
      while FCommunicator.Open do
        Sleep(100);
    end;
  end;

  constructor TManagement.Create(ACommunicator: TWebsocketCommunicator);
  begin
    FCommunicator := ACommunicator;
    FCommunicator.OnClose := @StreamClosed;
    FCommunicator.OnReceiveMessage := @ReceiveMessage;
    FCommunicator.StartReceiveMessageThread;
  end;

  destructor TManagement.Destroy;
  begin
    FCommunicator.StopReceiveMessageThread;
    while FCommunicator.ReceiveMessageThreadRunning do
      Sleep(10);
    FCommunicator.Free;
    inherited Destroy;
  end;

  function TManagement.GetFileText(filepath: string): string;
  var
    fs: TBufferedFileStream;
    buf: array [1..BUFSIZE] of byte;
    readen: longint;
    myutf8string: utf8string;
    builder: TStringBuilder;
  begin
    fs := TBufferedFileStream.Create(filepath, fmOpenRead);
    builder := TStringBuilder.Create;
    repeat
      readen := fs.Read(buf, BUFSIZE);
      setlength(myutf8string, readen);
      move(buf[1], myutf8string[1], readen);
      builder.Append(myutf8string);
    until readen < BUFSIZE;
    fs.Flush;
    fs.Destroy;
    Result := builder.ToString();
  end;


var
  client: TWebsocketClient;
  mng: TManagement;
  settings: TMngSettings;
  command: string;
  option: string;
  valid: boolean;
  ExportFile: string;
begin
  WriteLn('Feeder management service');
  valid := False;
  if (ParamCount = 1) and ((ParamStr(1) = 'terminate') or (ParamStr(1) = 'last') or
    (ParamStr(1) = 'list') or (ParamStr(1) = 'l')) then
  begin
    command := ParamStr(1);
    option := '';
    valid := True;
  end
  else if ParamCount = 2 then
  begin
    command := ParamStr(1);
    option := ParamStr(2);
    if (command = 'list') and (option = 'guids') then
    begin
      valid := True;
    end
    else if (command = 'show') or (command = 's') or (command = 'remove') or
      (command = 'upload') then
    begin
      valid := True;
    end;
  end
  else if (ParamCount = 3) and (ParamStr(1) = 'change') then
  begin
    command := ParamStr(1);
    option := ParamStr(2) + '*' + ParamStr(3);
    valid := True;
  end
  else if (ParamCount = 3) and (ParamStr(1) = 'export') then
  begin
    command := ParamStr(1);
    option := ParamStr(2) + '*' + ParamStr(3);
    valid := True;
  end;
  if ((ParamCount = 0) or (not valid)) then
  begin
    if ((ParamCount <> 0) and (not valid)) then
    begin
      Writeln('ERR:Invalid paramter.');
    end;
    Writeln();
    Writeln(
      'Usage:' + LineEnding + ' FeederMngmt <command>' + LineEnding +
      LineEnding + 'Commands:' + LineEnding + LineEnding +
      ' upload <text file> : upload text file.' + LineEnding +
      ' l OR list OR list guids : list all guids.' + LineEnding +
      ' s <GUID> OR show <GUID> : show the specified content.' +
      LineEnding + ' remove <GUID> : remove the specified content.' +
      LineEnding + ' change <GUID> <text file> : remove the specified content.' +
      LineEnding + ' last : show the last GUID (last upload).' +
      LineEnding +
      ' export <GUID> <text file> : export specified GUID to specified text file.' +
      LineEnding + ' terminate : send terminate signal to server.');
    Writeln();
    exit;
  end;

  settings := TMngSettings.Create;
  try
    settings.ReadSettings(ParamStr(0));
  except
    WriteLn('ERR:CANT READ ''mngmt.ini''');
    settings.Free;
    Exit;
  end;
  try
    client := TWebsocketClient.Create(settings.Host, settings.Port);

    try
      try
        mng := TManagement.Create(client.Connect(TSocketHandler.Create));
        mng.Execute(command, option);
      except
        on e: ESocketError do
        begin
          WriteLn('ERR:CONNECTION FAILED');
          Exit;
        end;
      end;
    finally
      mng.Free;
    end;

  finally
    if client <> nil then client.Free;
    if settings <> nil then settings.Free;
  end;
end.
