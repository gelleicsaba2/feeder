unit serverSettingsUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

const
  IniFile = 'server.ini';

type
  TServerSettings=class(TObject)
    public
      DbPath: String;
      //Host: String;
      Port: Integer;
      ServicePort: Integer;
      procedure ReadSettings(arg0: String);
  end;

implementation

procedure TServerSettings.ReadSettings(arg0: String);
var
  Sett : TIniFile;
begin
  Sett:=TIniFile.Create(ExtractFilePath(arg0)+IniFile);
  //Host:=Sett.ReadString('Settings', 'host', '127.0.0.1');
  Port:=Sett.ReadInteger('Settings', 'port', 9781);
  DbPath:=Sett.ReadString('Settings', 'dbpath', './contents');
  ServicePort:=Sett.ReadInteger('Service', 'port', 9782);
  Sett.Free;
end;

end.

