unit clientsettingsunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

const
  IniFile = 'client.ini';

type
  TClientSettings = class(TObject)
  public
    Host: string;
    Port: integer;
    procedure ReadSettings(arg0: string);
  end;

implementation

procedure TClientSettings.ReadSettings(arg0: string);
var
  Sett: TIniFile;
begin
  Sett := TIniFile.Create(ExtractFilePath(arg0) + IniFile);
  Host := Sett.ReadString('Settings', 'host', '127.0.0.1');
  Port := Sett.ReadInteger('Settings', 'port', 9781);
  Sett.Free;
end;

end.
