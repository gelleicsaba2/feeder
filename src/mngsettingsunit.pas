unit mngsettingsunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

const
  IniFile = 'mngmt.ini';

type
  TMngSettings=class(TObject)
    public
      Host: String;
      Port: Integer;
      procedure ReadSettings(arg0: String);
  end;

implementation

procedure TMngSettings.ReadSettings(arg0: String);
var
  Sett : TIniFile;
begin
  Sett:=TIniFile.Create(ExtractFilePath(arg0)+IniFile);
  Host:=Sett.ReadString('Service', 'host', '127.0.0.1');
  Port:=Sett.ReadInteger('Service', 'port', 9782);
  Sett.Free;
end;

end.

