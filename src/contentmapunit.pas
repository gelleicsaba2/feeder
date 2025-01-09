unit ContentMapUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  fileServicesUnit;

type
  TMapDict = specialize TDictionary<string, string>;

  TContentMap = class(TObject)
  public
    map: TMapDict;
    v: string;
    constructor Create;
    destructor Free;
    procedure Build(service: TFileServices); virtual;
    function GetValue(key: string): string; virtual;
    procedure Add(key: string; Content: string); virtual;
    function Remove(key: string; constref service: TFileServices): boolean; virtual;
  end;

implementation

constructor TContentMap.Create;
begin
  map := TMapDict.Create;
end;

destructor TContentMap.Free;
begin
  map.Free;
  inherited Free;
end;

procedure TContentMap.Build(service: TFileServices);
var
  keys: TStringList;
  i: integer;
  content: string;
  key: string;
begin
  keys := service.ReadKeys();
  for i := 0 to keys.Count - 1 do
  begin
    key := keys.Strings[i];
    content := service.GetFileText(key);
    map.Add(keys.Strings[i], content);
  end;
end;

function TContentMap.GetValue(key: string): string;
begin
  v := '';
  map.TryGetValue(key, v);
  Result := v;
end;

procedure TContentMap.Add(key: string; Content: string);
begin
  map.Add(key, Content);
end;

function TContentMap.Remove(key: string; constref service: TFileServices): boolean;
begin
  v := '';
  map.TryGetValue(key, v);
  if v <> '' then
  begin
    map.Remove(key);
    service.RemoveGuid(key);
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

end.
