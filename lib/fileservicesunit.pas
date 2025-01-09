unit fileServicesUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, bufstream;

const
  {$IFDEF Linux}
  PATHSEP='/';
  {$ELSE}
  {$IFDEF UNIX}
  PATHSEP='/';
  {$ELSE}
  {$IFDEF WINDOWS}
  PATHSEP='\';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  BUFSIZE = 4096;

type
  TFileServices = class(TObject)
  private
    FDbPath: string;
    procedure SetDbPath(_DbPath: string);
  public
    property DbPath: string read FDbPath write SetDbPath;
    function GetFileText(Guid: string): string; virtual;
    function SetFileText(content: string): string; virtual;
    function Exist(Guid: string): boolean; virtual;
    procedure AppendKey(Guid: string); virtual;
    function ReadKeys(): TStringList; virtual;
    procedure RemoveGuid(Guid: string); virtual;
    function ChangeFileText(Guid: string; content: string): boolean; virtual;
  end;

implementation

procedure TFileServices.SetDbPath(_DbPath: string);
begin
  FDbPath := ExcludeTrailingBackslash(_DbPath) + PATHSEP;
end;

function TFileServices.Exist(Guid: string): boolean;
begin
  Result := FileExists(FDbPath + Guid);
end;

function TFileServices.GetFileText(Guid: string): string;
var
  fs: TBufferedFileStream;
  buf: array [1..BUFSIZE] of byte;
  readen: longint;
  myutf8String: utf8string;
  builder: TStringBuilder;
  filepath: string;
begin
  filepath := FDbPath + Guid;
  fs := TBufferedFileStream.Create(filepath, fmOpenRead);
  builder := TStringBuilder.Create;
  repeat
    readen := fs.Read(buf, BUFSIZE);
    setlength(myutf8String, readen);
    move(buf[1], myutf8String[1], readen);
    builder.Append(myutf8String);
  until readen < BUFSIZE;
  fs.Flush;
  fs.Destroy;
  Result := builder.ToString();
end;

function TFileServices.SetFileText(content: string): string;
var
  tfOut: TextFile;
  filepath: string;
  Guid: TGuid;
  GuidStr: string;
begin
  CreateGUID(Guid);
  GuidStr := Guid.ToString().ToLower();
  GuidStr := GuidStr.SubString(1, GuidStr.Length - 2);
  filepath := FDbPath + GuidStr;
  AssignFile(tfOut, filepath);
  {$I+}
  try
    rewrite(tfOut);
    writeln(tfOut, content);
    CloseFile(tfOut);
  except
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
  end;
  writeln('File ', filepath, ' created.');
  Result := GuidStr;
end;

procedure TFileServices.AppendKey(Guid: string);
var
  f: Text;
  filepath: string;
begin
  Guid := Guid.ToLower;
  filepath := FDbPath + 'keys.txt';
  Assign(f, filepath);
  if not FileExists(filepath) then
  begin
    Rewrite(f);
    Writeln(F, Guid);
    Close(f);
  end
  else
  begin
    Append(f);
    Writeln(f, Guid);
    Close(f);
  end;
end;

function TFileServices.ReadKeys(): TStringList;
var
  tfIn: TextFile;
  s: string;
  filepath: string;
begin
  Result := TStringList.Create;
  filepath := FDbPath + 'keys.txt';
  AssignFile(tfIn, filepath);
  try
    reset(tfIn);
    while not EOF(tfIn) do
    begin
      readln(tfIn, s);
      if s <> '' then
      begin
        Result.Add(s);
      end;
    end;
    CloseFile(tfIn);
  except
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.Message);
  end;
end;


procedure TFileServices.RemoveGuid(Guid: string);
var
  tfIn: TextFile;
  s: string;
  filepath: string;
  list: TStringList;
  f: Text;
  i: integer;
begin
  Guid := Guid.ToLower;
  list := TStringList.Create;
  filepath := FDbPath + 'keys.txt';
  AssignFile(tfIn, filepath);
  try
    reset(tfIn);
    while not EOF(tfIn) do
    begin
      readln(tfIn, s);
      if (s <> Guid) and (s <> '') then
      begin
        list.Add(s);
      end;
    end;
    CloseFile(tfIn);
  except
    on E: EInOutError do
    begin
      writeln('File handling error occurred. Details: ', E.Message);
      exit;
    end;
  end;
  RenameFile(FDbPath + 'keys.txt', FDbPath + '____keys.txt');
  filepath := FDbPath + 'keys.txt';
  Assign(f, filepath);
  Rewrite(f);
  for i := 0 to list.Count - 1 do
  begin
    Writeln(F, list.Strings[i]);
  end;
  Close(f);
  DeleteFile(FDbPath + '____keys.txt');
  DeleteFile(FDbPath + Guid);
end;

function TFileServices.ChangeFileText(Guid: string; content: string): boolean;
var
  tfOut: TextFile;
  filepath: string;
  GuidStr: string;
begin
  GuidStr := Guid.ToLower();
  filepath := FDbPath + GuidStr;
  AssignFile(tfOut, filepath);
  {$I+}
  try
    rewrite(tfOut);
    writeln(tfOut, content);
    CloseFile(tfOut);
  except
    on E: EInOutError do
    begin
      writeln('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
      exit(False);
    end;
  end;
  Result := True;
end;

end.
