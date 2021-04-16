unit ucheckupini;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type

  { TCheckupSettings }

  TCheckupSettings=class(TObject)
  private
    FIniFile : TIniFile;
    FServer : TStringList;
    function GetServerBaseUrl: string;
  protected
    procedure Load;
    procedure Unload;
  public
    constructor Create(AIniFile:string);
    destructor Destroy; override;

    property ServerBaseUrl : string
      read GetServerBaseUrl;
  end;

implementation

{ TCheckupSettings }

function TCheckupSettings.GetServerBaseUrl: string;
begin
  Result := FServer.Values['proto'] +
         '://' + FServer.Values['Host'] + ':' + FServer.Values['port'];
end;

procedure TCheckupSettings.Load;
begin
  FServer := TStringList.Create;
  FIniFile.ReadSection('Server', FServer);
end;

procedure TCheckupSettings.Unload;
begin
  FreeAndNil(FServer);
end;

constructor TCheckupSettings.Create(AIniFile: string);
begin
  FIniFile := TIniFile.Create(AIniFile);
  FServer := nil;
  try
     Load;
  except
    Unload;
  end;
end;

destructor TCheckupSettings.Destroy;
begin
  FIniFile.Free;
  Unload;
  inherited;
end;

end.

