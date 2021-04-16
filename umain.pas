unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ucheckupini, ExtendedTabControls, IdHTTP;

resourcestring
  sError = 'Error';
  sMissingIniFile = 'Missing ini file: ';

type

  { TfrmMain }

  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FSettings : TCheckupSettings;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  fpath : string;
begin
  fpath := ParamStr(0) + '\CheckUp.ini';
  if not FileExists(fpath) then
  begin
    MessageDlg(sError, sMissingIniFile + fpath, mtError, [mbClose], '');
    Application.Terminate;
  end;
end;

end.

