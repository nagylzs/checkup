unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons, IdHTTP, IdSSLOpenSSL, IniFiles,
  IdComponent, shellapi;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnAbort: TBitBtn;
    btnRun: TBitBtn;
    http: TIdHTTP;
    iohandler: TIdSSLIOHandlerSocketOpenSSL;
    m: TMemo;
    pnlBottom: TPanel;
    ProgressBar: TProgressBar;
    timer: TTimer;
    procedure btnAbortClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure httpWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
    procedure timerTimer(Sender: TObject);
  private
    procedure LoadParams(ASectionPostfix: string);
    function GetVersions: TStringList;
    procedure DoUpdate(ASectionPostfix: string);
    procedure Log(const msg: string);
    function Body2FileList(const ARelUrl: string; const ALogPrefix: string;
      const APostfix: string): TStringList;
    function GetBodyFirstLine: string;
    procedure DownloadDir(const ARelURL: string; const ADestDir: string;
      const ALogPrefix: string = '');
    function GetBodyFileName: string;
    // function GetContentType: string;
    // function GetHeaderFileName: string;
    procedure Get(RelUrl: string);
    procedure SetupHTTP;
    procedure CopyAllFiles(ASourceDir, ADestDir: string;
      AReset_Rights: boolean = False; AOverWrite: boolean = True);
    function IsParentURL(AParentURL, ACurrentURL: string): boolean;
    procedure DoRun;
  public
    FIniFile: TIniFile;
    FLogFile: TFileStream;

    FSectionPostfix: string;

    FServerSection: string;
    FProxySection: string;
    FResourceSection: string;
    FDestinationSection: string;

    FProto: string;
    FHost: string;
    FPort: integer;
    FProxyHost: string;
    FProxyPort: integer;
    FProxyUser: string;
    FProxyPassword: string;

    FRun: string;
    FRunParams: string;

    FResourceName: string;
    FLastVersionDir: string;
    FLastVersion: string;
    FVersionPath: string;

    FDownloadDir: string;
    FInstallDir: string;
    FCheckWritable: string;
    FReplicate: boolean;

    FSectionIndex: integer;
    FAbort: boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  fileutil, uHelp, uFunctions;

{$R *.lfm}

{ TfrmMain }


constructor TfrmMain.Create(AOwner: TComponent);
var
  fpath: string;
  flogfilepath: string;
begin
  inherited;

  FSectionIndex := 1;
  FAbort := False;

  if ParamCount > 0 then
  begin

    if ((ParamStr(1) = '/?') or (ParamStr(1) = '-h') or
      (ParamStr(1) = '--help')) then
    begin
      TfrmHelp.Execute;
      Application.Terminate;
      exit;
    end;


    if not FileExists(ParamStr(1)) then
      raise Exception.Create('Nem tal??lhat??: ' + ParamStr(1));
    FIniFile := TIniFile.Create(ParamStr(1));
  end
  else
  begin
    fpath := GetExeDir + 'checkup.ini';
    if not FileExists(fpath) then
      fpath := GetExeDir + 'autoupdate.ini';
    if not FileExists(fpath) then
      raise Exception.Create('Nem tal??lhat??: checkup.ini');
    FIniFile := TIniFile.Create(fpath);
  end;

  flogfilepath := GetExedir + 'CheckUp.log';
  if FileExists(flogfilepath) then
  begin
    FLogFile := TFileStream.Create(flogfilepath, fmOpenWrite or fmShareDenyWrite);
    FLogFile.Seek(0, soEnd);
  end
  else
  begin
    FLogFile := TFileStream.Create(flogfilepath, fmCreate or fmOpenWrite);
  end;
end;

destructor TfrmMain.Destroy;
begin
  try
    FIniFile.Free;
    FLogFile.Free;
  finally
    inherited;
  end;
end;

procedure TfrmMain.Log(const msg: string);
var
  s: string;
begin
  m.Lines.add(msg);
  s := DateTimeToStr(now) + ': ' + msg + #13 + #10;
  FLogFile.Write(s[1], Length(s));
end;

procedure TfrmMain.LoadParams(ASectionPostfix: string);
begin
  with FIniFile do
  begin
    FSectionPostfix := ASectionPostfix;

    if SectionExists('Server' + ASectionPostfix) then
    begin
      FServerSection := 'Server' + ASectionPostfix;
    end
    else
    begin
      FServerSection := 'Server';
    end;

    FProto := ReadString(FServerSection, 'Proto', 'http');
    FHost := ReadString(FServerSection, 'Host', '');
    FPort := ReadInteger(FServerSection, 'Port', 8000);

    Log(FServerSection + ': ' + FProto + '://' + FHost + ':' + IntToStr(FPort));

    if SectionExists('Proxy' + ASectionPostfix) then
    begin
      FProxySection := 'Proxy' + ASectionPostfix;
    end
    else
    begin
      FProxySection := 'Proxy';
    end;

    FProxyHost := ReadString(FProxySection, 'Host', '');
    FProxyPort := ReadInteger(FProxySection, 'Port', 80);
    FProxyUser := ReadString(FProxySection, 'User', '');
    FProxyPassword := ReadString(FProxySection, 'Password', '');

    if FProxyHost <> '' then
      Log(FProxySection + ': ' + FProxyHost + ':' + IntToStr(FProxyPort));

    FRun := ReadString('AfterUpdate' + ASectionPostfix, 'Run', '');
    FRunParams := ReadString('AfterUpdate' + ASectionPostfix, 'Params', '');

    FResourceSection := 'Resource' + ASectionPostfix;

    FResourceName := ReadString(FResourceSection, 'Name', '');
    if (FResourceName <> '') then
    begin
      if FResourceName[Length(FResourceName)] <> '/' then
        FResourceName := FResourceName + '/';
    end;
    FVersionPath := ReadString(FResourceSection, 'VersionPath', '');
    FLastVersionDir := ReadString(FResourceSection, 'LastVersionDir', '');
    FLastVersion := ReadString(FResourceSection, 'LastVersion', '');

    Log(FResourceSection + ': ' + FResourceName);

    FDestinationSection := 'Destination' + ASectionPostfix;

    FDownloadDir := ReadString(FDestinationSection, 'DownloadDir', '');
    if FDownloadDir = '' then
      FDownloadDir := GetExedir + 'Download';
    FInstallDir := ReadString(FDestinationSection, 'InstallDir', '');
    if FInstallDir = '' then
      FInstallDir := StrDec(GetExedir);

    FCheckWritable := ReadString(FDestinationSection, 'CheckWritable', '');
    if FCheckWritable = '' then
      FCheckWritable := GetExedir + FRun;

    FReplicate := ReadInteger(FDestinationSection, 'Replicate', 0) <> 0;

    Log('Local version: ' + FLastVersion);
    Log('Local version dir: ' + FLastVersionDir);
  end;
end;

function TfrmMain.GetBodyFileName: string;
begin
  Result := GetExedir + 'BODY';
end;

procedure TfrmMain.SetupHTTP;
begin
  with http do
  begin
    if FProxyHost <> '' then
    begin
      ProxyParams.ProxyServer := FProxyHost;
      ProxyParams.ProxyPort := FProxyPort;
      ProxyParams.ProxyUsername := FProxyUser;
      ProxyParams.ProxyPassword := FProxyPassword;
    end;

  end;
end;

procedure TfrmMain.Get(RelUrl: string);
var
  resp: TFileStream;
  cnt: integer;

  url: string;
begin
  if FileExists(GetBodyFileName) then
    DeleteFile(GetBodyFileName);
  resp := TFileStream.Create(GetBodyFileName, fmCreate or fmShareDenyWrite);
  cnt := 0;
  try
    repeat
      try
        // For https, openssl dlls are needed.
        // http://www.indyproject.org/sockets/ssl.en.aspx
        url := FProto + '://' + FHost + ':' + IntToStr(FPort) + RelUrl;
        Log('    GET ' + url);
        http.Get(url, resp);
        Application.ProcessMessages;
        break;
      except
        on E: EIdHTTPProtocolException do
        begin
          Log('HTTP ' + IntToStr(e.ErrorCode)+': '+e.Message);
          raise;
        end;
        on E: Exception do
        begin
          Log(e.Message);
          Log('HIBA('+IntToStr(cnt)+') - hamarosan ??jra pr??b??lom');
          if cnt > 20 then
            raise;
          sleep(300);
          Inc(cnt);
        end;
      end;
    until False;
    Application.ProcessMessages;
  finally
    resp.Free;
  end;
end;

function TfrmMain.Body2FileList(const ARelUrl: string; const ALogPrefix: string;
  const APostfix: string): TStringList;
var
  index: integer;
  b, bu: string;
  nnname: string;

  prefix: string;
begin
  prefix := FResourceName + ARelUrl;

  b := File2String(GetBodyFileName);
  bu := AnsiUpperCase(b);

  Result := TStringList.Create;
  repeat
    index := Pos('HREF="', bu);
    if index > 0 then
    begin
      b := System.Copy(b, index + Length('HREF="'), Length(b));
      bu := System.Copy(bu, index + Length('HREF="'), Length(bu));

      index := Pos('"', bu);
      if index > 0 then
      begin
        nnname := Trim(System.Copy(b, 1, index - 1));

        if Pos(prefix, nnname) = 1 then
          nnname := System.Copy(nnname, Length(prefix) + 1, Length(nnname));

        if (Length(nnname) > 1) then
        begin
          if (APostfix = '') or ((APostfix <> '') and StrEndsWith(nnname, APostfix)) then
          begin
            Result.Add(nnname);
            Log(ALogPrefix + nnname);
          end;
        end;

        b := System.Copy(b, index + Length('"'), Length(b));
        bu := System.Copy(bu, index + Length('"'), Length(bu));
      end
      else
        break;
    end
    else
      break;
  until False;
end;

function URLIsDir(url: string): boolean;
begin
  Result :=
    (Length(url) > 0) and (url[Length(url)] = '/');
end;

procedure TfrmMain.TimerTimer(Sender: TObject);
var
  secname: string;
begin
  if Showing then
  begin
    if btnRun.Enabled then
      exit;

    Timer.Enabled := False;
    if FSectionIndex = 1 then
    begin
      DoUpdate('');
    end
    else
    begin
      secname := 'Resource' + IntToStr(FSectionIndex);
      if not FIniFile.SectionExists(secname) then
      begin
        Close;
        Exit;
      end;
      if FIniFile.SectionExists(secname) then
      begin
        DoUpdate(IntToStr(FSectionIndex));
      end;
    end;
    Inc(FSectionIndex);
    Timer.Enabled := True;
  end;
end;

procedure TfrmMain.DoUpdate(ASectionPostfix: string);
var
  FVersions: TStringList;
  FNewestVersion: string;
  FNewestVersionDir: string;
  fs: TFileStream;
begin
  //if ASectionPostfix<>'' then
  Log('K??vetkez?? szekci?? fedolgoz??sa: ' + ASectionPostfix);

  LoadParams(ASectionPostfix);
  SetupHTTP;

  FVersions := nil;
  FNewestVersionDir := '';

  if FVersionPath <> '' then
  begin
    Get(FVersionPath);
    FNewestVersion := GetBodyFirstLine;
    if FNewestVersion = '' then
      raise Exception.Create('Az utols?? verzi?? neve nem hat??rozhat?? meg' +
        ' (' + FVersionPath + '), nem lehet friss??teni.');
  end
  else
  begin
    FVersions := GetVersions;
    if FVersions.Count = 0 then
    begin
      FVersions.Free;
      raise Exception.Create(
        'Nincsenek el??rhet?? verzi??k, tal??n rosszak a be??ll??t??sok.');
    end;
    FVersions.Sort;
    FNewestVersionDir := FVersions[FVersions.Count - 1];
    FNewestVersion := FNewestVersionDir;
  end;

  try
    if FNewestVersion <> FLastVersion then
    begin
      // Ha VersionPath -ot haszn??ltunk, akkor m??g mindig le kell t??lteni
      // a verzi?? k??nyvt??r list??t, ??s kiszedni az utols??t.
      if FVersions = nil then
      begin
        FVersions := GetVersions;
        if FVersions.Count = 0 then
        begin
          FVersions.Free;
          raise Exception.Create(
            'Nincsenek el??rhet?? verzi??k, tal??n rosszak a be??ll??t??sok.');
        end;
        FVersions.Sort;
        FNewestVersionDir := FVersions[FVersions.Count - 1];
      end;

      if (FCheckWritable <> '') and FileExists(FCheckWritable) then
      begin
        Log('??rhat??s??g ellen??rz??se: ' + FCheckWritable);

        while not FAbort do
        begin
          try
            fs := TFileStream.Create(FCheckWritable, fmOpenReadWrite or
              fmShareDenyWrite);
            fs.Free;
            break; // Successful write attempt.
          except
            on E: EFOpenError do
            begin
              Log(E.Message);
              if MessageDlg('K??rd??s',
                'Van el??rhet?? ??j verzi??, de a friss??t??st nem lehet elv??gezni, mert a k??vetkez?? ??llom??ny nem ??rhat??: '
                + #13 + #13 + FCheckWritable + #13 + #13 +
                'Ha m??r fut a program egy p??ld??nya, akkor l??pjen ki bel??le, miel??tt a friss??t??seket elv??gezn??, ??s pr??b??lja ??jra.' + #13 + 'Szeretn?? most ??jra megpr??b??lni?', mtConfirmation, [mbYes, mbNo], '') <> mrYes then
                Abort;
            end;
          end;
        end;
      end;

      if FAbort then
      begin
        Log('Megszak??tva.');
        Exit;
      end;

      Log('Friss??t??s erre a verzi??ra: ' + FNewestVersion);
      Log('Friss??t??s erre a k??nyvt??rra: ' + FNewestVersionDir);
      Log('-------------------------------');

      if not DirectoryExists(FDownloadDir) then
        MkDir(FDownloadDir);
      FDownloadDir := FDownloadDir + '\' + StrDec(FNewestVersionDir);

      if not DirectoryExists(FInstallDir) then
        MkDir(FInstallDir);

      if StrEndsWith(FResourceName,'/')
         then DownloadDir(FResourceName + FNewestVersionDir, FDownloadDir)
         else DownloadDir(FResourceName + '/' + FNewestVersionDir, FDownloadDir);
      Log('-------------------------------');

      if FAbort then
      begin
        Log('Megszak??tva.');
        Exit;
      end;

      if FReplicate then
      begin
        FInstallDir := FInstallDir + '\' + StrDec(FNewestVersionDir);
        if not DirectoryExists(FInstallDir) then
          MkDir(FInstallDir);
      end;

      if FAbort then
      begin
        Log('Megszak??tva.');
        Exit;
      end;

      CopyAllFiles(FDownloadDir, FInstallDir);
      FIniFile.WriteString(FResourceSection, 'LastVersion', FNewestVersion);
      FIniFile.WriteString(FResourceSection, 'LastVersionDir', FNewestVersionDir);
    end
    else
    begin
      Log('??gy t??nik ez m??r a leg??jabb verzi??...');
    end;
    Log('-------------------------------');

    (*
    if CtrlDown and (FRun<>'') then
    begin
      Log('Futtatni: ' + FRun);
      btnRun.Enabled := true;
      Log('--------[ CTRL lenyomva ]-------');
      Log('Nyomja meg a futtat??s vagy a megszak??t??s gombot a folytat??shoz.');
    end else begin
      DoRun;
      btnRun.Enabled := false;
    end;
    *)

    DoRun;
    btnRun.Enabled := False;

  finally
    FVersions.Free;
  end;
end;

procedure TfrmMain.DoRun;
begin
  if FRun <> '' then
    ShellExecute(
      0,nil,
      PChar(GetExeDir + FRun),
      PChar(FRunParams),
      PChar(GetExeDir),
      1 // SW_SHOWNORMAL
  );
end;

function TfrmMain.GetVersions: TStringList;
begin
  Log('Verzi??k keres??se...');
  Get(FResourceName + '/');
  Result := Body2FileList('/', ' -> ', '/');
end;

// Igazat ad ha AParentURL az ACurrentURL parent-je
function TfrmMain.IsParentURL(AParentURL, ACurrentURL: string): boolean;
begin
  if (AParentUrl = '..') or (AParentUrl = '../') or
    (Pos(AParentURL, ACurrentURL) = 1) or (AParentURL = ACurrentURL + '/..') or
    (AParentURL = ACurrentURL + '/../') or (AParentURL = ACurrentURL + '..') or
    (AParentURL = ACurrentURL + '../') then
    Result := True
  else
    Result := False;
end;

procedure TfrmMain.DownloadDir(const ARelURL: string; const ADestDir: string;
  const ALogPrefix: string = '');
var
  FFiles: TStringList;
  index: integer;

  item_url, dirname: string;
begin
  if not DirectoryExists(ADestDir) then
    MkDir(ADestDir);

  if StrStartsWith(ARelURL, '/')
     then Get(ARelURL)
     else Get('/' + ARelURL);
  FFiles := Body2FileList('/' + ARelURL, ALogPrefix, '');
  try
    if FAbort then
    begin
      Log('DownloadDir: Megszak??tva.');
      Exit;
    end;

    for index := 0 to FFiles.Count - 1 do
    begin
      // Itt megn??zz??k hogy parent dir hivatkoz??s van-e
      if not IsParentURL(FFiles[index], FResourceName + '/' + ARelUrl) then
      begin
        item_url := ARelUrl + FFiles[index];
        Log('Let??lt??s ' + item_url);
        if StrStartsWith(item_url, '/')
           then Get(item_url)
           else Get('/' + item_url);
        if URLIsDir(item_url) then
        begin
          dirname := StrDec(FFiles[index]);
          DownloadDir(
            ARelUrl + FFiles[index],
            ADestDir + '\' + dirname,
            ALogPrefix + '/' + dirname
            );
        end
        else
          RenameFile(
            GetBodyFileName,
            ADestDir + '\' + FFiles[index]
            );
      end;

      if FAbort then
      begin
        Log('DownloadDir: Megszak??tva.');
        Exit;
      end;

    end;
  finally
    FFiles.Free;
  end;
end;

procedure TfrmMain.CopyAllFiles(ASourceDir, ADestDir: string;
  AReset_Rights: boolean = False; AOverWrite: boolean = True);
var
  F: TSearchRec;
  res: integer;

  procedure DoCopy(AFileName: string);
  var
    SourceFile: string;
    DestFile: string;
  begin
    Repaint;
    SourceFile := ASourceDir + '\' + AFileName;
    DestFile := ADestDir + '\' + AFileName;
    Log('Copy: ' + DestFile);

    if DirectoryExists(SourceFile) then
    begin
      // K??nyvt??r m??sol??sa
      if not DirectoryExists(DestFile) then
        if not CreateDir(DestFile) then
          raise Exception.Create(
            'Nem lehetett l??trehozni ezt a k??nyvt??rat: ' + #13 + DestFile);
      CopyAllFiles(SourceFile, DestFile);
    end
    else
    begin
      // ??llom??ny m??sol??sa
      if FileExists(DestFile) then
      begin
        if not AOverWrite then
          raise Exception.Create('Ez az ??llom??ny m??r l??tezik: ' +
            DestFile);
        if AReset_Rights then
          if (FileSetAttr(DestFile, faArchive) <> 0) or
            (not DeleteFile(DestFile)) then
            raise Exception.Create('Nem t??r??lhet??: ' + DestFile);
      end;
      if not CopyFile(SourceFile, DestFile, False) then
        raise Exception.Create('Nem m??solhat??: ' + #13 + SourceFile +
          ' -> ' + DestFile);
      if AReset_Rights then
        if FileSetAttr(DestFile, faArchive) <> 0 then
          ShowMessage(
            'Figyelem! ' + #13 + 'File attrib??tum nem t??r??lhet??: ' +
            #13 + DestFile
            );
    end;
  end;

begin
  res := FindFirst(ASourceDir + '\*.*', SysUtils.faReadOnly or
    SysUtils.faDirectory, F);
  try
    while res = 0 do
    begin
      if FAbort then
      begin
        Log('CopyAllFiles: Megszak??tva.');
        Exit;
      end;

      if (F.Name <> '.') and (F.Name <> '..') and (F.Name <> '../') then
        DoCopy(F.Name);
      res := FindNext(F);
    end;
  finally
    FindClose(F);
  end;
end;

function TfrmMain.GetBodyFirstLine: string;
var
  body: string;
  idx: integer;
begin
  body := File2String(GetBodyFileName);
  idx := Pos('\n', body);
  if (idx > 0) then
  begin
    body := System.Copy(body, 1, idx);
  end;
  Result := Trim(body);
end;

procedure TfrmMain.btnRunClick(Sender: TObject);
begin
  DoRun;
  btnRun.Enabled := False;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin

end;

procedure TfrmMain.httpWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
begin
  with http do
  begin
    if Response.ContentLength > 0 then
      ProgressBar.Max := Response.ContentLength;
    if (AWorkCount >= 0) and (AWorkCount <= Response.ContentLength) then
      ProgressBar.Position := AWorkCount;
  end;
  //if FAbort then
  // http.Abort ???

  Application.ProcessMessages;
end;

procedure TfrmMain.btnAbortClick(Sender: TObject);
begin
  btnRun.Enabled := True;
  FAbort := True;
  Close;
end;


end.































