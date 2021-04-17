unit ufunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

function GetExedir: string;
function File2String(AFilename: string): string;
function StrDec(s: string): string;

function StrEndsWith(AStr, ATail: string; ACaseSensistive: boolean = False): boolean;

function StrStartsWith(AStr, AHead: string; ACaseSensistive: boolean = False): boolean;

implementation


function StrDec(s: string): string;
begin
  Result := System.Copy(s, 1, Length(s) - 1);
end;

function GetExedir: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

function File2String(AFilename: string): string;
var
  fs: TFileStream;
  ss: TStringStream;
begin
  ss := nil;
  fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    ss := TStringStream.Create('');
    ss.CopyFrom(fs, fs.Size);
    Result := ss.DataString;
  finally
    fs.Free;
    ss.Free;
  end;
end;

function StrEndsWith(AStr, ATail: string; ACaseSensistive: boolean = False): boolean;
var
  Ls, Lt: integer;
begin
  Ls := Length(AStr);
  Lt := Length(ATail);
  if Lt = 0 then
    Result := True
  else
  begin
    if ACaseSensistive then
      Result := Copy(AStr, Ls - Lt + 1, Lt) = ATail
    else
      Result := AnsiUpperCase(Copy(AStr, Ls - Lt + 1, Lt)) = AnsiUpperCase(ATail);
  end;
end;



function StrStartsWith(AStr, AHead: string; ACaseSensistive: boolean = False): boolean;
var
  Ls, Lh: integer;
begin
  Ls := Length(AStr);
  Lh := Length(AHead);
  if Lh > Ls then
    Result := False
  else
  begin
    if ACaseSensistive then
      Result := Copy(AStr, 1, Lh) = AHead
    else
      Result := AnsiUpperCase(Copy(AStr, 1, Lh)) = AnsiUpperCase(AHead);
  end;
end;


end.

