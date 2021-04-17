unit uHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls;

type

  { TfrmHelp }

  TfrmHelp = class(TForm)
    btnClose: TBitBtn;
    memo: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure memoChange(Sender: TObject);
  private

  public
    class procedure Execute;

  end;

var
  frmHelp: TfrmHelp;

implementation

{$R *.lfm}

procedure TfrmHelp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    CloseAction:= caFree;
end;

procedure TfrmHelp.memoChange(Sender: TObject);
begin

end;

class procedure TfrmHelp.Execute;
begin
  TfrmHelp.Create(Application).ShowModal;
end;
end.

