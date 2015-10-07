unit uAbout;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ShellAPI,
  // Spring Framework
  Spring.Utils,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // DLLs
  uExport;

type
  TAbout = class(TForm)
    lVersion: TLabel;
    lVersionValue: TLabel;
    lCopyright: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  About: TAbout;

implementation

{$R *.dfm}

procedure TAbout.FormCreate(Sender: TObject);
var
  s: string;
begin
  s := ProgrammName + ' v' + TFileVersionInfo.GetVersionInfo(ParamStr(0)).FileVersion;

  if IsPortable then
    s := s + 'p';

  lVersionValue.Caption := s;
end;

procedure TAbout.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

end.
