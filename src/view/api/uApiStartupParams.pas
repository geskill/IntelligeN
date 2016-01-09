unit uApiStartupParams;

interface

uses
  // Delphi
  SysUtils, Forms,
  // Utils
  uFileUtils, uPathUtils;

procedure AnalyzeStartupParams; overload;
procedure AnalyzeStartupParams(const StartupParams: array of string); overload;

implementation

uses
  uMain;

procedure AnalyzeStartupParams;
var
  I: integer;
  Params: array of string;
begin
  for I := 1 to ParamCount do
  begin
    SetLength(Params, I);
    Params[I - 1] := ParamStr(I);
  end;

  AnalyzeStartupParams(Params);
end;

procedure AnalyzeStartupParams(const StartupParams: array of string);
const
  Params: array [0 .. 0] of string = ('openfile');
var
  I, J: integer;
begin

  for I := 0 to Length(StartupParams) - 1 do
    for J := 0 to Length(Params) - 1 do
      if (StartupParams[I] = '/' + Params[J]) or (StartupParams[I] = '-' + Params[J]) or (StartupParams[I] = Params[J]) then
        case J of
          0:
            begin
              if ((I + 1) < Length(StartupParams)) then
                Main.fMain.OpenToNewTab(StartupParams[I + 1])
              else
                Main.fMain.OpenToNewTab();
            end;
        end;
end;

end.
