unit uReleasenameUtils;

interface

uses
  // Delphi
  Windows, SysUtils,
  // RegEx
  RegExpr;

type
  TReleasenameUtils = class
  public
    class function IsSeries(const AReleaseName: string): Boolean;
  end;

implementation

{ TReleasenameUtils }

class function TReleasenameUtils.IsSeries(const AReleaseName: string): Boolean;
const
  expr: array [0 .. 6] of string = ('\.S(\d+)\.', '\.S(\d+)E(\d+)\.', '\.E(\d+)\.', '\.season', '\.episode', '\.serie', '\.staffel');
var
  I: Integer;
begin
  with TRegExpr.Create do
    try
      InputString := AReleaseName;

      for I := 0 to length(expr) - 1 do
      begin
        Expression := expr[I];

        if Exec(InputString) then
          Exit(True);
      end;
    finally
      Free;
    end;

  result := False;
end;

end.
