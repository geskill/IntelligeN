unit uMyfsScript;

interface

uses
  // Delphi
  Variants,
  // FastScript
  fs_iInterpreter;

type
  TMyfsScript = class(TfsScript)
  public
    function CallFunction3(const Name: String; const Params: Variant; sGlobal: Boolean = false): Variant;
  end;

implementation

{ TMyfsScript }

function TMyfsScript.CallFunction3(const Name: String; const Params: Variant; sGlobal: Boolean): Variant;
var
  v: TfsCustomVariable;
  p: TfsProcVariable;
begin
  if sGlobal then
    v := Find(Name)
  else
    v := FindLocal(Name);
  if (v <> nil) and (v is TfsProcVariable) then
  begin
    p := TfsProcVariable(v);

    if VarIsArray(Params) then
        p.Params[0].Value := Params;
    Result := p.Value;
  end
  else
  begin
    Result := Null;
  end
end;

end.
