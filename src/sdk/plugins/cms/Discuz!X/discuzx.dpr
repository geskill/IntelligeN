library discuzx;

{$R *.dres}

uses
  uPlugInInterface,
  uPlugInCMSClass,
  uDiscuzX in 'uDiscuzX.pas';

{$R *.res}

function LoadPlugin(var APlugIn: ICMSPlugIn): WordBool; safecall; export;
begin
  try
    APlugIn := TDiscuzX.Create;
    Result := True;
  except
    Result := False;
  end;
end;

exports
  LoadPlugIn name 'LoadPlugIn';

begin
end.
