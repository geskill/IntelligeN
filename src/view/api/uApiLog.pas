unit uApiLog;

interface

uses
  // Delphi
  Windows, SysUtils,
  // Common
  uAppInterface;

type
  TLog = class(TInterfacedObject, ILog)
  private
    FTime: TDateTime;
    FMessage: WideString;
  protected
    function GetTime: TDateTime; safecall;
    function GetMessage: WideString; safecall;
  public
    constructor Create(const AMessage: WideString); overload;
    constructor Create(const ATime: TDateTime; const AMessage: WideString); overload;

    property Time: TDateTime read GetTime;
    property Message: WideString read GetMessage;
  end;

implementation

{ TLog }

constructor TLog.Create(const AMessage: WideString);
begin
  Create(Now, AMessage);
end;

constructor TLog.Create(const ATime: TDateTime; const AMessage: WideString);
begin
  FTime := ATime;
  FMessage := AMessage;
end;

function TLog.GetTime: TDateTime;
begin
  Result := FTime;
end;

function TLog.GetMessage: WideString;
begin
  Result := FMessage;
end;

end.
