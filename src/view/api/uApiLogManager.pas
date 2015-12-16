unit uApiLogManager;

interface

uses
  // Delphi
  Windows, SysUtils,
  // OmniThreadLibrary
  OtlCommon, OtlSync,
  // MultiEvent
  Generics.MultiEvents.Event, Generics.MultiEvents.Handler,
  // Api
  uApiLog,
  // Common
  uAppInterface;

type
  TLogMethod = procedure(const ALog: ILog) of object;

  TILogEventHandler = class(TGenericEventHandler<TLogMethod>, ILogEventHandler)
  public
    procedure Invoke(const ALog: ILog); safecall;
  end;

  TILogEvent = class(TGenericEvent<ILogEventHandler>, ILogEvent)
  public
    procedure Invoke(const ALog: ILog); safecall;
  end;

  TLogManager = class(TInterfacedObject, ILogManager)
  strict private
    class var FLock: TOmniCS;
  private
    FNewLogEvent: ILogEvent;

    class var FLogManager: ILogManager;

    constructor Create;
  protected
    function GetNewLog: ILogEvent; safecall;
  public
    class function Instance(): ILogManager;
    class destructor Destroy;

    procedure Add(const AMessage: WideString); safecall;

    property OnNewLog: ILogEvent read GetNewLog;

    destructor Destroy; override;
  end;

implementation

{ TILogEventHandler }

procedure TILogEventHandler.Invoke(const ALog: ILog);
begin
  if (@FHandler <> nil) then
    FHandler(ALog);
end;

{ TILogEvent }

procedure TILogEvent.Invoke(const ALog: ILog);
var
  LLogEventHandler: ILogEventHandler;
begin
  for LLogEventHandler in Methods do
    LLogEventHandler.Invoke(ALog);
end;

{ TLogManager }

constructor TLogManager.Create;
begin
  FNewLogEvent := TILogEvent.Create;
end;

function TLogManager.GetNewLog: ILogEvent;
begin
  Result := FNewLogEvent;
end;

class function TLogManager.Instance: ILogManager;
begin
  if Assigned(FLogManager) then
    Exit(FLogManager);

  FLock.Acquire;
  try
    if not Assigned(FLogManager) then
      FLogManager := TLogManager.Create;
    Result := FLogManager;
  finally
    FLock.Release;
  end;
end;

class destructor TLogManager.Destroy;
begin
  FLogManager := nil;
end;

procedure TLogManager.Add(const AMessage: WideString);
var
  LLog: ILog;
begin
  LLog := TLog.Create(AMessage);
  FNewLogEvent.Invoke(LLog);
end;

destructor TLogManager.Destroy;
begin
  FNewLogEvent := nil;
  inherited Destroy;
end;

end.
