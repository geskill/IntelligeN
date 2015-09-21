unit uHTTPEvent;

interface

uses
  // Interface
  uHTTPInterface,
  // MultiEvent
  Generics.MultiEvents.Event, Generics.MultiEvents.Handler,
  // Delphi
  Generics.Collections;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Event method for sending a full HTTP process.
  ///	</summary>
  ///	<param name="AHTTPProcess">
  ///	  HTTP process
  ///	</param>
  {$ENDREGION}
  THTTPProcessMethod = procedure(const AHTTPProcess: IHTTPProcess) of object;

  TIHTTPProcessEventHandler = class(TGenericEventHandler<THTTPProcessMethod>, IHTTPProcessEventHandler)
  public
    procedure Invoke(const AHTTPProcess: IHTTPProcess); safecall;
  end;

  TIHTTPProcessEvent = class(TGenericEvent<IHTTPProcessEventHandler>, IHTTPProcessEvent)
  public
    procedure Invoke(const AHTTPProcess: IHTTPProcess); safecall;
  end;

implementation

{ TIHTTPProcessEventHandler }

procedure TIHTTPProcessEventHandler.Invoke(const AHTTPProcess: IHTTPProcess);
begin
  if (@FHandler <> nil) then
    FHandler(AHTTPProcess);
end;

{ TIHTTPProcessEvent }

procedure TIHTTPProcessEvent.Invoke(const AHTTPProcess: IHTTPProcess);
var
  HTTPProcessEventHandler: IHTTPProcessEventHandler;
begin
  for HTTPProcessEventHandler in Methods do
    HTTPProcessEventHandler.Invoke(AHTTPProcess);
end;

end.
