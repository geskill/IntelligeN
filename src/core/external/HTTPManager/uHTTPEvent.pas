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
  THTTPScrapeMethod = procedure(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool) of object;
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Event method for sending a full HTTP process.
  ///	</summary>
  ///	<param name="AHTTPProcess">
  ///	  HTTP process
  ///	</param>
  {$ENDREGION}
  THTTPProcessMethod = procedure(const AHTTPProcess: IHTTPProcess) of object;

  TIHTTPScrapeEventHandler = class(TGenericEventHandler<THTTPScrapeMethod>, IHTTPScrapeEventHandler)
  public
    procedure Invoke(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool); safecall;
  end;

  TIHTTPScrapeEvent = class(TGenericEvent<IHTTPScrapeEventHandler>, IHTTPScrapeEvent)
  public
    procedure Invoke(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool); safecall;
  end;

  TIHTTPProcessEventHandler = class(TGenericEventHandler<THTTPProcessMethod>, IHTTPProcessEventHandler)
  public
    procedure Invoke(const AHTTPProcess: IHTTPProcess); safecall;
  end;

  TIHTTPProcessEvent = class(TGenericEvent<IHTTPProcessEventHandler>, IHTTPProcessEvent)
  public
    procedure Invoke(const AHTTPProcess: IHTTPProcess); safecall;
  end;

implementation

{ TIHTTPScrapeEventHandler }

procedure TIHTTPScrapeEventHandler.Invoke(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool);
begin
  if (@FHandler <> nil) then
    FHandler(AHTTPProcess, AHTTPData, AHandled);
end;

{ TIHTTPScrapeEvent }

procedure TIHTTPScrapeEvent.Invoke(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool);
var
  HTTPScrapeEventHandler: IHTTPScrapeEventHandler;
begin
  for HTTPScrapeEventHandler in Methods do
    if not AHandled then
      HTTPScrapeEventHandler.Invoke(AHTTPProcess, AHTTPData, AHandled);
end;

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
