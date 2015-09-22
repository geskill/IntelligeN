unit uPlugInEvent;

interface

uses
  // MultiEvent
  Generics.MultiEvents.Handler,
  Generics.MultiEvents.NotifyInterface,
  // Common
  uConst, uAppInterface;

type
  TViewChangeMethod = procedure(const NewViewType: TViewType) of object;

  TIViewChangeEventHandler = class(TGenericEventHandler<TViewChangeMethod>, IViewChangeEventHandler)
  public
    procedure Invoke(const NewViewType: TViewType); safecall;
  end;

  TControlMethod = procedure(const Sender: IBasic) of object;

  TIControlEventHandler = class(TGenericEventHandler<TControlMethod>, IControlEventHandler)
  public
    procedure Invoke(const Sender: IBasic); safecall;
  end;

  TUpdateCMSListMethod = procedure(const Sender: IPublishController) of object;

  TIUpdateCMSListEventHandler = class(TGenericEventHandler<TUpdateCMSListMethod>, IUpdateCMSListEventHandler)
  public
    procedure Invoke(const Sender: IPublishController); safecall;
  end;

  TUpdateCMSWebsiteListMethod = procedure(const Sender: ICMSContainer; CMSIndex: Integer) of object;

  TIUpdateCMSWebsiteListEventHandler = class(TGenericEventHandler<TUpdateCMSWebsiteListMethod>, IUpdateCMSWebsiteListEventHandler)
  public
    procedure Invoke(const Sender: ICMSContainer; CMSIndex: Integer); safecall;
  end;

  TUpdateCMSWebsiteMethod = procedure(CMSIndex, WebsiteIndex: Integer; NewStatus: WordBool) of object;

  TIUpdateCMSWebsiteEventHandler = class(TGenericEventHandler<TUpdateCMSWebsiteMethod>, IUpdateCMSWebsiteEventHandler)
  public
    procedure Invoke(CMSIndex, WebsiteIndex: Integer; NewStatus: WordBool); safecall;
  end;

implementation

{ TIViewChangeEvent }

procedure TIViewChangeEventHandler.Invoke(const NewViewType: TViewType);
begin
  if (@FHandler <> nil) then
    FHandler(NewViewType);
end;

{ TIControlEvent }

procedure TIControlEventHandler.Invoke;
begin
  if (@FHandler <> nil) then
    FHandler(Sender);
end;

{ TIUpdateCMSListEvent }

procedure TIUpdateCMSListEventHandler.Invoke(const Sender: IPublishController);
begin
  if (@FHandler <> nil) then
    FHandler(Sender);
end;

{ TIUpdateCMSWebsiteListEvent }

procedure TIUpdateCMSWebsiteListEventHandler.Invoke(const Sender: ICMSContainer; CMSIndex: Integer);
begin
  if (@FHandler <> nil) then
    FHandler(Sender, CMSIndex);
end;

{ TIUpdateCMSWebsiteEvent }

procedure TIUpdateCMSWebsiteEventHandler.Invoke(CMSIndex, WebsiteIndex: Integer; NewStatus: WordBool);
begin
  if (@FHandler <> nil) then
    FHandler(CMSIndex, WebsiteIndex, NewStatus);
end;

end.
