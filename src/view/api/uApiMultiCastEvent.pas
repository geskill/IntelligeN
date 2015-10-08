unit uApiMultiCastEvent;

interface

uses
  // MultiEvent
  Generics.MultiEvents.Event,
  Generics.MultiEvents.Handler,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface;

type
  TIViewChangeEvent = class(TGenericEvent<IViewChangeEventHandler>, IViewChangeEvent)
  public
    procedure Invoke(const ANewViewType: TTabViewType); safecall;
  end;

  TIControlChangeEvent = class(TGenericEvent<IControlEventHandler>, IControlChangeEvent)
  public
    procedure Invoke(const ASender: IControlBasic); safecall;
  end;

  TIUpdateCMSListEvent = class(TGenericEvent<IUpdateCMSListEventHandler>, IUpdateCMSListEvent)
  public
    procedure Invoke(const ASender: IPublishController); safecall;
  end;

  TIUpdateCMSWebsiteListEvent = class(TGenericEvent<IUpdateCMSWebsiteListEventHandler>, IUpdateCMSWebsiteListEvent)
  public
    procedure Invoke(const ASender: ICMSContainer; ACMSIndex: Integer); safecall;
  end;

  TIUpdateCMSWebsiteEvent = class(TGenericEvent<IUpdateCMSWebsiteEventHandler>, IUpdateCMSWebsiteEvent)
  public
    procedure Invoke(ACMSIndex, AWebsiteIndex: Integer; ANewStatus: WordBool); safecall;
  end;

  ///

  TPluginChangeType = (pctAdd, pctMove, pctDelete, pctEnabled);

  IPluginChangeEventHandler = interface(IUnknown)
    ['{854A94D1-B52C-457B-89E9-9E30948BA9C3}']
    procedure Invoke(PluginChangeType: TPluginChangeType; Index: Integer; Param: Integer); safecall;
  end;

  IPluginChangeEvent = interface(IUnknown)
    ['{5ED70CAE-A54D-479F-BEE0-349D2A20F1BE}']
    procedure Add(const AHandler: IPluginChangeEventHandler); safecall;
    procedure Remove(const AHandler: IPluginChangeEventHandler); safecall;
    procedure Invoke(APluginChangeType: TPluginChangeType; AIndex: Integer; AParam: Integer); safecall;
  end;

  TPluginChangeMethod = procedure(PluginChangeType: TPluginChangeType; Index: Integer; Param: Integer) of object;

  TIPluginChangeEventHandler = class(TGenericEventHandler<TPluginChangeMethod>, IPluginChangeEventHandler)
  public
    procedure Invoke(PluginChangeType: TPluginChangeType; Index: Integer; Param: Integer); safecall;
  end;

  TIPluginChangeEvent = class(TGenericEvent<IPluginChangeEventHandler>, IPluginChangeEvent)
  public
    procedure Invoke(APluginChangeType: TPluginChangeType; AIndex: Integer; AParam: Integer); safecall;
  end;

  TCMSItemChangeType = (cctAdd, cctChange, cctDelete, cctEnabled);

  ICMSItemChangeEventHandler = interface(IUnknown)
    ['{D25F38E2-68E3-4538-9605-3C80D1B6AD9D}']
    procedure Invoke(CMSItemChangeType: TCMSItemChangeType; Index: Integer; Param: Integer); safecall;
  end;

  ICMSItemChangeEvent = interface(IUnknown)
    ['{B6930447-2460-408D-8509-CECFFC3532D4}']
    procedure Add(const AHandler: ICMSItemChangeEventHandler); safecall;
    procedure Remove(const AHandler: ICMSItemChangeEventHandler); safecall;
    procedure Invoke(ACMSItemChangeType: TCMSItemChangeType; AIndex: Integer; AParam: Integer); safecall;
  end;

  TCMSItemChangeMethod = procedure(CMSItemChangeType: TCMSItemChangeType; Index: Integer; Param: Integer) of object;

  TICMSItemChangeEventHandler = class(TGenericEventHandler<TCMSItemChangeMethod>, ICMSItemChangeEventHandler)
  public
    procedure Invoke(CMSItemChangeType: TCMSItemChangeType; Index: Integer; Param: Integer); safecall;
  end;

  TICMSItemChangeEvent = class(TGenericEvent<ICMSItemChangeEventHandler>, ICMSItemChangeEvent)
  public
    procedure Invoke(ACMSItemChangeType: TCMSItemChangeType; AIndex: Integer; AParam: Integer); safecall;
  end;

implementation

{ TIViewChangeEvent }

procedure TIViewChangeEvent.Invoke(const ANewViewType: TTabViewType);
var
  LViewChangeEvent: IViewChangeEventHandler;
begin
  for LViewChangeEvent in Methods do
    LViewChangeEvent.Invoke(ANewViewType);
end;

{ TIControlChangeEvent }

procedure TIControlChangeEvent.Invoke(const ASender: IControlBasic);
var
  LControlEvent: IControlEventHandler;
begin
  for LControlEvent in Methods do
    LControlEvent.Invoke(ASender);
end;

{ TUpdateCMSListMultiCastEvent }

procedure TIUpdateCMSListEvent.Invoke(const ASender: IPublishController);
var
  LUpdateCMSListEvent: IUpdateCMSListEventHandler;
begin
  for LUpdateCMSListEvent in Methods do
    LUpdateCMSListEvent.Invoke(ASender);
end;

{ TUpdateCMSWebsiteListMultiCastEvent }

procedure TIUpdateCMSWebsiteListEvent.Invoke(const ASender: ICMSContainer; ACMSIndex: Integer);
var
  LUpdateCMSWebsiteListEvent: IUpdateCMSWebsiteListEventHandler;
begin
  for LUpdateCMSWebsiteListEvent in Methods do
    LUpdateCMSWebsiteListEvent.Invoke(ASender, ACMSIndex);
end;

{ TUpdateCMSWebsiteMultiCastEvent }

procedure TIUpdateCMSWebsiteEvent.Invoke(ACMSIndex, AWebsiteIndex: Integer; ANewStatus: WordBool);
var
  LUpdateCMSWebsiteEvent: IUpdateCMSWebsiteEventHandler;
begin
  for LUpdateCMSWebsiteEvent in Methods do
    LUpdateCMSWebsiteEvent.Invoke(ACMSIndex, AWebsiteIndex, ANewStatus);
end;

{ TIPluginChangeEventHandler }

procedure TIPluginChangeEventHandler.Invoke(PluginChangeType: TPluginChangeType; Index, Param: Integer);
begin
  if (@FHandler <> nil) then
    FHandler(PluginChangeType, Index, Param);
end;

{ TIPluginChangeEvent }

procedure TIPluginChangeEvent.Invoke(APluginChangeType: TPluginChangeType; AIndex, AParam: Integer);
var
  LPluginChangeEventHandler: IPluginChangeEventHandler;
begin
  for LPluginChangeEventHandler in Methods do
    LPluginChangeEventHandler.Invoke(APluginChangeType, AIndex, AParam);
end;

{ TICMSItemChangeEventHandler }

procedure TICMSItemChangeEventHandler.Invoke(CMSItemChangeType: TCMSItemChangeType; Index, Param: Integer);
begin
  if (@FHandler <> nil) then
    FHandler(CMSItemChangeType, Index, Param);
end;

{ TICMSItemChangeEvent }

procedure TICMSItemChangeEvent.Invoke(ACMSItemChangeType: TCMSItemChangeType; AIndex, AParam: Integer);
var
  LCMSItemChangeEventHandler: ICMSItemChangeEventHandler;
begin
  for LCMSItemChangeEventHandler in Methods do
    LCMSItemChangeEventHandler.Invoke(ACMSItemChangeType, AIndex, AParam);
end;

end.
