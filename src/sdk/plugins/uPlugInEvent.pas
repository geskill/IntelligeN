{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn events                                       *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInEvent;

interface

uses
  // MultiEvent
  Generics.MultiEvents.Handler,
  Generics.MultiEvents.NotifyInterface,
  // Common
  uBaseConst, uAppInterface;

type
  TViewChangeMethod = procedure(const NewViewType: TTabViewType) of object;

  TIViewChangeEventHandler = class(TGenericEventHandler<TViewChangeMethod>, IViewChangeEventHandler)
  public
    procedure Invoke(const NewViewType: TTabViewType); safecall;
  end;

  TCaptionChangeMethod = procedure(const ACaption: WideString) of object;

  TICaptionChangeEventHandler = class(TGenericEventHandler<TCaptionChangeMethod>, ICaptionChangeEventHandler)
  public
    procedure Invoke(const NewCaption: WideString); safecall;
  end;

  TITabSheetMethod = procedure(const Sender: ITabSheetController) of object;

  TITabSheetEventHandler = class(TGenericEventHandler<TITabSheetMethod>, ITabSheetEventHandler)
  public
    procedure Invoke(const Sender: ITabSheetController); safecall;
  end;

  TControlMethod = procedure(const Sender: IControlBasic) of object;

  TIControlEventHandler = class(TGenericEventHandler<TControlMethod>, IControlEventHandler)
  public
    procedure Invoke(const Sender: IControlBasic); safecall;
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

procedure TIViewChangeEventHandler.Invoke(const NewViewType: TTabViewType);
begin
  if (@FHandler <> nil) then
    FHandler(NewViewType);
end;

{ TICaptionChangeEventHandler }

procedure TICaptionChangeEventHandler.Invoke(const NewCaption: WideString);
begin
  if (@FHandler <> nil) then
    FHandler(NewCaption);
end;

{ TITabSheetEventHandler }

procedure TITabSheetEventHandler.Invoke(const Sender: ITabSheetController);
begin
  if (@FHandler <> nil) then
    FHandler(Sender);
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
