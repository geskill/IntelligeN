{ ********************************************************
  *                                                      *
  *  MultiEvents - Interface based MultiCastEvents       *
  *                                                      *
  *  NotifyEvent                                         *
  *  Version 1.0.0.0                                     *
  *  Copyright (c) 2013 Sebastian Klatte                 *
  *  Mail sebastianklatte(at)gmx(dot)net                 *
  *                                                      *
  ******************************************************** }
unit Generics.MultiEvents.NotifyEvent;

interface

uses
  // MultiEvent
  Generics.MultiEvents.Event,
  Generics.MultiEvents.NotifyInterface;

type
  TINotifyEvent = class(TGenericEvent<INotifyEventHandler>, INotifyEvent)
  public
    procedure Invoke(const Sender: IUnknown); safecall;
  end;

implementation

{ TINotifyEvent }

procedure TINotifyEvent.Invoke(const Sender: IUnknown);
var
  LNotifyEventHandler: INotifyEventHandler;
begin
  for LNotifyEventHandler in Methods do
    LNotifyEventHandler.Invoke(Sender);
end;

end.
