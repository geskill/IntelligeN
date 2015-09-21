{ ********************************************************
  *                                                      *
  *  MultiEvents - Interface based MultiCastEvents       *
  *                                                      *
  *  NotifyHandler                                       *
  *  Version 1.0.0.0                                     *
  *  Copyright (c) 2013 Sebastian Klatte                 *
  *  Mail sebastianklatte(at)gmx(dot)net                 *
  *                                                      *
  ******************************************************** }
unit Generics.MultiEvents.NotifyHandler;

interface

uses
  // MultiEvent
  Generics.MultiEvents.Handler,
  Generics.MultiEvents.NotifyInterface;

type
  TNotifyMethod = procedure(const Sender: IUnknown) of object;

  TINotifyEventHandler = class(TGenericEventHandler<TNotifyMethod>, INotifyEventHandler)
  public
    procedure Invoke(const Sender: IUnknown); safecall;
  end;

implementation

{ TINotifyEventHandler }

procedure TINotifyEventHandler.Invoke(const Sender: IUnknown);
begin
  if (@FHandler <> nil) then
    FHandler(Sender);
end;

end.
