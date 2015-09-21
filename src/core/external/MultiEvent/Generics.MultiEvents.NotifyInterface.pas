{ ********************************************************
  *                                                      *
  *  MultiEvents - Interface based MultiCastEvents       *
  *                                                      *
  *  NotifyInterface                                     *
  *  Version 1.0.0.0                                     *
  *  Copyright (c) 2013 Sebastian Klatte                 *
  *  Mail sebastianklatte(at)gmx(dot)net                 *
  *                                                      *
  ******************************************************** }
unit Generics.MultiEvents.NotifyInterface;

interface

type
  INotifyEventHandler = interface(IUnknown)
    ['{EE9407DD-2337-4DFC-BC35-A40C4FA0A1A7}']
    procedure Invoke(const Sender: IUnknown); safecall;
  end;

  INotifyEvent = interface(IUnknown)
    ['{14551B63-78C4-4A70-9E54-7656CEF4D6A7}']
    procedure Add(const AHandler: INotifyEventHandler); safecall;
    procedure Remove(const AHandler: INotifyEventHandler); safecall;
    procedure Invoke(const Sender: IUnknown); safecall;
  end;

implementation

end.
