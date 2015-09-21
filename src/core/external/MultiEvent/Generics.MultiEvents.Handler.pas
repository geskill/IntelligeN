{ ********************************************************
  *                                                      *
  *  MultiEvents - Interface based MultiCastEvents       *
  *                                                      *
  *  Generic EventHandler                                *
  *  Version 1.0.0.0                                     *
  *  Copyright (c) 2013 Sebastian Klatte                 *
  *  Mail sebastianklatte(at)gmx(dot)net                 *
  *                                                      *
  ******************************************************** }
unit Generics.MultiEvents.Handler;

interface

type
  TGenericEventHandler<T: constructor> = class abstract(TInterfacedObject)
  protected
    FHandler: T;
  public
    constructor Create(AEventHandler: T);
    property EventHandler: T read FHandler write FHandler;
    destructor Destroy; override;
  end;

implementation

{ TGenericEventHandler<T> }

constructor TGenericEventHandler<T>.Create(AEventHandler: T);
begin
  FHandler := AEventHandler;
end;

destructor TGenericEventHandler<T>.Destroy;
begin
  FHandler := nil;
  inherited Destroy;
end;

end.
