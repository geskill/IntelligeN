{ ********************************************************
  *                                                      *
  *  MultiEvents - Interface based MultiCastEvents       *
  *                                                      *
  *  Generic Event                                       *
  *  Version 1.0.0.0                                     *
  *  Copyright (c) 2013 Sebastian Klatte                 *
  *  Mail sebastianklatte(at)gmx(dot)net                 *
  *                                                      *
  ******************************************************** }
unit Generics.MultiEvents.Event;

interface

uses
  // Delphi
  Generics.Collections;

type
  TGenericEvent<T: IUnknown> = class abstract(TInterfacedObject)
  private
    FMethods: TList<T>;
  protected
    property Methods: TList<T>read FMethods;
  public
    constructor Create; virtual;
    procedure Add(const AHandler: T); safecall;
    procedure Remove(const AHandler: T); safecall;
    destructor Destroy; override;
  end;

implementation

{ TGenericEvent<T> }

constructor TGenericEvent<T>.Create;
begin
  inherited Create;
  FMethods := TList<T>.Create;
end;

procedure TGenericEvent<T>.Add(const AHandler: T);
begin
  if (FMethods.IndexOf(AHandler) = -1) then
    FMethods.Add(AHandler);
end;

procedure TGenericEvent<T>.Remove(const AHandler: T);
begin
  if not(FMethods.IndexOf(AHandler) = -1) then
    FMethods.Remove(AHandler);
end;

destructor TGenericEvent<T>.Destroy;
begin
  FMethods.Free;
  inherited Destroy;
end;

end.
