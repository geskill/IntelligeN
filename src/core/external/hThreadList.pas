unit hThreadList;

  (****************************************************** )
  (                                                       )
  (  Copyright (c) 1997-2011 FNS Enterprize's™            )
  (                2003-2011 himitsu @ Delphi-PRAXiS      )
  (                                                       )
  (  Description   generic TThreadList/TObjectThreadList  )
  (  Filename      hThreadList.pas                        )
  (  Version       v1.1                                   )
  (  Date          15.12.2011                             )
  (  InitialDate   17.12.2011                             )
  (  System        Win32                                  )
  (  Personality   Delphi 2010-XE                         )
  (  Project       none, but included in hCodes           )
  (  Support       http://geheimniswelten.de/ThreadList   )
  (                                                       )
  (  License       MPL v1.1 , GPL v3.0 or LGPL v3.0       )
  (                                                       )
  (  Donation      http://geheimniswelten.de/Spenden      )
  (                                                       )
  (*******************************************************)

interface

uses
  RTLConsts, SysUtils, Classes, SyncObjs, Generics.Collections, TypInfo;

type
  TThreadList<T> = class(TEnumerable<T>)
  public type
    TListItem = T;
    TListObj  = class(TList<TListItem>)
    private
      FOwns: Boolean;
    public
      procedure Notify(const Item: TListItem; Action: TCollectionNotification); override;
    end;
    TListEnum = class(TEnumerator<TListItem>)
    private
      FList:  TThreadList<TListItem>;
      FIndex: Integer;
    protected
      function DoGetCurrent: TListItem; override;
      function DoMoveNext:   Boolean;   override;
    public
      constructor Create(List: TThreadList<TListItem>);
      destructor  Destroy; override;
    end;
  private
    FList: TListObj;
    FLock: TCriticalSection;
  protected
    function DoGetEnumerator: TEnumerator<TListItem>; override;
  public
    constructor Create(OwnsObjects: Boolean);
    destructor  Destroy; override;
    function  OwnsObjects: Boolean;
    procedure Add     (const Item: TListItem; Duplicates: TDuplicates = dupIgnore);
    procedure Remove  (const Item: TListItem);
    function  Contains(const Item: TListItem): Boolean;
    function  Count: Integer;
    procedure Clear;
    function  LockList: TListObj;
    procedure UnlockList;
  end;

implementation

procedure TThreadList<T>.TListObj.Notify(const Item: TListItem; Action: TCollectionNotification);
var
  I: PTypeInfo;
  D: PTypeData;
begin
  if (Action = cnRemoved) and FOwns then begin
    I := PTypeInfo(TypeInfo(TListItem));
    D := PTypeData(NativeInt(I) + SizeOf(TTypeInfo));
    case I.Kind of
      tkClass:  PObject(@Item).Free;
      tkMethod: if D.MethodKind in [mkProcedure, mkFunction] then
                  TObject(TMethod(Pointer(@Item)^).Data).Free;
    end;
  end;
end;

function TThreadList<T>.TListEnum.DoGetCurrent: TListItem;
begin
  Result := FList.FList[FIndex];
end;

function TThreadList<T>.TListEnum.DoMoveNext: Boolean;
begin
  if FIndex >= FList.FList.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.FList.Count;
end;

constructor TThreadList<T>.TListEnum.Create(List: TThreadList<TListItem>);
begin
  inherited Create;
  FList  := List;
  FIndex := -1;
  FList.LockList;
end;

destructor TThreadList<T>.TListEnum.Destroy;
begin
  FList.UnlockList;
  inherited;
end;

function TThreadList<T>.DoGetEnumerator: TEnumerator<TListItem>;
begin
  Result := TListEnum.Create(Self);
end;

constructor TThreadList<T>.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FLock       := TCriticalSection.Create;
  FList       := TListObj.Create;
  FList.FOwns := OwnsObjects;
end;

destructor TThreadList<T>.Destroy;
begin
  LockList;
  try
    if OwnsObjects then
      Clear;
  finally
    FreeAndNil(FList);
    UnlockList;
    FreeAndNil(FLock);
  end;
  inherited;
end;

function TThreadList<T>.OwnsObjects: Boolean;
begin
  Result := FList.FOwns;
end;

procedure TThreadList<T>.Add(const Item: TListItem; Duplicates: TDuplicates);
begin
  FLock.Enter;
  try
    if (Duplicates = dupAccept) or (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
    else if Duplicates = dupError then
      raise EListError.CreateFmt(SDuplicateItem, [NativeInt(Pointer(@Item)^)]);
  finally
    FLock.Leave;
  end;
end;

procedure TThreadList<T>.Remove(const Item: TListItem);
begin
  FLock.Enter;
  try
    FList.Remove(Item);
  finally
    FLock.Leave;
  end;
end;

function TThreadList<T>.Contains(const Item: TListItem): Boolean;
begin
  FLock.Enter;
  try
    Result := FList.Contains(Item);
  finally
    FLock.Leave;
  end;
end;

function TThreadList<T>.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FList.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadList<T>.Clear;
begin
  FLock.Enter;
  try
    while FList.Count > 0 do
      Remove(FList[FList.Count - 1]);
  finally
    FLock.Leave;
  end;
end;

function TThreadList<T>.LockList: TListObj;
begin
  FLock.Enter;
  Result := FList;
end;

procedure TThreadList<T>.UnlockList;
begin
  FLock.Leave;
end;

end.

