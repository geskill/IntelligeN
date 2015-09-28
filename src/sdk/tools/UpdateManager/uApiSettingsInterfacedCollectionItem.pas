unit uApiSettingsInterfacedCollectionItem;

interface

uses
  // Delphi
  Windows, Classes;

type
  TInterfacedCollectionItem = class(TCollectionItem, IInterface)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

implementation

procedure TInterfacedCollectionItem.AfterConstruction;
begin

end;

procedure TInterfacedCollectionItem.BeforeDestruction;
begin

end;

class function TInterfacedCollectionItem.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TInterfacedCollectionItem(Result).FRefCount := 1;
end;

function TInterfacedCollectionItem.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := 0
end;

function TInterfacedCollectionItem._AddRef: Integer;
begin
  Result := -1;
end;

function TInterfacedCollectionItem._Release: Integer;
begin
  Result := -1;
end;

end.
