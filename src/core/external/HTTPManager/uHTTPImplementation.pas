unit uHTTPImplementation;

interface

uses
  // Interface
  uHTTPInterface;

type
  THTTPImplementation = class abstract(TInterfacedObject, IHTTPImplementation)
  protected
    function GetName: WideString; safecall;
  public
    class function GetImplementationName: string; virtual; abstract;

    procedure Handle(const AHTTPData: IHTTPData; out AHTTPResult: IHTTPResult); virtual; safecall; abstract;

    property Name: WideString read GetName;
  end;

implementation

{ THTTPImplementation }

function THTTPImplementation.GetName: WideString;
begin
  Result := GetImplementationName;
end;

end.
