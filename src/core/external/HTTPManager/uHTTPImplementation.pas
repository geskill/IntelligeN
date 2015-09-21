unit uHTTPImplementation;

interface

uses
  // Interface
  uHTTPInterface;

type
  THTTPImplementation = class abstract(TInterfacedObject, IHTTPImplementation)
  protected
    function GetName: WideString; virtual; safecall; abstract;
  public
    procedure Handle(const AHTTPData: IHTTPData; out AHTTPResult: IHTTPResult); virtual; safecall; abstract;

    property Name: WideString read GetName;
  end;

implementation

end.
