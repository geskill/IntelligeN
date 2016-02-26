unit uHTTPAntiScrape;

interface

uses
  // Interface
  uHTTPInterface;

type
  THTTPAntiScrape = class abstract(TInterfacedObject, IHTTPAntiScrape)
  protected
    function GetName: WideString; safecall;
  public
    class function GetAntiScrapeName: string; virtual; abstract;

    procedure Handle(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool); virtual; safecall; abstract;

    property Name: WideString read GetName;
  end;

implementation

{ THTTPAntiScrape }

function THTTPAntiScrape.GetName: WideString;
begin
  Result := GetAntiScrapeName;
end;

end.
