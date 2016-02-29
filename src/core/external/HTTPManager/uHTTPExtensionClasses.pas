unit uHTTPExtensionClasses;

interface

uses
  // Interface
  uHTTPInterface,
  // Delphi
  Windows;

type
  THTTPExtension = class abstract(TInterfacedObject, IHTTPExtension)
  private
    FInUseCounter: Integer;
  protected
    procedure BeginUse;
    procedure EndUse;

    function GetInUse: WordBool; safecall;
    function GetName: WideString; safecall;
  public
    constructor Create;

    class function GetExtensionName: string; virtual; abstract;

    property Name: WideString read GetName;
  end;

  THTTPAntiScrape = class abstract(THTTPExtension, IHTTPAntiScrape)
  public
    procedure Handle(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool); virtual; safecall; abstract;

    property Name;
  end;

  THTTPImplementation = class abstract(THTTPExtension, IHTTPImplementation)
  public
    procedure Handle(const AHTTPData: IHTTPData; out AHTTPResult: IHTTPResult); virtual; safecall; abstract;

    property Name;
  end;

implementation

{ THTTPExtension }

procedure THTTPExtension.BeginUse;
begin
  InterlockedIncrement(FInUseCounter);
end;

procedure THTTPExtension.EndUse;
begin
  InterlockedDecrement(FInUseCounter);
end;

function THTTPExtension.GetInUse: WordBool;
begin
  Result := (FInUseCounter = 0);
end;

function THTTPExtension.GetName: WideString;
begin
  Result := GetExtensionName;
end;

constructor THTTPExtension.Create;
begin
  inherited Create;
  FInUseCounter := 0;
end;

end.
