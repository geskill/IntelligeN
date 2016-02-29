unit uHTTPExtensionManager;

interface

uses
  // Interface
  uHTTPInterface,
  // Classes
  uHTTPManagerClasses, uHTTPClasses,
  // Const
  uHTTPConst,
  // Events
  uHTTPEvent,
  // Delphi
  Windows, SysUtils, Classes;

type
  THTTPExtensionManager = class(TInterfacedObject, IHTTPExtensionManager)
  private
    FExtensions: TInterfaceList;
  protected
    function FindExtension(const AName: WideString): IHTTPExtension; safecall;
    function GetCount: Integer; safecall;
    function GetExtension(AIndex: Integer): IHTTPExtension; safecall;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Register(const AHTTPExtension: IHTTPExtension): WordBool; safecall;
    function Unregister(const AName: WideString): WordBool; safecall;

    property Count: Integer read GetCount;
    property Extension[AIndex: Integer]: IHTTPExtension read GetExtension; default;
  end;

  THTTPAntiScrapeManager = class(THTTPExtensionManager, IHTTPAntiScrapeManager)
  protected
    function GetExtension(AIndex: Integer): IHTTPAntiScrape; safecall;
  public
    function Register(const AAntiScrape: IHTTPAntiScrape): WordBool; safecall;

    property Count;
    property Extension[AIndex: Integer]: IHTTPAntiScrape read GetExtension; default;
  end;

  THTTPImplementationManager = class(THTTPExtensionManager, IHTTPImplementationManager)
  protected
    function GetExtension(AIndex: Integer): IHTTPImplementation; safecall;
  public
    function Register(const AImplementation: IHTTPImplementation): WordBool; safecall;

    property Count;
    property Extension[AIndex: Integer]: IHTTPImplementation read GetExtension; default;
  end;

implementation

{ THTTPExtensionManager<T> }

function THTTPExtensionManager.FindExtension(const AName: WideString): IHTTPExtension;
var
  LExtensionIndex: Integer;
  LExtension: IHTTPExtension;
begin
  Result := nil;

  for LExtensionIndex := 0 to FExtensions.Count - 1 do
  begin
    LExtension := Extension[LExtensionIndex];

    if SameText(AName, LExtension.Name) then
    begin
      Result := LExtension;
      break;
    end;
  end;
end;

function THTTPExtensionManager.GetCount: Integer;
begin
  Result := FExtensions.Count;
end;

function THTTPExtensionManager.GetExtension(AIndex: Integer): IHTTPExtension;
begin
  Result := FExtensions.Items[AIndex] as IHTTPExtension;
end;

constructor THTTPExtensionManager.Create;
begin
  inherited Create;
  FExtensions := TInterfaceList.Create;
end;

destructor THTTPExtensionManager.Destroy;
begin
  FExtensions.Free;
  inherited Destroy;
end;

function THTTPExtensionManager.Register(const AHTTPExtension: IHTTPExtension): WordBool;
begin
  Result := not Assigned(FindExtension(AHTTPExtension.Name));
  if Result then
    FExtensions.Add(AHTTPExtension);
end;

function THTTPExtensionManager.Unregister(const AName: WideString): WordBool;
var
  LExtension: IHTTPExtension;
begin
  LExtension := FindExtension(AName);
  Result := Assigned(LExtension) and not LExtension.InUse;
  if Result then
    try
      FExtensions.Remove(LExtension);
    finally
      LExtension := nil;
    end;
end;

{ THTTPAntiScrapeManager }

function THTTPAntiScrapeManager.GetExtension(AIndex: Integer): IHTTPAntiScrape;
begin
  Result := inherited GetExtension(AIndex) as IHTTPAntiScrape;
end;

function THTTPAntiScrapeManager.Register(const AAntiScrape: IHTTPAntiScrape): WordBool;
begin
  Result := inherited Register(AAntiScrape);
end;

{ THTTPImplementationManager }

function THTTPImplementationManager.GetExtension(AIndex: Integer): IHTTPImplementation;
begin
  Result := inherited GetExtension(AIndex) as IHTTPImplementation;
end;

function THTTPImplementationManager.Register(const AImplementation: IHTTPImplementation): WordBool;
begin
  Result := inherited Register(AImplementation);
end;

end.
