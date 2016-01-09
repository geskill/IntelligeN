unit uHTTPManagerClasses;

interface

uses
  // Interface
  uHTTPInterface;

type
  THTTPData = class(TInterfacedObject, IHTTPData)
  private
    FHTTPRequest: IHTTPRequest;
    FHTTPParams: IHTTPParams;
    FHTTPOptions: IHTTPOptions;
  protected
    function GetWebsite: WideString; safecall;
    function GetHTTPParams: IHTTPParams; safecall;
    procedure SetHTTPParams(const AHTTPParams: IHTTPParams); safecall;
    function GetHTTPRequest: IHTTPRequest; safecall;
    procedure SetHTTPRequest(const AHTTPRequest: IHTTPRequest); safecall;
    function GetHTTPOptions: IHTTPOptions; safecall;
    procedure SetHTTPOptions(const AHTTPOptions: IHTTPOptions); safecall;
  public
    constructor Create(const AHTTPRequest: IHTTPRequest; const AHTTPOptions: IHTTPOptions; const AHTTPParams: IHTTPParams); reintroduce; virtual;
    destructor Destroy; override;

    property Website: WideString read GetWebsite;
    property HTTPParams: IHTTPParams read GetHTTPParams write SetHTTPParams;
    property HTTPRequest: IHTTPRequest read GetHTTPRequest write SetHTTPRequest;
    property HTTPOptions: IHTTPOptions read GetHTTPOptions write SetHTTPOptions;
  end;

  THTTPResult = class(TInterfacedObject, IHTTPResult)
  private
    FHTTPResponse: IHTTPResponse;
    FHTTPResponseInfo: IHTTPResponseInfo;
  protected
    function GetSourceCode: WideString; safecall;
    function GetHasError: WordBool; safecall;
    function GetHTTPResponse: IHTTPResponse; safecall;
    function GetHTTPResponseInfo: IHTTPResponseInfo; safecall;
  public
    constructor Create(const AHTTPResponse: IHTTPResponse; const AHTTPResponseInfo: IHTTPResponseInfo); reintroduce; virtual;
    destructor Destroy; override;

    property SourceCode: WideString read GetSourceCode;
    property HasError: WordBool read GetHasError;
    property HTTPResponse: IHTTPResponse read GetHTTPResponse;
    property HTTPResponseInfo: IHTTPResponseInfo read GetHTTPResponseInfo;
  end;

  THTTPProcess = class(TInterfacedObject, IHTTPProcess)
  private
    FUniqueID: Double;
    FHTTPData: IHTTPData;
    FHTTPResult: IHTTPResult;
  protected
    function GetUniqueID: Double; safecall;
    function GetHTTPData: IHTTPData; safecall;
    procedure SetHTTPData(const AHTTPData: IHTTPData); safecall;
    function GetHTTPResult: IHTTPResult; safecall;
    procedure SetHTTPResult(const AHTTPResult: IHTTPResult); safecall;
  public
    constructor Create(const AUniqueID: Double); reintroduce; virtual;
    destructor Destroy; override;

    property UniqueID: Double read GetUniqueID;
    property HTTPData: IHTTPData read GetHTTPData write SetHTTPData;
    property HTTPResult: IHTTPResult read GetHTTPResult write SetHTTPResult;
  end;

implementation

{ THTTPData }

constructor THTTPData.Create(const AHTTPRequest: IHTTPRequest; const AHTTPOptions: IHTTPOptions; const AHTTPParams: IHTTPParams);
begin
  inherited Create;
  FHTTPRequest := AHTTPRequest;
  FHTTPOptions := AHTTPOptions;
  FHTTPParams := AHTTPParams;
end;

destructor THTTPData.Destroy;
begin
  FHTTPParams := nil;
  FHTTPOptions := nil;
  FHTTPRequest := nil;
  inherited Destroy;
end;

function THTTPData.GetWebsite: WideString;
begin
  Result := FHTTPRequest.URL;
end;

function THTTPData.GetHTTPParams: IHTTPParams;
begin
  Result := FHTTPParams;
end;

procedure THTTPData.SetHTTPParams(const AHTTPParams: IHTTPParams);
begin
  FHTTPParams := AHTTPParams;
end;

function THTTPData.GetHTTPRequest: IHTTPRequest;
begin
  Result := FHTTPRequest;
end;

procedure THTTPData.SetHTTPRequest(const AHTTPRequest: IHTTPRequest);
begin
  FHTTPRequest := AHTTPRequest;
end;

function THTTPData.GetHTTPOptions: IHTTPOptions;
begin
  Result := FHTTPOptions;
end;

procedure THTTPData.SetHTTPOptions(const AHTTPOptions: IHTTPOptions);
begin
  FHTTPOptions := AHTTPOptions;
end;

{ THTTPResult }

constructor THTTPResult.Create(const AHTTPResponse: IHTTPResponse; const AHTTPResponseInfo: IHTTPResponseInfo);
begin
  inherited Create;
  FHTTPResponse := AHTTPResponse;
  FHTTPResponseInfo := AHTTPResponseInfo;
end;

destructor THTTPResult.Destroy;
begin
  FHTTPResponseInfo := nil;
  FHTTPResponse := nil;
  inherited Destroy;
end;

function THTTPResult.GetSourceCode: WideString;
begin
  Result := FHTTPResponse.Content;
end;

function THTTPResult.GetHasError: WordBool;
begin
  Result := not(FHTTPResponseInfo.ErrorMessage = '');
end;

function THTTPResult.GetHTTPResponse: IHTTPResponse;
begin
  Result := FHTTPResponse;
end;

function THTTPResult.GetHTTPResponseInfo: IHTTPResponseInfo;
begin
  Result := FHTTPResponseInfo;
end;

{ THTTPRequest }

function THTTPProcess.GetUniqueID: Double;
begin
  Result := FUniqueID;
end;

function THTTPProcess.GetHTTPData: IHTTPData;
begin
  Result := FHTTPData;
end;

procedure THTTPProcess.SetHTTPData(const AHTTPData: IHTTPData);
begin
  FHTTPData := AHTTPData;
end;

function THTTPProcess.GetHTTPResult: IHTTPResult;
begin
  Result := FHTTPResult;
end;

procedure THTTPProcess.SetHTTPResult(const AHTTPResult: IHTTPResult);
begin
  FHTTPResult := AHTTPResult;
end;

constructor THTTPProcess.Create(const AUniqueID: Double);
begin
  inherited Create;
  FUniqueID := AUniqueID;
end;

destructor THTTPProcess.Destroy;
begin
  FHTTPResult := nil;
  FHTTPData := nil;
  inherited Destroy;
end;

end.
