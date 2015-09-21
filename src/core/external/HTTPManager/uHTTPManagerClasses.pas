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
    procedure SetHTTPParams(AHTTPParams: IHTTPParams); safecall;
    function GetHTTPRequest: IHTTPRequest; safecall;
    procedure SetHTTPRequest(AHTTPRequest: IHTTPRequest); safecall;
    function GetHTTPOptions: IHTTPOptions; safecall;
    procedure SetHTTPOptions(AHTTPOptions: IHTTPOptions); safecall;
  public
    constructor Create(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions; AHTTPParams: IHTTPParams);

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
    constructor Create(AHTTPResponse: IHTTPResponse; AHTTPResponseInfo: IHTTPResponseInfo);

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
    procedure SetHTTPData(AHTTPData: IHTTPData); safecall;
    function GetHTTPResult: IHTTPResult; safecall;
    procedure SetHTTPResult(AHTTPResult: IHTTPResult); safecall;
  public
    constructor Create(AUniqueID: Double);

    property UniqueID: Double read GetUniqueID;
    property HTTPData: IHTTPData read GetHTTPData write SetHTTPData;
    property HTTPResult: IHTTPResult read GetHTTPResult write SetHTTPResult;
  end;

implementation

{ THTTPData }

constructor THTTPData.Create(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions; AHTTPParams: IHTTPParams);
begin
  FHTTPRequest := AHTTPRequest;
  FHTTPOptions := AHTTPOptions;
  FHTTPParams := AHTTPParams;
end;

function THTTPData.GetWebsite: WideString;
begin
  Result := FHTTPRequest.URL;
end;

function THTTPData.GetHTTPParams: IHTTPParams;
begin
  Result := FHTTPParams;
end;

procedure THTTPData.SetHTTPParams(AHTTPParams: IHTTPParams);
begin
  FHTTPParams := AHTTPParams;
end;

function THTTPData.GetHTTPRequest: IHTTPRequest;
begin
  Result := FHTTPRequest;
end;

procedure THTTPData.SetHTTPRequest(AHTTPRequest: IHTTPRequest);
begin
  FHTTPRequest := AHTTPRequest;
end;

function THTTPData.GetHTTPOptions: IHTTPOptions;
begin
  Result := FHTTPOptions;
end;

procedure THTTPData.SetHTTPOptions(AHTTPOptions: IHTTPOptions);
begin
  FHTTPOptions := AHTTPOptions;
end;

{ THTTPResult }

constructor THTTPResult.Create(AHTTPResponse: IHTTPResponse; AHTTPResponseInfo: IHTTPResponseInfo);
begin
  FHTTPResponse := AHTTPResponse;
  FHTTPResponseInfo := AHTTPResponseInfo;
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

procedure THTTPProcess.SetHTTPData(AHTTPData: IHTTPData);
begin
  FHTTPData := AHTTPData;
end;

function THTTPProcess.GetHTTPResult: IHTTPResult;
begin
  Result := FHTTPResult;
end;

procedure THTTPProcess.SetHTTPResult(AHTTPResult: IHTTPResult);
begin
  FHTTPResult := AHTTPResult;
end;

constructor THTTPProcess.Create(AUniqueID: Double);
begin
  FUniqueID := AUniqueID;
end;

end.
