{$REGION 'Documentation'}
/// <summary>
///   <para>
///     This is the main unit for managing everything.
///   </para>
///   <para>
///     Use only the interface <see cref="uHTTPInterface|IHTTPManager" /> for
///     distribution (i.e. for COM compatible DLLs).
///   </para>
/// </summary>
{$ENDREGION}
unit uHTTPManager;

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
  Windows, SysUtils, Classes, Math, ActiveX,
  // OmniThreadLibrary
  OtlCommon, OtlSync, OtlParallel;

type
  THTTPImplementationManager = class(TInterfacedObject, IHTTPImplementationManager)
  private
    FImplementations: TInterfaceList;
    function FindImplementation(const AName: WideString): IHTTPImplementation;
  protected
    function GetCount: Integer; safecall;
    function GetImplementation(AIndex: Integer): IHTTPImplementation; safecall;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Register(const AHTTPImplementation: IHTTPImplementation): WordBool; safecall;
    function Unregister(const AName: WideString): WordBool; safecall;

    property Count: Integer read GetCount;
    property Implementations[Index: Integer]: IHTTPImplementation read GetImplementation; default;
  end;
  {$REGION 'Documentation'}
  /// <summary>
  ///   <para>
  ///     This is the base class, which handles the queued access for the
  ///     HTTP-requests. Every GET/POST request gets a unique number. Through
  ///     this you can check via <see cref="HasResult">
  ///     HasResult(&lt;UniqueID&gt;)</see> whether a specific request is
  ///     full-filled or not. If the request is finished [i.e. <see cref="HasResult" />
  ///      = True], you can call <see cref="GetResult">
  ///     GetResult(&lt;UniqueID&gt;)</see> to access the HTTP-request
  ///     result.
  ///   </para>
  ///   <para>
  ///     Be sure to create (using the Instance function) the class before
  ///     you access it outside of your main application! You can customize
  ///     (override) the internal constructor <see cref="Create" /> for your
  ///     own needs (i.e. connect HTTP logging feature).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   This class is a singleton.
  /// </remarks>
  {$ENDREGION}

  THTTPManager = class(TInterfacedObject, IHTTPManager)
  strict private
    class var FLock: TOmniCS;
  private
    FBackgroundWorker: IOmniBackgroundWorker;
    FRequestArray: array of IHTTPProcess;
    FRequestArrayLock: TOmniMREW;
    FRequestArrayLowerBoundUpdate: Boolean;
    FRequestArrayLowerBound: TOmniAlignedInt32;
    FConnectionMaximum: TOmniAlignedInt32;
    FImplementor: IHTTPImplementation;
    FImplementationManager: IHTTPImplementationManager;
    FRequestScrape: IHTTPScrapeEvent;
    FRequestDoneEvent: IHTTPProcessEvent;

    procedure UpdateCapacity(const ARequestArrayLength: Integer);

    class var FHTTPManager: IHTTPManager;

    procedure Execute(const workItem: IOmniWorkItem);

    function DoRequest(AHTTPMethod: THTTPMethod; AURL: string; AFollowUp: Double; AHTTPOptions: IHTTPOptions = nil; AHTTPParams: IHTTPParams = nil): Double; overload;
    function DoRequest(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil; AHTTPParams: IHTTPParams = nil): Double; overload;

    constructor Create;
  protected
    function GetConnectionMaximum: Integer; safecall;
    procedure SetConnectionMaximum(const AConnectionMaximum: Integer); safecall;
    function GetImplementor: IHTTPImplementation; safecall;
    procedure SetImplementor(const AImplementor: IHTTPImplementation); safecall;
    function GetImplementationManager: IHTTPImplementationManager; safecall;
    function GetRequestScrape: IHTTPScrapeEvent; safecall;
    function GetRequestDone: IHTTPProcessEvent; safecall;
  public
    {$REGION 'Documentation'}
    /// <summary>
    ///   This returns the single instance to this class. If this class is not
    ///   created, this functions creates it.
    /// </summary>
    /// <returns>
    ///   The instance of this class.
    /// </returns>
    /// <remarks>
    ///   Be sure to create the class before you access it outside of your main
    ///   application! You can customize (override) the internal constructor <see cref="Create" />
    ///    for your own needs (i.e. connect HTTP logging feature).
    /// </remarks>
    {$ENDREGION}
    class function Instance(): IHTTPManager;
    class procedure Wait(ARequestID: Double; AMilliseconds: Integer = 50);
    class destructor Destroy;

    property ConnectionMaximum: Integer read GetConnectionMaximum write SetConnectionMaximum;
    {$REGION 'Documentation'}
    /// <summary>
    ///   <para>
    ///     Adds a follow-up GET-request to the internal queue and returns
    ///     immediately.
    ///   </para>
    ///   <para>
    ///     A follow-up request is a new request based upon a previous
    ///     request. All HTTP header information from the previous request
    ///     like <see cref="uHTTPInterface|IHTTPHeader.Cookies">Cookies</see>
    ///     , <see cref="uHTTPInterface|IHTTPHeader.CharSet">CharSet</see> as
    ///     well as <see cref="uHTTPInterface|IHTTPHeader.CustomHeaders">
    ///     CustomHeaders</see> are copied into this new request.
    ///   </para>
    /// </summary>
    /// <seealso cref="Get(IHTTPRequest,IHTTPOptions)">
    ///   Get(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil):
    ///   Double;
    /// </seealso>
    {$ENDREGION}
    function Get(AURL: WideString; AFollowUp: Double; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
    {$REGION 'Documentation'}
    /// <summary>
    ///   Adds a new GET-request to the internal queue and returns immediately.
    /// </summary>
    /// <seealso cref="Get(WideString,Double,IHTTPOptions)">
    ///   Get(AURL: WideString; AFollowUp: Double; AHTTPOptions: IHTTPOptions =
    ///   nil): Double;
    /// </seealso>
    {$ENDREGION}
    function Get(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
    {$REGION 'Documentation'}
    /// <summary>
    ///   <para>
    ///     Adds a follow-up HTTP-request to the internal queue and returns
    ///     immediately.
    ///   </para>
    ///   <para>
    ///     A follow-up request is a new request based upon a previous
    ///     request. All HTTP header information from the previous request
    ///     like <see cref="uHTTPInterface|IHTTPHeader.Cookies">Cookies</see>
    ///     , <see cref="uHTTPInterface|IHTTPHeader.CharSet">CharSet</see> as
    ///     well as <see cref="uHTTPInterface|IHTTPHeader.CustomHeaders">
    ///     CustomHeaders</see> are copied into this new request.
    ///   </para>
    /// </summary>
    /// <seealso cref="Post(IHTTPRequest,IHTTPParams,IHTTPOptions)">
    ///   Post(AHTTPRequest: IHTTPRequest; AHTTPParams: IHTTPParams;
    ///   AHTTPOptions: IHTTPOptions = nil): Double;
    /// </seealso>
    {$ENDREGION}
    function Post(AURL: WideString; AFollowUp: Double; AHTTPParams: IHTTPParams; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
    {$REGION 'Documentation'}
    /// <summary>
    ///   Adds a new HTTP-request to the internal queue and returns
    ///   immediately.
    /// </summary>
    /// <seealso cref="Post(WideString,Double,IHTTPParams,IHTTPOptions)">
    ///   Post(AURL: WideString; AFollowUp: Double; AHTTPParams: IHTTPParams;
    ///   AHTTPOptions: IHTTPOptions = nil): Double;
    /// </seealso>
    {$ENDREGION}
    function Post(AHTTPRequest: IHTTPRequest; AHTTPParams: IHTTPParams; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
    {$REGION 'Documentation'}
    /// <summary>
    ///   The function returns True if the given request is processed and False
    ///   otherwise.
    /// </summary>
    /// <param name="AUniqueID">
    ///   Unique request id
    /// </param>
    /// <returns>
    ///   True if the given request is processed and False otherwise.
    /// </returns>
    /// <remarks>
    ///   The function uses internally <see cref="GetResult" /> to check it the
    ///   request is processed.
    /// </remarks>
    {$ENDREGION}
    function HasResult(AUniqueID: Double): WordBool; safecall;
    {$REGION 'Documentation'}
    /// <summary>
    ///   This function returns the information about the full HTTP process if
    ///   completed and nil otherwise.
    /// </summary>
    /// <param name="AUniqueID">
    ///   Unique request id
    /// </param>
    /// <returns>
    ///   The full HTTP process if completed and nil otherwise.
    /// </returns>
    {$ENDREGION}
    function GetResult(AUniqueID: Double): IHTTPProcess; safecall;

    property Implementor: IHTTPImplementation read GetImplementor write SetImplementor;
    property ImplementationManager: IHTTPImplementationManager read GetImplementationManager;
    {$REGION 'Documentation'}
    /// <summary>
    ///   This event occurs when a HTTP request is processed.
    /// </summary>
    /// <remarks>
    ///   The event call is synchronized.
    /// </remarks>
    {$ENDREGION}
    property OnRequestScrape: IHTTPScrapeEvent read GetRequestScrape write FRequestScrape;
    property OnRequestDone: IHTTPProcessEvent read GetRequestDone write FRequestDoneEvent;

    destructor Destroy; override;
  end;

implementation

{ THTTPImplementationManager }

function THTTPImplementationManager.FindImplementation(const AName: WideString): IHTTPImplementation;
var
  LImplementationIndex: Integer;
  LImplementation: IHTTPImplementation;
begin
  Result := nil;

  for LImplementationIndex := 0 to FImplementations.Count - 1 do
  begin
    LImplementation := Implementations[LImplementationIndex];

    if SameText(AName, LImplementation.Name) then
    begin
      Result := LImplementation;
      break;
    end;
  end;
end;

function THTTPImplementationManager.GetCount: Integer;
begin
  Result := FImplementations.Count;
end;

function THTTPImplementationManager.GetImplementation(AIndex: Integer): IHTTPImplementation;
begin
  Result := FImplementations.Items[AIndex] as IHTTPImplementation;
end;

constructor THTTPImplementationManager.Create;
begin
  inherited Create;
  FImplementations := TInterfaceList.Create;
end;

destructor THTTPImplementationManager.Destroy;
begin
  FImplementations.Free;
  inherited Destroy;
end;

function THTTPImplementationManager.Register(const AHTTPImplementation: IHTTPImplementation): WordBool;
begin
  Result := not Assigned(FindImplementation(AHTTPImplementation.Name));
  if Result then
    FImplementations.Add(AHTTPImplementation)
end;

function THTTPImplementationManager.Unregister(const AName: WideString): WordBool;
var
  LImplementation: IHTTPImplementation;
begin
  LImplementation := FindImplementation(AName);
  Result := Assigned(LImplementation);
  if Result then
    try
      FImplementations.Remove(LImplementation);
    finally
      LImplementation := nil;
    end;
end;

{ TIdHTTPManager }

procedure THTTPManager.UpdateCapacity(const ARequestArrayLength: Integer);
var
  Index, Threshold, NewLowerBound: Integer;
begin
  Threshold := FConnectionMaximum.Value * 100;
  if not FRequestArrayLowerBoundUpdate and ((ARequestArrayLength - FRequestArrayLowerBound.Value) > Threshold) then
  begin
    FRequestArrayLowerBoundUpdate := True;
    NewLowerBound := FRequestArrayLowerBound.Value + Threshold;
    FRequestArrayLock.EnterWriteLock;
    try
      for Index := FRequestArrayLowerBound.Value + 1 to NewLowerBound do
        FRequestArray[Index] := nil;
    finally
      FRequestArrayLock.ExitWriteLock;
    end;
    FRequestArrayLowerBound.Value := NewLowerBound;
    FRequestArrayLowerBoundUpdate := False;
  end;
end;

procedure THTTPManager.Execute(const workItem: IOmniWorkItem);
var
  HTTPData: IHTTPData;
  HTTPResult: IHTTPResult;
  HTTPProcess: IHTTPProcess;

  ScrapeData: IHTTPData;
  ScrapeHandled: WordBool;
  ScrapeResult: IHTTPResult;

  ScrapedData: IHTTPData;
begin
  HTTPData := IHTTPData(workItem.Data.AsInterface);

  try
    Implementor.Handle(HTTPData, HTTPResult);
  except
    OutputDebugString('HTTPManager execute error');
  end;

  HTTPProcess := THTTPProcess.Create(workItem.UniqueID);
  HTTPProcess.HTTPData := HTTPData;
  if not workItem.IsExceptional then
  begin
    HTTPProcess.HTTPResult := HTTPResult;

    ScrapeHandled := False;
    FRequestScrape.Invoke(HTTPProcess, ScrapeData, ScrapeHandled);

    if ScrapeHandled then
    begin
      HTTPProcess := nil;

      try
        Implementor.Handle(ScrapeData, ScrapeResult);
      except
        OutputDebugString('HTTPManager execute error (handling scrape)');
      end;

      HTTPProcess := THTTPProcess.Create(workItem.UniqueID);
      HTTPProcess.HTTPData := ScrapeData;
      HTTPProcess.HTTPResult := ScrapeResult;

      HTTPProcess.HTTPData.HTTPRequest.URL := HTTPData.HTTPRequest.URL;
      HTTPProcess.HTTPData.HTTPRequest.Referer := HTTPData.HTTPRequest.Referer;

      ScrapedData := THTTPData.Create(THTTPRequest.FollowUpClone(HTTPProcess, HTTPData.HTTPRequest), HTTPData.HTTPOptions, HTTPData.HTTPParams);
      HTTPProcess := nil;
      HTTPResult := nil;

      try
        Implementor.Handle(ScrapedData, HTTPResult);
      except
        OutputDebugString('HTTPManager execute error (after handling scrape)');
      end;

      HTTPProcess := THTTPProcess.Create(workItem.UniqueID);
      HTTPProcess.HTTPData := ScrapedData;
      if not workItem.IsExceptional then
        HTTPProcess.HTTPResult := HTTPResult;
    end;
  end;

  workItem.Result := TOmniValue.CastFrom(HTTPProcess);
end;

function THTTPManager.DoRequest(AHTTPMethod: THTTPMethod; AURL: string; AFollowUp: Double; AHTTPOptions: IHTTPOptions = nil; AHTTPParams: IHTTPParams = nil): Double;
begin
  Result := DoRequest(THTTPRequest.FollowUpClone(GetResult(AFollowUp), AHTTPMethod, AURL), AHTTPOptions, AHTTPParams);
end;

function THTTPManager.DoRequest(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil; AHTTPParams: IHTTPParams = nil): Double;
var
  HTTPOptions: IHTTPOptions;
  HTTPParams: IHTTPParams;

  HTTPData: IHTTPData;
  OmniWorkItem: IOmniWorkItem;
begin
  if not Assigned(AHTTPOptions) then
    HTTPOptions := THTTPOptions.Create
  else
    HTTPOptions := THTTPOptions.Clone(AHTTPOptions);

  if Assigned(AHTTPParams) then
    HTTPParams := THTTPParams.Clone(AHTTPParams);

  HTTPData := THTTPData.Create(AHTTPRequest, HTTPOptions, HTTPParams);

  OmniWorkItem := FBackgroundWorker.CreateWorkItem(TOmniValue.CastFrom(HTTPData));

  FBackgroundWorker.Schedule(OmniWorkItem);

  Result := OmniWorkItem.UniqueID;
end;

constructor THTTPManager.Create;
begin
  CoInitializeEx(nil, COINIT_MULTITHREADED);

  FBackgroundWorker := Parallel.BackgroundWorker;

  SetLength(FRequestArray, 0);
  FRequestArrayLowerBoundUpdate := False;
  FRequestArrayLowerBound.Value := 0;

  FConnectionMaximum.Value := 1;
  FImplementor := nil;
  FImplementationManager := THTTPImplementationManager.Create;

  FRequestScrape := TIHTTPScrapeEvent.Create;
  FRequestDoneEvent := TIHTTPProcessEvent.Create;

  FBackgroundWorker.NumTasks(FConnectionMaximum.Value).Execute(Execute).OnRequestDone_Asy(
    { } procedure(const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem)
    { } var
    { . } HTTPProcess: IHTTPProcess;
    { . } NewArrayLength: Integer;
    { } begin
    { . } HTTPProcess := IHTTPProcess(workItem.Result.AsInterface);

    { . } FRequestArrayLock.EnterWriteLock;
    { . } try
    { ... } NewArrayLength := Max(workItem.UniqueID, length(FRequestArray));
    { ... } SetLength(FRequestArray, NewArrayLength + 1);
    { ... } FRequestArray[workItem.UniqueID] := HTTPProcess;
    { . } finally
    { ... } FRequestArrayLock.ExitWriteLock;
    { . } end;
    { . } UpdateCapacity(NewArrayLength);

    { } end).OnRequestDone(
    { } procedure(const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem)
    { } begin
    { . } FRequestDoneEvent.Invoke(GetResult(workItem.UniqueID));
    { } end);
end;

function THTTPManager.GetImplementor: IHTTPImplementation;
begin
  if not Assigned(FImplementor) then
  begin
    if ImplementationManager.Count > 0 then
      FImplementor := ImplementationManager.Implementations[0]
    else
      raise Exception.Create('Assign a HTTPImplementor');
  end;

  Result := FImplementor;
end;

function THTTPManager.GetImplementationManager: IHTTPImplementationManager;
begin
  Result := FImplementationManager;
end;

procedure THTTPManager.SetImplementor(const AImplementor: IHTTPImplementation);
begin
  FImplementor := AImplementor;
end;

function THTTPManager.GetRequestScrape: IHTTPScrapeEvent;
begin
  Result := FRequestScrape;
end;

function THTTPManager.GetRequestDone: IHTTPProcessEvent;
begin
  Result := FRequestDoneEvent;
end;

class function THTTPManager.Instance(): IHTTPManager;
begin
  if Assigned(FHTTPManager) then
    Exit(FHTTPManager);

  FLock.Acquire;
  try
    if not Assigned(FHTTPManager) then
      FHTTPManager := THTTPManager.Create;
    Result := FHTTPManager;
  finally
    FLock.Release;
  end;
end;

class procedure THTTPManager.Wait(ARequestID: Double; AMilliseconds: Integer = 50);
begin
  repeat
    sleep(AMilliseconds);
  until Instance().HasResult(ARequestID);
end;

class destructor THTTPManager.Destroy;
begin
  FHTTPManager := nil;
end;

function THTTPManager.GetConnectionMaximum: Integer;
begin
  Result := FConnectionMaximum.Value;
end;

procedure THTTPManager.SetConnectionMaximum(const AConnectionMaximum: Integer);
begin
  if not(AConnectionMaximum = FConnectionMaximum.Value) then
  begin
    FConnectionMaximum.Value := AConnectionMaximum;
    FBackgroundWorker.NumTasks(AConnectionMaximum);
  end;
end;

function THTTPManager.Get(AURL: WideString; AFollowUp: Double; AHTTPOptions: IHTTPOptions = nil): Double;
begin
  Result := DoRequest(mGET, AURL, AFollowUp, AHTTPOptions);
end;

function THTTPManager.Get(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil): Double;
begin
  AHTTPRequest.Method := mGET;
  Result := DoRequest(THTTPRequest.Clone(AHTTPRequest), AHTTPOptions);
end;

function THTTPManager.Post(AURL: WideString; AFollowUp: Double; AHTTPParams: IHTTPParams; AHTTPOptions: IHTTPOptions = nil): Double;
begin
  Result := DoRequest(mPOST, AURL, AFollowUp, AHTTPOptions, AHTTPParams);
end;

function THTTPManager.Post(AHTTPRequest: IHTTPRequest; AHTTPParams: IHTTPParams; AHTTPOptions: IHTTPOptions = nil): Double;
begin
  AHTTPRequest.Method := mPOST;
  Result := DoRequest(THTTPRequest.Clone(AHTTPRequest), AHTTPOptions, AHTTPParams);
end;

function THTTPManager.HasResult(AUniqueID: Double): WordBool;
begin
  Result := Assigned(GetResult(AUniqueID));
end;

function THTTPManager.GetResult(AUniqueID: Double): IHTTPProcess;
var
  Index: Integer;
begin
  Result := nil;

  Index := Trunc(AUniqueID);

  FRequestArrayLock.EnterReadLock;
  try
    if (Index > FRequestArrayLowerBound.Value) and (Index < length(FRequestArray)) then
      Result := FRequestArray[Index];
  finally
    FRequestArrayLock.ExitReadLock;
  end;
end;

destructor THTTPManager.Destroy;
begin
  FRequestDoneEvent := nil;
  FRequestScrape := nil;
  FImplementationManager := nil;
  FImplementor := nil;

  SetLength(FRequestArray, 0);

  FBackgroundWorker.Terminate(INFINITE);
  FBackgroundWorker := nil;

  CoUninitialize;

  inherited Destroy;
end;

end.
