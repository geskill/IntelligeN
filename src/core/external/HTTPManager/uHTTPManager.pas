{$REGION 'Documentation'}
/// <summary>
/// <para>
/// This is the main unit for managing everything.
/// </para>
/// <para>
/// Use only the interface <see cref="uHTTPInterface|IHTTPManager" /> for
/// distribution (i.e. for COM compatible DLLs).
/// </para>
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
  // Implementor
  uHTTPIndyImplementor,
  // Delphi
  Windows, SysUtils, Math,
  // OmniThreadLibrary
  OtlCommon, OtlSync, OtlParallel;

type
{$REGION 'Documentation'}
  /// <summary>
  /// <para>
  /// This is the base class, which handles the queued access for the
  /// HTTP-requests. Every GET/POST request gets a unique number. Through
  /// this you can check via
  /// <see cref="HasResult">HasResult(&lt;UniqueID&gt;)</see> whether a
  /// specific request is full-filled or not. If the request is finished
  /// [i.e. <see cref="HasResult" /> = True], you can call
  /// <see cref="GetResult">GetResult(&lt;UniqueID&gt;)</see> to access the
  /// HTTP-request result.
  /// </para>
  /// <para>
  /// Be sure to create (using the Instance function) the class before you
  /// access it outside of your main application! You can customize
  /// (override) the internal constructor <see cref="Create" /> for your
  /// own needs (i.e. connect HTTP logging feature).
  /// </para>
  /// </summary>
  /// <remarks>
  /// This class is a singleton.
  /// </remarks>
{$ENDREGION}
  THTTPManager = class(TInterfacedObject, IHTTPManager)
  strict private
    class var FLock: TOmniCS;
  private
    FBackgroundWorker: IOmniBackgroundWorker;
    FRequestArray: array of IHTTPProcess;
    FRequestArrayLock: TOmniMREW;
    FImplementor: IHTTPImplementation;
    FRequestDoneEvent: IHTTPProcessEvent;

    class var FHTTPManager: IHTTPManager;

    // procedure HandleBlockingScripts(AHTTPHelper: THTTPHelper; AWebsite: string; AHTTPResponse: IHTTPResponse);
    procedure Execute(const workItem: IOmniWorkItem);

    function DoRequest(AHTTPMethod: THTTPMethod; AURL: string; AFollowUp: Double; AHTTPOptions: IHTTPOptions = nil; AHTTPParams: IHTTPParams = nil): Double; overload;
    function DoRequest(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil; AHTTPParams: IHTTPParams = nil): Double; overload;

    constructor Create;
  protected
    function GetImplementor: IHTTPImplementation; safecall;
    procedure SetImplementor(const AImplementor: IHTTPImplementation); safecall;
    function GetRequestDone: IHTTPProcessEvent; safecall;
  public
{$REGION 'Documentation'}
    /// <summary>
    /// This returns the single instance to this class. If this class is not
    /// created, this functions creates it.
    /// </summary>
    /// <returns>
    /// The instance of this class.
    /// </returns>
    /// <remarks>
    /// Be sure to create the class before you access it outside of your main
    /// application! You can customize (override) the internal constructor
    /// <see cref="Create" /> for your own needs (i.e. connect HTTP logging
    /// feature).
    /// </remarks>
{$ENDREGION}
    class function Instance(): IHTTPManager;
    class procedure Wait(ARequestID: Double; AMilliseconds: Integer = 50);
    class destructor Destroy;
{$REGION 'Documentation'}
    /// <summary>
    /// <para>
    /// Adds a follow-up GET-request to the internal queue and returns
    /// immediately.
    /// </para>
    /// <para>
    /// A follow-up request is a new request based upon a previous request.
    /// All HTTP header information from the previous request like
    /// <see cref="uHTTPInterface|IHTTPHeader.Cookies">Cookies</see>,
    /// <see cref="uHTTPInterface|IHTTPHeader.CharSet">CharSet</see> as
    /// well as
    /// <see cref="uHTTPInterface|IHTTPHeader.CustomHeaders">CustomHeaders</see>
    /// are copied into this new request.
    /// </para>
    /// </summary>
    /// <seealso cref="Get(IHTTPRequest,IHTTPOptions)">
    /// Get(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil):
    /// Double;
    /// </seealso>
{$ENDREGION}
    function Get(AURL: WideString; AFollowUp: Double; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
{$REGION 'Documentation'}
    /// <summary>
    /// Adds a new GET-request to the internal queue and returns immediately.
    /// </summary>
    /// <seealso cref="Get(WideString,Double,IHTTPOptions)">
    /// Get(AURL: WideString; AFollowUp: Double; AHTTPOptions: IHTTPOptions =
    /// nil): Double;
    /// </seealso>
{$ENDREGION}
    function Get(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
{$REGION 'Documentation'}
    /// <summary>
    /// <para>
    /// Adds a follow-up HTTP-request to the internal queue and returns
    /// immediately.
    /// </para>
    /// <para>
    /// A follow-up request is a new request based upon a previous request.
    /// All HTTP header information from the previous request like
    /// <see cref="uHTTPInterface|IHTTPHeader.Cookies">Cookies</see>,
    /// <see cref="uHTTPInterface|IHTTPHeader.CharSet">CharSet</see> as
    /// well as
    /// <see cref="uHTTPInterface|IHTTPHeader.CustomHeaders">CustomHeaders</see>
    /// are copied into this new request.
    /// </para>
    /// </summary>
    /// <seealso cref="Post(IHTTPRequest,IHTTPParams,IHTTPOptions)">
    /// Post(AHTTPRequest: IHTTPRequest; AHTTPParams: IHTTPParams;
    /// AHTTPOptions: IHTTPOptions = nil): Double;
    /// </seealso>
{$ENDREGION}
    function Post(AURL: WideString; AFollowUp: Double; AHTTPParams: IHTTPParams; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
{$REGION 'Documentation'}
    /// <summary>
    /// Adds a new HTTP-request to the internal queue and returns
    /// immediately.
    /// </summary>
    /// <seealso cref="Post(WideString,Double,IHTTPParams,IHTTPOptions)">
    /// Post(AURL: WideString; AFollowUp: Double; AHTTPParams: IHTTPParams;
    /// AHTTPOptions: IHTTPOptions = nil): Double;
    /// </seealso>
{$ENDREGION}
    function Post(AHTTPRequest: IHTTPRequest; AHTTPParams: IHTTPParams; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
{$REGION 'Documentation'}
    /// <summary>
    /// The function returns True if the given request is processed and False
    /// otherwise.
    /// </summary>
    /// <param name="AUniqueID">
    /// Unique request id
    /// </param>
    /// <returns>
    /// True if the given request is processed and False otherwise.
    /// </returns>
    /// <remarks>
    /// The function uses internally <see cref="GetResult" /> to check it the
    /// request is processed.
    /// </remarks>
{$ENDREGION}
    function HasResult(AUniqueID: Double): WordBool; safecall;
{$REGION 'Documentation'}
    /// <summary>
    /// This function returns the information about the full HTTP process if
    /// completed and nil otherwise.
    /// </summary>
    /// <param name="AUniqueID">
    /// Unique request id
    /// </param>
    /// <returns>
    /// The full HTTP process if completed and nil otherwise.
    /// </returns>
{$ENDREGION}
    function GetResult(AUniqueID: Double): IHTTPProcess; safecall;

    property Implementor: IHTTPImplementation read GetImplementor write SetImplementor;
{$REGION 'Documentation'}
    /// <summary>
    /// This event occurs when a HTTP request is processed.
    /// </summary>
    /// <remarks>
    /// The event call is synchronized.
    /// </remarks>
{$ENDREGION}
    property OnRequestDone: IHTTPProcessEvent read GetRequestDone write FRequestDoneEvent;

    destructor Destroy; override;
  end;

implementation

  { TIdHTTPManager }
{$REGION 'HandleBlockingScripts'}
  (*
    procedure THTTPManager.HandleBlockingScripts(AHTTPHelper: THTTPHelper; AWebsite:string; AHTTPResponse: IHTTPResponse);
    var
    I: Integer;
    CookieStartString, CookieEndString, JavaScriptPage: string;
    begin
    if Pos('onload="scf(', string(AHTTPResponse.Content)) > 0 then
    begin
    CookieStartString := '';
    with TRegExpr.Create do
    try
    InputString := AHTTPResponse.Content;
    Expression := 'onload="scf\((.*?),';

    if Exec(InputString) then
    begin
    for I := 1 to length(Match[1]) - 1 do
    if Match[1][I] in ['0' .. '9', 'a' .. 'z', 'A' .. 'Z'] then
    CookieStartString := CookieStartString + Match[1][I];
    end;
    finally
    Free;
    end;

    with TRegExpr.Create do
    try
    InputString := AHTTPResponse.Content;
    Expression := 'language="javascript" src="\/sc_(\w+)\.js"';

    if Exec(InputString) then
    JavaScriptPage := AHTTPHelper.Get(AWebsite + 'sc_' + Match[1] + '.js');
    finally
    Free;
    end;

    CookieEndString := '';
    with TRegExpr.Create do
    try
    InputString := JavaScriptPage;
    Expression := 'escape\(hsh \+ "(.*?)"\)';

    if Exec(InputString) then
    begin
    for I := 1 to length(Match[1]) do
    if Match[1][I] in ['0' .. '9', 'a' .. 'z', 'A' .. 'Z'] then
    CookieEndString := CookieEndString + Match[1][I];
    end;
    finally
    Free;
    end;

    AHTTPResponse.Cookies.Add('sitechrx=' + CookieStartString + CookieEndString);
    end;
    end;
    *)
{$ENDREGION}

procedure THTTPManager.Execute(const workItem: IOmniWorkItem);
var
  HTTPData: IHTTPData;
  HTTPResult: IHTTPResult;
begin
  HTTPData := IHTTPData(workItem.Data.AsInterface);

  if Assigned(FImplementor) then
  begin
    try
      FImplementor.Handle(HTTPData, HTTPResult);
    except
      OutputDebugString('Error!');
    end;
  end
  else
    raise Exception.Create('Assign a HTTPImplementor');

  workItem.Result := TOmniValue.CastFrom(HTTPResult);
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
  FBackgroundWorker := Parallel.BackgroundWorker;

  FImplementor := THTTPIndyImplementation.Create;

  FRequestDoneEvent := TIHTTPProcessEvent.Create;

  FBackgroundWorker.NumTasks(1).Execute(Execute).OnRequestDone_Asy(
    { } procedure(const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem)
    { } var
    { . } HTTPProcess: IHTTPProcess;
    { } begin

    { . } HTTPProcess := THTTPProcess.Create(workItem.UniqueID);
    { . } HTTPProcess.HTTPData := IHTTPData(workItem.Data.AsInterface);
    { . } if not workItem.IsExceptional then
    { ... } HTTPProcess.HTTPResult := IHTTPResult(workItem.Result.AsInterface);

    { . } FRequestArrayLock.EnterWriteLock;
    { . } try
    { ... } SetLength(FRequestArray, Max(workItem.UniqueID, length(FRequestArray)) + 1);
    { ... } FRequestArray[workItem.UniqueID] := HTTPProcess;
    { . } finally
    { ... } FRequestArrayLock.ExitWriteLock;
    { . } end;

    { } end).OnRequestDone(
    { } procedure(const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem)
    { } begin
    { . } FRequestDoneEvent.Invoke(GetResult(workItem.UniqueID));
    { } end);

  SetLength(FRequestArray, 0);
end;

function THTTPManager.GetImplementor: IHTTPImplementation;
begin
  Result := FImplementor;
end;

procedure THTTPManager.SetImplementor(const AImplementor: IHTTPImplementation);
begin
  FImplementor := AImplementor;
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

  OutputDebugString(PChar('THTTPManager START GetResult'));
  FRequestArrayLock.EnterReadLock;
  try
    if Index < length(FRequestArray) then
      Result := FRequestArray[Index];
  finally
    FRequestArrayLock.ExitReadLock;
  end;
  OutputDebugString(PChar('THTTPManager END GetResult'));
end;

destructor THTTPManager.Destroy;
begin
  FRequestDoneEvent := nil;
  FImplementor := nil;

  SetLength(FRequestArray, 0);

  FBackgroundWorker.Terminate(INFINITE);
  FBackgroundWorker := nil;

  inherited Destroy;
end;

end.
