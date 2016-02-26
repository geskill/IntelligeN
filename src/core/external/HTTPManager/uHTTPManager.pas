{.$DEFINE DEBUG_HTTPMANAGER}
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
  OtlComm, OtlCommon, OtlEventMonitor, OtlSync, OtlTask, OtlTaskControl, OtlThreadPool;

type
  THTTPAntiScrapeManager = class(TInterfacedObject, IHTTPAntiScrapeManager)
  private
    FAntiScrapes: TInterfaceList;
    function FindAntiScrape(const AName: WideString): IHTTPAntiScrape;
  protected
    function GetCount: Integer; safecall;
    function GetAntiScrape(AIndex: Integer): IHTTPAntiScrape; safecall;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Register(const AAntiScrape: IHTTPAntiScrape): WordBool; safecall;
    function Unregister(const AName: WideString): WordBool; safecall;

    property Count: Integer read GetCount;
    property AntiScrapes[Index: Integer]: IHTTPAntiScrape read GetAntiScrape; default;
  end;

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

    function Register(const AImplementation: IHTTPImplementation): WordBool; safecall;
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
    FOmniEM: TOmniEventMonitor;
    FNextUniqueID: TOmniAlignedInt64;
    FTaskControlArray: array of IOmniTaskControl;
    FTaskControlArrayLock: TOmniMREW;
    FRequestArray: array of IHTTPProcess;
    FRequestArrayLock: TOmniMREW;
    FRequestArrayLowerBoundUpdate: Boolean;
    FRequestArrayLowerBound: TOmniAlignedInt32;
    FThreadPool: IOmniThreadPool;
    FAntiScrapeManager: IHTTPAntiScrapeManager;
    FImplementor: IHTTPImplementation;
    FImplementationManager: IHTTPImplementationManager;
    FRequestDoneEvent: IHTTPProcessEvent;

  const
    MSG_TASK_FINISHED = 0;

    procedure OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); virtual;

    procedure UpdateCapacity(const ARequestArrayLength: Integer);

    class var FHTTPManager: IHTTPManager;

    procedure Execute(AUniqueID: Int64; AHTTPData: IHTTPData; out AHTTPProcess: IHTTPProcess);
    procedure ExecuteFinished(AUniqueID: Int64; AHTTPProcess: IHTTPProcess);
    procedure RunRequestTask(const ATask: IOmniTask);

    procedure CreateRequestTask(AUniqueID: Int64; AHTTPData: IHTTPData);

    function DoRequest(AHTTPMethod: THTTPMethod; const AURL: string; AFollowUp: Double; AHTTPOptions: IHTTPOptions = nil; AHTTPParams: IHTTPParams = nil): Double; overload;
    function DoRequest(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil; AHTTPParams: IHTTPParams = nil): Double; overload;

    constructor Create;
  protected
    function GetConnectionMaximum: Integer; safecall;
    procedure SetConnectionMaximum(const AConnectionMaximum: Integer); safecall;
    function GetAntiScrapeManager: IHTTPAntiScrapeManager; safecall;
    function GetImplementor: IHTTPImplementation; safecall;
    procedure SetImplementor(const AImplementor: IHTTPImplementation); safecall;
    function GetImplementationManager: IHTTPImplementationManager; safecall;
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
    function Get(const AURL: WideString; AFollowUp: Double; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
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
    function Post(const AURL: WideString; AFollowUp: Double; AHTTPParams: IHTTPParams; AHTTPOptions: IHTTPOptions = nil): Double; overload; safecall;
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

    function WaitFor(AUniqueID: Double; AMaxWaitMS: Cardinal = INFINITE): WordBool; safecall;

    property AntiScrapeManager: IHTTPAntiScrapeManager read GetAntiScrapeManager;

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
    property OnRequestDone: IHTTPProcessEvent read GetRequestDone write FRequestDoneEvent;

    destructor Destroy; override;
  end;

implementation

{ THTTPAntiScrapeManager }

function THTTPAntiScrapeManager.FindAntiScrape(const AName: WideString): IHTTPAntiScrape;
var
  LAntiScrapeIndex: Integer;
  LAntiScrape: IHTTPAntiScrape;
begin
  Result := nil;

  for LAntiScrapeIndex := 0 to FAntiScrapes.Count - 1 do
  begin
    LAntiScrape := AntiScrapes[LAntiScrapeIndex];

    if SameText(AName, LAntiScrape.Name) then
    begin
      Result := LAntiScrape;
      break;
    end;
  end;
end;

function THTTPAntiScrapeManager.GetCount: Integer;
begin
  Result := FAntiScrapes.Count;
end;

function THTTPAntiScrapeManager.GetAntiScrape(AIndex: Integer): IHTTPAntiScrape;
begin
  Result := FAntiScrapes.Items[AIndex] as IHTTPAntiScrape;
end;

constructor THTTPAntiScrapeManager.Create;
begin
  inherited Create;
  FAntiScrapes := TInterfaceList.Create;
end;

destructor THTTPAntiScrapeManager.Destroy;
begin
  FAntiScrapes.Free;
  inherited Destroy;
end;

function THTTPAntiScrapeManager.Register(const AAntiScrape: IHTTPAntiScrape): WordBool;
begin
  Result := not Assigned(FindAntiScrape(AAntiScrape.Name));
  if Result then
    FAntiScrapes.Add(AAntiScrape)
end;

function THTTPAntiScrapeManager.Unregister(const AName: WideString): WordBool;
var
  LAntiScrape: IHTTPAntiScrape;
begin
  LAntiScrape := FindAntiScrape(AName);
  Result := Assigned(LAntiScrape);
  if Result then
    try
      FAntiScrapes.Remove(LAntiScrape);
    finally
      LAntiScrape := nil;
    end;
end;

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

function THTTPImplementationManager.Register(const AImplementation: IHTTPImplementation): WordBool;
begin
  Result := not Assigned(FindImplementation(AImplementation.Name));
  if Result then
    FImplementations.Add(AImplementation)
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

procedure THTTPManager.OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
var
  UniqueID: Int64;
begin
  case msg.MsgID of
    MSG_TASK_FINISHED:
      begin
        UniqueID := msg.MsgData[0].AsInt64;
{$IFDEF DEBUG_HTTPMANAGER}
        OutputDebugString(PChar('RequestDoneEvent.Invoke: ' + IntToStr(UniqueID)));
{$ENDIF}
        FRequestDoneEvent.Invoke(GetResult(UniqueID));
      end;
  end;
end;

procedure THTTPManager.UpdateCapacity(const ARequestArrayLength: Integer);
var
  Index, Threshold, NewLowerBound: Integer;
begin
  Threshold := FThreadPool.MaxExecuting * 100;
  if not FRequestArrayLowerBoundUpdate and ((ARequestArrayLength - FRequestArrayLowerBound.Value) > (Threshold + Threshold)) then
  begin
    FRequestArrayLowerBoundUpdate := True;
    NewLowerBound := FRequestArrayLowerBound.Value + Threshold;
{$IFDEF DEBUG_HTTPMANAGER}
    OutputDebugString(PChar('ARequestArrayLength: ' + IntToStr(ARequestArrayLength)));
    OutputDebugString(PChar('FRequestArrayLowerBound: ' + IntToStr(FRequestArrayLowerBound.Value)));
    OutputDebugString(PChar('NewLowerBound: ' + IntToStr(NewLowerBound)));
{$ENDIF}
    FTaskControlArrayLock.EnterWriteLock;
    try
      for Index := FRequestArrayLowerBound.Value + 1 to NewLowerBound do
        FTaskControlArray[Index] := nil;
    finally
      FTaskControlArrayLock.ExitWriteLock;
    end;
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

procedure THTTPManager.Execute(AUniqueID: Int64; AHTTPData: IHTTPData; out AHTTPProcess: IHTTPProcess);
var
  HTTPResult: IHTTPResult;

  AntiScrapeIndex: Integer;

  ScrapeHandled: WordBool;

  ScrapeData: IHTTPData;
  ScrapeResult: IHTTPResult;
  ScrapeProcess: IHTTPProcess;

  ScrapedData: IHTTPData;
begin
{$IFDEF DEBUG_HTTPMANAGER}
  OutputDebugString(PChar('NewRequest: ' + IntToStr(AUniqueID)));
{$ENDIF}
  try
    Implementor.Handle(AHTTPData, HTTPResult);
  except
    OutputDebugString('HTTPManager execute error');
  end;

  AHTTPProcess := THTTPProcess.Create(AUniqueID);
  AHTTPProcess.HTTPData := AHTTPData;
  AHTTPProcess.HTTPResult := HTTPResult;

  ScrapeHandled := False;
  for AntiScrapeIndex := 0 to AntiScrapeManager.Count - 1 do
  begin
    AntiScrapeManager.AntiScrapes[AntiScrapeIndex].Handle(AHTTPProcess, ScrapeData, ScrapeHandled);
    if ScrapeHandled then
      break;
  end;

  if ScrapeHandled then
  begin
    AHTTPProcess := nil;

    try
      Implementor.Handle(ScrapeData, ScrapeResult);
    except
      OutputDebugString('HTTPManager execute error (handling scrape)');
    end;

    ScrapeProcess := THTTPProcess.Create(AUniqueID);
    ScrapeProcess.HTTPData := ScrapeData;
    ScrapeProcess.HTTPResult := ScrapeResult;

    ScrapeProcess.HTTPData.HTTPRequest.URL := AHTTPData.HTTPRequest.URL;
    ScrapeProcess.HTTPData.HTTPRequest.Referer := AHTTPData.HTTPRequest.Referer;

    ScrapedData := THTTPData.Create(THTTPRequest.FollowUpClone(ScrapeProcess, AHTTPData.HTTPRequest), AHTTPData.HTTPOptions, AHTTPData.HTTPParams);
    ScrapeProcess := nil;
    HTTPResult := nil;

    try
      Implementor.Handle(ScrapedData, HTTPResult);
    except
      OutputDebugString('HTTPManager execute error (after handling scrape)');
    end;

    AHTTPProcess := THTTPProcess.Create(AUniqueID);
    AHTTPProcess.HTTPData := ScrapedData;
    AHTTPProcess.HTTPResult := HTTPResult;
  end;
end;

procedure THTTPManager.ExecuteFinished(AUniqueID: Int64; AHTTPProcess: IHTTPProcess);
var
  NewArrayLength: Integer;
begin
{$IFDEF DEBUG_HTTPMANAGER}
  OutputDebugString(PChar('NewRequest Done: ' + IntToStr(AUniqueID)));
{$ENDIF}
  FRequestArrayLock.EnterWriteLock;
  try
    NewArrayLength := Max(AUniqueID, length(FRequestArray));
    SetLength(FRequestArray, NewArrayLength + 1);
{$IFDEF DEBUG_HTTPMANAGER}
    OutputDebugString(PChar('NewArrayLength: ' + IntToStr(NewArrayLength) + ' [' + IntToStr(FRequestArrayLowerBound.Value) + ', ' + IntToStr(length(FRequestArray)) + ']'));
{$ENDIF}
    FRequestArray[AUniqueID] := AHTTPProcess;
  finally
    FRequestArrayLock.ExitWriteLock;
  end;
  UpdateCapacity(NewArrayLength);
end;

procedure THTTPManager.RunRequestTask(const ATask: IOmniTask);
var
  UniqueID: Int64;
  HTTPData: IHTTPData;

  HTTPProcess: IHTTPProcess;
begin
  UniqueID := ATask.Param.Item[0].AsInt64;
  HTTPData := IHTTPData(ATask.Param.Item[1].AsInterface);
  Execute(UniqueID, HTTPData, HTTPProcess);
  ExecuteFinished(UniqueID, HTTPProcess);
  ATask.Comm.Send(MSG_TASK_FINISHED, [UniqueID]);
end;

procedure THTTPManager.CreateRequestTask(AUniqueID: Int64; AHTTPData: IHTTPData);
var
  NewArrayLength: Integer;
  TaskControl: IOmniTaskControl;
begin
  TaskControl := CreateTask(RunRequestTask, 'THTTPManager.DoRequest');
  TaskControl.Param.Add(AUniqueID);
  TaskControl.Param.Add(TOmniValue.CastFrom(AHTTPData));
  TaskControl.MonitorWith(FOmniEM);

  FTaskControlArrayLock.EnterWriteLock;
  try
    NewArrayLength := Max(AUniqueID, length(FTaskControlArray));
    SetLength(FTaskControlArray, NewArrayLength + 1);
    FTaskControlArray[AUniqueID] := TaskControl;
  finally
    FTaskControlArrayLock.ExitWriteLock;
  end;

  TaskControl.Schedule(FThreadPool);
end;

function THTTPManager.DoRequest(AHTTPMethod: THTTPMethod; const AURL: string; AFollowUp: Double; AHTTPOptions: IHTTPOptions = nil; AHTTPParams: IHTTPParams = nil): Double;
begin
  Result := DoRequest(THTTPRequest.FollowUpClone(GetResult(AFollowUp), AHTTPMethod, AURL), AHTTPOptions, AHTTPParams);
end;

function THTTPManager.DoRequest(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil; AHTTPParams: IHTTPParams = nil): Double;
var
  HTTPOptions: IHTTPOptions;
  HTTPParams: IHTTPParams;

  HTTPData: IHTTPData;

  UniqueID: Int64;
begin
  if not Assigned(AHTTPOptions) then
    HTTPOptions := THTTPOptions.Create
  else
    HTTPOptions := THTTPOptions.Clone(AHTTPOptions);

  if Assigned(AHTTPParams) then
    HTTPParams := THTTPParams.Clone(AHTTPParams);

  HTTPData := THTTPData.Create(AHTTPRequest, HTTPOptions, HTTPParams);

  UniqueID := FNextUniqueID.Value;
  FNextUniqueID.Increment;

  CreateRequestTask(UniqueID, HTTPData);

  Result := UniqueID;
end;

constructor THTTPManager.Create;
begin
  inherited Create;

  CoInitializeEx(nil, COINIT_MULTITHREADED);

  FOmniEM := TOmniEventMonitor.Create(nil);
  with FOmniEM do
  begin
    OnTaskMessage := OmniEMTaskMessage;
  end;

  FNextUniqueID.Value := 0;

  SetLength(FTaskControlArray, 0);

  SetLength(FRequestArray, 0);
  FRequestArrayLowerBoundUpdate := False;
  FRequestArrayLowerBound.Value := -1;

  FThreadPool := CreateThreadPool('THTTPManager');
  with FThreadPool do
  begin
    MaxExecuting := 1;
    MaxQueued := 0;
  end;

  FAntiScrapeManager := THTTPAntiScrapeManager.Create;
  FImplementor := nil;
  FImplementationManager := THTTPImplementationManager.Create;

  FRequestDoneEvent := TIHTTPProcessEvent.Create;
end;

function THTTPManager.GetAntiScrapeManager: IHTTPAntiScrapeManager;
begin
  Result := FAntiScrapeManager;
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
  Result := FThreadPool.MaxExecuting;
end;

procedure THTTPManager.SetConnectionMaximum(const AConnectionMaximum: Integer);
begin
  if not(AConnectionMaximum = ConnectionMaximum) then
  begin
    FThreadPool.MaxExecuting := AConnectionMaximum;
  end;
end;

function THTTPManager.Get(const AURL: WideString; AFollowUp: Double; AHTTPOptions: IHTTPOptions = nil): Double;
begin
  Result := DoRequest(mGET, AURL, AFollowUp, AHTTPOptions);
end;

function THTTPManager.Get(AHTTPRequest: IHTTPRequest; AHTTPOptions: IHTTPOptions = nil): Double;
begin
  AHTTPRequest.Method := mGET;
  Result := DoRequest(THTTPRequest.Clone(AHTTPRequest), AHTTPOptions);
end;

function THTTPManager.Post(const AURL: WideString; AFollowUp: Double; AHTTPParams: IHTTPParams; AHTTPOptions: IHTTPOptions = nil): Double;
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
{$IFDEF DEBUG_HTTPMANAGER}
  OutputDebugString(PChar('GetResult: ' + IntToStr(Index) + ' [' + IntToStr(FRequestArrayLowerBound.Value) + ', ' + IntToStr(length(FRequestArray)) + ']'));
{$ENDIF}
  FRequestArrayLock.EnterReadLock;
  try
    if (Index > FRequestArrayLowerBound.Value) and (Index < length(FRequestArray)) then
      Result := FRequestArray[Index];
  finally
    FRequestArrayLock.ExitReadLock;
  end;
end;

function THTTPManager.WaitFor(AUniqueID: Double; AMaxWaitMS: Cardinal = INFINITE): WordBool;
var
  Index: Integer;
  TaskControl: IOmniTaskControl;
begin
  Result := False;

  Index := Trunc(AUniqueID);

  FTaskControlArrayLock.EnterWriteLock;
  try
    if (Index > FRequestArrayLowerBound.Value) and (Index < length(FTaskControlArray)) then
      TaskControl := FTaskControlArray[Index];
  finally
    FTaskControlArrayLock.ExitWriteLock;
  end;

  if Assigned(TaskControl) then
    Result := TaskControl.WaitFor(AMaxWaitMS);
end;

destructor THTTPManager.Destroy;
begin
  FRequestDoneEvent := nil;
  FImplementationManager := nil;
  FImplementor := nil;
  FAntiScrapeManager := nil;

  if Assigned(FThreadPool) then
  begin
    FThreadPool.CancelAll;
    FThreadPool := nil;
  end;

  SetLength(FRequestArray, 0);
  SetLength(FTaskControlArray, 0);

  FOmniEM.Free;

  CoUninitialize;

  inherited Destroy;
end;

end.
