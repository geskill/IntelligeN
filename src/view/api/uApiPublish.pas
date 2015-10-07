unit uApiPublish;

interface

uses
  // Delphi
  Windows, Forms, SysUtils, Classes, Messages,
  // OmniThreadLibrary
  GpStuff, OtlCollections, OtlComm, OtlCommon, OtlEventMonitor, OtlParallel, OtlTask, OtlTaskControl, OtlThreadPool,
  // ThreadStringList
  uThreadStringList,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // API
  uApiCAPTCHA, uApiPlugins, uApiSettings, uApiThreadPoolManager,
  // Utils
  uPathUtils, uStringUtils;

type
  TPublishManagerStatus = (pmsRESUMING, pmsPAUSING, pmsSTOPPING);
  TPublishManagerItemStatus = (pmisCREATED, pmisWORKING, pmisERROR, pmisCANCELED, pmisFINISHED);

  TGUIInteractionEvent = procedure(Status: TPublishManagerStatus) of object;
  TGUIInteractionItemEvent = procedure(Status: TPublishManagerItemStatus; PublishJob: IPublishJob; AProgressPosition: Extended; msg: string) of object;

  TPublishItemThread = class(TMyOmniWorker)
  private
    FWait: IOmniWaitableValue;
    FErrorMsg: string;
    FCAPTCHAResult: Boolean;
    FCAPTCHAImageUrl, FCAPTCHAName, FCAPTCHAText, FCAPTCHACookies: WideString;
    FIntelligentPostingResult, FIntelligentPostingRedoSearch: WordBool;
    FIntelligentPostingSearchValue, FIntelligentPostingSearchResults: WideString;
    FIntelligentPostingSearchIndex: Integer;
    procedure InternalErrorHandler(AErrorMsg: string);
    procedure CAPTCHAInputThreaded;
    function CAPTCHAInputSynchronizer(const AImageUrl, AName: WideString; out AText: WideString; var ACookies: WideString): WordBool; safecall;
    procedure IntelligentPostingHandlerThreaded;
    function IntelligentPostingHandlerSynchronizer(var ASearchValue: WideString; const ASearchResults: WideString; var ASearchIndex: Integer; out ARedoSearch: WordBool): WordBool; safecall;
  protected
    FPublishItem: IPublishItem;
    FPublishRetry: Integer;
  public
    constructor Create(const APublishItem: IPublishItem; APublishRetry: Integer);
    function Initialize: Boolean; override;
    procedure Cleanup; override;
  end;

  TPublishPool = class(TThreadPoolManager)
  private
    FCompletedCount: TGp4AlignedInt;
    FThreadStringList: TThreadStringList;
  protected
    FPublishJob: IPublishJob;
    FTotalCount: Integer;
    FOnGUIInteractionItem: TGUIInteractionItemEvent;
    procedure OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); override;
  public
    constructor Create(const APublishJob: IPublishJob; APublishRate, ATotalCount: Integer; AOnGUIInteractionItem: TGUIInteractionItemEvent); reintroduce;
    procedure AddPublishItem(const APublishItem: IPublishItem; APublishRetry: Integer);
    procedure RemovePublishItem(const APublishItem: IPublishItem);
    destructor Destroy; override;
  end;

  TPublishThread = class(TMyOmniWorker)
  private
    FPublishPool: TPublishPool;
  protected
    FPublishJob: IPublishJob;
    FPublishDelay, FPublishRetry: Integer;
    FTotalCount: Integer;
  public
    constructor Create(const APublishJob: IPublishJob; APublishRate, APublishDelay, APublishRetry: Integer; AOnGUIInteractionItem: TGUIInteractionItemEvent);
    function Initialize: Boolean; override;
    procedure Cleanup; override;
  end;

  TPublishManager = class(TThreadPoolManager, IPublishManager)
  private
    FNextUniqueID: Longword;

    FOnGUIInteraction: TGUIInteractionEvent;
    FOnGUIInteractionItem: TGUIInteractionItemEvent;
    function GetNextUniqueID: Longword;
  protected
    procedure OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); override;
  public
    constructor Create; reintroduce;
    function AddPublishJob(const APublishJob: IPublishJob): Longword;
    procedure RemovePublishJob(const APublishJob: IPublishJob); overload;
    procedure RemovePublishJob(const AUniqueID: Longword); overload;
    procedure RemoveAllJobs; // i.e. STOP

    procedure Resume; // play
    procedure Pause; // pause

    property OnGUIInteraction: TGUIInteractionEvent read FOnGUIInteraction write FOnGUIInteraction;
    property OnGUIInteractionItem: TGUIInteractionItemEvent read FOnGUIInteractionItem write FOnGUIInteractionItem;
  end;

const
  MSG_PUBLISH_ITEM_TASK_STARTED = 10;
  MSG_PUBLISH_ITEM_TASK_ERROR = 11;
  MSG_PUBLISH_ITEM_TASK_COMPLETED = 12;
  MSG_PUBLISH_TASK_CANCELED = 13;
  MSG_PUBLISH_TASK_FINISHED = 14;

implementation

uses
  uIntelligentPosting;

{ TPublishItemThread }

procedure TPublishItemThread.InternalErrorHandler(AErrorMsg: string);
begin
  FErrorMsg := AErrorMsg;
end;

procedure TPublishItemThread.CAPTCHAInputThreaded;
begin
  FCAPTCHAResult := TCAPTCHAClass.CAPTCHAInput(FCAPTCHAImageUrl, FCAPTCHAName, FCAPTCHAText, FCAPTCHACookies);
  FWait.Signal;
end;

function TPublishItemThread.CAPTCHAInputSynchronizer(const AImageUrl, AName: WideString; out AText: WideString; var ACookies: WideString): WordBool;
begin
  FCAPTCHAImageUrl := AImageUrl;
  FCAPTCHAName := AName;
  FCAPTCHACookies := ACookies;
  FWait := CreateWaitableValue;
  task.Invoke(CAPTCHAInputThreaded);
  FWait.WaitFor;
  FWait := nil;
  AText := FCAPTCHAText;
  ACookies := FCAPTCHACookies;
  Result := FCAPTCHAResult;
end;

procedure TPublishItemThread.IntelligentPostingHandlerThreaded;
begin
  FIntelligentPostingResult := TIntelligentPostingClass.IntelligentPostingHandler(FIntelligentPostingSearchValue, FIntelligentPostingSearchResults, FIntelligentPostingSearchIndex, FIntelligentPostingRedoSearch);
  FWait.Signal;
end;

function TPublishItemThread.IntelligentPostingHandlerSynchronizer(var ASearchValue: WideString; const ASearchResults: WideString; var ASearchIndex: Integer; out ARedoSearch: WordBool): WordBool;
begin
  FIntelligentPostingSearchValue := ASearchValue;
  FIntelligentPostingSearchResults := ASearchResults;
  FIntelligentPostingSearchIndex := ASearchIndex;
  FWait := CreateWaitableValue;
  task.Invoke(IntelligentPostingHandlerThreaded);
  FWait.WaitFor;
  FWait := nil;
  ASearchValue := FIntelligentPostingSearchValue;
  ASearchIndex := FIntelligentPostingSearchIndex;
  ARedoSearch := FIntelligentPostingRedoSearch;
  Result := FIntelligentPostingResult;
end;

constructor TPublishItemThread.Create(const APublishItem: IPublishItem; APublishRetry: Integer);
begin
  inherited Create;

  FErrorMsg := '';
  FPublishItem := APublishItem;
  FPublishRetry := APublishRetry;
end;

function TPublishItemThread.Initialize: Boolean;
var
  WebsiteHintLabel, ErrorMsg: string;
  RepeatIndex: Integer;
  Success: Boolean;
begin
  if CheckforBlacklist(FPublishItem) then
    Exit(False);

  WebsiteHintLabel := RemoveW(ExtractUrlHost(FPublishItem.Website));

  if not task.Terminated then
  begin
    task.Comm.Send(MSG_PUBLISH_ITEM_TASK_STARTED, WebsiteHintLabel);
    try
      // 10 sek pausieren
      // sleep(10000);

      RepeatIndex := 0;
      Success := False; // FPublishRetry = 3

      repeat
        FErrorMsg := '';
        Success := TApiPlugin.CMSExec(FPublishItem, InternalErrorHandler, CAPTCHAInputSynchronizer, IntelligentPostingHandlerSynchronizer);

        // improve this later, that ErrorHandler ll only called when error really exists string check is shabby
        if not SameStr('', FErrorMsg) then
          task.Comm.Send(MSG_PUBLISH_ITEM_TASK_ERROR, WebsiteHintLabel + ': ' + FErrorMsg);

        // RepeatIndex = 0, 1, 2
        Inc(RepeatIndex);
        // RepeatIndex = 1, 2, 3

      until (Success or (RepeatIndex >= FPublishRetry));

    finally
      task.Comm.Send(MSG_PUBLISH_ITEM_TASK_COMPLETED, WebsiteHintLabel);
    end;
  end;

  task.Invoke(
    { } procedure
    { } begin
    { .. } task.SetTimer(1, 1, MSG_SLEEP);
    { } end);

  Result := True;
end;

procedure TPublishItemThread.Cleanup;
begin
  task.Comm.Send(MSG_TASK_QUIT, FPublishItem);
  FPublishItem := nil;
end;

{ TPublishPool }

procedure TPublishPool.OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);

  function GetProgressPosition: Extended;
  var
    CompletedCount: Integer;
  begin
    CompletedCount := FCompletedCount.value;
    Result := CompletedCount / FTotalCount * 100;
  end;

  function GetHintLabeledText: string;
  begin
    with FThreadStringList do
    begin
      Delimiter := ',';
      Result := DelimitedText;
    end;
  end;

begin
  case msg.MsgID of
    MSG_PUBLISH_ITEM_TASK_STARTED:
      begin
        FThreadStringList.Add(msg.MsgData.AsString);

        if Assigned(FOnGUIInteractionItem) then
          FOnGUIInteractionItem(pmisWORKING, FPublishJob, GetProgressPosition, GetHintLabeledText);

      end;
    MSG_PUBLISH_ITEM_TASK_ERROR:
      begin
        if Assigned(FOnGUIInteractionItem) then
          FOnGUIInteractionItem(pmisERROR, FPublishJob, GetProgressPosition, msg.MsgData.AsString);
      end;
    MSG_PUBLISH_ITEM_TASK_COMPLETED:
      begin
        FCompletedCount.Increment;

        if Assigned(FOnGUIInteractionItem) then
          FOnGUIInteractionItem(pmisWORKING, FPublishJob, GetProgressPosition, GetHintLabeledText);

        with FThreadStringList do
          Delete(IndexOf(msg.MsgData.AsString));
      end;
  else
    inherited OmniTEDTaskMessage(task, msg);
  end;
end;

constructor TPublishPool.Create;
begin
  inherited Create('TPublishPool', APublishRate);

  FPublishJob := APublishJob;
  FTotalCount := ATotalCount;

  FCompletedCount.value := 0;
  FThreadStringList := TThreadStringList.Create;

  FOnGUIInteractionItem := AOnGUIInteractionItem;
end;

procedure TPublishPool.AddPublishItem;
begin
  FInList.Add(APublishItem);
  CreateTask(TPublishItemThread.Create(APublishItem, APublishRetry), 'TPublishItemThread (' + APublishItem.Website + ')').MonitorWith(FOmniTED).Schedule(FThreadPool);
end;

procedure TPublishPool.RemovePublishItem;
var
  indxA, indxB: Integer;
begin
  indxA := FInList.IndexOf(APublishItem);
  if indxA <> -1 then
  begin
    indxB := FBlackList.IndexOf(APublishItem);
    if indxB = -1 then
      FBlackList.Add(APublishItem);
  end;
end;

destructor TPublishPool.Destroy;
begin
  FPublishJob := nil;
  FThreadStringList.Free;
  FOnGUIInteractionItem := nil;
  inherited Destroy;
end;

{ TPublishThread }

constructor TPublishThread.Create;
var
  I: Integer;
begin
  inherited Create;

  FPublishJob := APublishJob;
  FPublishRetry := APublishRetry;
  FPublishDelay := APublishDelay;

  FTotalCount := 0;
  for I := 0 to FPublishJob.Count - 1 do
    FTotalCount := FTotalCount + FPublishJob.Upload[I].Count;

  FPublishPool := TPublishPool.Create(FPublishJob, APublishRate, FTotalCount, AOnGUIInteractionItem);
end;

function TPublishThread.Initialize: Boolean;
var
  I, J: Integer;
begin
  if CheckforBlacklist(FPublishJob) then
  begin
    task.Comm.Send(MSG_PUBLISH_TASK_CANCELED, FPublishJob);
    Exit(False);
  end;

  for I := 0 to FPublishJob.Count - 1 do
    if not task.Terminated then
    begin
      if (I > 0) and (FPublishDelay > 0) then
      begin
        sleep(FPublishDelay);

        if CheckforBlacklist(FPublishJob) then
        begin
          task.Comm.Send(MSG_PUBLISH_TASK_CANCELED, FPublishJob);
          Exit(False);
        end;
      end;

      for J := 0 to FPublishJob.Upload[I].Count - 1 do
        FPublishPool.AddPublishItem(FPublishJob.Upload[I].Item[J], FPublishRetry);

      repeat
        sleep(250);
        if CheckforBlacklist(FPublishJob) then
        begin
          // send cancel info
          task.Comm.Send(MSG_PUBLISH_TASK_CANCELED, FPublishJob);
          // insert all started publish jobs to inner thread pool blacklist
          for J := 0 to FPublishJob.Upload[I].Count - 1 do
            FPublishPool.RemovePublishItem(FPublishJob.Upload[I].Item[J]);
          // wait now until inner thread pool task are finished
          repeat
            sleep(250);
          until (FPublishPool.IsIdle);
          // give tasks from inner thread pool some time for cleanup
          sleep(1250);
          // now exit, inner thread pool is idle
          Exit(False);
        end;
        sleep(250);
      until (FPublishPool.IsIdle);
    end;

  task.Invoke(
    { } procedure
    { } begin
    { .. } task.SetTimer(1, 1, MSG_SLEEP);
    { } end);

  // this musst be done here (instead of in Cleanup) because when user calls cancel, this call is not allowed
  task.Comm.Send(MSG_PUBLISH_TASK_FINISHED, FPublishJob);

  Result := True;
end;

procedure TPublishThread.Cleanup;
begin
  task.Comm.Send(MSG_TASK_QUIT, FPublishJob);
  FPublishPool.Free;
  FPublishJob := nil;
end;

{ TPublishManager }
function TPublishManager.GetNextUniqueID: Longword;
begin
  Inc(FNextUniqueID);
  Result := FNextUniqueID;
end;

procedure TPublishManager.OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
begin
  case msg.MsgID of
    MSG_PUBLISH_TASK_CANCELED:
      begin
        if Assigned(FOnGUIInteractionItem) then
          FOnGUIInteractionItem(pmisCANCELED, IPublishJob(msg.MsgData.AsInterface), 0, '');
      end;
    MSG_PUBLISH_TASK_FINISHED:
      begin
        if Assigned(FOnGUIInteractionItem) then
          FOnGUIInteractionItem(pmisFINISHED, IPublishJob(msg.MsgData.AsInterface), 100, '');
      end
    else
      inherited OmniTEDTaskMessage(task, msg);
  end;
end;

constructor TPublishManager.Create;
begin
  inherited Create('TPublishManager', 1);
  FNextUniqueID := 0;
end;

function TPublishManager.AddPublishJob(const APublishJob: IPublishJob): Longword;
var
  PublishRate, PublishDelay, PublishRetry: Integer;

  UniqueID: Longword;
begin
  with SettingsManager.Settings.Publish do
  begin
    PublishRate := PublishMaxCount;
    PublishDelay := PublishDelaybetweenUploads;
    PublishRetry := RetryCount;
  end;

  FInList.Add(APublishJob);
  UniqueID := GetNextUniqueID;
  APublishJob.UniqueID := UniqueID;
  CreateTask(TPublishThread.Create(APublishJob, PublishRate, PublishDelay, PublishRetry, FOnGUIInteractionItem), 'TPublishThread (' + APublishJob.Description + ')').MonitorWith(FOmniTED).Schedule(FThreadPool);
  if Assigned(FOnGUIInteractionItem) then
    FOnGUIInteractionItem(pmisCREATED, APublishJob, 0, '');
  Result := UniqueID;
end;

procedure TPublishManager.RemovePublishJob(const APublishJob: IPublishJob);
var
  indxA, indxB: Integer;
begin
  indxA := FInList.IndexOf(APublishJob);
  if indxA <> -1 then
  begin
    indxB := FBlackList.IndexOf(APublishJob);
    if indxB = -1 then
      FBlackList.Add(APublishJob);
  end;
end;

procedure TPublishManager.RemovePublishJob(const AUniqueID: Longword);

  function Find(AUniqueID: Longword; AList: TInterfaceList): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    with AList do
      for I := 0 to Count - 1 do
        if AUniqueID = IPublishJob(Items[I]).UniqueID then
          Exit(I);
  end;

var
  indxA, indxB: Integer;
begin
  indxA := Find(AUniqueID, FInList);
  if indxA <> -1 then
  begin
    indxB := Find(AUniqueID, FBlackList);
    if indxB = -1 then
      FBlackList.Add(FInList[indxA]);
  end;
end;

procedure TPublishManager.RemoveAllJobs;
var
  I: Integer;
begin
  for I := 0 to FInList.Count - 1 do
    RemovePublishJob(IPublishJob(FInList[I]));

  Pause;
end;

procedure TPublishManager.Resume;
begin
  FThreadPool.MaxExecuting := 1;

  if Assigned(FOnGUIInteraction) then
    FOnGUIInteraction(pmsRESUMING);
end;

procedure TPublishManager.Pause;
begin
  FThreadPool.MaxExecuting := 0;
  if Assigned(FOnGUIInteraction) then
    FOnGUIInteraction(pmsPAUSING);
end;

end.
