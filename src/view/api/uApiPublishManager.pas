unit uApiPublishManager;

interface

uses
  // Delphi
  Windows, Forms, SysUtils, Classes, Messages,
  // OmniThreadLibrary
  GpStuff, OtlCollections, OtlComm, OtlCommon, OtlParallel, OtlTask, OtlTaskControl, OtlThreadPool,
  // Generic TThreadList
  hThreadList,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // API
  uApiPlugins, uApiSettings, uApiThreadManager,
  // Utils
  uPathUtils, uStringUtils;

type
  TPublishManagerStatus = (pmsRESUMING, pmsPAUSING, pmsSTOPPING);
  TPublishManagerItemStatus = (pmisCREATED, pmisWORKING, pmisERROR, pmisCANCELED, pmisFINISHED);

  TGUIInteractionEvent = procedure(Status: TPublishManagerStatus) of object;
  TGUIInteractionItemEvent = procedure(Status: TPublishManagerItemStatus; PublishJob: IPublishJob; AProgressPosition: Extended; msg: string) of object;

  TPublishInnerData = class(TThreadWorkData)
  protected
    FPublishItem: IPublishItem;
    FPublishRetry: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    property PublishItem: IPublishItem read FPublishItem write FPublishItem;
    property PublishRetry: Integer read FPublishRetry write FPublishRetry;
  end;

  TPublishInnerThread = class(TThreadWorker<TPublishInnerData>)
  protected
    FHasError: Boolean;
    FErrorMsg: string;
    procedure DefaultErrorHandler(AErrorMsg: string); override;
  public
    constructor Create(const APublishItem: IPublishItem; const APublishRetry: Integer); reintroduce;
    destructor Destroy; override;

    procedure Execute; override;
  end;

  TPublishInnerManager = class(TThreadManager<TPublishInnerData>)
  private
    FThreadPool: IOmniThreadPool;

    (*
      FCompletedCount: TGp4AlignedInt;
      FThreadStringList: TThreadList<string>;

      FPublishJob: IPublishJob;
      FTotalCount: Integer;
      *)

    FOnGUIInteractionItem: TGUIInteractionItemEvent;
  protected
    procedure OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); override;
  public
    constructor Create(const APublishJob: IPublishJob; APublishRate, ATotalCount: Integer; AOnGUIInteractionItem: TGUIInteractionItemEvent); reintroduce;
    destructor Destroy; override;

    procedure AddPublishItem(const APublishItem: IPublishItem; APublishRetry: Integer);
    procedure RemovePublishItem(const APublishItem: IPublishItem);

    property OnGUIInteractionItem: TGUIInteractionItemEvent read FOnGUIInteractionItem write FOnGUIInteractionItem;
  end;

  TPublishData = class(TThreadWorkData)
  protected
    FPublishJob: IPublishJob;
    FPublishDelay, FPublishRetry, FPublishTotalCount: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    property PublishJob: IPublishJob read FPublishJob write FPublishJob;
    property PublishDelay: Integer read FPublishDelay write FPublishDelay;
    property PublishRetry: Integer read FPublishRetry write FPublishRetry;
    property PublishTotalCount: Integer read FPublishTotalCount write FPublishTotalCount;
  end;

  TPublishThread = class(TThreadWorker<TPublishData>)
  protected

  public
    constructor Create(const APublishJob: IPublishJob; APublishRate, APublishDelay, APublishRetry: Integer; AOnGUIInteractionItem: TGUIInteractionItemEvent);
    destructor Destroy; override;

    procedure Execute; override;
  end;

  TPublishManager = class(TThreadManager<TPublishData>, IPublishManager)
  private
    FThreadPool: IOmniThreadPool;
    FNextUniqueID: LongWord;

    FOnGUIInteraction: TGUIInteractionEvent;
    FOnGUIInteractionItem: TGUIInteractionItemEvent;
    function GetNextUniqueID: LongWord;
  protected
    procedure OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); override;
  public
    constructor Create;
    destructor Destroy; override;

    function AddPublishJob(const APublishJob: IPublishJob): LongWord;
    procedure RemovePublishJob(const APublishJob: IPublishJob); overload;
    procedure RemovePublishJob(const AUniqueID: LongWord); overload;
    procedure RemoveAllJobs; // i.e. STOP

    procedure Resume; // play
    procedure Pause; // pause

    property OnGUIInteraction: TGUIInteractionEvent read FOnGUIInteraction write FOnGUIInteraction;
    property OnGUIInteractionItem: TGUIInteractionItemEvent read FOnGUIInteractionItem write FOnGUIInteractionItem;
  end;

const
  MSG_PUBLISH_ITEM_TASK_CREATED = 10;
  MSG_PUBLISH_ITEM_TASK_ERROR = 11;
  MSG_PUBLISH_ITEM_TASK_COMPLETED = 12;

  MSG_PUBLISH_TASK_CANCELED = 13;
  MSG_PUBLISH_TASK_FINISHED = 14;

implementation

{ TPublishInnerData }

constructor TPublishInnerData.Create;
begin
  inherited Create;
end;

destructor TPublishInnerData.Destroy;
begin
  FPublishItem := nil;
  inherited Destroy;
end;

{ TPublishThread }

procedure TPublishInnerThread.DefaultErrorHandler(AErrorMsg: string);
begin
  FHasError := True;
  FErrorMsg := AErrorMsg;
end;

constructor TPublishInnerThread.Create;
begin
  inherited Create;

  // TODO: Data.TabSheetController := ???;

  Data.PublishItem := APublishItem;
  Data.PublishRetry := APublishRetry;

  FHasError := False;
  FErrorMsg := '';
end;

destructor TPublishInnerThread.Destroy;
begin
  inherited Destroy;
end;

procedure TPublishInnerThread.Execute;
var
  LOmniValue: TOmniValue;

  LRepeatIndex: Integer;
  LSuccess: Boolean;
begin
  LOmniValue := TOmniValue.CastFrom<TPublishInnerData>(Data);
  task.Comm.Send(MSG_PUBLISH_ITEM_TASK_CREATED, [task.UniqueID, LOmniValue.AsObject]);

  if not task.Terminated then
  begin

    try
      LRepeatIndex := 0;
      LSuccess := False; // FPublishRetry = 3

      repeat
        FErrorMsg := '';

        with TApiThreadedPlugin.Create(task, DefaultErrorHandler) do
          try
            LSuccess := CMSExec(Data.PublishItem);
          finally
            Free;
          end;

        if FHasError then
        begin
          task.Comm.Send(MSG_PUBLISH_ITEM_TASK_ERROR, [task.UniqueID, LOmniValue.AsObject, FErrorMsg]);
        end;

        // RepeatIndex = 0, 1, 2
        Inc(LRepeatIndex);
        // RepeatIndex = 1, 2, 3

      until (LSuccess or (LRepeatIndex >= Data.PublishRetry));

    finally
      task.Comm.Send(MSG_PUBLISH_ITEM_TASK_COMPLETED, [task.UniqueID, LOmniValue.AsObject]);
    end;

    Finish;
  end;
end;

{ TPublishInnerManager }

procedure TPublishInnerManager.OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);

(*
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
  // TODO:
  // Delimiter := ',';
  Result := ToString;
  end;
  end;
  *)

begin
  (*
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

    // TODO:
    // with FThreadStringList do
    // Delete(IndexOf(msg.MsgData.AsString));
    end;
    else
    inherited OmniTEDTaskMessage(task, msg);
    end;
    *)
end;

constructor TPublishInnerManager.Create(const APublishJob: IPublishJob; APublishRate, ATotalCount: Integer; AOnGUIInteractionItem: TGUIInteractionItemEvent);
begin
  inherited Create;

  FThreadPool := CreateThreadPool('TPublishInnerManager');
  with FThreadPool do
  begin
    MaxExecuting := APublishRate;
    MaxQueued := 0;
  end;

  (*
    FPublishJob := APublishJob;
    FTotalCount := ATotalCount;

    FCompletedCount.value := 0;
    FThreadStringList := TThreadList<string>.Create(False);
    *)

  FOnGUIInteractionItem := AOnGUIInteractionItem;
end;

destructor TPublishInnerManager.Destroy;
begin
  FOnGUIInteractionItem := nil;
  (*
    FPublishJob := nil;
    FThreadStringList.Free;
    *)
  if Assigned(FThreadPool) then
  begin
    FThreadPool.CancelAll;
    FThreadPool := nil;
  end;
  inherited Destroy;
end;

procedure TPublishInnerManager.AddPublishItem(const APublishItem: IPublishItem; APublishRetry: Integer);
begin
  (*
    FInList.Add(APublishItem);
    CreateTask(TPublishItemThread.Create(APublishItem, APublishRetry), 'TPublishItemThread (' + APublishItem.Website + ')').MonitorWith(FOmniTED).Schedule(FThreadPool);
    *)
end;

procedure TPublishInnerManager.RemovePublishItem(const APublishItem: IPublishItem);
var
  indxA, indxB: Integer;
begin
  (*
    indxA := FInList.IndexOf(APublishItem);
    if indxA <> -1 then
    begin
    indxB := FBlackList.IndexOf(APublishItem);
    if indxB = -1 then
    FBlackList.Add(APublishItem);
    end;
    *)
end;

{ ****************************************************************************** }

{ TPublishData }

constructor TPublishData.Create;
begin
  inherited Create;
end;

destructor TPublishData.Destroy;
begin
  FPublishJob := nil;
  inherited Destroy;
end;

{ TPublishThread }

constructor TPublishThread.Create(const APublishJob: IPublishJob; APublishRate, APublishDelay, APublishRetry: Integer; AOnGUIInteractionItem: TGUIInteractionItemEvent);
begin
  inherited Create;

  // TODO: Data.TabSheetController := ???;

  Data.PublishJob := APublishJob;
  Data.PublishDelay := APublishDelay;
  Data.PublishRetry := APublishRetry;

  // TODO:
  (*
    FTotalCount := 0;
    for I := 0 to FPublishJob.Count - 1 do
    FTotalCount := FTotalCount + FPublishJob.Upload[I].Count;

    FPublishPool := TPublishPool.Create(FPublishJob, APublishRate, FTotalCount, AOnGUIInteractionItem);
    *)
end;

destructor TPublishThread.Destroy;
begin
  inherited Destroy;
end;

procedure TPublishThread.Execute;
begin
  (*
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
    *)
end;

{ TPublishManager }

function TPublishManager.GetNextUniqueID: LongWord;
begin
  Inc(FNextUniqueID);
  Result := FNextUniqueID;
end;

procedure TPublishManager.OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
var
  LJobWorkData: TPublishData;
begin
  LJobWorkData := TPublishData(msg.MsgData[1].AsObject);

  case msg.MsgID of
    MSG_PUBLISH_TASK_CANCELED:
      begin
        // if Assigned(FOnGUIInteractionItem) then
        // FOnGUIInteractionItem(pmisCANCELED, IPublishJob(msg.MsgData.AsInterface), 0, '');
      end;
    MSG_PUBLISH_TASK_FINISHED:
      begin
        // if Assigned(FOnGUIInteractionItem) then
        // FOnGUIInteractionItem(pmisFINISHED, IPublishJob(msg.MsgData.AsInterface), 100, '');
      end
    else
    begin
      inherited OmniEMTaskMessage(task, msg);
    end;
  end;
end;

constructor TPublishManager.Create;
begin
  inherited Create;

  FThreadPool := CreateThreadPool('TPublishManager');
  with FThreadPool do
  begin
    MaxExecuting := 1;
    MaxQueued := 0;
  end;

  FNextUniqueID := 0;
end;

destructor TPublishManager.Destroy;
begin
  if Assigned(FThreadPool) then
  begin
    FThreadPool.CancelAll;
    FThreadPool := nil;
  end;
  inherited Destroy;
end;

function TPublishManager.AddPublishJob(const APublishJob: IPublishJob): LongWord;
var
  PublishRate, PublishDelay, PublishRetry: Integer;

  UniqueID: LongWord;
begin
  with SettingsManager.Settings.Publish do
  begin
    PublishRate := PublishMaxCount;
    PublishDelay := PublishDelaybetweenUploads;
    PublishRetry := RetryCount;
  end;

  (*
    FInList.Add(APublishJob);
    UniqueID := GetNextUniqueID;
    APublishJob.UniqueID := UniqueID;
    CreateTask(TPublishThread.Create(APublishJob, PublishRate, PublishDelay, PublishRetry, FOnGUIInteractionItem), 'TPublishThread (' + APublishJob.Description + ')').MonitorWith(FOmniTED).Schedule(FThreadPool);
    if Assigned(FOnGUIInteractionItem) then
    FOnGUIInteractionItem(pmisCREATED, APublishJob, 0, '');
    Result := UniqueID;
    *)
end;

procedure TPublishManager.RemovePublishJob(const APublishJob: IPublishJob);
var
  indxA, indxB: Integer;
begin
  (*
    indxA := FInList.IndexOf(APublishJob);
    if indxA <> -1 then
    begin
    indxB := FBlackList.IndexOf(APublishJob);
    if indxB = -1 then
    FBlackList.Add(APublishJob);
    end;
    *)
end;

procedure TPublishManager.RemovePublishJob(const AUniqueID: LongWord);

  function Find(AUniqueID: LongWord; AList: TInterfaceList): Integer;
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
  (*
    indxA := Find(AUniqueID, FInList);
    if indxA <> -1 then
    begin
    indxB := Find(AUniqueID, FBlackList);
    if indxB = -1 then
    FBlackList.Add(FInList[indxA]);
    end;
    *)
end;

procedure TPublishManager.RemoveAllJobs;
var
  I: Integer;
begin
  (*
    for I := 0 to FInList.Count - 1 do
    RemovePublishJob(IPublishJob(FInList[I]));

    Pause;
    *)
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
