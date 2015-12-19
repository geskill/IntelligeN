unit uApiPublishManager;

interface

uses
  // Delphi
  Windows, Forms, SysUtils, Classes, Messages,
  // OmniThreadLibrary
  OtlCollections, OtlComm, OtlCommon, OtlParallel, OtlTask, OtlTaskControl, OtlThreadPool,
  // Generic TThreadList
  hThreadList,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // API
  uApiPlugins, uApiThreadManager,
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

    FPublishJob: IPublishJob;
    FTotalCount: Integer;
    FCompletedCount: TOmniAlignedInt32;
    FThreadStringList: TThreadList<string>;

    FOnGUIInteractionItem: TGUIInteractionItemEvent;
    function GetProgressPosition: Extended;
  protected
    procedure OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); override;
  public
    constructor Create(const APublishJob: IPublishJob; APublishRate: Integer; AOnGUIInteractionItem: TGUIInteractionItemEvent); reintroduce;
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
    FInnerPublishManager: TPublishInnerManager;
    function Initialize: Boolean; override;
  public
    constructor Create(const APublishJob: IPublishJob; APublishRate, APublishDelay, APublishRetry: Integer; AOnGUIInteractionItem: TGUIInteractionItemEvent); reintroduce;
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
    function InAnyList(const ATabSheetController: ITabSheetController): Boolean; override;
    procedure OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); override;
  public
    constructor Create;
    destructor Destroy; override;

    function AddPublishJob(const APublishJob: IPublishJob): LongWord;
    procedure RemovePublishJob(const APublishJob: IPublishJob); overload;
    procedure RemovePublishJob(const AUniqueID: LongWord); overload;
    procedure RemoveAllPublishJobs; // i.e. STOP

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

uses
  // Api
  uApiSettings;

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
  OutputDebugString('MSG_PUBLISH_ITEM_TASK_CREATED');

  if not task.Terminated then
  begin
    try

      try
        LRepeatIndex := 0;
        LSuccess := False; // FPublishRetry = 3

        repeat
          with TApiThreadedPlugin.Create(task, DefaultErrorHandler) do
            try
              LSuccess := CMSExec(Data.PublishItem);
            finally
              Free;
            end;
          OutputDebugString('Publish finished');

          if FHasError then
          begin
            task.Comm.Send(MSG_PUBLISH_ITEM_TASK_ERROR, [task.UniqueID, LOmniValue.AsObject, FErrorMsg]);
          end;

          // LRepeatIndex = 0, 1, 2
          Inc(LRepeatIndex);
          // LRepeatIndex = 1, 2, 3

        until (LSuccess or (LRepeatIndex > Data.PublishRetry));

      finally
        task.Comm.Send(MSG_PUBLISH_ITEM_TASK_COMPLETED, [task.UniqueID, LOmniValue.AsObject]);
        OutputDebugString('MSG_PUBLISH_ITEM_TASK_COMPLETED');
      end;

    finally
      Finish;
    end;
  end;
  OutputDebugString('TPublishInnerThread.Execute END');
end;

{ TPublishInnerManager }

function TPublishInnerManager.GetProgressPosition: Extended;
begin
  Result := FCompletedCount.Value / FTotalCount * 100;
end;

procedure TPublishInnerManager.OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);

  function GetHintLabeledText: string;
  var
    LHost: string;
  begin
    Result := '';
    for LHost in FThreadStringList do
    begin
      if not SameStr('', Result) then
        Result := Result + ', ';
      Result := Result + LHost;
    end;
  end;

var
  LJobWorkData: TPublishInnerData;
begin
  LJobWorkData := TPublishInnerData(msg.MsgData[1].AsObject);

  case msg.MsgID of
    MSG_PUBLISH_ITEM_TASK_CREATED:
      begin
        FThreadStringList.Add(LJobWorkData.PublishItem.HostWithPath);

        if Assigned(FOnGUIInteractionItem) then
          FOnGUIInteractionItem(pmisWORKING, FPublishJob, GetProgressPosition, GetHintLabeledText);
      end;
    MSG_PUBLISH_ITEM_TASK_ERROR:
      begin
        if Assigned(FOnGUIInteractionItem) then
          FOnGUIInteractionItem(pmisERROR, FPublishJob, GetProgressPosition, msg.MsgData[2].AsString);
      end;
    MSG_PUBLISH_ITEM_TASK_COMPLETED:
      begin
        FThreadStringList.Remove(LJobWorkData.PublishItem.HostWithPath);
        FCompletedCount.Increment;

        if Assigned(FOnGUIInteractionItem) then
          FOnGUIInteractionItem(pmisWORKING, FPublishJob, GetProgressPosition, GetHintLabeledText);
      end
    else
    begin
      inherited OmniEMTaskMessage(task, msg);
    end;
  end;
end;

constructor TPublishInnerManager.Create(const APublishJob: IPublishJob; APublishRate: Integer; AOnGUIInteractionItem: TGUIInteractionItemEvent);
var
  FPublishJobIndex: Integer;
begin
  inherited Create;

  FThreadPool := CreateThreadPool('TPublishInnerManager');
  with FThreadPool do
  begin
    MaxExecuting := APublishRate;
    MaxQueued := 0;
  end;

  FPublishJob := APublishJob;

  FTotalCount := 0;
  for FPublishJobIndex := 0 to FPublishJob.Count - 1 do
    FTotalCount := FTotalCount + FPublishJob.Upload[FPublishJobIndex].Count;

  FCompletedCount.Value := 0;
  FThreadStringList := TThreadList<string>.Create(False);
  FOnGUIInteractionItem := AOnGUIInteractionItem;
end;

destructor TPublishInnerManager.Destroy;
begin
  FOnGUIInteractionItem := nil;
  FThreadStringList.Free;
  FPublishJob := nil;

  if Assigned(FThreadPool) then
  begin
    FThreadPool.CancelAll;
    FThreadPool := nil;
  end;
  inherited Destroy;
end;

procedure TPublishInnerManager.AddPublishItem(const APublishItem: IPublishItem; APublishRetry: Integer);
var
  LPublishInnerThread: TPublishInnerThread;
begin
  LPublishInnerThread := TPublishInnerThread.Create(APublishItem, APublishRetry);
  AddJob(LPublishInnerThread.Data);
  CreateTask(LPublishInnerThread).MonitorWith(FOmniEM).Schedule(FThreadPool).Invoke(@TPublishInnerThread.Execute);
end;

procedure TPublishInnerManager.RemovePublishItem(const APublishItem: IPublishItem);
begin
  RemoveIterator(
    { } procedure(const AJobWorkData: TPublishInnerData; var ARemove: Boolean)
    { } begin
    { . } ARemove := (APublishItem = AJobWorkData.PublishItem);
    { } end);
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

function TPublishThread.Initialize: Boolean;
begin
  Result := True; // Do not check for InBlackList
end;

constructor TPublishThread.Create(const APublishJob: IPublishJob; APublishRate, APublishDelay, APublishRetry: Integer; AOnGUIInteractionItem: TGUIInteractionItemEvent);
begin
  inherited Create;

  // TODO: Data.TabSheetController := ???;

  Data.PublishJob := APublishJob;
  Data.PublishDelay := APublishDelay;
  Data.PublishRetry := APublishRetry;

  FInnerPublishManager := TPublishInnerManager.Create(APublishJob, APublishRate, AOnGUIInteractionItem);
end;

destructor TPublishThread.Destroy;
begin
  FInnerPublishManager.Free;

  inherited Destroy;
end;

procedure TPublishThread.Execute;
var
  LOmniValue: TOmniValue;

  FPublishJobIndex, FPublishJobItemIndex: Integer;
begin
  LOmniValue := TOmniValue.CastFrom<TPublishData>(Data);

  try
    for FPublishJobIndex := 0 to Data.PublishJob.Count - 1 do
    begin
      if not task.Terminated then
      begin
        if (FPublishJobIndex > 0) and (Data.PublishDelay > 0) then
        begin
          sleep(Data.PublishDelay);
        end;

        if InBlackList then // Check here for InBlackList because not done in TPublishThread.Initialize
        begin
          task.Comm.Send(MSG_PUBLISH_TASK_CANCELED, [task.UniqueID, LOmniValue.AsObject]);
          Exit;
        end;

        for FPublishJobItemIndex := 0 to Data.PublishJob.Upload[FPublishJobIndex].Count - 1 do
          FInnerPublishManager.AddPublishItem(Data.PublishJob.Upload[FPublishJobIndex].Item[FPublishJobItemIndex], Data.PublishRetry);

        repeat
          sleep(500);

          if InBlackList then
          begin
            // send cancel info
            task.Comm.Send(MSG_PUBLISH_TASK_CANCELED, [task.UniqueID, LOmniValue.AsObject]);

            // insert all started publish jobs to inner thread pool blacklist
            for FPublishJobItemIndex := 0 to Data.PublishJob.Upload[FPublishJobIndex].Count - 1 do
              FInnerPublishManager.RemovePublishItem(Data.PublishJob.Upload[FPublishJobIndex].Item[FPublishJobItemIndex]);

            // wait now until inner thread pool task are finished
            repeat
              sleep(500);
            until (FInnerPublishManager.IsIdle);

            // give tasks from inner thread pool some time for cleanup
            sleep(1250);

            // now exit, inner thread pool is idle
            Exit;
          end;

          sleep(500);

        until (FInnerPublishManager.IsIdle);
      end;
    end;

    // this musst be done here (instead of in Cleanup) because when user calls cancel, this call is not allowed
    task.Comm.Send(MSG_PUBLISH_TASK_FINISHED, [task.UniqueID, LOmniValue.AsObject]);
  finally

    Finish;
  end;
end;

{ TPublishManager }

function TPublishManager.GetNextUniqueID: LongWord;
begin
  Inc(FNextUniqueID);
  Result := FNextUniqueID;
end;

function TPublishManager.InAnyList(const ATabSheetController: ITabSheetController): Boolean;
var
  LListIndex: Integer;
begin
  Result := True;
  for LListIndex := 0 to FInList.Count - 1 do
  begin
    // TODO: Add loop
    // if ATabSheetController = FInList[LListIndex].PublishJob.Upload[] then
    // begin
    // Result := True;
    // Break;
    // end;
  end;
  for LListIndex := 0 to FBlackList.Count - 1 do
  begin
    // TODO: Add loop
    // if ATabSheetController = FBlackList[LListIndex].TabSheetController then
    // begin
    // Result := True;
    // Break;
    // end;
  end;
end;

procedure TPublishManager.OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
var
  LJobWorkData: TPublishData;
begin
  LJobWorkData := TPublishData(msg.MsgData[1].AsObject);

  case msg.MsgID of
    MSG_PUBLISH_TASK_CANCELED:
      begin
        if Assigned(FOnGUIInteractionItem) then
          FOnGUIInteractionItem(pmisCANCELED, LJobWorkData.PublishJob, 0, '');
      end;
    MSG_PUBLISH_TASK_FINISHED:
      begin
        if Assigned(FOnGUIInteractionItem) then
          FOnGUIInteractionItem(pmisFINISHED, LJobWorkData.PublishJob, 100, '');
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
  FOnGUIInteraction := nil;
  FOnGUIInteractionItem := nil;

  if Assigned(FThreadPool) then
  begin
    FThreadPool.CancelAll;
    FThreadPool := nil;
  end;
  inherited Destroy;
end;

function TPublishManager.AddPublishJob(const APublishJob: IPublishJob): LongWord;
var
  LUniqueID: LongWord;
  LPublishThread: TPublishThread;

  LPublishRate, LPublishDelay, LPublishRetry: Integer;
begin
  with SettingsManager.Settings.Publish do
  begin
    LPublishRate := PublishMaxCount;
    LPublishDelay := PublishDelaybetweenUploads;
    LPublishRetry := RetryCount;
  end;

  LUniqueID := GetNextUniqueID;
  APublishJob.UniqueID := LUniqueID;

  LPublishThread := TPublishThread.Create(APublishJob, LPublishRate, LPublishDelay, LPublishRetry, FOnGUIInteractionItem);
  AddJob(LPublishThread.Data);
  CreateTask(LPublishThread).MonitorWith(FOmniEM).Schedule(FThreadPool).Invoke(@TPublishThread.Execute);

  if Assigned(FOnGUIInteractionItem) then
    FOnGUIInteractionItem(pmisCREATED, APublishJob, 0, '');

  Result := LUniqueID;
end;

procedure TPublishManager.RemovePublishJob(const APublishJob: IPublishJob);
begin
  RemovePublishJob(APublishJob.UniqueID);
end;

procedure TPublishManager.RemovePublishJob(const AUniqueID: LongWord);
begin
  RemoveIterator(
    { } procedure(const AJobWorkData: TPublishData; var ARemove: Boolean)
    { } begin
    { . } ARemove := (AUniqueID = AJobWorkData.PublishJob.UniqueID);
    { } end);
end;

procedure TPublishManager.RemoveAllPublishJobs;
begin
  RemoveAllJobs;
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
