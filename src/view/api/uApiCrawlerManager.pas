unit uApiCrawlerManager;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes, Generics.Collections,
  // OmniThreadLibrary
  OtlComm, OtlCommon, OtlCollections, OtlTaskControl,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiThreadManager, uApiPlugins, uApiSettings;

type
  TCrawlerTaskStatus = (ctsCREATED, ctsWORKING, ctsFINISHED);
  TCrawlerGUIInteractionEvent = procedure(const AControlController: IControlController; AStatus: TCrawlerTaskStatus; AProgressPosition: Extended; AMessage: string) of object;

  TCrawlerData = class(TThreadWorkData)
  protected
    FControlController: IControlController;
  public
    constructor Create; override;
    destructor Destroy; override;

    property ControlController: IControlController read FControlController write FControlController;
  end;

  TCrawlerThread = class(TThreadWorker<TCrawlerData>)
  protected
    FFormatSettings: TFormatSettings;
    FTypeID: TTypeID;
    FControlControllerBase: IControlControllerBase;
    FOmniBlockingCollection: TOmniBlockingCollection; // contains all used CrawlerCollectionItem's
    FCrawlerCount: Integer;
    FCrawlersFinished: Integer;

    procedure SetPossibleControlValue;
    procedure UpdateControlValues;
    procedure InitiateImageRemoteUpload;
  public
    constructor Create(const AControlController: IControlController); reintroduce;
    destructor Destroy; override;

    procedure Execute; override;
  end;

  TCrawlerManager = class(TThreadManager<TCrawlerData>, ICrawlerManager)
  protected
    FOnGUIInteraction: TCrawlerGUIInteractionEvent;
  protected
    procedure OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddCrawlerJob(const AControlController: IControlController);
    procedure RemoveCrawlerJob(const AControlController: IControlController);

    property OnGUIInteraction: TCrawlerGUIInteractionEvent read FOnGUIInteraction write FOnGUIInteraction;
  end;

const
  MSG_CRAWLER_TASK_CREATED = 5;
  MSG_CRAWLER_TASK_STARTED = 6; // One crawler from a subset of crawlers for one tab begins to work
  MSG_CRAWLER_TASK_FINISHED = 7;

implementation

{ TCrawlerData }

constructor TCrawlerData.Create;
begin
  inherited Create;
end;

destructor TCrawlerData.Destroy;
begin
  FControlController := nil;
  inherited Destroy;
end;

{ TCrawlerThread }

procedure TCrawlerThread.SetPossibleControlValue;
var
  LControlIndex: Integer;
begin
  for LControlIndex := 0 to FControlControllerBase.ControlCount - 1 do
    with FControlControllerBase.Control[LControlIndex] do
      UpdateValueFromProposedValue;
end;

procedure TCrawlerThread.UpdateControlValues;
var
  LControlIndex: Integer;

  LControlBase: IControlBase;
  LCrawledControlProposedValueIndex: Integer;
begin
  for LControlIndex := 0 to Data.ControlController.ControlCount - 1 do
    with Data.ControlController.Control[LControlIndex] do
    begin
      LControlBase := FControlControllerBase.FindControl(ControlID);
      try
        if Assigned(LControlBase) then
        begin
          // Store all data from the crawler plugins stored in the virtual controls, in the "real" controls
          for LCrawledControlProposedValueIndex := 0 to LControlBase.ProposedValuesCount - 1 do
          begin
            AddProposedValue(LControlBase.GetProposedValueSender(LCrawledControlProposedValueIndex), LControlBase.GetProposedValue(LCrawledControlProposedValueIndex), LControlBase.GetProposedValueTitle(LCrawledControlProposedValueIndex));
          end;

          // Set "real" control value
          Value := LControlBase.Value;
        end;
      finally
        LControlBase := nil;
      end;
    end;
end;

procedure TCrawlerThread.InitiateImageRemoteUpload;
var
  LControlBasic: IControlBasic;
  LPicture: IPicture;
begin
  LControlBasic := Data.ControlController.FindControl(cPicture);
  try
    if Assigned(LControlBasic) then
    begin
      LControlBasic.QueryInterface(IPicture, LPicture);
      try
        if not SameStr('', LPicture.Value) then
          LPicture.RemoteUpload(True);
      finally
        LPicture := nil;
      end;
    end;
  finally
    LControlBasic := nil;
  end;
end;

constructor TCrawlerThread.Create(const AControlController: IControlController);
var
  LCrawlerIndex: Integer;
  LCrawlerCollectionItem: TCrawlerCollectionItem;

  LFound: Boolean;
  LContingentIndex: Integer;
  LCrawlerContingentCollectionItem: TCrawlerContingentCollectionItem;
begin
  inherited Create;

  Data.TabSheetController := AControlController.TabSheetController;

  Data.ControlController := AControlController;

  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, FFormatSettings);
  FTypeID := AControlController.TypeID;
  FControlControllerBase := AControlController.CloneInstance;
  FOmniBlockingCollection := TOmniBlockingCollection.Create();
  FCrawlerCount := 0;
  FCrawlersFinished := 0;

  for LCrawlerIndex := 0 to SettingsManager.Settings.Plugins.Crawler.Count - 1 do
  begin
    LCrawlerCollectionItem := TCrawlerCollectionItem(SettingsManager.Settings.Plugins.Crawler.Items[LCrawlerIndex]);
    if LCrawlerCollectionItem.Enabled then
    begin
      LFound := False;
      LContingentIndex := 0;
      // überprüfung ob mindestens ein ComponentItem aktiviert ist
      while not LFound and (LContingentIndex < LCrawlerCollectionItem.Contingent.Count) do
      begin
        LCrawlerContingentCollectionItem := TCrawlerContingentCollectionItem(LCrawlerCollectionItem.Contingent.Items[LContingentIndex]);
        LFound := (AControlController.TypeID = LCrawlerContingentCollectionItem.TypeID) and LCrawlerContingentCollectionItem.Status;
        Inc(LContingentIndex);
      end;
      if LFound then
      begin
        Inc(FCrawlerCount);
        FOmniBlockingCollection.Add(TOmniValue.CastFrom(LCrawlerCollectionItem));
      end;
    end;
  end;

  FOmniBlockingCollection.CompleteAdding;
end;

destructor TCrawlerThread.Destroy;
begin
  FControlControllerBase := nil;
  FOmniBlockingCollection.Free;
  inherited Destroy;
end;

procedure TCrawlerThread.Execute;
var
  LOmniValue: TOmniValue;

  LOmniCrawlerCollectionItem: TOmniValue;
  LCrawlerCollectionItem: TCrawlerCollectionItem;
begin
  LOmniValue := TOmniValue.CastFrom<TCrawlerData>(Data);
  task.Comm.Send(MSG_CRAWLER_TASK_CREATED, [task.UniqueID, LOmniValue.AsObject, FCrawlerCount]);

  while not task.Terminated and FOmniBlockingCollection.Take(LOmniCrawlerCollectionItem) do
  begin
    LCrawlerCollectionItem := TCrawlerCollectionItem(LOmniCrawlerCollectionItem.AsObject);

    task.Comm.Send(MSG_CRAWLER_TASK_STARTED, [task.UniqueID, LOmniValue.AsObject, FCrawlersFinished / FCrawlerCount, LCrawlerCollectionItem.Name]);

    with TApiThreadedPlugin.Create(task) do
      try
        CrawlerExec(LCrawlerCollectionItem, FTypeID, FControlControllerBase);
      finally
        Free;
      end;

    SetPossibleControlValue;
    Inc(FCrawlersFinished);
  end;

  task.Invoke(
    { } procedure
    { } begin
    { .. } UpdateControlValues;
    { .. } InitiateImageRemoteUpload;

    { .. } Finish;
    { } end);

  task.Comm.Send(MSG_CRAWLER_TASK_FINISHED, [task.UniqueID, LOmniValue.AsObject]);
end;

{ TCrawlerManager }

procedure TCrawlerManager.OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
var
  LJobWorkData: TCrawlerData;
  LCrawlerCountToFinishedRatio: Extended;
begin
  LJobWorkData := TCrawlerData(msg.MsgData[1].AsObject);

  case msg.MsgID of
    MSG_CRAWLER_TASK_CREATED:
      begin
        if Assigned(FOnGUIInteraction) then
          FOnGUIInteraction(LJobWorkData.ControlController, ctsCREATED, 0, '');
      end;
    MSG_CRAWLER_TASK_STARTED:
      begin
        LCrawlerCountToFinishedRatio := msg.MsgData[2].AsExtended;

        if Assigned(FOnGUIInteraction) then
          FOnGUIInteraction(LJobWorkData.ControlController, ctsWORKING, LCrawlerCountToFinishedRatio * 100, msg.MsgData[3].AsString);
      end;
    MSG_CRAWLER_TASK_FINISHED:
      begin
        if Assigned(FOnGUIInteraction) then
          FOnGUIInteraction(LJobWorkData.ControlController, ctsFINISHED, 100, '');
      end
    else
    begin
      inherited OmniEMTaskMessage(task, msg);
    end;
  end;
end;

constructor TCrawlerManager.Create;
begin
  inherited Create;
end;

destructor TCrawlerManager.Destroy;
begin
  inherited Destroy;
end;

procedure TCrawlerManager.AddCrawlerJob;
var
  LCrawlerThread: TCrawlerThread;
begin
  LCrawlerThread := TCrawlerThread.Create(AControlController);
  AddJob(LCrawlerThread.Data);
  CreateTask(LCrawlerThread).MonitorWith(FOmniEM).Run(@TCrawlerThread.Execute);
end;

procedure TCrawlerManager.RemoveCrawlerJob;
begin
  // TODO: Re-Implement this
  // RemoveJob();
end;

end.
