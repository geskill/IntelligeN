unit uApiCrawler;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes, Generics.Collections,
  // DevExpress
  cxProgressBar,
  // OmniThreadLibrary
  OtlCollections, OtlComm, OtlCommon, OtlEventMonitor, OtlSync, OtlTask, OtlTaskControl, OtlThreadPool,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiPlugins, uApiSettings, uApiThreadPoolManager;

type
  TCrawlerThread = class(TMyOmniWorker)
  protected
    FOmniBlockingCollection: IOmniBlockingCollection;
    FFormatSettings: TFormatSettings;
    FComponentController: IControlController;
    FContingent: Integer;
    procedure UpdateControlValues;
    procedure InitiateImageRemoteUpload;
  public
    constructor Create(const AControlController: IControlController);
    function Initialize: Boolean; override;
    procedure Cleanup; override;
  end;

  // TCrawlerTasks = record
  // TaskID: Integer;
  // ControlController: Pointer;
  // end;

  TCrawlerTaskStatus = (ctsCREATED, ctsWORKING, ctsFINISHED);
  TGUIInteractionEvent = procedure(AControlController: IControlController; Status: TCrawlerTaskStatus; AProgressPosition: Extended; msg: string) of object;

  TCrawlerManager = class(TThreadPoolManager, ICrawlerManager)
  private
    FActiveContingent: Integer;
    FStartedTasks: Integer;

    FOnGUIInteraction: TGUIInteractionEvent;
  protected
    procedure OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); override;
  public
    constructor Create; reintroduce;

    procedure AddCrawlerJob(const AControlController: IControlController);
    procedure RemoveCrawlerJob(const AControlController: IControlController);

    property OnGUIInteraction: TGUIInteractionEvent read FOnGUIInteraction write FOnGUIInteraction;
  end;

const
  MSG_CRAWLER_TASK_CREATED = 5;
  MSG_CRAWLER_TASK_STARTED = 6;
  MSG_CRAWLER_TASK_FINISHED = 7;

implementation

{ TCrawlerThread }

procedure TCrawlerThread.UpdateControlValues;
var
  _ControlIndex, _ControlValueCount: Integer;
begin
  for _ControlIndex := 0 to FComponentController.ControlCount - 1 do
    with FComponentController.Control[_ControlIndex] do
    begin
      _ControlValueCount := 0;
      // Problem, wenn Value gesetzt wurde, aber noch nicht als Message abgearbeitet wurde, dann ist Value noch leer
      while (_ControlValueCount < ProposedValuesCount) and ((Value = '') or ((ControlID = cReleaseDate) and (Value = DateToStr(Date, FFormatSettings)))) do
      begin
        Value := GetProposedValue(_ControlValueCount);
        sleep(0);
        if not(GetProposedValue(_ControlValueCount) = '') then
          break;
        Inc(_ControlValueCount);
      end;
    end;
end;

procedure TCrawlerThread.InitiateImageRemoteUpload;
var
  _Picture: IPicture;
begin
  _Picture := FComponentController.FindControl(cPicture) as IPicture;
  try
    if Assigned(_Picture) and not SameStr('', _Picture.Value) then
      _Picture.RemoteUpload(True);
  finally
    _Picture := nil;
  end;
end;

constructor TCrawlerThread.Create(const AControlController: IControlController);
var
  _CrawlerIndex, _ContingentIndex: Integer;
  _CrawlerCollectionItem: TCrawlerCollectionItem;
  _found: Boolean;
  _CrawlerContingentCollectionItem: TCrawlerContingentCollectionItem;
begin
  inherited Create;
  FOmniBlockingCollection := TOmniBlockingCollection.Create;

  GetLocaleFormatSettings(GetThreadLocale, FFormatSettings);

  FComponentController := AControlController;
  FContingent := 0;

  for _CrawlerIndex := 0 to SettingsManager.Settings.Plugins.Crawler.Count - 1 do
  begin
    _CrawlerCollectionItem := TCrawlerCollectionItem(SettingsManager.Settings.Plugins.Crawler.Items[_CrawlerIndex]);
    if _CrawlerCollectionItem.Enabled then
    begin
      _found := False;
      _ContingentIndex := 0;
      // überprüfung ob mindestens ein ComponentItem aktiviert ist
      while not _found and (_ContingentIndex < _CrawlerCollectionItem.Contingent.Count) do
      begin
        _CrawlerContingentCollectionItem := TCrawlerContingentCollectionItem(_CrawlerCollectionItem.Contingent.Items[_ContingentIndex]);
        _found := (FComponentController.TypeID = _CrawlerContingentCollectionItem.TypeID) and _CrawlerContingentCollectionItem.Status;
        Inc(_ContingentIndex);
      end;
      if _found then
      begin
        Inc(FContingent);
        FOmniBlockingCollection.Add(TOmniValue.CastFrom(_CrawlerCollectionItem));
      end;
    end;
  end;

  FOmniBlockingCollection.CompleteAdding;
end;

function TCrawlerThread.Initialize: Boolean;
var
  OmniValue: TOmniValue;

  _CrawlerCollectionItem: TCrawlerCollectionItem;
begin
  if CheckforBlacklist(FComponentController) then
    Exit(False);

  task.Comm.Send(MSG_CRAWLER_TASK_CREATED, [FComponentController, FContingent]);

  while not task.Terminated and FOmniBlockingCollection.Take(OmniValue) do
  begin
    _CrawlerCollectionItem := TCrawlerCollectionItem(OmniValue.AsObject);

    task.Comm.Send(MSG_CRAWLER_TASK_STARTED, [FComponentController, _CrawlerCollectionItem.name]);

    // TODO: Update
    //TApiPlugin.CrawlerExec(_CrawlerCollectionItem, FComponentController.TypeID, FComponentController);

    /// access of control.value is now thread-safe and should be done by this thread
    UpdateControlValues;
  end;

  task.Invoke(
    { } procedure
    { } begin
    { .. } InitiateImageRemoteUpload;
    { .. } task.SetTimer(1, 1, MSG_SLEEP);
    { } end);

  Result := True;
end;

procedure TCrawlerThread.Cleanup;
begin
  task.Comm.Send(MSG_CRAWLER_TASK_FINISHED, FComponentController);
  task.Comm.Send(MSG_TASK_QUIT, FComponentController);
  FComponentController := nil;
end;

{ TCrawlerManager }

procedure TCrawlerManager.OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
begin
  case msg.MsgID of
    MSG_CRAWLER_TASK_CREATED:
      begin
        FActiveContingent := msg.MsgData[1].AsInteger;
        FStartedTasks := 0;

        if Assigned(FOnGUIInteraction) then
          FOnGUIInteraction(IControlController(msg.MsgData[0].AsInterface), ctsCREATED, 0, '');
      end;
    MSG_CRAWLER_TASK_STARTED:
      begin
        if Assigned(FOnGUIInteraction) then
          FOnGUIInteraction(IControlController(msg.MsgData[0].AsInterface), ctsWORKING, FStartedTasks / FActiveContingent * 100, msg.MsgData[1].AsString);

        Inc(FStartedTasks);
      end;
    MSG_CRAWLER_TASK_FINISHED:
      begin
        if Assigned(FOnGUIInteraction) then
          FOnGUIInteraction(IControlController(msg.MsgData.AsInterface), ctsFINISHED, 100, '');
      end
    else
      inherited OmniTEDTaskMessage(task, msg);
  end;
end;

constructor TCrawlerManager.Create;
begin
  inherited Create('TCrawlerManager', 1);
end;

procedure TCrawlerManager.AddCrawlerJob;
begin
  FInList.Add(AControlController);
  CreateTask(TCrawlerThread.Create(AControlController), 'TCrawlerThread (' + AControlController.FindControl(cReleaseName).Value + ')').MonitorWith(FOmniTED)
    .Schedule(FThreadPool);
end;

procedure TCrawlerManager.RemoveCrawlerJob;
var
  indxA, indxB: Integer;
begin
  indxA := FInList.IndexOf(AControlController);
  if indxA <> -1 then
  begin
    indxB := FBlackList.IndexOf(AControlController);
    if indxB = -1 then
      FBlackList.Add(AControlController);
  end;
end;

end.
