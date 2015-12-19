unit ufPublishQueue;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Menus, StdCtrls, ExtCtrls, Dialogs,
  // Spring Framework
  Spring.Collections.Lists,
  // DevExpress
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxControls, cxContainer, cxEdit, cxProgressBar, cxButtons, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, cxLabel, cxButtonEdit, cxGridCustomTableView,
  cxGridTableView, cxGridCustomView, cxClasses, cxGridLevel, cxGrid, cxExtEditRepositoryItems, cxEditRepositoryItems,
  // MultiEvent
  Generics.MultiEvents.NotifyEvent,
  Generics.MultiEvents.NotifyInterface,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiPublishManager,
  // Utils
  uStringUtils;

type
  TfPublishQueue = class(TFrame)
    pTop: TPanel;
    cxBStart: TcxButton;
    cxBPause: TcxButton;
    cxBStop: TcxButton;
    cxPBPublishOverallProgress: TcxProgressBar;
    cxGPublishQueueLevel: TcxGridLevel;
    cxGPublishQueue: TcxGrid;
    cxGPublishQueueTableView: TcxGridTableView;
    cxGPublishQueueTableViewColumnPosition: TcxGridColumn;
    cxGPublishQueueTableViewColumnDescription: TcxGridColumn;
    cxGPublishQueueTableViewColumnProgress: TcxGridColumn;
    cxGPublishQueueTableViewColumnCanel: TcxGridColumn;
    cxGPublishQueueTableViewColumnHint: TcxGridColumn;
    cxGPublishQueueTableViewColumnErrorMsg: TcxGridColumn;
    cxEditRepo: TcxEditRepository;
    cxERProgressBarOK: TcxEditRepositoryProgressBar;
    cxERProgressBarError: TcxEditRepositoryProgressBar;
    cxERCancelButtonActive: TcxEditRepositoryButtonItem;
    cxERCancelButtonDisabled: TcxEditRepositoryButtonItem;
    procedure cxBStartClick(Sender: TObject);
    procedure cxBPauseClick(Sender: TObject);
    procedure cxBStopClick(Sender: TObject);
    procedure cxGPublishQueueTableViewColumnProgressGetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption;
      var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
    procedure cxGPublishQueueTableViewColumnProgressGetProperties(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
    procedure cxGPublishQueueTableViewColumnCanelGetProperties(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
    procedure cxERCancelButtonActivePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
  private
    FCompletedCount, FTotalCount: Integer;
    FCompletedList: TList<Integer>;

    procedure UpdateOverallProgress;
    function FindRecordByID(const AID: Longword): Integer;
    procedure AddJob(const AID: Longword; const ADescription: string);
    procedure JobFinished(const AID: Longword);
    procedure RemoveJob(const AID: Longword);
    procedure RemoveCompletedJobs(const ARemaining: Integer = 0);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GUIInteractionEvent(Status: TPublishManagerStatus);
    procedure GUIInteractionItemEvent(Status: TPublishManagerItemStatus; PublishJob: IPublishJob; AProgressPosition: Extended; msg: string);
  end;

implementation

uses
  uMain;
{$R *.dfm}
{ TfPublishQueue }

procedure TfPublishQueue.cxBStartClick(Sender: TObject);
begin
  Main.fMain.PublishManager.Resume;
end;

procedure TfPublishQueue.cxBPauseClick(Sender: TObject);
begin
  Main.fMain.PublishManager.Pause;
end;

procedure TfPublishQueue.cxBStopClick(Sender: TObject);
begin
  Main.fMain.PublishManager.RemoveAllPublishJobs;
end;

procedure TfPublishQueue.cxGPublishQueueTableViewColumnProgressGetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption;
  var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
var
  HasError: Boolean;
begin
  HasError := not SameStr('', VarToStr(ARecord.Values[cxGPublishQueueTableViewColumnErrorMsg.Index]));
  if HasError then
  begin
    AHintText := Trim(VarToStr(ARecord.Values[cxGPublishQueueTableViewColumnHint.Index]) + sLineBreak + VarToStr(ARecord.Values[cxGPublishQueueTableViewColumnErrorMsg.Index]));
  end
  else
  begin
    AHintText := VarToStr(ARecord.Values[cxGPublishQueueTableViewColumnHint.Index]);
  end;
  AIsHintMultiLine := HasError;
end;

procedure TfPublishQueue.cxGPublishQueueTableViewColumnProgressGetProperties(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
begin
  if Assigned(ARecord) and not SameStr('', VarToStr(ARecord.Values[cxGPublishQueueTableViewColumnErrorMsg.Index])) then
    AProperties := cxERProgressBarError.Properties
  else
    AProperties := cxERProgressBarOK.Properties;
end;

procedure TfPublishQueue.cxGPublishQueueTableViewColumnCanelGetProperties(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
begin
  if Assigned(ARecord) and not FCompletedList.Contains(ARecord.Values[cxGPublishQueueTableViewColumnPosition.Index]) then
    AProperties := cxERCancelButtonActive.Properties
  else
    AProperties := cxERCancelButtonDisabled.Properties;
end;

procedure TfPublishQueue.cxERCancelButtonActivePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  ID: Longword;
begin
  with cxGPublishQueueTableView.DataController do
    ID := Values[GetFocusedRecordIndex, cxGPublishQueueTableViewColumnPosition.Index];
  Main.fMain.PublishManager.RemovePublishJob(ID);
end;

procedure TfPublishQueue.UpdateOverallProgress;
begin
  cxPBPublishOverallProgress.Position := FCompletedCount / FTotalCount * 100;
end;

function TfPublishQueue.FindRecordByID(const AID: Longword): Integer;
var
  I: Integer;
begin
  Result := -1;
  with cxGPublishQueueTableView.DataController do
    for I := 0 to RecordCount - 1 do
      if (AID = Values[I, cxGPublishQueueTableViewColumnPosition.index]) then
        Exit(I);
end;

procedure TfPublishQueue.AddJob(const AID: Longword; const ADescription: string);
begin
  with cxGPublishQueueTableView.DataController do
  begin
    BeginUpdate;
    try
      RecordCount := RecordCount + 1;

      Values[RecordCount - 1, cxGPublishQueueTableViewColumnPosition.index] := AID;
      Values[RecordCount - 1, cxGPublishQueueTableViewColumnDescription.index] := ADescription;
      Values[RecordCount - 1, cxGPublishQueueTableViewColumnProgress.index] := 0;
      Values[RecordCount - 1, cxGPublishQueueTableViewColumnHint.index] := 'waiting to start';
    finally
      EndUpdate;
    end;
  end;

  if (FTotalCount = FCompletedCount) then
  begin
    FTotalCount := 0;
    FCompletedCount := 0;
  end
  else if (FTotalCount = 0) then
    FCompletedCount := 0;
  Inc(FTotalCount);
  cxBStop.Enabled := True;
  UpdateOverallProgress;

  if FCompletedList.Count > 25 then
    RemoveCompletedJobs(5);
end;

procedure TfPublishQueue.JobFinished(const AID: Longword);
begin
  FCompletedList.Add(AID);
  Inc(FCompletedCount);
  UpdateOverallProgress;
end;

procedure TfPublishQueue.RemoveJob(const AID: Longword);
var
  Index: Integer;
begin
  with cxGPublishQueueTableView.DataController do
  begin
    BeginUpdate;
    try
      Index := FindRecordByID(AID);
      if not(Index = -1) then
        DeleteRecord(Index);
    finally
      EndUpdate;
    end;
  end;

  Dec(FTotalCount);
  cxBStop.Enabled := (FTotalCount > 0);
  UpdateOverallProgress;
end;

procedure TfPublishQueue.RemoveCompletedJobs(const ARemaining: Integer = 0);
var
  LIndex, LRecordIndex: Integer;
begin
  with cxGPublishQueueTableView.DataController do
  begin
    BeginUpdate;
    try
      for LIndex := FCompletedList.Count - 1 - ARemaining downto 0 do
      begin
        LRecordIndex := FindRecordByID(FCompletedList.Items[LIndex]);
        if not(LRecordIndex = -1) then
          DeleteRecord(LRecordIndex);

        FCompletedList.Delete(LIndex);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

constructor TfPublishQueue.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCompletedCount := 0;
  FTotalCount := 0;

  FCompletedList := TList<Integer>.Create;
end;

destructor TfPublishQueue.Destroy;
begin
  FCompletedList.Free;
  inherited Destroy;
end;

procedure TfPublishQueue.GUIInteractionEvent;
begin
  case Status of
    pmsRESUMING:
      begin
        cxBStart.Enabled := False;
        cxBPause.Enabled := True;
      end;
    pmsPAUSING:
      begin
        cxBStart.Enabled := True;
        cxBPause.Enabled := False;
      end;
  end;
end;

procedure TfPublishQueue.GUIInteractionItemEvent;
var
  LIndex: Integer;
begin
  case Status of
    pmisCREATED:
      AddJob(PublishJob.UniqueID, PublishJob.Description);
    pmisWORKING, pmisERROR, pmisFINISHED:
      begin
        LIndex := FindRecordByID(PublishJob.UniqueID);
        if not(LIndex = -1) then
        begin
          with cxGPublishQueueTableView.DataController do
          begin
            BeginUpdate;
            try
              Values[LIndex, cxGPublishQueueTableViewColumnProgress.Index] := Round(AProgressPosition);
              if (pmisERROR = Status) then
                Values[LIndex, cxGPublishQueueTableViewColumnErrorMsg.Index] := VarToStr(Values[LIndex, cxGPublishQueueTableViewColumnErrorMsg.Index]) + sLineBreak + msg
              else
                Values[LIndex, cxGPublishQueueTableViewColumnHint.Index] := StringReplace(msg, ',', ', ', [rfReplaceAll]);
            finally
              EndUpdate;
            end;
          end;
        end;

        if (pmisFINISHED = Status) then
          JobFinished(PublishJob.UniqueID);
      end;
    pmisCANCELED:
      RemoveJob(PublishJob.UniqueID);
  end;
end;

end.
