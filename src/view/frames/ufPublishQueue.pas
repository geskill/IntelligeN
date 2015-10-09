unit ufPublishQueue;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Menus, StdCtrls, ExtCtrls, Dialogs,
  // DevExpress
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxControls, cxContainer, cxEdit, cxProgressBar, cxButtons, cxStyles, cxCustomData, cxFilter, cxData,
  cxDataStorage, cxNavigator, cxLabel, cxButtonEdit, cxGridCustomTableView, cxGridTableView, cxGridCustomView, cxClasses, cxGridLevel, cxGrid,
  // MultiEvent
  Generics.MultiEvents.NotifyEvent,
  Generics.MultiEvents.NotifyInterface,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiPublish,
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
    procedure cxBStartClick(Sender: TObject);
    procedure cxBPauseClick(Sender: TObject);
    procedure cxBStopClick(Sender: TObject);
    procedure cxGPublishQueueTableViewColumnProgressGetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption; var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
    procedure cxGPublishQueueTableViewColumnProgressGetProperties(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      var AProperties: TcxCustomEditProperties);
    procedure cxGPublishQueueTableViewColumnCanelPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
  private
    FCompletedCount, FTotalCount: Integer;

    procedure UpdateOverallProgress;
    function FindRecordByID(AID: Longword): Integer;
    procedure AddJob(AID: Longword; ADescription: string);
    procedure JobFinished(AID: Longword);
    procedure RemoveJob(AID: Longword);
  public
    constructor Create(AOwner: TComponent); override;

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
  Main.fMain.PublishManager.RemoveAllJobs;
end;

procedure TfPublishQueue.cxGPublishQueueTableViewColumnProgressGetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption; var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
var
  HasError: Boolean;
begin
  HasError := SameStr('', VarToStr(ARecord.Values[cxGPublishQueueTableViewColumnErrorMsg.Index]));
  if not HasError then
  begin
    AHintText := Trim(VarToStr(ARecord.Values[cxGPublishQueueTableViewColumnHint.Index]) + sLineBreak + VarToStr
        (ARecord.Values[cxGPublishQueueTableViewColumnErrorMsg.Index]));
  end
  else
    AHintText := VarToStr(ARecord.Values[cxGPublishQueueTableViewColumnHint.Index]);
  AIsHintMultiLine := HasError;
end;

procedure TfPublishQueue.cxGPublishQueueTableViewColumnProgressGetProperties(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  var AProperties: TcxCustomEditProperties);
begin
  with AProperties as TcxCustomProgressBarProperties do
  begin
    if not SameStr('', VarToStr(ARecord.Values[cxGPublishQueueTableViewColumnErrorMsg.Index])) then
      BeginColor := clRed;
  end;
end;

procedure TfPublishQueue.cxGPublishQueueTableViewColumnCanelPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  ID: Longword;
begin
  with cxGPublishQueueTableView.DataController do
    ID := Values[GetFocusedRecordIndex, cxGPublishQueueTableViewColumnPosition.index];
  Main.fMain.PublishManager.RemovePublishJob(ID);
end;

procedure TfPublishQueue.UpdateOverallProgress;
begin
  cxPBPublishOverallProgress.Position := FCompletedCount / FTotalCount * 100;
end;

function TfPublishQueue.FindRecordByID(AID: Longword): Integer;
var
  I: Integer;
begin
  Result := -1;
  with cxGPublishQueueTableView.DataController do
    for I := 0 to RecordCount - 1 do
      if (AID = Values[I, cxGPublishQueueTableViewColumnPosition.index]) then
        Exit(I);
end;

procedure TfPublishQueue.AddJob(AID: Longword; ADescription: string);
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
end;

procedure TfPublishQueue.JobFinished(AID: Longword);
begin
  Inc(FCompletedCount);
  UpdateOverallProgress;
end;

procedure TfPublishQueue.RemoveJob(AID: Longword);
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

constructor TfPublishQueue.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCompletedCount := 0;
  FTotalCount := 0;
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
  I, Index: Integer;
begin
  case Status of
    pmisCREATED:
      AddJob(PublishJob.UniqueID, PublishJob.Description);
    pmisWORKING, pmisERROR, pmisFINISHED:
      begin
        Index := FindRecordByID(PublishJob.UniqueID);
        if not(Index = -1) then
        begin
          with cxGPublishQueueTableView.DataController do
          begin
            BeginUpdate;
            try
              Values[Index, cxGPublishQueueTableViewColumnProgress.index] := round(AProgressPosition);
              if (pmisERROR = Status) then
                Values[Index, cxGPublishQueueTableViewColumnErrorMsg.index] := VarToStr(Values[Index, cxGPublishQueueTableViewColumnErrorMsg.index])
                  + sLineBreak + msg
              else
                Values[Index, cxGPublishQueueTableViewColumnHint.index] := StringReplace(msg, ',', ', ', [rfReplaceAll]);
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
