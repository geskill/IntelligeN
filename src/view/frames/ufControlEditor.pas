unit ufControlEditor;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Math, ExtCtrls,
  // DevExpress
  cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit, cxLabel, cxImage,
  cxGridCustomTableView, cxGridTableView, cxControls, cxGridCustomView, cxClasses, cxGridLevel, cxGrid,
  cxLookAndFeels, cxLookAndFeelPainters, cxHint, dxScreenTip, dxCustomHint, cxNavigator,
  // Interface
  uAppInterface, uConst;

type
  TfControlEditor = class(TFrame)
    cxGridLevel: TcxGridLevel;
    cxGrid: TcxGrid;
    cxGridTableView: TcxGridTableView;
    cxGridTableViewColumn1: TcxGridColumn;
    cxGridTableViewColumn2: TcxGridColumn;
    HintTimer: TTimer;
    cxHintStyleController: TcxHintStyleController;
    procedure cxGridTableViewCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
      AShift: TShiftState; var AHandled: Boolean);
    procedure cxGridTableViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HintTimerTimer(Sender: TObject);
  private
    FControl: IBasic;
    FHintDisplayed: Boolean;
    FGridRecord: TcxCustomGridRecord;
    FItem: TcxCustomGridTableItem;
    procedure SetControl(AControl: IBasic);
  public
    constructor Create(AOwner: TComponent); override;
    property Control: IBasic read FControl write SetControl;
  end;

implementation

{$R *.dfm}

procedure TfControlEditor.cxGridTableViewCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
  AShift: TShiftState; var AHandled: Boolean);
begin
  with cxGridTableView.DataController do
    FControl.Value := FControl.GetValueContent(FocusedRowIndex);
  // if not Supports(FControl, IPicture) then
  // FControl.Value := Values[FocusedRowIndex,1]
end;

procedure TfControlEditor.cxGridTableViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  function GetGraphicHint(APictureInfo: TPictureInfo): string;
  begin
    if APictureInfo.Downloaded then
    begin
      Result := 'Unknown TGraphic object';
      if not(VarToStrDef(APictureInfo.Picture, '') = '') then
        Result := IntToStr(APictureInfo.Width) + ' x ' + IntToStr(APictureInfo.Height) + ' pixel - ' + FloatToStr(RoundTo(APictureInfo.Size / 1024, -1)) + ' KB'
    end
    else
      Result := 'Download in progress';
  end;

var
  AHitTest: TcxCustomGridHitTest;
  AHint: string;
begin
  // determine the current mouse position
  AHitTest := cxGridTableView.ViewInfo.GetHitTest(X, Y);
  // hide displayed hint if mouse is not over a grid cell
  if AHitTest.HitTestCode <> htCell then
  begin
    HintTimer.Enabled := False;
    cxHintStyleController.HideHint;
    Exit;
  end;

  with TcxGridRecordCellHitTest(AHitTest) do
    /// check the current record and column over which the mouse is placed
    if (FGridRecord <> GridRecord) or (FItem <> Item) or not FHintDisplayed then
    begin
      // redisplay hint window is mouse has been moved to a new cell
      cxHintStyleController.HideHint;
      HintTimer.Enabled := False;
      // store the current record and column
      FItem := Item;
      FGridRecord := GridRecord;
      // obtain the current cell display text
      if not Supports(FControl, IPicture) or not(Item.index = cxGridTableViewColumn2.index) then
        AHint := cxGridTableView.DataController.DisplayTexts[GridRecord.RecordIndex, Item.index]
      else
        AHint := GetGraphicHint((FControl as IPictureEx).GetValuePicture(GridRecord.RecordIndex))
        { + sLineBreak + ((FControl as IPicture).GetValuePicture(GridRecord.RecordIndex). '' } ;
      with cxGridTableView.Site.ClientToScreen(Point(X, Y)) do
      begin
        FHintDisplayed := True;
        // show hint
        cxHintStyleController.ShowHint(X, Y, '', AHint, 500);
      end;
      // start the hide hint timer
      HintTimer.Enabled := True;
    end;

end;

procedure TfControlEditor.HintTimerTimer(Sender: TObject);
begin
  HintTimer.Enabled := False;
  cxHintStyleController.HideHint;
  Application.ProcessMessages;
  FHintDisplayed := False;
end;

procedure TfControlEditor.SetControl(AControl: IBasic);
var
  RecordIndex: Integer;
begin
  FControl := AControl;

  with cxGridTableView do
    if FControl = nil then
    begin
      try
        BeginUpdate;
        for RecordIndex := 0 to DataController.RecordCount - 1 do
          DataController.Values[RecordIndex, 1] := '';
        DataController.RecordCount := 0;
      finally
        EndUpdate;
      end;
    end
    else
    begin
      try
        BeginUpdate;

        if not Supports(FControl, IPicture) then
          Items[1].PropertiesClass := nil
        else
          Items[1].PropertiesClass := TcxImageProperties;

        with OptionsView do
          if Supports(FControl, IPicture) then
            DataRowHeight := 200
          else if Supports(FControl, IEdit) then
            DataRowHeight := 18
          else if Supports(FControl, IComboBox) then
            DataRowHeight := 18
          else if Supports(FControl, IRichEdit) then
            DataRowHeight := 50;

        with DataController do
        begin
          RecordCount := FControl.GetValueCount;

          for RecordIndex := 0 to RecordCount - 1 do
          begin
            Values[RecordIndex, 0] := FControl.GetValueName(RecordIndex);

            if Supports(FControl, IPicture) then
              Values[RecordIndex, 1] := (FControl as IPictureEx).GetValuePicture(RecordIndex).Picture
            else if Supports(FControl, ITrailer) then
              Values[RecordIndex, 1] := (FControl as ITrailer).GetValueTitle(RecordIndex)
            else
              Values[RecordIndex, 1] := FControl.GetValueContent(RecordIndex);
          end;
        end;
      finally
        EndUpdate;
      end;

      with DataController do
        for RecordIndex := 0 to RecordCount - 1 do
          if SameText(FControl.GetValueContent(RecordIndex), FControl.Value) then
          begin
            FocusedRecordIndex := RecordIndex;
            Break;
          end;
    end;
end;

constructor TfControlEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HintTimer.Interval := cxHintStyleController.HintHidePause;
end;

end.
