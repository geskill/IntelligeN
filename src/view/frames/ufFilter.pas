unit ufFilter;

interface

uses
  // Delphip
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  // Dev Expresss
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxLabel, cxGridCardView,
  cxGridCustomTableView, cxGridTableView, cxGridCustomView, cxClasses, cxGridLevel, cxGrid, cxCheckBox, cxCheckComboBox, cxDropDownEdit, cxButtonEdit,
  // Common
  uConst,
  // Api
  uApiConst, uApiPlugins, uApiSettings, cxGridCustomLayoutView, cxNavigator;

type
  TfFilter = class(TFrame)
    cxGridLevel1: TcxGridLevel;
    cxGrid: TcxGrid;
    tvSections: TcxGridTableView;
    cxGridLevel2: TcxGridLevel;
    cvValues: TcxGridCardView;
    tvSectionsColumn1: TcxGridColumn;
    cvValuesRow1: TcxGridCardViewRow;
    cvValuesRow2: TcxGridCardViewRow;
    cvValuesRow3: TcxGridCardViewRow;
    cvValuesRow4: TcxGridCardViewRow;
    procedure cvValuesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure cvValuesRow4PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
  private
    FtvDataController: TcxCustomDataController;
    FFilterName: string;
  public
    constructor Create(AOwner: TComponent); override;
    property FilterName: string read FFilterName write FFilterName;
    procedure UpdateData;
    procedure LoadData;
    procedure SaveData;
  end;

implementation

uses
  uSelectFilterFilterDialog;
{$R *.dfm}
{ TfFilter }

procedure TfFilter.cvValuesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Site: TcxGridSite;
  HitTest: TcxCustomGridHitTest;
  Item: TcxCustomGridTableItem;
  // Rec: TcxCustomGridRecord;
begin
  Site := Sender as TcxGridSite;
  HitTest := Site.GridView.ViewInfo.GetHitTest(X, Y);

  if HitTest is TcxGridRecordCellHitTest then
  begin
    Item := TcxGridRecordCellHitTest(HitTest).Item;
    // Rec := TcxGridRecordCellHitTest(HitTest).GridRecord;

    FtvDataController := Item.GridView.DataController;
  end;
end;

procedure TfFilter.cvValuesRow4PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  with TSelectFilterFilterDialog.Create(nil) do
    try
      Filter := FtvDataController.Values[FtvDataController.GetFocusedRecordIndex, cvValuesRow4.index];

      if Execute then
        FtvDataController.Values[FtvDataController.GetFocusedRecordIndex, cvValuesRow4.index] := Filter;
    finally
      Free;
    end;
end;

constructor TfFilter.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  for I := 0 to length(TStringTemplateTypeID) - 1 do
    with TcxCustomCheckComboBoxProperties(cvValuesRow3.Properties).Items.Add do
    begin
      Enabled := True;
      Description := TStringTemplateTypeID[I];

    end;
end;

procedure TfFilter.UpdateData;
var
  I, Y: Integer;
  CustomDataController: TcxCustomDataController;
  AStates: TcxCheckStates;
begin
  with TcxCustomCheckComboBoxProperties(cvValuesRow3.Properties) do
  begin
    SetLength(AStates, Items.Count);
    for I := 0 to Items.Count - 1 do
      AStates[I] := cbsChecked;
  end;

  with tvSections do
  begin
    BeginUpdate;
    try
      with SettingsManager.Settings.Plugins.CMS do
        with DataController do
        begin
          RecordCount := Count;
          for I := 0 to Count - 1 do
            with TCMSCollectionItem(Items[I]) do
              Values[I, 0] := name;
        end;
    finally
      EndUpdate;
    end;

    for I := 0 to SettingsManager.Settings.Plugins.CMS.Count - 1 do
      with TCMSCollectionItem(SettingsManager.Settings.Plugins.CMS.Items[I]) do
      begin
        with tvSections.DataController do
          CustomDataController := GetDetailDataController(I, 0);
        with CustomDataController do
        begin
          BeginUpdate;
          try
            RecordCount := Websites.Count;
            for Y := 0 to Websites.Count - 1 do
              with TPlugInCollectionItem(Websites.Items[Y]) do
              begin
                Values[Y, cvValuesRow1.index] := name;
                Values[Y, cvValuesRow2.index] := null;
                with TcxCustomCheckComboBoxProperties(cvValuesRow3.Properties) do
                  Values[Y, cvValuesRow3.index] := CalculateCheckStatesValue(AStates, Items, EditValueFormat);
                Values[Y, cvValuesRow4.index] := '';
              end;
          finally
            EndUpdate;
          end;
        end;
      end;
  end;
end;

procedure TfFilter.LoadData;

  function FindCMS(AName: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    with tvSections.DataController do
      for I := 0 to RecordCount - 1 do
        if SameText(AName, Values[I, 0]) then
        begin
          Result := I;
          break;
        end;
  end;

  function FindCMSSite(ACustomDataController: TcxCustomDataController; AName: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    with ACustomDataController do
      for I := 0 to RecordCount - 1 do
        if SameText(AName, Values[I, cvValuesRow1.index]) then
        begin
          Result := I;
          break;
        end;
  end;

var
  I, It, Y, Yt: Integer;
  CustomDataController: TcxCustomDataController;
begin
  with SettingsManager.Settings.Publish.GetFilter(FilterName).CMS do
    for I := 0 to Count - 1 do
    begin
      It := FindCMS(TSettings_PublishFilterItems(Items[I]).name);

      if It = -1 then
        Continue;

      with tvSections.DataController do
        CustomDataController := GetDetailDataController(It, 0);

      for Y := 0 to TSettings_PublishFilterItems(Items[I]).Websites.Count - 1 do
        with TSettings_PublishFilterItem(TSettings_PublishFilterItems(Items[I]).Websites.Items[Y]) do
        begin
          Yt := FindCMSSite(CustomDataController, name);

          CustomDataController.Values[Yt, cvValuesRow2.index] := Enabled;
          CustomDataController.Values[Yt, cvValuesRow3.index] := Categories;
          CustomDataController.Values[Yt, cvValuesRow4.index] := Filter;
        end;
    end;
end;

procedure TfFilter.SaveData;
var
  I, Y: Integer;
  CustomDataController: TcxCustomDataController;
begin
  with SettingsManager.Settings.Publish.GetFilter(FilterName).CMS do
  begin
    Clear;
    for I := 0 to tvSections.DataController.RecordCount - 1 do
      with tvSections.DataController, TSettings_PublishFilterItems(Add) do
      begin
        name := Values[I, 0];

        CustomDataController := GetDetailDataController(I, 0);

        for Y := 0 to CustomDataController.RecordCount - 1 do
          with CustomDataController, TSettings_PublishFilterItem(Websites.Add) do
          begin
            name := Values[Y, cvValuesRow1.index];
            Enabled := Values[Y, cvValuesRow2.index];
            Categories := Values[Y, cvValuesRow3.index];
            Filter := Values[Y, cvValuesRow4.index];
          end;
      end;
  end;
end;

end.
