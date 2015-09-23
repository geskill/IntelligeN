unit ufFilterFilter;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  // Dev Express
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxDropDownEdit, cxButtonEdit,
  cxGridLevel, cxGridCustomTableView, cxGridTableView, cxClasses, cxGridCustomView, cxGrid,
  // Common
  uAppInterface, uConst,
  // Utils
  uStringUtils, cxNavigator;

type
  TfFilterFilter = class(TFrame)
    cxGridLevel1: TcxGridLevel;
    cxGrid: TcxGrid;
    cxGridTableView1: TcxGridTableView;
    cxGridTableView1Column1: TcxGridColumn;
    cxGridTableView1Column2: TcxGridColumn;
    cxGridTableView1Column3: TcxGridColumn;
    procedure cxGridTableView1Column2PropertiesInitPopup(Sender: TObject);
    procedure cxGridTableView1Column3GetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; ACellViewInfo: TcxGridTableDataCellViewInfo;
      const AMousePos: TPoint; var AHintText: TCaption; var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
    procedure cxGridTableView1Column3PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxGridTableView1Column1PropertiesChange(Sender: TObject);
  private
    function GetFilter: string;
    procedure SetFilter(AFilter: string);
  public
    constructor Create(AOwner: TComponent); override;
    property Filter: string read GetFilter write SetFilter;
  end;

implementation

uses
  uMain;
{$R *.dfm}
{ TfFilterFilter }

procedure TfFilterFilter.cxGridTableView1Column1PropertiesChange(Sender: TObject);
begin
  TcxComboBox(Sender).PostEditValue;
end;

procedure TfFilterFilter.cxGridTableView1Column2PropertiesInitPopup(Sender: TObject);
var
  I: Integer;
  ComponentID: TComponentID;
  StringList: TStringList;
begin
  with cxGridTableView1.DataController do
    ComponentID := StringToTComponentID(Values[GetFocusedRecordIndex, cxGridTableView1Column1.index]);

  with TStringList.Create do
    try
      Sorted := True;
      Duplicates := dupIgnore;

      for I := low(TStringTemplateTypeID) to high(TStringTemplateTypeID) do
      begin
        StringList := TStringList.Create;
        try
          StringList.Text := (Main as IAppController).GetControlValues(TTemplateTypeID(I), ComponentID);
          AddStrings(StringList);
        finally
          StringList.Free;
        end;
      end;

      TcxComboBoxProperties(cxGridTableView1Column2.Properties).Items.Text := Text;
      TcxComboBox(Sender).Properties.Items.Text := Text;
    finally
      Free;
    end;
end;

procedure TfFilterFilter.cxGridTableView1Column3GetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint; var AHintText: TCaption; var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
begin
  with cxGridTableView1.DataController do
    if NewItemRowFocused then
      AHintText := 'Add new detail filter'
    else
      AHintText := 'Remove selected detail filter';
end;

procedure TfFilterFilter.cxGridTableView1Column3PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  with cxGridTableView1.DataController do
    if NewItemRowFocused then
      Insert
    else
      DeleteRecord(GetFocusedRecordIndex);
end;

function TfFilterFilter.GetFilter: string;
var
  I: Integer;
begin
  with TStringList.Create do
    try
      with cxGridTableView1.DataController do
        for I := 0 to RecordCount - 1 do
          Add(VarToStr(Values[I, cxGridTableView1Column1.index]) + NameValueSeparator + VarToStr(Values[I, cxGridTableView1Column2.index]));
      Result := StringReplace(Text, sLineBreak, ';', [rfReplaceAll]);
      if (length(Result) > 0) and (copy(Result, length(Result), 1) = ';') then
        System.Delete(Result, length(Result), 1);
    finally
      Free;
    end;
end;

procedure TfFilterFilter.SetFilter(AFilter: string);
var
  I: Integer;
  StringList: TStrings;
begin
  if AFilter = '' then
    cxGridTableView1.DataController.RecordCount := 0
  else
  begin
    StringList := SplittString(';', AFilter);
    try
      with cxGridTableView1.DataController do
      begin
        BeginUpdate;
        try
          RecordCount := StringList.Count;
          for I := 0 to StringList.Count - 1 do
          begin
            Values[I, cxGridTableView1Column1.index] := StringList.Names[I];
            Values[I, cxGridTableView1Column2.index] := StringList.ValueFromIndex[I];
          end;
        finally
          EndUpdate;
        end;
      end;
    finally
      StringList.Free;
    end;
  end;
end;

constructor TfFilterFilter.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  with TcxComboBoxProperties(cxGridTableView1Column1.Properties) do
  begin
    for I := low(TStringComponentID) to high(TStringComponentID) do
      Items.Add(TStringComponentID[I]);
  end;
end;

end.
