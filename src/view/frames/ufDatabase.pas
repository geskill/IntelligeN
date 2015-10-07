unit ufDatabase;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, DB,
  // DevExpress
  cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit, cxDBData, cxGridLevel, cxClasses,
  cxControls, cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, cxLabel,
  cxTextEdit, cxContainer, cxMaskEdit, cxDropDownEdit, cxButtonEdit, cxLookAndFeels, cxLookAndFeelPainters, cxNavigator,
  // API
  uApiSettings;

type
  TfDatabase = class(TFrame)
    cxGridLevel: TcxGridLevel;
    cxGrid: TcxGrid;
    cxGridDBTableView: TcxGridDBTableView;
    cxCOBActiveDatabaseName: TcxComboBox;
    DataSource: TDataSource;
    procedure cxCOBActiveDatabaseNamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxCOBActiveDatabaseNamePropertiesChange(Sender: TObject);
    procedure cxCOBActiveDatabaseNamePropertiesInitPopup(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  uSettings;
{$R *.dfm}

procedure TfDatabase.cxCOBActiveDatabaseNamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if AButtonIndex = 1 then
  begin
    with Settings do
    begin
      cxPCMain.ActivePage := cxTSDatabase;
      if cxCOBActiveDatabaseName.ItemIndex <> -1 then
      begin
        cxCLBDatabase.ItemIndex := cxCLBDatabase.Items.IndexOf(cxCOBActiveDatabaseName.Text);
        cxCLBDatabase.OnClick(cxCLBDatabase);
      end;
      Show;
    end;
  end;
end;

procedure TfDatabase.cxCOBActiveDatabaseNamePropertiesChange(Sender: TObject);
// var
// sl: TStringList;
begin
  {
    SettingsManager.Settings.Database.ActiveDatabaseName := cxCOBActiveDatabaseName.Text;


    with SettingsManager.Settings.Database.ActiveDatabase do
    with ADConnection do
    begin
    DriverName := C_AD_PhysRDBMSKinds[Connectivity];
    Params.Add('Database=' + Database);

    Connected := True;

    sl := TStringList.Create;
    with sl do
    try
    GetTableNames('','','',sl);

    ShowMessage(sl.Text);
    finally
    Free;
    end;


    end;
    ADQuery.SQL.Text := 'select * from int2k9_releases';
    //ADQuery.SQL.Text := 'select * from int2k9_releases inner join int2k9_crypterlinks on int2k9_crypterlinks.relid = int2k9_releases.id';
    ADQuery.Active := True;
    }
  cxGridDBTableView.DataController.CreateAllItems(True);
end;

procedure TfDatabase.cxCOBActiveDatabaseNamePropertiesInitPopup(Sender: TObject);
begin
  with SettingsManager.Settings.Database.GetDatabaseItemList do
    try
      cxCOBActiveDatabaseName.Properties.Items.Text := Text;
    finally
      Free;
    end;
end;

constructor TfDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  cxCOBActiveDatabaseName.Text := SettingsManager.Settings.Database.ActiveDatabaseName;

  with TcxComboBoxProperties(cxCOBActiveDatabaseName.Properties) do
  begin
    OnButtonClick := cxCOBActiveDatabaseNamePropertiesButtonClick;

    with TcxEditButton(Buttons.Add) do
    begin
      LeftAlignment := True;
      Kind := bkEllipsis;
    end;
  end;

end;

destructor TfDatabase.Destroy;
begin

  inherited Destroy;
end;

end.
