unit ufErrorLogger;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  // Dev Express
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit,
  cxGridCustomTableView, cxGridTableView, cxGridCustomView, cxClasses, cxGridLevel, cxGrid, cxLabel, cxTextEdit, cxBlobEdit, cxNavigator,
  // Common
  uAppInterface,
  // Api
  uApiLogManager, uApiSettings;


type
  TfErrorLogger = class(TFrame)
    Log: TcxGridLevel;
    cxGrid: TcxGrid;
    tvLog: TcxGridTableView;
    tvLogColumnTime: TcxGridColumn;
    tvLogColumnMessage: TcxGridColumn;
  private
    FILogEventHandler: ILogEventHandler;
    procedure AddLogToGrid(const ALog: ILog);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddLog(const ALog: ILog);
  end;

implementation

{$R *.dfm}

{ TfErrorLogger }

procedure TfErrorLogger.AddLogToGrid(const ALog: ILog);
begin
  with tvLog.DataController do
  begin
    BeginUpdate;
    try
      RecordCount := RecordCount + 1;

      Values[RecordCount - 1, tvLogColumnTime.Index] := ALog.Time;
      Values[RecordCount - 1, tvLogColumnMessage.Index] := ALog.Message;
    finally
      EndUpdate;
    end;
  end;
end;

constructor TfErrorLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FILogEventHandler := TILogEventHandler.Create(AddLog);

  with TLogManager.Instance() do
    OnNewLog.Add(FILogEventHandler);
end;

destructor TfErrorLogger.Destroy;
begin
  with TLogManager.Instance() do
    OnNewLog.Remove(FILogEventHandler);

  FILogEventHandler := nil;

  inherited Destroy;
end;

procedure TfErrorLogger.AddLog(const ALog: ILog);
var
  LCanAdd: Boolean;
begin
  LCanAdd := not (SettingsManager.Settings.Log.MaxLogEntries = 0);

  with tvLog.DataController do
  begin
    BeginUpdate;
    try
      if not LCanAdd then
      begin
        RecordCount := 0;
      end
      else
      begin
        while ((RecordCount + 1) > SettingsManager.Settings.Log.MaxHTTPLogEntries) do
        begin
          DeleteRecord(0);
        end;
      end;
    finally
      EndUpdate;
    end;
  end;

  if LCanAdd then
    AddLogToGrid(ALog);
end;

end.
