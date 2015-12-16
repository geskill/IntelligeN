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
  // API
  uApiLogManager;


type
  TfErrorLogger = class(TFrame)
  private
    FILogEventHandler: ILogEventHandler;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddLog(const ALog: ILog);
  end;

implementation

{$R *.dfm}

{ TfErrorLogger }

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
begin
  // TODO:
end;

end.
