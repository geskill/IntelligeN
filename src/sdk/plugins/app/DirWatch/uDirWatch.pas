unit uDirWatch;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants,
  // RegEx
  RegExpr,
  // MultiEvent
  Generics.MultiEvents.NotifyHandler,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Plugin system
  uPlugInAppClass, uPlugInEvent, uPlugInHTTPClasses,
  // Utils
  uPathUtils, uStringUtils, uSystemUtils, uURLUtils,
  // DirWatch
  uDirWatchEngine, uDirWatchSettings;

type
  TDirWatch = class(TAppPlugIn)
  private
    FAppController: IAppController;
    FDirWatchSettings: TDirWatchSettings;

    FDirWatchEngine: TDirWatchEngine;

    FNewMenuItem, FSettingsMenuItem: IMenuItem;
    FNotifyEventHandler, FSettingsNotifyEventHandler: TINotifyEventHandler;
    procedure OnClick(const Sender: IUnknown);
    procedure OnSettings(const Sender: IUnknown);
  protected
    function GetAppController: IAppController;
    function GetSettings: TDirWatchSettings;

    function GetEngine: TDirWatchEngine;
    procedure SetEngine(const AEngine: TDirWatchEngine);

    procedure InitializeEngine(); virtual;
    procedure FinalizeEngine(); virtual;

    property Engine: TDirWatchEngine read GetEngine write SetEngine;
  public
    function GetName: WideString; override;
    function Start(const AAppController: IAppController): WordBool; override;
    function Stop: WordBool; override;
  end;

implementation

uses
  // DirWatch
  uDirWatchSettingsForm;

{ TDirWatch }

procedure TDirWatch.OnClick(const Sender: IInterface);
begin
  Engine.Active := not Engine.Active;
  if Engine.Active then
    FNewMenuItem.SetCaption('Disable DirWatch')
  else
    FNewMenuItem.SetCaption('Enable DirWatch');
end;

procedure TDirWatch.OnSettings(const Sender: IInterface);
begin
  if not Assigned(fDirWatchSettingsForm) then
    fDirWatchSettingsForm := TfDirWatchSettingsForm.Create(nil);

  fDirWatchSettingsForm.LoadSettings(FDirWatchSettings);
  fDirWatchSettingsForm.Show;
end;

function TDirWatch.GetAppController: IAppController;
begin
  Result := FAppController;
end;

function TDirWatch.GetSettings: TDirWatchSettings;
begin
  Result := FDirWatchSettings;
end;

function TDirWatch.GetEngine: TDirWatchEngine;
begin
  Result := FDirWatchEngine;
end;

procedure TDirWatch.SetEngine(const AEngine: TDirWatchEngine);
begin
  FDirWatchEngine := AEngine;
end;

procedure TDirWatch.InitializeEngine();
begin
  Engine := TDirWatchEngine.Create(GetAppController.PageController, GetSettings);
end;

procedure TDirWatch.FinalizeEngine();
begin
  Engine.Free;
end;

function TDirWatch.GetName: WideString;
begin
  Result := 'DirWatch';
end;

function TDirWatch.Start(const AAppController: IAppController): WordBool;
begin
  FAppController := AAppController;

  FDirWatchSettings := TDirWatchSettings.Create(ExtractFilePath(GetModulePath) + ChangeFileExt(ExtractFileName(GetModulePath), '.json'));
  InitializeEngine();

  FNotifyEventHandler := TINotifyEventHandler.Create(OnClick);
  with FAppController.MainMenu.GetMenuItems.GetItem(3) do
    FNewMenuItem := InsertMenuItem(GetMenuItems.GetCount, 'Enable DirWatch', 'Enable/Disable DirWatch', 0, -1, 0, FNotifyEventHandler);

  FSettingsNotifyEventHandler := TINotifyEventHandler.Create(OnSettings);
  with FAppController.MainMenu.GetMenuItems.GetItem(3) do
    FSettingsMenuItem := InsertMenuItem(GetMenuItems.GetCount, 'DirWatch settings', 'Configurate the DirWatch plugin', 0, -1, 0, FSettingsNotifyEventHandler);

  Result := True;
end;

function TDirWatch.Stop: WordBool;
begin
  if Assigned(fDirWatchSettingsForm) then
    fDirWatchSettingsForm.Free;

  FAppController.MainMenu.GetMenuItems.GetItem(3).GetMenuItems.RemoveItem(FSettingsMenuItem);
  FSettingsNotifyEventHandler := nil;

  FAppController.MainMenu.GetMenuItems.GetItem(3).GetMenuItems.RemoveItem(FNewMenuItem);
  FNotifyEventHandler := nil;

  FinalizeEngine();
  FDirWatchSettings.Free;
  FAppController := nil;

  Result := True;
end;

end.
