unit uDirWatch;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants,
  // RegEx
  RegExpr,
  // LkJSON
  uLkJSON,
  // MultiEvent
  Generics.MultiEvents.NotifyHandler,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Plugin system
  uPlugInAppClass, uPlugInEvent, uPlugInHTTPClasses,
  // Utils,
  uPathUtils, uStringUtils, uURLUtils,
  //
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
  public
    function GetName: WideString; override;
    function Start(const AAppController: IAppController): WordBool; override;
    function Stop: WordBool; override;
  end;

implementation

uses
  uDirWatchSettingsForm;

function GetModulePath: string;
var
  QueryRes: TMemoryBasicInformation;
  LBuffer: string;
begin
  VirtualQuery(@GetModulePath, QueryRes, SizeOf(QueryRes));
  SetLength(LBuffer, MAX_PATH);
  SetLength(LBuffer, GetModuleFileName(Cardinal(QueryRes.AllocationBase), PChar(LBuffer), Length(LBuffer)));
  Result := LBuffer;
end;

{ TDirWatch }

procedure TDirWatch.OnClick(const Sender: IInterface);
begin
  FDirWatchEngine.Active := not FDirWatchEngine.Active;
  if FDirWatchEngine.Active then
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

function TDirWatch.GetName: WideString;
begin
  Result := 'DirWatch';
end;

function TDirWatch.Start(const AAppController: IAppController): WordBool;
begin
  FAppController := AAppController;

  FDirWatchSettings := TDirWatchSettings.Create(ExtractFilePath(GetModulePath) + ChangeFileExt(ExtractFileName(GetModulePath), '.ini'));
  FDirWatchEngine := TDirWatchEngine.Create(AAppController.PageController, FDirWatchSettings);

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

  FDirWatchEngine.Free;
  FDirWatchSettings.Free;

  Result := True;
end;

end.
