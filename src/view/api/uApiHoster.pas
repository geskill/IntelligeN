unit uApiHoster;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Messages,
  // OmniThreadLibrary
  OtlTaskControl, OtlThreadPool,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // API
  uApiPlugins, uApiSettings, uApiThreadPoolManager,
  // Plugin system
  uPlugInConst;

type
  // TODO: Rework
  THosterCheckThread = class(TMyOmniWorker)
  protected
    FDirectlink: IDirectlink;
    FDirectlinks: string;
    FHoster: TPlugInCollectionItem;
  public
    constructor Create(const ADirectlink: IDirectlink);
    function Initialize: Boolean; override;
    procedure Cleanup; override;
  end;

  THosterManager = class(TThreadPoolManager, IHosterManager)
  public
    constructor Create; reintroduce;

    procedure AddHosterCheckJob(const ADirectlink: IDirectlink);
    procedure RemoveHosterCheckJob(const ADirectlink: IDirectlink);
  end;

implementation

{ THosterCheckThread }

constructor THosterCheckThread.Create(const ADirectlink: IDirectlink);
begin
  inherited Create;

  FDirectlink := ADirectlink;
  FDirectlinks := FDirectlink.Value;

  with SettingsManager.Settings.Plugins do
    FHoster := TPlugInCollectionItem(FindPlugInCollectionItemFromCollection(FDirectlink.Hoster, FileHoster));
end;

function THosterCheckThread.Initialize: Boolean;
var
  LinksInfo: TLinksInfo;
begin
  if CheckforBlacklist(FDirectlink) then
    Exit(False);

  if Assigned(FHoster) then
  begin
    LinksInfo := TApiPlugin.CheckFiles(FHoster, FDirectlinks);

    if not task.Terminated then
    begin
      if CheckforBlacklist(FDirectlink) then
        Exit(False);

      // FDirectlink.LinksInfo := LinksInfo;

      task.Invoke(
        { } procedure
        { } begin
        { . } try
        { ... } // FDirectlink.UpdateGUI;
        { . } except
        { . } end;

        { . } task.SetTimer(1, 1, MSG_SLEEP);
        { } end);
    end;
  end;

  Result := True;
end;

procedure THosterCheckThread.Cleanup;
begin
  task.Comm.Send(MSG_TASK_QUIT, FDirectlink);
  FDirectlink := nil;
end;

{ THosterManager }

constructor THosterManager.Create;
begin
  inherited Create('THosterManager', 2);
end;

procedure THosterManager.AddHosterCheckJob;
begin
  FInList.Add(ADirectlink);
  CreateTask(THosterCheckThread.Create(ADirectlink), 'THosterCheckThread ' + ADirectlink.Hoster).MonitorWith(FOmniTED).Schedule(FThreadPool);
end;

procedure THosterManager.RemoveHosterCheckJob;
var
  indxA, indxB: Integer;
begin
  indxA := FInList.IndexOf(ADirectlink);
  if indxA <> -1 then
  begin
    indxB := FBlackList.IndexOf(ADirectlink);
    if indxB = -1 then
      FBlackList.Add(ADirectlink);
  end;
end;

end.
