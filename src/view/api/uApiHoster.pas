unit uApiHoster;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Messages,
  // OmniThreadLibrary
  OtlTaskControl, OtlThreadPool,
  // Common
  uAppInterface,
  // API
  uApiPlugins, uApiSettings, uApiThreadPoolManager,
  // Plugin system
  uPlugInConst;

type
  THosterCheckThread = class(TMyOmniWorker)
  protected
    FDirectlinksMirror: IDirectlinksMirror;
    FDirectlinks: string;
    FHoster: TPlugInCollectionItem;
  public
    constructor Create(const ADirectlinksMirror: IDirectlinksMirror);
    function Initialize: Boolean; override;
    procedure Cleanup; override;
  end;

  THosterManager = class(TThreadPoolManager, IHosterManager)
  public
    constructor Create; reintroduce;

    procedure AddHosterCheckJob(const ADirectlinksMirror: IDirectlinksMirror);
    procedure RemoveHosterCheckJob(const ADirectlinksMirror: IDirectlinksMirror);
  end;

implementation

{ THosterCheckThread }

constructor THosterCheckThread.Create(const ADirectlinksMirror: IDirectlinksMirror);
begin
  inherited Create;

  FDirectlinksMirror := ADirectlinksMirror;
  FDirectlinks := FDirectlinksMirror.Value;

  with SettingsManager.Settings.Plugins do
    FHoster := TPlugInCollectionItem(FindPlugInCollectionItemFromCollection(FDirectlinksMirror.Hoster, FileHoster));
end;

function THosterCheckThread.Initialize: Boolean;
var
  LinksInfo: TLinksInfo;
begin
  if CheckforBlacklist(FDirectlinksMirror) then
    Exit(False);

  if Assigned(FHoster) then
  begin
    LinksInfo := TApiPlugin.CheckFiles(FHoster, FDirectlinks);

    if not task.Terminated then
    begin
      if CheckforBlacklist(FDirectlinksMirror) then
        Exit(False);

      FDirectlinksMirror.LinksInfo := LinksInfo;

      task.Invoke(
        { } procedure
        { } begin
        { . } try
        { ... } FDirectlinksMirror.RefreshInfo;
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
  task.Comm.Send(MSG_TASK_QUIT, FDirectlinksMirror);
  FDirectlinksMirror := nil;
end;

{ THosterManager }

constructor THosterManager.Create;
begin
  inherited Create('THosterManager', 2);
end;

procedure THosterManager.AddHosterCheckJob;
begin
  FInList.Add(ADirectlinksMirror);
  CreateTask(THosterCheckThread.Create(ADirectlinksMirror), 'THosterCheckThread ' + ADirectlinksMirror.Hoster).MonitorWith(FOmniTED).Schedule(FThreadPool);
end;

procedure THosterManager.RemoveHosterCheckJob;
var
  indxA, indxB: Integer;
begin
  indxA := FInList.IndexOf(ADirectlinksMirror);
  if indxA <> -1 then
  begin
    indxB := FBlackList.IndexOf(ADirectlinksMirror);
    if indxB = -1 then
      FBlackList.Add(ADirectlinksMirror);
  end;
end;

end.
