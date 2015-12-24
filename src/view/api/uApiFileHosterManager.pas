unit uApiFileHosterManager;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Messages,
  // OmniThreadLibrary
  OtlCommon, OtlTaskControl,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiThreadManager, uApiPlugins, uApiSettings,
  // Plugin system
  uPlugInConst;

type
  TFileHosterData = class(TThreadWorkData)
  protected
    FDirectlink: IDirectlinksMirror;
    FFileHosterCollectionItem: TPlugInCollectionItem;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Directlink: IDirectlinksMirror read FDirectlink write FDirectlink;
    property FileHosterCollectionItem: TPlugInCollectionItem read FFileHosterCollectionItem write FFileHosterCollectionItem;
  end;

  TFileHosterCheckThread = class(TThreadWorker<TFileHosterData>)
  protected
    FLinks: string;
    FLinksInfo: TLinksInfo;
    procedure DefaultErrorHandler(const AErrorMsg: string); override;
  public
    constructor Create(const ADirectlink: IDirectlinksMirror); reintroduce;
    destructor Destroy; override;

    procedure Execute; override;
  end;

  TFileHosterManager = class(TThreadManager<TFileHosterData>, IFileHosterManager)
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddHosterCheckJob(const ADirectlink: IDirectlinksMirror);
    procedure RemoveHosterJob(const ADirectlink: IDirectlinksMirror);
  end;

implementation

{ TFileHosterData }

constructor TFileHosterData.Create;
begin
  inherited Create;
end;

destructor TFileHosterData.Destroy;
begin
  FDirectlink := nil;
  FFileHosterCollectionItem := nil;
  inherited Destroy;
end;

{ THosterCheckThread }

procedure TFileHosterCheckThread.DefaultErrorHandler(const AErrorMsg: string);
begin
  task.Invoke(
    { } procedure
    { } begin
    { . } Data.Directlink.ErrorMsg := AErrorMsg;
    { } end);
end;

constructor TFileHosterCheckThread.Create(const ADirectlink: IDirectlinksMirror);
begin
  inherited Create;

  Data.TabSheetController := ADirectlink.DirectlinksPanel.MirrorControl.MirrorController.TabSheetController;

  Data.Directlink := ADirectlink;

  Data.Directlink.ResetErrorMsg();

  with SettingsManager.Settings.Plugins do
    Data.FileHosterCollectionItem := TPlugInCollectionItem(FindPlugInCollectionItemFromCollection(ADirectlink.Hoster, FileHoster));

  FLinks := ADirectlink.Value;
end;

destructor TFileHosterCheckThread.Destroy;
begin
  inherited Destroy;
end;

procedure TFileHosterCheckThread.Execute;
begin
  with TApiThreadedPlugin.Create(task, DefaultErrorHandler) do
    try
      if FileHosterCheckFiles(Data.FileHosterCollectionItem, FLinks, FLinksInfo) then
      begin
        Data.Directlink.LinksInfo := FLinksInfo;

        task.Invoke(
          { } procedure
          { } begin
          { . } Data.Directlink.UpdateGUI;

          { . } Finish;
          { } end);
      end
      else
      begin
        Finish;
      end;
    finally
      Free;
    end;
end;

{ THosterManager }

constructor TFileHosterManager.Create;
begin
  inherited Create;
end;

destructor TFileHosterManager.Destroy;
begin
  inherited Destroy;
end;

procedure TFileHosterManager.AddHosterCheckJob;
var
  LFileHosterCheckThread: TFileHosterCheckThread;
begin
  LFileHosterCheckThread := TFileHosterCheckThread.Create(ADirectlink);
  AddJob(LFileHosterCheckThread.Data);
  CreateTask(LFileHosterCheckThread).MonitorWith(FOmniEM).Run(@TFileHosterCheckThread.Execute);
end;

procedure TFileHosterManager.RemoveHosterJob;
begin
  RemoveIterator(
    { } procedure(const AJobWorkData: TFileHosterData; var ARemove: Boolean)
    { } begin
    { . } ARemove := (ADirectlink = AJobWorkData.Directlink);
    { } end);
end;

end.

