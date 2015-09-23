unit uApiCrypter;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Dialogs, Messages, SyncObjs,
  // OmniThreadLibrary
  OtlTaskControl, OtlThreadPool,
  // Common
  uAppInterface,
  // API
  uApiPlugins, uApiSettings,
  // plugin system
  uPlugInConst;

const
  MSG_SLEEP = 2;

type
  TCrypterThread = class(TOmniWorker)
  protected
    FFolderURL: string;
    FCrypterPanel: ICrypterPanel;
    FCrypter: TCrypterCollectionItem;
  public
    constructor Create(const ACrypterPanel: ICrypterPanel);
    procedure SleepTask(var msg: TMessage); message MSG_SLEEP;
    destructor Destroy; override;
  end;

  TCrypterCryptThread = class(TCrypterThread)
  public
    constructor Create(const ACrypterPanel: ICrypterPanel);
    function Initialize: Boolean; override;
  end;

  TCrypterCheckThread = class(TCrypterThread)
  private
    FCheckDelay: Integer;
    FEvent: TEvent;
  public
    constructor Create(const ACrypterPanel: ICrypterPanel; const AUseCheckDelay: Boolean = False);
    function Initialize: Boolean; override;
    destructor Destroy; override;
  end;

  TCrypterManager = class(TInterfacedObject, ICrypterManager)
  private
    FThreadPool: IOmniThreadPool;
  public
    constructor Create;
    procedure AddCrypterJob(const ACrypterPanel: ICrypterPanel);
    procedure AddCrypterCheckJob(const ACrypterPanel: ICrypterPanel; const AUseCheckDelay: WordBool = False);
    destructor Destroy; override;
  end;

implementation

{ TCrypterThread }

constructor TCrypterThread.Create(const ACrypterPanel: ICrypterPanel);
begin
  inherited Create;

  FCrypterPanel := ACrypterPanel;
  FFolderURL := FCrypterPanel.Link;

  with SettingsManager.Settings.Plugins do
    FCrypter := TCrypterCollectionItem(FindPlugInCollectionItemFromCollection(FCrypterPanel.name, Crypter));
end;

procedure TCrypterThread.SleepTask(var msg: TMessage);
begin
  Task.SetTimer(0);
  Sleep(1000);
  Task.Terminate;
end;

destructor TCrypterThread.Destroy;
begin
  FCrypterPanel := nil;
  FCrypter := nil;
  inherited Destroy;
end;

{ TCrypterCryptThread }

constructor TCrypterCryptThread.Create(const ACrypterPanel: ICrypterPanel);
var
  _SubMirrorIndex: Integer;
begin
  inherited Create(ACrypterPanel);

  if SettingsManager.Settings.ComponentParser.ModyBeforeCrypt then
    for _SubMirrorIndex := 0 to FCrypterPanel.MirrorControl.Directlink.MirrorCount - 1 do
      FCrypterPanel.MirrorControl.Directlink.Mirror[_SubMirrorIndex].Mody;
end;

function TCrypterCryptThread.Initialize: Boolean;
var
  _ErrorMsg: string;
begin
  _ErrorMsg := '';

  FFolderURL := TApiPlugin.GenerateFolder(FCrypter, FCrypterPanel.MirrorControl,
    FCrypterPanel.MirrorControl.MirrorController.TabSheetController.ComponentController, _ErrorMsg);

  Task.Invoke(
    { } procedure
    { } var
    { . } _CrypterIndex: Integer;
    { } begin
    { . } if SameText(_ErrorMsg, '') then
    { . } begin
    { ... } FCrypterPanel.Link := FFolderURL;
    { ... } FCrypterPanel.CheckFolder(True);
    { . } end
    { . } else
    { ... } MessageDlg(_ErrorMsg, mtError, [mbOK], 0);

    { . } if SettingsManager.Settings.ComponentParser.SwichAfterCrypt then
    { . } begin
    { ... } for _CrypterIndex := 0 to FCrypterPanel.MirrorControl.CrypterCount - 1 do
    { ..... } if SameText(FCrypterPanel.name, FCrypterPanel.MirrorControl.Crypter[_CrypterIndex].name) then
    { ....... } Break;
    { ... } FCrypterPanel.MirrorControl.TabIndex := _CrypterIndex + 1;
    { . } end;

    { . } FCrypterPanel := nil;

    { . } Task.SetTimer(1, MSG_SLEEP);
    { } end);

  Result := True;
end;

{ TCrypterCheckThread }

constructor TCrypterCheckThread.Create(const ACrypterPanel: ICrypterPanel; const AUseCheckDelay: Boolean = False);
begin
  inherited Create(ACrypterPanel);

  FCheckDelay := 0;
  if AUseCheckDelay then
    FCheckDelay := FCrypter.CheckDelay;

  FEvent := TEvent.Create(nil, True, False, '');
end;

function TCrypterCheckThread.Initialize: Boolean;
var
  CrypterFolderInfo: TCrypterFolderInfo;
begin
  FEvent.ResetEvent;
  if FCheckDelay > 0 then
    FEvent.WaitFor(FCheckDelay);

  CrypterFolderInfo := TApiPlugin.GetCrypterFolderInfo(FCrypter, FFolderURL);

  Task.Invoke(
    { } procedure
    { } begin
    { . } FCrypterPanel.CrypterFolderInfo := CrypterFolderInfo;
    { . } FCrypterPanel.RefreshGrid;
    { . } Task.SetTimer(1, MSG_SLEEP);
    { } end);

  Result := True;
end;

destructor TCrypterCheckThread.Destroy;
begin
  FEvent.Free;
  inherited Destroy;
end;

{ TCrypterManager }

constructor TCrypterManager.Create;
begin
  FThreadPool := CreateThreadPool('TCrypterManager');
  with FThreadPool do
  begin
    MaxExecuting := 1;
    MaxQueued := 0;
  end;
end;

procedure TCrypterManager.AddCrypterJob;
begin
  CreateTask(TCrypterCryptThread.Create(ACrypterPanel)).Unobserved.Schedule(FThreadPool);
end;

procedure TCrypterManager.AddCrypterCheckJob;
begin
  CreateTask(TCrypterCheckThread.Create(ACrypterPanel, AUseCheckDelay)).Unobserved.Schedule(FThreadPool);
end;

destructor TCrypterManager.Destroy;
begin
  if Assigned(FThreadPool) then
    FThreadPool.CancelAll;
  FThreadPool := nil;
  inherited Destroy;
end;

end.
