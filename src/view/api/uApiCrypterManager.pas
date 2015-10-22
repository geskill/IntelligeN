unit uApiCrypterManager;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Dialogs, Messages, SyncObjs,
  // OmniThreadLibrary
  OtlCommon, OtlTaskControl,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiThreadManager, uApiPlugins, uApiSettings,
  // Plugin system
  uPlugInConst;

type
  TCrypterData = class(TThreadWorkData)
  protected
    FCrypterPanel: ICrypterPanel;
    FCrypterCollectionItem: TCrypterCollectionItem;
  public
    constructor Create; override;
    destructor Destroy; override;

    property CrypterPanel: ICrypterPanel read FCrypterPanel write FCrypterPanel;
    property CrypterCollectionItem: TCrypterCollectionItem read FCrypterCollectionItem write FCrypterCollectionItem;
  end;

  TCrypterThread = class(TThreadWorker<TCrypterData>)
  protected
    FFolderInfo: TCrypterFolderInfo;
  public
    constructor Create(const ACrypterPanel: ICrypterPanel); reintroduce;
    destructor Destroy; override;
  end;

  TCrypterCryptThread = class(TCrypterThread)
  protected
    FMirrorContainer: IDirectlinkContainer;
    FControlController: IControlControllerBase;
  public
    constructor Create(const ACrypterPanel: ICrypterPanel);
    procedure Execute; override;
  end;

  TCrypterCheckThread = class(TCrypterThread)
  private
    FFolderURL: string;
    FCheckDelay: Integer;
    FEvent: TEvent;
  public
    constructor Create(const ACrypterPanel: ICrypterPanel; const AUseCheckDelay: Boolean = False);
    destructor Destroy; override;

    procedure Execute; override;
  end;

  TCrypterManager = class(TThreadManager<TCrypterData>, ICrypterManager)
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddCrypterJob(const ACrypterPanel: ICrypterPanel);
    procedure AddCrypterCheckJob(const ACrypterPanel: ICrypterPanel; const AUseCheckDelay: WordBool = False);
  end;

implementation

{ TCrypterData }

constructor TCrypterData.Create;
begin
  inherited Create;
end;

destructor TCrypterData.Destroy;
begin
  FCrypterPanel := nil;
  FCrypterCollectionItem := nil;
  inherited Destroy;
end;

{ TCrypterThread }

constructor TCrypterThread.Create(const ACrypterPanel: ICrypterPanel);
begin
  inherited Create;

  Data.TabSheetController := ACrypterPanel.MirrorControl.MirrorController.TabSheetController;

  Data.CrypterPanel := ACrypterPanel;

  with SettingsManager.Settings.Plugins do
    Data.CrypterCollectionItem := TCrypterCollectionItem(FindPlugInCollectionItemFromCollection(ACrypterPanel.Name, Crypter));
end;

destructor TCrypterThread.Destroy;
begin
  inherited Destroy;
end;

{ TCrypterCryptThread }

constructor TCrypterCryptThread.Create(const ACrypterPanel: ICrypterPanel);
var
  LMirrorIndex: Integer;
begin
  inherited Create(ACrypterPanel);

  if SettingsManager.Settings.ControlAligner.ModyBeforeCrypt then
    for LMirrorIndex := 0 to ACrypterPanel.MirrorControl.DirectlinkCount - 1 do
      ACrypterPanel.MirrorControl.Directlink[LMirrorIndex].Mody;

  FMirrorContainer := ACrypterPanel.MirrorControl.CloneInstance();
  FControlController := ACrypterPanel.MirrorControl.MirrorController.TabSheetController.ControlController.CloneInstance();
end;

procedure TCrypterCryptThread.Execute;
begin
  with TApiThreadedPlugin.Create(task, DefaultErrorHandler) do
    try
      if CrypterAddFolder(Data.CrypterCollectionItem, FMirrorContainer, FControlController, FFolderInfo) then
      begin
        Data.CrypterPanel.CrypterFolderInfo := FFolderInfo;

        task.Invoke(
          { } procedure
          { } var
          { . } LCrypterIndex: Integer;
          { } begin
          { . } Data.CrypterPanel.Value := FFolderInfo.Link;

          { . } Data.CrypterPanel.CheckFolder(True);

          { . } if SettingsManager.Settings.ControlAligner.SwichAfterCrypt then
          { . } begin
          { ... } for LCrypterIndex := 0 to Data.CrypterPanel.MirrorControl.CrypterCount - 1 do
          { ..... } if SameText(Data.CrypterPanel.Name, Data.CrypterPanel.MirrorControl.Crypter[LCrypterIndex].Name) then
          { ..... } begin
          { ....... } Data.CrypterPanel.MirrorControl.TabIndex := LCrypterIndex + 1;
          { ....... } Break;
          { ..... } end;
          { . } end;

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

{ TCrypterCheckThread }

constructor TCrypterCheckThread.Create(const ACrypterPanel: ICrypterPanel; const AUseCheckDelay: Boolean = False);
begin
  inherited Create(ACrypterPanel);

  FFolderURL := Data.FCrypterPanel.Value;

  FCheckDelay := 0;
  if AUseCheckDelay then
    FCheckDelay := Data.FCrypterCollectionItem.CheckDelay;

  FEvent := TEvent.Create(nil, True, False, '');
end;

destructor TCrypterCheckThread.Destroy;
begin
  FEvent.Free;
  inherited Destroy;
end;

procedure TCrypterCheckThread.Execute;
begin
  FEvent.ResetEvent;
  if FCheckDelay > 0 then
    FEvent.WaitFor(FCheckDelay);

  with TApiThreadedPlugin.Create(task, DefaultErrorHandler) do
    try
      if CrypterGetFolder(Data.CrypterCollectionItem, FFolderURL, FFolderInfo) then
      begin
        Data.CrypterPanel.CrypterFolderInfo := FFolderInfo;

        task.Invoke(
          { } procedure
          { } begin
          { . } Data.CrypterPanel.UpdateGUI;

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

{ TCrypterManager }

constructor TCrypterManager.Create;
begin
  inherited Create;
end;

destructor TCrypterManager.Destroy;
begin
  inherited Destroy;
end;

procedure TCrypterManager.AddCrypterJob;
var
  LCrypterThread: TCrypterThread;
begin
  LCrypterThread := TCrypterCryptThread.Create(ACrypterPanel);
  AddJob(LCrypterThread.Data);
  CreateTask(LCrypterThread).MonitorWith(FOmniEM).Run(@TCrypterCryptThread.Execute);
end;

procedure TCrypterManager.AddCrypterCheckJob;
var
  LCrypterThread: TCrypterThread;
begin
  LCrypterThread := TCrypterCheckThread.Create(ACrypterPanel, AUseCheckDelay);
  AddJob(LCrypterThread.Data);
  CreateTask(LCrypterThread).MonitorWith(FOmniEM).Run(@TCrypterCheckThread.Execute);
end;

end.
