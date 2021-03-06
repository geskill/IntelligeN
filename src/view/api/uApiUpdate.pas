unit uApiUpdate;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Forms, Graphics, Variants, XMLDoc, XMLIntf, ActiveX,
  // Indy
  IdComponent,
  // OmniThreadLibrary
  OtlCommon, OtlParallel, OtlTaskControl, OtlSync, OtlTask,
  // Spring Framework
  Spring.SystemUtils, Spring.Collections.Lists, Spring.Utils,
  // AB
  AbArcTyp, AbUnZper,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPManager, uHTTPIndyHelper,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiConst, uApiUpdateInterfaceBase, uApiUpdateModelBase,
  // DLLs
  uExport,
  // Utils
  uFileUtils, uPathUtils, uStringUtils, uVariantUtils;

type
  TUpdateVersion = class
  private
    FDownloadPath: string;
    FUpdateVersion: IFileVersion;
    FUpdateFiles: TInterfaceList<IUpdateSystemFile>;
    FUpdateSize: Int64;
    FUpdateCreated: TDateTime;
    FUpdateModified: TDateTime;
  public
    constructor Create(const AFileVersion: IFileVersion = nil);
    constructor Clone(AUpdateVersion: TUpdateVersion);

    property DownloadPath: string read FDownloadPath write FDownloadPath;
    property UpdateVersion: IFileVersion read FUpdateVersion write FUpdateVersion;
    property UpdateFiles: TInterfaceList<IUpdateSystemFile>read FUpdateFiles write FUpdateFiles;
    property UpdateSize: Int64 read FUpdateSize write FUpdateSize;
    property UpdateCreated: TDateTime read FUpdateCreated write FUpdateCreated;
    property UpdateModified: TDateTime read FUpdateModified write FUpdateModified;

    destructor Destroy; override;
  end;

  TUpdateVersions = class
  private
    FUpgrade, FUpdate: TUpdateVersion;
  public
    constructor Create;

    function HasUpdate: Boolean;

    property Upgrade: TUpdateVersion read FUpgrade write FUpgrade;
    property Update: TUpdateVersion read FUpdate write FUpdate;

    destructor Destroy; override;
  end;

  TUpdateErrorNotifyEvent = procedure(Sender: TObject; ErrorMsg: string) of object;
  TUpdateDownloadPositionNotifyEvent = procedure(Sender: TObject; Position: Integer) of object;
  TUpdateHasChangesNotifyEvent = procedure(Sender: TObject; UpdateVersions: TUpdateVersions) of object;

  TUpdateController = class
  private
    FProxy: IProxy;
    FConnectTimeout, FReadTimeout: Integer;

    FUpdateVersions: TUpdateVersions;

    FSQLFormatSettings: TFormatSettings;

    FBusy: Boolean;
    FTaskControl: IOmniTaskControl;

    FUpdateTotalBytes: Int64;
    FDownloadedBytes: TOmniAlignedInt64;
    FErrorMsg: string;

    FOnSearchingUpdate, FOnUpdateNoChanges, FOnUpdateStartDownload, FOnUpdateFinishedDownload: TNotifyEvent;
    FOnUpdateHasChanges: TUpdateHasChangesNotifyEvent;
    FOnUpdateError: TUpdateErrorNotifyEvent;
    FOnUpdateDownloading: TUpdateDownloadPositionNotifyEvent;
  protected
    function GetBusy: Boolean;
    function ReadUpdate(): Boolean;
    procedure ReadFiles(const ANode: IXMLNode; AUpdateVersion: TUpdateVersion);
    function LocalPathVariableFromFileSystem(AFileSystem: TFileSystem): string;
    procedure DownloadFiles(const task: IOmniTask; AUpdateVersion: TUpdateVersion);

    procedure DoUpdateSearching;
    procedure DoUpdateNoChanges;
    procedure DoUpdateHasChanges;
    procedure DoUpdateError;
    procedure DoUpdateStartDownload;
    procedure DoUpdateDownloading;
    procedure DoUpdateFinishedDownload;
  public
    constructor Create(const AProxy: IProxy = nil; AConnectTimeout: Integer = 5000; AReadTimeout: Integer = 10000);
    destructor Destroy; override;

    property Busy: Boolean read GetBusy;

    procedure CheckForUpdates(const AForm: TForm = nil);
    procedure Download(AUpdateVersion: TUpdateVersion);
    procedure DownloadUpgrade;
    procedure DownloadUpdate;
  published
    property OnUpdateSearching: TNotifyEvent read FOnSearchingUpdate write FOnSearchingUpdate;
    property OnUpdateNoChanges: TNotifyEvent read FOnUpdateNoChanges write FOnUpdateNoChanges;
    property OnUpdateHasChanges: TUpdateHasChangesNotifyEvent read FOnUpdateHasChanges write FOnUpdateHasChanges;
    property OnUpdateError: TUpdateErrorNotifyEvent read FOnUpdateError write FOnUpdateError;
    property OnUpdateStartDownload: TNotifyEvent read FOnUpdateStartDownload write FOnUpdateStartDownload;
    property OnUpdateDownloading: TUpdateDownloadPositionNotifyEvent read FOnUpdateDownloading write FOnUpdateDownloading;
    property OnUpdateFinishedDownload: TNotifyEvent read FOnUpdateFinishedDownload write FOnUpdateFinishedDownload;
  end;

implementation

{ TUpdateVersion }

constructor TUpdateVersion.Create(const AFileVersion: IFileVersion = nil);
begin
  inherited Create;

  if not Assigned(AFileVersion) then
    FUpdateVersion := TIFileVersion.Create
  else
    FUpdateVersion := AFileVersion;

  FUpdateFiles := TInterfaceList<IUpdateSystemFile>.Create();
end;

constructor TUpdateVersion.Clone(AUpdateVersion: TUpdateVersion);
var
  LFileVersion: IFileVersion;
  LUpdateSystemFile: IUpdateSystemFile;
  LUpdateFileIndex: Integer;
begin
  LFileVersion := TIFileVersion.Clone(AUpdateVersion.UpdateVersion);

  Create(LFileVersion);

  FDownloadPath := AUpdateVersion.DownloadPath;

  // Do NOT use "For in" loop
  // see: http://stackoverflow.com/questions/28010268/why-does-delphi-unclear-increases-the-refcount-of-the-last-element-in-the-list-o
  for LUpdateFileIndex := 0 to AUpdateVersion.UpdateFiles.Count - 1 do
  begin
    LUpdateSystemFile := TIUpdateSystemFile.Clone(AUpdateVersion.UpdateFiles[LUpdateFileIndex]);
    FUpdateFiles.Add(LUpdateSystemFile);
  end;

  FUpdateSize := AUpdateVersion.UpdateSize;
  FUpdateCreated := AUpdateVersion.UpdateCreated;
  FUpdateModified := AUpdateVersion.UpdateModified;
end;

destructor TUpdateVersion.Destroy;
begin
  FUpdateVersion := nil;
  FUpdateFiles.Free;
  inherited Destroy;
end;

{ TUpdateVersions }

constructor TUpdateVersions.Create;
begin
  inherited Create;
  FUpgrade := TUpdateVersion.Create;
  FUpdate := TUpdateVersion.Create;
end;

function TUpdateVersions.HasUpdate: Boolean;
begin
  result := (Upgrade.UpdateFiles.Count > 0) or (Update.UpdateFiles.Count > 0);
end;

destructor TUpdateVersions.Destroy;
begin
  FUpgrade.Free;
  FUpdate.Free;
  inherited Destroy;
end;

{ TUpdateController }

function TUpdateController.GetBusy: Boolean;
begin
  result := FBusy;
end;

function TUpdateController.ReadUpdate(): Boolean;
const
  u = 'upd/';
var
  LNeedToUninitialize: Boolean;
  LHTTPManager: IHTTPManager;
  LHTTPRequest: IHTTPRequest;
  LHTTPParams: IHTTPParams;
  LHTTPOptions: IHTTPOptions;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;

  LXMLDoc: IXMLDocument;
begin
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, FSQLFormatSettings);
  FSQLFormatSettings.DateSeparator := '-';
  FSQLFormatSettings.TimeSeparator := ':';
  FSQLFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  FSQLFormatSettings.ShortTimeFormat := 'hh:mm:ss';

  LHTTPManager := THTTPManager.Instance();

  LHTTPRequest := THTTPRequest.Create(Homepage + u + copy(u, 2, 1) + '.php?action=update_v2');

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams, TFileVersionInfo.GetVersionInfo(ParamStr(0)).FileVersionNumber do
  begin
    AddFormField('major_version', IntToStr(Major));
    AddFormField('minor_version', IntToStr(Minor));
    AddFormField('major_build', IntToStr(Build));
    AddFormField('minor_build', IntToStr(Reversion));
  end;

  LHTTPOptions := THTTPOptions.Create(FProxy);
  with LHTTPOptions do
  begin
    ConnectTimeout := FConnectTimeout;
    ReadTimeout := FReadTimeout;
  end;

  LRequestID := LHTTPManager.Post(LHTTPRequest, LHTTPParams, LHTTPOptions);

  LHTTPManager.WaitFor(LRequestID);

  LHTTPProcess := LHTTPManager.GetResult(LRequestID);

  if (LHTTPProcess.HTTPResult.HasError) then
  begin
    FErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorClassName + ': ' + LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
    Exit(False);
  end;

  LNeedToUninitialize := Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED));
  try
    LXMLDoc := NewXMLDocument;
    try
      with LXMLDoc do
      begin
        LoadFromXML(LHTTPProcess.HTTPResult.SourceCode);
        Active := True;
      end;

      with LXMLDoc.DocumentElement do
        if HasChildNodes then
        begin
          if ChildNodes.Nodes['upgrade'].HasChildNodes then
            ReadFiles(ChildNodes.Nodes['upgrade'], FUpdateVersions.Upgrade);
          if ChildNodes.Nodes['update'].HasChildNodes then
            ReadFiles(ChildNodes.Nodes['update'], FUpdateVersions.Update);
        end;
    finally
      LXMLDoc := nil;
    end;
  finally
    if LNeedToUninitialize then
      CoUninitialize;
  end;

  result := True;
end;

procedure TUpdateController.ReadFiles(const ANode: IXMLNode; AUpdateVersion: TUpdateVersion);
var
  LFileIndex: Integer;
  LFileSystem: TFileSystem;
  LFilePathName: string;

  LUpdateSystemFile: IUpdateSystemFile;
begin
  with ANode.ChildNodes.Nodes['header'] do
    if HasChildNodes then
    begin
      AUpdateVersion.DownloadPath := VarToStr(ChildNodes.Nodes['files_dir'].NodeValue);

      AUpdateVersion.UpdateVersion.MajorVersion := VarToIntDef(ChildNodes.Nodes['major_version'].NodeValue, 0);
      AUpdateVersion.UpdateVersion.MinorVersion := VarToIntDef(ChildNodes.Nodes['minor_version'].NodeValue, 0);
      AUpdateVersion.UpdateVersion.MajorBuild := VarToIntDef(ChildNodes.Nodes['major_build'].NodeValue, 0);
      AUpdateVersion.UpdateVersion.MinorBuild := VarToIntDef(ChildNodes.Nodes['minor_build'].NodeValue, 0);

      AUpdateVersion.UpdateCreated := StrToDateTime(VarToStr(ChildNodes.Nodes['created'].NodeValue), FSQLFormatSettings);
      AUpdateVersion.UpdateModified := StrToDateTime(VarToStr(ChildNodes.Nodes['modified'].NodeValue), FSQLFormatSettings);
    end;

  with ANode.ChildNodes.Nodes['files'] do
    if HasChildNodes then
    begin
      for LFileIndex := 0 to ChildNodes.Count - 1 do
        with ChildNodes.Nodes[LFileIndex] do
        begin
          if (IsNumber(ChildNodes.Nodes['filesystem_id'].NodeValue)) then
            LFileSystem := TFileSystem(VarToIntDef(ChildNodes.Nodes['filesystem_id'].NodeValue, 0))
          else
            LFileSystem := TEnum.Parse<TFileSystem>(VarToStr(ChildNodes.Nodes['filesystem_id'].NodeValue));

          LFilePathName := IncludeTrailingPathDelimiter(
            { . } IncludeTrailingPathDelimiter(GetPathFromFileSystemID(LFileSystem)) + VarToStr(ChildNodes.Nodes['path_appendix'].NodeValue)) +
          { . } VarToStr(ChildNodes.Nodes['name'].NodeValue);

          if (not FileExists(LFilePathName)) or (not SameText(GetMD5FromFile(LFilePathName), VarToStr(ChildNodes.Nodes['checksum'].NodeValue))) then
          begin
            LUpdateSystemFile := TIUpdateSystemFile.Create(VarToStr(ChildNodes.Nodes['name'].NodeValue));

            with LUpdateSystemFile do
            begin
              FileBase.FileSystem := LFileSystem;

              FileBase.FilePathAppendix := VarToStr(ChildNodes.Nodes['path_appendix'].NodeValue);

              FileVersion.MajorVersion := VarToIntDef(ChildNodes.Nodes['major_version'].NodeValue, 0);
              FileVersion.MinorVersion := VarToIntDef(ChildNodes.Nodes['minor_version'].NodeValue, 0);
              FileVersion.MajorBuild := VarToIntDef(ChildNodes.Nodes['major_build'].NodeValue, 0);
              FileVersion.MinorBuild := VarToIntDef(ChildNodes.Nodes['minor_build'].NodeValue, 0);

              FileSizeCompressed := VarToIntDef(ChildNodes.Nodes['size_compressed'].NodeValue, 0);
              FileChecksum := VarToStr(ChildNodes.Nodes['checksum'].NodeValue);

              AUpdateVersion.UpdateSize := AUpdateVersion.UpdateSize + FileSizeCompressed;
            end;
            AUpdateVersion.UpdateFiles.Add(LUpdateSystemFile);
          end;
        end;
    end;
end;

function TUpdateController.LocalPathVariableFromFileSystem(AFileSystem: TFileSystem): string;
begin
  case AFileSystem of
    fsRoot:
      result := '%intelligen_root%';
    fsConfig:
      result := '%intelligen_configuration%';
    fsPlugins:
      result := '%intelligen_plugins%';
    fsSettings:
      result := '%intelligen_settings%';
    fsCMS:
      result := '%intelligen_templates_cms%';
    fsCMSSubject:
      result := '%intelligen_templates_cms%\subject\';
    fsCMSMessage:
      result := '%intelligen_templates_cms%\message\';
    fsSite:
      result := '%intelligen_templates_site%';
    fsType:
      result := '%intelligen_templates_type%';
  end;
end;

procedure TUpdateController.DownloadFiles(const task: IOmniTask; AUpdateVersion: TUpdateVersion);
var
  LUpdatePath, LUpdateFilesPath: string;

  LCommandLine: TStringList;

  LUpdateFileIndex: Integer;
  LUpdateFile: IUpdateSystemFile;
  LUpdateFilePath: string;

  LMemoryStream: TMemoryStream;
begin
  LUpdatePath := GetHiddenDataDir + 'update\';
  LUpdateFilesPath := LUpdatePath + 'files\';
  ForceDirectories(LUpdateFilesPath);

  FUpdateTotalBytes := AUpdateVersion.UpdateSize;
  FDownloadedBytes.Value := 0;

  LCommandLine := TStringList.Create;
  try
    LCommandLine.Add('if "%ProgramFiles(x86)%XXX"=="XXX" goto x86');
    LCommandLine.Add('set waitexe=sleep64.exe');
    LCommandLine.Add('goto checkdone');

    LCommandLine.Add(':x86');
    LCommandLine.Add('set waitexe=sleep32.exe');

    LCommandLine.Add(':checkdone');

    LCommandLine.Add('"' + LUpdatePath + '%waitexe%" 5');
    LCommandLine.Add('start "/w" "taskkill.exe" /F /IM IntelligeN.exe /t');
    LCommandLine.Add('set intelligen_root=' + ExtractFilePath(ParamStr(0)));
    LCommandLine.Add('set intelligen_configuration=' + GetConfigurationFolder);
    LCommandLine.Add('set intelligen_plugins=' + GetPluginFolder);
    LCommandLine.Add('set intelligen_settings=' + GetSettingsFolder);
    LCommandLine.Add('set intelligen_templates_cms=' + GetTemplatesCMSFolder);
    LCommandLine.Add('set intelligen_templates_site=' + GetTemplatesSiteFolder);
    LCommandLine.Add('set intelligen_templates_type=' + GetTemplatesTypeFolder);
    LCommandLine.Add('"' + LUpdatePath + '%waitexe%" 5');

    with THTTPIndyHelper.Create do
      try
        HandleWrongProtocolException := False;

        for LUpdateFileIndex := 0 to AUpdateVersion.UpdateFiles.Count - 1 do
        begin
          LUpdateFile := AUpdateVersion.UpdateFiles[LUpdateFileIndex];

          LMemoryStream := TMemoryStream.Create;
          try
            try
              Get(AUpdateVersion.DownloadPath + LUpdateFile.FileChecksum, LMemoryStream);
            except
              on E: Exception do
              begin
                FErrorMsg := 'downloading ' + LUpdateFile.FileBase.FileName + ' ' + E.message;
                task.Invoke(
                  { } procedure
                  { } begin
                  { . } DoUpdateError;
                  { } end);
                Exit;
              end;
            end;
            FDownloadedBytes.Increment(LUpdateFile.FileSizeCompressed);
            task.Invoke(
              { } procedure
              { } begin
              { . } DoUpdateDownloading;
              { } end);
            LMemoryStream.SaveToFile(LUpdateFilesPath + LUpdateFile.FileChecksum + '.zip');

            // with TAbArchive.CreateFromStream(_MemoryStream, _UpdateList.Items[I].Filechecksum + '.zip') do
            // FArchive := TAbArchive.CreateFromStream(_MemoryStream, _UpdateList.Items[I].Filechecksum + '.zip');

            try
              with TAbUnZipper.Create(nil) do
                try
                  FileName := LUpdateFilesPath + LUpdateFile.FileChecksum + '.zip';
                  BaseDirectory := LUpdateFilesPath;
                  ExtractFiles('*.*');
                finally
                  Free;
                end;
            except
              on E: Exception do
              begin
                FErrorMsg := 'extracting ' + LUpdateFile.FileBase.FileName + ' ' + E.message;
                task.Invoke(
                  { } procedure
                  { } begin
                  { . } DoUpdateError;
                  { } end);
                Exit;
              end;
            end;

            DeleteFile(LUpdateFilesPath + LUpdateFile.FileChecksum + '.zip');

            LUpdateFilePath := GetPathFromFileSystemID(LUpdateFile.FileBase.FileSystem) + LUpdateFile.FileBase.FilePathAppendix;
            if not ForceDirectories(LUpdateFilePath) then
              LCommandLine.Add('mkdir "' + LocalPathVariableFromFileSystem(LUpdateFile.FileBase.FileSystem) + LUpdateFile.FileBase.FilePathAppendix + '"');

            LCommandLine.Add('copy "' + LUpdateFilesPath + LUpdateFile.FileBase.FileName + '" "' + LocalPathVariableFromFileSystem(LUpdateFile.FileBase.FileSystem) + LUpdateFile.FileBase.FilePathAppendix + '"');
          finally
            LMemoryStream.Free;
          end;

          LUpdateFile := nil;
        end;
      finally
        Free;
      end;

    LCommandLine.Add('rmdir /s /q files');
    LCommandLine.Add('del "' + LUpdatePath + 'sleep32.exe"');
    LCommandLine.Add('del "' + LUpdatePath + 'sleep64.exe"');
    LCommandLine.Add('start "" "%intelligen_root%IntelligeN.exe"');
    LCommandLine.SaveToFile(LUpdatePath + 'exec_update.bat');
  finally
    LCommandLine.Free;
  end;

  with TResourceStream.Create(hInstance, 'sleep32', RT_RCDATA) do
    try
      SaveToFile(LUpdatePath + 'sleep32.exe');
    finally
      Free;
    end;

  with TResourceStream.Create(hInstance, 'sleep64', RT_RCDATA) do
    try
      SaveToFile(LUpdatePath + 'sleep64.exe');
    finally
      Free;
    end;

  task.Invoke(
    { } procedure
    { } begin
    { . } DoUpdateFinishedDownload;
    { } end);
end;

procedure TUpdateController.DoUpdateSearching;
begin
  if Assigned(FOnSearchingUpdate) then
    FOnSearchingUpdate(Self);
end;

procedure TUpdateController.DoUpdateNoChanges;
begin
  if Assigned(FOnUpdateNoChanges) then
    FOnUpdateNoChanges(Self);
end;

procedure TUpdateController.DoUpdateHasChanges;
begin
  if Assigned(FOnUpdateHasChanges) then
    FOnUpdateHasChanges(Self, FUpdateVersions);
end;

procedure TUpdateController.DoUpdateError;
begin
  if Assigned(FOnUpdateError) then
    FOnUpdateError(Self, FErrorMsg);
end;

procedure TUpdateController.DoUpdateStartDownload;
begin
  if Assigned(FOnUpdateStartDownload) then
    FOnUpdateStartDownload(Self);
end;

procedure TUpdateController.DoUpdateDownloading;
begin
  if Assigned(FOnUpdateDownloading) then
    FOnUpdateDownloading(Self, (Round((FDownloadedBytes.Value / FUpdateTotalBytes) * 100)));
end;

procedure TUpdateController.DoUpdateFinishedDownload;
begin
  if Assigned(FOnUpdateFinishedDownload) then
    FOnUpdateFinishedDownload(Self);
end;

constructor TUpdateController.Create(const AProxy: IProxy = nil; AConnectTimeout: Integer = 5000; AReadTimeout: Integer = 10000);
begin
  inherited Create;

  FProxy := AProxy;
  FConnectTimeout := AConnectTimeout;
  FReadTimeout := AReadTimeout;

  FUpdateVersions := nil; // Create at CheckForUpdates point
end;

destructor TUpdateController.Destroy;
begin
  if Assigned(FTaskControl) then
  begin
    FTaskControl.Terminate;
    FTaskControl.WaitFor(INFINITE);
  end;
  FTaskControl := nil;

  FUpdateVersions.Free;

  inherited Destroy;
end;

procedure TUpdateController.CheckForUpdates(const AForm: TForm = nil);
var
  SearchResult: Boolean;
begin
  if not FBusy then
  begin
    FBusy := True;

    DoUpdateSearching;

    if not Assigned(FUpdateVersions) then
    begin
      FUpdateVersions := TUpdateVersions.Create;
    end
    else
    begin
      FUpdateVersions.Free;
      FUpdateVersions := TUpdateVersions.Create;
    end;

    FTaskControl := CreateTask(
      { } procedure(const task: IOmniTask)
      { } begin
      { . } SearchResult := ReadUpdate();
      { } end);
    FTaskControl.OnTerminated(
      { } procedure(const task: IOmniTaskControl)
      { } begin
      { . } if (SearchResult) then
      { . } begin
      { . } if not FUpdateVersions.HasUpdate then
      { ... } begin
      { ..... } DoUpdateNoChanges;
      { ... } end
      { ... } else
      { ... } begin
      { ..... } DoUpdateHasChanges;
      { ..... } if Assigned(AForm) then
      { ....... } AForm.Show;
      { ... } end;
      { . } end
      { . } else
      { . } begin
      { ... } DoUpdateError;
      { . } end;
      { . } FBusy := False;
      { } end);
    FTaskControl.Run;
  end;
end;

procedure TUpdateController.Download(AUpdateVersion: TUpdateVersion);
var
  LUpdateVersion: TUpdateVersion;
begin
  if not FBusy then
  begin
    FBusy := True;

    LUpdateVersion := TUpdateVersion.Clone(AUpdateVersion);

    DoUpdateStartDownload;

    Parallel.Async(
      { } procedure(const task: IOmniTask)
      { } begin
      { . } DownloadFiles(task, LUpdateVersion);
      { } end,
      { } Parallel.TaskConfig.OnTerminated(
        { } procedure(const task: IOmniTaskControl)
        { } begin
        { . } LUpdateVersion.Free;
        { . } FBusy := False;
        { } end
        { } ));
  end;
end;

procedure TUpdateController.DownloadUpgrade;
begin
  Download(FUpdateVersions.Upgrade);
end;

procedure TUpdateController.DownloadUpdate;
begin
  Download(FUpdateVersions.Update);
end;

end.
