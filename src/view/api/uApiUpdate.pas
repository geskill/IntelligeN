unit uApiUpdate;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Graphics, Generics.Collections, IdComponent, Variants, XMLDoc, XMLIntf, ActiveX,
  // AB
  AbArcTyp, AbUnZper,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPManager, uHTTPIndyHelper,
  // Api
  uBase, uApiConst, uApiSettings,
  // DLLs
  uExport,
  // plugin system
  uPlugInInterface, uPlugInClass,
  // Utils
  uFileUtils, uPathUtils;

type
  TUpdateController = class(TThread)
  private
    //FProxy: IProxy;
    FUpdateSize, FDownloadedSize: Int64;
    FVersionInfo, FFilesInfo, FErrorMsg: string;

    procedure UpdatePosition;
    procedure SearchingUpdate;
    procedure NoUpdateAvailable;
    procedure UpdateAvailable;
    procedure UpdateFinished;
    procedure UpdateError;

    procedure Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure WorkEnd(ASender: TObject; AWorkMode: TWorkMode);

    function LocalPathFromID(AID: Integer): string;
    function LocalPathVariableFormID(AID: Integer): string;
  public
    constructor Create(CreateSuspended: Boolean);
    procedure Execute; override;
    //property Proxy: IProxy read FProxy write FProxy;
  end;

  TUpdateFile = class
  private
    FFiletype: Integer;
    FFilesize: Int64;
    FFilename, FFilechecksum: string;
  public
    property Filetype: Integer read FFiletype write FFiletype;
    property Filesize: Int64 read FFilesize write FFilesize;
    property Filename: string read FFilename write FFilename;
    property Filechecksum: string read FFilechecksum write FFilechecksum;
  end;

implementation

uses
  uUpdate;

procedure TUpdateController.UpdatePosition;
begin
  Update.update_position(round((FUpdateSize / FDownloadedSize) * 100));
end;

procedure TUpdateController.SearchingUpdate;
begin
  Update.update_searching;
end;

procedure TUpdateController.NoUpdateAvailable;
begin
  Update.update_unnecessary;
end;

procedure TUpdateController.UpdateAvailable;
begin
  Update.update_available(FVersionInfo + sLineBreak + FFilesInfo);
end;

procedure TUpdateController.UpdateFinished;
begin
  Update.update_finished;
end;

procedure TUpdateController.UpdateError;
begin
  Update.update_error(FErrorMsg);
end;

procedure TUpdateController.Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  //
end;

procedure TUpdateController.WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  //
end;

procedure TUpdateController.WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  //
end;

function TUpdateController.LocalPathFromID(AID: Integer): string;
begin
  result := GetPathFromFileSystemID(uBase.TFileSystem(AID));
end;

function TUpdateController.LocalPathVariableFormID(AID: Integer): string;
begin
  case AID of
    1:
      Result := '%intelligen_root%';
    2:
      Result := '%intelligen_configuration%';
    3:
      Result := '%intelligen_plugins%';
    4:
      Result := '%intelligen_settings%';
    5:
      Result := '%intelligen_templates_cms%';
    6:
      Result := '%intelligen_templates_cms%\message\';
    7:
      Result := '%intelligen_templates_cms%\subject\';
    8:
      Result := '%intelligen_templates_site%';
    9:
      Result := '%intelligen_templates_type%';
  end;
end;


constructor TUpdateController.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);

  // FProxy := TProxy.Create;
end;

procedure TUpdateController.Execute;
const
  u = 'upd/';
var
  LHTTPManager: IHTTPManager;
  LHTTPRequest: IHTTPRequest;
  LHTTPParams: IHTTPParams;
  LHTTPOptions: IHTTPOptions;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;

  LUpdateList: TList<TUpdateFile>;

  XMLDoc: IXMLDocument;
  I: Integer;

  LFilePathName: string;

  _StringList: TStringList;
  _MemoryStream: TMemoryStream;
  _DownloadPath, _UpdatePath: string;
  _UpdateList: TList<TUpdateFile>;
  _UpdateFile: TUpdateFile;
begin
  Queue(SearchingUpdate);

  LHTTPManager := THTTPManager.Instance();

  LHTTPRequest := THTTPRequest.Create(Homepage + u + copy(u, 2, 1) + '.php?action=update_v2');

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams, GetFileVersion(ParamStr(0)) do
  begin
    AddFormField('major_version', IntToStr(MajorVersion));
    AddFormField('minor_version', IntToStr(MinorVersion));
    AddFormField('major_build', IntToStr(MajorBuild));
    AddFormField('minor_build', IntToStr(MinorBuild));
  end;

  LHTTPOptions := THTTPOptions.Create(SettingsManager.Settings.HTTP.GetProxy(psaMain));

  LHTTPOptions.ConnectTimeout := SettingsManager.Settings.HTTP.ConnectTimeout;
  LHTTPOptions.ReadTimeout := SettingsManager.Settings.HTTP.ReadTimeout;

  LRequestID := LHTTPManager.Post(LHTTPRequest, LHTTPParams, LHTTPOptions);

  repeat
    sleep(75);
  until LHTTPManager.HasResult(LRequestID);

  LHTTPProcess := LHTTPManager.GetResult(LRequestID);

  if (LHTTPProcess.HTTPResult.HasError) then
  begin
    Queue(UpdateError);
    FErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorClassName + ': ' + LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
    Exit;
  end;

  LUpdateList := TList<TUpdateFile>.Create;
  try

    OleInitialize(nil);
    XMLDoc := NewXMLDocument;
    try
      with XMLDoc do
      begin
        LoadFromXML(LHTTPProcess.HTTPResult.SourceCode);
        Active := True;
      end;

      with XMLDoc.DocumentElement do
        if HasChildNodes then
        begin
          with ChildNodes.Nodes['header'] do
            if HasChildNodes then
            begin
              _DownloadPath := VarToStr(ChildNodes.Nodes['files_dir'].NodeValue);
              FVersionInfo := VarToStr(ChildNodes.Nodes['major_version'].NodeValue) + '.' +
              { . } VarToStr(ChildNodes.Nodes['minor_version'].NodeValue) + '.' +
              { . } VarToStr(ChildNodes.Nodes['major_build'].NodeValue) + '.' +
              { . } VarToStr(ChildNodes.Nodes['minor_build'].NodeValue);
            end;

          // TODO: seperate between upgrade / update

          with ChildNodes.Nodes['files'] do
            if HasChildNodes then
            begin
              FUpdateSize := 0;
              FFilesInfo := '';
              for I := 0 to ChildNodes.Count - 1 do
                with ChildNodes.Nodes[I] do
                begin

                  LFilePathName := IncludeTrailingPathDelimiter(
                    { . } IncludeTrailingPathDelimiter(LocalPathFromID(StrToIntDef(VarToStr(ChildNodes.Nodes['filesystem_id'].NodeValue), 0))) +
                    { ... } VarToStr(ChildNodes.Nodes['path_appendix'].NodeValue)
                    { . } ) +
                  { . } VarToStr(ChildNodes.Nodes['name'].NodeValue);

                  if (not FileExists(LFilePathName)) or (not SameText(GetMD5FromFile(LFilePathName), VarToStr(ChildNodes.Nodes['checksum'].NodeValue))) then
                  begin
                    _UpdateFile := TUpdateFile.Create;

                    with _UpdateFile do
                    begin
                      Filetype := StrToIntDef(VarToStr(ChildNodes.Nodes['filesystem_id'].NodeValue), 0);
                      Filesize := StrToIntDef(VarToStr(ChildNodes.Nodes['size'].NodeValue), 0);
                      Filename := VarToStr(ChildNodes.Nodes['name'].NodeValue);
                      Filechecksum := VarToStr(ChildNodes.Nodes['checksum'].NodeValue);

                      Inc(FUpdateSize, Filesize);
                    end;
                    _UpdateList.Add(_UpdateFile);
                  end;
                end;
            end;
        end;
    finally
      XMLDoc := nil;
      OleUninitialize;
    end;

    if not(_UpdateList.Count > 0) then
    begin
      Queue(NoUpdateAvailable);
    end
    else
    begin

      with THTTPIndyHelper.Create do
        try
          OnWork := Work;
          OnWorkBegin := WorkBegin;
          OnWorkEnd := WorkEnd;
          FDownloadedSize := 0;
          Queue(UpdateAvailable);

          _UpdatePath := GetHiddenDataDir + 'update\';
          ForceDirectories(_UpdatePath + 'files\');

          _StringList := TStringList.Create;
          try
            _StringList.Add('if "%ProgramFiles(x86)%XXX"=="XXX" goto x86');
            _StringList.Add('set waitexe=sleep64.exe');
            _StringList.Add('goto checkdone');

            _StringList.Add(':x86');
            _StringList.Add('set waitexe=sleep32.exe');

            _StringList.Add(':checkdone');

            _StringList.Add('"' + _UpdatePath + '%waitexe%" 5');
            _StringList.Add('start "/w" "taskkill.exe" /F /IM IntelligeN.exe /t');
            _StringList.Add('set intelligen_root=' + ExtractFilePath(ParamStr(0)));
            _StringList.Add('set intelligen_configuration=' + GetConfigurationFolder);
            _StringList.Add('set intelligen_plugins=' + GetPluginFolder);
            _StringList.Add('set intelligen_settings=' + GetSettingsFolder);
            _StringList.Add('set intelligen_templates_cms=' + GetTemplatesCMSFolder);
            _StringList.Add('set intelligen_templates_type=' + GetTemplatesTypeFolder);
            _StringList.Add('"' + _UpdatePath + '%waitexe%" 5');

            for I := 0 to _UpdateList.Count - 1 do
            begin
              _MemoryStream := TMemoryStream.Create;
              try
                Get(_DownloadPath + _UpdateList.Items[I].Filechecksum, _MemoryStream);
                Inc(FDownloadedSize, _UpdateList.Items[I].Filesize);
                Queue(UpdatePosition);
                _MemoryStream.SaveToFile(_UpdatePath + 'files\' + _UpdateList.Items[I].Filechecksum + '.zip');
                // with TAbArchive.CreateFromStream(_MemoryStream, _UpdateList.Items[I].Filechecksum + '.zip') do

                // Exit;

                with TAbUnZipper.Create(nil) do
                  try
                    // FArchive := TAbArchive.CreateFromStream(_MemoryStream, _UpdateList.Items[I].Filechecksum + '.zip');
                    Filename := _UpdatePath + 'files\' + _UpdateList.Items[I].Filechecksum + '.zip';
                    BaseDirectory := _UpdatePath + 'files\';
                    ExtractFiles('*.*');
                  finally
                    Free;
                  end;

                DeleteFile(_UpdatePath + 'files\' + _UpdateList.Items[I].Filechecksum + '.zip');

                _StringList.Add('copy "' + _UpdatePath + 'files\' + _UpdateList.Items[I].Filename + '" "' + LocalPathVariableFormID(_UpdateList.Items[I].Filetype) + '"');
              finally
                _MemoryStream.Free;
              end;
            end;
          finally
            Free;
          end;

          _StringList.Add('rmdir /s /q files');
          _StringList.Add('del "' + _UpdatePath + 'sleep32.exe"');
          _StringList.Add('del "' + _UpdatePath + 'sleep64.exe"');
          _StringList.Add('start "" "%intelligen_root%IntelligeN.exe"');
          _StringList.SaveToFile(_UpdatePath + 'exec_update.bat');
        finally
          _StringList.Free;
        end;

      with TResourceStream.Create(hInstance, 'sleep32', RT_RCDATA) do
        try
          SaveToFile(_UpdatePath + 'sleep32.exe');
        finally
          Free;
        end;

      with TResourceStream.Create(hInstance, 'sleep64', RT_RCDATA) do
        try
          SaveToFile(_UpdatePath + 'sleep64.exe');
        finally
          Free;
        end;

      Queue(UpdateFinished);
    end;
  finally
    LUpdateList.Free;
  end;

end;

end.
