unit uApiUpdate;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Graphics, Generics.Collections, IdComponent, Variants, XMLDoc, XMLIntf, ActiveX,
  // AB
  AbArcTyp, AbUnZper,
  // Api
  uApiConst, uApiHTTP,
  // DLLs
  uExport,
  // plugin system
  uPlugInInterface, uPlugInClass, uHTTPInterface, uHTTPClasses,
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
    FFilename, FFilechecksum, FFileinfo: string;
  public
    property Filetype: Integer read FFiletype write FFiletype;
    property Filesize: Int64 read FFilesize write FFilesize;
    property Filename: string read FFilename write FFilename;
    property Filechecksum: string read FFilechecksum write FFilechecksum;
    property Fileinfo: string read FFileinfo write FFileinfo;
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
  case AID of
    0:
      Result := ExtractFilePath(ParamStr(0));
    1:
      Result := GetConfigurationFolder;
    2:
      Result := GetPluginFolder;
    3:
      Result := GetSettingsFolder;
    4:
      Result := GetTemplatesCMSFolder;
    5:
      Result := GetTemplatesCMSFolder + '\message\';
    6:
      Result := GetTemplatesCMSFolder + '\subject\';
    7:
      Result := GetTemplatesSiteFolder;
    8:
      Result := GetTemplatesTypeFolder;
  end;
end;

function TUpdateController.LocalPathVariableFormID(AID: Integer): string;
begin
  case AID of
    0:
      Result := '%intelligen_root%';
    1:
      Result := '%intelligen_configuration%';
    2:
      Result := '%intelligen_plugins%';
    3:
      Result := '%intelligen_settings%';
    4:
      Result := '%intelligen_templates_cms%';
    5:
      Result := '%intelligen_templates_cms%\message\';
    6:
      Result := '%intelligen_templates_cms%\subject\';
    7:
      Result := '%intelligen_templates_site%';
    8:
      Result := '%intelligen_templates_type%';
  end;
end;

constructor TUpdateController.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);

  //FProxy := TProxy.Create;
end;

procedure TUpdateController.Execute;
const
  u = 'upd/';
var
  XMLDoc: IXMLDocument;
  I: Integer;
  _StringList: TStringList;
  _StringStream: TStringStream;
  _MemoryStream: TMemoryStream;
  _DownloadPath, _UpdatePath: string;
  _UpdateList: TList<TUpdateFile>;
  _UpdateFile: TUpdateFile;
begin
  Queue(SearchingUpdate);

  _UpdateList := TList<TUpdateFile>.Create;
  try
    with TApiHTTP.Create do
      try
        _StringStream := TStringStream.Create;
        try
          try
            Get(Homepage + u + copy(u, 2, 1) + '.php?action=update_v1', _StringStream);
          except
            on E: Exception do
            begin
              Queue(UpdateError);
              FErrorMsg := E.message;
              Exit;
            end;
          end;

          OleInitialize(nil);
          XMLDoc := NewXMLDocument;
          try
            with XMLDoc do
            begin
              LoadFromStream(_StringStream, xetUTF_8);
              Active := True;
            end;

            with XMLDoc.DocumentElement do
              if HasChildNodes then
              begin
                with ChildNodes.Nodes['header'] do
                  if HasChildNodes then
                  begin
                    _DownloadPath := Homepage + VarToStr(ChildNodes.Nodes['files_dir'].NodeValue);
                    FVersionInfo := VarToStr(ChildNodes.Nodes['major_version'].NodeValue) + '.' + VarToStr(ChildNodes.Nodes['minor_version'].NodeValue)
                      + '.' + VarToStr(ChildNodes.Nodes['major_build'].NodeValue) + '.' + VarToStr(ChildNodes.Nodes['minor_build'].NodeValue);
                  end;
                with ChildNodes.Nodes['files'] do
                  if HasChildNodes then
                  begin
                    FUpdateSize := 0;
                    FFilesInfo := '';
                    for I := 0 to ChildNodes.Count - 1 do
                      with ChildNodes.Nodes[I] do
                        if (not FileExists(LocalPathFromID(Attributes['type']) + ChildNodes.Nodes['name'].NodeValue)) or
                          (not SameText(GetMD5FromFile(LocalPathFromID(Attributes['type']) + ChildNodes.Nodes['name'].NodeValue), Attributes['csum'])) then
                        begin
                          _UpdateFile := TUpdateFile.Create;

                          with _UpdateFile do
                          begin
                            Filetype := Attributes['type'];
                            Filesize := Attributes['size'];
                            Filename := VarToStr(ChildNodes.Nodes['name'].NodeValue);
                            Filechecksum := VarToStr(Attributes['csum']);
                            Fileinfo := VarToStr(ChildNodes.Nodes['info'].NodeValue);

                            FFilesInfo := FFilesInfo + Filename + ':' + #13#10 + Fileinfo + #13#10;

                            Inc(FUpdateSize, Filesize);
                          end;
                          _UpdateList.Add(_UpdateFile);
                        end;
                  end;
              end;
          finally
            XMLDoc := nil;
            OleUninitialize;
          end;
        finally
          _StringStream.Free;
        end;

        if not(_UpdateList.Count > 0) then
        begin
          Queue(NoUpdateAvailable);
        end
        else
        begin
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

                _StringList.Add('copy "' + _UpdatePath + 'files\' + _UpdateList.Items[I].Filename + '" "' + LocalPathVariableFormID
                    (_UpdateList.Items[I].Filetype) + '"');
              finally
                _MemoryStream.Free;
              end;
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
        Free;
      end;
  finally
    _UpdateList.Free;
  end;
end;

end.
