unit uApiUpdateController;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, Generics.Collections, HTTPApp, Variants,
  // AB
  AbArcTyp, AbZipper,
  // DEC
  DECCipher, DECHash, DECFmt,
  // Spring Framework
  Spring.Utils, Spring.SystemUtils,
  // OmniThreadLibrary
  OtlParallel, OtlTaskControl,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPManager,
  // Common
  uBase,
  // Export
  uDynamicExport,
  // Api
  uApiServerXMLReader, uApiServerInterface, uApiUpdateConst,
  uApiUpdateInterface, uApiUpdateInterfaceBase, uApiUpdateModel,
  // Utils
  uFileUtils;

type
  /// <author>Sebastian Klatte</author>
  TUpdateController = class
  protected
    FStoreUpdateFilesPath: string;
  public

  end;

  TUpdateManagerLocalFileList = TList<IUpdateManagerSystemFile>;

  TUpdateManagerVersionsList = TList<IUpdateManagerVersion>;
  TUpdateManagerSystemsList = TList<IUpdateManagerSystemFileBase>;
  TUpdateSystemFileBaseList = TList<IUpdateSystemFileBase>;

  TLocalUpdateController = class(TUpdateController)
  private
    FIntelligeNFileSystem: TIntelligeNFileSystem;

  type
    TLocalFileProcess = reference to procedure(const ALocalSystemFile: IUpdateManagerSystemFile);
  protected
    procedure InspectPossibleActions(const ALocalSystemFile: IUpdateManagerSystemFile);
    procedure GatherBaseLocalFileInformation(const ALocalSystemFile: IUpdateManagerSystemFile);
    procedure GatherLocalFileInformation(const ALocalSystemFile: IUpdateManagerSystemFile);
    procedure CompressLocalFile(const ALocalSystemFile: IUpdateManagerSystemFile);

    procedure ProcessLocalFiles(ALocalFileProcess: TLocalFileProcess; AList: TUpdateManagerLocalFileList);
  public
    constructor Create(AFileSystemLib: WideString);

    // class procedure ExtractExecuteFiles(AList: TUpdateLocalFileList; out oList: TUpdateFileVersionList);

    procedure GetLocalFiles(ASystemsList: TUpdateManagerSystemsList; out AList: TUpdateManagerLocalFileList);
    procedure GetPossibleActionsForLocalFiles(AList: TUpdateManagerLocalFileList);

    destructor Destroy; override;
  end;

  TLocalUploadController = class(TUpdateController)
  private
    FServer: IUpdateServer;

  type
    TUpdateRequest = reference to procedure(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
  private
    procedure RequestGetVersions(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
    procedure RequestGetSystems(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
    procedure RequestFTPServer(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
    procedure RequestVersionAdd(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
    procedure RequestSystemsAdd(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
  protected
    procedure Request(ARequestID: Double; AUpdateRequest: TUpdateRequest; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
  public
    constructor Create(AServer: IUpdateServer);

    function GetVersions(out AVersionsList: TUpdateManagerVersionsList; out AErrorMsg: WideString): WordBool;
    function GetSystems(out ASystemsList: TUpdateManagerSystemsList; out AErrorMsg: WideString): WordBool;
    function GetFTPServer(out AFTPServer: IFTPServer; out AErrorMsg: WideString): WordBool;

    function AddVersion(AMajorVersion, AMinorVersion, AMajorBuild, AMinorBuild: Integer; out AVersionID: Integer; out AErrorMsg: WideString): WordBool;
    function AddSystems(ASystemFileBaseList: TUpdateSystemFileBaseList; out AErrorMsg: WideString): WordBool;

    destructor Destroy; override;
  end;

implementation

procedure TLocalUpdateController.InspectPossibleActions(const ALocalSystemFile: IUpdateManagerSystemFile);
begin
  with ALocalSystemFile.LocalFile do
    case Condition of
      ucNew:
        begin
          if Status then
            Action := uaAddnUpdate
          else
            Action := uaIgnoreThisUpdate;

          Actions := [uaAddnUpdate, uaIgnoreThisUpdate];
        end;
      ucFound:
        begin
          if Status then
            Action := uaEditnUpdate
          else
            Action := uaDelete;

          Actions := [uaEditnUpdate, uaDelete, uaIgnoreThisUpdate];
        end;
      ucMissing:
        begin
          if Status then
            Action := uaDelete
          else
            Action := uaIgnoreThisUpdate;

          Action := uaIgnoreThisUpdate;
          Actions := [uaDelete, uaIgnoreThisUpdate];
        end;
    end;
end;

procedure TLocalUpdateController.GatherBaseLocalFileInformation(const ALocalSystemFile: IUpdateManagerSystemFile);
begin
  with ALocalSystemFile do
  begin
    FileBase.FileSystem := FIntelligeNFileSystem.GetFileSystemIDFromPath(ALocalSystemFile.LocalFile.FileName);
    FileBase.FilePathAppendix := ExtractRelativePath(FIntelligeNFileSystem.GetPathFromFileSystemID(FileBase.FileSystem), ExtractFilePath(ALocalSystemFile.LocalFile.FileName));
    FileBase.FileName := ExtractFileName(ALocalSystemFile.LocalFile.FileName);
  end;
end;

procedure TLocalUpdateController.GatherLocalFileInformation(const ALocalSystemFile: IUpdateManagerSystemFile);
var
  LFileVersion: TFileVersionInfo;
begin
  LFileVersion := TFileVersionInfo.GetVersionInfo(ALocalSystemFile.LocalFile.FileName);

  with ALocalSystemFile do
  begin
    FileChecksum := GetMD5FromFile(ALocalSystemFile.LocalFile.FileName);

    with FileVersion do
    begin
      MajorVersion := LFileVersion.FileVersionNumber.Major;
      MinorVersion := LFileVersion.FileVersionNumber.Minor;
      MajorBuild := LFileVersion.FileVersionNumber.Build;
      MinorBuild := LFileVersion.FileVersionNumber.Reversion;
    end;
  end;
end;

procedure TLocalUpdateController.CompressLocalFile(const ALocalSystemFile: IUpdateManagerSystemFile);
begin
  with TAbZipper.Create(nil) do
    try
      AutoSave := True;
      DOSMode := False;

      FileName := FStoreUpdateFilesPath + ALocalSystemFile.FileChecksum + '.zip';
      BaseDirectory := ExtractFilePath(FileName);

      AddFiles(ALocalSystemFile.LocalFile.FileName, 0);

      CloseArchive;

      ALocalSystemFile.FileSizeCompressed := GetFileSize(FileName);
    finally
      Free;
    end;
end;

procedure TLocalUpdateController.ProcessLocalFiles(ALocalFileProcess: TLocalFileProcess; AList: TUpdateManagerLocalFileList);
var
  LFileIndex: Integer;
begin
  for LFileIndex := 0 to AList.Count - 1 do
    ALocalFileProcess(AList[LFileIndex]);
end;

constructor TLocalUpdateController.Create;
begin
  inherited Create;
  FIntelligeNFileSystem := TIntelligeNFileSystem.Create(AFileSystemLib);
end;

(*
  class procedure TLocalUpdateController.ExtractExecuteFiles;
  const
  EXECUTE_FILES: array [0 .. 2] of string = ('.EXE', '.DLL', '.BPL');
  var
  LUpdateLocalFile: IUpdateLocalFile;
  begin
  oList := TUpdateFileVersionList.Create;

  for LUpdateLocalFile in AList do
  if not(IndexText(ExtractFileExt(LUpdateLocalFile.FileName), EXECUTE_FILES) = -1) then
  oList.Add(LUpdateLocalFile.FileVersion);
  end;
*)

procedure TLocalUpdateController.GetLocalFiles;
var
  LRootDir: string;
  LFileList: TStringList;
  LFileIndex, LSystemFileIndex: Integer;

  LUpdateManagerSystemFile: IUpdateManagerSystemFile;
  LUpdateManagerLocalFile: IUpdateManagerLocalFile;
  LUpdateManagerSystemFileBase: IUpdateManagerSystemFileBase;

  LLocalFileNotInSystem: Boolean;
  LLocalFileNotFound: Boolean;
begin
  AList := TUpdateManagerLocalFileList.Create;

  LRootDir := FIntelligeNFileSystem.GetRootDir;

  // Generate list of all local files.
  LFileList := TStringList.Create;
  try
    GetFilesInDirectory(LRootDir, '*.*', LFileList, True, True, True, True);

    for LFileIndex := 0 to LFileList.Count - 1 do
    begin
      LUpdateManagerLocalFile := TIUpdateManagerLocalFile.Create(LFileList.Strings[LFileIndex]);

      LUpdateManagerSystemFileBase := nil;

      // Iterate over list of all system files to find already integrated files.
      LLocalFileNotInSystem := True;
      for LSystemFileIndex := 0 to ASystemsList.Count - 1 do
      begin
        with ASystemsList[LSystemFileIndex] do
          if SameText(LUpdateManagerLocalFile.FileName, GetFullFileName(FIntelligeNFileSystem)) then
          begin
            LUpdateManagerSystemFileBase := ASystemsList[LSystemFileIndex];

            LUpdateManagerLocalFile.Online := True;
            LUpdateManagerLocalFile.Status := True;
            LUpdateManagerLocalFile.Condition := ucFound;

            LLocalFileNotInSystem := False;
            break;
          end;
      end;

      LUpdateManagerSystemFile := TIUpdateManagerSystemFile.Create(LUpdateManagerLocalFile, LUpdateManagerSystemFileBase);

      if (LLocalFileNotInSystem) then
      begin
        GatherBaseLocalFileInformation(LUpdateManagerSystemFile);
      end;

      AList.Add(LUpdateManagerSystemFile);
    end;
  finally
    LFileList.Free;
  end;

  ProcessLocalFiles(GatherLocalFileInformation, AList);

  // Iterate over list of all system files to find missing local files.
  for LSystemFileIndex := 0 to ASystemsList.Count - 1 do
  begin
    LLocalFileNotFound := False;
    for LFileIndex := 0 to AList.Count - 1 do
      if SameText(AList[LFileIndex].LocalFile.FileName, ASystemsList[LSystemFileIndex].GetFullFileName(FIntelligeNFileSystem)) then
      begin
        LLocalFileNotFound := True;
        break;
      end;

    if not LLocalFileNotFound then
    begin
      LUpdateManagerSystemFile := TIUpdateManagerSystemFile.Create(nil, nil);

      with LUpdateManagerSystemFile do
      begin
        LocalFile.Online := False;
        LocalFile.Status := True;
        LocalFile.Condition := ucMissing;
      end;
      AList.Add(LUpdateManagerSystemFile);
    end;
  end;
end;

procedure TLocalUpdateController.GetPossibleActionsForLocalFiles(AList: TUpdateManagerLocalFileList);
begin
  ProcessLocalFiles(InspectPossibleActions, AList);
end;

destructor TLocalUpdateController.Destroy;
begin
  FIntelligeNFileSystem.Free;
  inherited Destroy;
end;

{ TLocalUploadController }

procedure TLocalUploadController.RequestGetVersions;
var
  LVersionsResponse: IVersionsResponse;
begin
  OutputDebugString(PChar(HTTPResult.SourceCode));

  LVersionsResponse := TServerXMLReader.ReadVersions(HTTPResult.SourceCode);

  if LVersionsResponse.HasError then
    oErrorMsg := LVersionsResponse.Msg
  else
    oErrorMsg := '';
  oResponse := LVersionsResponse;
end;

procedure TLocalUploadController.RequestGetSystems;
var
  LSystemsResponse: ISystemsResponse;
begin
  OutputDebugString(PChar(HTTPResult.SourceCode));

  LSystemsResponse := TServerXMLReader.ReadSystems(HTTPResult.SourceCode);

  if LSystemsResponse.HasError then
    oErrorMsg := LSystemsResponse.Msg
  else
    oErrorMsg := '';
  oResponse := LSystemsResponse;
end;

procedure TLocalUploadController.RequestFTPServer;

  function DecodeValue(AValue, AKey: string): string;
  begin
    with TCipher_Rijndael.Create do
      try
        Mode := cmCFB8;
        Init(THash_MD5.CalcBinary(AKey, TFormat_HEXL));
        Result := DecodeBinary(AValue, TFormat_MIME64);
      finally
        Free;
      end;
  end;

var
  LFTPServerResponse: IFTPServerResponse;
  LDecryptKey: string;
begin
  LFTPServerResponse := TServerXMLReader.ReadFTPServer(HTTPResult.SourceCode);

  if LFTPServerResponse.HasError then
    oErrorMsg := LFTPServerResponse.Msg
  else
  begin
    with LFTPServerResponse.Server do
    begin
      LDecryptKey := Name + FServer.AccessToken + Name;

      Port := DecodeValue(Port, LDecryptKey);

      Path := DecodeValue(Path, LDecryptKey);

      Username := DecodeValue(Username, LDecryptKey);

      Password := DecodeValue(Password, LDecryptKey);
    end;
    oErrorMsg := '';
  end;
  oResponse := LFTPServerResponse;
end;

procedure TLocalUploadController.RequestVersionAdd;
var
  LVersionAddResponse: IVersionAddResponse;
begin
  OutputDebugString(PChar(HTTPResult.SourceCode));

  LVersionAddResponse := TServerXMLReader.ReadVersionAdd(HTTPResult.SourceCode);

  if LVersionAddResponse.HasError then
    oErrorMsg := LVersionAddResponse.Msg
  else
    oErrorMsg := '';
  oResponse := LVersionAddResponse;
end;

procedure TLocalUploadController.RequestSystemsAdd(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
var
  LBasicServerResponse: IBasicServerResponse;
begin
  OutputDebugString(PChar(HTTPResult.SourceCode));

  LBasicServerResponse := TServerXMLReader.ReadSystemsAdd(HTTPResult.SourceCode);

  if LBasicServerResponse.HasError then
    oErrorMsg := LBasicServerResponse.Msg
  else
    oErrorMsg := '';
  oResponse := LBasicServerResponse;
end;

procedure TLocalUploadController.Request;
var
  HTTPProcess: IHTTPProcess;
begin
  oResponse := nil;
  oErrorMsg := '';

  with THTTPManager.Instance() do
  begin
    THTTPManager.Wait(ARequestID);

    HTTPProcess := THTTPManager.Instance().GetResult(ARequestID);
    if HTTPProcess.HTTPResult.HasError then
      oErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage
    else
      AUpdateRequest(HTTPProcess.HTTPResult, oResponse, oErrorMsg);
  end;
end;

constructor TLocalUploadController.Create;
begin
  inherited Create;
  FServer := AServer;
end;

function TLocalUploadController.GetVersions(out AVersionsList: TUpdateManagerVersionsList; out AErrorMsg: WideString): WordBool;
var
  LBasicServerResponse: IBasicServerResponse;
  LVersionsResponse: IVersionsResponse;
begin
  Request(THTTPManager.Instance().Get(THTTPRequest.Create(FServer.Name + 'p.php?action=upload_v2&upload=get_versions_v2&access_token=' + HTTPEncode(FServer.AccessToken))), { }
    RequestGetVersions, { }
    LBasicServerResponse, { }
    AErrorMsg);

  if Assigned(LBasicServerResponse) and (LBasicServerResponse.QueryInterface(IVersionsResponse, LVersionsResponse) = 0) then
    AVersionsList := LVersionsResponse.Versions;

  Result := Assigned(LVersionsResponse);
end;

function TLocalUploadController.GetSystems(out ASystemsList: TUpdateManagerSystemsList; out AErrorMsg: WideString): WordBool;
var
  LBasicServerResponse: IBasicServerResponse;
  LSystemsResponse: ISystemsResponse;
begin
  Request(THTTPManager.Instance().Get(THTTPRequest.Create(FServer.Name + 'p.php?action=upload_v2&upload=get_systems_v2&access_token=' + HTTPEncode(FServer.AccessToken))), { }
    RequestGetSystems, { }
    LBasicServerResponse, { }
    AErrorMsg);

  if Assigned(LBasicServerResponse) and (LBasicServerResponse.QueryInterface(ISystemsResponse, LSystemsResponse) = 0) then
    ASystemsList := LSystemsResponse.Systems;

  Result := Assigned(LSystemsResponse);
end;

function TLocalUploadController.GetFTPServer;
var
  LBasicServerResponse: IBasicServerResponse;
  LFTPServerResponse: IFTPServerResponse;

  buf: Integer;
begin
  Request(THTTPManager.Instance().Get(THTTPRequest.Create(FServer.Name + 'p.php?action=upload_v2&upload=get_ftp_server_v2&access_token=' + HTTPEncode(FServer.AccessToken))), { }
    RequestFTPServer, { }
    LBasicServerResponse, { }
    AErrorMsg);

  if Assigned(LBasicServerResponse) and (LBasicServerResponse.QueryInterface(IFTPServerResponse, LFTPServerResponse) = 0) then
    AFTPServer := LFTPServerResponse.Server;

  Result := Assigned(LFTPServerResponse) and TryStrToInt(LFTPServerResponse.Server.Port, buf);
end;

function TLocalUploadController.AddVersion(AMajorVersion, AMinorVersion, AMajorBuild, AMinorBuild: Integer; out AVersionID: Integer; out AErrorMsg: WideString): WordBool;
var
  LHTTPParams: IHTTPParams;

  LBasicServerResponse: IBasicServerResponse;
  LVersionAddResponse: IVersionAddResponse;
begin
  AVersionID := 0;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('major_version', IntToStr(AMajorVersion));
    AddFormField('minor_version', IntToStr(AMinorVersion));
    AddFormField('major_build', IntToStr(AMajorBuild));
    AddFormField('minor_build', IntToStr(AMinorBuild));
  end;

  Request(THTTPManager.Instance().Post(THTTPRequest.Create(FServer.Name + 'p.php?action=upload_v2&upload=add_version_v2&access_token=' + HTTPEncode(FServer.AccessToken)), LHTTPParams), { }
    RequestVersionAdd, { }
    LBasicServerResponse, { }
    AErrorMsg);

  if Assigned(LBasicServerResponse) and (LBasicServerResponse.QueryInterface(IVersionAddResponse, LVersionAddResponse) = 0) then
    AVersionID := LVersionAddResponse.VersionID;

  Result := Assigned(LBasicServerResponse) and (AVersionID > 0);
end;

function TLocalUploadController.AddSystems(ASystemFileBaseList: TUpdateSystemFileBaseList; out AErrorMsg: WideString): WordBool;
var
  LHTTPParams: IHTTPParams;
  LSystemFileBaseIndex: Integer;

  LFileVersion: IFileVersion;

  LBasicServerResponse: IBasicServerResponse;
  LVersionsResponse: IVersionsResponse;
begin
  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    for LSystemFileBaseIndex := 0 to ASystemFileBaseList.Count - 1 do
      with ASystemFileBaseList[LSystemFileBaseIndex] do
      begin
        AddFormField('systems[' + IntToStr(LSystemFileBaseIndex) + '][name]', FileName);
        AddFormField('systems[' + IntToStr(LSystemFileBaseIndex) + '][filesystem_id]', TEnum.GetName<TFileSystem>(FileSystem));
        AddFormField('systems[' + IntToStr(LSystemFileBaseIndex) + '][path_appendix]', FilePathAppendix);
      end;
  end;

  Request(THTTPManager.Instance().Post(THTTPRequest.Create(FServer.Name + 'p.php?action=upload_v2&upload=add_systems_v2&access_token=' + HTTPEncode(FServer.AccessToken)), LHTTPParams), { }
    RequestSystemsAdd, { }
    LBasicServerResponse, { }
    AErrorMsg);

  Result := Assigned(LBasicServerResponse);
end;

destructor TLocalUploadController.Destroy;
begin
  FServer := nil;
  inherited Destroy;
end;

end.
