unit uApiUpdateController;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, Generics.Collections, HTTPApp, Variants,
  // AB
  AbArcTyp, AbZipper,
  // DEC
  DECCipher, DECHash, DECFmt,
  // OmniThreadLibrary
  OtlParallel, OtlTaskControl,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPManager,
  // Api
  uApiServerXMLReader, uApiServerInterface, uApiUpdateConst, uApiUpdateInterface, uApiUpdateModel,
  // Utils
  uFileUtils,
  // IntelligeN
  IntelligeNFileSystem;

type
  /// <author>Sebastian Klatte</author>
  TUpdateController = class
  protected
    FStoreUpdateFilesPath: string;
  public

  end;

  // TUpdateLocalFileList = TList<IUpdateLocalFile>;
  // TUpdateFileVersionList = TList<IFileVersion>;
  TUpdateManagerVersionsList = TList<IUpdateManagerVersion>;
  TUpdateManagerSystemsList = TList<IUpdateManagerSystemFileBase>;

  (*
  TLocalUpdateController = class(TUpdateController)
  private
    FIntelligeNFileSystem: TIntelligeNFileSystem;

  type
    TLocalFileProcess = reference to procedure(const AUpdateLocalFile: IUpdateLocalFile);
  protected
    procedure InspectPossibleActions(const ALocalUpdateFile: IUpdateLocalFile);
    procedure GatherLocalFileInformation(const ALocalUpdateFile: IUpdateLocalFile);
    procedure CompressLocalFile(const ALocalUpdateFile: IUpdateLocalFile);
  public
    constructor Create(AFileSystemLib: WideString);

    class procedure ExtractExecuteFiles(AList: TUpdateLocalFileList; out oList: TUpdateFileVersionList);

    procedure GetLocalFiles(ASettingFiles: TList<IStatusFile>; out AList: TUpdateLocalFileList);
    procedure ProcessLocalFiles(ALocalFileProcess: TLocalFileProcess; AList: TUpdateLocalFileList);

    destructor Destroy; override;
  end;
  *)

  TLocalUploadController = class(TUpdateController)
  private
    FServer: IUpdateServer;

  type
    TUpdateRequest = reference to procedure(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
  private
    procedure RequestGetVersions(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
    procedure RequestGetSystems(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
    procedure RequestFTPServer(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
  protected
    procedure Request(ARequestID: Double; AUpdateRequest: TUpdateRequest; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
  public
    constructor Create(AServer: IUpdateServer);

    function GetVersions(out AVersionsList: TUpdateManagerVersionsList; out AErrorMsg: WideString): WordBool;
    function GetSystems(out ASystemsList: TUpdateManagerSystemsList; out AErrorMsg: WideString): WordBool;
    function GetFTPServer(out AFTPServer: IFTPServer; out AErrorMsg: WideString): WordBool;

    destructor Destroy; override;
  end;

implementation

(*
procedure TLocalUpdateController.InspectPossibleActions(const ALocalUpdateFile: IUpdateLocalFile);
begin
  with ALocalUpdateFile do
    case Condition of
      ucNew:
        begin
          Action := uaAddnUpdate;
          Actions := [Action];
        end;
      ucCondition:
        begin
          Action := uaEditnUpdate;
          Actions := [Action, uaDelete, uaIgnoreThisUpdate];
        end;
      ucMissing:
        begin
          Action := uaIgnoreThisUpdate;
          Actions := [Action, uaDelete];
        end;
    end;
end;

procedure TLocalUpdateController.GatherLocalFileInformation(const ALocalUpdateFile: IUpdateLocalFile);
var
  LFileVersion: RFileVersion;
begin
  LFileVersion := GetFileVersion(ALocalUpdateFile.FileName);

  with ALocalUpdateFile do
  begin
    FileChecksum := GetMD5FromFile(FileName);
    FileSystem := FIntelligeNFileSystem.GetFileSystemIDFromPath(FileName);
    FilePathAddition := ExtractRelativePath(FIntelligeNFileSystem.GetPathFromFileSystemID(FileSystem), ExtractFilePath(FileName));

    with FileVersion do
    begin
      MajorVersion := LFileVersion.MajorVersion;
      MinorVersion := LFileVersion.MinorVersion;
      MajorBuild := LFileVersion.MajorBuild;
      MinorBuild := LFileVersion.MinorBuild;
    end;
  end;
end;

procedure TLocalUpdateController.CompressLocalFile(const ALocalUpdateFile: IUpdateLocalFile);
begin
  with ALocalUpdateFile, TAbZipper.Create(nil) do
    try
      AutoSave := True;
      DOSMode := False;

      FileName := FStoreUpdateFilesPath + FileChecksum + '.zip';
      BaseDirectory := ExtractFilePath(FileName);

      AddFiles(ALocalUpdateFile.FileName, 0);

      CloseArchive;

      FileSizeCompressed := GetFileSize(FileName);
    finally
      Free;
    end;
end;

constructor TLocalUpdateController.Create;
begin
  inherited Create;
  FIntelligeNFileSystem := TIntelligeNFileSystem.Create(AFileSystemLib);
end;

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

procedure TLocalUpdateController.GetLocalFiles;
var
  LRootDir: string;
  LSettingFileIndex, LFileIndex, LListIndex: Integer;
  LFileList: TStringList;
  LNewFile: Boolean;
begin
  AList := TUpdateLocalFileList.Create;

  LRootDir := FIntelligeNFileSystem.GetRootDir;

  // Adding all previous files (all tracking files)
  for LSettingFileIndex := 0 to ASettingFiles.Count - 1 do
    with ASettingFiles.Items[LSettingFileIndex] do
    begin
      if FileExists(LRootDir + FileName) then
        AList.Add(TIUpdateLocalFile.Create(LRootDir + FileName, Status, ucCondition))
      else
        AList.Add(TIUpdateLocalFile.Create(LRootDir + FileName, Status, ucMissing))
    end;

  // Adding all files in selected directory
  LFileList := TStringList.Create;
  try
    GetFilesInDirectory(LRootDir, '*.*', LFileList, True, True, True, True);

    for LFileIndex := 0 to LFileList.Count - 1 do
    begin
      LNewFile := True;
      for LListIndex := 0 to AList.Count - 1 do
        if SameText(LFileList.Strings[LFileIndex], AList.Items[LListIndex].FileName) then
        begin
          LNewFile := True;
          break;
        end;

      if LNewFile then
        AList.Add(TIUpdateLocalFile.Create(LFileList.Strings[LFileIndex], False, ucNew));
    end;
  finally
    LFileList.Free;
  end;

  ProcessLocalFiles(InspectPossibleActions, AList);

  ProcessLocalFiles(GatherLocalFileInformation, AList);
end;

procedure TLocalUpdateController.ProcessLocalFiles(ALocalFileProcess: TLocalFileProcess; AList: TUpdateLocalFileList);
var
  LFileIndex: Integer;
begin
  for LFileIndex := 0 to AList.Count - 1 do
    ALocalFileProcess(AList.Items[LFileIndex]);
end;

destructor TLocalUpdateController.Destroy;
begin
  FIntelligeNFileSystem.Free;
  inherited Destroy;
end;

*)

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

procedure TLocalUploadController.RequestGetSystems(const HTTPResult: IHTTPResult; out oResponse: IBasicServerResponse; out oErrorMsg: WideString);
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

destructor TLocalUploadController.Destroy;
begin
  FServer := nil;
  inherited Destroy;
end;

end.
