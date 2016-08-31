unit uShareLinksBiz;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Math,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uStringUtils, uURLUtils;

type
  TShareLinksBiz = class(TCrypterPlugIn)
  private const
    website = 'http://share-links.biz/';
    function GetFolderID(AFolderName: string): string;
  public
    function GetName: WideString; override;
    function AddFolder(const AMirrorContainer: IDirectlinkContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function EditFolder(const AMirrorContainer: IDirectlinkContainer; var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function DeleteFolder(AFolderIdentifier: WideString): WordBool; override; safecall;
    function GetFolder(AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
  end;

implementation

function TShareLinksBiz.GetFolderID(AFolderName: string): string;
begin
  Result := copy(AFolderName, Pos('/_', string(AFolderName)) + 2);
end;

function TShareLinksBiz.GetName;
begin
  Result := 'Share-Links.biz';
end;

function TShareLinksBiz.AddFolder;
var
  LFoldertypes: TFoldertypes;
  LContainertypes: TContainertypes;

  LDirectlinkIndex: Integer;

  LHTTPParams: IHTTPParams;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
begin
  Result := False;

  with ACrypterFolderInfo do
  begin
    Link := '';
    Status := csNotChecked;
    Size := 0;
    PartSize := 0;
    Hoster := '';
    HosterShort := '';
    Parts := 0;
    StatusImage := '';
    StatusImageText := '';
  end;

  LFoldertypes := TFoldertypes(TFoldertype(Foldertypes));
  LContainertypes := TContainertypes(TContainertype(ContainerTypes));

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    if UseAccount then
      AddFormField('apikey', AccountName);

    if not(FolderName = '') then
      AddFormField('folderName', FolderName);

    AddFormField('links', AMirrorContainer.Directlink[0].Value);

    AddFormField('backup', IfThen(AMirrorContainer.DirectlinkCount > 1, '1', '0'));

    if AMirrorContainer.DirectlinkCount > 1 then
    begin
      AddFormField('backup_mode', '0');
      for LDirectlinkIndex := 1 to AMirrorContainer.DirectlinkCount - 1 do
      begin
        if LDirectlinkIndex = 10 then
          break;

        AddFormField('blinks' + IntToStr(LDirectlinkIndex), AMirrorContainer.Directlink[LDirectlinkIndex].Value);
      end;
    end;

    AddFormField('captcha', IfThen(UseCaptcha, '1', '0'));

    if UseVisitorPassword then
      AddFormField('pass_user', Visitorpassword);

    if UseAdminPassword then
      AddFormField('pass_admin', AdminPassword);

    AddFormField('c_web', IfThen(ftWeb in LFoldertypes, '1', '0'));

    if ftContainer in LFoldertypes then
    begin
      AddFormField('c_ccf', IfThen(ctCCF in LContainertypes, '1', '0'));

      AddFormField('c_dlc', IfThen(ctDLC in LContainertypes, '1', '0'));

      AddFormField('c_rsdf', IfThen(ctRSDF in LContainertypes, '1', '0'));
    end;

    AddFormField('c_cnl', IfThen(UseCNL, '1', '0'));

    if UseDescription then
      AddFormField('comment', Description);
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api/insert'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else if (Pos('***', string(LHTTPProcess.HTTPResult.SourceCode)) = 0) then
  begin
    ACrypterFolderInfo.Link := copy(LHTTPProcess.HTTPResult.SourceCode, 6);
    Result := True;
  end
  else
  begin
    ErrorMsg := Trim(LHTTPProcess.HTTPResult.SourceCode);
  end;
end;

function TShareLinksBiz.EditFolder;
begin
  //
end;

function TShareLinksBiz.DeleteFolder;
begin
  //
end;

function TShareLinksBiz.GetFolder;
var
  LHTTPParams: IHTTPParams;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;

  LCompleteList, LSingleList: TStrings;
  LSizeInBytes, LPartSizeInBytes: Int64;
  I, unknown, online, offline: Integer;
begin
  Result := False;

  with ACrypterFolderInfo do
  begin
    Status := csNotChecked;
    Size := 0;
    PartSize := 0;
    Hoster := '';
    HosterShort := '';
    Parts := 0;
    StatusImage := '';
    StatusImageText := '';
  end;

  LSizeInBytes := 0;
  LPartSizeInBytes := 0;
  unknown := 0;
  online := 0;
  offline := 0;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    if UseAccount then
      AddFormField('apikey', AccountName);

    AddFormField('folderCode', GetFolderID(AFolderIdentifier));
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api/content'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else if Pos(';', LHTTPProcess.HTTPResult.SourceCode) > 0 then
  begin
    LCompleteList := TStringList.Create;
    try
      LCompleteList.Text := LHTTPProcess.HTTPResult.SourceCode;
      for I := 0 to LCompleteList.Count - 1 do
      begin
        if not(LCompleteList[I] = '') then
        begin
          Inc(ACrypterFolderInfo.Parts);
          LSingleList := SplittString(';', LCompleteList[I]);
          try
            try
              case IndexText(LSingleList[5], ['2', '1', '0']) of
                0:
                  Inc(unknown);
                1:
                  Inc(online);
                2:
                  Inc(offline);
              end;
              if StrToIntDef(LSingleList[2], 0) > LPartSizeInBytes then
              begin
                LPartSizeInBytes := StrToIntDef(LSingleList[2], 0);
              end;
              LSizeInBytes := LSizeInBytes + StrToIntDef(LSingleList[2], 0);
              ACrypterFolderInfo.Hoster := LSingleList[3];
            except
              on E: Exception do
              begin
                ErrorMsg := 'The result from ' + GetName + ' was invaild: ' + E.message;
              end;
            end;
          finally
            LSingleList.Free;
          end;
        end;
      end;
    finally
      LCompleteList.Free;
    end;
    if (unknown = 0) and (online = 0) then
      ACrypterFolderInfo.Status := csOffline
    else if (unknown = 0) and (offline = 0) then
      ACrypterFolderInfo.Status := csOnline
    else if (offline > 0) and (online > 0) then
      ACrypterFolderInfo.Status := csMixedOnOffline
    else
      ACrypterFolderInfo.Status := csUnknown;

    ACrypterFolderInfo.Link := AFolderIdentifier;
    ACrypterFolderInfo.Size := RoundTo((LSizeInBytes / 1048576), -2);
    ACrypterFolderInfo.PartSize := RoundTo((LPartSizeInBytes / 1048576), -2);

    Result := True;
  end
  else
  begin
    ErrorMsg := Trim(LHTTPProcess.HTTPResult.SourceCode);
  end;

  {
  [ API-RESPONSE ]
    CSV-Output   URL;filename;filesize;provider_shortcut;backup_number;status
                 0   1        2        3                 4             5
  }
end;

end.
