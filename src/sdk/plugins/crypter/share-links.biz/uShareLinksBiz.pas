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
  uPlugInInterface, uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TShareLinksBiz = class(TCrypterPlugIn)
  protected { . }
  const
    WEBSITE = 'http://share-links.biz/';

    function GetFolderID(AFolderName: string): string;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function GetServiceRequiresAccess: TCrypterAccess; override;

    function AddFolder(const ACrypterData: ICrypterData; const AMirrorContainer: IDirectlinkContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override;
    function EditFolder(const ACrypterData: ICrypterData; const AMirrorContainer: IDirectlinkContainer; var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override;
    function DeleteFolder(const AAccountData: IAccountData; const AFolderIdentifier: WideString): WordBool; override;
    function GetFolder(const AAccountData: IAccountData; const AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override;
  end;

implementation

function TShareLinksBiz.GetFolderID(AFolderName: string): string;
begin
  Result := copy(AFolderName, Pos('/_', string(AFolderName)) + 2);
end;

function TShareLinksBiz.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TShareLinksBiz.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TShareLinksBiz.GetDescription;
begin
  Result := GetName + ' crypter plug-in.';
end;

function TShareLinksBiz.GetName;
begin
  Result := 'Share-Links.biz';
end;

function TShareLinksBiz.GetServiceRequiresAccess;
begin
  Result := caAPIKey;
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

  LFoldertypes := TFoldertypes(TFoldertype(ACrypterData.Foldertypes));
  LContainertypes := TContainertypes(TContainertype(ACrypterData.ContainerTypes));

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    if ACrypterData.UseAccount then
      AddFormField('apikey', ACrypterData.AccountName);

    if not(ACrypterData.FolderName = '') then
      AddFormField('folderName', ACrypterData.FolderName);

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

    AddFormField('captcha', IfThen(ACrypterData.UseCaptcha, '1', '0'));

    if ACrypterData.UseVisitorPassword then
      AddFormField('pass_user', ACrypterData.Visitorpassword);

    if ACrypterData.UseAdminPassword then
      AddFormField('pass_admin', ACrypterData.AdminPassword);

    AddFormField('c_web', IfThen(ftWeb in LFoldertypes, '1', '0'));

    if ftContainer in LFoldertypes then
    begin
      AddFormField('c_ccf', IfThen(ctCCF in LContainertypes, '1', '0'));

      AddFormField('c_dlc', IfThen(ctDLC in LContainertypes, '1', '0'));

      AddFormField('c_rsdf', IfThen(ctRSDF in LContainertypes, '1', '0'));
    end;

    AddFormField('c_cnl', IfThen(ACrypterData.UseCNL, '1', '0'));

    if ACrypterData.UseDescription then
      AddFormField('comment', ACrypterData.Description);
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api/insert'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

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

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    if AAccountData.UseAccount then
      AddFormField('apikey', AAccountData.AccountName);

    AddFormField('folderCode', GetFolderID(AFolderIdentifier));
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api/content'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else if (Pos('***', string(LHTTPProcess.HTTPResult.SourceCode)) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LHTTPProcess.HTTPResult.SourceCode;
        Expression := '(.*?);(.*?);(\d+);(.*?);(\d+);(.*?)';

        if Exec(InputString) then
        begin
          // TODO: Validate this

          case IndexText(Match[6], ['online', 'unknown', 'offline', 'unchecked', 'parts']) of
            0:
              ACrypterFolderInfo.Status := csOnline;
            1:
              ACrypterFolderInfo.Status := csUnknown;
            2:
              ACrypterFolderInfo.Status := csOffline;
            3:
              ACrypterFolderInfo.Status := csMixedOnOffline;
          else
            ACrypterFolderInfo.Status := csUnknown;
          end;

          ACrypterFolderInfo.Size := RoundTo((StrToInt64(Match[3]) / 1048576), -2);
          ACrypterFolderInfo.Hoster := Match[4];
          ACrypterFolderInfo.Parts := 0;

          Result := True;
        end
        else
        begin
          Self.ErrorMsg := LHTTPProcess.HTTPResult.SourceCode;
        end;
      finally
        Free;
      end;
  end
  else
  begin
    ErrorMsg := Trim(LHTTPProcess.HTTPResult.SourceCode);
  end;
end;

end.
