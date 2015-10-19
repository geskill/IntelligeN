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
  uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst;

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
  Result := copy(AFolderName, Pos('/_', string(AFolderName)) + 1);
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
    ErrorMsg := LHTTPProcess.HTTPResult.SourceCode;
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
    if UseAccount then
      AddFormField('apikey', AccountName);

    AddFormField('folderCodes', GetFolderID(AFolderIdentifier));
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
    ErrorMsg := LHTTPProcess.HTTPResult.SourceCode;
  end;
end;

end.
