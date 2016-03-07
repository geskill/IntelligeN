unit uRelinkTo;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Math, Variants,
  // RegEx
  RegExpr,
  // JSON
  uLkJSON,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInInterface, uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uStringUtils, uVariantUtils, uURLUtils;

type
  TRelinkTo = class(TCrypterPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.relink.to/';

    function GetFolderID(AFolderName: string): string;
    function GetStatusImageLink(AFolderIdentifier: WideString; Small: WordBool = True): WideString;
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

function TRelinkTo.GetFolderID(AFolderName: string): string;
begin
  if not(Pos('=', AFolderName) = 0) then
    Result := copy(AFolderName, Pos('=', AFolderName) + 1)
  else
    Result := ExtractUrlFileName(AFolderName);
end;

function TRelinkTo.GetStatusImageLink(AFolderIdentifier: WideString; Small: WordBool = True): WideString;
const
  VIEW_SNIPPET = 'view.php?id=';
  OLD_VIEW_SNIPPET = 'go.php?id=';
var
  LViewSnippet: string;
begin
  LViewSnippet := 'f/';
  if Pos(VIEW_SNIPPET, string(AFolderIdentifier)) > 0 then
    LViewSnippet := VIEW_SNIPPET
  else if Pos(OLD_VIEW_SNIPPET, string(AFolderIdentifier)) > 0 then
    LViewSnippet := OLD_VIEW_SNIPPET;

  case Small of
    True:
      Result := StringReplace(AFolderIdentifier, LViewSnippet, 'st/', []) + '.png';
    False:
      Result := StringReplace(AFolderIdentifier, LViewSnippet, 'std/', []) + '.png';
  end;
end;

function TRelinkTo.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TRelinkTo.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TRelinkTo.GetDescription;
begin
  Result := GetName + ' crypter plug-in.';
end;

function TRelinkTo.GetName;
begin
  Result := 'Relink.to';
end;

function TRelinkTo.GetServiceRequiresAccess;
begin
  Result := caAPIKey;
end;

function TRelinkTo.AddFolder;
var
  LFoldertypes: TFoldertypes;
  LContainertypes: TContainertypes;

  LDirectlinkIndex: Integer;

  LHTTPParams: IHTTPParams;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;

  LJSONobject: TlkJSONobject;
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

  LHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with LHTTPParams do
  begin
    AddFormField('protect', 'protect');

    if ACrypterData.UseAccount then
      if not(ACrypterData.AccountPassword = '') then
      begin
        AddFormField('user', ACrypterData.AccountName);
        AddFormField('pw', ACrypterData.AccountPassword);
      end
      else
        AddFormField('api', ACrypterData.AccountName);

    AddFormField('url[0]', TrimRight(StringReplace(AMirrorContainer.Directlink[0].Value, sLineBreak, ';', [rfReplaceAll]), ';'));

    for LDirectlinkIndex := 1 to AMirrorContainer.DirectlinkCount - 1 do
      AddFormField('backup_links[0][]', TrimRight(StringReplace(AMirrorContainer.Directlink[LDirectlinkIndex].Value, sLineBreak, ';', [rfReplaceAll]), ';'));

    if not(ACrypterData.FolderName = '') then
      AddFormField('title', ACrypterData.FolderName);

    if ACrypterData.UseDescription then
      AddFormField('comment', ACrypterData.Description);

    if ACrypterData.UseVisitorPassword then
      AddFormField('password', ACrypterData.Visitorpassword);

    AddFormField('web', IfThen(ftWeb in LFoldertypes, 'yes', 'no'));

    if ftContainer in LFoldertypes then
    begin
      AddFormField('dlc', IfThen(ctDLC in LContainertypes, 'yes', 'no'));
    end;

    AddFormField('cnl', IfThen(ACrypterData.UseCNL, 'yes', 'no'));

    AddFormField('captcha', IfThen(ACrypterData.UseCaptcha, 'yes', 'no'));

    if ACrypterData.UseFilePassword then
    begin
      AddFormField('password_zip_public', 'yes');
      AddFormField('password_zip', ACrypterData.FilePassword);
    end;
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(WEBSITE + 'api/api.php'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else
  begin
    try
      try
        LJSONobject := TlkJSON.ParseText(LHTTPProcess.HTTPResult.SourceCode) as TlkJSONobject;
        if (VarToStr(LJSONobject.Field['status'].Value) = '1') then
        begin
          ACrypterFolderInfo.Link := LJSONobject.Field['message'].Value;
          ACrypterFolderInfo.StatusImage := GetStatusImageLink(ACrypterFolderInfo.Link);
          ACrypterFolderInfo.StatusImageText := GetStatusImageLink(ACrypterFolderInfo.Link, False);
          Result := True;
        end
        else
        begin
          ErrorMsg := VarToStr(LJSONobject.Field['status'].Value) + ': ' + VarToStr(LJSONobject.Field['message'].Value);
        end;
      finally
        LJSONobject.Free;
      end;
    except
      on E: Exception do
      begin
        ErrorMsg := 'The result from ' + GetName + ' was invaild: ' + E.Message;
      end;
    end;
  end;
end;

function TRelinkTo.EditFolder;
begin
  Result := False;
end;

function TRelinkTo.DeleteFolder;
begin
  Result := False;
end;

function TRelinkTo.GetFolder;
var
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
  LJSONobject: TlkJSONobject;

  LResponseStr: string;

  LSizeInBytes, LPartSizeInBytes, LBestPartSizeInBytes: Int64;
  LLinksIndex, LLinksUnknown, LLinksOnline, LLinksOffline: Integer;
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
  LBestPartSizeInBytes := 0;

  LLinksUnknown := 0;
  LLinksOnline := 0;
  LLinksOffline := 0;

  LRequestID := HTTPManager.Get(THTTPRequest.Create(WEBSITE + 'api/container_link_info.php' + '?id=' + GetFolderID(AFolderIdentifier)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else
  begin
    try
      LResponseStr := LHTTPProcess.HTTPResult.SourceCode;
      if (Pos('<pre>', LResponseStr) = 1) then // Workaround for stupid relink.to implementation ...
        LResponseStr := copy(LResponseStr, 6);
      try
        LJSONobject := TlkJSON.ParseText(LResponseStr) as TlkJSONobject;
        if (LJSONobject.IndexOfName('error') = -1) then
        begin
          for LLinksIndex := 0 to LJSONobject.Field['links'].Count - 1 do
          begin
            Inc(ACrypterFolderInfo.Parts);
            case IndexText(VarToStr(LJSONobject.Field['links'].Child[LLinksIndex].Field['status'].Value), ['unknown', 'online', 'offline']) of
              0:
                Inc(LLinksUnknown);
              1:
                Inc(LLinksOnline);
              2:
                Inc(LLinksOffline);
            end;
            LPartSizeInBytes := StrToIntDef(LJSONobject.Field['links'].Child[LLinksIndex].Field['size'].Value, 0);
            if LPartSizeInBytes > LBestPartSizeInBytes then
            begin
              LBestPartSizeInBytes := LPartSizeInBytes;
            end;
            Inc(LSizeInBytes, LPartSizeInBytes);
            ACrypterFolderInfo.Hoster := VarToStr(LJSONobject.Field['links'].Child[LLinksIndex].Field['hoster'].Value);
          end;
          if (LLinksUnknown = 0) and (LLinksOnline = 0) then
            ACrypterFolderInfo.Status := csOffline
          else if (LLinksUnknown = 0) and (LLinksOffline = 0) then
            ACrypterFolderInfo.Status := csOnline
          else if (LLinksOffline > 0) and (LLinksOnline > 0) then
            ACrypterFolderInfo.Status := csMixedOnOffline
          else
            ACrypterFolderInfo.Status := csUnknown;

          ACrypterFolderInfo.Link := AFolderIdentifier;
          ACrypterFolderInfo.Size := RoundTo((LSizeInBytes / 1048576), -2);
          ACrypterFolderInfo.PartSize := RoundTo((LBestPartSizeInBytes / 1048576), -2);
          ACrypterFolderInfo.StatusImage := GetStatusImageLink(ACrypterFolderInfo.Link);
          ACrypterFolderInfo.StatusImageText := GetStatusImageLink(ACrypterFolderInfo.Link, False);
          Result := True;
        end
        else
        begin
          ErrorMsg := LJSONobject.Field['error'].Value;
        end;
      finally
        LJSONobject.Free;
      end;
    except
      on E: Exception do
      begin
        ErrorMsg := 'The result from ' + GetName + ' was invaild: ' + E.Message;
      end;
    end;
  end;
end;

end.
