unit u%FullName%;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Math,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInInterface, uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uVariantUtils;

type
  T%FullName% = class(TCrypterPlugIn)
  protected { . }
  const
    WEBSITE = '%Website%';

    function GetFolderID(AFolderURL: string): string;
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

function T%FullName%.GetFolderID;
begin
  { TODO : implement? }
  Result := '';
end;

function T%FullName%.GetAuthor;
begin
  Result := '%CompanyName%';
end;

function T%FullName%.GetAuthorURL;
begin
  Result := 'http://example.com/';
end;

function T%FullName%.GetDescription;
begin
  Result := GetName + ' crypter plug-in.';
end;

function T%FullName%.GetName;
begin
  { TODO : change name? }
  Result := '%FullName%';
end;

function T%FullName%.GetServiceRequiresAccess;
begin
  { TODO : change? }
  Result := caAPIKey;
end;

function T%FullName%.AddFolder;
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
    { TODO : update example parameters }
    if ACrypterData.UseAccount then
      AddFormField('auth_code', ACrypterData.AccountName);

    AddFormField('foldername', ACrypterData.FolderName);

    AddFormField('links', AMirrorContainer.Directlink[0].Value);

    for LDirectlinkIndex := 1 to AMirrorContainer.DirectlinkCount - 1 do
      AddFormField('mirror[]', AMirrorContainer.Directlink[LDirectlinkIndex].Value);

    AddFormField('show_mirrors', '0');

    AddFormField('show_links', IfThen(ftWeb in LFoldertypes, '1', '0'));

    AddFormField('show_container', IfThen(ftContainer in LFoldertypes, '1', '0'));

    if ftContainer in LFoldertypes then
    begin
      AddFormField('ccf', IfThen(ctCCF in LContainertypes, '1', '0'));

      AddFormField('dlc', IfThen(ctDLC in LContainertypes, '1', '0'));

      AddFormField('rsdf', IfThen(ctRSDF in LContainertypes, '1', '0'));
    end;

    AddFormField('cnl', IfThen(ACrypterData.UseCNL, '1', '0'));

    AddFormField('captcha', IfThen(ACrypterData.UseCaptcha, '1', '0'));

    if ACrypterData.UseVisitorPassword then
      AddFormField('password', ACrypterData.Visitorpassword);

    if ACrypterData.UseDescription then
      AddFormField('description', ACrypterData.Description);

    if ACrypterData.UseCoverLink then
      AddFormField('image', ACrypterData.CoverLink);

    if ACrypterData.UseEMailforStatusNotice then
    begin
      AddFormField('notify_adress', ACrypterData.EMailforStatusNotice);
    end;
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(website + 'TODO_URL-ADD-FOLDER-REQUEST'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else if not(Pos('TODO_OK-VALUE', string(LHTTPProcess.HTTPResult.SourceCode)) = 0) then
  begin
    { TODO : implement the evaluation of the request result }
    ACrypterFolderInfo.Link := '';
    Result := True;
  end
  else
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.SourceCode;
  end;
end;

function T%FullName%.EditFolder;
begin
  Result := False;
end;

function T%FullName%.DeleteFolder;
begin
  Result := False;
end;

function T%FullName%.GetFolder;
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
    { TODO : update example parameters }
    AddFormField('link', AFolderIdentifier);
  end;

  { TODO : update the example code here }
  LRequestID := HTTPManager.Post(THTTPRequest.Create(website + 'TODO_URL-GET-FOLDER-INFO-REQUEST'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else
  begin
    { TODO : implement the evaluation of the request result }
  end;
end;

end.
