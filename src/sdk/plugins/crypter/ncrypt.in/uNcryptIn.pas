unit uNcryptIn;

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
  // plugin system
  uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TNcryptIn = class(TCrypterPlugIn)
  private const
    website = 'http://ncrypt.in/';
  public
    function GetName: WideString; override;
    function AddFolder(const AMirrorContainer: IDirectlinkContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function EditFolder(const AMirrorContainer: IDirectlinkContainer; var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function DeleteFolder(AFolderIdentifier: WideString): WordBool; override; safecall;
    function GetFolder(AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
  end;

implementation

{ TNcryptIn }

function TNcryptIn.GetName;
begin
  Result := 'nCrypt.in';
end;

function TNcryptIn.AddFolder;
var
  _Foldertypes: TFoldertypes;
  _Containertypes: TContainertypes;

  I: Integer;

  HTTPParams: IHTTPParams;

  RequestID: Double;

  HTTPProcess: IHTTPProcess;
begin
  _Foldertypes := TFoldertypes(TFoldertype(Foldertypes));
  _Containertypes := TContainertypes(TContainertype(ContainerTypes));

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    if UseAccount then
      AddFormField('auth_code', AccountName);

    AddFormField('links', AMirrorContainer.Directlink[0].Value);

    for I := 1 to AMirrorContainer.DirectlinkCount - 1 do
      AddFormField('mirror[]', AMirrorContainer.Directlink[I].Value);

    AddFormField('show_mirrors', '0');

    if ftWeb in _Foldertypes then
      AddFormField('show_links', '1')
    else
      AddFormField('show_links', '0');

    if ftContainer in _Foldertypes then
    begin
      AddFormField('show_container', '1');
      if ctCCF in _Containertypes then
        AddFormField('ccf', '1')
      else
        AddFormField('ccf', '0');
      if ctDLC in _Containertypes then
        AddFormField('dlc', '1')
      else
        AddFormField('dlc', '0');
      if ctRSDF in _Containertypes then
        AddFormField('rsdf', '1')
      else
        AddFormField('rsdf', '0');
    end
    else
      AddFormField('show_container', '0');

    if UseCNL then
      AddFormField('cnl', '1')
    else
      AddFormField('cnl', '0');

    if UseCaptcha then
      AddFormField('captcha', '1')
    else
      AddFormField('captcha', '0');

    // if not(FolderName = '') then
    AddFormField('foldername', FolderName);

    if UseCoverLink then
      AddFormField('image', CoverLink);

    if UseDescription then
      AddFormField('description', Description);

    if UseEMailforStatusNotice then
    begin
      AddFormField('notify_adress', EMailforStatusNotice);
    end;

    if UseVisitorPassword then
      AddFormField('password', Visitorpassword);
  end;

  RequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api.php'), HTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  HTTPProcess := HTTPManager.GetResult(RequestID);

  if HTTPProcess.HTTPResult.HasError then
    ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage
  else if not(Pos('ncrypt.in', string(HTTPProcess.HTTPResult.SourceCode)) = 0) then
    ErrorMsg := copy(HTTPProcess.HTTPResult.SourceCode, 1, Pos(#$A, HTTPProcess.HTTPResult.SourceCode) - 1)
  else
    ErrorMsg := HTTPProcess.HTTPResult.SourceCode;
end;

function TNcryptIn.EditFolder;
begin
  //
end;

function TNcryptIn.DeleteFolder;
begin
  //
end;

function TNcryptIn.GetFolder;
var
  CrypterFolderInfo: TCrypterFolderInfo;

  HTTPParams: IHTTPParams;

  RequestID: Double;

  HTTPProcess: IHTTPProcess;
begin
  with CrypterFolderInfo do
  begin
    Status := csNotChecked;
    Size := 0;
    Hoster := '';
    Parts := 0;
  end;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
    AddFormField('link', AFolderIdentifier);

  RequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api_status.php'), HTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  HTTPProcess := HTTPManager.GetResult(RequestID);

  with TRegExpr.Create do
    try
      InputString := HTTPProcess.HTTPResult.SourceCode;
      Expression := '(.*?);(.*?);(\d+);(\d+)';

      if Exec(InputString) then
      begin

        case IndexText(Match[1], ['online', 'unknown', 'offline', 'unchecked', 'partly_offline']) of
          0:
            CrypterFolderInfo.Status := csOnline;
          1:
            CrypterFolderInfo.Status := csUnknown;
          2:
            CrypterFolderInfo.Status := csOffline;
          3:
            CrypterFolderInfo.Status := csNotChecked;
          4:
            CrypterFolderInfo.Status := csMixedOnOffline;
        else
          CrypterFolderInfo.Status := csNotChecked;
        end;

        CrypterFolderInfo.Hoster := Match[2];
        CrypterFolderInfo.Size := RoundTo((StrToInt64(Match[3]) / 1048576), -2);
        CrypterFolderInfo.Parts := StrToIntDef(Match[4], 0);
      end;
    finally
      Free;
    end;

    // Result := StringReplace(FolderURL, '/folder-', '/status-', []);

  Result := False; // TODO
end;

end.
