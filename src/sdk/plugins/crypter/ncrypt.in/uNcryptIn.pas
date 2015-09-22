unit uNcryptIn;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Math,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
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
    function GenerateFolder(MirrorController: IMirrorControl): WideString; override;
    function GetFolderInfo(FolderURL: WideString): TCrypterFolderInfo; override;
    procedure GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: WordBool = True); override;
  end;

implementation

{ TNcryptIn }

function TNcryptIn.GetName;
begin
  Result := 'nCrypt.in';
end;

function TNcryptIn.GenerateFolder;
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

    AddFormField('links', MirrorController.DirectlinksMirror[0]);

    for I := 1 to MirrorController.DirectlinksMirrorCount - 1 do
      AddFormField('mirror[]', MirrorController.DirectlinksMirror[I]);

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
    Result := copy(HTTPProcess.HTTPResult.SourceCode, 1, Pos(#$A, HTTPProcess.HTTPResult.SourceCode) - 1)
  else
    ErrorMsg := HTTPProcess.HTTPResult.SourceCode;
end;

function TNcryptIn.GetFolderInfo;
var
  CrypterFolderInfo: TCrypterFolderInfo;

  HTTPParams: IHTTPParams;

  RequestID: Double;

  HTTPProcess: IHTTPProcess;
begin
  with CrypterFolderInfo do
  begin
    Status := 255;
    Size := 0;
    Hoster := '';
    Parts := 0;
  end;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
    AddFormField('link', FolderURL);

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
            CrypterFolderInfo.Status := 1;
          1:
            CrypterFolderInfo.Status := 2;
          2:
            CrypterFolderInfo.Status := 0;
          3:
            CrypterFolderInfo.Status := 3;
          4:
            CrypterFolderInfo.Status := 4;
        else
          CrypterFolderInfo.Status := 255;
        end;

        CrypterFolderInfo.Hoster := Match[2];
        CrypterFolderInfo.Size := RoundTo((StrToInt64(Match[3]) / 1048576), -2);
        CrypterFolderInfo.Parts := StrToIntDef(Match[4], 0);
      end;
    finally
      Free;
    end;

  Result := CrypterFolderInfo;
end;

procedure TNcryptIn.GetFolderPicture;
begin
  Result := StringReplace(FolderURL, '/folder-', '/status-', []);
end;

end.
