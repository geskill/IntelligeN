unit uShareLinksBiz;

interface

uses
  // Delphi
  Windows, SysUtils, Classes,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TShareLinksBiz = class(TCrypterPlugIn)
  private const
    website = 'http://share-links.biz/';
  public
    function GetName: WideString; override;
    function GenerateFolder(MirrorController: IMirrorControl): WideString; override;
    function GetFolderInfo(FolderURL: WideString): TCrypterFolderInfo; override;
    procedure GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: WordBool = True); override;
  end;

implementation

function TShareLinksBiz.GetName;
begin
  Result := 'Share-Links.biz';
end;

function TShareLinksBiz.GenerateFolder;
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
      AddFormField('apikey', AccountName);

    if not(FolderName = '') then
      AddFormField('folderName', FolderName);

    AddFormField('links', MirrorController.DirectlinksMirror[0]);

    if MirrorController.DirectlinksMirrorCount > 1 then
    begin
      AddFormField('backup', '1');
      AddFormField('backup_mode', '0');
      for I := 1 to MirrorController.DirectlinksMirrorCount - 1 do
      begin
        if I = 10 then
          break;

        AddFormField('blinks' + IntToStr(I), MirrorController.DirectlinksMirror[I]);
      end;
    end
    else
      AddFormField('backup', '0');

    if UseCaptcha then
      AddFormField('captcha', '1')
    else
      AddFormField('captcha', '0');

    if UseVisitorPassword then
      AddFormField('pass_user', Visitorpassword);

    if UseAdminPassword then
      AddFormField('pass_admin', AdminPassword);

    if ftWeb in _Foldertypes then
      AddFormField('c_web', '1')
    else
      AddFormField('c_web', '0');

    if ftContainer in _Foldertypes then
    begin
      if ctCCF in _Containertypes then
        AddFormField('c_ccf', '1')
      else
        AddFormField('c_ccf', '0');
      if ctDLC in _Containertypes then
        AddFormField('c_dlc', '1')
      else
        AddFormField('c_dlc', '0');
      if ctRSDF in _Containertypes then
        AddFormField('c_rsdf', '1')
      else
        AddFormField('c_rsdf', '0');
    end;

    if UseCNL then
      AddFormField('c_cnl', '1')
    else
      AddFormField('c_cnl', '0');

    if UseDescription then
      AddFormField('comment', Description);
  end;

  RequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api/insert'), HTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  HTTPProcess := HTTPManager.GetResult(RequestID);

  if HTTPProcess.HTTPResult.HasError then
    ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage
  else if (Pos('***', string(HTTPProcess.HTTPResult.SourceCode)) = 0) then
    Result := copy(HTTPProcess.HTTPResult.SourceCode, 6)
  else
    ErrorMsg := HTTPProcess.HTTPResult.SourceCode;
end;

function TShareLinksBiz.GetFolderInfo;
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
    AddFormField('folderCodes', copy(FolderURL, Pos('/_', string(FolderURL)) + 1));

  RequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api/content'), HTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  HTTPProcess := HTTPManager.GetResult(RequestID);

  // handling the result is impossible because no information is send without api key,
  // but api key is for folder status check unavailable

  // see: http://the-lounge.org/share-links-vorschläge-wünsche/42811-kleine-erweiterungen-an-der-api/

  Result := CrypterFolderInfo;
end;

procedure TShareLinksBiz.GetFolderPicture;
begin
  // A folder-status-is not possible because the link is not connected to the folder URL
  case Small of
    True:
      Result := FolderURL;
    False:
      Result := FolderURL;
  end;
end;

end.
