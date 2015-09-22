unit uLinksaveIn;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, HTTPApp,
  // Common
  uConst, uAppInterface,
  // Utils
  uPathUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TLinksaveIn = class(TCrypterPlugIn)
  public
    function GetName: WideString; override;
    function GenerateFolder(MirrorController: IMirrorControl): WideString; override;
    function GetFolderInfo(FolderURL: WideString): TCrypterFolderInfo; override;
    procedure GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: WordBool = True); override;
  end;

implementation

function TLinksaveIn.GetName;
begin
  Result := 'Linksave.in';
end;

function TLinksaveIn.GenerateFolder;
const
  curl = 'http://linksave.in/';
  purl = curl + 'protect?api=';
  werbung: array [0 .. 2] of string = ('layer', 'mirror', 'banner');
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
    AddFormField('links', MirrorController.DirectlinksMirror[0]);

    for I := 1 to MirrorController.DirectlinksMirrorCount - 1 do
      AddFormField('mirrors[]', MirrorController.DirectlinksMirror[I]);

    if _Foldertypes = [ftWeb, ftContainer] then
      AddFormField('myschutz', 'container_js')
    else if _Foldertypes = [ftPlain, ftContainer] then
      AddFormField('myschutz', 'container_plain')
    else if _Foldertypes = [ftContainer] then
      AddFormField('myschutz', 'container')
    else if _Foldertypes = [ftPlain] then
      AddFormField('myschutz', 'plain')
    else if _Foldertypes = [ftWeb] then
      AddFormField('myschutz', 'js');

    if ftContainer in _Foldertypes then
    begin
      if ctCCF in _Containertypes then
        AddFormField('container_ccf', '1')
      else
        AddFormField('container_ccf', '0');
      if ctDLC in _Containertypes then
        AddFormField('container_dlc', '1')
      else
        AddFormField('container_dlc', '0');
      if ctRSDF in _Containertypes then
        AddFormField('container_rsdf', '1')
      else
        AddFormField('container_rsdf', '0');
    end;

    if UseCNL then
      AddFormField('container_cnl', '1')
    else
      AddFormField('container_cnl', '0');

    if UseCaptcha then
      AddFormField('captcha', 'an')
    else
      AddFormField('captcha', 'aus');

    if not(FolderName = '') then
      AddFormField('ordnername', copy(FolderName, 1, 100));

    AddFormField('werbung', werbung[AdvertismentType]);
    case AdvertismentType of
      1:
        if UseAdvertismentLink then
          AddFormField('mirror', AdvertismentLink);
      2:
        if UseAdvertismentLink then
          AddFormField('bannerlink', AdvertismentLink);
    end;
    if UseAdvertismentPicture then
      AddFormField('bannerurl', AdvertismentPicture);

    if UseCoverLink then
      AddFormField('cover', CoverLink);

    if UseDescription then
      AddFormField('beschreibung', Description);

    if UseWebseiteLink then
      AddFormField('website', WebseiteLink);

    if UseEMailforStatusNotice then
    begin
      AddFormField('notify', '1');
      AddFormField('email', EMailforStatusNotice);
    end;

    if UseAdminPassword then
      AddFormField('adminpasswort', AdminPassword);

    if UseVisitorPassword then
      AddFormField('besucherpasswort', Visitorpassword);

    AddFormField('protect', 'protect');
  end;

  case UseAccount of
    True:
      RequestID := HTTPManager.Post(THTTPRequest.Create(purl + HTTPEncode(AccountName) + ':' + HTTPEncode(AccountPassword)), HTTPParams, TPlugInHTTPOptions.Create(Self));

    False:
      RequestID := HTTPManager.Post(THTTPRequest.Create(purl + 'TRUE'), HTTPParams, TPlugInHTTPOptions.Create(Self));
  end;

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  HTTPProcess := HTTPManager.GetResult(RequestID);

  if HTTPProcess.HTTPResult.HasError then
    ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage
  else if (Pos('ERROR', string(HTTPProcess.HTTPResult.SourceCode)) = 0) then
    Result := curl + HTTPProcess.HTTPResult.SourceCode
  else
    ErrorMsg := HTTPProcess.HTTPResult.SourceCode;
end;

function TLinksaveIn.GetFolderInfo;
var
  CrypterFolderInfo: TCrypterFolderInfo;

  RequestID: Double;

  HTTPProcess: IHTTPProcess;

  FormatSettings: TFormatSettings;
  StringList: TStringList;
begin
  with CrypterFolderInfo do
  begin
    Status := 255;
    Size := 0;
    Hoster := '';
    Parts := 0;
  end;

  // thread-safe ???
  GetLocaleFormatSettings(GetThreadLocale, FormatSettings);
  FormatSettings.DecimalSeparator := '.';

  RequestID := HTTPManager.Get(THTTPRequest.Create(FolderURL + '/api-more'), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  HTTPProcess := HTTPManager.GetResult(RequestID);

  StringList := TStringList.Create;
  try
    StringList.Text := HTTPProcess.HTTPResult.SourceCode;

    { api-more
      Ordnerstatus / Folderstatus
      Containerarten (getrennt mit "|") / Containertypes (divided by "|")
      Hostername (ohne TLD) / Hostname (without TLD)
      Ordnergröße (in MB) / Foldersize (in MB)
      Dateiname des ersten Links (ohne Dateiendung) / Filename of the first link (without extension)
      Anzahl der Links / Linkcount. }

    // IdHTTP.Get(FolderURL +'/api-ordnername');

    if (StringList.Count > 4) then
    begin
      case IndexText(StringList[0], ['online', 'unknown', 'offline', 'notyet', '']) of
        0:
          CrypterFolderInfo.Status := 1;
        1:
          CrypterFolderInfo.Status := 2;
        2:
          CrypterFolderInfo.Status := 0;
        3, 4:
          CrypterFolderInfo.Status := 3;
      else
        CrypterFolderInfo.Status := 255;
      end;

      CrypterFolderInfo.Hoster := StringList[2];

      CrypterFolderInfo.Size := StrToFloatDef(StringList[3], 0, FormatSettings);

      CrypterFolderInfo.Parts := StrToIntDef(StringList[5], 0);
    end;
  finally
    StringList.Free;
  end;

  Result := CrypterFolderInfo;
end;

procedure TLinksaveIn.GetFolderPicture;
begin
  case Small of
    True:
      Result := ExcludeTrailingUrlDelimiter(FolderURL) + '.png';
    False:
      Result := ExcludeTrailingUrlDelimiter(FolderURL) + '-t.png';
  end;
end;

end.
