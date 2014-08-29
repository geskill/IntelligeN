unit uLinksaveIn;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Variants, HTTPApp,
  // Common
  uConst, uAppInterface,
  // Utils
  uPathUtils,
  // plugin system
  uPlugInCrypterClass, uPlugInConst, uIdHTTPHelper;

type
  TLinksaveIn = class(TCrypterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function GenerateFolder(MirrorController: IMirrorControl): WideString; override;
    function GetFolderInfo(FolderURL: WideString): TCrypterFolderInfo; override;
    procedure GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: Boolean = True); override; stdcall;
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
  _params, _result: TStringStream;
  _Foldertypes: TFoldertypes;
  _Containertypes: TContainertypes;
  I: Integer;
begin
  _Foldertypes := TFoldertypes(TFoldertype(Foldertypes));
  _Containertypes := TContainertypes(TContainertype(ContainerTypes));

  with TIdHTTPHelper.Create(Self) do
    try
      _params := TStringStream.Create('', CP_UTF8);
      _result := TStringStream.Create('', CP_UTF8);
      try
        _params.WriteString('links=' + HTTPEncode(MirrorController.DirectlinksMirror[0]) + '&');

        for I := 1 to MirrorController.DirectlinksMirrorCount - 1 do
          _params.WriteString('mirrors[]=' + HTTPEncode(MirrorController.DirectlinksMirror[I]) + '&');

        if _Foldertypes = [ftWeb, ftContainer] then
          _params.WriteString('myschutz=container_js&')
        else if _Foldertypes = [ftPlain, ftContainer] then
          _params.WriteString('myschutz=container_plain&')
        else if _Foldertypes = [ftContainer] then
          _params.WriteString('myschutz=container&')
        else if _Foldertypes = [ftPlain] then
          _params.WriteString('myschutz=plain&')
        else if _Foldertypes = [ftWeb] then
          _params.WriteString('myschutz=js&');

        if ftContainer in _Foldertypes then
        begin
          if ctCCF in _Containertypes then
            _params.WriteString('container_ccf=1&')
          else
            _params.WriteString('container_ccf=0&');
          if ctDLC in _Containertypes then
            _params.WriteString('container_dlc=1&')
          else
            _params.WriteString('container_dlc=0&');
          if ctRSDF in _Containertypes then
            _params.WriteString('container_rsdf=1&')
          else
            _params.WriteString('container_rsdf=0&');
        end;

        if UseCNL then
          _params.WriteString('container_cnl=1&')
        else
          _params.WriteString('container_cnl=0&');

        if UseCaptcha then
          _params.WriteString('captcha=an&')
        else
          _params.WriteString('captcha=aus&');

        if not(FolderName = '') then
          _params.WriteString('ordnername=' + HTTPEncode(copy(FolderName, 1, 100)) + '&');

        _params.WriteString('werbung=' + HTTPEncode(werbung[AdvertismentType]) + '&');
        case AdvertismentType of
          1:
            if UseAdvertismentLink then
              _params.WriteString('mirror=' + HTTPEncode(AdvertismentLink) + '&');
          2:
            if UseAdvertismentLink then
              _params.WriteString('bannerlink=' + HTTPEncode(AdvertismentLink) + '&');
        end;
        if UseAdvertismentPicture then
          _params.WriteString('bannerurl=' + HTTPEncode(AdvertismentPicture) + '&');

        if UseCoverLink then
          _params.WriteString('cover=' + HTTPEncode(CoverLink) + '&');

        if UseDescription then
          _params.WriteString('beschreibung=' + HTTPEncode(Description) + '&');

        if UseWebseiteLink then
          _params.WriteString('website=' + HTTPEncode(WebseiteLink) + '&');

        if UseEMailforStatusNotice then
        begin
          _params.WriteString('notify=1&');
          _params.WriteString('email=' + HTTPEncode(EMailforStatusNotice) + '&');
        end;

        if UseAdminPassword then
          _params.WriteString('adminpasswort=' + HTTPEncode(AdminPassword) + '&');

        if UseVisitorPassword then
          _params.WriteString('besucherpasswort=' + HTTPEncode(Visitorpassword) + '&');

        _params.WriteString('protect=protect');

        try
          ///
          // Get('http://linksave.in/');
          // Request.CustomHeaders.Clear;
          // Request.CustomHeaders.Add('Cookie: ' + Response.RawHeaders.Values['Set-Cookie'] + ';');
          ///
          case UseAccount of
            True:
              Post(purl + HTTPEncode(AccountName) + ':' + HTTPEncode(AccountPassword), _params, _result);
            False:
              Post(purl + 'TRUE', _params, _result);
          end;
        except

        end;

        if (Pos('ERROR', _result.DataString) = 0) then
          Result := curl + _result.DataString
        else
          ErrorMsg := _result.DataString;
      finally
        _result.Free;
        _params.Free;
      end;
    finally
      Free;
    end;
end;

function TLinksaveIn.GetFolderInfo;
var
  StringList: TStringList;
  CrypterFolderInfo: TCrypterFolderInfo;
  FormatSettings: TFormatSettings;
begin
  with CrypterFolderInfo do
  begin
    Status := 255;
    Size := 0;
    Hoster := '';
    Parts := 0;
  end;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
  FormatSettings.DecimalSeparator := '.';

  with TIdHTTPHelper.Create(Self) do
    try
      StringList := TStringList.Create;
      try
        try
          ///
          // StringList.Text := Get('http://linksave.in/');
          // Request.CustomHeaders.Clear;
          // Request.CustomHeaders.Add('Cookie: ' + Response.RawHeaders.Values['Set-Cookie'] + ';');
          ///
          StringList.Text := Get(FolderURL + '/api-more');
        except
          Exit;
        end;

        { api-more
          Ordnerstatus / Folderstatus
          Containerarten (getrennt mit "|") / Containertypes (divided by "|")
          Hostername (ohne TLD) / Hostname (without TLD)
          Ordnergröße (in MB) / Foldersize (in MB)
          Dateiname des ersten Links (ohne Dateiendung) / Filename of the first link (without extension)
          Anzahl der Links / Linkcount. }

        // IdHTTP.Get(FolderURL +'/api-ordnername');

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

        Result := CrypterFolderInfo;
      finally
        StringList.Free;
      end;
    finally
      Free;
    end;
end;

procedure TLinksaveIn.GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: Boolean);
begin
  case Small of
    True:
      Result := ExcludeTrailingUrlDelimiter(FolderURL) + '.png';
    False:
      Result := ExcludeTrailingUrlDelimiter(FolderURL) + '-t.png';
  end;
end;

end.
