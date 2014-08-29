unit uNcryptIn;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Variants, HTTPApp, Math,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // plugin system
  uPlugInCrypterClass, uPlugInConst, uIdHTTPHelper;

type
  TNcryptIn = class(TCrypterPlugIn)
  private const
    website = 'http://ncrypt.in/';
  public
    function GetName: WideString; override; safecall;
    function GenerateFolder(MirrorController: IMirrorControl): WideString; override;
    function GetFolderInfo(FolderURL: WideString): TCrypterFolderInfo; override;
    procedure GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: Boolean = True); override; stdcall;
  end;

implementation

{ TNcryptIn }

function TNcryptIn.GetName: WideString;
begin
  Result := 'nCrypt.in';
end;

function TNcryptIn.GenerateFolder(MirrorController: IMirrorControl): WideString;
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
        if UseAccount then
          _params.WriteString('auth_code=' + (AccountName) + '&');

        _params.WriteString('links=' + (MirrorController.DirectlinksMirror[0]) + '&');

        for I := 1 to MirrorController.DirectlinksMirrorCount - 1 do
          _params.WriteString(('mirror[]') + '=' + (MirrorController.DirectlinksMirror[I]) + '&');

        _params.WriteString('show_mirrors=0&');

        if ftWeb in _Foldertypes then
          _params.WriteString('show_links=1&')
        else
          _params.WriteString('show_links=0&');

        if ftContainer in _Foldertypes then
        begin
          _params.WriteString('show_container=1&');
          if ctCCF in _Containertypes then
            _params.WriteString('ccf=1&')
          else
            _params.WriteString('ccf=0&');
          if ctDLC in _Containertypes then
            _params.WriteString('dlc=1&')
          else
            _params.WriteString('dlc=0&');
          if ctRSDF in _Containertypes then
            _params.WriteString('rsdf=1&')
          else
            _params.WriteString('rsdf=0&');
        end
        else
          _params.WriteString('show_container=0&');

        if UseCNL then
          _params.WriteString('cnl=1&')
        else
          _params.WriteString('cnl=0&');

        if UseCaptcha then
          _params.WriteString('captcha=1&')
        else
          _params.WriteString('captcha=0&');

        // if not(FolderName = '') then
        _params.WriteString('foldername=' + (FolderName) + '&');

        if UseCoverLink then
          _params.WriteString('image=' + (CoverLink) + '&');

        if UseDescription then
          _params.WriteString('description=' + (Description) + '&');

        if UseEMailforStatusNotice then
        begin
          _params.WriteString('notify_adress=' + (EMailforStatusNotice) + '&');
        end;

        if UseVisitorPassword then
          _params.WriteString('password=' + (Visitorpassword) + '&');

        try
          Post(website + 'api.php', _params, _result);
        except

        end;

        // if (Pos('ERROR', _result.DataString) = 0) then
        if not(Pos('ncrypt.in', _result.DataString) = 0) then
          Result := copy(_result.DataString, 1, Pos(#$A, _result.DataString) - 1)
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

function TNcryptIn.GetFolderInfo(FolderURL: WideString): TCrypterFolderInfo;
var
  _params, _result: TStringStream;
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
      _params := TStringStream.Create('', CP_UTF8);
      _result := TStringStream.Create('', CP_UTF8);
      try
        _params.WriteString('link=' + HTTPEncode(FolderURL) + '&');

        try
          Post(website + 'api_status.php', _params, _result);
        except

        end;

        with TRegExpr.Create do
          try
            InputString := _result.DataString;
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
      finally
        _result.Free;
        _params.Free;
      end;

      Result := CrypterFolderInfo;
    finally
      Free;
    end;
end;

procedure TNcryptIn.GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: Boolean);
begin
  Result := StringReplace(FolderURL, '/folder-', '/status-', []);
end;

end.
