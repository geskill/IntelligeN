unit uLinkcryptWs;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Variants, HTTPApp, XMLDoc, XMLIntf, ActiveX,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // plugin system
  uPlugInCrypterClass, uPlugInConst, uIdHTTPHelper;

type
  TLinkcryptWs = class(TCrypterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function GenerateFolder(MirrorController: IMirrorControl): WideString; override;
    function GetFolderInfo(FolderURL: WideString): TCrypterFolderInfo; override;
    procedure GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: Boolean = True); override; stdcall;
  end;

implementation

function TLinkcryptWs.GetName;
begin
  Result := 'Linkcrypt.ws';
end;

function TLinkcryptWs.GenerateFolder;
const
  website = 'http://linkcrypt.ws/';
var
  I: Integer;
  _params, _result: TStringStream;
  _Foldertypes: TFoldertypes;
  _Containertypes: TContainertypes;
  XMLDoc: IXMLDocument;
begin
  _Foldertypes := TFoldertypes(TFoldertype(Foldertypes));
  _Containertypes := TContainertypes(TContainertype(ContainerTypes));

  with TIdHTTPHelper.Create(Self) do
    try
      _params := TStringStream.Create('', CP_UTF8);
      _result := TStringStream.Create('', CP_UTF8);
      try
        _params.WriteString('urls=' + HTTPEncode(StringReplace(MirrorController.DirectlinksMirror[0], sLineBreak, ';', [rfReplaceAll])) + '&');

        for I := 1 to MirrorController.DirectlinksMirrorCount - 1 do
          _params.WriteString('mirror_' + IntToStr(I) + '=' + HTTPEncode(StringReplace(MirrorController.DirectlinksMirror[I], sLineBreak, ';', [rfReplaceAll])) + '&');

        if ftWeb in _Foldertypes then
          _params.WriteString('weburls=0&')
        else
          _params.WriteString('weburls=1&');

        (*
          if ftPlain in _Foldertypes then
          _params.WriteString('plainlinks=1&')
          else
          _params.WriteString('plainlinks=0&');
          *)
        if ftContainer in _Foldertypes then
        begin
          if ctDLC in _Containertypes then
            _params.WriteString('dlc=0&')
          else
            _params.WriteString('dlc=1&');
          if ctRSDF in _Containertypes then
            _params.WriteString('rsdf=0&')
          else
            _params.WriteString('rsdf=1&');
          if ctCCF in _Containertypes then
            _params.WriteString('ccf=0&')
          else
            _params.WriteString('ccf=1&');
        end
        else
        begin
          _params.WriteString('dlc=1&');
          _params.WriteString('rsdf=1&');
          _params.WriteString('ccf=1&');
        end;

        if UseCNL then
          _params.WriteString('cnl=0&')
        else
          _params.WriteString('cnl=1&');

        if UseCaptcha then
          _params.WriteString('captx=0&')
        else
          _params.WriteString('captx=1&');

        if not(FolderName = '') then
          _params.WriteString('title=' + HTTPEncode(FolderName) + '&');

        (*
          _params.WriteString('layer_id=' + HTTPEncode(AdvertismentLayerName) + '&');

          _params.WriteString('layer=' + HTTPEncode(AdvertismentLayerValue) + '&');

          if UseAdvertismentLink then
          _params.WriteString('bannerlink=' + HTTPEncode(AdvertismentLink) + '&');

          if UseAdvertismentPicture then
          _params.WriteString('banner=' + HTTPEncode(AdvertismentPicture) + '&');

          if UseCoverLink then
          _params.WriteString('cover=' + HTTPEncode(CoverLink) + '&');

          if UseDescription then
          _params.WriteString('info=' + HTTPEncode(Description) + '&');

          if UseWebseiteLink then
          _params.WriteString('homepage=' + HTTPEncode(WebseiteLink) + '&');

          if UseEMailforStatusNotice then
          _params.WriteString('offlineemail=' + HTTPEncode(EMailforStatusNotice) + '&');

          if UseAdminPassword then
          _params.WriteString('edit_password=' + HTTPEncode(AdminPassword) + '&');
          *)
        if UseFilePassword then
          _params.WriteString('download_password=' + HTTPEncode(FilePassword) + '&');

        if UseVisitorPassword then
          _params.WriteString('folder_password=' + HTTPEncode(Visitorpassword) + '&');

        if UseAccount then
          _params.WriteString('api_user_id=' + HTTPEncode(AccountName) + '&');

        _params.WriteString('api=create_V1');

        try
          Post(website + 'api.html', _params, _result);
        except

        end;

        OleInitialize(nil);
        try
          XMLDoc := NewXMLDocument;
          try
            with XMLDoc do
            begin
              LoadFromStream(_result);
              Active := True;
            end;
            with XMLDoc.ChildNodes.Nodes['data'].ChildNodes do
              if (Nodes['status'].NodeValue = '1') then
                Result := Nodes['folderUrl'].NodeValue
              else
                ErrorMsg := Nodes['errorCode'].NodeValue + ': ' + Nodes['errorMsg'].NodeValue;
          finally
            XMLDoc := nil;
          end;
        finally
          OleUninitialize;
        end;
      finally
        _result.Free;
        _params.Free;
      end;
    finally
      Free;
    end;
end;

function TLinkcryptWs.GetFolderInfo;
var
  _result: TStringStream;
  CrypterFolderInfo: TCrypterFolderInfo;
  FormatSettings: TFormatSettings;
  XMLDoc: IXMLDocument;

  function GetFolderID(AFolderURL: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := AFolderURL;
        Expression := 'dir\/(\w+)';

        if Exec(InputString) then
          Result := Match[1];
      finally
        Free;
      end;
  end;

begin
  with CrypterFolderInfo do
  begin
    Status := 255;
    Size := 0;
    Hoster := '';
    Parts := 0;
  end;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
  FormatSettings.DecimalSeparator := ',';

  with TIdHTTPHelper.Create(Self) do
    try
      _result := TStringStream.Create;
      try
        try
          Get('http://linkcrypt.ws/api.html?api=status_V1&detail=1&folderKey=' + GetFolderID(FolderURL), _result);
        except
          Exit;
        end;

        _result.Position := 0;

        OleInitialize(nil);
        try
          XMLDoc := NewXMLDocument;
          try
            with XMLDoc do
            begin
              LoadFromStream(_result);
              Active := True;
            end;
            with XMLDoc.ChildNodes.Nodes['data'].ChildNodes do
            begin
              case IndexText(VarToStr(Nodes['folderStatus'].NodeValue), ['1', '3', '2', '0']) of
                0:
                  CrypterFolderInfo.Status := 1;
                1:
                  CrypterFolderInfo.Status := 4;
                2:
                  CrypterFolderInfo.Status := 0;
                3:
                  CrypterFolderInfo.Status := 3;
              else
                CrypterFolderInfo.Status := 255;
              end;
              CrypterFolderInfo.Hoster := VarToStr(Nodes['folderHoster'].NodeValue);
              CrypterFolderInfo.Size := StrToFloatDef(StringReplace(VarToStr(Nodes['folderSize'].NodeValue), '.', ',', [rfReplaceAll]), 0, FormatSettings);
              CrypterFolderInfo.Parts := StrToIntDef(VarToStr(Nodes['fileCount'].NodeValue), 0);
            end;
          finally
            XMLDoc := nil;
          end;
        finally
          OleUninitialize;
        end;
      finally
        _result.Free;
      end;
    finally
      Free;
    end;
  Result := CrypterFolderInfo;
end;

procedure TLinkcryptWs.GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: Boolean);
begin
  case Small of
    True:
      Result := StringReplace(FolderURL, '/dir/', '/png/', []);
    False:
      Result := StringReplace(FolderURL, '/dir/', '/textpng/', []);
  end;
end;

end.
