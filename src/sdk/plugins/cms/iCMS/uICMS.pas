unit uICMS;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Controls, Variants, XMLDoc, XMLIntf, ActiveX,
  // Common
  uConst, uWebsiteInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInConst, uPlugInCMSClass, uPlugInHTTPClasses;

type
  TICMSSettings = class(TCMSPlugInSettings)

  end;

  TICMS = class(TCMSPlugIn)
  private
    ICMSSettings: TICMSSettings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;

    function NeedLogin: Boolean; override;
    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function DoBuildPostRequest(const AWebsiteData: ICMSWebsiteData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;
  public
    function GetName: WideString; override; safecall;
    function CMSType: TCMSType; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(AWebsiteSourceCode: WideString): WordBool; override; safecall;
    function GetIDs: Integer; override; safecall;
  end;

implementation

function TICMS.SettingsClass;
begin
  Result := TICMSSettings;
end;

function TICMS.GetSettings;
begin
  Result := ICMSSettings;
end;

procedure TICMS.SetSettings;
begin
  ICMSSettings := ACMSPlugInSettings as TICMSSettings;
end;

function TICMS.NeedLogin;
begin
  Result := False;
end;

function TICMS.DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean): Boolean;
begin
  Result := True; // function not needed, because no login needed
end;

function TICMS.DoAnalyzeLogin(AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean;
begin
  Result := True; // function not needed, because no login needed
end;

function TICMS.DoBuildPostRequest;
var
  I, J: Integer;
  MirrorPrefix, MirrorDirectlinkPrefix, MirrorCrypterPrefix: string;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'icms_add_entry.php');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := ICMSSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with AHTTPParams do
  begin
    if not(AccountName = '') then
    begin
      AddFormField('account_username', AccountName);
      AddFormField('account_password', AccountPassword);
    end;

    AddFormField('action', 'add_entry');

    AddFormField('IType', TTemplateTypeIDToString(AWebsiteData.TemplateTypeID));
    AddFormField('ISubject', Subject);
    AddFormField('ITags', Tags);
    AddFormField('IMessage', Message);

    for I := 0 to AWebsiteData.ControlCount - 1 do
      with AWebsiteData.Control[I] do
        AddFormField(TComponentIDToString(ComponentID), Value);

    AddFormField('MirrorCount', IntToStr(AWebsiteData.MirrorCount));
    for I := 0 to AWebsiteData.MirrorCount - 1 do
      with AWebsiteData.Mirror[I] do
      begin
        MirrorPrefix := 'Mirror_' + IntToStr(I) + '_';
        AddFormField(MirrorPrefix + 'Size', FloatToStr(Size));
        AddFormField(MirrorPrefix + 'PartSize', FloatToStr(PartSize));
        AddFormField(MirrorPrefix + 'Hoster', Hoster);
        AddFormField(MirrorPrefix + 'HosterShort', HosterShort);
        AddFormField(MirrorPrefix + 'Parts', IntToStr(Parts));

        AddFormField(MirrorPrefix + 'DirectlinkCount', IntToStr(DirectlinkCount));
        for J := 0 to DirectlinkCount - 1 do
        begin
          MirrorDirectlinkPrefix := 'Mirror_' + IntToStr(I) + '_Directlink_' + IntToStr(J) + '_';
          AddFormField(MirrorDirectlinkPrefix + 'Value', Directlink[J].Value);
          AddFormField(MirrorDirectlinkPrefix + 'Size', FloatToStr(Directlink[J].Size));
          AddFormField(MirrorDirectlinkPrefix + 'PartSize', FloatToStr(Directlink[J].PartSize));
          AddFormField(MirrorDirectlinkPrefix + 'Hoster', Directlink[J].Hoster);
          AddFormField(MirrorDirectlinkPrefix + 'HosterShort', Directlink[J].HosterShort);
          AddFormField(MirrorDirectlinkPrefix + 'Parts', IntToStr(Directlink[J].Parts));
        end;

        AddFormField(MirrorPrefix + 'DirectlinksMirrorCount', IntToStr(DirectlinkCount)); // for compatibility
        for J := 0 to DirectlinkCount - 1 do // for compatibility
          AddFormField(MirrorPrefix + 'DirectlinksMirror_' + IntToStr(J), Directlink[J].Value); // for compatibility

        AddFormField(MirrorPrefix + 'CrypterCount', IntToStr(CrypterCount));
        for J := 0 to CrypterCount - 1 do
        begin
          MirrorCrypterPrefix := 'Mirror_' + IntToStr(I) + '_Crypter_' + IntToStr(J) + '_';
          AddFormField(MirrorCrypterPrefix + 'Name', Crypter[J].Name);
          AddFormField(MirrorCrypterPrefix + 'Link', Crypter[J].Value); // for compatibility
          AddFormField(MirrorCrypterPrefix + 'Value', Crypter[J].Value);
          AddFormField(MirrorCrypterPrefix + 'Size', FloatToStr(Crypter[J].Size));
          AddFormField(MirrorCrypterPrefix + 'Hoster', Crypter[J].Hoster);
          AddFormField(MirrorCrypterPrefix + 'HosterShort', Crypter[J].HosterShort);
          AddFormField(MirrorCrypterPrefix + 'Parts', IntToStr(Crypter[J].Parts));
          AddFormField(MirrorCrypterPrefix + 'StatusImage', Crypter[J].StatusImage);
          AddFormField(MirrorCrypterPrefix + 'StatusImageText', Crypter[J].StatusImageText);
        end;
      end;
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TICMS.DoAnalyzePost;
var
  XMLDoc: IXMLDocument;
begin
  Result := False;
  try
    OleInitialize(nil);
    try
      XMLDoc := NewXMLDocument;
      try
        with XMLDoc do
        begin
          LoadFromXML(AResponseStr);
          Active := True;
        end;
        with XMLDoc.DocumentElement do
          if HasChildNodes then
          begin
            with ChildNodes.Nodes['code'] do
              Result := NodeValue;
            with ChildNodes.Nodes['msg'] do
              ErrorMsg := VarToWideStr(NodeValue);
          end;
      finally
        XMLDoc := nil;
      end;
    finally
      OleUninitialize;
    end;
  except
    ErrorMsg := 'error parsing ICMS-RESULT-XML (' + SysErrorMessage(GetLastError()) + ')';
  end;
end;

function TICMS.GetName;
begin
  Result := 'ICMS';
end;

function TICMS.CMSType;
begin
  Result := cmsFormbased;
end;

function TICMS.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TICMS.BelongsTo;
begin
  Result := False;
end;

function TICMS.GetIDs;
begin
  Result := 0;
end;

end.
