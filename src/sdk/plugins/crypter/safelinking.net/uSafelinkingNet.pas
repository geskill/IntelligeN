unit uSafelinkingNet;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Math, HTTPApp, Variants, XMLDoc, XMLIntf, ActiveX,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uVariantUtils;

type
  Tsafelinkingnet = class(TCrypterPlugIn)
  protected { . }
  const
    WEBSITE = 'http://safelinking.net/';
  public
    function GetName: WideString; override; safecall;

    function AddFolder(const AMirrorContainer: IDirectlinkContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function EditFolder(const AMirrorContainer: IDirectlinkContainer; var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function DeleteFolder(AFolderIdentifier: WideString): WordBool; override; safecall;
    function GetFolder(AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
  end;

implementation

function Tsafelinkingnet.GetName;
begin
  Result := 'Safelinking.net';
end;

function Tsafelinkingnet.AddFolder;
var
  LNeedToUninitialize: Boolean;
  LFoldertypes: TFoldertypes;
  LContainertypes: TContainertypes;

  LDirectlinkIndex: Integer;

  LHTTPParams: IHTTPParams;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;

  LXMLDoc: IXMLDocument;
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

  LFoldertypes := TFoldertypes(TFoldertype(Foldertypes));
  LContainertypes := TContainertypes(TContainertype(ContainerTypes));

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    if UseAccount then
    begin
      AddFormField('username', AccountName);
      AddFormField('api_hash', AccountPassword);
    end;

    AddFormField('link-title', FolderName);

    AddFormField('links-to-protect', AMirrorContainer.Directlink[0].Value);

    for LDirectlinkIndex := 1 to AMirrorContainer.DirectlinkCount - 1 do
      AddFormField('mirror' + IntToStr(LDirectlinkIndex), AMirrorContainer.Directlink[LDirectlinkIndex].Value);

    AddFormField('individual-links', 'no');

    AddFormField('links-per-mirror', 'no');

    AddFormField('show-short-links', 'no');

    if ftContainer in LFoldertypes then
    begin
      AddFormField('rtc', IfThen(ctCCF in LContainertypes, 'on', 'no'));

      AddFormField('dlc', IfThen(ctDLC in LContainertypes, 'on', 'no'));

      AddFormField('rsdf', IfThen(ctRSDF in LContainertypes, 'on', 'no'));
    end;

    AddFormField('cnl2', IfThen(UseCNL, 'on', 'no'));

    AddFormField('enable-captcha', IfThen(UseCaptcha, 'on', 'no'));

    if UseVisitorPassword then
      AddFormField('link-password', Visitorpassword);

    if UseDescription then
      AddFormField('link-description', Description);

    if UseWebseiteLink then
      AddFormField('provider-url', WebseiteLink);

    AddFormField('use-default-options', '1');
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(WEBSITE + 'api'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

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
    LNeedToUninitialize := Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED));
    try
      LXMLDoc := NewXMLDocument;
      try
        try
          with LXMLDoc do
          begin
            LoadFromXML(LHTTPProcess.HTTPResult.SourceCode);
            Active := True;
          end;
          with LXMLDoc.ChildNodes.Nodes['response'].ChildNodes do

            if Assigned(FindNode('p_links')) then
            begin
              // TODO: API will get changes in the future
              ACrypterFolderInfo.Link := VarToStr(Nodes['p_links'].NodeValue);

              Result := True;
            end
            else
            begin
              ErrorMsg := VarToStr(Nodes['api_error'].NodeValue);
            end;
        except
          on E: Exception do
          begin
            ErrorMsg := 'The XML from ' + GetName + ' was invaild: ' + E.message;
          end;
        end;
      finally
        LXMLDoc := nil;
      end;
    finally
      if LNeedToUninitialize then
        CoUninitialize;
    end;
  end;
end;

function Tsafelinkingnet.EditFolder;
begin
  Result := False;
end;

function Tsafelinkingnet.DeleteFolder;
begin
  Result := False;
end;

function Tsafelinkingnet.GetFolder;
var
  LNeedToUninitialize: Boolean;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;

  LXMLDoc: IXMLDocument;
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

  LRequestID := HTTPManager.Get(THTTPRequest.Create(WEBSITE + 'check?link=' + HTTPEncode(AFolderIdentifier)), TPlugInHTTPOptions.Create(Self));

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
    LNeedToUninitialize := Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED));
    try
      LXMLDoc := NewXMLDocument;
      try
        try
          with LXMLDoc do
          begin
            LoadFromXML(LHTTPProcess.HTTPResult.SourceCode);
            Active := True;
          end;
          with LXMLDoc.ChildNodes.Nodes['response'].ChildNodes do

            if Assigned(FindNode('link_status')) then
            begin
              // TODO: API will get changes in the future
              case IndexText(VarToStr(Nodes['link_status'].NodeValue), ['Online', 'Not yet checked', 'Broken', 'Unknown', 'Offline']) of
                0:
                  ACrypterFolderInfo.Status := csOnline;
                1:
                  ACrypterFolderInfo.Status := csUnknown;
                2:
                  ACrypterFolderInfo.Status := csMixedOnOffline;
                3:
                  ACrypterFolderInfo.Status := csUnknown;
                4:
                  ACrypterFolderInfo.Status := csOffline;
              else
                ACrypterFolderInfo.Status := csUnknown;
              end;
              Result := True;
            end
            else
            begin
              ErrorMsg := VarToStr(Nodes['api_error'].NodeValue);
            end;
        except
          on E: Exception do
          begin
            ErrorMsg := 'The XML from ' + GetName + ' was invaild: ' + E.message;
          end;
        end;
      finally
        LXMLDoc := nil;
      end;
    finally
      if LNeedToUninitialize then
        CoUninitialize;
    end;
  end;
end;

end.
