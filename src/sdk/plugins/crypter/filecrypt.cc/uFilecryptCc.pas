unit uFilecryptCc;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Math, Variants, XMLDoc, XMLIntf, ActiveX,
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
  // see: http://filecrypt.cc/api.pdf
  TFilecryptCc = class(TCrypterPlugIn)
  protected { . }
  const
    WEBSITE = 'http://filecrypt.cc/';

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

function TFilecryptCc.GetFolderID(AFolderURL: string): string;
begin
  Result := '';

  with TRegExpr.Create do
    try
      InputString := AFolderURL;
      Expression := 'Container\/(\w+)';

      if Exec(InputString) then
        Result := Match[1];
    finally
      Free;
    end;
end;

function TFilecryptCc.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TFilecryptCc.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TFilecryptCc.GetDescription;
begin
  Result := GetName + ' crypter plug-in.';
end;

function TFilecryptCc.GetName;
begin
  Result := 'Filecrypt.cc';
end;

function TFilecryptCc.GetServiceRequiresAccess;
begin
  Result := caAPIKey;
end;

function TFilecryptCc.AddFolder;
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

  LFoldertypes := TFoldertypes(TFoldertype(ACrypterData.Foldertypes));
  LContainertypes := TContainertypes(TContainertype(ACrypterData.ContainerTypes));

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    if ACrypterData.UseAccount then
      AddFormField('api_key', ACrypterData.AccountName);

    AddFormField('foldername', ACrypterData.FolderName);

    for LDirectlinkIndex := 0 to AMirrorContainer.DirectlinkCount - 1 do
      AddFormField('mirror[]', AMirrorContainer.Directlink[LDirectlinkIndex].Value);

    AddFormField('allow_links', IfThen(ftWeb in LFoldertypes, '1', '0'));

    AddFormField('allow_container', IfThen((ftContainer in LFoldertypes) or ACrypterData.UseCNL, '1', '0'));

    AddFormField('captcha', IfThen(ACrypterData.UseCaptcha, '1', '0'));

    if ACrypterData.UseVisitorPassword then
      AddFormField('folderpass', ACrypterData.Visitorpassword);

    AddFormField('fn', 'container');

    AddFormField('sub', 'create');

    AddFormField('fmt', 'xml');
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(WEBSITE + 'api.php'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

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
          with LXMLDoc.ChildNodes.Nodes['filecrypt'].ChildNodes do
            if (VarToStr(Nodes['state'].NodeValue) = '1') then
            begin
              with Nodes['container'].ChildNodes do
              begin
                ACrypterFolderInfo.Link := VarToStr(Nodes['link'].NodeValue);
                case IndexText(VarToStr(Nodes['status'].NodeValue), ['0', '1', '2', '3', '4', '5', '6']) of
                  0:
                    ACrypterFolderInfo.Status := csUnknown;
                  1:
                    ACrypterFolderInfo.Status := csOnline;
                  2:
                    ACrypterFolderInfo.Status := csUnknown;
                  3:
                    ACrypterFolderInfo.Status := csUnknown;
                  4:
                    ACrypterFolderInfo.Status := csOffline;
                  5:
                    ACrypterFolderInfo.Status := csMixedOnOffline;
                  6:
                    ACrypterFolderInfo.Status := csOnline;
                else
                  ACrypterFolderInfo.Status := csUnknown;
                end;
                ACrypterFolderInfo.Size := RoundTo((VarToInt64Def(Nodes['size'].NodeValue, 0) / 1048576), -2);
                ACrypterFolderInfo.Hoster := VarToStr(Nodes['hoster'].NodeValue);
                ACrypterFolderInfo.Parts := VarToIntDef(Nodes['links'].NodeValue, 0);
                ACrypterFolderInfo.StatusImage := VarToStr(Nodes['smallimg'].NodeValue) + '.png';
                ACrypterFolderInfo.StatusImageText := VarToStr(Nodes['bigimg'].NodeValue) + '.png';
              end;
              Result := True;
            end
            else
            begin
              ErrorMsg := VarToStr(Nodes['error'].NodeValue);
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

function TFilecryptCc.EditFolder;
begin
  Result := False;
end;

function TFilecryptCc.DeleteFolder;
begin
  Result := False;
end;

function TFilecryptCc.GetFolder;
var
  LNeedToUninitialize: Boolean;
  LHTTPParams: IHTTPParams;
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

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    if AAccountData.UseAccount then
      AddFormField('api_key', AAccountData.AccountName);

    AddFormField('container_id', GetFolderID(AFolderIdentifier));

    AddFormField('fn', 'container');

    AddFormField('sub', 'status');

    AddFormField('fmt', 'xml');
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(WEBSITE + 'api.php'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

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
          with LXMLDoc.ChildNodes.Nodes['filecrypt'].ChildNodes do
            if (VarToStr(Nodes['state'].NodeValue) = '1') then
            begin
              with Nodes['container'].ChildNodes do
              begin
                case IndexText(VarToStr(Nodes['status'].NodeValue), ['0', '1', '2', '3', '4', '5', '6']) of
                  0:
                    ACrypterFolderInfo.Status := csUnknown;
                  1:
                    ACrypterFolderInfo.Status := csOnline;
                  2:
                    ACrypterFolderInfo.Status := csUnknown;
                  3:
                    ACrypterFolderInfo.Status := csUnknown;
                  4:
                    ACrypterFolderInfo.Status := csOffline;
                  5:
                    ACrypterFolderInfo.Status := csMixedOnOffline;
                  6:
                    ACrypterFolderInfo.Status := csOnline;
                else
                  ACrypterFolderInfo.Status := csUnknown;
                end;
                ACrypterFolderInfo.Size := RoundTo((VarToInt64Def(Nodes['size'].NodeValue, 0) / 1048576), -2);
                ACrypterFolderInfo.Hoster := VarToStr(Nodes['hoster'].NodeValue);
                ACrypterFolderInfo.Parts := VarToIntDef(Nodes['links'].NodeValue, 0);
                ACrypterFolderInfo.StatusImage := VarToStr(Nodes['smallimg'].NodeValue) + '.png';
                ACrypterFolderInfo.StatusImageText := VarToStr(Nodes['bigimg'].NodeValue) + '.png';
              end;
              Result := True;
            end
            else
            begin
              ErrorMsg := VarToStr(Nodes['error'].NodeValue);
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
