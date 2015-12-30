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
  uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst,
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
    function GetName: WideString; override; safecall;

    function AddFolder(const AMirrorContainer: IDirectlinkContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function EditFolder(const AMirrorContainer: IDirectlinkContainer; var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function DeleteFolder(AFolderIdentifier: WideString): WordBool; override; safecall;
    function GetFolder(AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
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

function TFilecryptCc.GetName;
begin
  Result := 'Filecrypt.cc';
end;

function TFilecryptCc.AddFolder;
var
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
      AddFormField('api_key', AccountName);

    AddFormField('foldername', FolderName);

    for LDirectlinkIndex := 0 to AMirrorContainer.DirectlinkCount - 1 do
      AddFormField('mirror[]', AMirrorContainer.Directlink[LDirectlinkIndex].Value);

    AddFormField('allow_links', IfThen(ftWeb in LFoldertypes, '1', '0'));

    AddFormField('allow_container', IfThen((ftContainer in LFoldertypes) or UseCNL, '1', '0'));

    AddFormField('captcha', IfThen(UseCaptcha, '1', '0'));

    if UseVisitorPassword then
      AddFormField('folderpass', Visitorpassword);

    AddFormField('fn', 'container');

    AddFormField('sub', 'create');

    AddFormField('fmt', 'xml');
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(WEBSITE + 'api.php'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

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
    CoInitializeEx(nil, COINIT_MULTITHREADED);
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
    if UseAccount then
      AddFormField('api_key', AccountName);

    AddFormField('container_id', GetFolderID(AFolderIdentifier));

    AddFormField('fn', 'container');

    AddFormField('sub', 'status');

    AddFormField('fmt', 'xml');
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(WEBSITE + 'api.php'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

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
    CoInitializeEx(nil, COINIT_MULTITHREADED);
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
      CoUninitialize;
    end;
  end;
end;

end.
