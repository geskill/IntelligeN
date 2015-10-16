unit uLinkcryptWs;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Variants, SyncObjs, XMLDoc, XMLIntf, ActiveX,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uVariantUtils;

type
  // see: http://linkcrypt.ws/image/Linkcrypt.ws_API-Create_folder_DE.pdf
  TLinkcryptWs = class(TCrypterPlugIn)
  private const
    website = 'http://linkcrypt.ws/';

    function GetFolderID(AFolderURL: string): string;
    function GetStatusImageLink(AFolderIdentifier: WideString; Small: WordBool = True): WideString;
  public
    function GetName: WideString; override;
    function AddFolder(const AMirrorContainer: IDirectlinkContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function EditFolder(const AMirrorContainer: IDirectlinkContainer; var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function DeleteFolder(AFolderIdentifier: WideString): WordBool; override; safecall;
    function GetFolder(AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
  end;

implementation

function TLinkcryptWs.GetFolderID(AFolderURL: string): string;
begin
  Result := '';

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

function TLinkcryptWs.GetStatusImageLink(AFolderIdentifier: WideString; Small: WordBool = True): WideString;
begin
  case Small of
    True:
      Result := StringReplace(AFolderIdentifier, '/dir/', '/png/', []);
    False:
      Result := StringReplace(AFolderIdentifier, '/dir/', '/textpng/', []);
  end;
end;

function TLinkcryptWs.GetName;
begin
  Result := 'Linkcrypt.ws';
end;

function TLinkcryptWs.AddFolder;
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
      AddFormField('apiKey', AccountName);

    AddFormField('urls', StringReplace(AMirrorContainer.Directlink[0].Value, sLineBreak, ';', [rfReplaceAll]));

    if not SameStr('', FolderName) then
      AddFormField('title', FolderName);

    if UseVisitorPassword then
      AddFormField('folder_password', Visitorpassword);

    if UseFilePassword then
      AddFormField('download_password', FilePassword);

    AddFormField('captx', IfThen(UseCaptcha, '0', '1'));

    AddFormField('weburls', IfThen(ftWeb in LFoldertypes, '0', '1'));

    if ftContainer in LFoldertypes then
    begin
      AddFormField('dlc', IfThen(ctDLC in LContainertypes, '0', '1'));
      AddFormField('rsdf', IfThen(ctRSDF in LContainertypes, '0', '1'));
      AddFormField('ccf', IfThen(ctCCF in LContainertypes, '0', '1'));
    end
    else
    begin
      AddFormField('dlc', '1');
      AddFormField('rsdf', '1');
      AddFormField('ccf', '1');
    end;

    AddFormField('cnl', IfThen(UseCNL, '0', '1'));

    for LDirectlinkIndex := 1 to AMirrorContainer.DirectlinkCount - 1 do
      AddFormField('mirror_' + IntToStr(LDirectlinkIndex), StringReplace(AMirrorContainer.Directlink[LDirectlinkIndex].Value, sLineBreak, ';', [rfReplaceAll]));
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api.html?api=create_V2'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else if not SameStr('', LHTTPProcess.HTTPResult.SourceCode) then
  begin
    OleInitialize(nil);
    try
      LXMLDoc := NewXMLDocument;
      try
        try
          with LXMLDoc do
          begin
            LoadFromXML(LHTTPProcess.HTTPResult.SourceCode);
            Active := True;
          end;
          with LXMLDoc.ChildNodes.Nodes['data'].ChildNodes do
            if (Nodes['status'].NodeValue = '1') then
            begin
              ACrypterFolderInfo.Link := Nodes['folderUrl'].NodeValue;
              ACrypterFolderInfo.StatusImage := GetStatusImageLink(ACrypterFolderInfo.Link);
              ACrypterFolderInfo.StatusImageText := GetStatusImageLink(ACrypterFolderInfo.Link, False);
              Result := True;
            end
            else
            begin
              ErrorMsg := Nodes['errorCode'].NodeValue + ': ' + Nodes['errorMsg'].NodeValue;
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
      OleUninitialize;
    end;
  end
  else
  begin
    ErrorMsg := 'The Server response was empty!';
  end;
end;

function TLinkcryptWs.EditFolder;
begin
  Result := False;
end;

function TLinkcryptWs.DeleteFolder;
begin
  Result := False;
end;

function TLinkcryptWs.GetFolder;
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
      AddFormField('apiKey', AccountName);

    AddFormField('folderKey', GetFolderID(AFolderIdentifier));
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api.html?api=getFolder_V2'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else if not SameStr('', LHTTPProcess.HTTPResult.SourceCode) then
  begin
    OleInitialize(nil);
    try
      LXMLDoc := NewXMLDocument;
      try
        try
          with LXMLDoc do
          begin
            LoadFromXML(LHTTPProcess.HTTPResult.SourceCode);
            Active := True;
          end;
          with LXMLDoc.ChildNodes.Nodes['data'].ChildNodes do
          begin
            if not Assigned(FindNode('errorCode')) then
            begin
              case IndexText(VarToStr(Nodes['folderStatus'].NodeValue), ['1', '3', '2', '0']) of
                0:
                  ACrypterFolderInfo.Status := csOnline;
                1:
                  ACrypterFolderInfo.Status := csMixedOnOffline;
                2:
                  ACrypterFolderInfo.Status := csOffline;
                3:
                  ACrypterFolderInfo.Status := csUnknown;
              else
                ACrypterFolderInfo.Status := csNotChecked;
              end;
              ACrypterFolderInfo.Size := VarToFloatDefault(VarToStr(Nodes['folderSize'].NodeValue), 0, False);
              if Nodes['files'].ChildNodes.Count > 0 then
                ACrypterFolderInfo.PartSize := VarToFloatDefault(Nodes['files'].ChildNodes.Nodes[0].ChildNodes.Nodes['filesize'], 0, False);
              ACrypterFolderInfo.Hoster := VarToStr(Nodes['folderHoster'].NodeValue);
              ACrypterFolderInfo.Parts := Nodes['files'].ChildNodes.Count;
              ACrypterFolderInfo.StatusImage := GetStatusImageLink(ACrypterFolderInfo.Link);
              ACrypterFolderInfo.StatusImageText := GetStatusImageLink(ACrypterFolderInfo.Link, False);
              Result := True;
            end
            else
            begin
              ErrorMsg := Nodes['errorCode'].NodeValue + ': ' + Nodes['errorMsg'].NodeValue;
            end;
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
      OleUninitialize;
    end;
  end
  else
  begin
    ErrorMsg := 'The Server response was empty!';
  end;
end;

end.
