unit uLinkcryptWs;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Variants, SyncObjs, XMLDoc, XMLIntf, ActiveX,
  // RegEx
  RegExpr,
  // Common
  uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TLinkcryptWs = class(TCrypterPlugIn)
  private const
    website = 'http://linkcrypt.ws/';

    function GetFolderID(AFolderURL: string): string;
  public
    function GetName: WideString; override;
    function AddFolder(const AMirrorContainer: IMirrorContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function EditFolder(const AMirrorContainer: IMirrorContainer; ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
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

function TLinkcryptWs.GetName;
begin
  Result := 'Linkcrypt.ws';
end;

function TLinkcryptWs.AddFolder;
var
  _Foldertypes: TFoldertypes;
  _Containertypes: TContainertypes;

  I: Integer;

  HTTPParams: IHTTPParams;

  RequestID: Double;

  HTTPProcess: IHTTPProcess;

  XMLDoc: IXMLDocument;
begin
  _Foldertypes := TFoldertypes(TFoldertype(Foldertypes));
  _Containertypes := TContainertypes(TContainertype(ContainerTypes));

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFormField('urls', StringReplace(AMirrorContainer.Directlink[0].Value, sLineBreak, ';', [rfReplaceAll]));

    for I := 1 to AMirrorContainer.DirectlinkCount - 1 do
      AddFormField('mirror_' + IntToStr(I), StringReplace(AMirrorContainer.Directlink[I].Value, sLineBreak, ';', [rfReplaceAll]));

    if ftWeb in _Foldertypes then
      AddFormField('weburls', '0')
    else
      AddFormField('weburls', '1');

    if ftContainer in _Foldertypes then
    begin
      if ctDLC in _Containertypes then
        AddFormField('dlc', '0')
      else
        AddFormField('dlc', '1');
      if ctRSDF in _Containertypes then
        AddFormField('rsdf', '0')
      else
        AddFormField('rsdf', '1');
      if ctCCF in _Containertypes then
        AddFormField('ccf', '0')
      else
        AddFormField('ccf', '1');
    end
    else
    begin
      AddFormField('dlc', '1');
      AddFormField('rsdf', '1');
      AddFormField('ccf', '1');
    end;

    if UseCNL then
      AddFormField('cnl', '0')
    else
      AddFormField('cnl', '1');

    if UseCaptcha then
      AddFormField('captx', '0')
    else
      AddFormField('captx', '1');

    if not(FolderName = '') then
      AddFormField('title', FolderName);

    if UseFilePassword then
      AddFormField('download_password', FilePassword);

    if UseVisitorPassword then
      AddFormField('folder_password', Visitorpassword);

    if UseAccount then
      AddFormField('api_user_id', AccountName);

    AddFormField('api', 'create_V1');
  end;

  RequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api.html'), HTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  HTTPProcess := HTTPManager.GetResult(RequestID);

  if HTTPProcess.HTTPResult.HasError then
    ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage
  else if not SameStr('', HTTPProcess.HTTPResult.SourceCode) then
  begin
    OleInitialize(nil);
    try
      XMLDoc := NewXMLDocument;
      try
        try
          with XMLDoc do
          begin
            LoadFromXML(HTTPProcess.HTTPResult.SourceCode);
            Active := True;
          end;
          with XMLDoc.ChildNodes.Nodes['data'].ChildNodes do
            if (Nodes['status'].NodeValue = '1') then
              Result := Nodes['folderUrl'].NodeValue
            else
              ErrorMsg := Nodes['errorCode'].NodeValue + ': ' + Nodes['errorMsg'].NodeValue;
        except
          on E: Exception do
          begin
            ErrorMsg := 'The XML from ' + GetName + ' was invaild: ' + E.message;
          end;
        end;
      finally
        XMLDoc := nil;
      end;
    finally
      OleUninitialize;
    end;
  end
  else
    ErrorMsg := 'The Server response was empty!';
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
  CrypterFolderInfo: TCrypterFolderInfo;

  RequestID: Double;

  HTTPProcess: IHTTPProcess;

  FormatSettings: TFormatSettings;
  XMLDoc: IXMLDocument;
begin
  with CrypterFolderInfo do
  begin
    Status := 255;
    Size := 0;
    Hoster := '';
    Parts := 0;
  end;

  // thread-safe ???
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
  FormatSettings.DecimalSeparator := ',';

  RequestID := HTTPManager.Get(THTTPRequest.Create(website + 'api.html?api=status_V1&detail=1&folderKey=' + GetFolderID(AFolderIdentifier)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  HTTPProcess := HTTPManager.GetResult(RequestID);

  if HTTPProcess.HTTPResult.HasError then
    ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage
  else if not SameStr('', HTTPProcess.HTTPResult.SourceCode) then
  begin
    OleInitialize(nil);
    try
      XMLDoc := NewXMLDocument;
      try
        try
          with XMLDoc do
          begin
            LoadFromXML(HTTPProcess.HTTPResult.SourceCode);
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
        except
          on E: Exception do
          begin
            ErrorMsg := 'The XML from ' + GetName + ' was invaild: ' + E.message;
            // Exit(CrypterFolderInfo);
          end;
        end;
      finally
        XMLDoc := nil;
      end;
    finally
      OleUninitialize;
    end;
  end
  else
    ErrorMsg := 'The Server response was empty!';

  (*
    case Small of
    True:
    Result := StringReplace(FolderURL, '/dir/', '/png/', []);
    False:
    Result := StringReplace(FolderURL, '/dir/', '/textpng/', []);
    end;
    *)

  // Result := CrypterFolderInfo;
end;

end.
