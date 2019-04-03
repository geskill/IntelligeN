unit uTolinkto;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Math, Variants,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uVariantUtils,
  // LkJSON
  uLkJSON,
  // XYZ
  IniFiles, uSystemUtils;

type
  // see: https://tolink.to/app/index/api
  TTolinkto = class(TCrypterPlugIn)
  protected { . }
  const
    WEBSITE = 'https://tolink.to/';

    function GetFolderID(AFolderURL: string): string;
  public
    function GetName: WideString; override; safecall;

    function AddFolder(const AMirrorContainer: IDirectlinkContainer;
      out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override;
      safecall;
    function EditFolder(const AMirrorContainer: IDirectlinkContainer;
      var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override;
      safecall;
    function DeleteFolder(AFolderIdentifier: WideString): WordBool; override;
      safecall;
    function GetFolder(AFolderIdentifier: WideString;
      out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override;
      safecall;
  end;

implementation

function TTolinkto.GetFolderID(AFolderURL: string): string;
begin
  Result := '';

  with TRegExpr.Create do
    try
      InputString := AFolderURL;
      Expression := 'f\/(\w+)';

      if Exec(InputString) then
        Result := Match[1];
    finally
      Free;
    end;
end;

function TTolinkto.GetName;
begin
  Result := 'Tolink.to';
end;

function TTolinkto.AddFolder;
var
  ini: TIniFile;
  LFoldertypes: TFoldertypes;
  LContainertypes: TContainertypes;

  LDirectlinkIndex: Integer;

  LHTTPParams: IHTTPParams;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;

  MainJSONObject, BodyJSONObject, OptionsJSONObject, LJSONobject: TlkJSONobject;
  LJSONStr: string;
begin
  // update 15.03 add captcha_text
  // {
  // "apikey": "key123456",
  // "body": {
  // "title": "Test Folder",
  // "links": "http://linkZumDownload;http://linkZumDownload;http://linkZumDownload",
  // "options": {
  // "web": true,
  // "container": false,
  // "cln": true,
  // "captcha": true,
  // "captcha_text": false,
  // "password": ""
  // }
  // }
  // }
  ini := TIniFile.Create(ExtractFilePath(GetModulePath) + 'tolinkto.ini');
  LFoldertypes := TFoldertypes(TFoldertype(Foldertypes));
  LContainertypes := TContainertypes(TContainertype(ContainerTypes));

  Result := False;
  LJSONStr := '';

  MainJSONObject := TlkJSONobject.Create;
  BodyJSONObject := TlkJSONobject.Create;
  OptionsJSONObject := TlkJSONobject.Create;
  try
    MainJSONObject.Add('apikey', AccountName);
    // body
    BodyJSONObject.Add('title', FolderName);
    BodyJSONObject.Add('links',
      StringReplace(AMirrorContainer.Directlink[0].Value, sLineBreak, ';',
        [rfReplaceAll]));
    // fix Boolean as string error 18.03.19
    if (ftWeb in LFoldertypes) then
      OptionsJSONObject.Add('web', True)
    else
      OptionsJSONObject.Add('web', False);
    if (ftContainer in LFoldertypes) then
      OptionsJSONObject.Add('container', True)
    else
      OptionsJSONObject.Add('container', False);
    if (UseCNL) then
      OptionsJSONObject.Add('cln', True)
    else
      OptionsJSONObject.Add('cln', False);
    if (UseCaptcha) then
    begin
      if (ini.ReadBool('tolinkto', 'text_captcha', False)) then
      begin
        OptionsJSONObject.Add('captcha', False);
        OptionsJSONObject.Add('captcha_text', True)
      end
      else
      begin
        OptionsJSONObject.Add('captcha', True);
        OptionsJSONObject.Add('captcha_text', False)
      end
    end
    else
    begin
      OptionsJSONObject.Add('captcha', False);
      OptionsJSONObject.Add('captcha_text', False)
    end;
    OptionsJSONObject.Add('password', Visitorpassword);
    BodyJSONObject.Add('options', OptionsJSONObject);
    MainJSONObject.Add('body', BodyJSONObject);

    LJSONStr := TlkJSON.GenerateText(MainJSONObject);
  finally

  end;
  LHTTPParams := THTTPParams.Create(LJSONStr);

  LRequestID := HTTPManager.Post(THTTPRequest.Create
      (WEBSITE + 'api/v1/folder/create'), LHTTPParams,
    TPlugInHTTPOptions.Create(Self));

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
    try
      try
        LJSONobject := TlkJSON.ParseText(LHTTPProcess.HTTPResult.SourceCode)
          as TlkJSONobject;
        if (VarToStr(LJSONobject.Field['response'].Field['errorCode'].Value)
            = '0') then
        begin
          ACrypterFolderInfo.Link := LJSONobject.Field['response'].Field['body']
            .Value;
          ACrypterFolderInfo.StatusImage :=
            ACrypterFolderInfo.Link + '/s/status.png';
          ACrypterFolderInfo.StatusImageText :=
            ACrypterFolderInfo.Link + '/s/status.png';
          Result := True;
        end
        else
        begin
          ErrorMsg := VarToStr(LJSONobject.Field['response'].Field['errorCode']
              .Value) + ': ' + VarToStr
            (LJSONobject.Field['response'].Field['errorMsg'].Value);
        end;
      finally
        LJSONobject.Free;
      end;
    except
      on E: Exception do
      begin
        ErrorMsg := 'The result from ' + GetName + ' was invaild: ' + E.Message;
      end;
    end;
  end;
end;

function TTolinkto.EditFolder;
begin
  Result := False;
end;

function TTolinkto.DeleteFolder;
begin
  Result := False;
end;

function TTolinkto.GetFolder;
var

  LHTTPParams: IHTTPParams;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
  LJSONStr, LResponseStr: string;
  MainJSONObject, BodyJSONObject, LJSONobject: TlkJSONobject;
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
  MainJSONObject := TlkJSONobject.Create;
  BodyJSONObject := TlkJSONobject.Create;
  try
    MainJSONObject.Add('apikey', AccountName);
    // body
    BodyJSONObject.Add('folder', GetFolderID(AFolderIdentifier));
    MainJSONObject.Add('body', BodyJSONObject);

    LJSONStr := TlkJSON.GenerateText(MainJSONObject);
  finally

  end;
  LHTTPParams := THTTPParams.Create(LJSONStr);

  LRequestID := HTTPManager.Post(THTTPRequest.Create
      (WEBSITE + 'api/v1/folder/status'), LHTTPParams,
    TPlugInHTTPOptions.Create(Self));

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
    try
      LResponseStr := LHTTPProcess.HTTPResult.SourceCode;
      try
        LJSONobject := TlkJSON.ParseText(LResponseStr) as TlkJSONobject;
        if (VarToStr(LJSONobject.Field['response'].Field['errorCode'].Value)
            = '0') then
        begin
          if (LJSONobject.Field['response'].Field['body'].Field['status']
              .Value = '2') then
            ACrypterFolderInfo.Status := csOffline
          else if (LJSONobject.Field['response'].Field['body'].Field['status']
              .Value = '1') then
            ACrypterFolderInfo.Status := csOnline
          else if (LJSONobject.Field['response'].Field['body'].Field['status']
              .Value = '3') then
            ACrypterFolderInfo.Status := csMixedOnOffline
          else
            ACrypterFolderInfo.Status := csUnknown;

          ACrypterFolderInfo.Link := AFolderIdentifier;
          ACrypterFolderInfo.Size := 0;
          ACrypterFolderInfo.PartSize := 0;
          ACrypterFolderInfo.StatusImage :=
            ACrypterFolderInfo.Link + '/s/status.png';
          ACrypterFolderInfo.StatusImageText :=
            ACrypterFolderInfo.Link + '/s/status.png';
          Result := True;
        end
        else

        begin
          ErrorMsg := VarToStr(LJSONobject.Field['response'].Field['errorCode']
              .Value) + ': ' + VarToStr
            (LJSONobject.Field['response'].Field['errorMsg'].Value);
        end;
      finally
        LJSONobject.Free;
      end;
    except
      on E: Exception do
      begin
        ErrorMsg := 'The result from ' + GetName + ' was invaild: ' + E.Message;
      end;
    end;
  end;
end;

end.
