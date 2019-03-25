unit uCriptto;

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
  TCriptto = class(TCrypterPlugIn)
  protected { . }
  const
    WEBSITE = 'https://cript.to/api/';

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

function TCriptto.GetFolderID(AFolderURL: string): string;
begin
  Result := '';

  with TRegExpr.Create do
    try
      InputString := AFolderURL;
      Expression := 'folder\/(\w+)';

      if Exec(InputString) then
        Result := Match[1];
    finally
      Free;
    end;
end;

function TCriptto.GetName;
begin
  Result := 'Cript.to';
end;

function TCriptto.AddFolder;
var
  ini: TIniFile;
  LFoldertypes: TFoldertypes;
  LContainertypes: TContainertypes;

  LDirectlinkIndex: Integer;

  LHTTPParams: IHTTPParams;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;

  LJSONobject: TlkJSONobject;
  LJSONStr: string;
begin
  // key 	string 	true
  // name 	string 	true
  // mirror 	Array 	true
  // password 	string 	false
  // captcha 	int 	false (if not provided it uses default settings)
  // allow_links 	int 	false (if not provided it uses default settings)
  // allow_cnl 	int 	false (if not provided it uses default settings)
  // allow_dlc 	int 	false (if not provided it uses default settings)
  ini := TIniFile.Create(ExtractFilePath(GetModulePath) + 'tolinkto.ini');
  LFoldertypes := TFoldertypes(TFoldertype(Foldertypes));
  LContainertypes := TContainertypes(TContainertype(ContainerTypes));

  Result := False;
  LJSONStr := '';
  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin

    AddFormField('key', AccountName);
    AddFormField('name', IfThen(FolderName = '','No name folder', FolderName));
    AddFormField('mirror[]',
      StringReplace(AMirrorContainer.Directlink[0].Value, sLineBreak, ';',
        [rfReplaceAll]));

    AddFormField('allow_links', IfThen(ftWeb in LFoldertypes, '1', '0'));

    AddFormField('allow_dlc', IfThen(ftContainer in LFoldertypes, '1', '0'));

    AddFormField('allow_cnl', IfThen(UseCNL, '1', '0'));
    AddFormField('captcha', IfThen(UseCaptcha, '1', '0'));

    AddFormField('password', Visitorpassword);
    LRequestID := HTTPManager.Post(THTTPRequest.Create
        (WEBSITE + 'v1/folder/create'), LHTTPParams,
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
          if (VarToStr(LJSONobject.Field['status'].Value) = 'success') then
          begin
            ACrypterFolderInfo.Link := LJSONobject.Field['data'].Field
              ['folder_link'].Value;
            ACrypterFolderInfo.StatusImage := LJSONobject.Field['data'].Field
              ['status_images'].Field['small'].Value;
            ACrypterFolderInfo.StatusImageText := LJSONobject.Field['data']
              .Field['status_images'].Field['txt'].Value;
            Result := True;
          end
          else
          begin
            ErrorMsg := VarToStr(LJSONobject.Field['status'].Value)
              + ': ' + VarToStr(LJSONobject.Field['message'].Value);
          end;
        finally
          LJSONobject.Free;
        end;
      except
        on E: Exception do
        begin
          ErrorMsg := 'The result from ' + GetName + ' was invaild: ' +
            E.Message;
        end;
      end;
    end;
  end;
end;

function TCriptto.EditFolder;
begin
  Result := False;
end;

function TCriptto.DeleteFolder;
begin
  Result := False;
end;

function TCriptto.GetFolder;
var

  LHTTPParams: IHTTPParams;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
  LJSONStr, LResponseStr: string;
  LJSONobject: TlkJSONobject;
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

  LRequestID := HTTPManager.Post(THTTPRequest.Create
      (WEBSITE + 'v1/folder/info?id=' + GetFolderID(AFolderIdentifier)),
    THTTPParams.Create, TPlugInHTTPOptions.Create(Self));

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
        if (VarToStr(LJSONobject.Field['status'].Value) = 'success') then
        begin
          case IndexText
            (VarToStr(LJSONobject.Field['data'].Field['status'].Value),
            ['online', 'mixed', 'offline', 'unknown']) of
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

          ACrypterFolderInfo.Link := AFolderIdentifier;
          ACrypterFolderInfo.Size := VarToFloatDefault
            (LJSONobject.Field['data'].Field['size'].Field['mb'].Value, 0,
            True);
          ACrypterFolderInfo.Parts := VarToIntDef
            (LJSONobject.Field['data'].Field['parts'].Value, 1);

          ACrypterFolderInfo.StatusImage := LJSONobject.Field['data'].Field
            ['images'].Field['small'].Value;
          ACrypterFolderInfo.StatusImageText := LJSONobject.Field['data'].Field
            ['images'].Field['txt'].Value;
          Result := True;
        end
        else

        begin
          ErrorMsg := VarToStr(LJSONobject.Field['status'].Value)
            + ': ' + VarToStr(LJSONobject.Field['message'].Value);
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
