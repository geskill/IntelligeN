unit uAnyimgCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp, DECFmt,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TAnyimgCom = class(TImageHosterPlugIn)
  private const
    website: string = 'http://anyimg.com/';
    function Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString): WideString; override;
    function RemoteUpload(AImageUrl: WideString): WideString; override;
  end;

implementation

{ TAnyimgCom }

function TAnyimgCom.Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
var
  HTTPRequest: IHTTPRequest;
  HTTPOptions: IHTTPOptions;

  RequestID: Double;

  // LoginUserNameField,
  ImageURLBase64: string;
begin
  Result := False;

  /// Remote Upload for Accounts not working right now
{$REGION 'Remote Upload for Accounts'}
  (*
    if UseAccount then
    begin
    HTTPRequest := THTTPRequest.Create(website + 'Login');

    RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

    repeat
    sleep(50);
    until HTTPManager.HasResult(RequestID);

    with TRegExpr.Create do
    try
    InputString := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;
    Expression := 'value="" name="(.*?)" \/><\/dd>';

    if Exec(InputString) then
    LoginUserNameField := Match[1];
    finally
    Free;
    end;

    HTTPParams := THTTPParams.Create;
    with HTTPParams do
    begin
    AddFormField(LoginUserNameField, AccountName);
    AddFormField('password', AccountPassword);
    AddFormField('autologin', '1');
    AddFormField('sent', 'true');
    end;

    RequestID := HTTPManager.Post(website + 'Login', RequestID, HTTPParams, TPlugInHTTPOptions.Create(Self));

    repeat
    sleep(50);
    until HTTPManager.HasResult(RequestID);

    with TRegExpr.Create do
    try
    InputString := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;
    Expression := '<p class="errorMessage" .*?>(.*?)<\/p>';

    if Exec(InputString) then
    Self.ErrorMsg := Match[1];
    finally
    Free;
    end;
    end;
    *)
{$ENDREGION}
  HTTPRequest := THTTPRequest.Create('http://s1.anyimg.com/upload.php');
  HTTPRequest.Referer := website;

  with AHTTPParams do
  begin
    AddFormField('uploadSent', '1');

    AddFormField('user_secret', '');

    AddFormField('watermark', '');
    AddFormField('watermarkPosition', '');
    AddFormField('watermarkFontSize', '');

    AddFormField('thumbSize', '150_150');
    AddFormField('imageRotate', '1');
    AddFormField('imageFileType', '1');
    AddFormField('createGallery', '-1');
    AddFormField('createGalleryName', '');

    AddFormField('par_imagesize', '1');
    AddFormField('par_thumbsize', '1');
    AddFormField('image_content_type', '2');

    AddFormField('upload_secret', '');
    AddFormField('APC_UPLOAD_PROGRESS', '');

    AddFormField('remoteUploadFileList', '');

    AddFormField('a', '2');

    if not(ImageHostResize = irNone) then
    begin
      case ImageHostResize of
        ir320x240:
          AddFormField('imageSize', '320_240');
        ir450x338:
          AddFormField('imageSize', '450_338');
        ir640x480:
          AddFormField('imageSize', '640_480');
        ir800x600:
          AddFormField('imageSize', '800_600');
      end;

    end
    else
      AddFormField('imageSize', '1');
  end;

  HTTPOptions := TPlugInHTTPOptions.Create(Self);
  HTTPOptions.HandleSketchyRedirects := False;
  HTTPOptions.RedirectMaximum := 0;

  RequestID := HTTPManager.Post(HTTPRequest, AHTTPParams, HTTPOptions);

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ImageURLBase64 := HTTPManager.GetResult(RequestID).HTTPResult.HTTPResponse.Location;

  /// Server liefert nach POST Request URL in Base64 Encoding zurück
  /// Diese muss noch aufgerufen werden, damit das Bild auch verfügbar ist
  /// Deshalb auch HandleSketchyRedirects = False, damit man an die Location kommt
  /// Es muss aber nicht abgewartet werden bis der Request ausgeführt wurde,
  /// da das Bild nicht sofort für den User verfügbar sein muss.

  HTTPManager.Get(ImageURLBase64, RequestID, HTTPOptions);

  with TRegExpr.Create do
    try
      InputString := TFormat_MIME64.Decode(copy(ImageURLBase64, Pos('Uploaded/', ImageURLBase64) + length('Uploaded/')));
      Expression := '"ServerDomain";s:20:"(.*?)".*?"data";a:1:\{i:0;a:3:\{i:0;s:7:"(\w+)";i:1;s:\d+:"(.*?)"';

      if Exec(InputString) then
      begin
        AImageUrl := Match[1] + '/img/' + Match[2] + '/' + Match[3];
        Result := True;
      end;
    finally
      Free;
    end;
end;

function TAnyimgCom.GetName: WideString;
begin
  Result := 'Anyimg.com';
end;

function TAnyimgCom.LocalUpload(ALocalPath: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := '';

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFormField('uploadMode', 'multi');

    AddFile('uploadFiles[]', ALocalPath);
  end;

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

function TAnyimgCom.RemoteUpload(AImageUrl: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := AImageUrl;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFormField('uploadMode', 'remote');

    AddFormField('remoteFiles[]', AImageUrl);
  end;

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

end.
