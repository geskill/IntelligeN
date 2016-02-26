unit uAnyimgCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp,
  // DEC
  DECFmt,
  // RegEx
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TAnyimgCom = class(TImageHosterPlugIn)
  protected { . }
  const
    WEBSITE: string = 'http://anyimg.com/';
    function Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(const ALocalPath: WideString; out AUrl: WideString): WordBool; override;
    function RemoteUpload(const ARemoteUrl: WideString; out AUrl: WideString): WordBool; override;
  end;

implementation

{ TAnyimgCom }

function TAnyimgCom.Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString): Boolean;
const
  UPLOADED_STRING: string = 'Uploaded/';
var
  LHTTPRequest: IHTTPRequest;
  LHTTPOptions: IHTTPOptions;

  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;

  // LoginUserNameField,
  LImageURLBase64: string;
begin
  Result := False;
  AImageUrl := '';

  /// Remote Upload for Accounts not working right now
{$REGION 'Remote Upload for Accounts'}
  (*
    if UseAccount then
    begin
    HTTPRequest := THTTPRequest.Create(website + 'Login');

    RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

    HTTPManager.WaitFor(RequestID);

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

    HTTPManager.WaitFor(RequestID);

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
  LHTTPRequest := THTTPRequest.Create('http://s1.anyimg.com/upload.php');
  with LHTTPRequest do
  begin
    Referer := WEBSITE;
  end;

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

    AddFormField('a', '2'); // Adult Content

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

  LHTTPOptions := TPlugInHTTPOptions.Create(Self);
  LHTTPOptions.HandleSketchyRedirects := False;
  LHTTPOptions.RedirectMaximum := 0;

  LRequestID := HTTPManager.Post(LHTTPRequest, AHTTPParams, LHTTPOptions);

  HTTPManager.WaitFor(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  LImageURLBase64 := LHTTPProcess.HTTPResult.HTTPResponse.Location;

  /// Server liefert nach POST Request URL in Base64 Encoding zurück
  /// Diese muss noch aufgerufen werden, damit das Bild auch verfügbar ist
  /// Deshalb auch HandleSketchyRedirects = False, damit man an die Location kommt
  /// Es muss aber nicht abgewartet werden bis der Request ausgeführt wurde,
  /// da das Bild nicht sofort für den User verfügbar sein muss.

  if (Pos('successfullyUploaded', LImageURLBase64) > 0) then
  begin
    HTTPManager.Get(LImageURLBase64, LRequestID, LHTTPOptions);

  (*
   a:4:{
      s:12:"ServerDomain";
      s:20:"http://s1.anyimg.com";
      s:4:"data";
      a:1:{i:0;
           a:3:{i:0;
                s:7:"ubg6h3t";
                i:1;
                s:36:"35b76d34ee3d3e6cf8f4a474d51ab63f.jpg";
                i:2;
                s:1:"2";
         }
      }
      s:6:"images";
      a:1:{i:0;
           a:3:{i:0;
                s:7:"ubg6h3t";
                i:1;
                a:2:{s:5:"thumb";
                     a:3:{i:0;
                          i:100;
                          i:1;
                          i:150;
                          i:2;
                          i:4531;
               }
               s:5:"image";
               a:3:{i:0;
                    i:267;
                    i:1;
                    i:400;
                    i:2;
                    i:18708;
               }
            }
            i:2;
            d:0.017841339111328125;
         }
      }
      s:6:"common";
      a:2:{i:0;
           N;
           i:1;
           s:20:"http://s1.anyimg.com";
      }
   }
  *)

    with TRegExpr.Create do
      try
        InputString := TFormat_MIME64.Decode(copy(LImageURLBase64, Pos(UPLOADED_STRING, LImageURLBase64) + length(UPLOADED_STRING)));
        Expression := '"ServerDomain";s:20:"(.*?)".*?"data";a:1:\{i:0;a:3:\{i:0;s:7:"(\w+)";i:1;s:\d+:"(.*?)"';

        if Exec(InputString) then
        begin
          // http://s1.anyimg.com/img/2wfeq97/35b76d34ee3d3e6cf8f4a474d51ab63f.jpg

          AImageUrl := Match[1] + '/img/' + Match[2] + '/' + Match[3];
          Result := True;
        end;
      finally
        Free;
      end;
  end
  else if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else
  begin
    with TRegExpr.Create do
      try
        InputString := LImageURLBase64;
        Expression := 'com\/(.*?)\/(.*?)$';

        if Exec(InputString) then
        begin
          Self.ErrorMsg := Match[1] + ': ' + Match[2];
        end
        else
        begin
          Self.ErrorMsg := LImageURLBase64;
        end;
      finally
        Free;
      end;
  end;
end;

function TAnyimgCom.GetName: WideString;
begin
  Result := 'Anyimg.com';
end;

function TAnyimgCom.LocalUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('uploadMode', 'multi');

    AddFile('uploadFiles[]', ALocalPath);
  end;

  Result := Upload(LHTTPParams, AUrl);
end;

function TAnyimgCom.RemoteUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('uploadMode', 'remote');

    AddFormField('remoteFiles[]', ARemoteUrl);
  end;

  Result := Upload(LHTTPParams, AUrl);
end;

end.
