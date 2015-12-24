unit uImgurCom;

interface

uses
  // Delphi
  Windows, SysUtils,
  // RegEx
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TImgurCom = class(TImageHosterPlugIn)
  protected { . }
  const
    WEBSITE: string = 'http://imgur.com/';
    function Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString; AFileExt: string): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(const ALocalPath: WideString; out AUrl: WideString): WordBool; override;
    function RemoteUpload(const ARemoteUrl: WideString; out AUrl: WideString): WordBool; override;
  end;

implementation

{ TImgurCom }

function TImgurCom.Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString; AFileExt: string): Boolean;
var
  LHTTPRequest: IHTTPRequest;

  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
begin
  Result := False;

  LHTTPRequest := THTTPRequest.Create(WEBSITE + 'upload');
  with LHTTPRequest do
  begin
    CustomHeaders.Add('X-Requested-With: XMLHttpRequest');
    Referer := WEBSITE;
  end;

  with AHTTPParams do
  begin
    AddFormField('create_album', '0');
    AddFormField('album_title', 'Optional+Album+Title');
    AddFormField('album_layout', 'b');
    AddFormField('edit_url', '0');
    AddFormField('catify', '0');
  end;

  LRequestID := HTTPManager.Post(LHTTPRequest, AHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if (Pos('hash', string(LHTTPProcess.HTTPResult.SourceCode)) > 0) then
  begin

    with TRegExpr.Create do
      try
        InputString := LHTTPProcess.HTTPResult.SourceCode;
        Expression := '"hash":"(.*?)"';

        if Exec(InputString) then
        begin
          AImageUrl := 'http://i.imgur.com/' + Match[1] + AFileExt;
          Result := True;
        end;

        // http://i.imgur.com/voxuS.png
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
        InputString := string(LHTTPProcess.HTTPResult.SourceCode);
        Expression := '"message":"(.*?)"';

        if Exec(InputString) then
        begin
          Self.ErrorMsg := Match[1];
        end;
      finally
        Free;
      end;
  end;
end;

function TImgurCom.GetName: WideString;
begin
  Result := 'Imgur.com';
end;

function TImgurCom.LocalUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
    AddFile('Filedata', ALocalPath);

  Result := Upload(LHTTPParams, AUrl, ExtractFileExt(ALocalPath));
end;

function TImgurCom.RemoteUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
    AddFormField('url', ARemoteUrl);

  Result := Upload(LHTTPParams, AUrl, ExtractFileExt(ARemoteUrl));
end;

end.
