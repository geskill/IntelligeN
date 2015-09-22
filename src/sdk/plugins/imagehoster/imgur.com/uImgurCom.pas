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
  private const
    website: string = 'http://imgur.com/';
    function Upload(AHTTPParams: IHTTPParams; out AImageUrl: string; AFileExt: string): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString): WideString; override;
    function RemoteUpload(AImageUrl: WideString): WideString; override;
  end;

implementation

{ TImgurCom }

function TImgurCom.Upload(AHTTPParams: IHTTPParams; out AImageUrl: string; AFileExt: string): Boolean;
var
  HTTPRequest: IHTTPRequest;

  RequestID: Double;

  sid_hash: string;
begin
  Result := False;

  HTTPRequest := THTTPRequest.Create(website);
  HTTPRequest.CustomHeaders.Add('X-Requested-With: XMLHttpRequest');
  HTTPRequest.Referer := website;

  RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  with TRegExpr.Create do
    try
      InputString := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;
      Expression := 'sid_hash   = ''(.*?)''';

      if Exec(InputString) then
        sid_hash := Match[1];
    finally
      Free;
    end;

  with AHTTPParams do
  begin
    AddFormField('create_album', '0');
    AddFormField('album_title', 'Optional+Album+Title');
    AddFormField('album_layout', 'b');
    AddFormField('edit_url', '0');
    AddFormField('catify', '0');
  end;

  RequestID := HTTPManager.Post(website + 'upload?sid_hash=' + sid_hash, RequestID, AHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  with TRegExpr.Create do
    try
      InputString := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;
      Expression := '"hash":"(.*?)"';

      if Exec(InputString) then
      begin
        AImageUrl := 'http://i.imgur.com/' + Match[1] + AFileExt ;
        Result := True;
      end;

      // http://i.imgur.com/voxuS.png
    finally
      Free;
    end;
end;

function TImgurCom.GetName: WideString;
begin
  Result := 'Imgur.com';
end;

function TImgurCom.LocalUpload(ALocalPath: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := '';

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
    AddFile('Filedata', ALocalPath);

  if Upload(HTTPParams, LImageUrl, ExtractFileExt(ALocalPath)) then
    Result := LImageUrl;
end;

function TImgurCom.RemoteUpload(AImageUrl: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := AImageUrl;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
    AddFormField('url', AImageUrl);

  if Upload(HTTPParams, LImageUrl, ExtractFileExt(AImageUrl)) then
    Result := LImageUrl;
end;

end.
