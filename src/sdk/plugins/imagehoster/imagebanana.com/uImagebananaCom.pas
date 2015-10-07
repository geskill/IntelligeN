unit uImagebananaCom;

interface

uses
  // Delphi
  Windows, SysUtils,
  // RegEx
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TImagebananaCom = class(TImageHosterPlugIn)
  private const
    website: string = 'http://www.imagebanana.com/';
    function Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString): WideString; override;
    function RemoteUpload(AImageUrl: WideString): WideString; override;
  end;

implementation

function TImagebananaCom.Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
const
  SearchStringBegin: string = '[url=';
  SearchStringEnd: string = ']';
var
  HTTPRequest: IHTTPRequest;

  RequestID: Double;

  ResponeStr, Text: string;
begin
  result := False;

  HTTPRequest := THTTPRequest.Create(website);
  HTTPRequest.Cookies.Add('upload_method=remote');
  HTTPRequest.Host := 'www.imagebanana.com';
  HTTPRequest.Referer := website;

  with AHTTPParams do
  begin
    AddFormField('options[thumbnail_infobar_disable]', '1');
    if not(ImageHostResize = irNone) then
    begin
      case ImageHostResize of
        ir320x240:
          AddFormField('options[resize]', '320x240');
        ir450x338:
          AddFormField('options[resize]', '450x338');
        ir640x480:
          AddFormField('options[resize]', '640x480');
        ir800x600:
          AddFormField('options[resize]', '800x600');
      end;
    end
    else
      AddFormField('options[resize]', '-0');
  end;

  RequestID := HTTPManager.Post(HTTPRequest, AHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  /// Über Location Header zu gehen bringt nichts, weil man zwar den Bildlink bekommt,
  /// aber man kommt so nicht an die Subdomain Information, also auf welchem Server
  /// sich das Bild befindet

  Text := copy(ResponeStr, Pos(SearchStringBegin, LowerCase(ResponeStr)) + length(SearchStringBegin));
  Text := copy(Text, 0, Pos(SearchStringEnd, Text) - 1);

  if not SameStr('', Text) then
  begin
    AImageUrl := StringReplace(Text, 'view', 'img', []);
    Result := True;
  end;
end;

function TImagebananaCom.GetName;
begin
  result := 'Imagebanana.com';
end;

function TImagebananaCom.LocalUpload(ALocalPath: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := '';

  HTTPParams := THTTPParams.Create(ptMultipartFormData);
  with HTTPParams do
    AddFile('upload[]', ALocalPath);

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

function TImagebananaCom.RemoteUpload;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := AImageUrl;

  HTTPParams := THTTPParams.Create(ptMultipartFormData);
  with HTTPParams do
    AddFormField('url', AImageUrl);

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

end.
