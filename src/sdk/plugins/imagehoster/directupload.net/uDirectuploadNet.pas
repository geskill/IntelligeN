unit uDirectuploadNet;

interface

uses
  // Delphi
  Windows, SysUtils,
  // RegEx
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses;

type
  TDirectuploadNet = class(TImageHosterPlugIn)
  private const
    website: string = 'http://www.directupload.net/';
    function Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString): WideString; override;
    function RemoteUpload(AImageUrl: WideString): WideString; override;
  end;

implementation

{ TDirectuploadNet }

function TDirectuploadNet.Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
const
  SearchStringBegin: string = '[URL=http://www.directupload.net][IMG]';
  SearchStringEnd: string = '[/IMG]';
var
  HTTPRequest: IHTTPRequest;

  RequestID: Double;

  ResponeStr, Text: string;
begin
  Result := False;

  HTTPRequest := THTTPRequest.Create(website + 'index.php?mode=upload');
  HTTPRequest.Referer := website;

  with AHTTPParams do
    AddFormField('f_up_form', '');

  RequestID := HTTPManager.Post(HTTPRequest, AHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  Text := copy(ResponeStr, Pos(SearchStringBegin, ResponeStr) + length(SearchStringBegin));
  Text := copy(Text, 0, Pos(SearchStringEnd, string(Text)) - 1);

  if not SameStr('', Text) then
  begin
    AImageUrl := Text;
    Result := True;
  end;
end;

function TDirectuploadNet.GetName: WideString;
begin
  Result := 'Directupload.net';
end;

function TDirectuploadNet.LocalUpload(ALocalPath: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := '';

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFormField('input', 'file');
    AddFile('bilddatei', ALocalPath);
  end;

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

function TDirectuploadNet.RemoteUpload(AImageUrl: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := AImageUrl;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFormField('input', 'url');
    AddFormField('image_link', AImageUrl);
  end;

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

end.
