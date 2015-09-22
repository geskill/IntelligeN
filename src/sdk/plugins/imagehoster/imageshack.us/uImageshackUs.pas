unit uImageshackUs;

interface

uses
  // Delphi
  Windows,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TImageshackUs = class(TImageHosterPlugIn)
  private const
    website: string = 'http://post.imageshack.us/';
    function Upload(AHTTPParams: IHTTPParams; out AImageUrl: string; ARemotePath: string = ''): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString): WideString; override;
    function RemoteUpload(AImageUrl: WideString): WideString; override;
  end;

implementation

{ TImageshackUs }

function TImageshackUs.Upload(AHTTPParams: IHTTPParams; out AImageUrl: string; ARemotePath: string): Boolean;
const
  SearchStringBegin: string = 'onClick="return false;" onDoubleClick="return false;" readonly="readonly" class="readonly" value="';
  SearchStringEnd: string = '"';
var
  RequestID: Double;

  ResponeStr, Text: string;

  _startpos: Integer;
begin
  result := False;

  with AHTTPParams do
  begin
    AddFormField('uploadtype', 'on');
    if not(ImageHostResize = irNone) then
    begin
      case ImageHostResize of
        ir320x240:
          AddFormField('optsize', '320x320');
        ir450x338:
          AddFormField('optsize', '450x338');
        ir640x480:
          AddFormField('optsize', '640x640');
        ir800x600:
          AddFormField('optsize', '800x800');
      end;
    end;
    AddFormField('key', '07AHPRSUb540e13d31461ec4d69d5fc122bd5ab7');
    AddFormField('rembar', '1');
  end;

  RequestID := HTTPManager.Post(THTTPRequest.Create(website + ARemotePath), AHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  _startpos := Pos(SearchStringBegin, ResponeStr);

  if not(_startpos = 0) then
  begin
    Text := copy(ResponeStr, _startpos + length(SearchStringBegin));
    AImageUrl := copy(Text, 0, Pos(SearchStringEnd, Text) - 1);
    result := True;
  end
  else
    with TRegExpr.Create do
      try
        InputString := ResponeStr;
        Expression := 'color="red">(.*?)<\/';

        if Exec(InputString) then
          Self.ErrorMsg := Match[1];
      finally
        Free;
      end;
end;

function TImageshackUs.GetName;
begin
  result := 'Imageshack.us';
end;

function TImageshackUs.LocalUpload(ALocalPath: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  result := '';

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
    AddFile('fileupload', ALocalPath);

  if Upload(HTTPParams, LImageUrl) then
    result := LImageUrl;
end;

function TImageshackUs.RemoteUpload;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  result := AImageUrl;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
    AddFormField('url', AImageUrl);

  if Upload(HTTPParams, LImageUrl, 'transload.php') then
    result := LImageUrl;
end;

end.
