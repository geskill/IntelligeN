unit uPicfrontOrg;

interface

uses
  // Delphi
  Windows, SysUtils,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TPicfrontOrg = class(TImageHosterPlugIn)
  private
    function Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString): WideString; override;
    function RemoteUpload(AImageUrl: WideString): WideString; override;
  end;

implementation

{ TPicfrontOrg }

function TPicfrontOrg.Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
const
  SearchStringBegin: string = '[IMG]';
  SearchStringEnd: string = '[/IMG]';
var
  RequestID: Double;

  ResponeStr, Text: string;
begin
  result := False;

  with AHTTPParams do
  begin
    AddFormField('SID', '');
    AddFormField('HOST_WWW', '0');
    AddFormField('HOST_END', 'org');
    AddFormField('UPLOAD_IDENTIFIER', '');
    AddFormField('urlimage', '');
    if not(ImageHostResize = irNone) then
    begin
      case ImageHostResize of
        ir320x240:
          begin
            AddFormField('resize_x', '320');
            AddFormField('resize_y', '240');
          end;
        ir450x338:
          begin
            AddFormField('resize_x', '450');
            AddFormField('resize_y', '338');
          end;
        ir640x480:
          begin
            AddFormField('resize_x', '640');
            AddFormField('resize_y', '480');
          end;
        ir800x600:
          begin
            AddFormField('resize_x', '800');
            AddFormField('resize_y', '600');
          end;
      end;
    end;
  end;

  RequestID := HTTPManager.Post(THTTPRequest.Create('http://www12.picfront.org/index.php'), AHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  Text := copy(ResponeStr, Pos(SearchStringBegin, ResponeStr) + length(SearchStringBegin));
  Text := copy(Text, 0, Pos(SearchStringEnd, Text) - 1);

  if not(Text = '') then
  begin
    AImageUrl := StringReplace(Text, '/thb/', '/img/', []);
    Result := True;
  end;
end;

function TPicfrontOrg.GetName;
begin
  result := 'Picfront.org';
end;

function TPicfrontOrg.LocalUpload(ALocalPath: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := '';

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
    AddFile('file_0', ALocalPath);

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

function TPicfrontOrg.RemoteUpload;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := AImageUrl;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
    AddFormField('urluploadfile_0', AImageUrl);

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

end.
