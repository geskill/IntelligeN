unit uDdlwOrg;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TDdlwOrg = class(TImageHosterPlugIn)
  private const
    website: string = 'http://ddlw.org/';
    function Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString): WideString; override;
    function RemoteUpload(AImageUrl: WideString): WideString; override;
  end;

implementation

{ TDdlwOrg }

function TDdlwOrg.Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
var
  HTTPRequest: IHTTPRequest;
  HTTPOptions: IHTTPOptions;

  RequestID: Double;
begin
  Result := False;

  HTTPRequest := THTTPRequest.Create(website + 'upload.py');
  HTTPRequest.Referer := website;

  with AHTTPParams do
  begin
    if not(ImageHostResize = irNone) then
    begin
      case ImageHostResize of
        ir320x240:
          AddFormField('imgsize', '240');
        ir450x338:
          AddFormField('imgsize', '338');
        ir640x480:
          AddFormField('imgsize', '480');
        ir800x600:
          AddFormField('imgsize', '600');
      end;

    end
    else
      AddFormField('imgsize', 'original');
  end;

  HTTPOptions := TPlugInHTTPOptions.Create(Self);
  HTTPOptions.HandleSketchyRedirects := False;
  HTTPOptions.RedirectMaximum := 0;

  RequestID := HTTPManager.Post(HTTPRequest, AHTTPParams, HTTPOptions);

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  if not SameStr('', HTTPManager.GetResult(RequestID).HTTPResult.HTTPResponse.Location) then
  begin
    AImageUrl := HTTPManager.GetResult(RequestID).HTTPResult.HTTPResponse.Location;
    Result := True;
  end;
end;

function TDdlwOrg.GetName: WideString;
begin
  Result := 'Ddlw.org';
end;

function TDdlwOrg.LocalUpload(ALocalPath: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := '';

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
    AddFile('file', ALocalPath);

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

function TDdlwOrg.RemoteUpload(AImageUrl: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := AImageUrl;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
    AddFormField('url', AImageUrl);

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

end.
