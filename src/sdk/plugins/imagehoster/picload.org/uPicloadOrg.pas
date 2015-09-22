unit uPicloadOrg;

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
  uPlugInImageHosterClass, uPlugInHTTPClasses;

type
  TPicloadOrg = class(TImageHosterPlugIn)
  private const
    website: string = 'http://picload.org/';
    function Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString): WideString; override;
    function RemoteUpload(AImageUrl: WideString): WideString; override;
  end;

implementation

{ TPicloadOrg }

function TPicloadOrg.Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
var
  HTTPRequest: IHTTPRequest;
  HTTPOptions: IHTTPOptions;

  RequestID: Double;
begin
  Result := False;

  if not(Pos('picload.org/', string(AImageUrl)) > 0) then
  begin
    HTTPRequest := THTTPRequest.Create('http://upload.picload.org/upload.html');
    with HTTPRequest do
    begin
      AcceptEncoding := 'deflate, identity, *;q=0';
      Referer := website;
    end;

    with AHTTPParams do
    begin
      AddFormField('sid', '');
      AddFormField('uid', '');
      AddFormField('upsubmit', 'Upload');
    end;

    HTTPOptions := TPlugInHTTPOptions.Create(Self);
    HTTPOptions.UseCompressor := False;

    RequestID := HTTPManager.Post(HTTPRequest, AHTTPParams, HTTPOptions);

    repeat
      sleep(50);
    until HTTPManager.HasResult(RequestID);

    with TRegExpr.Create do
      try
        InputString := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;
        Expression := '<b>Direktlink</b><br />.*?value="(.*?)"';

        if Exec(InputString) then
        begin
          AImageUrl := Match[1];
          Result := True;
        end;
      finally
        Free;
      end;
  end
  else
    ErrorMsg := 're-upload to this hoster is forbidden!';
end;

function TPicloadOrg.GetName;
begin
  Result := 'Picload.org';
end;

function TPicloadOrg.LocalUpload(ALocalPath: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := '';

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFormField('type', 'local');
    AddFile('images[]', ALocalPath);
  end;

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

function TPicloadOrg.RemoteUpload;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := AImageUrl;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFormField('type', 'remote');
    AddFormField('links', AImageUrl);
  end;

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

end.
