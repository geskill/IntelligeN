unit uPicloadOrg;

interface

uses
  // Delphi
  Windows, SysUtils,
  // RegEx
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils;

type
  TPicloadOrg = class(TImageHosterPlugIn)
  protected { . }
  const
    WEBSITE: string = 'http://picload.org/';
    function Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString; out AUrl: WideString): WordBool; override;
    function RemoteUpload(ARemoteUrl: WideString; out AUrl: WideString): WordBool; override;
  end;

implementation

{ TPicloadOrg }

function TPicloadOrg.Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString): Boolean;
var
  LHTTPRequest: IHTTPRequest;
  LHTTPOptions: IHTTPOptions;

  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
begin
  Result := False;

  LHTTPRequest := THTTPRequest.Create('http://upload.picload.org/upload.html');
  with LHTTPRequest do
  begin
    AcceptEncoding := 'deflate, identity, *;q=0';
    Referer := WEBSITE;
  end;

  with AHTTPParams do
  begin
    AddFormField('sid', '');
    AddFormField('uid', '');
    AddFormField('upsubmit', 'Upload');
  end;

  LHTTPOptions := TPlugInHTTPOptions.Create(Self);
  LHTTPOptions.UseCompressor := False;

  LRequestID := HTTPManager.Post(LHTTPRequest, AHTTPParams, LHTTPOptions);

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if (Pos('Direktlink', string(LHTTPProcess.HTTPResult.SourceCode)) > 0) then
  begin

    with TRegExpr.Create do
      try
        InputString := LHTTPProcess.HTTPResult.SourceCode;
        Expression := 'Direktlink.*?value="(.*?)"';

        if Exec(InputString) then
        begin
          AImageUrl := Match[1];
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
        InputString := string(LHTTPProcess.HTTPResult.SourceCode);
        Expression := 'class="message">(.*?)<\/';

        if Exec(InputString) then
        begin
          Self.ErrorMsg := Trim(HTML2Text(Match[1]));
        end;
      finally
        Free;
      end;
  end;
end;

function TPicloadOrg.GetName;
begin
  Result := 'Picload.org';
end;

function TPicloadOrg.LocalUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('type', 'local');
    AddFile('images[]', ALocalPath);
  end;

  Result := Upload(LHTTPParams, AUrl);
end;

function TPicloadOrg.RemoteUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('type', 'remote');
    AddFormField('links', ARemoteUrl);
  end;

  Result := Upload(LHTTPParams, AUrl);
end;

end.
