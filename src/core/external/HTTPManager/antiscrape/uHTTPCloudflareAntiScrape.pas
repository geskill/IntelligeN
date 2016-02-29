unit uHTTPCloudflareAntiScrape;

interface

uses
  // Interface
  uHTTPInterface,
  // Classes
  uHTTPManager, uHTTPManagerClasses, uHTTPClasses, uHTTPExtensionClasses,
  // Const
  uHTTPConst,
  // RegExpr
  RegExpr,
  // BESEN
  BESEN, BESENConstants, BESENErrors, BESENValue,
  // Delphi
  Windows, SysUtils, Generics.Collections,
  // Indy
  IdURI;

type
  THTTPCloudflareAntiScrape = class(THTTPAntiScrape)
  private
    FCookieBuffer: TDictionary<string, string>;

    function NeedToHandle(const AIHTTPResponse: IHTTPResponse): Boolean;
    function ExecJavaScript(const AScript: string): string;

  public
    constructor Create;
    destructor Destroy; override;

    class function GetExtensionName: string; override;
    procedure Handle(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool); override;
  end;

implementation

{ THTTPCloudflareAntiScrape }

function THTTPCloudflareAntiScrape.NeedToHandle(const AIHTTPResponse: IHTTPResponse): Boolean;
begin
  Result := (AIHTTPResponse.Server = 'cloudflare-nginx') and (Pos('/cdn-cgi/', string(AIHTTPResponse.Refresh)) > 0);
end;

function THTTPCloudflareAntiScrape.ExecJavaScript(const AScript: string): string;
var
  LCompatibility: LongWord;
  LInstance: TBESEN;
  LValue: TBESENValue;
begin
  Result := '';

  LCompatibility := COMPAT_BESEN;
  LInstance := TBESEN.Create(LCompatibility);
  try
    LValue.ValueType := bvtUNDEFINED;
    LValue.Obj := nil;
    LValue := TBESEN(LInstance).Execute(AScript);
    try
      Result := IntToStr(Round(LValue.Num));

      LValue.ValueType := bvtUNDEFINED;
      LValue.Obj := nil;
      TBESEN(LInstance).GarbageCollector.CollectAll;
    except
      on e: EBESENError do
      begin
        OutputDebugString(PChar(e.Name + '(' + IntToStr(TBESEN(LInstance).LineNumber) + '): ' + e.Message));
      end;
      on e: Exception do
      begin
        OutputDebugString(PChar('Exception(' + IntToStr(TBESEN(LInstance).LineNumber) + '): ' + e.Message));
      end;
    end;
  finally
    LInstance.Free;
  end;
end;

constructor THTTPCloudflareAntiScrape.Create;
begin
  inherited Create;
  FCookieBuffer := TDictionary<string, string>.Create;
end;

destructor THTTPCloudflareAntiScrape.Destroy;
begin
  FCookieBuffer.Free;
  inherited Destroy;
end;

class function THTTPCloudflareAntiScrape.GetExtensionName: string;
begin
  Result := 'Cloudflare';
end;

procedure THTTPCloudflareAntiScrape.Handle(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool);
var
  cf_clearance, jschl_vc, pass, jschl_answer, jschl_script: string;

  LProtocol, LHost, LURL, LParams: string;
  LCanHandleCloudflare: Boolean;

  HTTPRequest: IHTTPRequest;
  HTTPParams: IHTTPParams;
  HTTPOptions: IHTTPOptions;

  LRequestID: Double;
begin
  BeginUse;
  try

    LCanHandleCloudflare := False;

    with TIdURI.Create(AHTTPProcess.HTTPData.Website) do
      try
        LProtocol := Protocol;
        LHost := Host;
      finally
        Free;
      end;

    if (AHTTPProcess.HTTPData.HTTPRequest.Cookies.IndexOfName('cf_clearance') > 0) then
    begin
      FCookieBuffer.AddOrSetValue(LHost, AHTTPProcess.HTTPData.HTTPRequest.Cookies.Values['cf_clearance']);
    end
    else if NeedToHandle(AHTTPProcess.HTTPResult.HTTPResponse) then
    begin
      if (Pos('why_captcha', string(AHTTPProcess.HTTPResult.SourceCode)) > 0) then
      begin
        // TODO: Handle CAPTCHA
        Exit;
      end;

      if FCookieBuffer.ContainsKey(LHost) then
      begin
        cf_clearance := FCookieBuffer[LHost];
        FCookieBuffer.Remove(LHost);

        HTTPRequest := THTTPRequest.Clone(AHTTPProcess.HTTPData.HTTPRequest);
        with HTTPRequest do
        begin
          Cookies.Add('cf_clearance=' + cf_clearance);
        end;
        HTTPOptions := THTTPOptions.Clone(AHTTPProcess.HTTPData.HTTPOptions);
        if (HTTPRequest.Method = mPOST) then
        begin
          HTTPParams := THTTPParams.Clone(AHTTPProcess.HTTPData.HTTPParams);

          LRequestID := THTTPManager.Instance().Post(HTTPRequest, HTTPParams, HTTPOptions);
        end
        else
        begin
          LRequestID := THTTPManager.Instance().Get(HTTPRequest, HTTPOptions);
        end;
        HTTPOptions := nil;
        HTTPParams := nil;
        HTTPRequest := nil;

        THTTPManager.Instance().WaitFor(LRequestID);

        if not NeedToHandle(THTTPManager.Instance().GetResult(LRequestID).HTTPResult.HTTPResponse) then
        begin
          AHTTPProcess.HTTPResult := THTTPManager.Instance().GetResult(LRequestID).HTTPResult;
          FCookieBuffer.AddOrSetValue(LHost, cf_clearance);
          Exit;
        end;
      end;

      with TRegExpr.Create do
        try
          InputString := AHTTPProcess.HTTPResult.SourceCode;

          Expression := 'name="jschl_vc" value="(\w+)"';
          if Exec(InputString) then
          begin
            jschl_vc := Match[1];
          end;

          Expression := 'name="pass" value="(.*?)"';
          if Exec(InputString) then
          begin
            pass := Match[1];
          end;

          Expression := 'setTimeout\(function\(\){\s+(var t,r,a,f.+?\r?\n.+?a\.value =.+?)\r?\n';
          if Exec(InputString) then
          begin
            jschl_script := Match[1];

            InputString := jschl_script;
            Expression := 'a\.value =(.+?) \+ .+?;';

            jschl_script := Replace(InputString, '$1', True);

            ModifierM := True;
            InputString := jschl_script;
            Expression := '\s{3,}([a-z]{1}.*?$)';

            jschl_script := Replace(InputString, '', False);

            jschl_answer := IntToStr(StrToInt(ExecJavaScript(jschl_script)) + Length(LHost));

            LCanHandleCloudflare := True;
          end;
        finally
          Free;
        end;

      if LCanHandleCloudflare then
      begin
        sleep(5000);

        LParams := 'jschl_vc=' + jschl_vc + '&jschl_answer=' + jschl_answer + '&pass=' + pass;
        LURL := LProtocol + '://' + LHost;

        HTTPRequest := THTTPRequest.Create(LURL + '/cdn-cgi/l/chk_jschl?' + LParams);
        with HTTPRequest do
        begin
          Method := mGET;
          Referer := LURL;
          Cookies.Add('__cfduid=' + AHTTPProcess.HTTPResult.HTTPResponse.Cookies.Values['__cfduid']);
        end;

        HTTPOptions := AHTTPProcess.HTTPData.HTTPOptions;
        with HTTPOptions do
        begin
          HandleRedirects := True;
          RedirectMaximum := 5;
        end;
        AHTTPData := THTTPData.Create(HTTPRequest, HTTPOptions, nil);

        AHandled := True;
      end;
    end;
  finally
    EndUse;
  end;
end;

initialization

THTTPManager.Instance().AntiScrapeManager.Register(THTTPCloudflareAntiScrape.Create);

finalization

THTTPManager.Instance().AntiScrapeManager.Unregister(THTTPCloudflareAntiScrape.GetExtensionName);

end.
