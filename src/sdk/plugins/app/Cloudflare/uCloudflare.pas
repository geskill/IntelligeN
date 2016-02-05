unit uCloudflare;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants,
  // Spring Framework
  Spring.Collections.Lists, Spring.Collections.Dictionaries,
  // RegExpr
  RegExpr,
  // BESEN
  BESEN, BESENConstants, BESENErrors, BESENValue,
  // Indy
  IdURI,
  // LkJSON
  uLkJSON,
  // MultiEvent
  Generics.MultiEvents.NotifyHandler,
  // HTTPManager
  uHTTPInterface, uHTTPConst, uHTTPManagerClasses, uHTTPClasses, uHTTPEvent,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Plugin system
  uPlugInAppClass, uPlugInHTTPClasses,
  // Utils,
  uPathUtils, uStringUtils;

type
  TCloudflare = class(TAppPlugIn)
  private
    FAppController: IAppController;
    FCookieBuffer: TDictionary<string, string>;
    FInUseCounter: Integer;
    FHTTPScrapeEventHandler: IHTTPScrapeEventHandler;
    function NeedHandle(AIHTTPResponse: IHTTPResponse): Boolean;
    procedure HandleScrape(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool);
    function ExecJavaScript(const AScript: string): string;
  public
    function GetName: WideString; override;
    function Start(const AAppController: IAppController): WordBool; override;
    function Stop: WordBool; override;
  end;

implementation

function GetModulePath: string;
var
  QueryRes: TMemoryBasicInformation;
  LBuffer: string;
begin
  VirtualQuery(@GetModulePath, QueryRes, SizeOf(QueryRes));
  SetLength(LBuffer, MAX_PATH);
  SetLength(LBuffer, GetModuleFileName(Cardinal(QueryRes.AllocationBase), PChar(LBuffer), Length(LBuffer)));
  Result := LBuffer;
end;

{ TCloudflare }

function TCloudflare.NeedHandle(AIHTTPResponse: IHTTPResponse): Boolean;
begin
  Result := (AIHTTPResponse.Server = 'cloudflare-nginx') and (Pos('/cdn-cgi/', string(AIHTTPResponse.Refresh)) > 0);
end;

procedure TCloudflare.HandleScrape(const AHTTPProcess: IHTTPProcess; out AHTTPData: IHTTPData; var AHandled: WordBool);
var
  cf_clearance, jschl_vc, pass, jschl_answer, jschl_script: string;

  LProtocol, LHost, LURL, LParams: string;
  LCanHandleCloudflare: Boolean;

  HTTPRequest: IHTTPRequest;
  HTTPParams: IHTTPParams;
  HTTPOptions: IHTTPOptions;

  LRequestID: Double;
begin
  InterlockedIncrement(FInUseCounter);
  try
    LCanHandleCloudflare := False;
    OutputDebugString(PChar('HandleScrape start'));

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
    else if NeedHandle(AHTTPProcess.HTTPResult.HTTPResponse) then
    begin
      if (Pos('why_captcha', string(AHTTPProcess.HTTPResult.SourceCode)) > 0) then
      begin
        // TODO: Handle CAPTCHA
        FAppController.LogManager.Add('Cloudflare: CAPTCHA for ' + LHost + ' detected.');
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

          LRequestID := HTTPManager.Post(HTTPRequest, HTTPParams, HTTPOptions);
        end
        else
        begin
          LRequestID := HTTPManager.Get(HTTPRequest, HTTPOptions);
        end;
        HTTPOptions := nil;
        HTTPParams := nil;
        HTTPRequest := nil;
        OutputDebugString(PChar('LRequestID: ' + IntToStr(Trunc(LRequestID))));

        repeat
          sleep(75);
        until HTTPManager.HasResult(LRequestID);

        if not NeedHandle(HTTPManager.GetResult(LRequestID).HTTPResult.HTTPResponse) then
        begin
          AHTTPProcess.HTTPResult := HTTPManager.GetResult(LRequestID).HTTPResult;
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
    OutputDebugString(PChar('HandleScrape end'));
  finally
    InterlockedDecrement(FInUseCounter);
  end;
end;

function TCloudflare.ExecJavaScript(const AScript: string): string;
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

function TCloudflare.GetName: WideString;
begin
  Result := 'Cloudflare';
end;

function TCloudflare.Start(const AAppController: IAppController): WordBool;
begin
  FInUseCounter := 0;
  Result := True;

  if Result then
  begin
    FAppController := AAppController;
    FCookieBuffer := TDictionary<string, string>.Create;
    FHTTPScrapeEventHandler := TIHTTPScrapeEventHandler.Create(HandleScrape);
    HTTPManager.OnRequestScrape.Add(FHTTPScrapeEventHandler);
  end;
end;

function TCloudflare.Stop: WordBool;
begin
  Result := (FInUseCounter = 0);

  if Result then
  begin
    HTTPManager.OnRequestScrape.Remove(FHTTPScrapeEventHandler);
    FHTTPScrapeEventHandler := nil;
    FCookieBuffer.Free;
    FAppController := nil;
  end;
end;

end.
