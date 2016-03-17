{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Share-online.biz Delphi API                         *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uShareOnlineBiz;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp,
  // Reg Ex
  RegExpr,
  // Common
  uBaseConst,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // plugin system
  uPlugInConst, uPlugInInterface, uPlugInFileHosterClass, uPlugInFileHosterClasses, uPlugInHTTPClasses,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TShareOnlineBiz = class(TFileHosterPlugIn)
  protected
    function InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TShareOnlineBiz }

function TShareOnlineBiz.InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool;

  function GetDownloadlinkID(ALink: string): string;
  begin
    Result := '';

    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '\/dl\/([a-zA-Z0-9]+)';

        if Exec(InputString) then
        begin
          Result := Match[1];
        end
        else
        begin
          Expression := 'id=([a-zA-Z0-9]+)';

          if Exec(InputString) then
            Result := Match[1];
        end;
      finally
        Free;
      end;
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := csOffline;
    if (AValue = 'OK') then
      Result := csOnline;
  end;

var
  LLinkIndex: Integer;
  LLinksInfo: TLinksInfo;

  LHTTPRequest: IHTTPRequest;
  LHTTPParams: IHTTPParams;
  LRequestID: Double;

  LResponeStr, LIDsString, LOverAllPostReply: string;
begin
  Result := True;

  LLinksInfo := TLinksInfo.Create;

  with TStringList.Create do
    try
      Text := AFiles;

      LOverAllPostReply := '';
      LIDsString := '';
      for LLinkIndex := 0 to Count - 1 do
      begin
        LIDsString := LIDsString + GetDownloadlinkID(Strings[LLinkIndex]);
        if not(LLinkIndex = Count - 1) then
          LIDsString := LIDsString + sLineBreak;

        if (length(LIDsString) > 200) or (LLinkIndex = Count - 1) then
        begin
          LHTTPParams := THTTPParams.Create('links=' + LIDsString);

          LHTTPRequest := THTTPRequest.Create('http://api.share-online.biz/cgi-bin?q=checklinks&md5=1');

          LRequestID := HTTPManager.Post(LHTTPRequest, LHTTPParams, TPlugInHTTPOptions.Create(Self));

          HTTPManager.WaitFor(LRequestID);

          LResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

          LOverAllPostReply := LOverAllPostReply + LResponeStr;
          LIDsString := '';
        end;
      end;

      with TRegExpr.Create do
        try
          ModifierS := False;

          InputString := LOverAllPostReply;
          Expression := '(\w+);(\w+);(.*?);(\d+);(\w+)|(\w+);(\w+);';

          // 07LZU12G89;NOTFOUND;
          // L65EJ6WX8K9P;OK;Autos.part1.rar;524288000;1d0c8fecbf25b0a0690ec9db644b9a99

          if Exec(InputString) then
          begin
            repeat
              for LLinkIndex := 0 to Count - 1 do
              begin
                if SameText(GetDownloadlinkID(Strings[LLinkIndex]), Match[1]) or SameText(GetDownloadlinkID(Strings[LLinkIndex]), Match[6]) then
                begin
                  if SameStr(Match[6], '') then
                    LLinksInfo.AddLink(Strings[LLinkIndex], Match[3], APIResultToStatus(Match[2]), StrToInt64Def(Match[4], 0), Match[5])
                  else
                    LLinksInfo.AddLink(Strings[LLinkIndex], '', APIResultToStatus(Match[6]), 0);
                  break;
                end;
              end;
            until not ExecNext;
          end;
        finally
          Free;
        end;

    finally
      Free;
    end;

  ALinksInfo := LLinksInfo;
end;

function TShareOnlineBiz.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TShareOnlineBiz.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TShareOnlineBiz.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TShareOnlineBiz.GetName: WideString;
begin
  Result := 'Share-online.biz';
end;

end.
