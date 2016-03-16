{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Letitbit.net Delphi API                             *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uLetitbitNet;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp,
  // Reg Ex
  RegExpr,
  // Common
  uBaseConst,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInConst, uPlugInInterface, uPlugInFileHosterClass, uPlugInFileHosterClasses, uPlugInHTTPClasses,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TLetitbitNet = class(TFileHosterPlugIn)
  protected
    function InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TLetitbitNet }

function TLetitbitNet.InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool;

  function GetDownloadlinkID(const ALink: string): string;
  begin
    Result := '';

    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '([^<>"\/]*?)\/[^<>"\/]+\.html';

        if Exec(InputString) then
        begin
          Result := Match[1];
        end;
      finally
        Free;
      end;
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
        LIDsString := LIDsString + Strings[LLinkIndex];
        if not(LLinkIndex = Count - 1) then
          LIDsString := LIDsString + sLineBreak;

        if (LLinkIndex > 0) and (LLinkIndex mod 50 = 0) or (LLinkIndex = Count - 1) then
        begin
          LHTTPRequest := THTTPRequest.Create('https://api.letitbit.net/');

          LHTTPParams := THTTPParams.Create('r=["V4uSrFRCq",["download/info",{"link":"' + StringReplace(LIDsString, sLineBreak, '"}],["download/info",{"link":"', [rfReplaceAll]) + '"}]]');

          LRequestID := HTTPManager.Post(LHTTPRequest, LHTTPParams, TPlugInHTTPOptions.Create(Self));

          HTTPManager.WaitFor(LRequestID);

          LResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

          LOverAllPostReply := LOverAllPostReply + LResponeStr;
          LIDsString := '';
        end;
      end;

      for LLinkIndex := 0 to Count - 1 do
        with TRegExpr.Create do
          try
            InputString := LOverAllPostReply;
            Expression := '"name":"([^<>"]*?)","size":(")?(\d+)(")?,"uid":"' + GetDownloadlinkID(Strings[LLinkIndex]) + '","project":"(letitbit\.net|shareflare\.net|vip\-file\.com)","md5":"([a-z0-9]{32}|0)"';

            if Exec(InputString) then
              LLinksInfo.AddLink(Strings[LLinkIndex], Match[1], csOnline, StrToInt64Def(Match[3], 0), Match[6])
            else
              LLinksInfo.AddLink(Strings[LLinkIndex], '', csOffline, 0);
          finally
            Free;
          end;
    finally
      Free;
    end;

  ALinksInfo := LLinksInfo;
end;

function TLetitbitNet.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TLetitbitNet.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TLetitbitNet.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TLetitbitNet.GetName: WideString;
begin
  Result := 'Letitbit.net';
end;

end.
