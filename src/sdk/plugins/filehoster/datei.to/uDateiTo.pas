{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Datei.to Delphi API                                 *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uDateiTo;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp,
  // Reg Ex
  RegExpr,
  // LkJSON
  uLkJSON,
  // Common
  uBaseConst,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInConst, uPlugInInterface, uPlugInFileHosterClass, uPlugInFileHosterClasses, uPlugInHTTPClasses,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TDateiTo = class(TFileHosterPlugIn)
  protected
    function InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TDateiTo }

function TDateiTo.InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool;

  function GetDownloadlinkID(const ALink: string): string;
  begin
    Result := '';

    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '\?([a-zA-Z0-9]+)';

        if Exec(InputString) then
        begin
          Result := Match[1];
        end;
      finally
        Free;
      end;
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := csOffline;
    if (AValue = 'online') then
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
          LIDsString := LIDsString + ';';

        if (LLinkIndex > 0) and (LLinkIndex mod 50 = 0) or (LLinkIndex = Count - 1) then
        begin
          LHTTPRequest := THTTPRequest.Create('http://datei.to/api/jdownloader/');

          LHTTPParams := THTTPParams.Create('op=check&file=' + LIDsString);

          LRequestID := HTTPManager.Post(LHTTPRequest, LHTTPParams, TPlugInHTTPOptions.Create(Self));

          HTTPManager.WaitFor(LRequestID);

          LResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

          LOverAllPostReply := LOverAllPostReply + LResponeStr;
          LIDsString := '';
        end;
      end;

      with TRegExpr.Create do
        try
          InputString := LOverAllPostReply;
          Expression := '(\w+);([^;]+);([^<>\\"\/;]*?);(\d+)|(\w+);(\w+);';

          if Exec(InputString) then
          begin
            repeat
              for LLinkIndex := 0 to Count - 1 do
              begin
                if SameText(GetDownloadlinkID(Strings[LLinkIndex]), Match[1]) or SameText(GetDownloadlinkID(Strings[LLinkIndex]), Match[5]) then
                begin
                  if SameText(Match[5], '') then
                    LLinksInfo.AddLink(Strings[LLinkIndex], Match[3], APIResultToStatus(Match[2]), StrToInt64Def(Match[4], 0))
                  else
                    LLinksInfo.AddLink(Strings[LLinkIndex], '', APIResultToStatus(Match[5]), 0);
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

function TDateiTo.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TDateiTo.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TDateiTo.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TDateiTo.GetName: WideString;
begin
  Result := 'Datei.to';
end;

end.
