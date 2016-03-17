{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Uploaded.net Delphi API                             *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uUploadedNet;

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
  TUploadedNet = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
    function InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

function GetDownloadlinkID(ALink: string): string;
begin
  ALink := StringReplace(ALink, 'uploaded.to', 'uploaded.net', [rfReplaceAll, rfIgnoreCase]);
  ALink := StringReplace(ALink, 'ul.to/', 'uploaded.net/file/', [rfReplaceAll, rfIgnoreCase]);
  ALink := StringReplace(ALink, '/?id=', '/file/', [rfReplaceAll, rfIgnoreCase]);
  ALink := StringReplace(ALink, '?id=', 'file/', [rfReplaceAll, rfIgnoreCase]);

  with TRegExpr.Create do
    try
      InputString := IncludeTrailingUrlDelimiter(ALink);
      Expression := 'uploaded.net/file/(.*?)/';

      if Exec(InputString) then
        Result := Match[1];
    finally
      Free;
    end;
end;

{ TUploadedTo }

function TUploadedNet.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
var
  LHTTPRequest: IHTTPRequest;
  LRequestID: Double;
  LResponeStr: string;
begin
  ALinkInfo := TLinkInfo.Create;

  LHTTPRequest := THTTPRequest.Create('http://uploaded.net/file/' + GetDownloadlinkID(AFile) + '/status');

  LRequestID := HTTPManager.Get(LHTTPRequest, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

  if (HTTPManager.GetResult(LRequestID).HTTPResult.HTTPResponse.Code = 404) or (HTTPManager.GetResult(LRequestID).HTTPResult.HTTPResponse.Code = 410) then
  begin
    ALinkInfo.Status := csOffline
  end
  else
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := '(.*?)\s+([\d,]+) (\w+)';

        if Exec(InputString) then
        begin
          ALinkInfo.Status := csOnline;
          ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3], False);
          ALinkInfo.FileName := Match[1];
        end;
      finally
        Free;
      end;

  Result := True;
end;

function TUploadedNet.InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := csOffline;
    if (AValue = 'online') then
      Result := csOnline;
  end;

var
  LLinkIndex, LLinkPostIndex, LRetryIndex: Integer;
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

      LHTTPParams := THTTPParams.Create;
      LLinkPostIndex := 0;
      for LLinkIndex := 0 to Count - 1 do
      begin
        LHTTPParams.AddFormField('id_' + IntToStr(LLinkPostIndex), GetDownloadlinkID(Strings[LLinkIndex]));
        Inc(LLinkPostIndex);

        if (LLinkIndex > 0) and (LLinkIndex mod 80 = 0) or (LLinkIndex = Count - 1) then
        begin
          LHTTPParams.AddFormField('apikey', 'lhF2IeeprweDfu9ccWlxXVVypA5nA3EL');
          LRetryIndex := 0;

          while LRetryIndex < 5 do
          begin
            try
              LRequestID := HTTPManager.Post(THTTPRequest.Create('http://uploaded.net/api/filemultiple'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

              HTTPManager.WaitFor(LRequestID);

              LResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

              if SameText(LResponeStr, '') then
              begin
                Inc(LRetryIndex);
                sleep(550);
              end
              else
                break;
            except

            end;
          end;

          LOverAllPostReply := LOverAllPostReply + LResponeStr;
          LHTTPParams := THTTPParams.Create;
          LLinkPostIndex := 0;
        end;
      end;

      with TRegExpr.Create do
        try
          InputString := LOverAllPostReply;
          Expression := '(\w+),(\w+),(\d*?),(\w*?),(.*)';

          if Exec(InputString) then
          begin
            repeat
              for LLinkIndex := 0 to Count - 1 do
              begin
                if SameText(GetDownloadlinkID(Strings[LLinkIndex]), Match[2]) then
                begin
                  LLinksInfo.AddLink(Strings[LLinkIndex], Match[5], APIResultToStatus(Match[1]), StrToInt64Def(Match[3], 0), Match[4]);
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

function TUploadedNet.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TUploadedNet.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TUploadedNet.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TUploadedNet.GetName: WideString;
begin
  Result := 'Uploaded.net';
end;

end.
