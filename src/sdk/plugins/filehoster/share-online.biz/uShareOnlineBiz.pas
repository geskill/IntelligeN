{ ********************************************************
  *                                                      *
  *  Share-online.biz Delphi API                         *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
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
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TShareOnlineBiz = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TShareOnlineBiz }

function TShareOnlineBiz.GetName: WideString;
begin
  Result := 'Share-online.biz';
end;

function TShareOnlineBiz.CheckLink(AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := csUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;
  Result := LinkInfo;
end;

function TShareOnlineBiz.CheckLinks(AFiles: WideString): Integer;

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
  LFileIndex: Integer;
  LOverAllPostReply, LIDsString: string;

  LHTTPParams: IHTTPParams;

  LRequestID: Double;

  ResponeStr: string;

  LResultList: TStringList;
begin
  with TStringList.Create do
    try
      Text := AFiles;

      LOverAllPostReply := '';
      LIDsString := '';
      for LFileIndex := 0 to Count - 1 do
      begin
        LIDsString := LIDsString + GetDownloadlinkID(Strings[LFileIndex]);
        if not(LFileIndex = Count - 1) then
          LIDsString := LIDsString + sLineBreak;

        if (length(LIDsString) > 200) or (LFileIndex = Count - 1) then
        begin
          LHTTPParams := THTTPParams.Create('links=' + LIDsString);

          LRequestID := HTTPManager.Post(THTTPRequest.Create('http://api.share-online.biz/cgi-bin?q=checklinks&md5=1'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(LRequestID);

          ResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

          LOverAllPostReply := LOverAllPostReply + ResponeStr;
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
              for LFileIndex := 0 to Count - 1 do
              begin
                if SameText(GetDownloadlinkID(Strings[LFileIndex]), Match[1]) or SameText(GetDownloadlinkID(Strings[LFileIndex]), Match[6]) then
                begin
                  if SameText(Match[6], '') then
                    AddLink(Strings[LFileIndex], Match[3], APIResultToStatus(Match[2]), StrToInt64Def(Match[4], 0), Match[5])
                  else
                    AddLink(Strings[LFileIndex], '', APIResultToStatus(Match[6]), 0);
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
  Result := FCheckedLinksList.Count;
end;

end.
