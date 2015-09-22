{ ********************************************************
  *                                                      *
  *  Filepost.com Delphi API                             *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uFilepostCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp, DateUtils,
  // Reg Ex
  RegExpr,
  // LkJSON
  uLkJSON,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TFilepostCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TFilepostCom }

function TFilepostCom.GetName: WideString;
begin
  Result := 'Filepost.com';
end;

function TFilepostCom.CheckLink(AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := lsUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;
  Result := LinkInfo;
end;

function TFilepostCom.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '\/files\/(get\/)?(\w+)';

        if Exec(InputString) then
          Result := Match[2];
      finally
        Free;
      end;
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := lsOffline;
    if (AValue = 'x') then
      Result := lsOnline;
  end;

var
  I: Integer;
  _OverAllPostReply, _Links: string;
  _systemtime: SystemTime;

  HTTPRequest: IHTTPRequest;
  HTTPParams: IHTTPParams;

  RequestID: Double;

  ResponeStr: string;
begin
  with TStringList.Create do
    try
      Text := AFiles;

      _OverAllPostReply := '';
      _Links := '';
      for I := 0 to Count - 1 do
      begin
        _Links := _Links + HTTPEncode(Strings[I]);
        if not(I = Count - 1) then
          _Links := _Links + sLineBreak;

        if (I > 0) and (I mod 100 = 0) or (I = Count - 1) then
        begin
          /// GetSystemTime(_systemtime);

          /// HTTPRequest := THTTPRequest.Create('http://filepost.com/files/checker/?SID=&JsHttpRequest=' + IntToStr
          /// (DateTimeToUnix(EncodeDateTime(_systemtime.wYear, _systemtime.wMonth, _systemtime.wDay, _systemtime.wHour, _systemtime.wMinute,
          /// _systemtime.wSecond, _systemtime.wMilliseconds))) + '1234-xml');

          HTTPRequest := THTTPRequest.Create('http://filepost.com/files/checker/?JsHttpRequest=' + IntToStr(MilliSecondsBetween(Now,
                EncodeDateTime(1970, 1, 1, 0, 0, 0, 0))) + '-xml');

          with HTTPRequest do
          begin
            // Referer := 'http://filepost.com/linkchecker/';
            CharSet := '';
            ContentType := 'application/octet-stream';
          end;

          HTTPParams := THTTPParams.Create('urls=' + _Links);

          RequestID := HTTPManager.Post(HTTPRequest, HTTPParams, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID);

          ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

          _OverAllPostReply := _OverAllPostReply + ResponeStr;
          _Links := '';
        end;
      end;

      with TRegExpr.Create do
        try
          InputString := StringReplace(_OverAllPostReply, '\', '', [rfReplaceAll]);

          for I := 0 to Count - 1 do
          begin
            Expression := '\/files\/' + GetDownloadlinkID(Strings[I]) + '\/(.*?)\/<\/a><\/td>nttt<td>([\d\.]+) (\w+)<\/td>nttt<td>ntttt<span class="(x|v)"';

            if Exec(InputString) then
              AddLink(Strings[I], HTTPDecode(Match[1]), APIResultToStatus(Match[4]), TSizeFormatter.SizeToByte(Match[2], Match[3]))
            else
              AddLink(Strings[I], '', lsOffline, 0);
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
