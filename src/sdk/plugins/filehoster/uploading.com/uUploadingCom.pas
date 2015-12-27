{ ********************************************************
  *                                                      *
  *  Uploading.com Delphi API                            *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uUploadingCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp, DateUtils,
  // Reg Ex
  RegExpr,
  // Common
  uBaseConst,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TUploadingCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(const AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(const AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TUploadingCom }

function TUploadingCom.GetName: WideString;
begin
  Result := 'Uploading.com';
end;

function TUploadingCom.CheckLink(const AFile: WideString): TLinkInfo;
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

function TUploadingCom.CheckLinks(const AFiles: WideString): Integer;

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
    Result := csOffline;
    if (AValue = 'ok') then
      Result := csOnline;
  end;

var
  I: Integer;
  _OverAllPostReply, _Links: string;

  HTTPRequest: IHTTPRequest;
  HTTPParams: IHTTPParams;
  RequestID: Double;

  ResponeStr: string;
begin
  HTTPRequest := THTTPRequest.Create('http://uploading.com/filechecker/?ajax');
  HTTPRequest.Accept := 'application/json, text/javascript, */*; q=0.01';
  HTTPRequest.CustomHeaders.Add('X-Requested-With: XMLHttpRequest');

  with TStringList.Create do
    try
      Text := AFiles;

      _OverAllPostReply := '';
      _Links := '';
      for I := 0 to Count - 1 do
      begin
        _Links := _Links + Strings[I];
        if not(I = Count - 1) then
          _Links := _Links + sLineBreak;

        if (I > 0) and (I mod 100 = 0) or (I = Count - 1) then
        begin
          HTTPParams := THTTPParams.Create;
          with HTTPParams do
            AddFormField('urls', _Links);

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
          InputString := _OverAllPostReply; // StringReplace(_OverAllPostReply, '\', '', [rfReplaceAll]);

          for I := 0 to Count - 1 do
          begin
            // Expression := '\/files\/' + GetDownloadlinkID(Strings[I]) + '\/(.*?)<\/a><\/td>ntttt<td>(\w+)<\/td>ntttt<td>([\d\.]+) (\w+)<';

            Expression := 'class=\\"result clearfix (failed|ok)\\".*?files\\/(get\\/)?' + GetDownloadlinkID(Strings[I]) +
              '\\/([^>]+)<\\/a>.*?class=\\"size\\">([\d\.]+) (B|KB|MB|GB)<\\/span>';

            if Exec(InputString) then
              AddLink(Strings[I], HTTPDecode(Match[3]), APIResultToStatus(Match[1]), TSizeFormatter.SizeToByte(Match[4], Match[5]))
            else
              AddLink(Strings[I], '', csOffline, 0);
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
