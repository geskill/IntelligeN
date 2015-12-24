{ ********************************************************
  *                                                      *
  *  Letitbit.net Delphi API                             *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
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
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TLetitbitNet = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(const AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(const AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TLetitbitNet }

function TLetitbitNet.GetName: WideString;
begin
  Result := 'Letitbit.net';
end;

function TLetitbitNet.CheckLink(const AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;

  HTTPRequest: IHTTPRequest;

  RequestID: Double;

  ResponeStr: string;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := csUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;

  HTTPRequest := THTTPRequest.Create(AFile);
  HTTPRequest.Cookies.Add('lang=en');

  RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('<title>404</title>', ResponeStr) > 0) or (Pos('File not found', ResponeStr) > 0) then
    LinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := ResponeStr;

        Expression := 'name="realname" value="(.*?)"';
        if Exec(InputString) then
        begin
          LinkInfo.Status := csOnline;
          LinkInfo.FileName := Match[1];
        end;

        Expression := 'name="sssize" value="(\d+)"';
        if Exec(InputString) then
          LinkInfo.Size := StrToInt64Def(Match[1], 0);

      finally
        Free;
      end;

  Result := LinkInfo;
end;

end.
