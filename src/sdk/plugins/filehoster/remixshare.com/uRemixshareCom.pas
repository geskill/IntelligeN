{ ********************************************************
  *                                                      *
  *  Remixshare.com Delphi API                           *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uRemixshareCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp,
  // Reg Ex
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TRemixshareCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TRemixshareCom }

function TRemixshareCom.GetName: WideString;
begin
  Result := 'Remixshare.com';
end;

function TRemixshareCom.CheckLink(AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;

  HTTPRequest: IHTTPRequest;

  RequestID: Double;

  ResponeStr: string;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := lsUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;

  HTTPRequest := THTTPRequest.Create(AFile);
  HTTPRequest.Cookies.Add('lang_en=english');

  RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('Error Code:', ResponeStr) > 0) then
    LinkInfo.Status := lsOffline
  else
    with TRegExpr.Create do
      try
        InputString := ResponeStr;
        Expression := '<span title=''(.*?)''>.*?&nbsp;\(([\d\.]+)&nbsp;(\w+)\)<br />MD5: (\w+)<';

        if Exec(InputString) then
        begin
          LinkInfo.Status := lsOnline;
          LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
          LinkInfo.FileName := Match[1];
          LinkInfo.Checksum := Match[4];
        end;
      finally
        Free;
      end;

  Result := LinkInfo;
end;

end.
