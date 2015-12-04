{ ********************************************************
  *                                                      *
  *  Terabit.to Delphi API                               *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uTerabitTo;

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
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TTerabitTo = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TTerabitTo }

function TTerabitTo.GetName: WideString;
begin
  Result := 'Terabit.to';
end;

function TTerabitTo.CheckLink(AFile: WideString): TLinkInfo;
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
  HTTPRequest.Cookies.Add('lang=english');

  RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('No such file', ResponeStr) > 0) or (Pos('>File Not Found.<', ResponeStr) > 0) or (Pos('The file was removed', ResponeStr) > 0) then
    LinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        ModifierS := True;

        InputString := ResponeStr;
        Expression := '<h1 .*?>(.*?)<\/h1><\/div>\s+<div .*?: ([\d\.]+) (\w+)<br>';

        if Exec(InputString) then
        begin
          LinkInfo.Status := csOnline;
          LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
          LinkInfo.FileName := Match[1];
        end;
      finally
        Free;
      end;

  Result := LinkInfo;
end;

end.
