{ ********************************************************
  *                                                      *
  *  Zippyshare.com Delphi API                           *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uZippyshareCom;

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
  TZippyshareCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TZippyshareCom }

function TZippyshareCom.GetName: WideString;
begin
  Result := 'Zippyshare.com';
end;

function TZippyshareCom.CheckLink(AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;

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

  RequestID := HTTPManager.Get(THTTPRequest.Create(AFile), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('File does not exist on this server', ResponeStr) > 0) then
    LinkInfo.Status := lsOffline
  else
    with TRegExpr.Create do
      try
        InputString := ResponeStr;

        if (Pos('"og:title" content="Private file"', ResponeStr) > 0) then
        begin
          Expression := '\+"\/(.*?)";';
          if Exec(InputString) then
            LinkInfo.FileName := HTTPDecode(Match[1]);

          Expression := '>([\d\.]+) (\w+)<';
          if Exec(InputString) then
          begin
            LinkInfo.Status := lsOnline;
            LinkInfo.Size := TSizeFormatter.SizeToByte(Match[1], Match[2]);
          end;
        end
        else
        begin
          Expression := '"og:title" content="(.*?) ".*?>([\d\.]+) (\w+)<';
          if Exec(InputString) then
          begin
            LinkInfo.Status := lsOnline;
            LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
            LinkInfo.FileName := Match[1];
          end;
        end;
      finally
        Free;
      end;

  Result := LinkInfo;
end;

end.
