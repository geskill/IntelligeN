{ ********************************************************
  *                                                      *
  *  Qshare.com Delphi API                               *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uQshareCom;

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
  uPathUtils;

type
  TQshareCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TQshareCom }

function TQshareCom.GetName: WideString;
begin
  Result := 'Qshare.com';
end;

function TQshareCom.CheckLink(AFile: WideString): TLinkInfo;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := 'get\/(\d+)\/';

        if Exec(InputString) then
          Result := Match[1];
      finally
        Free;
      end;
  end;

var
  LinkInfo: TLinkInfo;

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

  RequestID := HTTPManager.Get(THTTPRequest.Create('http://qshare.com/api/file_info.php?id=' + GetDownloadlinkID(AFile)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponeStr;
      Expression := 'ONLINE:1#SIZE:(\d+)#NAME:(.*?)#MD5:(\w+)';

      if Exec(InputString) then
      begin
        LinkInfo.Status := csOnline;
        LinkInfo.Size := StrToInt64Def(Match[1], 0);
        LinkInfo.FileName := Match[2];
        LinkInfo.Checksum := Match[3]
      end
      else
        LinkInfo.Status := csOffline;
    finally
      Free;
    end;

  Result := LinkInfo;
end;

end.
