{ ********************************************************
  *                                                      *
  *  Load.to Delphi API                                  *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uLoadTo;

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
  TLoadTo = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TLoadTo }

function TLoadTo.GetName: WideString;
begin
  Result := 'Load.to';
end;

function TLoadTo.CheckLink(AFile: WideString): TLinkInfo;
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

  RequestID := HTTPManager.Get(THTTPRequest.Create(AFile), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('Can''t find file', ResponeStr) > 0) then
    LinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := ResponeStr;
        Expression := '<h1>(.*?)<\/h1>.*?download_table_right">(\d+) (\w+)<';

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
