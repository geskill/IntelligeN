{ ********************************************************
  *                                                      *
  *  Junocloud.me Delphi API                             *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2013 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uJunocloudMe;

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
  TJunocloudMe = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(const AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(const AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TJunocloudMe }

function TJunocloudMe.GetName: WideString;
begin
  Result := 'Junocloud.me';
end;

function TJunocloudMe.CheckLink(const AFile: WideString): TLinkInfo;
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

  HTTPManager.WaitFor(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('name="fname"', ResponeStr) = 0) then
    LinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := ResponeStr;
        Expression := ': ([\d\.]+) (\w+).*?name="fname" value="(.*?)"';

        if Exec(InputString) then
        begin
          LinkInfo.Status := csOnline;
          LinkInfo.Size := TSizeFormatter.SizeToByte(Match[1], Match[2]);
          LinkInfo.FileName := Match[3];
        end;
      finally
        Free;
      end;

  Result := LinkInfo;
end;

end.
