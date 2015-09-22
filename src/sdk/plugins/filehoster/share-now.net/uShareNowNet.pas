{ ********************************************************
  *                                                      *
  *  Share-now.net Delphi API                            *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uShareNowNet;

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
  TShareNowNet = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TShareNowNet }

function TShareNowNet.GetName: WideString;
begin
  Result := 'Share-now.net';
end;

function TShareNowNet.CheckLink(AFile: WideString): TLinkInfo;
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

  if (Pos('Datei existiert nicht oder', ResponeStr) > 0) then
    LinkInfo.Status := lsOffline
  else
    with TRegExpr.Create do
      try
        InputString := ResponeStr;
        Expression :=
          '(<p><span class="style1">\s+<br \/>|<p><span class="style1">Download ->|<h3>)\s+(.*?)(<\/span>| \([^\)]*?\) <span).*?<span class="style1">([\d\.]+) (\w+)<';

        if Exec(InputString) then
        begin
          LinkInfo.Status := lsOnline;
          LinkInfo.Size := TSizeFormatter.SizeToByte(Match[4], Match[5]);
          LinkInfo.FileName := Match[2];
        end;
      finally
        Free;
      end;

  Result := LinkInfo;
end;

end.
