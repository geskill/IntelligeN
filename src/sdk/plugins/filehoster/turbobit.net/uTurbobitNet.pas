{ ********************************************************
  *                                                      *
  *  Turbobit.net Delphi API                             *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uTurbobitNet;

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
  TTurbobitNet = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TTurbobitNet }

function TTurbobitNet.GetName: WideString;
begin
  Result := 'Turbobit.net';
end;

function TTurbobitNet.CheckLink(AFile: WideString): TLinkInfo;
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
  HTTPRequest.Cookies.Add('set_user_lang_change=en');

  RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('File not found.', ResponeStr) > 0) then
    LinkInfo.Status := lsOffline
  else
    with TRegExpr.Create do
      try
        InputString := ResponeStr;
        Expression := '<h1 class="download-file">.*?''>(.*?)<\/span>\s+\(([\d,]+) (\w+)\)';

        if Exec(InputString) then
        begin
          LinkInfo.Status := lsOnline;
          LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3], False);
          LinkInfo.FileName := Match[1];
        end;
      finally
        Free;
      end;

  Result := LinkInfo;
end;

end.
