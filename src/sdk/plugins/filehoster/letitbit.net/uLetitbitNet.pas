{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Letitbit.net Delphi API                             *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
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
  uPlugInConst, uPlugInInterface, uPlugInFileHosterClass, uPlugInFileHosterClasses, uPlugInHTTPClasses,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TLetitbitNet = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TLetitbitNet }

function TLetitbitNet.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TLetitbitNet.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TLetitbitNet.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

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

  HTTPManager.WaitFor(RequestID);

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
