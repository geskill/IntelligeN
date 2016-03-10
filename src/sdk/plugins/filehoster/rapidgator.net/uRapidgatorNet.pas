{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Rapidgator.net Delphi API                           *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uRapidgatorNet;

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
  uPlugInConst, uPlugInInterface, uPlugInFileHosterClass, uPlugInFileHosterClasses, uPlugInHTTPClasses,
  // Utils
  uPathUtils, uSizeUtils, uStringUtils, uURLUtils;

type
  TRapidgatorNet = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TRapidgatorNet }

function TRapidgatorNet.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TRapidgatorNet.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TRapidgatorNet.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TRapidgatorNet.GetName: WideString;
begin
  Result := 'Rapidgator.net';
end;

function TRapidgatorNet.CheckLink(const AFile: WideString): TLinkInfo;
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

  if (Pos('File not found', ResponeStr) > 0) then
    LinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        ModifierS := True;

        InputString := ResponeStr;
        Expression := 'Downloading:\s+<\/strong>.*?>(.*?)<\/a>.*?size:\s+<strong>([\d\.]+) (\w+)<\/strong>';

        if Exec(InputString) then
        begin
          LinkInfo.Status := csOnline;
          LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
          LinkInfo.FileName := Trim(ReduceWhitespace(Match[1]));
        end;
      finally
        Free;
      end;

  Result := LinkInfo;
end;

end.
