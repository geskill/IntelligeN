{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Crocko.com Delphi API                               *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uCrockoCom;

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
  uPathUtils, uSizeUtils, uURLUtils;

  // OFFLINE ???

type
  TCrockoCom = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TCrockoCom }

function TCrockoCom.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
var
  LHTTPRequest: IHTTPRequest;
  LRequestID: Double;
  LResponeStr: string;
begin
  ALinkInfo := TLinkInfo.Create;

  LHTTPRequest := THTTPRequest.Create(AFile);
  LHTTPRequest.Cookies.Add('language=en');

  LRequestID := HTTPManager.Get(LHTTPRequest, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

  if (Pos('<title>Crocko.com 404</title>', LResponeStr) > 0) or (Pos('Requested file is deleted', LResponeStr) > 0) or (Pos('>Searching for file', LResponeStr) > 0) then
  begin
    ALinkInfo.Status := csOffline
  end
  else if (Pos('<h1>Software error:</h1>', LResponeStr) > 0) then
  begin
    ALinkInfo.Status := csUnknown
  end
  else
    with TRegExpr.Create do
      try
        InputString := LResponeStr;

        Expression := '>Download: +<strong>(.*?)<\/strong>';
        if Exec(InputString) then
          ALinkInfo.FileName := Trim(HTTPDecode(Match[1]));

        if SameStr('', ALinkInfo.FileName) then
        begin
          Expression := '>Download:<\/span> <br \/>[\t\n\r ]+<strong>(.*?)<\/strong>';
          if Exec(InputString) then
            ALinkInfo.FileName := Trim(HTTPDecode(Match[3]));
        end;

        Expression := '<span class=\"tip1\"><span class=\"inner\">([\d\.]+) (\w+)<\/span><\/span>';
        if Exec(InputString) then
          ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[1], Match[2]);

        ALinkInfo.Status := csOnline;
      finally
        Free;
      end;

  Result := True;
end;

function TCrockoCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TCrockoCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TCrockoCom.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TCrockoCom.GetName: WideString;
begin
  Result := 'Crocko.com';
end;

function TCrockoCom.CheckLink(const AFile: WideString): TLinkInfo;
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
  HTTPRequest.Cookies.Add('language=en');

  RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('', ResponeStr) > 0) then
    LinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        ModifierS := True;

        InputString := ResponeStr;
        // Easy-share.com
        // Expression := 'requesting:<\/span>\s+(.*?)\s+<span class="txtgray">\(([\d\.]+) (\w+)';
        Expression := 'Download:<.*?strong>(.*?)<\/.*?inner">([\d\.]+) (\w+)<\/';

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
