{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Sendspace.com Delphi API                            *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uSendspaceCom;

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
  TSendspaceCom = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TSendspaceCom }

function TSendspaceCom.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
var
  LHTTPRequest: IHTTPRequest;
  LRequestID: Double;
  LResponeStr: string;
begin
  ALinkInfo := TLinkInfo.Create;

  LHTTPRequest := THTTPRequest.Create(AFile);
  LHTTPRequest.Cookies.Add('set_user_lang_change=en');

  LRequestID := HTTPManager.Get(LHTTPRequest, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

  if (Pos('class="msg error"', LResponeStr) > 0) then
    ALinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := '<h2.*?<b>(.*?)<.*?<\/b> ([\d\.]+)(\w+)';

        if Exec(InputString) then
        begin
          ALinkInfo.Status := csOnline;
          ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
          ALinkInfo.FileName := Match[1];
        end;

      finally
        Free;
      end;

  Result := True;
end;

function TSendspaceCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TSendspaceCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TSendspaceCom.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TSendspaceCom.GetName: WideString;
begin
  Result := 'Sendspace.com';
end;

end.
