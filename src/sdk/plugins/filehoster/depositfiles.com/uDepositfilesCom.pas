{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Depositfiles.com Delphi API                         *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uDepositfilesCom;

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

type
  TDepositfilesCom = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TDepositfilesCom }

function TDepositfilesCom.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
var
  LHTTPRequest: IHTTPRequest;
  LRequestID: Double;
  LResponeStr, LEncodedResponeStr: string;
begin
  ALinkInfo := TLinkInfo.Create;

  LHTTPRequest := THTTPRequest.Create(AFile);
  LHTTPRequest.Cookies.Add('lang_current=de');

  LRequestID := HTTPManager.Get(LHTTPRequest, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

  if (Pos('Dieser File existiert nicht', LResponeStr) > 0) or (Pos('Diese Datei besteht nicht', LResponeStr) > 0) or (Pos('Entweder existiert diese Datei nicht oder sie wurde', LResponeStr) > 0) then
    ALinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := LResponeStr;

        Expression := 'class=\"info\".*?unescape\(''(.*?)''';
        if Exec(InputString) then
          LEncodedResponeStr := Trim(HTMLDecode(Match[1]));

        Expression := '>Datei Grösse: <b>([\d\.]+) (\w+)<\/b>';
        if Exec(InputString) then
          ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[1], Match[2]);

        InputString := LEncodedResponeStr;
        Expression := 'Dateiname: <b title=\"(.*?)\">.*?<\/b>';
        if Exec(InputString) then
          ALinkInfo.FileName := Trim(HTMLDecode(Match[1]));

        ALinkInfo.Status := csOnline;
      finally
        Free;
      end;

  Result := True;
end;

function TDepositfilesCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TDepositfilesCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TDepositfilesCom.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TDepositfilesCom.GetName: WideString;
begin
  Result := 'Depositfiles.com';
end;

end.
