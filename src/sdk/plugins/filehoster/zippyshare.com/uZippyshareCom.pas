{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Zippyshare.com Delphi API                           *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uZippyshareCom;

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
  TZippyshareCom = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TZippyshareCom }

function TZippyshareCom.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
var
  LHTTPRequest: IHTTPRequest;
  LRequestID: Double;
  LResponeStr: string;
begin
  ALinkInfo := TLinkInfo.Create;

  LHTTPRequest := THTTPRequest.Create(AFile);
  LHTTPRequest.Cookies.Add('ziplocale=en');

  LRequestID := HTTPManager.Get(LHTTPRequest, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

  if (Pos('File has expired and does not exist anymore on this server', LResponeStr) > 0) or (Pos('File does not exist', LResponeStr) > 0) then
    ALinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := LResponeStr;

        Expression := '<title>Zippyshare\.com \- (.*?)<\/title>';
        if Exec(InputString) then
          ALinkInfo.FileName := Trim(HTMLDecode(Match[1]));

        if SameStr('', ALinkInfo.FileName) then
        begin
          Expression := 'Name:(\s+)?<\/font>(\s+)?<font style=.*?>(.*?)<\/font>';
          if Exec(InputString) then
            ALinkInfo.FileName := Trim(HTMLDecode(Match[3]));
        end;

        if SameStr('', ALinkInfo.FileName) then
        begin
          Expression := 'document\.getElementById\(\''dlbutton\''\)\.href.*"\/(.*?)";';
          if Exec(InputString) then
            ALinkInfo.FileName := Trim(HTMLDecode(Match[1]));
        end;

        if not SameStr('>Share movie:', LResponeStr) then
        begin
          Expression := 'Size:(\s+)?<\/font>(\s+)?<font style=.*?>([\d\.]+) (\w+)<\/font>';
          if Exec(InputString) then
            ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[3], Match[4]);
        end;

        ALinkInfo.Status := csOnline;
      finally
        Free;
      end;

  Result := True;
end;

function TZippyshareCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TZippyshareCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TZippyshareCom.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TZippyshareCom.GetName: WideString;
begin
  Result := 'Zippyshare.com';
end;

end.
