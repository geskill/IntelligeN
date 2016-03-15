{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Junocloud.me Delphi API                             *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
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
  uPlugInConst, uPlugInInterface, uPlugInFileHosterClass, uPlugInFileHosterClasses, uPlugInHTTPClasses,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TJunocloudMe = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TJunocloudMe }

function TJunocloudMe.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
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

  if not(Pos('name="fname"', LResponeStr) > 0) then
    ALinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := ': ([\d\.]+) (\w+).*?name="fname" value="(.*?)"';

        if Exec(InputString) then
        begin
          ALinkInfo.Status := csOnline;
          ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[1], Match[2]);
          ALinkInfo.FileName := Match[3];
        end;
      finally
        Free;
      end;

  Result := True;
end;

function TJunocloudMe.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TJunocloudMe.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TJunocloudMe.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TJunocloudMe.GetName: WideString;
begin
  Result := 'Junocloud.me';
end;

end.
