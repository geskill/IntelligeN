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

function TRapidgatorNet.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
var
  LHTTPRequest: IHTTPRequest;
  LRequestID: Double;
  LResponeStr: string;
begin
  ALinkInfo := TLinkInfo.Create;

  LHTTPRequest := THTTPRequest.Create(AFile);
  LHTTPRequest.Cookies.Add('lang=en');

  LRequestID := HTTPManager.Get(LHTTPRequest, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

  if (Pos('File not found', LResponeStr) > 0) then
    ALinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := LResponeStr;

        Expression := 'Downloading:\s+<\/strong>.*?>\s+(.*?)<\/a>.*?size:\s+<strong>([\d\.]+) (\w+)<\/strong>\s+.*?MD5: (\w+)';
        if Exec(InputString) then
        begin
          ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
          ALinkInfo.FileName := Trim(HTMLDecode(Match[1]));
          ALinkInfo.Checksum := Match[4];
        end;

        ALinkInfo.Status := csOnline;
      finally
        Free;
      end;

  Result := True;
end;

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

end.
