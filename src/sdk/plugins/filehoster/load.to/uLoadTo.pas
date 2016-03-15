{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Load.to Delphi API                                  *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uLoadTo;

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
  TLoadTo = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TLoadTo }

function TLoadTo.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
var
  LHTTPRequest: IHTTPRequest;
  LRequestID: Double;
  LResponeStr: string;
begin
  ALinkInfo := TLinkInfo.Create;

  LHTTPRequest := THTTPRequest.Create(AFile);

  LRequestID := HTTPManager.Get(LHTTPRequest, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LResponeStr := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

  if (Pos('Can''t find file', LResponeStr) > 0) then
    ALinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := LResponeStr;

        Expression := '<h1>(.*?)<\/h1>';
        if Exec(InputString) then
          ALinkInfo.FileName := Trim(HTMLDecode(Match[1]));

        Expression := 'Size: ([\d\.]+) (\w+)';
        if Exec(InputString) then
          ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[1], Match[2]);

        ALinkInfo.Status := csOnline;
      finally
        Free;
      end;

  Result := True;
end;

function TLoadTo.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TLoadTo.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TLoadTo.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TLoadTo.GetName: WideString;
begin
  Result := 'Load.to';
end;

end.
