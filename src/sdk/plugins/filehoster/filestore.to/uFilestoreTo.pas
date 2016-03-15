{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Filestore.to Delphi API                             *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uFilestoreTo;

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
  TFilestoreTo = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TFilestoreTo }

function TFilestoreTo.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
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

  if (Pos('>Download-Datei wurde nicht gefunden<', LResponeStr) > 0) or (Pos('>Download-Datei wurde gesperrt<', LResponeStr) > 0) then
    ALinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := LResponeStr;

        Expression := 'File: .*?>(.*?)<\/span>';
        if Exec(InputString) then
          ALinkInfo.FileName := Trim(HTMLDecode(Match[1]));

        Expression := 'Size: ([\d\.]+) (\w+)';
        if Exec(InputString) then
          ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[1], Match[2], False);

        ALinkInfo.Status := csOnline;
      finally
        Free;
      end;

  Result := True;
end;

function TFilestoreTo.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TFilestoreTo.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TFilestoreTo.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TFilestoreTo.GetName: WideString;
begin
  Result := 'Filestore.to';
end;

end.
