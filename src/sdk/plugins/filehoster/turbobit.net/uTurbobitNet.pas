{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Turbobit.net Delphi API                             *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uTurbobitNet;

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
  uPathUtils, uSizeUtils, uStringUtils, uURLUtils;

type
  TTurbobitNet = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TTurbobitNet }

function TTurbobitNet.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
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

  if (Pos('File not found.', LResponeStr) > 0) or (Pos('File was not found.', LResponeStr) > 0) then
    ALinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := '="file-title">(.*?)<\/span>.*?class="file-size">([\d, ]+) (\w+)<\/span>';

        if Exec(InputString) then
        begin
          ALinkInfo.Status := csOnline;
          ALinkInfo.Size := TSizeFormatter.SizeToByte(RemoveWhitespace(Match[2]), Match[3], False);
          ALinkInfo.FileName := Match[1];
        end;

      finally
        Free;
      end;

  Result := True;
end;

function TTurbobitNet.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TTurbobitNet.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TTurbobitNet.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TTurbobitNet.GetName: WideString;
begin
  Result := 'Turbobit.net';
end;

end.
