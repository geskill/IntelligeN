{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Filefactory.com Delphi API                          *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uFilefactoryCom;

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
  TFilefactoryCom = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TFilefactoryCom }

function TFilefactoryCom.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
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

        Expression := '<title>(.*?) \- FileFactory<\/title>';
        if Exec(InputString) then
          ALinkInfo.FileName := Trim(HTMLDecode(Match[1]));

        if SameStr('', ALinkInfo.FileName) then
        begin
          Expression := 'id="file_name".*?\s+<h2>(.*?)<\/h2>';
          if Exec(InputString) then
            ALinkInfo.FileName := Trim(HTMLDecode(Match[1]));
        end;

          Expression := 'id="file_info">([\d\.,]+) (\w+)';
          if Exec(InputString) then
            ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[1], Match[2]);

        ALinkInfo.Status := csOnline;
      finally
        Free;
      end;

  Result := True;
end;

function TFilefactoryCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TFilefactoryCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TFilefactoryCom.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TFilefactoryCom.GetName: WideString;
begin
  Result := 'Filefactory.com';
end;

end.
