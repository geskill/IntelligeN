{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Xup.in Delphi API                                   *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uXupIn;

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
  TXupIn = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TXupIn }

function TXupIn.InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool;
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

  if (Pos('Datei existiert nicht', LResponeStr) > 0) then
  begin
    ALinkInfo.Status := csOffline
  end
  else
    with TRegExpr.Create do
      try
        InputString := LResponeStr;

        if SameStr('xup.raidrush.ws/', LResponeStr) then
        begin
          Expression := '<title>XUP - Download (.*?) \| ';
          if Exec(InputString) then
            ALinkInfo.FileName := Trim(HTMLDecode(Match[1]));

          if SameStr('', ALinkInfo.FileName) then
          begin
            Expression := '<h1>XUP - Download (.*?) \| ';
            if Exec(InputString) then
              ALinkInfo.FileName := Trim(HTMLDecode(Match[1]));
          end;

          Expression := 'Size<\/font><\/td>[\t\n\r ]+<td>(\d+)<\/td>';
          if Exec(InputString) then
            ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[1]);
        end
        else
        begin
          Expression := '<legend>.*?<.*?>Download: (.*?)<\/.*?>';
          if Exec(InputString) then
            ALinkInfo.FileName := Trim(HTMLDecode(Match[1]));

          Expression := 'File Size: ([\d\.]+) (\w+)<';
          if Exec(InputString) then
            ALinkInfo.Size := TSizeFormatter.SizeToByte(Match[1], Match[2]);
        end;

        ALinkInfo.Status := csOnline;
      finally
        Free;
      end;

  Result := True;
end;

function TXupIn.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TXupIn.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TXupIn.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TXupIn.GetName: WideString;
begin
  Result := 'Xup.in';
end;

end.
