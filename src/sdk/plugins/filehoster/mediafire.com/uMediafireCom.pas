{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Mediafire.com Delphi API                            *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uMediafireCom;

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
  TMediafireCom = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TMediafireCom }

function TMediafireCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TMediafireCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TMediafireCom.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TMediafireCom.GetName: WideString;
begin
  Result := 'Mediafire.com';
end;

function TMediafireCom.CheckLink(const AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;

  RequestID: Double;

  ResponeStr: string;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := csUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;

  RequestID := HTTPManager.Get(THTTPRequest.Create(AFile), TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('Invalid or Deleted File', ResponeStr) > 0) or (Pos('tos_aup_violation', ResponeStr) > 0) then
    LinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := ResponeStr;
        Expression := '<div class="download_file_title".*?> (.*?) <div.*?\(([\d\.]+) (\w+)\)';

        if Exec(InputString) then
        begin
          LinkInfo.Status := csOnline;
          LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
          LinkInfo.FileName := Match[1];
        end;
      finally
        Free;
      end;

  Result := LinkInfo;
end;

end.
