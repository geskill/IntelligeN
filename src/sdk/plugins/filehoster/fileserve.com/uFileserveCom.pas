{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Fileserve.com Delphi API                            *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uFileserveCom;

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
  TFileserveCom = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
    function InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TFileserveCom }

function TFileserveCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TFileserveCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TFileserveCom.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TFileserveCom.GetName: WideString;
begin
  Result := 'Fileserve.com';
end;

function TFileserveCom.CheckLink(const AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := csUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;
  Result := LinkInfo;
end;

function TFileserveCom.CheckLinks(const AFiles: WideString): Integer;
var
  HTTPParams: IHTTPParams;

  RequestID: Double;

  ResponeStr: string;
begin
  with TStringList.Create do
    try
      Text := AFiles;

      HTTPParams := THTTPParams.Create;
      with HTTPParams do
      begin
        AddFormField('urls', Text);
        AddFormField('submit', 'Check+Urls');
      end;

      RequestID := HTTPManager.Post(THTTPRequest.Create('http://fileserve.com/link-checker.php'), HTTPParams, TPlugInHTTPOptions.Create(Self));

      HTTPManager.WaitFor(RequestID);

      ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

      with TRegExpr.Create do
        try
          InputString := ResponeStr;
          Expression := '<td>http(.*?)\s+<\/td>\s+<td>(.*?)<\/td>\s+<td>([\d\.]+) (\w+)<\/td>';

          if Exec(InputString) then
          begin
            repeat
              AddLink('http' + Match[1], Match[2], csOnline, TSizeFormatter.SizeToByte(Match[3], Match[4]));
            until not ExecNext;
          end;

          Expression := '<td>http(.*?)<\/td>\s+<td>(.*?)<\/td>\s+<td>--<\/td>';

          if Exec(InputString) then
          begin
            repeat
              AddLink('http' + Match[1], Match[2], csOffline, 0);
            until not ExecNext;
          end;

        finally
          Free;
        end;

    finally
      Free;
    end;
  Result := FCheckedLinksList.Count;
end;

end.
