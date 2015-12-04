{ ********************************************************
  *                                                      *
  *  Fileserve.com Delphi API                            *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
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
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TFileserveCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TFileserveCom }

function TFileserveCom.GetName: WideString;
begin
  Result := 'Fileserve.com';
end;

function TFileserveCom.CheckLink(AFile: WideString): TLinkInfo;
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

function TFileserveCom.CheckLinks(AFiles: WideString): Integer;
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

      repeat
        sleep(50);
      until HTTPManager.HasResult(RequestID);

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
