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
  // plugin system
  uIdHTTPHelper, uPlugInFileHosterClass, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TFileserveCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    function CheckLinks(AFiles: WideString): Integer; override; stdcall;
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
    Status := lsUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;
  Result := LinkInfo;
end;

function TFileserveCom.CheckLinks(AFiles: WideString): Integer;
var
  _params, _postreply: TStringStream;
begin
  with TIdHTTPHelper.Create(Self) do
    try
      with TStringList.Create do
        try
          Text := AFiles;

          _params := TStringStream.Create('');
          _postreply := TStringStream.Create('');
          try
            _params.WriteString('urls=' + HTTPEncode(Text) + '&');
            _params.WriteString('submit=Check+Urls');

            try
              Post('http://fileserve.com/link-checker.php', _params, _postreply);
            except

            end;

            with TRegExpr.Create do
              try
                InputString := _postreply.DataString;
                Expression := '<td>http(.*?)\s+<\/td>\s+<td>(.*?)<\/td>\s+<td>([\d\.]+) (\w+)<\/td>';

                if Exec(InputString) then
                begin
                  repeat
                    AddLink('http' + Match[1], Match[2], lsOnline, TSizeFormatter.SizeToByte(Match[3], Match[4]));
                  until not ExecNext;
                end;

                Expression := '<td>http(.*?)<\/td>\s+<td>(.*?)<\/td>\s+<td>--<\/td>';

                if Exec(InputString) then
                begin
                  repeat
                    AddLink('http' + Match[1], Match[2], lsOffline, 0);
                  until not ExecNext;
                end;

              finally
                Free;
              end;
          finally
            _postreply.Free;
            _params.Free;
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
