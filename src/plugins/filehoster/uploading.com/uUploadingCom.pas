{ ********************************************************
  *                                                      *
  *  Uploading.com Delphi API                            *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uUploadingCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp, DateUtils,
  // Reg Ex
  RegExpr,
  // plugin system
  uIdHTTPHelper, uPlugInFileHosterClass, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TUploadingCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TUploadingCom }

function TUploadingCom.GetName: WideString;
begin
  Result := 'Uploading.com';
end;

function TUploadingCom.CheckLink(AFile: WideString): TLinkInfo;
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

function TUploadingCom.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '\/files\/(get\/)?(\w+)';

        if Exec(InputString) then
          Result := Match[2];
      finally
        Free;
      end;
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := lsOffline;
    if (AValue = 'active') then
      Result := lsOnline;
  end;

var
  I: Integer;
  _params, _postreply: TStringStream;
  _OverAllPostReply, _Links: string;
  _systemtime: SystemTime;
begin
  with TIdHTTPHelper.Create(Self) do
    try
      AddCookie('lang=1', 'http://uploading.com/');

      with TStringList.Create do
        try
          Text := AFiles;

          _params := TStringStream.Create('');
          _postreply := TStringStream.Create('');
          try
            _OverAllPostReply := '';
            _Links := '';
            for I := 0 to Count - 1 do
            begin
              _Links := _Links + HTTPEncode(Strings[I]);
              if not(I = Count - 1) then
                _Links := _Links + sLineBreak;

              if (I > 0) and (I mod 100 = 0) or (I = Count - 1) then
              begin
                _params.WriteString('urls=' + _Links);
                GetSystemTime(_systemtime);
                try
                  Post('http://uploading.com/files/checker/?JsHttpRequest=' + IntToStr(DateTimeToUnix(EncodeDateTime(_systemtime.wYear, _systemtime.wMonth,
                          _systemtime.wDay, _systemtime.wHour, _systemtime.wMinute, _systemtime.wSecond, _systemtime.wMilliseconds))) + '-xml', _params,
                    _postreply);
                except

                end;
                _OverAllPostReply := _OverAllPostReply + _postreply.DataString;
                _Links := '';
                _params.Clear;
                _postreply.Clear;
              end;
            end;

          finally
            _postreply.Free;
            _params.Free;
          end;

          with TRegExpr.Create do
            try
              InputString := StringReplace(_OverAllPostReply, '\', '', [rfReplaceAll]);

              for I := 0 to Count - 1 do
              begin
                Expression := '\/files\/' + GetDownloadlinkID(Strings[I]) + '\/(.*?)<\/a><\/td>ntttt<td>(\w+)<\/td>ntttt<td>([\d\.]+) (\w+)<';

                if Exec(InputString) then
                  AddLink(Strings[I], HTTPDecode(Match[1]), APIResultToStatus(Match[2]), TSizeFormatter.SizeToByte(Match[3], Match[4]))
                else
                  AddLink(Strings[I], '', lsOffline, 0);
              end;
            finally
              Free;
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
