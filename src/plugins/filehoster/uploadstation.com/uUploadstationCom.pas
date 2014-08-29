{ ********************************************************
  *                                                      *
  *  Uploadstation.com Delphi API                        *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uUploadstationCom;

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
  TUploadstationCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TUploadstationCom }

function TUploadstationCom.GetName: WideString;
begin
  Result := 'Uploadstation.com';
end;

function TUploadstationCom.CheckLink(AFile: WideString): TLinkInfo;
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

function TUploadstationCom.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '\/file\/(\w+)';

        if Exec(InputString) then
          Result := Match[2];
      finally
        Free;
      end;
  end;

  function APIResultToString(AValue: string): string;
  begin
    Result := AValue;
    if (AValue = '--') then
      Result := '';
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := lsOffline;
    if (AValue = 'Available') then
      Result := lsOnline;
  end;

var
  I: Integer;
  _params, _postreply: TStringStream;
  _OverAllPostReply, _Links: string;
begin
  with TIdHTTPHelper.Create(Self) do
    try
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
                try
                  Post('http://www.uploadstation.com/check-links.php', _params, _postreply);
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
              InputString := _OverAllPostReply;
              // Leerzeichen vorne wichtig
              Expression := ' col1">(.*?)<.*?col2">(.*?)<.*?col3">([\d\.]*?)[ -]+(\w*?)<.*?col4">(.*?)<';

              if Exec(InputString) then
              begin
                repeat
                  for I := 0 to Count - 1 do
                    if SameText(GetDownloadlinkID(Strings[I]), GetDownloadlinkID(Match[1])) then
                    begin
                      AddLink(Strings[I], APIResultToString(Match[2]), APIResultToStatus(Match[5]), TSizeFormatter.SizeToByte(Match[3], Match[4]));
                      break;
                    end;
                until not ExecNext;
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
