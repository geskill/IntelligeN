{ ******************************************************
  *                                                    *
  *  Uploaded.to Delphi API                            *
  *  Version 2.0.0.0                                   *
  *  Copyright (c) 2010 Sebastian Klatte               *
  *                                                    *
  ****************************************************** }
unit uUploadedTo;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp,
  // Reg Ex
  RegExpr,
  // plugin system
  uIdHTTPHelper, uPlugInFileHosterClass, uPlugInConst,
  // Utils
  uPathUtils;

type
  TUploadedTo = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TUploadedTo }

function TUploadedTo.GetName: WideString;
begin
  Result := 'Uploaded.to';
end;

function TUploadedTo.CheckLink(AFile: WideString): TLinkInfo;
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

function TUploadedTo.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    ALink := StringReplace(ALink, 'ul.to/', 'uploaded.to/file/', [rfReplaceAll, rfIgnoreCase]);
    ALink := StringReplace(ALink, '/?id=', '/file/', [rfReplaceAll, rfIgnoreCase]);
    ALink := StringReplace(ALink, '?id=', 'file/', [rfReplaceAll, rfIgnoreCase]);

    with TRegExpr.Create do
      try
        InputString := IncludeTrailingUrlDelimiter(ALink);
        Expression := 'uploaded.to/file/(.*?)/';

        if Exec(InputString) then
          Result := Match[1];
      finally
        Free;
      end;
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := lsOffline;
    if (AValue = 'online') then
      Result := lsOnline;
  end;

var
  I, J, retry: Integer;
  _params, _postreply: TStringStream;
  _OverAllPostReply: string;
begin
  with TIdHTTPHelper.Create(Self) do
    try
      with TStringList.Create do
        try
          Text := AFiles;

          _params := TStringStream.Create('');
          _postreply := TStringStream.Create('');
          try
            _params.WriteString('apikey=hP5Y37ulYfr8gSsS97LCT7kG5Gqp8Uug&');
            J := 0;
            for I := 0 to Count - 1 do
            begin
              _params.WriteString('id_' + IntToStr(J) + '=' + GetDownloadlinkID(Strings[I]) + '&');
              Inc(J);

              if (I > 0) and (I mod 80 = 0) or (I = Count - 1) then
              begin
                retry := 0;

                while retry < 5 do
                begin
                  try
                    Post('http://uploaded.to/api/filemultiple', _params, _postreply);

                    if SameText(_postreply.DataString, '') then
                    begin
                      Inc(retry);
                      sleep(550);
                    end
                    else
                      break;
                  except

                  end;
                end;

                _OverAllPostReply := _OverAllPostReply + _postreply.DataString;
                _params.Clear;
                _params.WriteString('apikey=hP5Y37ulYfr8gSsS97LCT7kG5Gqp8Uug&');
                J := 0;
                _postreply.Clear;
              end;
            end;

            with TRegExpr.Create do
              try
                ModifierS := False;
                InputString := _OverAllPostReply;
                Expression := '(\w+),(\w+),(\d*?),(\w*?),(.*)';

                if Exec(InputString) then
                begin
                  repeat
                    for I := 0 to Count - 1 do
                      if SameText(GetDownloadlinkID(Strings[I]), Match[2]) then
                      begin
                        AddLink(Strings[I], Match[5], APIResultToStatus(Match[1]), StrToInt64Def(Match[3], 0), Match[4]);
                        break;
                      end;
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
