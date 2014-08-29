{ ********************************************************
  *                                                      *
  *  Netload.in Delphi API                               *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uNetloadIn;

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
  TNetloadIn = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TNetloadIn }

function TNetloadIn.GetName: WideString;
begin
  Result := 'Netload.in';
end;

function TNetloadIn.CheckLink(AFile: WideString): TLinkInfo;
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

function TNetloadIn.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '\/datei([a-zA-Z0-9]+)';

        if Exec(InputString) then
          Result := Match[1];
      finally
        Free;
      end;
    if (Result = '') then
      with TRegExpr.Create do
        try
          InputString := ALink;
          Expression := 'file_id=([a-zA-Z0-9]+)';

          if Exec(InputString) then
            Result := Match[1];
        finally
          Free;
        end;
    if (Result = '') then
      with TRegExpr.Create do
        try
          InputString := ALink;
          Expression := 'netload\.in\/([a-zA-Z0-9]+)\/';

          if Exec(InputString) then
            Result := Match[1];
        finally
          Free;
        end;
  end;

  function APIResultToString(AValue: string): string;
  begin
    Result := AValue;
    if (AValue = 'unknown') then
      Result := '';
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := lsOffline;
    if (AValue = 'online') then
      Result := lsOnline;
  end;

var
  _params, _postreply: TStringStream;
  _OverAllPostReply: string;
  I: Integer;
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
            _params.WriteString('auth=BVm96BWDSoB4WkfbEhn42HgnjIe1ilMt&bz=1&md5=1&file_id=');
            for I := 0 to Count - 1 do
            begin
              if (I > 0) then
                _params.WriteString(';');
              _params.WriteString(GetDownloadlinkID(Strings[I]));

              if (I > 0) and (I mod 100 = 0) or (I = Count - 1) then
              begin
                Post('http://api.netload.in/info.php', _params, _postreply);
                _OverAllPostReply := _OverAllPostReply + _postreply.DataString;
                _params.Clear;
                _params.WriteString('auth=BVm96BWDSoB4WkfbEhn42HgnjIe1ilMt&bz=1&md5=1&file_id=');
                _postreply.Clear;
              end;
            end;
          finally
            _postreply.Free;
            _params.Free;
          end;

          // c8Dig5OG2L;Board Detector.rar;469412;online;9f6787473902087256d5d25006104e8c

          with TRegExpr.Create do
            try
              InputString := _OverAllPostReply;
              Expression := '(\w+);(.*?);(unknown|\d+);(\w+);(\w+)';

              if Exec(InputString) then
              begin
                repeat
                  for I := 0 to Count - 1 do
                    if SameText(GetDownloadlinkID(Strings[I]), Match[1]) then
                    begin
                      AddLink(Strings[I], APIResultToString(Match[2]), APIResultToStatus(Match[4]), StrToInt64Def(Match[3], 0), APIResultToString(Match[5]));
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
