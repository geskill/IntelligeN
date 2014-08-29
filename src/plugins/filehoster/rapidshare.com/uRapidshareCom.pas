{ ********************************************************
  *                                                      *
  *  Rapidshare.com Delphi API                           *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uRapidshareCom;

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
  TRapidshareCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TRapidshareCom }

function TRapidshareCom.GetName: WideString;
begin
  Result := 'Rapidshare.com';
end;

function TRapidshareCom.CheckLink(AFile: WideString): TLinkInfo;
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

function TRapidshareCom.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := 'files\/(\d+)\/';

        if Exec(InputString) then
          Result := Match[1]
        else
        begin
          Expression := '#!download\|.*?\|(\d+)\|';
          if Exec(InputString) then
            Result := Match[1];
        end;
      finally
        Free;
      end;
  end;

  function GetDownloadlinkFileName(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '#!download\|.*?\|\d+\|(.*?)\|';

        if Exec(InputString) then
          Result := Match[1]
        else
          Result := ExtractUrlFileName(ALink);
      finally
        Free;
      end;
  end;

  function GetRequestString(AFiles, AFilenames: string): string;
  begin
    Result := 'http://api.rapidshare.com/cgi-bin/rsapi.cgi?sub=checkfiles&incmd5=1&files=' + AFiles + '&filenames=' + AFilenames;
  end;

  function APIResultToString(AValue: string): string;
  begin
    Result := AValue;
    if (AValue = '0') then
      Result := '';
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := lsOffline;
    if (AValue = '1') then
      Result := lsOnline;
  end;

var
  I: Integer;
  _postreply: TStringStream;
  _OverAllPostReply, _Files, _Filenames: string;
begin
  with TIdHTTPHelper.Create(Self) do
    try
      with TStringList.Create do
        try
          Text := AFiles;

          _postreply := TStringStream.Create('');
          try
            _OverAllPostReply := '';
            _Files := '';
            _Filenames := '';
            for I := 0 to Count - 1 do
            begin
              _Files := _Files + GetDownloadlinkID(Strings[I]);
              if not(I = Count - 1) then
                _Files := _Files + ',';
              _Filenames := _Filenames + GetDownloadlinkFileName(Strings[I]);
              if not(I = Count - 1) then
                _Filenames := _Filenames + ',';

              if (length(GetRequestString(_Files, _Filenames)) > 3000) or (I = Count - 1) then
              begin
                try
                  Get(GetRequestString(_Files, _Filenames), _postreply);
                except

                end;
                _OverAllPostReply := _OverAllPostReply + _postreply.DataString;
                _Files := '';
                _Filenames := '';
                _postreply.Clear;
              end;
            end;

            // 349318395,IntelligeN.zip,5181101,567,1,tl2,689D27AA50DFA689D86A0D0CF6E8633D
            // 546456456,IntelligeN.zip,0      ,0  ,0,0  ,0

            with TRegExpr.Create do
              try
                InputString := _OverAllPostReply;
                Expression := '(\d+),(.*?),(\d+),(\d+),(\d+),(\w+),(\w+)';

                if Exec(InputString) then
                begin
                  repeat
                    for I := 0 to Count - 1 do
                      if SameText(GetDownloadlinkID(Strings[I]), Match[1]) then
                      begin
                        AddLink(Strings[I], Match[2], APIResultToStatus(Match[5]), StrToInt64Def(Match[3], 0), APIResultToString(Match[7]));
                        break;
                      end;
                  until not ExecNext;
                end;
              finally
                Free;
              end;

          finally
            _postreply.Free;
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
