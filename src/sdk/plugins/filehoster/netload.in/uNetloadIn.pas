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
  // Common
  uBaseConst,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TNetloadIn = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(const AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(const AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TNetloadIn }

function TNetloadIn.GetName: WideString;
begin
  Result := 'Netload.in';
end;

function TNetloadIn.CheckLink(const AFile: WideString): TLinkInfo;
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

function TNetloadIn.CheckLinks(const AFiles: WideString): Integer;

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
    Result := csOffline;
    if (AValue = 'online') then
      Result := csOnline;
  end;

var
  _OverAllPostReply, _URLs: string;
  I: Integer;

  HTTPParams: IHTTPParams;

  RequestID: Double;

  ResponeStr: string;
begin
  with TStringList.Create do
    try
      Text := AFiles;

      _OverAllPostReply := '';
      _URLs := '';
      for I := 0 to Count - 1 do
      begin
        if (I > 0) then
          _URLs := _URLs + ';';
        _URLs := _URLs + GetDownloadlinkID(Strings[I]);

        if (I > 0) and (I mod 100 = 0) or (I = Count - 1) then
        begin
          HTTPParams := THTTPParams.Create;
          with HTTPParams do
          begin
            AddFormField('auth', 'BVm96BWDSoB4WkfbEhn42HgnjIe1ilMt');
            AddFormField('bz', '1');
            AddFormField('md5', '1');
            AddFormField('file_id', _URLs);
          end;

          RequestID := HTTPManager.Post(THTTPRequest.Create('http://api.netload.in/info.php'), HTTPParams, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID);

          ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

          _OverAllPostReply := _OverAllPostReply + ResponeStr;
          _URLs := '';
        end;
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
  Result := FCheckedLinksList.Count;
end;

end.
