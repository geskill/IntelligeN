{ ********************************************************
  *                                                      *
  *  Hotfile.com Delphi API                              *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uHotfileCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp,
  // Reg Ex
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils;

type
  THotfileCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ THotfileCom }

function THotfileCom.GetName: WideString;
begin
  Result := 'Hotfile.com';
end;

function THotfileCom.CheckLink(AFile: WideString): TLinkInfo;
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

function THotfileCom.CheckLinks(AFiles: WideString): Integer;
var
  _keys, _ids: string;
  I: Integer;

  function GetDownloadlinkID(ALink: string): string; overload;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '/dl/(\d+)/';

        if Exec(InputString) then
          Result := Match[1];

      finally
        Free;
      end;
  end;

  procedure GetDownloadlinkID(ALink: string; var AKey, AID: string); overload;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '/dl/(\d+)/(\w+)';

        if Exec(InputString) then
        begin
          AKey := AKey + Match[2];
          AID := AID + Match[1];
        end;
      finally
        Free;
      end;
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := lsOffline;
    if (AValue = '1') or (AValue = '2') then
      Result := lsOnline;
  end;

var
  HTTPParams: IHTTPParams;

  RequestID: Double;

  ResponeStr: string;
begin
  with TStringList.Create do
    try
      Text := AFiles;

      _keys := '';
      _ids := '';
      for I := 0 to Count - 1 do
      begin
        if (I > 0) then
        begin
          _keys := _keys + ',';
          _ids := _ids + ',';
        end;

        GetDownloadlinkID(Strings[I], _keys, _ids);
      end;

      HTTPParams := THTTPParams.Create;
      with HTTPParams do
      begin
        AddFormField('action', 'checklinks');
        AddFormField('fields', 'id,status,name,size,md5');
        AddFormField('keys', _keys); // 6352f40,c2d67b8,6d99ae4,4546d630
        AddFormField('ids', _ids); // 124846042,182987,61714969,92049098e
      end;

      RequestID := HTTPManager.Post(THTTPRequest.Create('http://api.hotfile.com/'), HTTPParams, TPlugInHTTPOptions.Create(Self));

      repeat
        sleep(50);
      until HTTPManager.HasResult(RequestID);

      ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

      with TRegExpr.Create do
        try
          ModifierS := False;
          InputString := ResponeStr;
          for I := 0 to Count - 1 do
          begin
            Expression := GetDownloadlinkID(Strings[I]) + ',(\d+),(.*?),(\d+),(.*?)';

            if Exec(InputString) then
              AddLink(Strings[I], Match[2], APIResultToStatus(Match[1]), StrToInt64Def(Match[3], 0), Match[4])
            else
              AddLink(Strings[I], '', lsOffline, 0);
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
