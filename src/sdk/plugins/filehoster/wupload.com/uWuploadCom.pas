{ ********************************************************
  *                                                      *
  *  Wupload.com Delphi API                              *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uWuploadCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp, Variants,
  // Reg Ex
  RegExpr,
  // LkJSON
  uLkJSON,
  // Common
  uBaseConst,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TWuploadCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TWuploadCom }

function TWuploadCom.GetName: WideString;
begin
  Result := 'Wupload.com';
end;

function TWuploadCom.CheckLink(AFile: WideString): TLinkInfo;
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

function TWuploadCom.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '\/file\/(\w+\/)?(\d+)';

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
    Result := csOffline;
    if (AValue = 'AVAILABLE') then
      Result := csOnline;
  end;

var
  I, J: Integer;
  _lkJSONobject: TlkJSONobject;
  _params, _postreply: TStringStream;
  _OverAllPostReply, _IDs: string;
  FileName: string;
  FileSize: Int64;
begin
  {
  with TIdHTTPHelper.Create(Self) do
    try
      ProtocolVersion := pv1_1;
      HTTPOptions := HTTPOptions + [hoKeepOrigProtocol];

      AddCookie('lang=en', 'http://api.wupload.com/');

      with TStringList.Create do
        try
          Text := AFiles;

          _params := TStringStream.Create('');
          _postreply := TStringStream.Create('');
          try
            _OverAllPostReply := '';
            _IDs := '';
            for I := 0 to Count - 1 do
            begin
              _IDs := _IDs + GetDownloadlinkID(Strings[I]);
              if not(I = Count - 1) then
                _IDs := _IDs + ',';

              if (I > 0) and (I mod 80 = 0) or (I = Count - 1) then
              begin
                _params.WriteString('ids=' + _IDs);
                try
                  Post('http://api.wupload.com/link?method=getInfo', _params, _postreply);
                except

                end;
                _OverAllPostReply := _OverAllPostReply + _postreply.DataString;
                _IDs := '';
                _params.Clear;
                _postreply.Clear;
              end;
            end;

          finally
            _postreply.Free;
            _params.Free;
          end;

          try
            _lkJSONobject := TlkJSON.ParseText(_OverAllPostReply) as TlkJSONobject;

            for I := 0 to Count - 1 do
            begin
              for J := 0 to _lkJSONobject.Field['FSApi_Link'].Field['getInfo'].Field['response'].Field['links'].Count - 1 do
                if (GetDownloadlinkID(Strings[I]) = _lkJSONobject.Field['FSApi_Link'].Field['getInfo'].Field['response'].Field['links'].Child[J].Field['id']
                    .Value) then
                begin

                  FileName := '';
                  if Assigned(_lkJSONobject.Field['FSApi_Link'].Field['getInfo'].Field['response'].Field['links'].Child[J].Field['filename']) then
                    FileName := VarToStr(_lkJSONobject.Field['FSApi_Link'].Field['getInfo'].Field['response'].Field['links'].Child[J].Field['filename'].Value);

                  FileSize := 0;
                  if Assigned(_lkJSONobject.Field['FSApi_Link'].Field['getInfo'].Field['response'].Field['links'].Child[J].Field['size']) then
                    FileSize := StrToInt64Def
                      (VarToStr(_lkJSONobject.Field['FSApi_Link'].Field['getInfo'].Field['response'].Field['links'].Child[J].Field['size'].Value), 0);

                  AddLink(Strings[I], FileName,
                    APIResultToStatus(_lkJSONobject.Field['FSApi_Link'].Field['getInfo'].Field['response'].Field['links'].Child[J].Field['status'].Value),
                    FileSize);

                  break;
                end;
            end;

          finally
            _lkJSONobject.Free;
          end;
        finally
          Free;
        end;
    finally
      Free;
    end;
  }
  Result := FCheckedLinksList.Count;
end;

end.
