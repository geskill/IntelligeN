{ ********************************************************
  *                                                      *
  *  Filesonic.com Delphi API                            *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uFilesonicCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, Variants, HTTPApp,
  // Reg Ex
  RegExpr,
  // LkJSON
  uLkJSON,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils;

type
  TFilesonicCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TFilesonicCom }

function TFilesonicCom.GetName: WideString;
begin
  result := 'Filesonic.com';
end;

function TFilesonicCom.CheckLink(AFile: WideString): TLinkInfo;
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
  result := LinkInfo;
end;

function TFilesonicCom.CheckLinks(AFiles: WideString): Integer;
var
  _params, _postreply: TStringStream;
  _lkJSONobject: TlkJSONobject;
  I, J: Integer;
  FileName: string;
  FileSize: Int64;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '/file/(\d+)';

        if Exec(InputString) then
          result := Match[1]
        else
        begin
          Expression := '/file/[a-z0-9]+/(\d+)';
          if Exec(InputString) then
            result := Match[1];
        end;

      finally
        Free;
      end;
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    result := lsOffline;
    if (AValue = 'AVAILABLE') then
      result := lsOnline;
  end;

begin
  {
  with TIdHTTPHelper.Create(Self) do
    try
      ProtocolVersion := pv1_1;
      HTTPOptions := HTTPOptions + [hoKeepOrigProtocol];

      AddCookie('lang=en', 'http://api.filesonic.com/');

      with TStringList.Create do
        try
          Text := AFiles;

          _params := TStringStream.Create('');
          _postreply := TStringStream.Create('');
          try
            _params.WriteString('ids=');
            for I := 0 to Count - 1 do
            begin
              if (I > 0) then
                _params.WriteString(',');
              _params.WriteString(GetDownloadlinkID(Strings[I]));
            end;

            try
              Post('http://api.filesonic.com/link?method=getInfo', _params, _postreply);
            except

            end;

            try
              _lkJSONobject := TlkJSON.ParseText(_postreply.DataString) as TlkJSONobject;

              for I := 0 to Count - 1 do
              begin
                for J := 0 to _lkJSONobject.Field['FSApi_Link'].Field['getInfo'].Field['response'].Field['links'].Count - 1 do
                  if (GetDownloadlinkID(Strings[I]) = _lkJSONobject.Field['FSApi_Link'].Field['getInfo'].Field['response'].Field['links'].Child[J].Field['id']
                      .Value) then
                  begin

                    FileName := '';
                    if Assigned(_lkJSONobject.Field['FSApi_Link'].Field['getInfo'].Field['response'].Field['links'].Child[J].Field['filename']) then
                      FileName := VarToStr(_lkJSONobject.Field['FSApi_Link'].Field['getInfo'].Field['response'].Field['links'].Child[J].Field['filename']
                          .Value);

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
            _postreply.Free;
            _params.Free;
          end;

        finally
          Free;
        end;
    finally
      Free;
    end;
  }
  result := FCheckedLinksList.Count;
end;

end.
