{ ********************************************************
  *                                                      *
  *  Letitbit.net Delphi API                             *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uLetitbitNet;

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
  TLetitbitNet = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    // function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TLetitbitNet }

function TLetitbitNet.GetName: WideString;
begin
  Result := 'Letitbit.net';
end;

function TLetitbitNet.CheckLink(AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;
  _postreply: TStringStream;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := lsUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;
  with TIdHTTPHelper.Create(Self) do
    try
      AddCookie('lang=en', 'http://letitbit.net/');

      _postreply := TStringStream.Create('', CP_UTF8);
      try
        Get(AFile, _postreply);

        if (Pos('<title>404</title>', _postreply.DataString) > 0) or (Pos('File not found', _postreply.DataString) > 0) then
          LinkInfo.Status := lsOffline
        else
          with TRegExpr.Create do
            try
              InputString := _postreply.DataString;

              Expression := 'name="realname" value="(.*?)"';
              if Exec(InputString) then
              begin
                LinkInfo.Status := lsOnline;
                LinkInfo.FileName := Match[1];
              end;

              Expression := 'name="sssize" value="(\d+)"';
              if Exec(InputString) then
                LinkInfo.Size := StrToInt64Def(Match[1], 0);

            finally
              Free;
            end;

      finally
        _postreply.Free;
      end;
    finally
      Free;
    end;
  Result := LinkInfo;
end;

end.
