{ ********************************************************
  *                                                      *
  *  Remixshare.com Delphi API                           *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uRemixshareCom;

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
  TRemixshareCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    // function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TRemixshareCom }

function TRemixshareCom.GetName: WideString;
begin
  Result := 'Remixshare.com';
end;

function TRemixshareCom.CheckLink(AFile: WideString): TLinkInfo;
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
      AddCookie('lang_en=english', 'http://remixshare.com/');
      AddCookie('lang_en=english', 'http://www.remixshare.com/');

      _postreply := TStringStream.Create('', CP_UTF8);
      try
        Get(AFile, _postreply);

        if (Pos('Error Code:', _postreply.DataString) > 0) then
          LinkInfo.Status := lsOffline
        else
          with TRegExpr.Create do
            try
              InputString := _postreply.DataString;
              Expression := '<span title=''(.*?)''>.*?&nbsp;\(([\d\.]+)&nbsp;(\w+)\)<br />MD5: (\w+)<';

              if Exec(InputString) then
              begin
                LinkInfo.Status := lsOnline;
                LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
                LinkInfo.FileName := Match[1];
                LinkInfo.Checksum := Match[4];
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
  Result := LinkInfo;
end;

end.
