{ ********************************************************
  *                                                      *
  *  Qshare.com Delphi API                               *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uQshareCom;

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
  TQshareCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    // function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TQshareCom }

function TQshareCom.GetName: WideString;
begin
  Result := 'Qshare.com';
end;

function TQshareCom.CheckLink(AFile: WideString): TLinkInfo;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := 'get\/(\d+)\/';

        if Exec(InputString) then
          Result := Match[1];
      finally
        Free;
      end;
  end;

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
      _postreply := TStringStream.Create('', CP_UTF8);
      try
        Get('http://qshare.com/api/file_info.php?id=' + GetDownloadlinkID(AFile), _postreply);

        with TRegExpr.Create do
          try
            InputString := _postreply.DataString;
            Expression := 'ONLINE:1#SIZE:(\d+)#NAME:(.*?)#MD5:(\w+)';

            if Exec(InputString) then
            begin
              LinkInfo.Status := lsOnline;
              LinkInfo.Size := StrToInt64Def(Match[1], 0);
              LinkInfo.FileName := Match[2];
              LinkInfo.Checksum := Match[3]
            end
            else
              LinkInfo.Status := lsOffline;
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
