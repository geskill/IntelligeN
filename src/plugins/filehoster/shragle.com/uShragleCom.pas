{ ********************************************************
  *                                                      *
  *  Shragle.com Delphi API                              *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uShragleCom;

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
  TShragleCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    // function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TShragleCom }

function TShragleCom.GetName: WideString;
begin
  Result := 'Shragle.com';
end;

function TShragleCom.CheckLink(AFile: WideString): TLinkInfo;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := 'shragle.com/files/(.*?)/';

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
        Get('http://www.shragle.com/api.php?key=078e5ca290d728fd874121030efb4a0d&action=getStatus&fileID=' + GetDownloadlinkID(AFile), _postreply);

        with TRegExpr.Create do
          try
            InputString := _postreply.DataString;
            Expression := '(.*?)\s+(\d+)\s+(\w+)\s+';

            if Exec(InputString) then
            begin
              LinkInfo.Status := lsOnline;
              LinkInfo.Size := StrToInt64Def(Match[2], 0);
              LinkInfo.FileName := Match[1];
              LinkInfo.Checksum := Match[3];
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
