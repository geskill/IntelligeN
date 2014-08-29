{ ********************************************************
  *                                                      *
  *  Datei.to Delphi API                                 *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uDateiTo;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp,
  // Reg Ex
  RegExpr,
  // LkJSON
  uLkJSON,
  // plugin system
  uIdHTTPHelper, uPlugInFileHosterClass, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TDateiTo = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    // function CheckLinks(AFiles: WideString): Integer; override;
  end;

implementation

{ TDateiTo }

function TDateiTo.GetName: WideString;
begin
  Result := 'Datei.to';
end;

function TDateiTo.CheckLink(AFile: WideString): TLinkInfo;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := 'datei\.to/datei/(.*?)\.html';

        if Exec(InputString) then
          Result := Match[1];
      finally
        Free;
      end;
  end;

var
  LinkInfo: TLinkInfo;
  _params, _postreply: TStringStream;
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
      _params := TStringStream.Create('', CP_UTF8);
      try
        _params.WriteString('key=YYMHGBR9SFQA0ZWA&info=COMPLETE&datei=' + GetDownloadlinkID(AFile));

        Post('http://api.datei.to/', _params, _postreply);

        if (Pos('offline', _postreply.DataString) > 0) then
          LinkInfo.Status := lsOffline
        else
          with TRegExpr.Create do
            try
              ModifierS := True;

              InputString := _postreply.DataString;
              Expression := ';(.*?);(\d+)';

              if Exec(InputString) then
              begin
                LinkInfo.Status := lsOnline;
                LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2]);
                LinkInfo.FileName := Match[1];
              end;
            finally
              Free;
            end;
      finally
        _params.Free;
        _postreply.Free;
      end;
    finally
      Free;
    end;
  Result := LinkInfo;
end;

end.
