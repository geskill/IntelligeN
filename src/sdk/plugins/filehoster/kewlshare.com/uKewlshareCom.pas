{ ********************************************************
  *                                                      *
  *  Kewlshare.com Delphi API                            *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uKewlshareCom;

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
  uPathUtils, uSizeUtils;

type
  TKewlshareCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TKewlshareCom }

function TKewlshareCom.GetName: WideString;
begin
  result := 'Kewlshare.com';
end;

function TKewlshareCom.CheckLink(AFile: WideString): TLinkInfo;
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
  {
  with TIdHTTPHelper.Create(Self) do
    try
      _postreply := TStringStream.Create('', CP_UTF8);
      try
        Get(AFile, _postreply);

        if (Pos('<h1 style="text-align:left;">', _postreply.DataString) > 0) then
        begin
          LinkInfo.Status := lsOffline;
          Exit;
        end;

        with TRegExpr.Create do
          try
            ModifierS := True;

            InputString := _postreply.DataString;
            Expression := '<title>(.*?)\.html</title>';

            LinkInfo.Status := lsOnline;
            LinkInfo.Size := 0;

            if Exec(InputString) then
              LinkInfo.FileName := Match[1];
          finally
            Free;
          end;

      finally
        _postreply.Free;
      end;
    finally
      Free;
    end;
  }
  result := LinkInfo;
end;

end.
