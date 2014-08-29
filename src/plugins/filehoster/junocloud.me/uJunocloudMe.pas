{ ********************************************************
  *                                                      *
  *  Junocloud.me Delphi API                             *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2013 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uJunocloudMe;

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
  TJunocloudMe = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
  end;

implementation

{ TJunocloudMe }

function TJunocloudMe.GetName: WideString;
begin
  Result := 'Junocloud.me';
end;

function TJunocloudMe.CheckLink(AFile: WideString): TLinkInfo;
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
        Get(AFile, _postreply);

        if (Pos('name="fname"', _postreply.DataString) = 0) then
          LinkInfo.Status := lsOffline
        else
          with TRegExpr.Create do
            try
              InputString := _postreply.DataString;
              Expression := ': ([\d\.]+) (\w+).*?name="fname" value="(.*?)"';

              if Exec(InputString) then
              begin
                LinkInfo.Status := lsOnline;
                LinkInfo.Size := TSizeFormatter.SizeToByte(Match[1], Match[2]);
                LinkInfo.FileName := Match[3];
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
