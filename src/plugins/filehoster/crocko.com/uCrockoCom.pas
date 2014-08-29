{ ********************************************************
  *                                                      *
  *  Crocko.com Delphi API                               *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uCrockoCom;

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
  TCrockoCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    // function CheckLinks(AFiles: WideString): Integer; override;
  end;

implementation

{ TCrockoCom }

function TCrockoCom.GetName: WideString;
begin
  Result := 'Crocko.com';
end;

function TCrockoCom.CheckLink(AFile: WideString): TLinkInfo;
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
      AddCookie('language=en', 'http://www.crocko.com/');

      _postreply := TStringStream.Create('', CP_UTF8);
      try
        Get(AFile, _postreply);

        if (Pos('Requested file is deleted', _postreply.DataString) > 0) then
          LinkInfo.Status := lsOffline
        else
          with TRegExpr.Create do
            try
              ModifierS := True;

              InputString := _postreply.DataString;
              // Easy-share.com
              // Expression := 'requesting:<\/span>\s+(.*?)\s+<span class="txtgray">\(([\d\.]+) (\w+)';
              Expression := 'Download:<.*?strong>(.*?)<\/.*?inner">([\d\.]+) (\w+)<\/';

              if Exec(InputString) then
              begin
                LinkInfo.Status := lsOnline;
                LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
                LinkInfo.FileName := Match[1];
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
