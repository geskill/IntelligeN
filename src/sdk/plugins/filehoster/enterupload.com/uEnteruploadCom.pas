{ ********************************************************
  *                                                      *
  *  Enterupload.com Delphi API                          *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uEnteruploadCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp,
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
  TEnteruploadCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override;
  end;

implementation

{ TEnteruploadCom }

function TEnteruploadCom.GetName: WideString;
begin
  Result := 'Enterupload.com';
end;

function TEnteruploadCom.CheckLink(AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;
  _postreply: TStringStream;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := csUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;
  {
  with TIdHTTPHelper.Create(Self) do
    try
      AddCookie('lang=english', 'http://www.enterupload.com/');

      _postreply := TStringStream.Create('', CP_UTF8);
      try
        Get(AFile, _postreply);

        if (Pos('File Not Found', _postreply.DataString) > 0) or (Pos('The file was removed', _postreply.DataString) > 0) then
          LinkInfo.Status := csOffline
        else
          with TRegExpr.Create do
            try
              ModifierS := True;

              InputString := _postreply.DataString;

              Expression := '<h3>(.*?)<.*?([\d\.]+) (\w+)<';

              if Exec(InputString) then
              begin
                LinkInfo.Status := csOnline;
                LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
                LinkInfo.FileName := Match[1];
              end;

              if (LinkInfo.FileName = '') then
              begin
                Expression := 'name="fname" value="(.*?)"';

                if Exec(InputString) then
                begin
                  LinkInfo.Status := csOnline;
                  LinkInfo.FileName := Match[1];
                end;
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
  }
  Result := LinkInfo;
end;

end.
