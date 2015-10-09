{ ********************************************************
  *                                                      *
  *  Oron.com Delphi API                                 *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uOronCom;

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
  TOronCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TOronCom }

function TOronCom.GetName: WideString;
begin
  Result := 'Oron.com';
end;

function TOronCom.CheckLink(AFile: WideString): TLinkInfo;
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
      AddCookie('lang=english', 'http://oron.com/');
      AddCookie('lang=english', 'http://www.oron.com/');

      _postreply := TStringStream.Create('', CP_UTF8);
      try
        Get(AFile, _postreply);

        if (Pos('File Not Found', _postreply.DataString) > 0) then
          LinkInfo.Status := csOffline
        else
          with TRegExpr.Create do
            try
              InputString := _postreply.DataString;
              Expression := '<b class="f_arial f_14px">(.*?)<.*?: ([\d\.]+) (\w+)<';

              if Exec(InputString) then
              begin
                LinkInfo.Status := csOnline;
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
  }
  Result := LinkInfo;
end;

end.
