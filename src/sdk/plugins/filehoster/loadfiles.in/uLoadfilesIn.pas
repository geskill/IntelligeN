{ ********************************************************
  *                                                      *
  *  Loadfiles.in Delphi API                             *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uLoadfilesIn;

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
  TLoadfilesIn = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TLoadfilesIn }

function TLoadfilesIn.GetName: WideString;
begin
  Result := 'Loadfiles.in';
end;

function TLoadfilesIn.CheckLink(AFile: WideString): TLinkInfo;
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
      AddCookie('lang=english', 'http://loadfiles.in/');

      _postreply := TStringStream.Create('', CP_UTF8);
      try
        Get(AFile, _postreply);

        if (Pos('File Not Found', _postreply.DataString) > 0) then
          LinkInfo.Status := lsOffline
        else
          with TRegExpr.Create do
            try
              InputString := _postreply.DataString;
              Expression := '<h3 class="filename">(.*?)<.*?<small>\((\d+) bytes\)';

              if Exec(InputString) then
              begin
                LinkInfo.Status := lsOnline;
                LinkInfo.Size := StrToInt64Def(Match[2], 0);
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
