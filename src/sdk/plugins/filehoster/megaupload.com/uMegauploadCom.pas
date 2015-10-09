{ ********************************************************
  *                                                      *
  *  Megaupload.com Delphi API                           *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uMegauploadCom;

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
  uPathUtils;

type
  TMegauploadCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TMegauploadCom }

function TMegauploadCom.GetName: WideString;
begin
  result := 'Megaupload.com';
end;

function TMegauploadCom.CheckLink(AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := csUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;
  result := LinkInfo;
end;

function TMegauploadCom.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := 'd=([a-zA-Z0-9]+)';

        if Exec(InputString) then
          result := Match[1];
      finally
        Free;
      end;
  end;

var
  I: Integer;
  _params, _postreply: TStringStream;
begin
  {
  with TIdHTTPHelper.Create(Self) do
    try
      with TStringList.Create do
        try
          Text := AFiles;

          _params := TStringStream.Create('');
          _postreply := TStringStream.Create('');
          try
            for I := 0 to Count - 1 do
              _params.WriteString('id' + IntToStr(I) + '=' + GetDownloadlinkID(Strings[I]) + '&');

            try
              Post('http://megaupload.com/mgr_linkcheck.php', _params, _postreply);
            except

            end;

            with TRegExpr.Create do
              try
                InputString := _postreply.DataString;

                for I := 0 to Count - 1 do
                begin
                  // \s = Zeilenende
                  Expression := '&id' + IntToStr(I) + '=(\d+)&s=(\d+)&d=0&n=(.*?)(&|\Z)';

                  if Exec(InputString) then
                    AddLink(Strings[I], Match[3], csOnline, StrToInt64Def(Match[2], 0))
                  else
                    AddLink(Strings[I], '', csOffline, 0);
                end;
              finally
                Free;
              end;
          finally
            _postreply.Free;
            _params.Free;
          end;

        finally
          Free;
        end;
    finally
      Free;
    end;
  }
  result := FCheckedLinksList.Count;
end;

end.
