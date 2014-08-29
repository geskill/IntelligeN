{ ********************************************************
  *                                                      *
  *  Depositfiles.com Delphi API                         *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uDepositfilesCom;

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
  TDepositfilesCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    // function CheckLinks(AFiles: WideString): Integer; override;
  end;

implementation

{ TDepositfilesCom }

function TDepositfilesCom.GetName: WideString;
begin
  Result := 'Depositfiles.com';
end;

function TDepositfilesCom.CheckLink(AFile: WideString): TLinkInfo;
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
      AddCookie('lang_current=de', 'http://depositfiles.com/');

      _postreply := TStringStream.Create('', CP_UTF8);
      try
        Get(AFile, _postreply);

        if (Pos('html_download_api-not_exists', _postreply.DataString) > 0) then
          LinkInfo.Status := lsOffline
        else
          with TRegExpr.Create do
            try
              ModifierS := True;

              InputString := _postreply.DataString;
              Expression := '<b title=\"(.*?)\".*?<b>([\d\.]+)&nbsp;(\w+)';

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

(*
  // Cookie vorgeschaltet

  {"links_existed":[],"links_deleted":{"ve4w82ro7":{"id_str":"ve4w82ro7","filename":"DFManagerSetup.exe","size":"1063760","download_url":"http://depositfiles.com/files/ve4w82ro7"}}}

  function TDepositfilesCom.CheckLinks(AFiles: WideString): Integer;
  var
  I: Integer;
  _params, _postreply: TStringStream;
  _lkJSONobject: TlkJSONobject;
  begin
  with TIdHTTPHelper.Create(Self) do
  try
  AddCookie('lang_current=de', 'http://bonus.depositfiles.com/');

  with TStringList.Create do
  try
  Text := AFiles;

  _params := TStringStream.Create('');
  _postreply := TStringStream.Create('');
  try
  _params.WriteString('links=');
  for I := 0 to Count - 1 do
  begin
  if (I > 0) then
  _params.WriteString(sLineBreak);
  _params.WriteString(Strings[I]);
  end;

  try
  Post('http://bonus.depositfiles.com/de/links_checker.php', _params, _postreply);

  except

  end;

  _lkJSONobject := TlkJSON.ParseText(_postreply.DataString) as TlkJSONobject;
  try
  with _lkJSONobject.Field['links_existed'] do
  for I := 0 to Count - 1 do
  with Child[I] do
  AddLink(Field['download_url'].Value, Field['filename'].Value, lsOnline, Field['size'].Value);
  with _lkJSONobject.Field['links_deleted'] do
  for I := 0 to Count - 1 do
  with Child[I] do
  AddLink(Field['download_url'].Value, Field['filename'].Value, lsOffline, Field['size'].Value);
  finally
  _lkJSONobject.Free;
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
  end;
  *)
end.
