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
  // Common
  uBaseConst,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TDepositfilesCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
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

  HTTPRequest: IHTTPRequest;

  RequestID: Double;

  ResponeStr: string;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := csUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;

  HTTPRequest := THTTPRequest.Create(AFile);
  HTTPRequest.Cookies.Add('lang_current=de');

  RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('html_download_api-not_exists', ResponeStr) > 0) then
    LinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        ModifierS := True;

        InputString := ResponeStr;
        Expression := '<b title=\"(.*?)\".*?<b>([\d\.]+)&nbsp;(\w+)';

        if Exec(InputString) then
        begin
          LinkInfo.Status := csOnline;
          LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
          LinkInfo.FileName := Match[1];
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
  AddLink(Field['download_url'].Value, Field['filename'].Value, csOnline, Field['size'].Value);
  with _lkJSONobject.Field['links_deleted'] do
  for I := 0 to Count - 1 do
  with Child[I] do
  AddLink(Field['download_url'].Value, Field['filename'].Value, csOffline, Field['size'].Value);
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
