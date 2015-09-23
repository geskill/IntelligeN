{ ********************************************************
  *                                                      *
  *  X7.to Delphi API                                    *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uX7To;

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
  uPathUtils, uSizeUtils, uHTMLUtils;

type
  TX7To = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TX7To }

function TX7To.GetName: WideString;
begin
  Result := 'X7.to';
end;

function TX7To.CheckLink(AFile: WideString): TLinkInfo;
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
        try
          Get(AFile, _postreply);
        except

        end;

        if (Pos('404', _postreply.DataString) > 0) then
          LinkInfo.Status := lsOffline
        else
          with TRegExpr.Create do
            try
              InputString := _postreply.DataString;
              Expression := 'Download<\/b>\s+\(([\d,]+) (\w+)\).*?1px 1px 2px">(.*?)<\/span>';

              if Exec(InputString) then
              begin
                LinkInfo.Status := lsOnline;
                LinkInfo.Size := TSizeFormatter.SizeToByte(Match[1], Match[2], False);
                LinkInfo.FileName := HTML2Text(Match[3]);
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

{

  function TX7To.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
  with TRegExpr.Create do
  try
  InputString := ALink;
  Expression := 'to\/(\w+)';

  if Exec(InputString) then
  Result := Match[1];
  finally
  Free;
  end;
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
  Result := lsOffline;
  if (AValue = 'online') then
  Result := lsOnline;
  end;

  var
  I: Integer;
  _params, _postreply: TStringStream;
  _OverAllPostReply: string;
  begin
  with TIdHTTPHelper.Create(Self) do
  try
  with TStringList.Create do
  try
  Text := AFiles;

  _params := TStringStream.Create('');
  _postreply := TStringStream.Create('');
  try
  _OverAllPostReply := '';
  for I := 0 to Count - 1 do
  begin
  _params.WriteString('id_' + IntToStr(I) + '=' + GetDownloadlinkID(Strings[I]) + '&');

  if (I > 0) and (I mod 80 = 0) or (I = Count - 1) then
  begin
  try
  Post('http://x7.to/api?fnc=onlinecheck', _params, _postreply);
  except

  end;
  _OverAllPostReply := _OverAllPostReply + _postreply.DataString;
  _params.Clear;
  _postreply.Clear;
  end;
  end;

  finally
  _postreply.Free;
  _params.Free;
  end;

  with TRegExpr.Create do
  try
  ModifierS := False;
  InputString := _OverAllPostReply;
  Expression := '(\w+),(\w+),(\d*?),(\w*?),(.*)';

  if Exec(InputString) then
  begin
  repeat
  for I := 0 to Count - 1 do
  if SameText(GetDownloadlinkID(Strings[I]), Match[2]) then
  begin
  AddLink(Strings[I], Match[5], APIResultToStatus(Match[1]), StrToInt64Def(Match[3], 0), Match[4]);
  break;
  end;
  until not ExecNext;
  end;
  finally
  Free;
  end;

  finally
  Free;
  end;
  finally
  Free;
  end;
  Result := FCheckedLinksList.Count;
  end;
}
end.
