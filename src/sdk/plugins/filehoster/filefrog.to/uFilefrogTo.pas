{ ********************************************************
  *                                                      *
  *  Filefrog.to Delphi API                              *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uFilefrogTo;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp,
  // Reg Ex
  RegExpr,
  // Common
  uBaseConst,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TFilefrogTo = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
  end;

implementation

{ TFilefrogTo }

function TFilefrogTo.GetName: WideString;
begin
  Result := 'Filefrog.to';
end;

function TFilefrogTo.CheckLink(AFile: WideString): TLinkInfo;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := csOffline;
    if (AValue = 'online') then
      Result := csOnline;
  end;

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
      _postreply := TStringStream.Create('', CP_UTF8);
      try
        try
          Get(StringReplace(AFile, 'download', 'api/status', [rfIgnoreCase]), _postreply);
        except

        end;
        with TRegExpr.Create do
          try
            InputString := _postreply.DataString;
            Expression := '(.*?);(.*?);(\d+)';

            if Exec(InputString) then
            begin
              LinkInfo.Status := APIResultToStatus(Match[1]);
              LinkInfo.Size := TSizeFormatter.SizeToByte(Match[3]);
              LinkInfo.FileName := Match[2];
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
