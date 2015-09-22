{ ********************************************************
  *                                                      *
  *  Kickload.com Delphi API                             *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uKickloadCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, Variants, HTTPApp,
  // Reg Ex
  RegExpr,
  // LkJSON
  uLkJSON,
  // plugin system
  uIdHTTPHelper, uPlugInFileHosterClass, uPlugInConst,
  // Utils
  uPathUtils;

type
  TKickloadCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TKickloadCom }

function TKickloadCom.GetName: WideString;
begin
  Result := 'Kickload.com';
end;

function TKickloadCom.CheckLink(AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := lsUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;
  Result := LinkInfo;
end;

function TKickloadCom.CheckLinks(AFiles: WideString): Integer;
var
  _params, _postreply: TStringStream;
  I: Integer;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := lsOffline;
    if (AValue = 'OK') then
      Result := lsOnline;
  end;

begin
  with TIdHTTPHelper.Create(Self) do
    try
      with TStringList.Create do
        try
          Text := AFiles;

          _params := TStringStream.Create('');
          _postreply := TStringStream.Create('');
          try
            _params.WriteString('url=');
            for I := 0 to Count - 1 do
            begin
              if (I > 0) then
                _params.WriteString(HTTPEncode('|'));
              _params.WriteString(HTTPEncode(Strings[I]));
            end;

            try
              Get('http://api.kickload.com/linkcheck.php?' + _params.DataString, _postreply);
            except

            end;

            with TRegExpr.Create do
              try
                InputString := _postreply.DataString;
                Expression := '(\w+);;(.*?);;(\d+)|(\d+);;FILE';

                I := 0;
                if Exec(InputString) then
                begin
                  repeat
                    AddLink(Strings[I], Match[2], APIResultToStatus(Match[1]), StrToInt64Def(Match[3], 0));
                    Inc(I);
                  until not ExecNext;
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
  Result := FCheckedLinksList.Count;
end;

end.
