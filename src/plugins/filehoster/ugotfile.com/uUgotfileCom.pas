{ ********************************************************
  *                                                      *
  *  Ugotfile.com Delphi API                             *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uUgotfileCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp,
  // Reg Ex
  RegExpr,
  // plugin system
  uIdHTTPHelper, uPlugInFileHosterClass, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TUgotfileCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TUgotfileCom }

function TUgotfileCom.GetName: WideString;
begin
  Result := 'Ugotfile.com';
end;

function TUgotfileCom.CheckLink(AFile: WideString): TLinkInfo;
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

function TUgotfileCom.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := 'file\/(\d+)\/';

        if Exec(InputString) then
          Result := Match[1];
      finally
        Free;
      end;
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := lsOffline;
    if (AValue = 'Alive') then
      Result := lsOnline;
  end;

var
  I: Integer;
  _params, _postreply: TStringStream;
  _OverAllPostReply, _Links: string;
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
            _Links := '';
            for I := 0 to Count - 1 do
            begin
              _Links := _Links + HTTPEncode(Strings[I]);
              if not(I = Count - 1) then
                _Links := _Links + sLineBreak;

              if (I > 0) and (I mod 100 = 0) or (I = Count - 1) then
              begin
                _params.WriteString('links=' + _Links);
                try
                  Post('http://ugotfile.com/tools/check-links', _params, _postreply);
                except

                end;
                _OverAllPostReply := _OverAllPostReply + _postreply.DataString;
                _Links := '';
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
              InputString := _OverAllPostReply;

              for I := 0 to Count - 1 do
              begin
                // \s = Zeilenende
                Expression := Strings[I] + '.*?''>([\d\.]+) (\w+).*?''>(.*?)<';

                if Exec(InputString) then
                  AddLink(Strings[I], ExtractUrlFileName(Strings[I]), APIResultToStatus(Match[3]), TSizeFormatter.SizeToByte(Match[1], Match[2]))
                else
                  AddLink(Strings[I], '', lsOffline, 0);
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

end.