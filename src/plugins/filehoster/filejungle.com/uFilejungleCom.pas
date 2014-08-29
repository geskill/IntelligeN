{ ********************************************************
  *                                                      *
  *  Filejungle.com Delphi API                           *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uFilejungleCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, Variants, HTTPApp,
  // Indy
  IdHTTP,
  // Reg Ex
  RegExpr,
  // plugin system
  uIdHTTPHelper, uPlugInFileHosterClass, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TFilejungleCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TFilejungleCom }

function TFilejungleCom.GetName: WideString;
begin
  Result := 'Filejungle.com';
end;

function TFilejungleCom.CheckLink(AFile: WideString): TLinkInfo;
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

function TFilejungleCom.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '\/f\/(\w+)';

        if Exec(InputString) then
          Result := Match[1];
      finally
        Free;
      end;
  end;

  function GetRequestString(AIDs: string): string;
  begin
    Result := 'urls=' + AIDs;
  end;

var
  I: Integer;
  _params, _postreply: TStringStream;
  _OverAllPostReply, _URLs: string;
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
            _URLs := '';
            for I := 0 to Count - 1 do
            begin
              _URLs := _URLs + HTTPEncode(Strings[I]);
              if not(I = Count - 1) then
                _URLs := _URLs + HTTPEncode(sLineBreak);

              if (length(GetRequestString(_URLs)) > 100) or (I = Count - 1) then
              begin
                _params.WriteString(GetRequestString(_URLs));
                try
                  Post('http://www.filejungle.com/check_links.php', _params, _postreply);
                except

                end;
                _OverAllPostReply := _OverAllPostReply + _postreply.DataString;
                _URLs := '';
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
                Expression := '\/f\/' + GetDownloadlinkID(Strings[I]) + '<\/.*?col2">(.*?)<\/.*?col3">([\d\.]+) (\w+)<\/';

                if Exec(InputString) then
                  AddLink(Strings[I], Match[1], lsOnline, TSizeFormatter.SizeToByte(Match[2], Match[3]))
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
