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
  // Reg Ex
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TFilejungleCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(AFiles: WideString): Integer; override; safecall;
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
  _OverAllPostReply, _URLs: string;

  HTTPParams: IHTTPParams;

  RequestID: Double;

  ResponeStr: string;
begin
  with TStringList.Create do
    try
      Text := AFiles;

      _OverAllPostReply := '';
      _URLs := '';
      for I := 0 to Count - 1 do
      begin
        _URLs := _URLs + HTTPEncode(Strings[I]);
        if not(I = Count - 1) then
          _URLs := _URLs + HTTPEncode(sLineBreak);

        if (length(GetRequestString(_URLs)) > 100) or (I = Count - 1) then
        begin
          HTTPParams := THTTPParams.Create;
          with HTTPParams do
            AddFormField('urls', _URLs);

          RequestID := HTTPManager.Post(THTTPRequest.Create('http://www.filejungle.com/check_links.php'), HTTPParams, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID);

          ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

          _OverAllPostReply := _OverAllPostReply + ResponeStr;
          _URLs := '';
        end;
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
  Result := FCheckedLinksList.Count;
end;

end.
