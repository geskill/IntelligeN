{ ********************************************************
  *                                                      *
  *  Uploadspace.pl Delphi API                           *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uUploadspacePl;

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
  TUploadspacePl = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    // function CheckLinks(AFiles: WideString): Integer; override; stdcall;
  end;

implementation

{ TUploadspacePl }

function TUploadspacePl.GetName: WideString;
begin
  Result := 'Uploadspace.pl';
end;

function TUploadspacePl.CheckLink(AFile: WideString): TLinkInfo;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := 'pl\/plik(\w+)';

        if Exec(InputString) then
          Result := Match[1];
      finally
        Free;
      end;
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := lsOffline;
    if (AValue = '1') then
      Result := lsOnline;
  end;

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
      _postreply := TStringStream.Create('', CP_UTF8);
      try
        try
          Get('http://uploadspace.pl/api/file.php?id=' + GetDownloadlinkID(AFile), _postreply);
        except

        end;

        // 1,Dc8GWLrHJIK,the-hurt-locker-PL-p24.part3.rar,400000000

        with TRegExpr.Create do
          try
            InputString := _postreply.DataString;
            Expression := '(\d+),\w+,(.*?),(\d+)';

            if Exec(InputString) then
            begin
              LinkInfo.Status := APIResultToStatus(Match[1]);
              LinkInfo.Size := TSizeFormatter.SizeToByte(Match[3]);
              LinkInfo.FileName := Match[2];
            end
            else
              LinkInfo.Status := lsOffline;
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

end.
