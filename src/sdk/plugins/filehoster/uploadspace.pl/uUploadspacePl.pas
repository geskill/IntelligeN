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
  // Common
  uBaseConst,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TUploadspacePl = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
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
    Result := csOffline;
    if (AValue = '1') then
      Result := csOnline;
  end;

var
  LinkInfo: TLinkInfo;

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

  RequestID := HTTPManager.Get(THTTPRequest.Create('http://uploadspace.pl/api/file.php?id=' + GetDownloadlinkID(AFile)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  // 1,Dc8GWLrHJIK,the-hurt-locker-PL-p24.part3.rar,400000000

  with TRegExpr.Create do
    try
      InputString := ResponeStr;
      Expression := '(\d+),\w+,(.*?),(\d+)';

      if Exec(InputString) then
      begin
        LinkInfo.Status := APIResultToStatus(Match[1]);
        LinkInfo.Size := TSizeFormatter.SizeToByte(Match[3]);
        LinkInfo.FileName := Match[2];
      end
      else
        LinkInfo.Status := csOffline;
    finally
      Free;
    end;

  Result := LinkInfo;
end;

end.
