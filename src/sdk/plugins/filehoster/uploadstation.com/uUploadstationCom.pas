{ ********************************************************
  *                                                      *
  *  Uploadstation.com Delphi API                        *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uUploadstationCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp, DateUtils,
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
  TUploadstationCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(const AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(const AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TUploadstationCom }

function TUploadstationCom.GetName: WideString;
begin
  Result := 'Uploadstation.com';
end;

function TUploadstationCom.CheckLink(const AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := csUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;
  Result := LinkInfo;
end;

function TUploadstationCom.CheckLinks(const AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '\/file\/(\w+)';

        if Exec(InputString) then
          Result := Match[2];
      finally
        Free;
      end;
  end;

  function APIResultToString(AValue: string): string;
  begin
    Result := AValue;
    if (AValue = '--') then
      Result := '';
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := csOffline;
    if (AValue = 'Available') then
      Result := csOnline;
  end;

var
  I: Integer;
  _OverAllPostReply, _Links: string;

  HTTPParams: IHTTPParams;

  RequestID: Double;

  ResponeStr: string;
begin
  with TStringList.Create do
    try
      Text := AFiles;

      _OverAllPostReply := '';
      _Links := '';
      for I := 0 to Count - 1 do
      begin
        _Links := _Links + HTTPEncode(Strings[I]);
        if not(I = Count - 1) then
          _Links := _Links + sLineBreak;

        if (I > 0) and (I mod 100 = 0) or (I = Count - 1) then
        begin
          HTTPParams := THTTPParams.Create;
          with HTTPParams do
            AddFormField('urls', _Links);

          RequestID := HTTPManager.Post(THTTPRequest.Create('http://www.uploadstation.com/check-links.php'), HTTPParams, TPlugInHTTPOptions.Create(Self));

          HTTPManager.WaitFor(RequestID);

          ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

          _OverAllPostReply := _OverAllPostReply + ResponeStr;
          _Links := '';
        end;
      end;

      with TRegExpr.Create do
        try
          InputString := _OverAllPostReply;
          // Leerzeichen vorne wichtig
          Expression := ' col1">(.*?)<.*?col2">(.*?)<.*?col3">([\d\.]*?)[ -]+(\w*?)<.*?col4">(.*?)<';

          if Exec(InputString) then
          begin
            repeat
              for I := 0 to Count - 1 do
                if SameText(GetDownloadlinkID(Strings[I]), GetDownloadlinkID(Match[1])) then
                begin
                  AddLink(Strings[I], APIResultToString(Match[2]), APIResultToStatus(Match[5]), TSizeFormatter.SizeToByte(Match[3], Match[4]));
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
  Result := FCheckedLinksList.Count;
end;

end.
