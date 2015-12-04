{ ******************************************************
  *                                                    *
  *  Cloudzer.net Delphi API                           *
  *  Version 2.0.0.0                                   *
  *  Copyright (c) 2013 Sebastian Klatte               *
  *                                                    *
  ****************************************************** }
unit uCloudzerNet;

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
  TCloudzerNet = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

function GetDownloadlinkID(ALink: string): string;
begin
  ALink := StringReplace(ALink, 'clz.to/', 'cloudzer.net/', [rfReplaceAll, rfIgnoreCase]);

  with TRegExpr.Create do
    try
      InputString := IncludeTrailingUrlDelimiter(ALink);
      Expression := 'cloudzer.net/file/(.*?)/';

      if Exec(InputString) then
        Result := Match[1];
    finally
      Free;
    end;
end;

{ TCloudzerNet }

function TCloudzerNet.GetName: WideString;
begin
  Result := 'Cloudzer.net';
end;

function TCloudzerNet.CheckLink(AFile: WideString): TLinkInfo;
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
  RequestID := HTTPManager.Get(THTTPRequest.Create('http://cloudzer.net/file/' + GetDownloadlinkID(AFile) + '/status'), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (HTTPManager.GetResult(RequestID).HTTPResult.HTTPResponse.Code = 404) or (HTTPManager.GetResult(RequestID).HTTPResult.HTTPResponse.Code = 410) then
    LinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        InputString := ResponeStr;
        Expression := '(.*?)\s+([\d,]+) (\w+)';

        if Exec(InputString) then
        begin
          LinkInfo.Status := csOnline;
          LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3], False);
          LinkInfo.FileName := Match[1];
        end;
      finally
        Free;
      end;

  Result := LinkInfo;
end;

function TCloudzerNet.CheckLinks(AFiles: WideString): Integer;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := csOffline;
    if (AValue = 'online') then
      Result := csOnline;
  end;

var
  I, J, retry: Integer;
  _OverAllPostReply: string;

  HTTPParams: IHTTPParams;

  RequestID: Double;

  ResponeStr: string;

begin
  with TStringList.Create do
    try
      Text := AFiles;

      HTTPParams := THTTPParams.Create;
      J := 0;
      for I := 0 to Count - 1 do
      begin
        HTTPParams.AddFormField('id_' + IntToStr(J), GetDownloadlinkID(Strings[I]));
        Inc(J);

        if (I > 0) and (I mod 80 = 0) or (I = Count - 1) then
        begin
          HTTPParams.AddFormField('apikey', 'mai1EN4Zieghey1QueGie7fei4eeh5ne');
          retry := 0;

          while retry < 5 do
          begin
            try
              RequestID := HTTPManager.Post(THTTPRequest.Create('http://cloudzer.net/api/filemultiple'), HTTPParams, TPlugInHTTPOptions.Create(Self));

              repeat
                sleep(50);
              until HTTPManager.HasResult(RequestID);

              ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

              if SameText(ResponeStr, '') then
              begin
                Inc(retry);
                sleep(550);
              end
              else
                break;
            except

            end;
          end;

          _OverAllPostReply := _OverAllPostReply + ResponeStr;
          HTTPParams := THTTPParams.Create;
          J := 0;
        end;
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
  Result := FCheckedLinksList.Count;
end;

end.
