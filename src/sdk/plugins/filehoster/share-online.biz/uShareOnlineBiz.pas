{ ********************************************************
  *                                                      *
  *  Share-online.biz Delphi API                         *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uShareOnlineBiz;

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
  uPathUtils;

type
  TShareOnlineBiz = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TShareOnlineBiz }

function TShareOnlineBiz.GetName: WideString;
begin
  Result := 'Share-online.biz';
end;

function TShareOnlineBiz.CheckLink(AFile: WideString): TLinkInfo;
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

function TShareOnlineBiz.CheckLinks(AFiles: WideString): Integer;

  function GetDownloadlinkID(ALink: string): string;
  begin
    with TRegExpr.Create do
      try
        InputString := ALink;
        Expression := '\/dl\/([a-zA-Z0-9]+)';

        if Exec(InputString) then
          Result := Match[1];
      finally
        Free;
      end;

    if (Result = '') then
      with TRegExpr.Create do
        try
          InputString := ALink;
          Expression := 'id=([a-zA-Z0-9]+)';

          if Exec(InputString) then
            Result := Match[1];
        finally
          Free;
        end;
  end;

  function APIResultToStatus(AValue: string): TLinkStatus;
  begin
    Result := csOffline;
    if (AValue = 'OK') then
      Result := csOnline;
  end;

  function GetRequestString(AIDs: string): string;
  begin
    Result := 'links=' + AIDs;
  end;

var
  I: Integer;
  _OverAllPostReply, _IDs: string;

  HTTPParams: IHTTPParams;

  RequestID: Double;

  ResponeStr: string;
begin
  with TStringList.Create do
    try
      Text := AFiles;

      _OverAllPostReply := '';
      _IDs := '';
      for I := 0 to Count - 1 do
      begin
        _IDs := _IDs + GetDownloadlinkID(Strings[I]);
        if not(I = Count - 1) then
          _IDs := _IDs + sLineBreak;

        if (length(GetRequestString(_IDs)) > 200) or (I = Count - 1) then
        begin
          HTTPParams := THTTPParams.Create;
          with HTTPParams do
            AddFormField('links', GetRequestString(_IDs));

          RequestID := HTTPManager.Post(THTTPRequest.Create('http://api.share-online.biz/linkcheck.php?md5=1'), HTTPParams, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID);

          ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

          _OverAllPostReply := _OverAllPostReply + ResponeStr;
          _IDs := '';
        end;
      end;

      with TRegExpr.Create do
        try
          InputString := _OverAllPostReply;
          Expression := '(\w+);(\w+);(.*?);(\d+);(\w+)|(\w+);(\w+);';

          // 3JHHUEPL157;OK;Bad.Teacher.2011.R5.Line.Dubbed.German.XviD-POE.part1.rar;471859200;ec53507a1efc8a1ceb3d1ce19751f2f4
          // XCUMPQJLIEK;NOTFOUND;

          if Exec(InputString) then
          begin
            repeat
              for I := 0 to Count - 1 do
                if SameText(GetDownloadlinkID(Strings[I]), Match[1]) or SameText(GetDownloadlinkID(Strings[I]), Match[6]) then
                begin
                  if SameText(Match[6], '') then
                    AddLink(Strings[I], Match[3], APIResultToStatus(Match[2]), StrToInt64Def(Match[4], 0), Match[5])
                  else
                    AddLink(Strings[I], '', csOffline, 0);
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
