{ ********************************************************
  *                                                      *
  *  Extabit.com Delphi API                              *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uExtabitCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, HTTPApp,
  // Reg Ex
  RegExpr,
  // LkJSON
  uLkJSON,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // plugin system
  uPlugInFileHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TExtabitCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    // function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TExtabitCom }

function TExtabitCom.GetName: WideString;
begin
  Result := 'Extabit.com';
end;

function TExtabitCom.CheckLink(AFile: WideString): TLinkInfo;
var
  LinkInfo: TLinkInfo;

  HTTPRequest: IHTTPRequest;

  RequestID: Double;

  ResponeStr: string;
begin
  with LinkInfo do
  begin
    Link := AFile;
    Status := lsUnknown;
    Size := 0;
    FileName := '';
    Checksum := '';
  end;

  HTTPRequest := THTTPRequest.Create(AFile);
  HTTPRequest.Cookies.Add('language=en');

  RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('File not found', ResponeStr) > 0) or (Pos('Such file doesn''t exsist', ResponeStr) > 0) then
    LinkInfo.Status := lsOffline
  else
    with TRegExpr.Create do
      try
        ModifierS := True;

        InputString := ResponeStr;
        Expression := '"col-fileinfo">\s+<div title="(.*?)">.*?"col-fileinfo">([\d\.]+) (\w+)<\/td>';

        if Exec(InputString) then
        begin
          LinkInfo.Status := lsOnline;
          LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
          LinkInfo.FileName := Match[1];
        end;
      finally
        Free;
      end;

  Result := LinkInfo;
end;

end.
