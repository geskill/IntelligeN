{ ********************************************************
  *                                                      *
  *  Filefactory.com Delphi API                          *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2010 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uFilefactoryCom;

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
  TFilefactoryCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; safecall;
    function CheckLinks(AFiles: WideString): Integer; override; safecall;
  end;

implementation

{ TFilefactoryCom }

function TFilefactoryCom.GetName: WideString;
begin
  Result := 'Filefactory.com';
end;

function TFilefactoryCom.CheckLink(AFile: WideString): TLinkInfo;
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

  RequestID := HTTPManager.Get(THTTPRequest.Create(AFile), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  if (Pos('File Not Found', ResponeStr) > 0) then
    LinkInfo.Status := csOffline
  else
    with TRegExpr.Create do
      try
        ModifierS := True;

        InputString := ResponeStr;
        Expression := 'class="last">(.*?)<\/span>.*?<span>([\d\.]+) (\w+)';

        if Exec(InputString) then
        begin
          LinkInfo.Status := csOnline;
          LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
          LinkInfo.FileName := Match[1];
        end;
      finally
        Free;
      end;

  Result := LinkInfo;
end;

(*
  <tr class="even">

  <td>
  <a href="http://www.filefactory.com/file/b33h633/n/Photoshop_CS5_Fuer_Fortgeschrittene_part01_rar">Photoshop.CS5.Fuer.Fortgeschrittene.part01.rar</a>
  <div class="metadata">http://www.filefactory.com/file/b33h633/n/Photoshop_CS5_Fuer_Fortgeschrittene_part01_rar</div>
  </td>
  <td>202.18 MB</td>
  </tr>
  <tr class="odd">
  <td>

  <a href="http://www.filefactory.com/file/b33h638/n/Photoshop_CS5_Fuer_Fortgeschrittene_part02_rar">Photoshop.CS5.Fuer.Fortgeschrittene.part02.rar</a>
  <div class="metadata">http://www.filefactory.com/file/b33h638/n/Photoshop_CS5_Fuer_Fortgeschrittene_part02_rar</div>
  </td>
  <td>202.18 MB</td>
  </tr>
  *)
function TFilefactoryCom.CheckLinks(AFiles: WideString): Integer;

  function ResonseLinkToGivenLink(AResonseLink, ALinks: string): string;
  var
    I: Integer;
  begin
    with TStringList.Create do
      try
        Text := ALinks;

        for I := 0 to Count - 1 do
          if (Pos(LowerCase(AResonseLink), LowerCase(Strings[I])) > 0) or (Pos(LowerCase(Strings[I]), LowerCase(AResonseLink)) > 0) then
          begin
            Result := Strings[I];
            break;
          end;
      finally
        Free;
      end;
  end;

  function GetRequestLinks(ALinks: TStrings): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to ALinks.Count - 1 do
    begin
      if (I > 0) then
        Result := Result + sLineBreak;
      Result := Result + ALinks.Strings[I];
    end;
  end;

var
  Links: TStringList;

  _UnrecognizedLinks: string;

  HTTPParams: IHTTPParams;

  RequestID: Double;

  ResponeStr: string;
begin
  Links := TStringList.Create;

  with Links do
    try
      Text := AFiles;

      HTTPParams := THTTPParams.Create;
      with HTTPParams do
      begin
        AddFormField('func', 'links');
        AddFormField('links', GetRequestLinks(Links));
      end;

      RequestID := HTTPManager.Post(THTTPRequest.Create('http://filefactory.com/tool/links.php'), HTTPParams, TPlugInHTTPOptions.Create(Self));

      repeat
        sleep(50);
      until HTTPManager.HasResult(RequestID);

      ResponeStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

      with TRegExpr.Create do
        try
          ModifierS := False;
          InputString := ResponeStr;
          Expression := '>(.*?)<\/a>\s+<div class="metadata">(.*?)<\/div>\s+<\/td>\s+<td>([\d\.]+) (\w+)<\/td>';

          if Exec(InputString) then
          begin
            repeat
              AddLink(Match[1], ResonseLinkToGivenLink(Match[1], AFiles), csOnline, TSizeFormatter.SizeToByte(Match[3], Match[4]));
            until not ExecNext;
          end;

          ModifierS := True;
          Expression := 'Unrecognized Links<\/h2>\s+<ul class="items">(.*?)<\/ul>';

          if Exec(InputString) then
          begin
            _UnrecognizedLinks := Match[1];

            ModifierS := False;
            Expression := '<div class="metadata">(.*?)<\/div>';
            if Exec(InputString) then
            begin
              repeat
                AddLink(Match[1], ResonseLinkToGivenLink(Match[1], AFiles), csOffline, 0);
              until not ExecNext;
            end;
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
