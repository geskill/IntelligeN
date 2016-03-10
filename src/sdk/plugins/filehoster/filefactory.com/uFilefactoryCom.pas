{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Filefactory.com Delphi API                          *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
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
  uPlugInConst, uPlugInInterface, uPlugInFileHosterClass, uPlugInFileHosterClasses, uPlugInHTTPClasses,
  // Utils
  uPathUtils, uSizeUtils, uURLUtils;

type
  TFilefactoryCom = class(TFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; override;
    function InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TFilefactoryCom }

function TFilefactoryCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TFilefactoryCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TFilefactoryCom.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TFilefactoryCom.GetName: WideString;
begin
  Result := 'Filefactory.com';
end;

function TFilefactoryCom.CheckLink(const AFile: WideString): TLinkInfo;
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

  HTTPManager.WaitFor(RequestID);

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
function TFilefactoryCom.CheckLinks(const AFiles: WideString): Integer;

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

      HTTPManager.WaitFor(RequestID);

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
