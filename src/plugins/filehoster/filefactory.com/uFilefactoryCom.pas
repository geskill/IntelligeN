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
  // plugin system
  uIdHTTPHelper, uPlugInFileHosterClass, uPlugInConst,
  // Utils
  uPathUtils, uSizeUtils;

type
  TFilefactoryCom = class(TFileHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function CheckLink(AFile: WideString): TLinkInfo; override; stdcall;
    function CheckLinks(AFiles: WideString): Integer; override; stdcall;
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
        Get(AFile, _postreply);

        if (Pos('File Not Found', _postreply.DataString) > 0) then
          LinkInfo.Status := lsOffline
        else
          with TRegExpr.Create do
            try
              ModifierS := True;

              InputString := _postreply.DataString;
              Expression := 'class="last">(.*?)<\/span>.*?<span>([\d\.]+) (\w+)';

              if Exec(InputString) then
              begin
                LinkInfo.Status := lsOnline;
                LinkInfo.Size := TSizeFormatter.SizeToByte(Match[2], Match[3]);
                LinkInfo.FileName := Match[1];
              end;
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
var
  I: Integer;
  _params, _postreply: TStringStream;
  _UnrecognizedLinks: string;

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

begin
  with TIdHTTPHelper.Create(Self) do
    try
      with TStringList.Create do
        try
          Text := AFiles;

          _params := TStringStream.Create('');
          _postreply := TStringStream.Create('');
          try
            _params.WriteString('func=links&links=');
            for I := 0 to Count - 1 do
            begin
              if (I > 0) then
                _params.WriteString(sLineBreak);
              _params.WriteString(Strings[I]);
            end;

            try
              Post('http://filefactory.com/tool/links.php', _params, _postreply);
            except

            end;

            with TRegExpr.Create do
              try
                ModifierS := False;
                InputString := _postreply.DataString;
                Expression := '>(.*?)<\/a>\s+<div class="metadata">(.*?)<\/div>\s+<\/td>\s+<td>([\d\.]+) (\w+)<\/td>';

                if Exec(InputString) then
                begin
                  repeat
                    AddLink(Match[1], ResonseLinkToGivenLink(Match[1], AFiles), lsOnline, TSizeFormatter.SizeToByte(Match[3], Match[4]));
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
                      AddLink(Match[1], ResonseLinkToGivenLink(Match[1], AFiles), lsOffline, 0);
                    until not ExecNext;
                  end;
                end;
              finally
                Free;
              end;
          finally
            _postreply.Free;
            _params.Free;
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
