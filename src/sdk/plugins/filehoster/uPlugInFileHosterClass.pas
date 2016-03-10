{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn file hoster class                            *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInFileHosterClass;

interface

uses
  // Delphi
  SysUtils, Classes, Variants,
  // Plugin
  uPlugInConst, uPlugInInterface, uPlugInClass, uPlugInFileHosterClasses;

type
  TFileHosterPlugIn = class(TPlugIn, IFileHosterPlugIn)
  protected
    function InternalCheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; virtual;
    function InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool; virtual;
  public
    function GetType: TPlugInType; override; safecall;

    function CheckLink(const AFile: WideString; out ALinkInfo: ILinkInfo): WordBool; safecall;
    function CheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool; safecall;
  end;

implementation

{ TFileHosterPlugIn }

function TFileHosterPlugIn.InternalCheckLink;
var
  LLinkInfo: ILinkInfo;
begin
  Result := False;

  LLinkInfo := TLinkInfo.Create;
  ALinkInfo := LLinkInfo;
end;

function TFileHosterPlugIn.InternalCheckLinks;
var
  LLinkIndex: Integer;
  LLinkInfo: ILinkInfo;
  LLinksInfo: TLinksInfo;
begin
  Result := True;

  LLinksInfo := TLinksInfo.Create;

  with TStringList.Create do
    try
      Text := AFiles;

      for LLinkIndex := 0 to Count - 1 do
      begin
        Result := Result and InternalCheckLink(Strings[LLinkIndex], LLinkInfo);
        LLinksInfo.AddLink(LLinkInfo);
      end;
    finally
      Free;
    end;

  ALinksInfo := LLinksInfo;
end;

function TFileHosterPlugIn.GetType;
begin
  Result := ptFileHoster;
end;

function TFileHosterPlugIn.CheckLink;
begin
  Result := InternalCheckLink(AFile, ALinkInfo);
end;

function TFileHosterPlugIn.CheckLinks;
begin
  Result := InternalCheckLinks(AFiles, ALinksInfo);
end;

end.
