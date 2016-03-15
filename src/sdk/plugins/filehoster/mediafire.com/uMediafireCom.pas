{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Mediafire.com Delphi API                            *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uMediafireCom;

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
  TMediafireCom = class(TFileHosterPlugIn)
  protected
    function InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TMediafireCom }

function TMediafireCom.InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool;
begin

end;

function TMediafireCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TMediafireCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TMediafireCom.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TMediafireCom.GetName: WideString;
begin
  Result := 'Mediafire.com';
end;

end.
