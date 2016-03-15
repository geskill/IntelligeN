{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  Letitbit.net Delphi API                             *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uLetitbitNet;

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
  TLetitbitNet = class(TFileHosterPlugIn)
  protected
    function InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TLetitbitNet }

function TLetitbitNet.InternalCheckLinks(const AFiles: WideString; out ALinksInfo: ILinksInfo): WordBool;
begin

end;

function TLetitbitNet.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TLetitbitNet.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TLetitbitNet.GetDescription;
begin
  Result := GetName + ' file hoster plug-in.';
end;

function TLetitbitNet.GetName: WideString;
begin
  Result := 'Letitbit.net';
end;

end.
