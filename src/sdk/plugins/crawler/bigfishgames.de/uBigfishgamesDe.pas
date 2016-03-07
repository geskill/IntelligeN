unit uBigfishgamesDe;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses, uBigfishgamesCom;

type
  TBigfishgamesDe = class(TBigfishgamesCom)
  protected { . }
  const
    WEBSITE: string = 'http://www.bigfishgames.de/';

    function BigfishgamesGetWebsite: string; override;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;
  end;

implementation

{ TBigfishgamesDe }

function TBigfishgamesDe.BigfishgamesGetWebsite: string;
begin
  Result := WEBSITE;
end;

function TBigfishgamesDe.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TBigfishgamesDe.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TBigfishgamesDe.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TBigfishgamesDe.GetName;
begin
  Result := 'bigfishgames.de';
end;

end.
