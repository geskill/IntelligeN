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
  uPlugInCrawlerClass, uPlugInHTTPClasses, uBigfishgamesCom;

type
  TBigfishgamesDe = class(TBigfishgamesCom)
  protected { . }
  const
    WEBSITE: string = 'http://www.bigfishgames.de/';

    function BigfishgamesGetWebsite: string; override;
  public
    function GetName: WideString; override; safecall;
  end;

implementation

{ TBigfishgamesDe }

function TBigfishgamesDe.BigfishgamesGetWebsite: string;
begin
  Result := WEBSITE;
end;

function TBigfishgamesDe.GetName;
begin
  Result := 'bigfishgames.de';
end;

end.
