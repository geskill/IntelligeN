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
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses, uBigfishgamesCom;

type
  TBigfishgamesDe = class(TBigfishgamesCom)
  protected
    function BigfishgamesURL: string; override;
  public
    function GetName: WideString; override;
  end;

implementation

{ TBigfishgamesDe }

function TBigfishgamesDe.BigfishgamesURL: string;
const
  website = 'http://www.bigfishgames.de/';
begin
  Result := website + 'download-spiele/';
end;

function TBigfishgamesDe.GetName;
begin
  Result := 'bigfishgames.de';
end;

end.
