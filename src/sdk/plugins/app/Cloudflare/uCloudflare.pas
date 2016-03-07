unit uCloudflare;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants,
  // MultiEvent
  Generics.MultiEvents.NotifyHandler,
  // HTTPManager
  uHTTPInterface, uHTTPConst, uHTTPManager, uHTTPCloudflareAntiScrape,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Plugin system
  uPlugInAppClass, uPlugInHTTPClasses,
  // Utils,
  uPathUtils, uStringUtils;

type
  TCloudflare = class(TAppPlugIn)
  private
    FAppController: IAppController;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function Start(const AAppController: IAppController): WordBool; override;
    function Stop: WordBool; override;
  end;

implementation

{ TCloudflare }

function TCloudflare.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TCloudflare.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TCloudflare.GetDescription;
begin
  Result := GetName + ' app plug-in.';
end;

function TCloudflare.GetName: WideString;
begin
  Result := 'Cloudflare';
end;

function TCloudflare.Start(const AAppController: IAppController): WordBool;
begin
  Result := True;

  if Result then
  begin
    FAppController := AAppController;
  end;
end;

function TCloudflare.Stop: WordBool;
var
  LHTTPExtension: IHTTPExtension;
begin
  LHTTPExtension := THTTPManager.Instance().AntiScrapeManager.FindExtension(GetName);
  Result := Assigned(LHTTPExtension) and not LHTTPExtension.InUse;

  if Result then
  begin
    FAppController := nil;
  end;
end;

end.
