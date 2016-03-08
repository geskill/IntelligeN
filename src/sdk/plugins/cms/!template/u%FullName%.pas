unit u%FullName%;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCMSClass, uPlugInHTTPClasses,
  // Utils,
  uStringUtils;

type
  { TODO : use blog/forum/formbased settings class }
  T%FullName%Settings = class(TCMSPlugInSettings)
  strict private

  public
    constructor Create; override;
    destructor Destroy; override;
  published

  end;

  { TODO : use blog/forum/formbased class }
  T%FullName% = class(TCMSPlugIn)
  private
    %FullName%Settings: T%FullName%Settings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(const AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;

    function DoAnalyzeIDsRequest(const AResponseStr: string): Integer; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(const AWebsiteSourceCode: WideString): WordBool; override; safecall;
    function GetIDs: Integer; override; safecall;
    function GetArticleLink(const AURL: WideString; const AArticleID: Integer): WideString; override; safecall;
  end;

implementation

{ T%FullName%Settings }

constructor T%FullName%Settings.Create;
begin
  inherited Create;

  // default setup
  { TODO : define here custom settings default values }
end;

destructor T%FullName%Settings.Destroy;
begin

  inherited Destroy;
end;

{ T%FullName% }

function T%FullName%.SettingsClass;
begin
  Result := T%FullName%Settings;
end;

function T%FullName%.GetSettings;
begin
  Result := %FullName%Settings;
end;

procedure T%FullName%.SetSettings;
begin
  %FullName%Settings := ACMSPlugInSettings as T%FullName%Settings;
end;

function T%FullName%.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with %FullName%Settings do
  begin
    { TODO : your code here }
  end;
end;

function T%FullName%.DoBuildLoginRequest;
begin
  Result := True;

  { TODO : check url here, it may needs a sub page }
  AHTTPRequest := THTTPRequest.Create(Website + '');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := %FullName%Settings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    { TODO : add params, define param format }
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function T%FullName%.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  { TODO : check if login was successful }
  Result := False;
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        { TODO : analyse the response and build a expression }
        Expression := '';

        if Exec(InputString) then
          { TODO : check whether match on position 1 is correct and ReduceWhitespace() is needed }
          Self.ErrorMsg := ReduceWhitespace(Trim(Match[1]));
      finally
        Free;
      end;
end;

function T%FullName%.DoBuildPostRequest;
begin
  Result := True;

  { TODO : check url here, it may needs a sub page }
  AHTTPRequest := THTTPRequest.Create(Website + '');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := %FullName%Settings.CharSet;
  end;

  { TODO : add params, define param format }
  AHTTPParams := THTTPParams.Create();

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function T%FullName%.DoAnalyzePost;
begin
  Result := True;

  with TRegExpr.Create do
    try
      InputString := AResponseStr;

      { TODO : analyse the response and build a expression }
      Expression := '';

      if Exec(InputString) then
      begin
        { TODO : check whether match on position 1 is correct and ReduceWhitespace() is needed }
        Self.ErrorMsg := ReduceWhitespace(Trim(Match[1]));
        Result := False;
      end;
    finally
      Free;
    end;
end;

function T%FullName%.DoAnalyzeIDsRequest;
begin
  { TODO : crawl after IDs }

  Result := FCheckedIDsList.Count;
end;

function T%FullName%.GetName;
begin
  Result := '%FullName%';
end;

function T%FullName%.DefaultCharset;
begin
  { TODO : check if this default charset is correct }
  Result := 'UTF-8';
end;

{$REGION 'Documentation'}
    /// <param name="AWebsiteSourceCode">
    /// contains the sourcode from any website of the webpage
    /// </param>
    /// <returns>
    /// The method returns True if the sourcode of a website
    /// matches to this CMS and False otherwise.
    /// </returns>
{$ENDREGION}
function T%FullName%.BelongsTo;
begin
  Result := False;
  { TODO : evaluate the AWebsiteSourceCode for matching webpage }
end;

function T%FullName%.GetIDs;
begin
  Result := 0;
  { TODO : implement }
end;

function T%FullName%.GetArticleLink;
begin
  Result := Format('%s?id=%d', [AURL, AArticleID]);
  { TODO : return the permalink for the specified article ID }
end;

end.
