unit uNfodbRu;

interface

uses
  // Delphi
  SysUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TNfodbRu = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://nfodb.ru/';
  public
    function GetName: WideString; override; safecall;

    function InternalGetAvailableTypeIDs: TTypeIDs; override; safecall;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override; safecall;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override; safecall;
    function InternalGetDependentControlIDs: TControlIDs; override; safecall;

    function InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override; safecall;

    function GetResultsLimitDefaultValue: Integer; override; safecall;
  end;

implementation

function TNfodbRu.GetName;
begin
  Result := 'Nfodb.ru';
end;

function TNfodbRu.InternalGetAvailableTypeIDs;
begin
  Result := [cAudio];
end;

function TNfodbRu.InternalGetAvailableControlIDs;
begin
  Result := [cGenre, cNFO];
end;

function TNfodbRu.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TNfodbRu.InternalGetDependentControlIDs: TControlIDs;
begin
  Result := [cReleaseName];
end;

function TNfodbRu.InternalExecute;
var
  LReleasename: string;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LReleasename := AControlController.FindControl(cReleaseName).Value;

  // http://nfodb.ru/?do_search=Search&frelease=...&fyear=

  LResponeStr := GETRequest(WEBSITE + '?do_search=Search&frelease=' + HTTPEncode(LReleasename), LRequestID1);

  if not(Pos('releases</div>', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := 'href=''nfo-(\d+)-.*?index\.php\?genreid=\d+''>(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat

            if ACanUse(cGenre) then
              AControlController.FindControl(cGenre).AddProposedValue(GetName, Match[2]);

            LResponeStr := GETFollowUpRequest(WEBSITE + 'get_nfo-' + Match[1] + '.php', LRequestID1, LRequestID2);

            if ACanUse(cNFO) then
              AControlController.FindControl(cNFO).AddProposedValue(GetName, LResponeStr);

          until not ExecNext;
        end;
      finally
        Free;
      end;
  end;
end;

function TNfodbRu.GetResultsLimitDefaultValue;
begin
  Result := 1;
end;

end.
