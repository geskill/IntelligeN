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
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TNfodbRu = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://nfodb.ru/';
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function InternalGetAvailableTypeIDs: TTypeIDs; override;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override;
    function InternalGetDependentControlIDs: TControlIDs; override;

    function InternalGetRetrieveData(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AAccountData: IAccountData; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override;

    function GetResultsLimitDefaultValue: Integer; override;
  end;

implementation

function TNfodbRu.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TNfodbRu.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TNfodbRu.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

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

function TNfodbRu.InternalGetRetrieveData;
var
  LReleasename: string;
  LCount: Integer;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LReleasename := AControlController.FindControl(cReleaseName).Value;
  LCount := 0;

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

            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end;

  Result := True;
end;

function TNfodbRu.GetResultsLimitDefaultValue;
begin
  Result := 1;
end;

end.
