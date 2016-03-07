unit uBigfishgamesCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, Math, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TBigfishgamesCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.bigfishgames.com/';

    function BigfishgamesGetWebsite: string; virtual;
    function BigfishgamesSearchRequest(AWebsite, ATitle: string; out AFollowUpRequest: Double): string;
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

{ TBigfishgamesCom }

function TBigfishgamesCom.BigfishgamesGetWebsite: string;
begin
  Result := WEBSITE;
end;

function TBigfishgamesCom.BigfishgamesSearchRequest(AWebsite, ATitle: string; out AFollowUpRequest: Double): string;
var
  LHTTPRequest: IHTTPRequest;
  LHTTPParams: IHTTPParams;
begin

  LHTTPRequest := THTTPRequest.Create(AWebsite + 'ajax.php');
  with LHTTPRequest do
  begin
    Referer := AWebsite;
    CustomHeaders.Add('X-Requested-With: XMLHttpRequest');
  end;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('search', 'true');
    AddFormField('response_type', 'results_page');
    AddFormField('type_sname', 'pc');
    AddFormField('rand', IntToStr(Floor(Random(100000000))));
    AddFormField('q', ATitle);
  end;

  AFollowUpRequest := HTTPManager.Post(LHTTPRequest, LHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(AFollowUpRequest);

  Result := HTTPManager.GetResult(AFollowUpRequest).HTTPResult.SourceCode;
end;

function TBigfishgamesCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TBigfishgamesCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TBigfishgamesCom.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TBigfishgamesCom.GetName;
begin
  Result := 'bigfishgames.com';
end;

function TBigfishgamesCom.InternalGetAvailableTypeIDs;
begin
  Result := [cPCGames];
end;

function TBigfishgamesCom.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cTrailer, cDescription];
end;

function TBigfishgamesCom.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TBigfishgamesCom.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TBigfishgamesCom.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="bfg-col-xs-12">\s+<img src="(.*?)"';

          if Exec(InputString) then
            AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if ACanUse(cTrailer) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="flash-frame">\s+<a href="(.*?)"';

          if Exec(InputString) then
            AControlController.FindControl(cTrailer).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if ACanUse(cDescription) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="long">(.*?)<\/div>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
        finally
          Free;
        end;
  end;

var
  LTitle: string;
  LCount: Integer;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  LResponeStr := BigfishgamesSearchRequest(BigfishgamesGetWebsite, LTitle, LRequestID1);

  if not(Pos('"code":"first_results"', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := 'download-games\\\/(\d+)\\';

        if Exec(InputString) then
        begin
          repeat

            LResponeStr := GETFollowUpRequest(BigfishgamesGetWebsite + 'download-games/' + Match[1], LRequestID1, LRequestID2);

            deep_search(LResponeStr);

            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end;

  Result := True;
end;

function TBigfishgamesCom.GetResultsLimitDefaultValue;
begin
  Result := 1;
end;

end.
