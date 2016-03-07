unit uBinsearchInfo;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TBinsearchInfo = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://binsearch.info/';
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

function TBinsearchInfo.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TBinsearchInfo.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TBinsearchInfo.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TBinsearchInfo.GetName;
begin
  Result := 'Binsearch.info';
end;

function TBinsearchInfo.InternalGetAvailableTypeIDs;
begin
  Result := [low(TTypeID) .. high(TTypeID)];
end;

function TBinsearchInfo.InternalGetAvailableControlIDs;
begin
  Result := [cNFO];
end;

function TBinsearchInfo.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TBinsearchInfo.InternalGetDependentControlIDs;
begin
  Result := [cReleaseName];
end;

function TBinsearchInfo.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  begin
    with TRegExpr.Create do
      try
        InputString := AWebsiteSourceCode;
        Expression := '<pre>(.*?)<\/pre>';

        if Exec(InputString) then
        begin
          repeat
            AControlController.FindControl(cNFO).AddProposedValue(GetName, HTML2Text(Match[1]));
          until not ExecNext;
        end;
      finally
        Free;
      end;
  end;

var
  LReleasename: string;
  LCount: Integer;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LReleasename := AControlController.FindControl(cReleaseName).Value;
  LCount := 0;

  LResponeStr := GETRequest(WEBSITE + '/index.php?q=' + HTTPEncode(LReleasename) + '&max=250&adv_age=&server=', LRequestID1);

  if not(Pos('viewNFO', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := 'viewNFO\.php\?oid=(.*?)&amp;';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(WEBSITE + '/viewNFO.php?oid=' + Match[1], LRequestID1, LRequestID2);

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

function TBinsearchInfo.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
