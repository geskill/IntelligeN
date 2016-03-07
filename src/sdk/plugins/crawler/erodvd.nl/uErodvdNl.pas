unit uErodvdNl;

interface

uses
  // Delphi
  SysUtils, StrUtils, HTTPApp,
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
  TErodvdNl = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.erodvd.nl/';
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

function TErodvdNl.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TErodvdNl.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TErodvdNl.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TErodvdNl.GetName;
begin
  Result := 'erodvd.nl';
end;

function TErodvdNl.InternalGetAvailableTypeIDs;
begin
  Result := [cXXX];
end;

function TErodvdNl.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cDescription];
end;

function TErodvdNl.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TErodvdNl.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TErodvdNl.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<a href="javascript:change_preview_image\(''\/(.*?)''';

          if Exec(InputString) then
          begin
            AControlController.FindControl(cPicture).AddProposedValue(GetName, WEBSITE + Match[1]);
          end;
        finally
          Free;
        end;

    if ACanUse(cDescription) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '255\);">(.*?)<\/span>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2TextAndDecode(Match[1])));
            until not ExecNext;
          end;
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

  LResponeStr := GETRequest(WEBSITE + 'ssl/index.php?searchStr=' + HTTPEncode(LTitle) + '&act=viewCat&Submit=Go', LRequestID1);

  if not(Pos('class="tdEven"', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := 'center" class="(tdOdd|tdEven)"><a href="(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(WEBSITE + 'ssl/' + Match[2], LRequestID1, LRequestID2);

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

function TErodvdNl.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
