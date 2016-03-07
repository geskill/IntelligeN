unit uWiiboxartCom;

interface

uses
  // Delphi
  SysUtils, Classes,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils, uStringUtils;

type
  TWiiboxartCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://wiiboxart.com/';
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

function TWiiboxartCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TWiiboxartCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TWiiboxartCom.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TWiiboxartCom.GetName;
begin
  Result := 'wiiboxart.com';
end;

function TWiiboxartCom.InternalGetAvailableTypeIDs;
begin
  Result := [cWii];
end;

function TWiiboxartCom.InternalGetAvailableControlIDs;
begin
  Result := [cCreator, cPublisher, cPicture, cGenre, cVideoSystem, cDescription];
end;

function TWiiboxartCom.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TWiiboxartCom.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TWiiboxartCom.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
    LStringList: TStringList;
  begin
    if ACanUse(cPicture) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Window\(''(.*?)''';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;

    if ACanUse(cCreator) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Developer:.*?210>(.*?)<\/TD>';

          if Exec(InputString) then
          begin
            s := Trim(Match[1]);

            if not SameStr('N/A', s) then
            begin
              AControlController.FindControl(cCreator).AddProposedValue(GetName, s);
            end;
          end;
        finally
          Free;
        end;
    end;

    if ACanUse(cPublisher) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Publisher:.*?210>(.*?)<\/TD>';

          if Exec(InputString) then
          begin
            s := Trim(Match[1]);

            if not SameStr('N/A', s) then
            begin
              AControlController.FindControl(cPublisher).AddProposedValue(GetName, s);
            end;
          end;
        finally
          Free;
        end;
    end;

    if ACanUse(cGenre) then
    begin
      with TRegExpr.Create do
        try
          ModifierM := True; // Causes ^ and $ to match the begin/end of each line
          InputString := AWebsiteSourceCode;
          Expression := 'Genre:.*?210>(.*?)$';

          if Exec(InputString) then
          begin
            s := Match[1];
            if (Pos('/', s) > 0) or (Pos(',', s) > 0) or (Pos('|', s) > 0) then
            begin
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '([^\/,|]+)';

                  if Exec(InputString) then
                  begin
                    repeat
                      AControlController.FindControl(cGenre).AddProposedValue(GetName, Trim(Match[1]));
                    until not ExecNext;
                  end;
                finally
                  Free;
                end;
              end;
            end
            else
            begin
              AControlController.FindControl(cGenre).AddProposedValue(GetName, Trim(s));
            end;
          end;
        finally
          Free;
        end;
    end;

    if ACanUse(cVideoSystem) then
    begin
      with TRegExpr.Create do
        try
          ModifierM := True; // Causes ^ and $ to match the begin/end of each line
          InputString := AWebsiteSourceCode;
          Expression := 'Region.*?>(.*?)$';

          if Exec(InputString) then
          begin
            AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, Trim(Match[1]));
          end;
        finally
          Free;
        end;
    end;

    if ACanUse(cDescription) then
    begin
      with TRegExpr.Create do
        try
          ModifierM := True; // Causes ^ and $ to match the begin/end of each line
          InputString := AWebsiteSourceCode;
          Expression := 'class=stdcopy width=600>(.*?)$';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
  end;

var
  LTitle: string;
  LCount: Integer;

  LHTTPRequest: IHTTPRequest;
  LHTTPParams: IHTTPParams;
  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  LHTTPRequest := THTTPRequest.Create(WEBSITE + 'search.php');
  with LHTTPRequest do
  begin
    Referer := WEBSITE;
  end;

  LHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with LHTTPParams do
  begin
    AddFormField('search', LTitle);
    AddFormField('ie', 'ISO-8859-1');
  end;

  LRequestID1 := HTTPManager.Post(LHTTPRequest, LHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID1);

  LResponeStr := HTTPManager.GetResult(LRequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := LResponeStr;
      Expression := '<td width=45 align=middle>\s+<a href="(.*?)"';

      if Exec(InputString) then
      begin
        repeat
          LResponeStr := GETFollowUpRequest(WEBSITE + Match[1], LRequestID1, LRequestID2);

          deep_search(LResponeStr);

          Inc(LCount);
        until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;

  Result := True;
end;

function TWiiboxartCom.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
