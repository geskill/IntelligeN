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
  uPlugInCrawlerClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils, uStringUtils;

type
  TWiiboxartCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://wiiboxart.com/';
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

function TWiiboxartCom.InternalExecute;

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
          Expression := 'Developer:.*?210>(.*?)</TD>';

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
          Expression := 'Publisher:.*?210>(.*?)</TD>';

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

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID1);

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
end;

function TWiiboxartCom.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
