unit uPcgdbDe;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils, uStringUtils;

type
  TPcgdbDe = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.pcgdb.de/';
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

function TPcgdbDe.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TPcgdbDe.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TPcgdbDe.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TPcgdbDe.GetName;
begin
  Result := 'pcgdb.de';
end;

function TPcgdbDe.InternalGetAvailableTypeIDs;
begin
  Result := [cPCGames];
end;

function TPcgdbDe.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cGenre, cCreator, cPublisher, cDescription];
end;

function TPcgdbDe.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TPcgdbDe.InternalGetDependentControlIDs: TControlIDs;
begin
  Result := [cTitle];
end;

function TPcgdbDe.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
    LStringList: TStringList;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<a href="titles(.*?)"';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cPicture).AddProposedValue(GetName, WEBSITE + 'titles' + Match[1]);
            until not ExecNext;
          end;
        finally
          Free;
        end;

    if ACanUse(cGenre) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<td>Genre:<\/td>\W+<td width="233" bgcolor="#000000">(.*?)<\/td>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cGenre).AddProposedValue(GetName, Match[1]);
            until not ExecNext;
          end;
        finally
          Free;
        end;

    if ACanUse(cCreator) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Entwickler:.*?">(.*?)<\/td>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '">(.*?)<\/a>';

                  if Exec(InputString) then
                  begin
                    repeat
                      LStringList.Add(Match[1]);
                    until not ExecNext;

                    AControlController.FindControl(cCreator).AddProposedValue(GetName, StringListSplit(LStringList, ';'));
                  end;
                finally
                  Free;
                end;
              end;
            finally
              LStringList.Free;
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cPublisher) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Publisher:.*?">(.*?)<\/td>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '">(.*?)<\/a>';

                  if Exec(InputString) then
                  begin
                    repeat
                      LStringList.Add(Match[1]);
                    until not ExecNext;

                    AControlController.FindControl(cPublisher).AddProposedValue(GetName, StringListSplit(LStringList, ';'));
                  end;
                finally
                  Free;
                end;
              end;
            finally
              LStringList.Free;
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cDescription) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<tr><td bgcolor="#FFFFFF" class="content">.*?<br \/><br \/>(.*?)<br><br>';

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

  LHTTPRequest := THTTPRequest.Create(WEBSITE + 'games_search.php');
  with LHTTPRequest do
  begin
    Referer := WEBSITE;
  end;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('titel', LTitle);
    AddFormField('search', 'submit');
  end;

  LRequestID1 := HTTPManager.Post(LHTTPRequest, LHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID1);

  LResponeStr := HTTPManager.GetResult(LRequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := LResponeStr;
      Expression := '<td class="newstext"><a href="(.*?)"';

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

function TPcgdbDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
