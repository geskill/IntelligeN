unit uImdbCom;

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
  uPlugInCrawlerClass, uPlugInHTTPClasses,
  // Utils
  uPathUtils, uHTMLUtils, uStringUtils;

type
  TImdbCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.imdb.com/';
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

{ TImdbCom }

function TImdbCom.GetName;
begin
  Result := 'IMDb.com';
end;

function TImdbCom.InternalGetAvailableTypeIDs;
begin
  Result := [cMovie];
end;

function TImdbCom.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cCreator, cDirector, cPublisher, cGenre, cRuntime, cDescription];
end;

function TImdbCom.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TImdbCom.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TImdbCom.InternalExecute;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
    LStringList: TStringList;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="image">.*?src="(.*?)"';

          if Exec(InputString) then
          begin
            s := Match[1];

            with TRegExpr.Create do
            begin
              try
                InputString := s;
                Expression := '(.*)_V1.*_(.*)$';

                AControlController.FindControl(cPicture).AddProposedValue(GetName, Replace(InputString, '$1_V1_$2', True));
              finally
                Free;
              end;
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cDirector) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'itemprop="director".*?>(.*?)<\/div>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := 'name">(.*?)<\/';

                  if Exec(InputString) then
                  begin
                    repeat
                      LStringList.Add(Match[1]);
                    until not ExecNext;

                    AControlController.FindControl(cDirector).AddProposedValue(GetName, StringListSplit(LStringList, ';'));
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

    if ACanUse(cGenre) then
      with TRegExpr.Create do
        try
          InputString := ExtractTextBetween(AWebsiteSourceCode, 'Genres:', '</div>');
          Expression := '<a.*?>(.*?)<\/a>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cGenre).AddProposedValue(GetName, Trim(Match[1]));
            until not ExecNext;
          end;
        finally
          Free;
        end;

    if ACanUse(cRuntime) then
      with TRegExpr.Create do
      begin
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Runtime:.*?>(\d+) min';

          if Exec(InputString) then
            AControlController.FindControl(cRuntime).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;
      end;

    if ACanUse(cDescription) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Storyline<\/h2>.*?itemprop="description">(.*?)<\/p>';

          if Exec(InputString) then
          begin
            repeat
              s := Match[1];

              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '<em.*?<\/em>';

                  s := Replace(InputString, '', False);
                finally
                  Free;
                end;
              end;

              // <a href="/updates?update=tt5111572%3Aoutlines.add.1&ref_=tt_ov_cn_pl">Add a Plot</a>
              if (Pos('outlines.add', s) = 0) then
                AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(s)));
            until not ExecNext;
          end;
        finally
          Free;
        end;
  end;

  procedure deep_search_creator_publisher(AWebsiteSourceCode: string);
  var
    s: string;
    LStringList: TStringList;
  begin
    if ACanUse(cCreator) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'name="production".*?>(.*?)<\/ul>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '<a.*?>(.*?)<\/a>';

                  if Exec(InputString) then
                  begin
                    repeat
                      if not(LStringList.IndexOf(Match[1]) > 0) then
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
          Expression := 'name="distributors".*?>(.*?)<\/ul>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '<a.*?>(.*?)<\/a>';

                  if Exec(InputString) then
                  begin
                    repeat
                      if not(LStringList.IndexOf(Match[1]) > 0) then
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
  end;

var
  LTitle: string;
  LCount: Integer;

  LRequestID1, LRequestID2, LRequestID3: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  // Works better then
  // http://www.imdb.com/find?q=James%20Bond%20007%20-%20Spectre&s=tt
  // specific title search ^-^
  // http://www.imdb.com/search/title?title=James%20Bond%20007%20-%20Spectre

  LResponeStr := GETRequest(WEBSITE + 'find?q=' + HTTPEncode(LTitle) + '&s=tt', LRequestID1);

  if not(Pos('findSection', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := '"result_text"> <a href="\/(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(WEBSITE + Match[1], LRequestID1, LRequestID2);

            deep_search(LResponeStr);

            if ACanUse(cCreator) or ACanUse(cPublisher) then
            begin
              LResponeStr := GETFollowUpRequest(ExtractUrlPath(WEBSITE + Match[1]) + 'companycredits', LRequestID2, LRequestID3);

              deep_search_creator_publisher(LResponeStr);
            end;

            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end;

  Result := True;
end;

function TImdbCom.GetResultsLimitDefaultValue: Integer;
begin
  Result := 5;
end;

end.
