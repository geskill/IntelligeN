unit uJadedvideoCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPConst, uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils, uStringUtils;

type
  TJadedvideoCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.jadedvideo.com/';
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

function TJadedvideoCom.GetName;
begin
  Result := 'Jadedvideo.com';
end;

function TJadedvideoCom.InternalGetAvailableTypeIDs;
begin
  Result := [cXXX];
end;

function TJadedvideoCom.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cCreator, cPublisher, cGenre, cDescription];
end;

function TJadedvideoCom.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TJadedvideoCom.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TJadedvideoCom.InternalExecute;

  procedure deep_search(AWebsiteSourceCode: string);

    procedure ExtractGenres(AGenreCode: string);
    begin
      with TRegExpr.Create do
        try
          InputString := AGenreCode;
          Expression := '">(.*?)<\/A>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cGenre).AddProposedValue(GetName, Match[1]);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;

  var
    s: string;
    LStringList: TStringList;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<SPAN class="eight_pt">.*?<IMG src="(.*?)"';

          if Exec(InputString) then
            AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if ACanUse(cCreator) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Studio:(.*?)<\/TR>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := 'class="little normal">(.*?)<\/';

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
    end;

    if ACanUse(cDirector) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Director:(.*?)<\/TR>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := 'class="little normal">(.*?)<\/';

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
          InputString := AWebsiteSourceCode;
          Expression := 'Categories:(.*?)<\/TR>';

          if Exec(InputString) then
          begin
            s := Match[1];

            with TRegExpr.Create do
            begin
              try
                InputString := s;
                Expression := 'class="little normal">(.*?)<\/';

                if Exec(InputString) then
                begin
                  repeat
                    AControlController.FindControl(cDirector).AddProposedValue(GetName, Match[1]);
                  until not ExecNext;
                end;
              finally
                Free;
              end;
            end;

          end;
        finally
          Free;
        end;

    if ACanUse(cDescription) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Product Description.*?<BR\/>(.*?)<\/TR>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
        finally
          Free;
        end;
  end;

var
  LTitle: string;
  LCount: Integer;

  LHTTPRequest: IHTTPRequest;
  LHTTPParams: IHTTPParams;
  LRequestID1, LRequestID2, LRequestID3: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  LResponeStr := GETRequest(WEBSITE + 'vap/Adult_Content_Accept.html', LRequestID1);

  // TODO: Need search fix

  LHTTPRequest := THTTPRequest.Create(WEBSITE + 'info/Advanced_Search.html');
  with LHTTPRequest do
  begin
    Referer := WEBSITE;
  end;

  LHTTPParams := THTTPParams.Create(ptList);
  with LHTTPParams do
  begin
    AddFormField('productNo', '');
    AddFormField('legacyProductId', '');
    AddFormField('title', LTitle);
    AddFormField('titleMatchTypeCd', '1');
    AddFormField('castNames', '');
    AddFormField('castMatchTypeCd', '1');
    AddFormField('directorName', '');
  end;

  LRequestID2 := HTTPManager.Post(LHTTPRequest.URL, LRequestID1, LHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID2);

  LResponeStr := HTTPManager.GetResult(LRequestID2).HTTPResult.SourceCode;

  if not(Pos('SEARCH RESULTS', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try

        InputString := LResponeStr;
        Expression := 'align="center">\s+<A href="\/(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(WEBSITE + Match[1], LRequestID2, LRequestID3);

            deep_search(LResponeStr);

            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end
  else if not(Pos('product_details', LResponeStr) = 0) then
  begin
    deep_search(LResponeStr);
  end;

  Result := True;
end;

function TJadedvideoCom.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
