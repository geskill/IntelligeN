unit uZelluloidDe;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
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
  TZelluloidDe = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.zelluloid.de/';
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

function TZelluloidDe.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TZelluloidDe.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TZelluloidDe.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TZelluloidDe.GetName;
begin
  Result := 'Zelluloid.de';
end;

function TZelluloidDe.InternalGetAvailableTypeIDs;
begin
  Result := [cMovie];
end;

function TZelluloidDe.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cDirector, cGenre, cDescription];
end;

function TZelluloidDe.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TZelluloidDe.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TZelluloidDe.InternalGetRetrieveData;

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
          Expression := '<a href="#" pic="\/(.*?)"';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cPicture).AddProposedValue(GetName, WEBSITE + Match[1]);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;

    if ACanUse(cDirector) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Regie:(.*?)<BR>';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := 'NOBR>(.*?)<\/NOBR>';

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
    end;

    if ACanUse(cGenre) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'az\.php3\?g=(\d*?)">(.*?)<';

          if Exec(InputString) then
          begin
            repeat
              s := Match[2];
              if Pos(',', s) > 0 then
                s := copy(s, 1, Pos(',', s) - 1);
              AControlController.FindControl(cGenre).AddProposedValue(GetName, s);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;

    if ACanUse(cDescription) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="bigtext">(.*?)<\/div>';

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

const
  SEARCH_URL = WEBSITE + 'suche/';
var
  LTitle: string;
  LCount: Integer;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  // http://www.zelluloid.de/suche/index.php3?qstring=Spectre&x=38&y=4

  LResponeStr := GETRequest(SEARCH_URL + 'index.php3?qstring=' + HTTPEncode(LTitle), LRequestID1);

  if not(Pos('Treffer', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := '<B><a href="(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(SEARCH_URL + Match[1], LRequestID1, LRequestID2);

            deep_search(LResponeStr);

            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end
  else if not(Pos('headlineleft', LResponeStr) = 0) then
  begin
    deep_search(LResponeStr);
  end;

  Result := True;
end;

function TZelluloidDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
