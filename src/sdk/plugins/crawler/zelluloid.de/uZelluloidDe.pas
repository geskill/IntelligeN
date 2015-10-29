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
  uPlugInCrawlerClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils, uStringUtils;

type
  TZelluloidDe = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.zelluloid.de/';
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

function TZelluloidDe.InternalExecute;

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
end;

function TZelluloidDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
