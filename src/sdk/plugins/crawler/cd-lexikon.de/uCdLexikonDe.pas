unit uCdLexikonDe;

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
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TCdLexikonDe = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://cd-lexikon.de/';
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function InternalGetAvailableTypeIDs: TTypeIDs; override;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override;
    function InternalGetDependentControlIDs: TControlIDs; override;

    function InternalGetRetrieveData(const AAccountData: IAccountData; const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override;

    function GetResultsLimitDefaultValue: Integer; override;
  end;

implementation

{ TCdLexikonDe }

function TCdLexikonDe.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TCdLexikonDe.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TCdLexikonDe.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TCdLexikonDe.GetName;
begin
  Result := 'cd-lexikon.de';
end;

function TCdLexikonDe.InternalGetAvailableTypeIDs;
begin
  Result := [cAudio];
end;

function TCdLexikonDe.InternalGetAvailableControlIDs;
begin
  Result := [cCreator, cPublisher, cPicture, cDescription];
end;

function TCdLexikonDe.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TCdLexikonDe.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TCdLexikonDe.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    LTracklist: string;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<tr><td valign=top><img src=''(.*?)''';

          if Exec(InputString) then
            AControlController.FindControl(cPicture).AddProposedValue(GetName, WEBSITE + Match[1]);
        finally
          Free;
        end;

    if ACanUse(cCreator) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Interpret: <td><td><font size=''2'' face=''Arial''>(.*?)<tr>';

          if Exec(InputString) then
            AControlController.FindControl(cCreator).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if ACanUse(cPublisher) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Label: <td><td><font size=''2'' face=''Arial''>(.*?)<tr>';

          if Exec(InputString) then
            AControlController.FindControl(cPublisher).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if ACanUse(cDescription) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<font size=''2'' face=''Arial''>(&nbsp;&nbsp;|)(\d+)\. .*?<a href=''.*?''>(.*?)<\/a>';

          if Exec(InputString) then
          begin
            LTracklist := '';
            repeat
              LTracklist := LTracklist + Match[2] + '. ' + Match[3] + sLineBreak;
            until not ExecNext;
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(LTracklist));
          end;
        finally
          Free;
        end;
    end;
  end;

var
  LTitle, LSearchQuery: string;
  LCount: Integer;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  LSearchQuery := TrimLeft(copy(LTitle, Pos('-', LTitle) + 1));

  LResponeStr := GETRequest(WEBSITE + 'suchen/albumsuche.php?r=0&q=' + HTTPEncode(LSearchQuery), LRequestID1);

  if not(Pos('<hr><br>', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := '<\/a><td><a href=''\.\.\/(.*?)'' title=';

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

  Result := True;
end;

function TCdLexikonDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
