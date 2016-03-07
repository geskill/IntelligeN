unit uNintendolifeCom;

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
  TNintendolifeCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.nintendolife.com/';

    function IsSystem(const ATypeID: TTypeID; const ASystem: string): Boolean;
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

{ TNintendolifeCom }

function TNintendolifeCom.IsSystem(const ATypeID: TTypeID; const ASystem: string): Boolean;
begin
  Result := False;

  case ATypeID of
    cNintendoDS:
      begin
        Result := (ASystem = 'DS') or (ASystem = 'DSiWare');
      end;
    cNintendo3DS:
      begin
        Result := (Pos('3DS', ASystem) > 0) or (ASystem = 'DSiWare');
      end;
    cWii:
      begin
        Result := (ASystem = 'Wii') or (ASystem = 'WiiWare');
      end;
    cWiiU:
      begin
        Result := (ASystem = 'Nintendo NX') or (Pos('Wii U', ASystem) > 0);
      end;
    cOther:
      begin
        Result := True;
      end;
  end;
end;

function TNintendolifeCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TNintendolifeCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TNintendolifeCom.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TNintendolifeCom.GetName;
begin
  Result := 'nintendolife.com';
end;

function TNintendolifeCom.InternalGetAvailableTypeIDs;
begin
  Result := [cNintendoDS, cNintendo3DS, cWii, cWiiU, cOther];
end;

function TNintendolifeCom.InternalGetAvailableControlIDs;
begin
  Result := [cTags, cPicture, cGenre, cCreator, cPublisher, cDescription];
end;

function TNintendolifeCom.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TNintendolifeCom.InternalGetDependentControlIDs: TControlIDs;
begin
  Result := [cTitle];
end;

function TNintendolifeCom.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
    LStringList: TStringList;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="cover"><a href="(.*?)"';

          if Exec(InputString) then
            AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if ACanUse(cPublisher) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Publisher.*?">(.*?)<\/div>';

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

    if ACanUse(cCreator) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Developer.*?">(.*?)<\/div>';

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

    if ACanUse(cGenre) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Genre.*?definition">(.*?)<\/';

          if Exec(InputString) then
          begin
            s := Match[1];

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
          end;
        finally
          Free;
        end;

    if ACanUse(cTags) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Tags.*?definition">(.*?)<\/';

          if Exec(InputString) then
          begin
            s := Match[1];

            LStringList := TStringList.Create;
            try
              with TRegExpr.Create do
              begin
                try
                  InputString := s;
                  Expression := '([^\/,|]+)';

                  if Exec(InputString) then
                  begin
                    repeat
                      LStringList.Add(Trim(Match[1]));
                    until not ExecNext;

                    AControlController.FindControl(cTags).AddProposedValue(GetName, StringListSplit(LStringList, ';'));
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
          Expression := '<article class="article">(.*?)<\/article>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
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

  LResponeStr := GETRequest(WEBSITE + 'games?title=' + HTTPEncode(LTitle), LRequestID1);

  if not(Pos('Showing items', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := ExtractTextBetween(LResponeStr, '<div class="table">', '</table>');
        Expression := '<td class="title"><a title=".*?" href="(.*?)".*?<td class="system"><abbr title="(.*?)"';

        if Exec(InputString) then
        begin
          repeat

            if IsSystem(ATypeID, Match[2]) then
            begin
              LResponeStr := GETFollowUpRequest(WEBSITE + Match[1], LRequestID1, LRequestID2);

              deep_search(LResponeStr);

              Inc(LCount);
            end;

          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end;

  Result := True;
end;

function TNintendolifeCom.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
