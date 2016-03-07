unit uIgnCom;

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
  uPathUtils, uHTMLUtils, uStringUtils;

type
  TIgnCom = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.ign.com/';

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

{ TIgnCom }

function TIgnCom.IsSystem(const ATypeID: TTypeID; const ASystem: string): Boolean;
begin
  Result := False;

  // cNintendoDS, cNintendo3DS, cPCGames, cPlayStation3, cPlayStation4, cPlayStationVita, cWii, cWiiU, cXbox360, cXboxOne

  case ATypeID of
    cNintendoDS:
      begin
        Result := (ASystem = 'NDS');
      end;
    cNintendo3DS:
      begin
        Result := (ASystem = '3DS');
      end;
    cPCGames:
      begin
        Result := (ASystem = 'PC');
      end;
    cPlayStation3:
      begin
        Result := (ASystem = 'PS3');
      end;
    cPlayStation4:
      begin
        Result := (ASystem = 'PS4');
      end;
    cPlayStationVita:
      begin
        Result := (ASystem = 'Vita');
      end;
    cWii:
      begin
        Result := (ASystem = 'Wii');
      end;
    cWiiU:
      begin
        Result := (ASystem = 'Wii U');
      end;
    cXbox360:
      begin
        Result := (ASystem = 'Xbox 360');
      end;
    cXboxOne:
      begin
        Result := (ASystem = 'Xbox One');
      end;
  end;
end;

function TIgnCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TIgnCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TIgnCom.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TIgnCom.GetName;
begin
  Result := 'ign.com';
end;

function TIgnCom.InternalGetAvailableTypeIDs;
begin
  Result := cGames;
end;

function TIgnCom.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cGenre, cCreator, cPublisher, cDescription];
end;

function TIgnCom.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TIgnCom.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TIgnCom.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
    LStringList: TStringList;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<img class="highlight-boxArt" src="(.*?)"';

          if Exec(InputString) then
          begin
            if Pos('ignimgs', string(Match[1])) > 0 then
              AControlController.FindControl(cPicture).AddProposedValue(GetName, StringReplace(Match[1], '_160h', '_640w', []))
            else
              AControlController.FindControl(cPicture).AddProposedValue(GetName, StringReplace(Match[1], '_160h', '_160w', []));
          end;
        finally
          Free;
        end;

    if ACanUse(cGenre) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Genre<\/strong>:(.*?)<\/div>';

          if Exec(InputString) then
          begin
            s := Match[1];

            with TRegExpr.Create do
            begin
              try
                InputString := s;
                // proper: ([\w-]+)|<a .*?">(.*?)</a>
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
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cCreator) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Developer(s)?<\/strong>:(.*?)<\/div>';

          if Exec(InputString) then
          begin
            s := Match[2];

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
          Expression := 'Publisher(s)?<\/strong>:(.*?)<\/div>';

          if Exec(InputString) then
          begin
            s := Match[2];

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

    if ACanUse(cDescription) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          if Pos('ratingImage', AWebsiteSourceCode) > 0 then
            Expression := '<div class="gameInfo">.*?a>(.*?)<\/p'
          else
            Expression := '<div class="gameInfo">.*?p>(.*?)<\/p';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
        finally
          Free;
        end;
  end;

var
  LTitle, s: string;
  LCount: Integer;

  LRequestID1, LRequestID2, LRequestID3: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  // http://www.ign.com/search?q=Halo&page=0&count=10&type=object&objectType=game&filter=games&

  LResponeStr := GETRequest(WEBSITE + 'search?q=' + HTTPEncode(LTitle) + '&page=0&count=10&type=object&objectType=game&filter=games', LRequestID1);

  if not(Pos('search-item', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := 'data-type="game".*?item-title">\s+<a href=".*?".*?<a href=".*?">.*?<\/a>(.*?)<\/div>';

        if Exec(InputString) then
        begin
          repeat
            s := Match[1];

            with TRegExpr.Create do
            begin
              try
                InputString := s;
                Expression := '<a href="(.*?)">(.*?)<\/a>';

                if Exec(InputString) then
                begin
                  repeat

                    if IsSystem(ATypeID, Match[2]) then
                    begin
                      LResponeStr := GETFollowUpRequest(Match[1], LRequestID1, LRequestID2);

                      deep_search(LResponeStr);
                    end;

                  until not ExecNext;
                end;
              finally
                Free;
              end;
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

function TIgnCom.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
