unit uGamingUniverseDe;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // Utils
  uHTMLUtils, uStringUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TGamingUniverseDe = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.gaming-universe.org/';

    function GetGameSearchType(const ATypeID: TTypeID): string;
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

{ TGamingUniverseDe }

function TGamingUniverseDe.GetGameSearchType(const ATypeID: TTypeID): string;
begin
  Result := '';

  // cNintendoDS, cPlayStation3, cPlayStation4, cPlayStationVita, cWii, cWiiU, cXbox360, cXboxOne

  case ATypeID of
    cNintendoDS:
      begin
        Result := 'nintendo3ds';
      end;
    cPlayStation3:
      begin
        Result := 'playstation3';
      end;
    cPlayStation4:
      begin
        Result := 'playstation4';
      end;
    cPlayStationVita:
      begin
        Result := 'psvita';
      end;
    cWii:
      begin
        Result := 'wii';
      end;
    cWiiU:
      begin
        Result := 'wiiu';
      end;
    cXbox360:
      begin
        Result := 'xbox360';
      end;
    cXboxOne:
      begin
        Result := 'xboxone';
      end;
  end;
end;

function TGamingUniverseDe.GetName;
begin
  Result := 'gaming-universe.org';
end;

function TGamingUniverseDe.InternalGetAvailableTypeIDs;
begin
  Result := cConsole;
end;

function TGamingUniverseDe.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cTrailer, cGenre, cCreator, cPublisher, cDescription];
end;

function TGamingUniverseDe.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TGamingUniverseDe.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TGamingUniverseDe.InternalExecute;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
    LStringList: TStringList;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'newFenster\(.*?, ''(.*?)''';

          if Exec(InputString) then
            AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;

    if ACanUse(cTrailer) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'data-youtube-plid=''.*?'' src=''(.*?)''';

          if Exec(InputString) then
            AControlController.FindControl(cTrailer).AddProposedValue(GetName, HTMLDecode(Match[1]));
        finally
          Free;
        end;

    if ACanUse(cGenre) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Genre:<\/b><br>(.*?)<br>';

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

    if ACanUse(cCreator) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Entwickler:<\/b><br>(.*?)<br>';

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
          Expression := 'Publisher:<\/b><br>(.*?)<br>';

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

    if ACanUse(cDescription) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Review<\/div>.*?<\/b><br>(.*?)<br>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
        finally
          Free;
        end;
  end;

var
  LTitle: string;
  LCount: Integer;

  LWebsite: string;

  LHTTPRequest: IHTTPRequest;
  LHTTPParams: IHTTPParams;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  LWebsite := 'http://' + GetGameSearchType(ATypeID) + '.gaming-universe.org/';

  LHTTPRequest := THTTPRequest.Create(LWebsite + 'spiele/');
  with LHTTPRequest do
  begin
    Referer := LWebsite;
  end;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('game_keyw', LTitle);
    AddFormField('game_suche', '');
    AddFormField('game_suche', 'Abschicken');
  end;

  LRequestID1 := HTTPManager.Post(LHTTPRequest, LHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID1);

  LResponeStr := HTTPManager.GetResult(LRequestID1).HTTPResult.SourceCode;

  if not(Pos('<div><a href=''', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := '<div><a href=''\/(.*?)''>';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(LWebsite + Match[1], LRequestID1, LRequestID2);

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

function TGamingUniverseDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
