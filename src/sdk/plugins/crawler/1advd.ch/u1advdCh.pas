unit u1advdCh;

interface

uses
  // Delphi
  SysUtils, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  T1advdCh = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.1advd.ch/';

    function GetBaseSearchType(const ATypeID: TTypeID): string;
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

function T1advdCh.GetBaseSearchType(const ATypeID: TTypeID): string;
begin
  if (ATypeID in cGames) or (ATypeID = cSoftware) then
    Result := 'G'
  else
  begin
    case ATypeID of
      cAudio:
        Result := 'CD';
      cEBook:
        Result := 'B';
      cMovie:
        Result := '';
      cXXX:
        Result := 'A';
      cOther:
        Result := 'TOP';
    end;
  end;
end;

function T1advdCh.GetGameSearchType(const ATypeID: TTypeID): string;
begin
  case ATypeID of
    cNintendoDS:
      Result := '34';
    cPCGames:
      Result := '14';
    cPlayStation3:
      Result := '38';
    cPlayStation4:
      Result := '214';
    cPlayStationVita:
      Result := '48';
    cWii:
      Result := '37';
    cWiiU:
      Result := '212';
    cXbox360:
      Result := '36';
    cXboxOne:
      Result := '215';
  end;
end;

function T1advdCh.GetName;
begin
  Result := '1advd.ch';
end;

function T1advdCh.InternalGetAvailableTypeIDs;
begin
  Result := [low(TTypeID) .. high(TTypeID)];
end;

function T1advdCh.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cGenre, cDescription];
end;

function T1advdCh.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function T1advdCh.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function T1advdCh.InternalExecute;

  function SpecialL(AText: string): string;
  var
    Offset: Integer;
  begin
    Result := AText;

    Offset := Pos('I', AText);
    while not(Offset = 0) do
    begin
      if (Offset > 1) and (AText[Offset - 1] in ['a' .. 'z', 'ä', 'ö', 'ü']) then
        AText[Offset] := 'l';

      Offset := PosEx('I', AText, Offset + 1);
    end;

    Result := AText;
    {
      with TRegExpr.Create do
      try
      result := ReplaceRegExpr('(?<=[\S])I', AText, 'l', True);
      finally
      Free;
      end;
      }
  end;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<img src="(.*?)"';

          if Exec(InputString) then
          begin
            s := Match[1];

            if (Pos('noch%5Fkein%5Fbild', s) = 0) then
            begin
              if not(TTypeID(ATypeID) = cXXX) then
              begin
                s := ChangeFileExt(s, 'f' + ExtractFileExt(s));
                s := StringReplace(s, '/imgCD/', '/imgCDbc/', []);
                s := StringReplace(s, '/imggames/', '/imggamesbc/', []);
                s := StringReplace(s, '/img/', '/imgbc/', []);
                s := StringReplace(s, '/imgAx/', '/imgA/', []);
              end;
              AControlController.FindControl(cPicture).AddProposedValue(GetName, HTTPDecode(s));
            end;
          end;
        finally
          Free;
        end;

    if ACanUse(cGenre) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'asp\?g=[0-9]+">(.*?)<\/a>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cGenre).AddProposedValue(GetName, Match[1]);
            until not ExecNext;
          end;
        finally
          Free;
        end;

    if ACanUse(cDescription) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '[;|1]"><font face="Arial, Helvetica, sans-serif" size="2">(.*?)<\/font>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(HTMLDecode(SpecialL(Match[1])))));
            until not ExecNext;
          end;
        finally
          Free;
        end;
  end;

var
  LTitle: string;
  LCount: Integer;

  LSearchString: string;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  // http://www.1advd.ch/Suchen.asp?sb=Fantastic+Four
  // http://www.1advd.ch/SuchenG.asp?sb=Office&redef=17

  LSearchString := 'Suchen' + GetBaseSearchType(ATypeID) + '.asp?sb=' + HTTPEncode(LTitle);

  if (ATypeID in cGames) then
    LSearchString := LSearchString + '&redef=' + GetGameSearchType(ATypeID)
  else if (ATypeID = cSoftware) then
    LSearchString := LSearchString + '&redef=17';

  LResponeStr := GETRequest(WEBSITE + LSearchString + '&hwAc=1', LRequestID1);

  if not(Pos('<title>Suchergebnis</title>', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        if TTypeID(ATypeID) = cMovie then
          Expression := '<td width=80% colspan="2"><font face="Arial, Helvetica, sans-serif" size="2"><b><a href="\/(.*?)"'
        else
          Expression := '<td rowspan=2 align="center" width=40><a href="\/(.*?)"';

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
  end
  else if not(Pos('korb.asp?', LResponeStr) = 0) then
  begin
    deep_search(LResponeStr);
  end;

  Result := True;
end;

function T1advdCh.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
