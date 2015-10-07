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
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    procedure Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase); override; safecall;
  end;

implementation

function T1advdCh.GetName;
begin
  result := '1advd.ch';
end;

function T1advdCh.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTypeID) .. high(TTypeID)];
  result := Word(_TemplateTypeIDs);
end;

function T1advdCh.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];

  result := LongWord(_ComponentIDs);
end;

function T1advdCh.GetControlIDDefaultValue;
begin
  result := True;
end;

function T1advdCh.GetResultsLimitDefaultValue;
begin
  result := 5;
end;

procedure T1advdCh.Exec;
const
  website = 'http://www.1advd.ch/';
var
  _ComponentIDs: TControlIDs;
  _Title, _search_alias: string;
  _Count: Integer;

  function GetFilmID(ALink: string): string;
  begin
    result := copy(ALink, Pos('=', ALink) + 1);
  end;

  function SpecialL(AText: string): string;
  var
    Offset: Integer;
  begin
    result := AText;

    Offset := Pos('I', AText);
    while not(Offset = 0) do
    begin
      if (Offset > 1) and (AText[Offset - 1] in ['a' .. 'z', 'ä', 'ö', 'ü']) then
        AText[Offset] := 'l';

      Offset := PosEx('I', AText, Offset + 1);
    end;

    result := AText;
    {
      with TRegExpr.Create do
      try
      result := ReplaceRegExpr('(?<=[\S])I', AText, 'l', True);
      finally
      Free;
      end;
      }
  end;

  procedure deep_search(aWebsitecode: string);
  var
    s: string;
  begin
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
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
    if (AControlController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
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
    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '[;|1]"><font face="Arial, Helvetica, sans-serif" size="2">(.*?)<\/font>';

          if Exec(InputString) then
          begin
            repeat

              AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(SpecialL(Match[1]))));
            until not ExecNext;
          end;
        finally
          Free;
        end;
  end;

var
  HTTPRequest: IHTTPRequest;

  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
  _Count := 0;

  case TTypeID(ATypeID) of
    cAudio:
      _search_alias := 'CD';
    cMovie:
      _search_alias := '';
    cGameCube, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cSoftware, cWii, cXbox, cXbox360:
      _search_alias := 'G';
    cXXX:
      _search_alias := 'A';
    cOther:
      _search_alias := 'B';
  end;

  HTTPRequest := THTTPRequest.Create(website + '/suchen' + _search_alias + '.asp?sb=' + HTTPEncode(_Title) + '&srt=2&hwAc=1');
  HTTPRequest.Referer := website;

  RequestID1 := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  if not(Pos('<title>Suchergebnis</title>', ResponseStrSearchResult) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := ResponseStrSearchResult;
        if TTypeID(ATypeID) = cMovie then
          Expression := '<td width=80% colspan="2"><font face="Arial, Helvetica, sans-serif" size="2"><b><a href="(.*?)"'
        else
          Expression := '<td rowspan=2 align="center" width=40><a href="(.*?)"';
        // InputString := Get(website + 'suchenKPL.asp?sb=' + HTTPEncode(_Title) + '&aps=50');
        // Expression := 'align=center valign=top><a href="(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            RequestID2 := HTTPManager.Get(website + Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

            repeat
              sleep(50);
            until HTTPManager.HasResult(RequestID2);

            deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

            {
              s1 := Match[1];
              s2 := copy(s1, 1, Pos('.', s1) - 1);
              case IndexText(s2, ['/DefaultCD', '/DefaultG', '/DefaultB']) of
              0:
              s2 := 'detailsCD/film';
              1:
              s2 := 'detailsG/film';
              2:
              s2 := 'detailsB/film';
              else
              s2 := 'details/film';
              end;

              s1 := '/' + s2 + GetFilmID(s1) + '.asp';

              deep_search(Get(website + s1));
              }
            Inc(_Count);
          until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end
  else
    deep_search(ResponseStrSearchResult);

end;

end.
