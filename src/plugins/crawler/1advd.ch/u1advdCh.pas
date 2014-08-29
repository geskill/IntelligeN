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
  uConst, uAppInterface,
  // Plugin system
  uPlugInCrawlerClass, uIdHTTPHelper;

type
  T1advdCh = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override; stdcall;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override; stdcall;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean; override; stdcall;
    function GetLimitDefaultValue: Integer; override; stdcall;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override; stdcall;
  end;

implementation

function T1advdCh.GetName;
begin
  result := '1advd.ch';
end;

function T1advdCh.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)];
  result := Word(_TemplateTypeIDs);
end;

function T1advdCh.GetAvailableComponentIDs;
var
  // _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  // _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

  _ComponentIDs := [cPicture, cGenre, cDescription];

  result := LongWord(_ComponentIDs);
end;

function T1advdCh.GetComponentIDDefaultValue;
begin
  result := True;
end;

function T1advdCh.GetLimitDefaultValue;
begin
  result := 5;
end;

procedure T1advdCh.Exec;
const
  website = 'http://www.1advd.ch/';
var
  _ComponentIDs: TComponentIDs;
  _Title, s1, s2: string;
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
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<img src="(.*?)"';

          if Exec(InputString) then
          begin
            s := Match[1];

            if (Pos('noch%5Fkein%5Fbild', s) = 0) then
            begin
              if not(TTemplateTypeID(ATemplateTypeID) = cXXX) then
              begin
                s := ChangeFileExt(s, 'f' + ExtractFileExt(s));
                s := StringReplace(s, '/imgCD/', '/imgCDbc/', []);
                s := StringReplace(s, '/imggames/', '/imggamesbc/', []);
                s := StringReplace(s, '/img/', '/imgbc/', []);
                s := StringReplace(s, '/imgAx/', '/imgA/', []);
              end;
              AComponentController.FindControl(cPicture).AddValue(HTTPDecode(s), GetName);
            end;
          end;
        finally
          Free;
        end;
    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'asp\?g=[0-9]+">(.*?)<\/a>';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cGenre).AddValue(Match[1], GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '[;|1]"><font face="Arial, Helvetica, sans-serif" size="2">(.*?)<\/font>';

          if Exec(InputString) then
          begin
            repeat

              AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(SpecialL(Match[1]))), GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
  end;

begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  with TIdHTTPHelper.Create(Self) do
    try
      with TRegExpr.Create do
      begin
        try
          case TTemplateTypeID(ATemplateTypeID) of
            cAudio:
              s1 := 'CD';
            cMovie:
              s1 := '';
            cGameCube, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cSoftware, cWii, cXbox, cXbox360:
              s1 := 'G';
            cXXX:
              s1 := 'A';
            cOther:
              s1 := 'B';
          end;

          s1 := '/suchen' + s1 + '.asp?sb=' + HTTPEncode(_Title) + '&srt=2';

          s2 := Get(website + s1);
          if (Pos('<title>Suchergebnis</title>', s2) = 0) then
            deep_search(s2);
          InputString := s2;
          if TTemplateTypeID(ATemplateTypeID) = cMovie then
            Expression := '<td width=80% colspan="2"><font face="Arial, Helvetica, sans-serif" size="2"><b><a href="(.*?)"'
          else
            Expression := '<td rowspan=2 align="center" width=40><a href="(.*?)"';
          // InputString := Get(website + 'suchenKPL.asp?sb=' + HTTPEncode(_Title) + '&aps=50');
          // Expression := 'align=center valign=top><a href="(.*?)"';

          if Exec(InputString) then
          begin
            repeat
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
                } deep_search(Get(website + Match[1]));
              Inc(_Count);
            until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
          end;
        finally
          Free;
        end;
      end;
    finally
      Free;
    end;
end;

end.
