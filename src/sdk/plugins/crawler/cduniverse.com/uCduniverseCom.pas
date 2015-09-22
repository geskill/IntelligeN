unit uCduniverseCom;

interface

uses
  // Delphi
  SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TCduniverseCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;
implementation

{ TCduniverseCom }

function TCduniverseCom.GetName;
begin
  Result := 'cduniverse.com';
end;

function TCduniverseCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)] - [cOther];
  Result := Word(_TemplateTypeIDs);
end;

function TCduniverseCom.GetAvailableComponentIDs;
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

  _ComponentIDs := [cPicture, cGenre, cDescription];

  if (_TemplateTypeID in [cAudio, cMovie]) then
    _ComponentIDs := _ComponentIDs + [cRuntime];

  Result := LongWord(_ComponentIDs);
end;

function TCduniverseCom.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TCduniverseCom.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TCduniverseCom.Exec;
const
  website = 'http://www.cduniverse.com/';
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
  _Count: Integer;
  _style, _Title, _tracklist: string;

  RequestID1, RequestID2, RequestID3: Double;

  procedure deep_image_search(aWebsitecode: string);
  begin
    with TRegExpr.Create do
      try
        InputString := aWebsitecode;
        Expression := '<center><p><img src="(.*?)"';

        if Exec(InputString) then
          AComponentController.FindControl(cPicture).AddValue(Match[1], GetName);
      finally
        Free;
      end;
  end;

  procedure deep_search(aWebsitecode: string);
  begin
    if (AComponentController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<nobr>(Recording|Running) Time <\/nobr><\/td><td>(\d+)';

          if Exec(InputString) then
          begin
            AComponentController.FindControl(cRuntime).AddValue(Match[2], GetName)
          end;
        finally
          Free;
        end;
    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '"categorylink" href="(.*?)">(.*?)<\/a>';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cGenre).AddValue(Match[2], GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'class="mp3gridfont">(.*?)\s?<';

          if Exec(InputString) then
          begin
            _tracklist := '';
            repeat
              _tracklist := _tracklist + Trim(Match[1]) + sLineBreak;
            until not ExecNext;
            AComponentController.FindControl(cDescription).AddValue(copy(_tracklist, 1, length(_tracklist) - 2), GetName);
          end;
        finally
          Free;
        end;
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<div style="margin-top:10px;"><!---- trimable --->(.*?)&nbsp;&nbsp;';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(Match[1])), GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<table id="igcovera1" cellPadding="0" cellSpacing="0" border="0">\s+<tr>\s+<td><a  href="\/(.*?)"';

          if Exec(InputString) then
          begin

            RequestID3 := HTTPManager.Get(website + Match[1], RequestID2, TPlugInHTTPOptions.Create(Self));

            repeat
              sleep(50);
            until HTTPManager.HasResult(RequestID3);

            deep_image_search(HTTPManager.GetResult(RequestID3).HTTPResult.SourceCode);

          end
          else
          begin
            Expression := '<table id="igcovera1" cellPadding="0" cellSpacing="0" border="0">\s+<tr>\s+<td><img src="(.*?)"';

            if Exec(InputString) and not SameText(Match[1], '/images/default_coverart.gif') then
              AComponentController.FindControl(cPicture).AddValue(Match[1], GetName);
          end;
        finally
          Free;
        end;
  end;

var
  HTTPRequest: IHTTPRequest;

  ResponseStrSearchResult: string;
begin
  _TemplateTypeID := TTemplateTypeID(ATemplateTypeID);
  LongWord(_ComponentIDs) := AComponentIDs;
  _Count := 0;
  _Title := AComponentController.FindControl(cTitle).Value;

  case _TemplateTypeID of
    cAudio:
      _style := 'music';
    cGameCube:
      _style := 'games';
    cMovie:
      _style := 'movie';
    cNintendoDS:
      _style := 'games&setpref=NDS';
    cPCGames, cSoftware:
      _style := 'games&setpref=PCG';
    cPlayStation2, cPlayStation3:
      _style := 'games&setpref=PS3';
    cPlayStationPortable:
      _style := 'games&setpref=PSP';
    cWii:
      _style := 'games&setpref=WII';
    cXbox, cXbox360:
      _style := 'games&setpref=XB360';
    cXXX:
      _style := 'ice';
  end;

  HTTPRequest := THTTPRequest.Create(website + 'sresult.asp?style=' + _style + '&HT_Search_Info=' + HTTPEncode(_Title));
  HTTPRequest.Referer := website;
  HTTPRequest.Cookies.Add('IAmAnAdult=yes');

  RequestID1 := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := '(2|4)px;"><a\s+href="\/(.*?)"';

      if Exec(InputString) then
      begin
        repeat

          RequestID2 := HTTPManager.Get(website + Match[2], RequestID1, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID2);

          deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;

end;

end.
