unit uPcgdbDe;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Utils
  uHTMLUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TPcgdbDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TPcgdbDe.GetName;
begin
  result := 'pcgdb.de';
end;

function TPcgdbDe.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cPCGames];
  result := Word(_TemplateTypeIDs);
end;

function TPcgdbDe.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];
  result := LongWord(_ComponentIDs);
end;

function TPcgdbDe.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TPcgdbDe.GetLimitDefaultValue;
begin
  result := 5;
end;

procedure TPcgdbDe.Exec;
const
  website = 'http://www.pcgdb.de/';
var
  _ComponentIDs: TComponentIDs;
  _Title: string;
  _Count: Integer;

  procedure deep_search(aWebsitecode: string);
  begin
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<a href="titles(.*?)"';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cPicture).AddValue(website + 'titles' + Match[1], GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<td>Genre:<\/td>\W+<td width="233" bgcolor="#000000">(.*?)<\/td>';

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
          Expression := '<tr><td bgcolor="#FFFFFF" class="content">(.*?)<br><br>';

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

var
  HTTPParams: IHTTPParams;

  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFormField('titel', _Title);
    AddFormField('search', 'submit');
  end;

  RequestID1 := HTTPManager.Post(THTTPRequest.Create(website + 'games_search.php'), HTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;

      Expression := '<td class="newstext"><a href="(.*?)"';

      if Exec(InputString) then
      begin
        repeat
          RequestID2 := HTTPManager.Get(website + Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

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
