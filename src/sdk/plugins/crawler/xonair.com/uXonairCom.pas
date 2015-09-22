unit uXonairCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TXonairCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TXonairCom.GetName;
begin
  result := 'Xonair.com';
end;

function TXonairCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cXXX];
  result := Word(_TemplateTypeIDs);
end;

function TXonairCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cLanguage, cDescription];
  result := LongWord(_ComponentIDs);
end;

function TXonairCom.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TXonairCom.GetLimitDefaultValue;
begin
  result := 5;
end;

procedure TXonairCom.Exec;
const
  website = 'http://xonair.com/';
  img = 'http://image3.imageservers.com/pdimages/';
var
  _ComponentIDs: TComponentIDs;
  _Title: string;

  procedure deep_search(AWebsiteSourceCode: string);
  begin
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<table border="0" cellpadding="0" cellspacing="0" style="margin-left:10px; margin-bottom:20px">(\s*?)<tr>(\s*?)<td>(.*?)</td>';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cDescription).AddValue(Match[3], GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;

    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<img src="' + img + '(.*?)"';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cPicture).AddValue(img + Match[1], GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
  end;

var
  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + 'search.cfm?lang=US&pid=home&str=' + HTTPEncode(_Title) + '&view=all&searchby=title'),
    TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  if not(Pos('Search Release Date', ResponseStrSearchResult) = 0) then
  begin
    with TRegExpr.Create do
      try
        ModifierG := False;
        InputString := ResponseStrSearchResult;
        Expression := '<table width="100%" border="0" cellspacing="0" cellpadding="0" style="font-family:verdana">(\s*?)<tr>(\s*?)<td><a href="(.*?)"';

        if Exec(InputString) then
        begin
          repeat

            RequestID2 := HTTPManager.Get(Match[3], RequestID1, TPlugInHTTPOptions.Create(Self));

            repeat
              sleep(50);
            until HTTPManager.HasResult(RequestID2);

            deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

          until not ExecNext;
        end;
      finally
        Free;
      end;
  end;
end;

end.
