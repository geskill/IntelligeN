unit uXonairCom;

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
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TXonairCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

function TXonairCom.GetName;
begin
  result := 'Xonair.com';
end;

function TXonairCom.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cXXX];
  result := LongWord(_TemplateTypeIDs);
end;

function TXonairCom.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture, cLanguage, cDescription];
  result := LongWord(_ComponentIDs);
end;

function TXonairCom.GetControlIDDefaultValue;
begin
  result := True;
end;

function TXonairCom.GetResultsLimitDefaultValue;
begin
  result := 5;
end;

function TXonairCom.Exec;
const
  website = 'http://xonair.com/';
  img = 'http://image3.imageservers.com/pdimages/';
var
  _ComponentIDs: TControlIDs;
  _Title: string;

  procedure deep_search(AWebsiteSourceCode: string);
  begin
    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<table border="0" cellpadding="0" cellspacing="0" style="margin-left:10px; margin-bottom:20px">(\s*?)<tr>(\s*?)<td>(.*?)</td>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cDescription).AddProposedValue(GetName, Match[3]);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;

    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<img src="' + img + '(.*?)"';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cPicture).AddProposedValue(GetName, img + Match[1]);
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
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;

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
