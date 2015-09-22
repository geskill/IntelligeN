unit uNintendolifeCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
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
  TNintendolifeCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TNintendolifeCom }

function TNintendolifeCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cGameCube, cNintendoDS, cWii, cOther];
  result := Word(_TemplateTypeIDs);
end;

function TNintendolifeCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];
  result := LongWord(_ComponentIDs);
end;

function TNintendolifeCom.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TNintendolifeCom.GetLimitDefaultValue;
begin
  result := 5;
end;

function TNintendolifeCom.GetName;
begin
  result := 'nintendolife.com';
end;

procedure TNintendolifeCom.Exec;
const
  website = 'http://www.nintendolife.com/';
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
  _Title: string;
  _Count: Integer;

  procedure deep_search(aWebsitecode: string);
  begin
    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'Genre<\/strong>:<\/p>\s+<p class="definition">(.*?)<\/p>';

          if Exec(InputString) then
            AComponentController.FindControl(cGenre).AddValue(Match[1], GetName);
        finally
          Free;
        end;
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<span>Overview<\/span><\/p><div class="text">(.*?)<\/div>';

          if Exec(InputString) then
            AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(StringReplace(Match[1], '<br />', #13#10, [rfReplaceAll]))), GetName);
        finally
          Free;
        end;
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<div class="cover">.*?class="framed" src="(.*?)"';

          if Exec(InputString) then
            AComponentController.FindControl(cPicture).AddValue(StringReplace(Match[1], 'cover_small', 'cover_large', []), GetName);
        finally
          Free;
        end;
  end;

var
  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  _TemplateTypeID := TTemplateTypeID(ATemplateTypeID);
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + 'games?title=' + HTTPEncode(_Title)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := '<td class="title"><a title=".*?" href="(.*?)".*?<td class="system"><abbr title=".*?"';

      if Exec(InputString) then
      begin
        repeat
          case _TemplateTypeID of
            cGameCube:
              if (Match[2] = 'GCN') then
              begin

                RequestID2 := HTTPManager.Get(Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

                repeat
                  sleep(50);
                until HTTPManager.HasResult(RequestID2);

                deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

              end;
            cNintendoDS:
              if (Match[2] = 'DS') or (Match[2] = 'DDSiWare') or (Match[2] = '3DS') then
              begin

                RequestID2 := HTTPManager.Get(Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

                repeat
                  sleep(50);
                until HTTPManager.HasResult(RequestID2);

                deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

              end;
            cWii, cOther:
              begin

                RequestID2 := HTTPManager.Get(Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

                repeat
                  sleep(50);
                until HTTPManager.HasResult(RequestID2);

                deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

              end;
          end;

          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
