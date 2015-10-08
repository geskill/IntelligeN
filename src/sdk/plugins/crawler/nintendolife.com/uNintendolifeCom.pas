unit uNintendolifeCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // Utils
  uHTMLUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TNintendolifeCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

{ TNintendolifeCom }

function TNintendolifeCom.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cGameCube, cNintendoDS, cWii, cOther];
  result := Word(_TemplateTypeIDs);
end;

function TNintendolifeCom.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];
  result := LongWord(_ComponentIDs);
end;

function TNintendolifeCom.GetControlIDDefaultValue;
begin
  result := True;
end;

function TNintendolifeCom.GetResultsLimitDefaultValue;
begin
  result := 5;
end;

function TNintendolifeCom.GetName;
begin
  result := 'nintendolife.com';
end;

function TNintendolifeCom.Exec;
const
  website = 'http://www.nintendolife.com/';
var
  _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
  _Title: string;
  _Count: Integer;

  procedure deep_search(aWebsitecode: string);
  begin
    if (AControlController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := 'Genre<\/strong>:<\/p>\s+<p class="definition">(.*?)<\/p>';

          if Exec(InputString) then
            AControlController.FindControl(cGenre).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;
    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<span>Overview<\/span><\/p><div class="text">(.*?)<\/div>';

          if Exec(InputString) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(StringReplace(Match[1], '<br />', #13#10, [rfReplaceAll]))));
        finally
          Free;
        end;
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<div class="cover">.*?class="framed" src="(.*?)"';

          if Exec(InputString) then
            AControlController.FindControl(cPicture).AddProposedValue(GetName, StringReplace(Match[1], 'cover_small', 'cover_large', []));
        finally
          Free;
        end;
  end;

var
  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  _TemplateTypeID := TTypeID(ATypeID);
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
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
