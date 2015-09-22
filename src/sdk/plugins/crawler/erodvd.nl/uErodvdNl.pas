unit uErodvdNl;

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
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TErodvdNl = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

function TErodvdNl.GetName;
begin
  result := 'erodvd.nl';
end;

function TErodvdNl.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cXXX];
  result := Word(_TemplateTypeIDs);
end;

function TErodvdNl.GetAvailableComponentIDs;
var
  // _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  // _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

  _ComponentIDs := [cPicture, cDescription];

  result := LongWord(_ComponentIDs);
end;

function TErodvdNl.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TErodvdNl.GetLimitDefaultValue;
begin
  result := 5;
end;

procedure TErodvdNl.Exec;
const
  website = 'http://www.erodvd.nl/';
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
          Expression := '<a href="javascript:change_preview_image\(''(.*?)''';

          if Exec(InputString) then
          begin
            AComponentController.FindControl(cPicture).AddValue(website + Match[1], GetName);
          end;
        finally
          Free;
        end;
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '255\);">(.*?)<\/span>';

          if Exec(InputString) then
          begin
            repeat
              AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(HTMLDecode(Match[1]))), GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
  end;

var
  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + 'ssl/index.php?searchStr=' + HTTPEncode(_Title) + '&act=viewCat&Submit=Go'),
    TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := 'center" class="(tdOdd|tdEven)"><a href="(.*?)"';

      if Exec(InputString) then
      begin
        repeat

          RequestID2 := HTTPManager.Get(HTMLDecode(website + 'ssl/' + Match[2]), RequestID1, TPlugInHTTPOptions.Create(Self));

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
