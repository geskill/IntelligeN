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
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TErodvdNl = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

function TErodvdNl.GetName;
begin
  result := 'erodvd.nl';
end;

function TErodvdNl.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cXXX];
  result := LongWord(_TemplateTypeIDs);
end;

function TErodvdNl.GetAvailableControlIDs;
var
  // _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
begin
  // _TemplateTypeID := TTypeID(ATypeID);

  _ComponentIDs := [cPicture, cDescription];

  result := LongWord(_ComponentIDs);
end;

function TErodvdNl.GetControlIDDefaultValue;
begin
  result := True;
end;

function TErodvdNl.GetResultsLimitDefaultValue;
begin
  result := 5;
end;

function TErodvdNl.Exec;
const
  website = 'http://www.erodvd.nl/';
var
  _ComponentIDs: TControlIDs;
  _Title: string;
  _Count: Integer;

  procedure deep_search(aWebsitecode: string);
  begin
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '<a href="javascript:change_preview_image\(''(.*?)''';

          if Exec(InputString) then
          begin
            AControlController.FindControl(cPicture).AddProposedValue(GetName, website + Match[1]);
          end;
        finally
          Free;
        end;
    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
      with TRegExpr.Create do
        try
          InputString := aWebsitecode;
          Expression := '255\);">(.*?)<\/span>';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(HTMLDecode(Match[1]))));
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
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
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
