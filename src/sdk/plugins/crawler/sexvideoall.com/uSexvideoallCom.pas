unit uSexvideoallCom;

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
  TSexvideoallCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

{ TSexvideoallCom }

function TSexvideoallCom.GetName;
begin
  result := 'sexvideoall.com';
end;

function TSexvideoallCom.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cXXX];
  result := LongWord(_TemplateTypeIDs);
end;

function TSexvideoallCom.GetAvailableControlIDs;
var
  // _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
begin
  // _TemplateTypeID := TTypeID(ATypeID);

  _ComponentIDs := [cPicture, cGenre];

  result := LongWord(_ComponentIDs);
end;

function TSexvideoallCom.GetControlIDDefaultValue;
begin
  result := True;
end;

function TSexvideoallCom.GetResultsLimitDefaultValue: Integer;
begin
  result := 5;
end;

function TSexvideoallCom.Exec;
const
  website = 'http://sexvideoall.com/';
var
  _ComponentIDs: TControlIDs;
  _Title, _FoundPictureFront: string;
  _Count: Integer;

  procedure ExtractGenres(AGenreCode: string);
  begin
    with TRegExpr.Create do
      try
        InputString := AGenreCode;
        Expression := '">(.*?)<\/a>&nbsp;';

        if Exec(InputString) then
        begin
          repeat
            AControlController.FindControl(cGenre).AddProposedValue(GetName, Match[1]);
          until not ExecNext;
        end;
      finally
        Free;
      end;
  end;

var
  RequestID: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
  _Count := 0;

  RequestID := HTTPManager.Get(THTTPRequest.Create(website + 'DirectE/BrowseResults.asp?T88=0&T3=' + HTTPEncode(_Title) + '&act=viewCat&Submit=Go'),
    TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := '<td align="center" width="70" height="100">.*?<img border="0" src="(.*?)".*?<b>Title:&nbsp;(.*?)<\/b>.*?Genre:<\/td>(.*?)<\/td>';

      if Exec(InputString) then
      begin
        repeat
          if (cPicture in _ComponentIDs) and Assigned(AControlController.FindControl(cPicture)) then
          begin
            _FoundPictureFront := StringReplace(Match[1], 'shop', 'SamplePhoto', [rfIgnoreCase]);
            AControlController.FindControl(cPicture).AddProposedValue(GetName, _FoundPictureFront);
            AControlController.FindControl(cPicture).AddProposedValue(GetName, StringReplace(_FoundPictureFront, '.jpg', 'f.jpg', [rfIgnoreCase]));
          end;
          if (cGenre in _ComponentIDs) and Assigned(AControlController.FindControl(cGenre)) then
            ExtractGenres(Match[3]);
          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
