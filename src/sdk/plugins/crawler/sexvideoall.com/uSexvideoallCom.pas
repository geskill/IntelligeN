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
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TSexvideoallCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TSexvideoallCom }

function TSexvideoallCom.GetName;
begin
  result := 'sexvideoall.com';
end;

function TSexvideoallCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cXXX];
  result := Word(_TemplateTypeIDs);
end;

function TSexvideoallCom.GetAvailableComponentIDs;
var
  // _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  // _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

  _ComponentIDs := [cPicture, cGenre];

  result := LongWord(_ComponentIDs);
end;

function TSexvideoallCom.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TSexvideoallCom.GetLimitDefaultValue: Integer;
begin
  result := 5;
end;

procedure TSexvideoallCom.Exec;
const
  website = 'http://sexvideoall.com/';
var
  _ComponentIDs: TComponentIDs;
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
            AComponentController.FindControl(cGenre).AddValue(Match[1], GetName);
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
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
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
          if (cPicture in _ComponentIDs) and Assigned(AComponentController.FindControl(cPicture)) then
          begin
            _FoundPictureFront := StringReplace(Match[1], 'shop', 'SamplePhoto', [rfIgnoreCase]);
            AComponentController.FindControl(cPicture).AddValue(_FoundPictureFront, GetName);
            AComponentController.FindControl(cPicture).AddValue(StringReplace(_FoundPictureFront, '.jpg', 'f.jpg', [rfIgnoreCase]), GetName);
          end;
          if (cGenre in _ComponentIDs) and Assigned(AComponentController.FindControl(cGenre)) then
            ExtractGenres(Match[3]);
          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
