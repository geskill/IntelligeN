unit uGamestarDe;

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
  TGamestarDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TGamestarDe }

function TGamestarDe.GetName;
begin
  Result := 'gamestar.de';
end;

function TGamestarDe.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cPCGames];
  Result := Word(_TemplateTypeIDs);
end;

function TGamestarDe.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TGamestarDe.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TGamestarDe.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TGamestarDe.Exec;
const
  website = 'http://www.gamestar.de/';
var
  _Count: Integer;
  _Title: string;
  _ComponentIDs: TComponentIDs;

  procedure deep_search(ASourceCode: string);
  begin
    if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
    begin

      with TRegExpr.Create do
        try
          InputString := ASourceCode;
          Expression := '<td>(Genre|Untergenre:)<\/td>\s+<td>(.*?>|-)(.*?)<\/';

          if Exec(InputString) then
          begin
            repeat
              if not(Match[2] = '-</td>') then
                AComponentController.FindControl(cGenre).AddValue(Match[3], GetName);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin

      with TRegExpr.Create do
        try
          InputString := ASourceCode;
          Expression := '<div class="productFooterDescription">(.*?)<\/div>';

          if Exec(InputString) and (Match[1] <> '') then
            AComponentController.FindControl(cDescription).AddValue(Match[1], GetName);
        finally
          Free;
        end;
    end;
  end;

  procedure deep_image_search(ASourceCode: string);
  begin
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := ASourceCode;
          Expression := '<p style="border:1px solid #C9C9C9">\s+<img src="(.*?)"';

          if Exec(InputString) then
            AComponentController.FindControl(cPicture).AddValue(Match[1], GetName);
        finally
          Free;
        end;
    end;
  end;

var
  RequestID1, RequestID2, RequestID3: Double;

  ResponseStrSearchResult, ImageRequestURL: string;
begin
  _Count := 0;
  _Title := AComponentController.FindControl(cTitle).Value;
  LongWord(_ComponentIDs) := AComponentIDs;

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + 'index.cfm?pid=1669&filter=search&s=' + HTTPEncode(_Title)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := '<div class="genreReleaseDate">.*?<\/div>.*?<a href="(.*?)"';

      if Exec(InputString) then
      begin
        repeat
          RequestID2 := HTTPManager.Get(HTMLDecode(website + Match[1]), RequestID1, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID2);

          deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

          ImageRequestURL := website + HTMLDecode(copy(Match[1], 1, LastDelimiter('/', Match[1])) + 'cover/' + copy(Match[1],
              LastDelimiter('/', Match[1]) + 1));

          RequestID3 := HTTPManager.Get(ImageRequestURL, RequestID2, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID3);

          deep_image_search(HTTPManager.GetResult(RequestID3).HTTPResult.SourceCode);

          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;
end;

end.
