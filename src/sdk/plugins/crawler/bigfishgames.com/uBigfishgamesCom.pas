unit uBigfishgamesCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils, uStringUtils,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TBigfishgamesCom = class(TCrawlerPlugIn)
  protected
    function BigfishgamesURL: string; virtual;
    function BigfishgamesMaskTitle(ATitle: string): string;
    procedure BigfishgamesGamesList(AStrings: TStrings);
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TBigfishgamesDe }

function TBigfishgamesCom.BigfishgamesURL: string;
const
  website = 'http://www.bigfishgames.com/';
begin
  Result := website + 'download-games/';
end;

function TBigfishgamesCom.BigfishgamesMaskTitle(ATitle: string): string;
begin
  Result := Trim(ATitle);

  with TRegExpr.Create do
    try
      Result := ReplaceRegExpr('[^\w\s]', Result, '%', False);
    finally
      Free;
    end;

  Result := '%' + Result + '%';
end;

procedure TBigfishgamesCom.BigfishgamesGamesList(AStrings: TStrings);
var
  HTTPRequest: IHTTPRequest;

  RequestID: Double;

  ResponseStr: string;
begin
  HTTPRequest := THTTPRequest.Create(BigfishgamesURL + 'all.html');

  RequestID := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponseStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStr;
      Expression := 'data-gid="(\d+)">(.*?)<\/a><\/li>';

      if Exec(InputString) then
        repeat
          AStrings.Add(Match[1] + AStrings.NameValueSeparator + HTML2Text(Match[2]));
        until not ExecNext;
    finally
      Free;
    end;
end;

function TBigfishgamesCom.GetName;
begin
  Result := 'bigfishgames.com';
end;

function TBigfishgamesCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cPCGames];
  Result := Word(_TemplateTypeIDs);
end;

function TBigfishgamesCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TBigfishgamesCom.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TBigfishgamesCom.GetLimitDefaultValue;
begin
  Result := 1;
end;

procedure TBigfishgamesCom.Exec;
var
  I: Integer;
  GamesList: TStringList;
  _Title, _ImageCode: string;
  _ComponentIDs: TComponentIDs;

  RequestID: Double;
  ResponseStr: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;

  if Assigned(AComponentController.FindControl(cTitle)) and ((Assigned(AComponentController.FindControl(cPicture)) and (cPicture in _ComponentIDs)) or
      (Assigned(AComponentController.FindControl(cDescription)) and (cDescription in _ComponentIDs))) then
  begin
    GamesList := TStringList.Create;
    try
      BigfishgamesGamesList(GamesList);

      for I := 0 to GamesList.Count - 1 do
        if MatchTextMask(BigfishgamesMaskTitle(_Title), GamesList.ValueFromIndex[I]) then
        begin

          RequestID := HTTPManager.Get(THTTPRequest.Create(BigfishgamesURL + GamesList.Names[I] + '/'), TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID);

          ResponseStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

          if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
          begin
            with TRegExpr.Create do
              try
                InputString := ResponseStr;
                Expression := '<p class="dlgi_description_text">(.*?)<\/p>';

                if Exec(InputString) then
                  AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(Match[1])), GetName);
              finally
                Free;
              end;
          end;

          if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
            with TRegExpr.Create do
              try
                InputString := ResponseStr;
                Expression := '<div class="dlgi_featureimage"(.*?)<\/div>';

                if Exec(InputString) then
                begin
                  _ImageCode := Match[1];

                  with TRegExpr.Create do
                    try
                      InputString := _ImageCode;
                      Expression := 'data-src="(.*?)"';

                      if Exec(InputString) then
                        AComponentController.FindControl(cPicture).AddValue(Match[1], GetName)
                      else
                      begin
                        Expression := 'src="(.*?)"';
                        if Exec(InputString) then
                          AComponentController.FindControl(cPicture).AddValue(Match[1], GetName);
                      end;
                    finally
                      Free;
                    end;
                end;
              finally
                Free;
              end;
        end;
    finally
      GamesList.Free;
    end;
  end;
end;

end.