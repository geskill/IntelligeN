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
  uBaseConst, uBaseInterface,
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
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
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

function TBigfishgamesCom.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cPCGames];
  Result := LongWord(_TemplateTypeIDs);
end;

function TBigfishgamesCom.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TBigfishgamesCom.GetControlIDDefaultValue;
begin
  Result := True;
end;

function TBigfishgamesCom.GetResultsLimitDefaultValue;
begin
  Result := 1;
end;

function TBigfishgamesCom.Exec;
var
  I: Integer;
  GamesList: TStringList;
  _Title, _ImageCode: string;
  _ComponentIDs: TControlIDs;

  RequestID: Double;
  ResponseStr: string;
begin
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;

  if Assigned(AControlController.FindControl(cTitle)) and ((Assigned(AControlController.FindControl(cPicture)) and (cPicture in _ComponentIDs)) or
      (Assigned(AControlController.FindControl(cDescription)) and (cDescription in _ComponentIDs))) then
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

          if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
          begin
            with TRegExpr.Create do
              try
                InputString := ResponseStr;
                Expression := '<p class="dlgi_description_text">(.*?)<\/p>';

                if Exec(InputString) then
                  AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(HTML2Text(Match[1])));
              finally
                Free;
              end;
          end;

          if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
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
                        AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1], GetName)
                      else
                      begin
                        Expression := 'src="(.*?)"';
                        if Exec(InputString) then
                          AControlController.FindControl(cPicture).AddProposedValue(GetName, Match[1]);
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
