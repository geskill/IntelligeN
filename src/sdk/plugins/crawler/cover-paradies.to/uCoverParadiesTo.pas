unit uCoverParadiesTo;

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
  TCoverParadiesTo = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TCoverParadiesTo }

function TCoverParadiesTo.GetName;
begin
  Result := 'cover-paradies.to';
end;

function TCoverParadiesTo.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)];
  Result := Word(_TemplateTypeIDs);
end;

function TCoverParadiesTo.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture];
  Result := LongWord(_ComponentIDs);
end;

function TCoverParadiesTo.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TCoverParadiesTo.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TCoverParadiesTo.Exec;
const
  website: string = 'http://cover-paradies.to/';
var
  _ComponentIDs: TComponentIDs;
  _Title: string;
  _Count: Integer;

  procedure deep_search(AWebsitecode: string);
  begin
    with TRegExpr.Create do
    begin
      try
        ModifierS := False;
        InputString := AWebsitecode;
        Expression := '<a class="ElementThumb" href="\.\/(.*?)"><img';

        if Exec(InputString) then
        begin
          repeat
            AComponentController.FindControl(cPicture).AddValue(website + Match[1], GetName);
          until not ExecNext;
        end;
      finally
        Free;
      end;
    end;
  end;

  function TemplateTypeIDToID(TemplateTypeID: TTemplateTypeID): string;
  begin
    case TemplateTypeID of
      cAudio:
        Result := '';
      cGameCube:
        Result := '21';
      cMovie:
        Result := '';
      cNintendoDS:
        Result := '29';
      cPCGames:
        Result := '4';
      cPlayStation2:
        Result := '7';
      cPlayStation3:
        Result := '25';
      cPlayStationPortable:
        Result := '';
      cSoftware:
        Result := '14';
      cWii:
        Result := '24';
      cXbox:
        Result := '20';
      cXbox360:
        Result := '23';
      cXXX:
        Result := '9';
      cOther:
        Result := '';
    end;
  end;

var
  HTTPParams: IHTTPParams;

  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;

  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
  begin
    HTTPParams := THTTPParams.Create;
    with HTTPParams do
    begin
      AddFormField('Page', '0');
      AddFormField('B1', 'Search!');
      AddFormField('B33', '');
      AddFormField('SearchString', _Title);
      AddFormField('Sektion', TemplateTypeIDToID(TTemplateTypeID(ATemplateTypeID)));
    end;

    RequestID1 := HTTPManager.Post(THTTPRequest.Create(website + '?Module=SimpleSearch'), HTTPParams, TPlugInHTTPOptions.Create(Self));

    repeat
      sleep(50);
    until HTTPManager.HasResult(RequestID1);

    ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

    if (Pos('download selection', ResponseStrSearchResult) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStrSearchResult;
          Expression := '<b><a href="(.*?)"';

          if Exec(InputString) then
          begin
            repeat

              RequestID2 := HTTPManager.Get(website + HTMLDecode(Match[1]), RequestID1, TPlugInHTTPOptions.Create(Self));

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
    end
    else
      deep_search(ResponseStrSearchResult);
  end;
end;

end.
