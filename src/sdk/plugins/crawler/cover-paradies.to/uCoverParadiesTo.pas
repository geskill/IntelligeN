unit uCoverParadiesTo;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TCoverParadiesTo = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://cover-paradies.to/';

    function ThumbToLargeImage(AImageURL: string): string;
  public
    function GetName: WideString; override; safecall;

    function InternalGetAvailableTypeIDs: TTypeIDs; override; safecall;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override; safecall;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override; safecall;
    function InternalGetDependentControlIDs: TControlIDs; override; safecall;

    function InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override; safecall;

    function GetResultsLimitDefaultValue: Integer; override; safecall;
  end;

implementation

{ TCoverParadiesTo }

function TCoverParadiesTo.GetName;
begin
  Result := 'cover-paradies.to';
end;

function TCoverParadiesTo.InternalGetAvailableTypeIDs;
begin
  Result := [low(TTypeID) .. high(TTypeID)];
end;

function TCoverParadiesTo.InternalGetAvailableControlIDs;
begin
  Result := [cPicture];
end;

function TCoverParadiesTo.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TCoverParadiesTo.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TCoverParadiesTo.InternalExecute;

  procedure deep_search(AWebsiteSourceCode: string);
  begin
    with TRegExpr.Create do
    begin
      try
        ModifierS := False;
        InputString := AWebsiteSourceCode;
        Expression := '<a class="ElementThumb" href="\.\/(.*?)"><img';

        if Exec(InputString) then
        begin
          repeat
            AControlController.FindControl(cPicture).AddProposedValue(GetName, website + Match[1]);
          until not ExecNext;
        end;
      finally
        Free;
      end;
    end;
  end;

  function TemplateTypeIDToID(ATypeID: TTypeID): string;
  begin
    case ATypeID of
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
  LTitle: string;
  LCount: Integer;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
  begin
    HTTPParams := THTTPParams.Create;
    with HTTPParams do
    begin
      AddFormField('Page', '0');
      AddFormField('B1', 'Search!');
      AddFormField('B33', '');
      AddFormField('SearchString', _Title);
      AddFormField('Sektion', TemplateTypeIDToID(TTypeID(ATypeID)));
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

function TCoverParadiesTo.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
