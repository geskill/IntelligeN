unit uGamestarDe;

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
  uPlugInInterface, uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TGamestarDe = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'http://www.gamestar.de/';
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function InternalGetAvailableTypeIDs: TTypeIDs; override;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override;
    function InternalGetDependentControlIDs: TControlIDs; override;

    function InternalGetRetrieveData(const AAccountData: IAccountData; const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override;

    function GetResultsLimitDefaultValue: Integer; override;
  end;

implementation

{ TGamestarDe }

function TGamestarDe.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TGamestarDe.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TGamestarDe.GetDescription;
begin
  Result := GetName + ' crawler plug-in.';
end;

function TGamestarDe.GetName;
begin
  Result := 'gamestar.de';
end;

function TGamestarDe.InternalGetAvailableTypeIDs;
begin
  Result := [cPCGames];
end;

function TGamestarDe.InternalGetAvailableControlIDs;
begin
  Result := [cPicture, cGenre, cDescription];
end;

function TGamestarDe.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TGamestarDe.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TGamestarDe.InternalGetRetrieveData;

  procedure deep_search(AWebsiteSourceCode: string);
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'productPackshot.*?\/idgwpgsgp\/bdb\/(\d+)\/';

          if Exec(InputString) then
            AControlController.FindControl(cPicture).AddProposedValue(GetName, 'http://1images.cgames.de/images/idgwpgsgp/bdb/' + Match[1] + '/600x.jpg');
        finally
          Free;
        end;

    if ACanUse(cGenre) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<td>(Genre|Untergenre:)<\/td>\s+<td>(.*?>|-)(.*?)<\/';

          if Exec(InputString) then
          begin
            repeat
              if not(Match[2] = '-</td>') then
                AControlController.FindControl(cGenre).AddProposedValue(GetName, Match[3]);
            until not ExecNext;
          end;
        finally
          Free;
        end;

    if ACanUse(cDescription) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="productFooterDescription">(.*?)<\/div>';

          if Exec(InputString) and not SameStr('', Match[1]) then
            AControlController.FindControl(cDescription).AddProposedValue(GetName, Trim(Match[1]));
        finally
          Free;
        end;
  end;

var
  LTitle, LImageRequestURL: string;
  LCount: Integer;

  LRequestID1, LRequestID2, LRequestID3: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  // http://www.gamestar.de/index.cfm?pid=1669&filter=search&s=Halo+2

  LResponeStr := GETRequest(WEBSITE + 'index.cfm?pid=1669&filter=search&s=' + HTTPEncode(LTitle), LRequestID1);

  if (Pos('keine passenden Spiele gefunden', LResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        InputString := LResponeStr;
        Expression := '<div class="genreReleaseDate">.*?<\/div>.*?<a href="\/(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            LResponeStr := GETFollowUpRequest(WEBSITE + Match[1], LRequestID1, LRequestID2);

            deep_search(LResponeStr);

            Inc(LCount);
          until not(ExecNext and ((LCount < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end;

  Result := True;
end;

function TGamestarDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
