unit uXrelTo;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCrawlerClass, uIdHTTPHelper;

type
  TXrelTo = class(TCrawlerPlugIn)
  protected { . }
  const
    WEBSITE = 'https://www.xrel.to/';
  protected
    function SimpleGETRequest(AURL: string; AIdHTTPHelper: TIdHTTPHelper): string;
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override; stdcall;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override; stdcall;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean; override; stdcall;
    function GetLimitDefaultValue: Integer; override; stdcall;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override; stdcall;
  end;

implementation

function TXrelTo.SimpleGETRequest;
var
  ReplyData: TStringStream;
begin
  ReplyData := TStringStream.Create('', CP_UTF8);
  try
    AIdHTTPHelper.Get(AURL, ReplyData);

    Result := ReplyData.DataString;
  finally
    ReplyData.Free;
  end;
end;

function TXrelTo.GetName;
begin
  Result := 'Xrel.to';
end;

function TXrelTo.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)] - [cAudio];
  Result := Word(_TemplateTypeIDs);
end;

function TXrelTo.GetAvailableComponentIDs;
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  _TemplateTypeID := TTemplateTypeID(TemplateTypeID);
  _ComponentIDs := [cReleaseDate, cTitle, cNFO];

  if not(_TemplateTypeID = cXXX) then
    _ComponentIDs := _ComponentIDs + [cPicture, cDescription];

  if not(_TemplateTypeID in [cSoftware, cXXX]) then
    _ComponentIDs := _ComponentIDs + [cGenre];

  if (_TemplateTypeID = cMovie) then
    _ComponentIDs := _ComponentIDs + [cRuntime, cAudioStream, cVideoStream];

  Result := LongWord(_ComponentIDs);
end;

function TXrelTo.GetComponentIDDefaultValue;
var
  _TemplateTypeID: TTemplateTypeID;
  _ComponentID: TComponentID;
begin
  _TemplateTypeID := TTemplateTypeID(TemplateTypeID);
  _ComponentID := TComponentID(ComponentID);

  Result := True;

  if (cPicture = _ComponentID) or (cNFO = _ComponentID) then
    Result := False;
end;

function TXrelTo.GetLimitDefaultValue;
begin
  Result := 0;
end;

procedure TXrelTo.Exec;
const
  XREL_MONTH: array [0 .. 11] of string = ('Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez');
var
  LReleasename: string;
  _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;

  IdHTTPHelper: TIdHTTPHelper;

  ProductInformationPageLink, ResponseStrReleaseInformation, ResponseStrProductInformation, s: string;

  LStringDate: string;
begin
  _TemplateTypeID := TTemplateTypeID(ATemplateTypeID);
  LongWord(_ComponentIDs) := AComponentIDs;
  LReleasename := AComponentController.FindControl(cReleaseName).Value;

  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    IdHTTPHelper.Request.Referer := WEBSITE + 'home.html';
    ResponseStrReleaseInformation := SimpleGETRequest(WEBSITE + 'search.html?xrel_search_query=' + LReleasename, IdHTTPHelper);

    if Pos('nfo_title', ResponseStrReleaseInformation) > 0 then
    begin
      if (AComponentController.FindControl(cReleaseDate) <> nil) and (cReleaseDate in _ComponentIDs) then
      begin
        with TRegExpr.Create do
        begin
          try
            InputString := ResponseStrReleaseInformation;
            Expression := 'Zeit: <\/div> <div class="l_right">\s+(\d+)\. ([a-zA-Z]{3}) (\d{4}),';

            if Exec(InputString) then
            begin
              repeat
                LStringDate := PadLeft(Match[1], '0', 2) + '.' + PadLeft(IntToStr(IndexText(Match[2], XREL_MONTH) + 1), '0', 2) + '.' + Match[3];
                // TODO: Improve localized date
                AComponentController.FindControl(cReleaseDate).AddValue(LStringDate, GetName);
              until not ExecNext;
            end;
          finally
            Free;
          end;
        end;
      end;

      if (AComponentController.FindControl(cNFO) <> nil) and (cNFO in _ComponentIDs) then
      begin
        with TRegExpr.Create do
        begin
          try
            InputString := ResponseStrReleaseInformation;
            Expression := '<pre>(.*?)<\/pre>';

            if Exec(InputString) then
            begin
              repeat
                AComponentController.FindControl(cNFO).AddValue(Trim(HTML2Text(Match[1], False)), GetName);
              until not ExecNext;
            end;
          finally
            Free;
          end;
        end;
      end;

      with TRegExpr.Create do
      begin
        try
          InputString := ResponseStrReleaseInformation;
          Expression := '<a href=\"\/([^\"]+?)\">Produktinformationen<\/a>';

          if Exec(InputString) then
          begin
            repeat
              ProductInformationPageLink := Match[1];
            until not ExecNext;
          end;
        finally
          Free;
        end;
      end;

      if (_TemplateTypeID = cMovie) then
      begin
        if (AComponentController.FindControl(cAudioStream) <> nil) and (cAudioStream in _ComponentIDs) then
          with TRegExpr.Create do
            try
              InputString := ResponseStrReleaseInformation;
              Expression := 'Audio-Stream: </div> <div class="l_right"> (.*?) </div>';
              if Exec(InputString) then
                AComponentController.FindControl(cAudioStream).AddValue(Match[1], GetName);
            finally
              Free;
            end;

        if (AComponentController.FindControl(cVideoStream) <> nil) and (cVideoStream in _ComponentIDs) then
          with TRegExpr.Create do
            try
              InputString := ResponseStrReleaseInformation;
              Expression := 'Video-Stream: </div> <div class="l_right"> (.*?) </div>';

              if Exec(InputString) then
                AComponentController.FindControl(cVideoStream).AddValue(Match[1], GetName);
            finally
              Free;
            end;
      end;

      if (cTitle in _ComponentIDs) or (cDescription in _ComponentIDs) or (cGenre in _ComponentIDs) then
      begin
        ResponseStrProductInformation := SimpleGETRequest(WEBSITE + ProductInformationPageLink, IdHTTPHelper);

        if Pos('extinfo_title', ResponseStrProductInformation) > 0 then
        begin
          if (AComponentController.FindControl(cTitle) <> nil) and (cTitle in _ComponentIDs) then
          begin
            with TRegExpr.Create do
            begin
              try
                InputString := ResponseStrProductInformation;
                Expression := '<h3>(.*?)<\/h3>';

                if Exec(InputString) then
                begin
                  repeat
                    AComponentController.FindControl(cTitle).AddValue(HTML2Text(Match[1]), GetName);
                  until not ExecNext;
                end;
              finally
                Free;
              end;
            end;
          end;

          if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
            with TRegExpr.Create do
              try
                InputString := ResponseStrProductInformation;
                Expression := '<div id="poster" style="line-height:0;"><div>  <img src="\/([^\"]+?)"';

                if Exec(InputString) then
                begin
                  repeat
                    if not(Match[1] = '') then
                    begin
                      s := WEBSITE + Match[1];

                      AComponentController.FindControl(cPicture).AddValue(s, GetName);
                    end;
                  until not ExecNext;
                end;
              finally
                Free;
              end;

          if (AComponentController.FindControl(cRuntime) <> nil) and (cRuntime in _ComponentIDs) then
            with TRegExpr.Create do
              try
                InputString := ResponseStrProductInformation;
                Expression := 'Laufzeit:</div> <div class="l_right" title="(\d+) ';

                if Exec(InputString) then
                  AComponentController.FindControl(cRuntime).AddValue(Match[1], GetName);
              finally
                Free;
              end;

          if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
          begin
            with TRegExpr.Create do
            begin
              try
                InputString := ResponseStrProductInformation;
                Expression := '<div class="article_text" style="margin:0;"> (.*?)(<table| <\/div>)';

                if Exec(InputString) then
                begin
                  repeat
                    AComponentController.FindControl(cDescription).AddValue(Trim(HTML2Text(Match[1])), GetName);
                  until not ExecNext;
                end;
              finally
                Free;
              end;
            end;
          end;

          if (AComponentController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
          begin
            with TRegExpr.Create do
            begin
              try
                InputString := ResponseStrProductInformation;
                Expression := 'Genre:</div> <div class="l_right">(.*?)<\/div>';

                if Exec(InputString) then
                begin
                  s := Match[1];
                  if (Pos('/', s) > 0) or (Pos(',', s) > 0) or (Pos('|', s) > 0) then
                  begin
                    with TRegExpr.Create do
                    begin
                      try
                        InputString := s;
                        Expression := '([^\/,|]+)';

                        if Exec(InputString) then
                        begin
                          repeat
                            AComponentController.FindControl(cGenre).AddValue(Trim(Match[1]), GetName);
                          until not ExecNext;
                        end;
                      finally
                        Free;
                      end;
                    end;
                  end
                  else
                  begin
                    AComponentController.FindControl(cGenre).AddValue(s, GetName);
                  end;
                end;
              finally
                Free;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    IdHTTPHelper.Free;
  end;
end;

end.
