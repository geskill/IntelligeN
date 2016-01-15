unit ufluxbb;

interface

uses
  // Delphi
  SysUtils, Classes, Variants,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInHTTPClasses;

type
  TfluxbbSettings = class(TCMSBoardPlugInSettings)
  strict private
    fhide_smilies: Boolean;
  public
    constructor Create; override;
  published
    property hide_smilies: Boolean read fhide_smilies write fhide_smilies;

    property forums;
    property threads;
  end;

  Tfluxbb = class(TCMSBoardPlugIn)
  private
    fluxbbSettings: TfluxbbSettings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;
    function LoadSettings(const AData: ITabSheetData = nil): Boolean; override;

    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function NeedPrePost(out ARequestURL: string): Boolean; override;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(const AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;

    function DoAnalyzeIDsRequest(const AResponseStr: string): Integer; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(const AWebsiteSourceCode: WideString): WordBool; override; safecall;
    function GetArticleLink(const AURL: WideString; const AArticleID: Integer): WideString; override; safecall;
  end;

implementation

{ TfluxbbSettings }

constructor TfluxbbSettings.Create;
begin
  inherited Create;

  // default setup
  hide_smilies := False;
end;

{ Tfluxbb }

function Tfluxbb.SettingsClass;
begin
  Result := TfluxbbSettings;
end;

function Tfluxbb.GetSettings;
begin
  Result := fluxbbSettings;
end;

procedure Tfluxbb.SetSettings;
begin
  fluxbbSettings := ACMSPlugInSettings as TfluxbbSettings;
end;

function Tfluxbb.LoadSettings;
begin
  Result := inherited LoadSettings(AData);
  with fluxbbSettings do
  begin
    if Assigned(AData) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function Tfluxbb.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'login.php?action=in');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := fluxbbSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('req_username', AccountName);
    AddFormField('req_password', AccountPassword);
    AddFormField('redirect_url', Website + 'index.php');
    AddFormField('form_sent', '1');
    AddFormField('save_pass', '1');
    AddFormField('login', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function Tfluxbb.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;
  Result := not((Pos('http-equiv="refresh" content="1', AResponseStr) = 0) and (Pos('login.php?action=out', AResponseStr) = 0));
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := '<div class="inbox">\s+<p>(.*?)<';

        if Exec(InputString) then
        begin
          Self.ErrorMsg := HTML2Text(Match[1]);
        end;
      finally
        Free;
      end;
end;

function Tfluxbb.NeedPrePost;
begin
  Result := False;
end;

function Tfluxbb.DoBuildPostRequest;
var
  RequestURL: string;
begin
  Result := True;

  if PostReply then
    RequestURL := Website + 'post.php?action=post&tid=' + VarToStr(fluxbbSettings.threads)
  else
    RequestURL := Website + 'post.php?action=post&fid=' + VarToStr(fluxbbSettings.forums);

  AHTTPRequest := THTTPRequest.Create(RequestURL);
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := fluxbbSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('form_sent', '1');
    AddFormField('form_user', AccountName);
    AddFormField('req_subject', Subject);
    AddFormField('req_message', Message);

    with fluxbbSettings do
    begin
      if hide_smilies then
        AddFormField('hide_smilies', '1');
    end;

    AddFormField('submit', '');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function Tfluxbb.DoAnalyzePost;
begin
  Result := True;

  with TRegExpr.Create do
    try
      InputString := AResponseStr;
      Expression := '"error-list">(.*?)<\/ul>';

      if Exec(InputString) then
      begin
        Self.ErrorMsg := HTML2Text(Match[1]);
        Result := False;
      end;
    finally
      Free;
    end;
end;

function Tfluxbb.DoAnalyzeIDsRequest;
var
  BoardLevel: TStringList;
  BoardLevelIndex: Integer;

  CategoryName, CategoryHTMLList: string;

  function IDPath(AStringList: TStringList): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to AStringList.Count - 1 do
    begin
      if not SameStr('', Result) then
        Result := Result + ' -> ';
      Result := Result + AStringList.Strings[I];
    end;
  end;

  function CleanPathName(AName: string): string;
  begin
    Result := Trim(HTML2Text(AName));
  end;

begin
  // FluxBB 1.2.X
  // http://www.fluxbb.de/forum/search.php

  // FluxBB 1.4.X
  // http://fluxbb.org/forums/search.php
  // http://fluxbb.fr/forums/search.php

  with TRegExpr.Create do
    try
      if Pos('name="forum"', AResponseStr) > 0 then
      begin
        // FluxBB 1.2.X
        InputString := ExtractTextBetween(AResponseStr, 'name="forum"', '</select>');
        Expression := 'optgroup label="(.*?)"(.*?)<\/optgroup';
      end
      else
      begin
        // FluxBB 1.4.X
        InputString := ExtractTextBetween(AResponseStr, 'class="checklist"', '</select>');
        Expression := '<fieldset><legend><span>(.*?)<\/span><\/legend>(.*?)<\/fieldset>';
      end;

      if Exec(InputString) then
      begin
        repeat
          CategoryName := Match[1];
          CategoryHTMLList := Match[2];

          BoardLevel := TStringList.Create;
          try
            // adding TOP-category
            BoardLevel.Add(CategoryName);

            with TRegExpr.Create do
              try
                InputString := CategoryHTMLList;
                if Pos('name="forum"', AResponseStr) > 0 then
                  // FluxBB 1.2.X
                  Expression := 'option.*? value="(\d+)">(.*?)<\/'
                else
                  // FluxBB 1.4.X
                  Expression := 'name="forums\[\]".*? value="(\d+)" \/><\/span> <label for="forum-\d+">(.*?)<\/';

                if Exec(InputString) then
                begin
                  repeat
                    // need +1 because the TOP-category has been added already
                    // there are no sub-sub-categories -> always 1
                    BoardLevelIndex := 1;

                    if (BoardLevelIndex = BoardLevel.Count) then
                      BoardLevel.Add(CleanPathName(Match[2]))
                    else
                    begin
                      repeat
                        BoardLevel.Delete(BoardLevel.Count - 1);
                      until (BoardLevelIndex = BoardLevel.Count);
                      BoardLevel.Add(CleanPathName(Match[2]));
                    end;

                    AddID(Match[1], IDPath(BoardLevel));
                  until not ExecNext;
                end;
              finally
                Free;
              end;

          finally
            BoardLevel.Free;
          end;

        until not ExecNext;
      end;
    finally
      Free;
    end;
  Result := FCheckedIDsList.Count;
end;

function Tfluxbb.GetName;
begin
  Result := 'FluxBB';
end;

function Tfluxbb.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function Tfluxbb.BelongsTo;
begin
  Result := (Pos('misc.php?action=rules', string(AWebsiteSourceCode)) > 0) or (Pos('extern.php?action=feed', string(AWebsiteSourceCode)) > 0);
end;

function Tfluxbb.GetArticleLink;
begin
  Result := Format('%sviewtopic.php?p=%d#p%1:d', [AURL, AArticleID]);
end;

end.
