unit ufluxbb;

interface

uses
  // Delphi 
  SysUtils, Classes, Controls, Variants,
  // Indy 
  IdGlobalProtocols,
  // RegEx 
  RegExpr,
  // Utils, 
  uHTMLUtils, uSpecialStringUtils,
  // Common 
  uConst, uAppInterface,
  // Plugin system 
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TfluxbbSettings = class(TCMSBoardPlugInSettings)
  strict private
    fhide_smilies: Boolean;
  published
    [AttrDefaultValue(False)]
    property hide_smilies: Boolean read fhide_smilies write fhide_smilies;

    property forums;
    property threads;
  end;

  Tfluxbb = class(TCMSBoardPlugIn)
  private
    fluxbbSettings: TfluxbbSettings;
  protected
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
    function PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean; override;
    function PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
      APrevResponse: string = ''): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override;
    function BelongsTo(AWebsiteSourceCode: WideString): Boolean; override;
    function GetIDs: Integer; override;
    function ShowWebsiteSettingsEditor(AWebsiteEditor: IWebsiteEditor): Boolean; override;
  end;

implementation

function Tfluxbb.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, fluxbbSettings, AComponentController);
  with fluxbbSettings do
  begin
    if SameStr('', CharSet) then
      CharSet := DefaultCharset;

    if Assigned(AComponentController) and (forums = null) then
    begin
      ErrorMsg := StrForumIdIsUndefine;
      Result := False;
    end;

    PostReply := not((threads = null) or (threads = '') or (threads = 0));
  end;
end;

function Tfluxbb.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('req_username=' + AccountName);
        Add('req_password=' + AccountPassword);
        Add('redirect_url= ' + Website + 'index.php');
        Add('form_sent=1');
        Add('save_pass=1');
        Add('login=');
      end;

      Request.CharSet := fluxbbSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'login.php?action=in', Params, Enc);
        except
          on E: Exception do
          begin
            ErrorMsg := E.message;
            Exit;
          end;
        end;
      finally
        Enc.Free;
      end;
    finally
      Params.Free;
    end;

    if (Pos('http-equiv="refresh" content="1', ResponseStr) = 0) and (Pos('login.php?action=out', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '<div class="inbox">\s+<p>(.*?)<';

          if Exec(InputString) then
          begin
            Self.ErrorMsg := HTML2Text(Match[1]);
          end;
        finally
          Free;
        end;

      Exit;
    end;
  end;

  Result := True;
end;

function Tfluxbb.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  AResponse := '';
  Result := True;
end;

function Tfluxbb.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('form_sent=1');
        Add('form_user=' + AccountName);
        Add('req_subject=' + Subject);
        Add('req_message=' + Message);

        with fluxbbSettings do
        begin
          if hide_smilies then
            Add('hide_smilies=1');
        end;

        Add('submit=');
      end;

      Request.CharSet := fluxbbSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          if PostReply then
            ResponseStr := Post(Website + 'post.php?action=post&tid=' + VarToStr(fluxbbSettings.threads), Params, Enc)
          else
            ResponseStr := Post(Website + 'post.php?action=post&fid=' + VarToStr(fluxbbSettings.forums), Params, Enc);
        except
          on E: Exception do
          begin
            ErrorMsg := E.message;
            Exit;
          end;
        end;
      finally
        Enc.Free;
      end;

      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '"error-list">(.*?)<\/ul>';

          if Exec(InputString) then
          begin
            Self.ErrorMsg := HTML2Text(Match[1]);
            Exit;
          end;
        finally
          Free;
        end;

      Result := True;
    finally
      Params.Free;
    end;
  end;
end;

constructor Tfluxbb.Create;
begin
  inherited Create;
  fluxbbSettings := TfluxbbSettings.Create;
end;

destructor Tfluxbb.Destroy;
begin
  fluxbbSettings.Free;
  inherited Destroy;
end;

function Tfluxbb.GetName;
begin
  Result := 'FluxBB';
end;

function Tfluxbb.DefaultCharset;
begin
  Result := 'ISO-8859-1';
end;

function Tfluxbb.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('misc.php?action=rules', string(AWebsiteSourceCode)) > 0) or (Pos('extern.php?action=feed', string(AWebsiteSourceCode)) > 0);
end;

function Tfluxbb.GetIDs: Integer;
var
  IdHTTPHelper: TIdHTTPHelper;
  ResponseStr: string;

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
  Result := 0;

  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    LoadSettings;

    if not(AccountName = '') then
      if not Login(IdHTTPHelper) then
        Exit;

    try
      ResponseStr := IdHTTPHelper.Get(Website + 'search.php');
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;
  finally
    IdHTTPHelper.Free;
  end;

  // FluxBB 1.2.X 
  // http://www.fluxbb.de/forum/search.php 

  // FluxBB 1.4.X 
  // http://fluxbb.org/forums/search.php 
  // http://fluxbb.fr/forums/search.php 

  with TRegExpr.Create do
    try
      if Pos('name="forum"', ResponseStr) > 0 then
      begin
        // FluxBB 1.2.X 
        InputString := ExtractTextBetween(ResponseStr, 'name="forum"', '</select>');
        Expression := 'optgroup label="(.*?)"(.*?)<\/optgroup';
      end
      else
      begin
        // FluxBB 1.4.X 
        InputString := ExtractTextBetween(ResponseStr, 'class="checklist"', '</select>');
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
                if Pos('name="forum"', ResponseStr) > 0 then
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

function Tfluxbb.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TfluxbbSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
