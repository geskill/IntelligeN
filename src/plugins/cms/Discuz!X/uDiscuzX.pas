unit uDiscuzX;

interface

uses
  // Delphi
  SysUtils, Classes, Controls, Variants,
  // Indy
  IdGlobalProtocols,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uPathUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TDiscuzXSettings = class(TCMSBoardPlugInSettings)
  strict private
    fhtmlon, fallowimgcode, fallowimgurl, fparseurloff, fsmileyoff, fbbcodeoff, fusesig, fordertype, fallownoticeauthor: Boolean;
  published
    [AttrDefaultValue(False)]
    property htmlon: Boolean read fhtmlon write fhtmlon;
    [AttrDefaultValue(True)]
    property allowimgcode: Boolean read fallowimgcode write fallowimgcode;
    [AttrDefaultValue(True)]
    property allowimgurl: Boolean read fallowimgurl write fallowimgurl;
    [AttrDefaultValue(False)]
    property parseurloff: Boolean read fparseurloff write fparseurloff;
    [AttrDefaultValue(False)]
    property smileyoff: Boolean read fsmileyoff write fsmileyoff;
    [AttrDefaultValue(False)]
    property bbcodeoff: Boolean read fbbcodeoff write fbbcodeoff;
    [AttrDefaultValue(False)]
    property usesig: Boolean read fusesig write fusesig;
    [AttrDefaultValue(False)]
    property ordertype: Boolean read fordertype write fordertype;
    [AttrDefaultValue(False)]
    property allownoticeauthor: Boolean read fallownoticeauthor write fallownoticeauthor;

    property forums;
    property threads;
  end;

  TDiscuzX = class(TCMSBoardPlugIn)
  private
    DiscuzXSettings: TDiscuzXSettings;

  const
    error_reg_ex: string = '<div id="messagetext" class="alert_error">\s+<p>(.*?)<\/';
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

{ TDiscuzX }

function TDiscuzX.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, DiscuzXSettings, AComponentController);
  with DiscuzXSettings do
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

function TDiscuzX.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  _formhash, _loginhash: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      ResponseStr := Get(Website + 'member.php?mod=logging&action=login');
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    with TRegExpr.Create do
      try
        InputString := ResponseStr;
        Expression := 'name="login" id="loginform_(.*?)"';

        if Exec(InputString) then
          _loginhash := Match[1];

        Expression := 'name="formhash" value="(.*?)"';

        if Exec(InputString) then
          _formhash := Match[1];
      finally
        Free;
      end;

    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('formhash=' + _formhash);
        Add('referer=' + Website);
        Add('loginfield=username');
        Add('username=' + AccountName);
        Add('password=' + AccountPassword);
        Add('cookietime=2592000');
        Add('questionid=0');
        Add('answer=');
        Add('loginsubmit=');
      end;

      Request.CharSet := DiscuzXSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'member.php?mod=logging&action=login&loginsubmit=yes&handlekey=login&floatlogin=yes&loginhash=' + _loginhash, Params);
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

    if (Pos('window.location.href', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := error_reg_ex;

          if Exec(InputString) then
            Self.ErrorMsg := HTML2Text(Match[1]);
        finally
          Free;
        end;
      Exit;
    end;
  end;

  Result := True;
end;

function TDiscuzX.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'forum.php?mod=post&action=reply&extra=&fid=' + VarToStr(DiscuzXSettings.forums) + '&tid=' + VarToStr
            (DiscuzXSettings.threads))
      else
        AResponse := Get(Website + 'forum.php?mod=post&action=newthread&extra=&fid=' + VarToStr(DiscuzXSettings.forums));
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    if (Pos('name="message"', AResponse) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := AResponse;
          Expression := error_reg_ex;

          if Exec(InputString) then
            Self.ErrorMsg := HTML2Text(Match[1]);
        finally
          Free;
        end;
    end;
  end;
  Result := True;
end;

function TDiscuzX.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
const
  security_inputs: array [0 .. 1] of string = ('formhash', 'posttime');
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;

  I: Integer;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
      with TRegExpr.Create do
        try
          for I := 0 to length(security_inputs) - 1 do
          begin
            InputString := APrevResponse;
            Expression := 'name="' + security_inputs[I] + '" id="' + security_inputs[I] + '" value="(.*?)"';

            if Exec(InputString) then
              Params.Add(security_inputs[I] + '=' + Match[1]);
          end;
        finally
          Free;
        end;

      with Params do
      begin
        Add('wysiwyg=0');
        Add('subject=' + Subject);
        Add('message=' + Message);
        Add('tags=' + Tags);

        Add('save=');
        Add('readperm=');

        with DiscuzXSettings do
        begin
          if htmlon then
            Add('htmlon=1')
          else
            Add('htmlon=0');
          if allowimgcode then
            Add('allowimgcode=on');
          if allowimgurl then
            Add('allowimgurl=on');
          if parseurloff then
            Add('parseurloff=1');
          if smileyoff then
            Add('smileyoff=1');
          if bbcodeoff then
            Add('bbcodeoff=1')
          else
            Add('bbcodeoff=0');
          if usesig then
            Add('usesig=1')
          else
            Add('usesig=0');
          if ordertype then
            Add('ordertype=1')
          else
            Add('ordertype=0');
          if allownoticeauthor then
            Add('allownoticeauthor=1')
          else
            Add('allownoticeauthor=0');
        end;

        Add('hiddenreplies=0');
        Add('addfeed=0');

        if PostReply then
          Add('replysubmit=true')
        else
          Add('topicsubmit=true');
      end;

      Request.CharSet := DiscuzXSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          if PostReply then
            ResponseStr := Post(Website + 'forum.php?mod=post&action=reply&extra=&replysubmit=yes&fid=' + VarToStr(DiscuzXSettings.forums) + '&tid=' + VarToStr
                (DiscuzXSettings.threads), Params)
          else
            ResponseStr := Post(Website + 'forum.php?mod=post&action=newthread&extra=&topicsubmit=yes&fid=' + VarToStr(DiscuzXSettings.forums), Params);
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

      if (Pos('window.location.href', ResponseStr) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := ResponseStr;
            Expression := error_reg_ex;

            if Exec(InputString) then
              Self.ErrorMsg := HTML2Text(Match[1]);
          finally
            Free;
          end;
        Exit;
      end;

      Result := True;
    finally
      Params.Free;
    end;
  end;
end;

constructor TDiscuzX.Create;
begin
  inherited Create;
  DiscuzXSettings := TDiscuzXSettings.Create;
end;

destructor TDiscuzX.Destroy;
begin
  DiscuzXSettings.Free;
  inherited Destroy;
end;

function TDiscuzX.GetName: WideString;
begin
  Result := 'Discuz!X';
end;

function TDiscuzX.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TDiscuzX.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('member.php?mod=logging&amp;action=login', string(AWebsiteSourceCode)) > 0);
end;

function TDiscuzX.GetIDs: Integer;
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
      ResponseStr := IdHTTPHelper.Get(Website + 'search.php?mod=forum&adv=yes');
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

  with TRegExpr.Create do
    try
      InputString := ExtractTextBetween(ResponseStr, 'name="srchfid[]"', '</select>');
      Expression := 'optgroup label="--(.*?)"(.*?)<\/optgroup';

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
                Expression := 'option.*? value="(\d+)"\s*>([&nbsp; ]*)(.*?)<\/';

                if Exec(InputString) then
                begin
                  repeat
                    // need +1 because the TOP-category has been added already
                    BoardLevelIndex := CharCount('&nbsp;', Match[2]) + 1;

                    if BoardLevelIndex > 0 then
                      BoardLevelIndex := (BoardLevelIndex div 3) + 1;

                    if (BoardLevelIndex = BoardLevel.Count) then
                      BoardLevel.Add(CleanPathName(Match[3]))
                    else
                    begin
                      repeat
                        BoardLevel.Delete(BoardLevel.Count - 1);
                      until (BoardLevelIndex = BoardLevel.Count);
                      BoardLevel.Add(CleanPathName(Match[3]));
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

function TDiscuzX.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TDiscuzXSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
