unit uDiscuz;

interface

uses
  // Delphi
  SysUtils, Classes, Controls, Variants,
  // Indy
  IdGlobalProtocols, IdMultipartFormData,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uPathUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBoardClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TDiscuzSettings = class(TCMSBoardPlugInSettings)
  strict private
    fhtmlon, fallowimgcode, fparseurloff, fsmileyoff, fbbcodeoff, ftagoff, fusesig: Boolean;
  published
    [AttrDefaultValue(False)]
    property htmlon: Boolean read fhtmlon write fhtmlon;
    [AttrDefaultValue(True)]
    property allowimgcode: Boolean read fallowimgcode write fallowimgcode;
    [AttrDefaultValue(False)]
    property parseurloff: Boolean read fparseurloff write fparseurloff;
    [AttrDefaultValue(False)]
    property smileyoff: Boolean read fsmileyoff write fsmileyoff;
    [AttrDefaultValue(False)]
    property bbcodeoff: Boolean read fbbcodeoff write fbbcodeoff;
    [AttrDefaultValue(False)]
    property tagoff: Boolean read ftagoff write ftagoff;
    [AttrDefaultValue(False)]
    property usesig: Boolean read fusesig write fusesig;

    property forums;
    property threads;
  end;

  TDiscuz = class(TCMSBoardPlugIn)
  private
    DiscuzSettings: TDiscuzSettings;
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

{ TDiscuz }

function TDiscuz.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, DiscuzSettings, AComponentController);
  with DiscuzSettings do
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

function TDiscuz.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
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
        Add('username=' + AccountName);
        Add('password=' + AccountPassword);
        Add('loginfield=username');
        Add('userlogin=');
      end;

      // POST /logging.php?action=login&loginsubmit=yes&inajax=1 HTTP/1.1

      Request.CharSet := DiscuzSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'logging.php?action=login&loginsubmit=true', Params);
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

    if (Pos('action=logout', ResponseStr) = 0) then
    begin
      with TRegExpr.Create do
        try
          InputString := ResponseStr;
          Expression := '<h1><\/h1>\s+<p>(.*?)<';

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

function TDiscuz.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      if PostReply then
        AResponse := Get(Website + 'post.php?action=reply&fid=' + VarToStr(DiscuzSettings.forums) + '&tid=' + VarToStr(DiscuzSettings.threads))
      else
        AResponse := Get(Website + 'post.php?action=newthread&fid=' + VarToStr(DiscuzSettings.forums));
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    // TODO: Überprüfung ob der User überhaupt Rechte hat, korrekt eingeloggt wurde
  end;
  Result := True;
end;

function TDiscuz.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
  APrevResponse: string): Boolean;
var
  Params: TIdMultiPartFormDataStream;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TIdMultiPartFormDataStream.Create;
    try
      with TRegExpr.Create do
        try
          InputString := APrevResponse;
          Expression := 'id="formhash" value="(.*?)"';

          if Exec(InputString) then
            Params.AddFormField('formhash', Match[1], DiscuzSettings.CharSet).ContentTransfer := 'binary';
        finally
          Free;
        end;

      with Params do
      begin
        AddFormField('subject', Subject, DiscuzSettings.CharSet).ContentTransfer := 'binary';
        AddFormField('message', Message, DiscuzSettings.CharSet).ContentTransfer := 'binary';
        AddFormField('tags', Tags, DiscuzSettings.CharSet).ContentTransfer := 'binary';

        if PostReply then
          AddFormField('fid', VarToStr(DiscuzSettings.forums), DiscuzSettings.CharSet).ContentTransfer := 'binary';

        with DiscuzSettings do
        begin
          if htmlon then
            AddFormField('htmlon', '1', DiscuzSettings.CharSet)
          else
            AddFormField('htmlon', '0', DiscuzSettings.CharSet);
          if allowimgcode then
            AddFormField('allowimgcode', 'on', DiscuzSettings.CharSet);
          if parseurloff then
            AddFormField('parseurloff', '1', DiscuzSettings.CharSet);
          if smileyoff then
            AddFormField('smileyoff', '1', DiscuzSettings.CharSet);
          if bbcodeoff then
            AddFormField('bbcodeoff', '1', DiscuzSettings.CharSet);
          if tagoff then
            AddFormField('tagoff', '1', DiscuzSettings.CharSet);
          if usesig then
            AddFormField('usesig', '1', DiscuzSettings.CharSet);
        end;

        if PostReply then
          AddFormField('replysubmit', '', DiscuzSettings.CharSet)
        else
          AddFormField('topicsubmit', '', DiscuzSettings.CharSet);
      end;

      Request.ContentType := 'multipart/form-data';
      try
        if PostReply then
          ResponseStr := Post(Website + 'post.php?action=reply&replysubmit=yes&fid=' + VarToStr(DiscuzSettings.forums) + '&tid=' + VarToStr
              (DiscuzSettings.threads), Params)
        else
          ResponseStr := Post(Website + 'post.php?action=newthread&topicsubmit=yes&fid=' + VarToStr(DiscuzSettings.forums), Params);
      except
        on E: Exception do
        begin
          ErrorMsg := E.message;
          Exit;
        end;
      end;

      if (Pos('http-equiv="refresh" content="3', ResponseStr) = 0) and (Pos('window.location.href', ResponseStr) = 0) then
      begin
        with TRegExpr.Create do
          try
            InputString := ResponseStr;
            Expression := 'message">.*?<p>(.*?)<';

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

constructor TDiscuz.Create;
begin
  inherited Create;
  DiscuzSettings := TDiscuzSettings.Create;
end;

destructor TDiscuz.Destroy;
begin
  DiscuzSettings.Free;
  inherited Destroy;
end;

function TDiscuz.GetName: WideString;
begin
  Result := 'Discuz!';
end;

function TDiscuz.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TDiscuz.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('logging.php?action=login', string(AWebsiteSourceCode)) > 0) { or (Pos('member.php?mod=logging&action=login',
    string(AWebsiteSourceCode)) > 0) } ;
end;

function TDiscuz.GetIDs: Integer;
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

  with TRegExpr.Create do
    try
      InputString := ExtractTextBetween(ResponseStr, 'name="srchfid[]"', '</select>');
      Expression := 'optgroup label="(.*?)"(.*?)<\/optgroup';

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
                Expression := 'option.*? value="(\d+)">([&nbsp; gt]*)(.*?)<\/';

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

function TDiscuz.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TDiscuzSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
