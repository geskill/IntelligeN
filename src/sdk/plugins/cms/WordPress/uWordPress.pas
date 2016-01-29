unit uWordPress;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants, XMLDoc, XMLIntf, ActiveX,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uStringUtils,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInConst, uPlugInCMSClass, uPlugInCMSBlogClass, uPlugInHTTPClasses;

type
  TWordPressSettings = class(TCMSBlogPlugInSettings)
  strict private
    fcomment_status, fdraft, fsticky: Boolean;

    fcustomfields: TCMSCustomFields;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property comment_status: Boolean read fcomment_status write fcomment_status;
    property draft: Boolean read fdraft write fdraft;
    property sticky: Boolean read fsticky write fsticky;

    property categorys;

    property customfields: TCMSCustomFields read fcustomfields write fcustomfields;
  end;

  TWordPress = class(TCMSBlogPlugIn)
  private
    WordPressSettings: TWordPressSettings;
  protected
    function SettingsClass: TCMSPlugInSettingsMeta; override;
    function GetSettings: TCMSPlugInSettings; override;
    procedure SetSettings(ACMSPlugInSettings: TCMSPlugInSettings); override;

    function DoBuildLoginRequest(out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; ACAPTCHALogin: Boolean = False): Boolean; override;
    function DoAnalyzeLogin(const AResponseStr: string; out ACAPTCHALogin: Boolean): Boolean; override;

    function NeedPrePost(out ARequestURL: string): Boolean; override;
    function DoAnalyzePrePost(const AResponseStr: string): Boolean; override;

    function DoBuildPostRequest(const AData: ITabSheetData; out AHTTPRequest: IHTTPRequest; out AHTTPParams: IHTTPParams; out AHTTPOptions: IHTTPOptions; APrevResponse: string; APrevRequest: Double): Boolean; override;
    function DoAnalyzePost(const AResponseStr: string; AHTTPProcess: IHTTPProcess): Boolean; override;

    function GetIDsRequestURL: string; override;
    function DoAnalyzeIDsRequest(const AResponseStr: string): Integer; override;
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override; safecall;
    function BelongsTo(const AWebsiteSourceCode: WideString): WordBool; override; safecall;
    function GetIDs: Integer; override; safecall;
    function GetArticleLink(const AURL: WideString; const AArticleID: Integer): WideString; override; safecall;
  end;

implementation

{ TWordPressSettings }

constructor TWordPressSettings.Create;
begin
  inherited Create;
  customfields := TCMSCustomFields.Create;

  // default setup
  comment_status := True;
  draft := False;
  sticky := False;
end;

destructor TWordPressSettings.Destroy;
begin
  customfields.Free;
  inherited Destroy;
end;

{ TWordPress }

function TWordPress.SettingsClass;
begin
  Result := TWordPressSettings;
end;

function TWordPress.GetSettings;
begin
  Result := WordPressSettings;
end;

procedure TWordPress.SetSettings;
begin
  WordPressSettings := ACMSPlugInSettings as TWordPressSettings;
end;

function TWordPress.DoBuildLoginRequest;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'wp-login.php');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := WordPressSettings.CharSet;

    Cookies.Add('wordpress_test_cookie=WP+Cookie+check');
  end;

  AHTTPParams := THTTPParams.Create;
  with AHTTPParams do
  begin
    AddFormField('log', AccountName);
    AddFormField('pwd', AccountPassword);
    AddFormField('rememberme', 'forever');

    AddFormField('wp-submit', '');
    AddFormField('testcookie', '1');
  end;

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
end;

function TWordPress.DoAnalyzeLogin;
begin
  ACAPTCHALogin := False;

  Result := not(Pos('action=logout', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := 'error">(.*?)<\/div>';

        if Exec(InputString) then
        begin
          repeat
            Self.ErrorMsg := HTML2Text(Match[1]);
          until not ExecNext;
        end;
      finally
        Free;
      end;
end;

function TWordPress.NeedPrePost;
begin
  Result := True;
  ARequestURL := Website + 'wp-admin/post-new.php';
end;

function TWordPress.DoAnalyzePrePost;
begin
  Result := not(Pos('name="_wpnonce"', AResponseStr) = 0);
  if not Result then
    with TRegExpr.Create do
      try
        InputString := AResponseStr;
        Expression := 'error-page">(.*?)<\/';

        if Exec(InputString) then
        begin
          repeat
            Self.ErrorMsg := Trim(HTML2Text(Match[1]));
          until not ExecNext;
        end;
      finally
        Free;
      end;
end;

function TWordPress.DoBuildPostRequest;
const
  security_inputs: array [0 .. 10] of string = ('_wpnonce', '_wp_http_referer', 'user_ID', 'post_author', 'referredby', '_wp_original_http_referer', 'post_ID', 'meta-box-order-nonce', 'closedpostboxesnonce', 'samplepermalinknonce',
    '_ajax_nonce-add-meta');
var
  I: Integer;

  _ajax_nonce_add_meta: string;

  HTTPParams: IHTTPParams;
  RequestID: Double;
  HTTPProcess: IHTTPProcess;
begin
  Result := True;

  AHTTPRequest := THTTPRequest.Create(Website + 'wp-admin/post.php');
  with AHTTPRequest do
  begin
    Referer := Website;
    CharSet := WordPressSettings.CharSet;
  end;

  AHTTPParams := THTTPParams.Create();
  with AHTTPParams do
  begin
    with TRegExpr.Create do
      try
        for I := 0 to length(security_inputs) - 1 do
        begin
          InputString := APrevResponse;
          Expression := 'name=("|'')' + security_inputs[I] + '("|'') value=("|'')(.*?)("|'')';

          if Exec(InputString) then
          begin
            repeat
              AddFormField(security_inputs[I], Match[4]);

              if SameStr('post_ID', security_inputs[I]) then
                ArticleID := StrToIntDef(Match[4], 0);
              if SameStr('_ajax_nonce-add-meta', security_inputs[I]) then
                _ajax_nonce_add_meta := Match[4];
            until not ExecNext;
          end;
        end;
      finally
        Free;
      end;

    AddFormField('_wp_http_referer', Website + 'wp-admin/post-new.php');
    AddFormField('referredby', Website + 'wp-admin/');
    AddFormField('_wp_original_http_referer', Website + 'wp-admin/');

    AddFormField('action', 'editpost');
    AddFormField('originalaction', 'editpost');
    AddFormField('post_type', 'post');
    AddFormField('original_post_status', 'auto-draft');
    AddFormField('auto_draft', '');

    AddFormField('post_title', Subject);
    AddFormField('content', Message);

    AddFormField('hidden_post_status', 'draft');
    AddFormField('post_status', 'draft');
    AddFormField('hidden_post_password', '');
    AddFormField('hidden_post_sticky', 'sticky');
    AddFormField('hidden_post_visibility', 'public');
    AddFormField('visibility', 'public');

    with WordPressSettings do
    begin
      if comment_status then
        AddFormField('comment_status', 'open');
      if sticky then
        AddFormField('sticky', 'sticky');
    end;

    AddFormField('original_publish', '1');

    if WordPressSettings.draft then
      AddFormField('save', '1')
    else
      AddFormField('publish', '1');

    AddFormField('post_format', '0');

    with TRegExpr.Create do
      try
        InputString := VarToStr(WordPressSettings.categorys);
        Expression := '(\d+)';

        if Exec(InputString) then
        begin
          repeat
            AddFormField('post_category[]', VarToStrDef(Match[1], ''));
          until not ExecNext;
        end;
      finally
        Free;
      end;

    AddFormField('tax_input[post_tag]', Tags);

    AddFormField('excerpt', '');
    AddFormField('trackback_url', '');

    for I := 0 to WordPressSettings.customfields.FieldCount - 1 do
    begin
      HTTPParams := THTTPParams.Create();
      with HTTPParams do
      begin
        AddFormField('_ajax_nonce', '0');
        AddFormField('action', 'add-meta');
        AddFormField('metakeyselect', '#NONE#');
        AddFormField('metakeyinput', WordPressSettings.customfields.Field[I].Name);
        AddFormField('metavalue', WordPressSettings.customfields.Field[I].Value);
        AddFormField('_ajax_nonce-add-meta', _ajax_nonce_add_meta);
        AddFormField('post_id', IntToStr(ArticleID));
      end;

      RequestID := HTTPManager.Post(Website + 'wp-admin/admin-ajax.php', APrevRequest, HTTPParams);

      repeat
        sleep(50);
      until HTTPManager.HasResult(RequestID);

      HTTPProcess := HTTPManager.GetResult(RequestID);

      with TRegExpr.Create do
        try
          InputString := HTTPProcess.HTTPResult.SourceCode;
          Expression := 'meta\[(\d+)\]';

          if Exec(InputString) then
          begin
            AddFormField('meta[' + Match[1] + '][key]', WordPressSettings.customfields.Field[I].Name);
            AddFormField('meta[' + Match[1] + '][value]', WordPressSettings.customfields.Field[I].Value);
          end;
        finally
          Free;
        end;
    end;
  end;

  AHTTPRequest.Cookies.Add('wp-saving-post-' + IntToStr(ArticleID) + '=check');

  AHTTPOptions := TPlugInHTTPOptions.Create(Self);
  with AHTTPOptions do
    HandleRedirects := False;
end;

function TWordPress.DoAnalyzePost;
begin
  Result := not(Pos('"hidden_post_status" value="publish"', AResponseStr) = 0) or not(Pos('=' + IntToStr(ArticleID), string(AHTTPProcess.HTTPResult.HTTPResponse.Location)) = 0);

  if not Result then
  begin
    ErrorMsg := 'Something went wrong publishing the article. Maybe a draft is created (Id: ' + IntToStr(ArticleID) + ')';
  end;
end;

function TWordPress.GetIDsRequestURL;
begin
  Result := Website + 'xmlrpc.php';
end;

function TWordPress.DoAnalyzeIDsRequest;
var
  LNeedToUninitialize: Boolean;
  XMLDoc: IXMLDocument;

  BoardLevel: TStringList;

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

  procedure RecursiveIDGrabber(AXMLNode: IXMLNode; AParentID: string; ALevelIndex: Integer; ABoardLevel: TStringList);
  var
    I, J: Integer;
    categoryId, parentId, categoryName: string;
  begin
    for I := 0 to AXMLNode.ChildNodes.Count - 1 do
      with AXMLNode.ChildNodes.Nodes[I].ChildNodes.Nodes['struct'] do
      begin
        for J := 0 to ChildNodes.Count - 1 do
          with ChildNodes.Nodes[J] do // member's
          begin
            if SameText('categoryId', VarToStr(ChildNodes.Nodes['name'].NodeValue)) then
            begin
              if Assigned(ChildNodes.Nodes['value'].ChildNodes.FindNode('string')) then
                categoryId := VarToStr(ChildNodes.Nodes['value'].ChildNodes.Nodes['string'].NodeValue)
              else
                categoryId := VarToStr(ChildNodes.Nodes['value'].ChildNodes.Nodes['int'].NodeValue)
            end
            else if SameText('parentId', VarToStr(ChildNodes.Nodes['name'].NodeValue)) then
            begin
              if Assigned(ChildNodes.Nodes['value'].ChildNodes.FindNode('string')) then
                parentId := VarToStr(ChildNodes.Nodes['value'].ChildNodes.Nodes['string'].NodeValue)
              else
                parentId := VarToStr(ChildNodes.Nodes['value'].ChildNodes.Nodes['int'].NodeValue)
            end
            else if SameText('categoryName', VarToStr(ChildNodes.Nodes['name'].NodeValue)) then
            begin
              categoryName := VarToStr(ChildNodes.Nodes['value'].ChildNodes.Nodes['string'].NodeValue);
            end;
          end;

        if SameText(AParentID, parentId) then
        begin
          if (ALevelIndex = ABoardLevel.Count) then
            ABoardLevel.Add(categoryName)
          else
          begin
            repeat
              ABoardLevel.Delete(ABoardLevel.Count - 1);
            until (ALevelIndex = ABoardLevel.Count);
            ABoardLevel.Add(categoryName);
          end;

          AddID(categoryId, IDPath(ABoardLevel));

          RecursiveIDGrabber(AXMLNode, categoryId, ALevelIndex + 1, ABoardLevel);

        end;
      end;
  end;

begin
  LNeedToUninitialize := Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED));
  try
    XMLDoc := NewXMLDocument;
    try
      with XMLDoc do
      begin
        LoadFromXML(Trim(AResponseStr));
        Active := True;
      end;

      with XMLDoc.ChildNodes.Nodes['methodResponse'] do
        if Assigned(ChildNodes.FindNode('params')) then
        begin
          BoardLevel := TStringList.Create;
          try
            RecursiveIDGrabber(ChildNodes.Nodes['params'].ChildNodes.Nodes['param'].ChildNodes.Nodes['value'].ChildNodes.Nodes['array'].ChildNodes.Nodes['data'], '0', 0, BoardLevel);
          finally
            BoardLevel.Free;
          end;
        end;

    finally
      XMLDoc := nil;
    end;
  finally
    if LNeedToUninitialize then
      CoUninitialize;
  end;
  Result := FCheckedIDsList.Count;
end;

function TWordPress.GetName;
begin
  Result := 'WordPress';
end;

function TWordPress.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TWordPress.BelongsTo;
begin
  Result := (Pos('xmlrpc.php', string(AWebsiteSourceCode)) > 0);
end;

function TWordPress.GetIDs;

  function newPageXMLDoc: string;
  var
    LNeedToUninitialize: Boolean;
    XMLDoc: IXMLDocument;
    StringStream: TStringStream;
  begin
    LNeedToUninitialize := Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED));
    try
      XMLDoc := NewXMLDocument;
      try
        with XMLDoc do
        begin
          Options := Options + [doNodeAutoIndent];
          Active := True;
          Encoding := 'utf-8';
          StandAlone := 'yes';
        end;

        with XMLDoc.AddChild('methodCall') do
        begin
          AddChild('methodName').NodeValue := 'metaWeblog.getCategories';
          with AddChild('params') do
          begin
            AddChild('param').AddChild('value').AddChild('string').NodeValue := '1';
            AddChild('param').AddChild('value').AddChild('string').NodeValue := AccountName;
            AddChild('param').AddChild('value').AddChild('string').NodeValue := AccountPassword;
          end;
        end;

        // Using SaveToXML didn't adds the encoding information in the xml header
        StringStream := TStringStream.Create('', CP_UTF8);
        try
          XMLDoc.SaveToStream(StringStream);
          Result := StringStream.DataString;
        finally
          StringStream.Free;
        end;

      finally
        XMLDoc := nil;
      end;

    finally
      if LNeedToUninitialize then
        CoUninitialize;
    end;
  end;

var
  HTTPRequest: IHTTPRequest;
  HTTPParams: IHTTPParams;
  HTTPOptions: IHTTPOptions;

  RequestID: Double;
begin
  Result := FCheckedIDsList.Count;

  LoadSettings;

  HTTPRequest := THTTPRequest.Create(GetIDsRequestURL);
  with HTTPRequest do
  begin
    Referer := Website;
    CharSet := WordPressSettings.CharSet;
  end;

  HTTPParams := THTTPParams.Create(newPageXMLDoc);

  HTTPOptions := TPlugInHTTPOptions.Create(Self);
  with HTTPOptions do
    RedirectMaximum := 1;

  RequestID := HTTPManager.Post(HTTPRequest, HTTPParams, HTTPOptions);

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  {
    if HTTPProcess.HTTPResult.HasError then
    begin
    ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
    Exit;
    end;
    }

  Result := DoAnalyzeIDsRequest(HTTPManager.GetResult(RequestID).HTTPResult.SourceCode);
end;

function TWordPress.GetArticleLink;
begin
  Result := Format('%s?p=%d', [AURL, AArticleID]);
end;

end.
