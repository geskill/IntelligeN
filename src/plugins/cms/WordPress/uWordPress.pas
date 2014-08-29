unit uWordPress;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Controls, Variants, XMLDoc, XMLIntf, ActiveX,
  // Indy
  IdGlobal, IdURI, IdHTTP, IdGlobalProtocols,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Utils,
  uHTMLUtils, uSpecialStringUtils,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBlogClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TWordPressSettings = class(TCMSBlogPlugInSettings)
  strict private
    fcomment_status, fdraft, fsticky: Boolean;
  published
    [AttrDefaultValue(True)]
    property comment_status: Boolean read fcomment_status write fcomment_status;
    [AttrDefaultValue(False)]
    property draft: Boolean read fdraft write fdraft;
    [AttrDefaultValue(False)]
    property sticky: Boolean read fsticky write fsticky;

    property categorys;
  end;

  TWordPress = class(TCMSBlogPlugIn)
  private
    WordPressSettings: TWordPressSettings;
  protected
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
    function PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
    function PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController; APrevResponse: string = ''): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override;
    function BelongsTo(AWebsiteSourceCode: WideString): Boolean; override;
    function GetIDs: Integer; override;
    function Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean; override;
    function ShowWebsiteSettingsEditor(AWebsiteEditor: IWebsiteEditor): Boolean; override;
  end;

implementation

function TWordPress.LoadSettings;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, WordPressSettings, AComponentController);
  with WordPressSettings do
  begin
    if SameStr('', CharSet) then
      CharSet := DefaultCharset;

    // no need to define any category
  end;
end;

function TWordPress.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params: TStringList;
  Enc: TEncoding;
  ResponseStr: string;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    AddCookie('wordpress_test_cookie=WP+Cookie+check', Website);

    Params := TStringList.Create;
    try
      with Params do
      begin
        Add('log=' + AccountName);
        Add('pwd=' + AccountPassword);
        Add('rememberme=forever');

        Add('wp-submit=');
        Add('testcookie=1');
      end;

      Request.CharSet := WordPressSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'wp-login.php', Params, Enc);
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
      begin
        try
          InputString := ResponseStr;
          Expression := 'error">(.*?)<\/div>';

          if Exec(InputString) then
          begin
            Self.ErrorMsg := HTML2Text(Match[1]);
            Exit;
          end;
        finally
          Free;
        end;
      end;
    end;
  end;
  Result := True;
end;

function TWordPress.PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    try
      AResponse := Get(Website + 'wp-admin/post-new.php');
    except
      on E: Exception do
      begin
        ErrorMsg := E.message;
        Exit;
      end;
    end;

    if (Pos('name="_wpnonce"', AResponse) = 0) then
    begin
      with TRegExpr.Create do
      begin
        try
          InputString := AResponse;
          Expression := 'error-page">(.*?)<\/';

          if Exec(InputString) then
            Self.ErrorMsg := HTML2Text(Trim(Match[1]));
        finally
          Free;
        end;
      end;
      Exit;
    end;
  end;
  Result := True;
end;

function TWordPress.PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController; APrevResponse: string): Boolean;
const
  security_inputs: array [0 .. 9] of string = ('_wpnonce', '_wp_http_referer', 'user_ID', 'post_author', 'referredby', '_wp_original_http_referer', 'post_ID', 'meta-box-order-nonce', 'closedpostboxesnonce', 'samplepermalinknonce');
var
  ResponseStr: string;
  Params: TStringList;
  Enc: TEncoding;

  post_ID: string;

  I: Integer;
begin
  Result := False;
  with AIdHTTPHelper do
  begin
    Params := TStringList.Create;
    try
      with Params do
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
                  Add(security_inputs[I] + '=' + Match[4]);

                  if SameStr('post_ID', security_inputs[I]) then
                    post_ID := Match[4];
                until not ExecNext;
              end;
            end;
          finally
            Free;
          end;

        Add('_wp_http_referer=' + Website + 'wp-admin/post-new.php');
        Add('referredby=' + Website + 'wp-admin/');
        Add('_wp_original_http_referer=' + Website + 'wp-admin/');

        Add('action=editpost');
        Add('originalaction=editpost');
        Add('post_type=post');
        Add('original_post_status=auto-draft');
        Add('auto_draft=');

        Add('post_title=' + Subject);
        Add('content=' + Message);

        Add('hidden_post_status=draft');
        Add('post_status=draft');
        Add('hidden_post_password=');
        Add('hidden_post_sticky=sticky');
        Add('hidden_post_visibility=public');
        Add('visibility=public');

        with WordPressSettings do
        begin
          if comment_status then
            Add('comment_status=open');
          if sticky then
            Add('sticky=sticky');
        end;

        Add('original_publish=1');

        if WordPressSettings.draft then
          Add('save=1')
        else
          Add('publish=1');

        Add('post_format=0');

        with TRegExpr.Create do
          try
            InputString := VarToStr(WordPressSettings.categorys);
            Expression := '(\d+)';

            if Exec(InputString) then
            begin
              repeat
                Add('post_category[]=' + VarToStrDef(Match[1], ''));
              until not ExecNext;
            end;
          finally
            Free;
          end;

        Add('tax_input[post_tag]=' + Tags);

        Add('excerpt=');
        Add('trackback_url=');
      end;

      AddCookie('wp-saving-post-' + post_ID + '=check', Website);

      HandleRedirects := False;

      Request.CharSet := WordPressSettings.CharSet;
      Enc := CharsetToEncoding(Request.CharSet);
      try
        try
          ResponseStr := Post(Website + 'wp-admin/post.php', Params, Enc);
        except
          //
        end;
      finally
        Enc.Free;
      end;

      Result := Pos('=' + post_ID, Response.Location) > 0;
    finally
      Params.Free;
    end;
  end;
end;

constructor TWordPress.Create;
begin
  inherited Create;
  WordPressSettings := TWordPressSettings.Create;
end;

destructor TWordPress.Destroy;
begin
  WordPressSettings.Free;
  inherited Destroy;
end;

function TWordPress.GetName;
begin
  Result := 'WordPress';
end;

function TWordPress.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TWordPress.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('xmlrpc.php', string(AWebsiteSourceCode)) > 0);
end;

function TWordPress.GetIDs;

  function newPageXMLDoc: string;
  var
    XMLDoc: IXMLDocument;
    StringStream: TStringStream;
  begin
    OleInitialize(nil);
    try
      XMLDoc := NewXMLDocument;

      with XMLDoc do
      begin
        Active := True;
        Encoding := 'UTF-8';
        StandAlone := 'yes';
        Options := Options + [doNodeAutoIndent];
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

      // XMLDoc.SaveToXML(Result);
      XMLDoc := nil;
    finally
      OleUninitialize;
    end;
  end;

  function resultPageXMLDoc(AXML: string): Integer;

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
            if SameText('categoryId', VarToStr(ChildNodes.Nodes[J].ChildNodes.Nodes['name'].NodeValue)) then
              categoryId := VarToStr(ChildNodes.Nodes[J].ChildNodes.Nodes['value'].ChildNodes.Nodes['string'].NodeValue)
            else if SameText('parentId', VarToStr(ChildNodes.Nodes[J].ChildNodes.Nodes['name'].NodeValue)) then
              parentId := VarToStr(ChildNodes.Nodes[J].ChildNodes.Nodes['value'].ChildNodes.Nodes['string'].NodeValue)
            else if SameText('categoryName', VarToStr(ChildNodes.Nodes[J].ChildNodes.Nodes['name'].NodeValue)) then
              categoryName := VarToStr(ChildNodes.Nodes[J].ChildNodes.Nodes['value'].ChildNodes.Nodes['string'].NodeValue);

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

  var
    XMLDoc: IXMLDocument;

    BoardLevel: TStringList;
  begin
    OleInitialize(nil);
    try
      XMLDoc := NewXMLDocument;
      with XMLDoc do
      begin
        LoadFromXML(AXML);
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

      XMLDoc := nil;
    finally
      OleUninitialize;
    end;
    Result := FCheckedIDsList.Count;
  end;

var
  IdHTTPHelper: TIdHTTPHelper;
  Params: TStringStream;
  ResponseStr: string;
begin
  Result := 0;
  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    IdHTTPHelper.HTTPOptions := IdHTTPHelper.HTTPOptions - [hoForceEncodeParams];
    IdHTTPHelper.Request.Referer := Website;
    IdHTTPHelper.RedirectMaximum := 1;

    LoadSettings;

    Params := TStringStream.Create('', CP_UTF8);
    try
      Params.WriteString(newPageXMLDoc);

      IdHTTPHelper.Request.CharSet := WordPressSettings.CharSet;
      try
        ResponseStr := IdHTTPHelper.Post(Website + 'xmlrpc.php', Params);
      except
        on E: Exception do
        begin
          ErrorMsg := E.message;
          Exit;
        end;
      end;

    finally
      Params.Free;
    end;
  finally
    IdHTTPHelper.Free;
  end;

  Result := resultPageXMLDoc(ResponseStr);
end;

function TWordPress.Exec;
var
  IdHTTPHelper: TIdHTTPHelper;

  ResponseStr: string;
begin
  Result := False;

  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    if LoadSettings(ComponentController) then
      if (not SameStr('', AccountName) and Login(IdHTTPHelper)) xor SameStr('', AccountName) then
      begin
        if not PrePostPage(IdHTTPHelper, ResponseStr) then
          Exit;

        Result := PostPage(IdHTTPHelper, ComponentController, MirrorController, ResponseStr);
      end;
  finally
    IdHTTPHelper.Free;
  end;
end;

function TWordPress.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TWordPressSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
