unit uBlogger;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Controls, Variants,
  // Indy
  IdMultipartFormData,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInCMSBlogClass, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TBloggerSettings = class(TCMSBlogPlugInSettings)
  strict private
    fid: string;
  published
    [AttrDefaultValue('')]
    property id: string read fid write fid;

    property categorys;
  end;

  TBlogger = class(TCMSBlogPlugIn)
  public
    function GetName: WideString; override; safecall;
    function DefaultCharset: WideString; override;
    function BelongsTo(AWebsiteSourceCode: WideString): Boolean; override;
    function GetIDs: Integer; override;
    function Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean; override;
    function ShowWebsiteSettingsEditor(AWebsiteEditor: IWebsiteEditor): Boolean; override;
  end;

implementation

{ TBlogger }

function TBlogger.GetName: WideString;
begin
  Result := 'Blogger';
end;

function TBlogger.DefaultCharset: WideString;
begin
  Result := 'UTF-8';
end;

function TBlogger.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('content=''blogger'' name=''generator''', string(AWebsiteSourceCode)) > 0) or (Pos('name="generator" content="Blogger"',
      string(AWebsiteSourceCode)) > 0) or (Pos('content="Blogger" name="generator"', string(AWebsiteSourceCode)) > 0);
end;

function TBlogger.GetIDs: Integer;
begin
  //
end;

function TBlogger.Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean;
const
  AUTH = 'Auth=';
var
  Params, ReplyData: TStringStream;
  BloggerSettings: TBloggerSettings;
  _Auth: string;

(*
  function newPageXMLDoc: string;
  var
  XMLDoc: IXMLDocument;
  StringStream: TStringStream;
  _StringList: TStrings;
  I: Integer;
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
  DocumentElement := CreateElement('entry', 'http://www.w3.org/2005/Atom');

  with DocumentElement do
  begin
  with AddChild('title') do
  begin
  Attributes['type'] := 'text';
  NodeValue := Subject;
  end;

  with AddChild('content') do
  begin
  Attributes['type'] := 'xhtml';
  with AddChild('div') do
  begin
  SetAttributeNS('xmlns', '', 'http://www.w3.org/1999/xhtml');
  NodeValue := Self.Text;
  end;
  end;

  if (BloggerSettings.category_value <> '') then
  begin
  _StringList := SplittString(',', BloggerSettings.category_value);
  try
  for I := 0 to _StringList.Count - 1 do
  with AddChild('category') do
  begin
  SetAttributeNS('scheme', '', 'http://www.blogger.com/atom/ns#');
  Attributes['term'] := Trim(_StringList.Strings[I]);
  end;
  finally
  _StringList.Free;
  end;
  end;
  end;
  end;

  StringStream := TStringStream.Create;
  try
  XMLDoc.SaveToStream(StringStream);
  Result := StringStream.DataString;
  finally
  StringStream.Free;
  end;
  XMLDoc := nil;
  finally
  OleUninitialize;
  end;
  end;
  *)

  function newPageXMLDoc2: string;
  var
    _StringList: TStrings;
    I: Integer;
  begin
    with TStringlist.Create do
      try
        Add('<entry xmlns="http://www.w3.org/2005/Atom">');
        Add(#9 + '<title type=''text''>' + Subject + '</title>');
        Add(#9 + '<content type=''xhtml''>');
        Add('<div xmlns="http://www.w3.org/1999/xhtml">' + Message + '</div>');
        Add('</content>');
        if (BloggerSettings.categorys <> '') then
        begin
          _StringList := SplittString(',', BloggerSettings.categorys);
          try
            for I := 0 to _StringList.Count - 1 do
              Add(#9 + '<category scheme="http://www.blogger.com/atom/ns#" term="' + Trim(_StringList.Strings[I]) + '" />');
          finally
            _StringList.Free;
          end;
        end;
        Add('</entry>');
        Result := Text;
      finally
        Free;
      end;
  end;

begin
  (*
    <entry xmlns='http://www.w3.org/2005/Atom'>
    <title type='text'>Marriage!</title>
    <content type='xhtml'>
    <div xmlns="http://www.w3.org/1999/xhtml">
    <p>Mr. Darcy has <em>proposed marriage</em> to me!</p>
    <p>He is the last man on earth I would ever desire to marry.</p>
    <p>Whatever shall I do?</p>
    </div>
    </content>
    <category scheme="http://www.blogger.com/atom/ns#" term="marriage" />
    <category scheme="http://www.blogger.com/atom/ns#" term="Mr. Darcy" />
    </entry>
    *)

  Result := False;

  with TIdHTTPHelper.Create(Self) do
    try
      RedirectMaximum := 1;
      Request.Referer := Website;

      BloggerSettings := TBloggerSettings.Create;

      try
        with BloggerSettings do
        begin
          id := '';

          if id = '' then
          begin
            ErrorMsg := 'blogID is incorrect or undefinded!';
            Exit;
          end;
        end;
{$REGION 'Login'}
        if not(AccountName = '') then
        begin
          Params := TStringStream.Create('', CP_UTF8);
          ReplyData := TStringStream.Create('', CP_UTF8);
          try
            Params.WriteString('Email=' + AccountName + '&');
            Params.WriteString('Passwd=' + AccountPassword + '&');
            Params.WriteString('service=blogger&');
            Params.WriteString('accountType=GOOGLE&');
            Params.WriteString('source=SRK-IntelligeN-2');

            try
              Post('https://www.google.com/accounts/ClientLogin', Params, ReplyData);
            except
              on E: Exception do
              begin
                ErrorMsg := E.message;
                Exit;
              end;
            end;

            if ReplyData.DataString = '' then
            begin
              ErrorMsg := 'E-Mail-Address or Accountpassword is wrong.';
              Exit;
            end;

            _Auth := Trim(copy(ReplyData.DataString, Pos(AUTH, ReplyData.DataString) + length(AUTH)));
          finally
            ReplyData.Free;
            Params.Free;
          end;
        end;
{$ENDREGION}
        Request.ContentType := 'application/atom+xml';
        with Request.CustomHeaders do
        begin
          Add('GData-Version: 2');
          Add('Authorization: GoogleLogin auth=' + _Auth);
        end;

        Params := TStringStream.Create('', CP_UTF8);
        ReplyData := TStringStream.Create('', CP_UTF8);
        try
          Params.WriteString(newPageXMLDoc2);
          try
            Post('http://www.blogger.com/feeds/' + BloggerSettings.id + '/posts/default', Params, ReplyData);
          except
            on E: Exception do
            begin
              ErrorMsg := E.message;
              Exit;
            end;
          end;

          Result := ResponseCode = 201;
        finally
          ReplyData.Free;
          Params.Free;
        end;
      finally
        BloggerSettings.Free;
      end;
    finally
      Free;
    end;
end;

function TBlogger.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TBloggerSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
