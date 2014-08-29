unit uPlugInCMSClass;

interface

uses
  // Delphi
  Classes, Generics.Collections,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInInterface, uPlugInClass, uPlugInConst, uIdHTTPHelper;

type
  TCMSPlugInSettings = class(TPersistent)
  strict private
    FCharset: string;
  published
    property Charset: string read FCharset write FCharset;
  end;

  TCMSPlugIn = class(TPlugIn, ICMSPlugIn)
  private
    FAccountname, FAccountpassword, FSettingsFileName, FSubject, FTags, FMessage, FWebsite: WideString;
    FArticleID: Integer;
    FIntelligentPostingHelper: TIntelligentPostingHelper;
  protected
    FCheckedIDsList: TList<TIDInfo>;
    procedure AddID(AID, APath: string);
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; virtual; abstract;
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; virtual; abstract;
    function PostPage(AIdHTTPHelper: TIdHTTPHelper; AComponentController: IComponentController; AMirrorController: IMirrorController;
      APrevResponse: string = ''): Boolean; virtual; abstract;
  public
    constructor Create; override;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString);
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString);
    function GetSettingsFileName: WideString; safecall;
    procedure SetSettingsFileName(ASettingsFileName: WideString);
    function GetSubject: WideString; safecall;
    procedure SetSubject(ASubject: WideString);
    function GetTags: WideString; safecall;
    procedure SetTags(ATags: WideString);
    function GetMessage: WideString; safecall;
    procedure SetMessage(AMessage: WideString);
    function GetWebsite: WideString; safecall;
    procedure SetWebsite(AWebsite: WideString);

    function GetArticleID: Integer;
    procedure SetArticleID(AArticleID: Integer);

    function GetIntelligentPostingHelper: TIntelligentPostingHelper; safecall;
    procedure SetIntelligentPostingHelper(AIntelligentPostingHelper: TIntelligentPostingHelper);

    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;
    property SettingsFileName: WideString read GetSettingsFileName write SetSettingsFileName;
    property Subject: WideString read GetSubject write SetSubject;
    property Tags: WideString read GetTags write SetTags;
    property Message: WideString read GetMessage write SetMessage;
    property Website: WideString read GetWebsite write SetWebsite;

    property ArticleID: Integer read GetArticleID write SetArticleID;

    property IntelligentPostingHelper: TIntelligentPostingHelper read GetIntelligentPostingHelper;

    function CMSType: TCMSType; virtual; abstract;
    function DefaultCharset: WideString; virtual; abstract;
    function BelongsTo(AWebsiteSourceCode: WideString): Boolean; virtual; abstract;
    function GetIDs: Integer; virtual; abstract;
    function ReadID(AIndex: Integer): TIDInfo;
    function Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean; virtual; abstract;
    function ShowWebsiteSettingsEditor(AWebsiteEditor: IWebsiteEditor): Boolean; virtual; abstract;
    destructor Destroy; override;
  end;

  AttrTopValue = class(TCustomAttribute)

  end;

  AttrDefaultValue = class(TCustomAttribute)
  private
    FValue: Variant;
  public
    constructor Create(Value: string); overload;
    constructor Create(Value: Integer); overload;
    constructor Create(Value: Boolean); overload;
    property Value: Variant read FValue write FValue;
  end;

resourcestring
  StrAbortedThrougthCAP = 'Aborted througth CAPTCHA-Helper';

implementation

{ TCMSPlugIn }

procedure TCMSPlugIn.AddID(AID, APath: string);
var
  IDInfo: TIDInfo;
begin
  with IDInfo do
  begin
    ID := AID;
    Path := APath;
  end;

  FCheckedIDsList.Add(IDInfo);
end;

constructor TCMSPlugIn.Create;
begin
  inherited Create;
  FCheckedIDsList := TList<TIDInfo>.Create;
end;

function TCMSPlugIn.GetAccountName: WideString;
begin
  Result := FAccountname;
end;

procedure TCMSPlugIn.SetAccountName(AAccountName: WideString);
begin
  FAccountname := AAccountName;
end;

function TCMSPlugIn.GetAccountPassword: WideString;
begin
  Result := FAccountpassword;
end;

procedure TCMSPlugIn.SetAccountPassword(AAccountPassword: WideString);
begin
  FAccountpassword := AAccountPassword;
end;

function TCMSPlugIn.GetSettingsFileName: WideString;
begin
  Result := FSettingsFileName;
end;

procedure TCMSPlugIn.SetSettingsFileName(ASettingsFileName: WideString);
begin
  FSettingsFileName := ASettingsFileName;
end;

function TCMSPlugIn.GetSubject: WideString;
begin
  Result := FSubject;
end;

procedure TCMSPlugIn.SetSubject(ASubject: WideString);
begin
  FSubject := ASubject;
end;

function TCMSPlugIn.GetTags: WideString;
begin
  Result := FTags;
end;

procedure TCMSPlugIn.SetTags(ATags: WideString);
begin
  FTags := ATags;
end;

function TCMSPlugIn.GetMessage: WideString;
begin
  Result := FMessage;
end;

procedure TCMSPlugIn.SetMessage(AMessage: WideString);
begin
  FMessage := AMessage;
end;

function TCMSPlugIn.GetWebsite: WideString;
begin
  Result := FWebsite;
end;

procedure TCMSPlugIn.SetWebsite(AWebsite: WideString);
begin
  FWebsite := AWebsite;
end;

function TCMSPlugIn.GetArticleID: Integer;
begin
  Result := FArticleID;
end;

procedure TCMSPlugIn.SetArticleID(AArticleID: Integer);
begin
  FArticleID := AArticleID;
end;

function TCMSPlugIn.GetIntelligentPostingHelper;
begin
  Result := FIntelligentPostingHelper;
end;

procedure TCMSPlugIn.SetIntelligentPostingHelper(AIntelligentPostingHelper: TIntelligentPostingHelper);
begin
  FIntelligentPostingHelper := AIntelligentPostingHelper;
end;

function TCMSPlugIn.ReadID(AIndex: Integer): TIDInfo;
begin
  Result := FCheckedIDsList.Items[AIndex];
end;

destructor TCMSPlugIn.Destroy;
begin
  FCheckedIDsList.Free;
  inherited Destroy;
end;

{ AttrDefaultValue }

constructor AttrDefaultValue.Create(Value: string);
begin
  FValue := Value;
end;

constructor AttrDefaultValue.Create(Value: Integer);
begin
  FValue := Value;
end;

constructor AttrDefaultValue.Create(Value: Boolean);
begin
  FValue := Value;
end;

end.
