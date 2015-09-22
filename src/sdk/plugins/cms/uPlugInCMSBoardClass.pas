unit uPlugInCMSBoardClass;

interface

uses
  // Delphi
  SysUtils,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // Plugin system
  uPlugInConst, uPlugInCMSClass, uPlugInHTTPClasses;

type
  TCMSBoardPlugInSettings = class(TCMSPlugInSettings)
  strict private
    fforums, fthreads: Variant;
  published
    property forums: Variant read fforums write fforums;
    property threads: Variant read fthreads write fthreads;
  end;

  TCMSBoardPlugIn = class(TCMSPlugIn)
  private
    FPostReply: Boolean;
  protected
    property PostReply: Boolean read FPostReply write FPostReply;

    function _AfterLogin(var ARequestID: Double; out AResponseStr: string): Boolean; override;
  public
    property AccountName;
    property AccountPassword;
    property SettingsFileName;
    property Subject;
    property Tags;
    property Message;
    property Website;

    property ArticleID;

    function CMSType: TCMSType; override; safecall;
  end;

  TCMSBoardIPPlugInSettings = class(TCMSBoardPlugInSettings)
  strict private
    fintelligent_posting, fintelligent_posting_helper: Boolean;

    fprefix, ficon: Variant;
  published
    [AttrDefaultValue(False)]
    property intelligent_posting: Boolean read fintelligent_posting write fintelligent_posting;
    [AttrDefaultValue(False)]
    property intelligent_posting_helper: Boolean read fintelligent_posting_helper write fintelligent_posting_helper;

    property prefix: Variant read fprefix write fprefix;
    property icon: Variant read ficon write ficon;
  end;

  TCMSBoardIPPlugIn = class(TCMSBoardPlugIn)
  protected
    property PostReply;
    function GetSearchRequestURL: string; virtual;
    function IntelligentPosting(var ARequestID: Double): Boolean; virtual; abstract;
  public
    property AccountName;
    property AccountPassword;
    property SettingsFileName;
    property Subject;
    property Tags;
    property Message;
    property Website;

    property ArticleID;
  end;

resourcestring
  StrForumIdIsUndefine = 'forum id is undefined!';
  StrAbortedThrougthInt = 'Aborted througth intelligent_posting-Helper';

implementation

{ TCMSBoardPlugIn }

function TCMSBoardPlugIn._AfterLogin(var ARequestID: Double; out AResponseStr: string): Boolean;
const
  INTELLIGEN_POSTING_MISSING_LOGIN = 'intelligent_posting without login is not supported';
begin
  Result := True;
  if Self is TCMSBoardIPPlugIn then
    with Self as TCMSBoardIPPlugIn do
    begin
      if (ARequestID = -1) then
      begin
        ErrorMsg := INTELLIGEN_POSTING_MISSING_LOGIN;
        Result := False;
      end;

      if (Settings as TCMSBoardIPPlugInSettings).intelligent_posting and not IntelligentPosting(ARequestID) then
        Result := False;
    end;
end;

function TCMSBoardPlugIn.CMSType;
begin
  Result := cmsBoard;
end;

{ TCMSBoardIPPlugIn }

function TCMSBoardIPPlugIn.GetSearchRequestURL: string;
begin
  Result := GetIDsRequestURL;
end;

end.
