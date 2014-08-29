unit uPlugInCMSBoardClass;

interface

uses
  // Delphi
  SysUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInConst, uPlugInCMSClass, uIdHTTPHelper;

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
    function PrePostPage(AIdHTTPHelper: TIdHTTPHelper; out AResponse: string): Boolean; virtual; abstract;
  public
    property AccountName;
    property AccountPassword;
    property SettingsFileName;
    property Subject;
    property Tags;
    property Message;
    property Website;

    property ArticleID;

    function CMSType: TCMSType; override;

    function Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean; override;
  end;

  TCMSBoardIPPlugIn = class(TCMSBoardPlugIn)
  protected
    property PostReply;
    function IntelligentPosting(AIdHTTPHelper: TIdHTTPHelper): Boolean; virtual; abstract;
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

function TCMSBoardPlugIn.CMSType;
begin
  Result := cmsBoard;
end;

function TCMSBoardPlugIn.Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean;
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
        if Self is TCMSBoardIPPlugIn then
          with Self as TCMSBoardIPPlugIn do
            if not IntelligentPosting(IdHTTPHelper) then
              Exit;

        if not PrePostPage(IdHTTPHelper, ResponseStr) then
          Exit;

        Result := PostPage(IdHTTPHelper, ComponentController, MirrorController, ResponseStr);
      end;
  finally
    IdHTTPHelper.Free;
  end;
end;

end.
