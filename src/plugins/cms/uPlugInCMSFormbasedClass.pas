unit uPlugInCMSFormbasedClass;

interface

uses
  // Delphi
  SysUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInConst, uPlugInCMSClass, uPlugInCMSBlogClass, uIdHTTPHelper;

type
  TCMSFormbasedPlugInSettings = class(TCMSBlogPlugInSettings)
  strict private
    fhoster_blacklist: string;
    fuse_plainlinks, fuse_textasdescription: Boolean;
  published
    [AttrDefaultValue('')]
    property hoster_blacklist: string read fhoster_blacklist write fhoster_blacklist;

    [AttrDefaultValue(False)]
    property use_plainlinks: Boolean read fuse_plainlinks write fuse_plainlinks;
    [AttrDefaultValue(False)]
    property use_textasdescription: Boolean read fuse_textasdescription write fuse_textasdescription;
  end;

  TCMSFormbasedPlugIn = class(TCMSPlugIn)
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

implementation

{ TCMSFormbasedPlugIn }

function TCMSFormbasedPlugIn.CMSType: TCMSType;
begin
  Result := cmsFormbased;
end;

function TCMSFormbasedPlugIn.Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean;
var
  IdHTTPHelper: TIdHTTPHelper;
begin
  Result := False;

  IdHTTPHelper := TIdHTTPHelper.Create(Self);
  try
    if LoadSettings(ComponentController) then
      if (not SameStr('', AccountName) and Login(IdHTTPHelper)) xor SameStr('', AccountName) then
        Result := PostPage(IdHTTPHelper, ComponentController, MirrorController);
  finally
    IdHTTPHelper.Free;
  end;
end;

end.
