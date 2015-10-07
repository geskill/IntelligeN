unit uPlugInCMSFormbasedClass;

interface

uses
  // Delphi
  SysUtils,
  // Common
  uBaseConst, uBaseInterface,
  // Plugin system
  uPlugInConst, uPlugInCMSClass, uPlugInCMSBlogClass;

type
  TCMSFormbasedPlugInSettings = class(TCMSBlogPlugInSettings)
  strict private
    fuse_plainlinks, fuse_textasdescription: Boolean;
  published
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

    function CMSType: TCMSType; override; safecall;
  end;

implementation

{ TCMSFormbasedPlugIn }

function TCMSFormbasedPlugIn.CMSType: TCMSType;
begin
  Result := cmsFormbased;
end;

end.
