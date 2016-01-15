{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn content management system class              *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
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
  public
    constructor Create; override;
  published
    property use_plainlinks: Boolean read fuse_plainlinks write fuse_plainlinks;
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

{ TCMSFormbasedPlugInSettings }

constructor TCMSFormbasedPlugInSettings.Create;
begin
  inherited Create;

  // default setup
  use_plainlinks := False;
  use_textasdescription := False;
end;

{ TCMSFormbasedPlugIn }

function TCMSFormbasedPlugIn.CMSType: TCMSType;
begin
  Result := cmsFormbased;
end;

end.
