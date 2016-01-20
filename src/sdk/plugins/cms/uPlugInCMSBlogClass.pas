{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn content management system class              *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInCMSBlogClass;

interface

uses
  // Delphi
  Variants,
  // Common
  uBaseConst, uBaseInterface,
  // Plugin system
  uPlugInConst, uPlugInCMSClass;

type
  TCMSBlogPlugInSettings = class(TCMSPlugInSettings)
  strict private
    fcategorys: Variant;
  published
    property categorys: Variant read fcategorys write fcategorys;
  end;

  TCMSBlogPlugIn = class(TCMSPlugIn)
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

{ TCMSBlogPlugIn }

function TCMSBlogPlugIn.CMSType: TCMSType;
begin
  Result := cmsBlog;
end;

end.
