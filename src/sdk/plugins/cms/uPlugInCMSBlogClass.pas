{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn content management system class              *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
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
    fcategories: Variant;
  published
    property categorys: Variant read fcategories write fcategories;
  end;

  TCMSBlogPlugIn = class(TCMSPlugIn)
  public
    function GetCMSType: TCMSType; override; safecall;
  end;

implementation

{ TCMSBlogPlugIn }

function TCMSBlogPlugIn.GetCMSType: TCMSType;
begin
  Result := cmsBlog;
end;

end.
