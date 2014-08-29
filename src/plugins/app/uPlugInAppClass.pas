unit uPlugInAppClass;

interface

uses
  // Common
  uConst, uAppInterface,
  // Plugin
  uPlugInInterface, uPlugInClass;

type
  TAppPlugIn = class(TPlugIn, IAppPlugIn)
  public
    function Start(const AAppController: IAppController): Boolean; virtual; stdcall; abstract;
    procedure Stop; virtual; stdcall; abstract;
  end;

implementation

end.
