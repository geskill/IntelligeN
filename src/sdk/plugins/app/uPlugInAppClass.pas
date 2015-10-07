unit uPlugInAppClass;

interface

uses
  // Common
  uAppInterface,
  // Plugin
  uPlugInInterfaceAdv, uPlugInClass;

type
  TAppPlugIn = class(TPlugIn, IAppPlugIn)
  public
    function Start(const AAppController: IAppController): WordBool; virtual; safecall; abstract;
    procedure Stop; virtual; safecall; abstract;
  end;

implementation

end.
