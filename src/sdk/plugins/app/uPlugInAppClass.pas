unit uPlugInAppClass;

interface

uses
  // Common
  uAppInterface,
  // Plugin
  uPlugInConst, uPlugInInterfaceAdv, uPlugInClass;

type
  TAppPlugIn = class(TPlugIn, IAppPlugIn)
  public
    function GetType: TPlugInType; override; safecall;

    function Start(const AAppController: IAppController): WordBool; virtual; safecall; abstract;
    procedure Stop; virtual; safecall; abstract;
  end;

implementation

{ TAppPlugIn }

function TAppPlugIn.GetType: TPlugInType;
begin
  Result := ptApp;
end;

end.
