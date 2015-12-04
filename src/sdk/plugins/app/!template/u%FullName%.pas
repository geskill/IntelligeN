unit u%FullName%;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants,
  // RegEx
  RegExpr,
  // LkJSON
  uLkJSON,
  // MultiEvent
  Generics.MultiEvents.NotifyHandler,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Plugin system
  uPlugInAppClass, uPlugInHTTPClasses,
  // Utils,
  uPathUtils, uStringUtils, uURLUtils;

type
  T%FullName% = class(TAppPlugIn)
  private
    FAppController: IAppController;
    FNewMenuItem: IMenuItem;
    FNotifyEventHandler: TINotifyEventHandler;
    procedure OnClick(const Sender: IUnknown);
  public
    function GetName: WideString; override;
    function Start(const AAppController: IAppController): WordBool; override;
    function Stop: WordBool; override;
  end;

implementation

function GetModulePath: string;
var
  QueryRes: TMemoryBasicInformation;
  LBuffer: string;
begin
  VirtualQuery(@GetModulePath, QueryRes, SizeOf(QueryRes));
  SetLength(LBuffer, MAX_PATH);
  SetLength(LBuffer, GetModuleFileName(Cardinal(QueryRes.AllocationBase), PChar(LBuffer), Length(LBuffer)));
  Result := LBuffer;
end;

{ T%FullName% }

procedure T%FullName%.OnClick(const Sender: IInterface);
begin
  with FAppController.PageController do
  begin
    { TODO : add your code for your menu item action here }
  end;
end;

function T%FullName%.GetName: WideString;
begin
  Result := '%FullName%';
end;

function T%FullName%.Start(const AAppController: IAppController): WordBool;
begin
  FAppController := AAppController;

  { TODO : add menu items here }
  FNotifyEventHandler := TINotifyEventHandler.Create(OnClick);
  with FAppController.MainMenu.GetMenuItems.GetItem(3) do
    FNewMenuItem := InsertMenuItem(GetMenuItems.GetCount, '%FullName%', '%FullName%', 0, -1, 0, FNotifyEventHandler);
	
  Result := True;
end;

function T%FullName%.Stop: WordBool;
begin
  { TODO : dont forget to remove the menu items here }
  FAppController.MainMenu.GetMenuItems.GetItem(3).GetMenuItems.RemoveItem(FNewMenuItem);
  FNotifyEventHandler := nil;
  
  Result := True;
end;

end.
