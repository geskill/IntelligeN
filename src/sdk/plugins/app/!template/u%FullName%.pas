unit u%FullName%;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants,
  // RegEx
  RegExpr,
  // MultiEvent
  Generics.MultiEvents.NotifyHandler,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Plugin system
  uPlugInAppClass, uPlugInEvent, uPlugInHTTPClasses,
  // Utils,
  uPathUtils, uStringUtils, uSystemUtils, uURLUtils;

type
  T%FullName% = class(TAppPlugIn)
  private
    FAppController: IAppController;
    FNewMenuItem: IMenuItem;
    FNotifyEventHandler: TINotifyEventHandler;
    procedure OnClick(const Sender: IUnknown);
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function Start(const AAppController: IAppController): WordBool; override;
    function Stop: WordBool; override;
  end;

implementation

{ T%FullName% }

procedure T%FullName%.OnClick(const Sender: IInterface);
begin
  with FAppController.PageController do
  begin
    { TODO : add your code for your menu item action here }
  end;
end;

function T%FullName%.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function T%FullName%.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function T%FullName%.GetDescription;
begin
  Result := GetName + ' app plug-in.';
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
