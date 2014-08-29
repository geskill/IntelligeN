{ ********************************************************
  *                                                      *
  *  IntelligeN 2009 NFO Stripper                        *
  *  Version 1.0.0.0                                     *
  *  Copyright (c) 2014 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uNfoStripper;

interface

uses
  // Delphi
  Character, Windows, SysUtils, StrUtils, Classes, Dialogs, Menus, HTTPApp, IniFiles, Generics.Collections, Math,
  // NFOHelper
  uNFOHelper,
  // Common
  uConst, uAppInterface,
  // Utils
  uPathUtils, uSpecialStringUtils,
  // Plugin system
  uPlugInAppClass, uIdHTTPHelper;

type
  TNotifyMethod = procedure(const Sender: IUnknown) of object;

  TINotifyEvent = class(TInterfacedObject, INotifyEvent)
  private
    FOnNotify: TNotifyMethod;
  public
    property OnNotifyHandler: TNotifyMethod read FOnNotify write FOnNotify;
    procedure OnNotify(const Sender: IUnknown); stdcall;
  end;

  TNfoStripper = class(TAppPlugIn)
  private
    FAppController: IAppController;
    FNewMenuItemActive, FNewMenuItemAll: IMenuItem;
    FNotifyEventActive, FNotifyEventAll: TINotifyEvent;
    procedure DoTab(const ATabSheetController: ITabSheetController);
    procedure OnClickActive(const Sender: IUnknown);
    procedure OnClickAll(const Sender: IUnknown);
  public
    function GetName: WideString; override; safecall;
    function Start(const AAppController: IAppController): Boolean; override; stdcall;
    procedure Stop; override; stdcall;
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

procedure TINotifyEvent.OnNotify;
begin
  if (@FOnNotify <> nil) then
    FOnNotify(Sender);
end;

{ TNfoStripper }

procedure TNfoStripper.OnClickActive(const Sender: IInterface);
begin
  with FAppController.PageController do
  begin
    if (TabSheetCount > 0) then
      DoTab(ActiveTabSheetController)
    else
      MessageDlg('First of all add a new tab!', mtWarning, [mbOK], 0);
  end;
end;

procedure TNfoStripper.OnClickAll(const Sender: IInterface);
var
  I: Integer;
begin
  with FAppController.PageController do
    for I := 0 to TabSheetCount - 1 do
      DoTab(TabSheetController[I]);
end;

procedure TNfoStripper.DoTab(const ATabSheetController: ITabSheetController);
var
  nfo_control: IBasic;
  nfo_value: string;
begin
  nfo_control := ATabSheetController.ComponentController.FindControl(cNFO);

  if Assigned(nfo_control) then
  begin
    nfo_value := nfo_control.Value;

    with TNFOHelper.Create do
      try
        nfo_control.Value := AsStrippedText(nfo_value);
      finally
        Free;
      end;
  end;
end;

function TNfoStripper.GetName: WideString;
begin
  Result := 'NFO Stripper';
end;

function TNfoStripper.Start(const AAppController: IAppController): Boolean;
begin
  FAppController := AAppController;

  Result := True;

  FNotifyEventActive := TINotifyEvent.Create;
  FNotifyEventActive.OnNotifyHandler := OnClickActive;
  with FAppController.MainMenu.GetMenuItems.GetItem(3) do
    FNewMenuItemActive := InsertMenuItem(GetMenuItems.GetCount, 'NFO Stripper (active)', 'This stripp''s the NFO-field', 0, -1, 0, FNotifyEventActive);

  FNotifyEventAll := TINotifyEvent.Create;
  FNotifyEventAll.OnNotifyHandler := OnClickAll;
  with FAppController.MainMenu.GetMenuItems.GetItem(3) do
    FNewMenuItemAll := InsertMenuItem(GetMenuItems.GetCount, 'NFO Stripper (all)', 'This stripp''s all the NFO-fields', 0, -1, 0, FNotifyEventAll);
end;

procedure TNfoStripper.Stop;
begin
  FAppController.MainMenu.GetMenuItems.GetItem(3).GetMenuItems.RemoveItem(FNewMenuItemAll);
  FNewMenuItemAll := nil;

  FAppController.MainMenu.GetMenuItems.GetItem(3).GetMenuItems.RemoveItem(FNewMenuItemActive);
  FNotifyEventActive := nil;
end;

end.
