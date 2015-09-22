unit dxPopupProxy;

interface

uses Classes, SysUtils, Controls, Menus, dxBar;

Type
  TdxPopupProxy = class(TPopupMenu)
  private
    FProxyMenu: TdxBarPopupMenu;
    procedure SetProxyMenu(const Value: TdxBarPopupMenu);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure Popup(X, Y: Integer); override;
  published
    property ProxyMenu: TdxBarPopupMenu read FProxyMenu write SetProxyMenu;
  end;

implementation

{ TdxPopupProxy }

procedure TdxPopupProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProxyMenu) then
    ProxyMenu := nil;
end;

procedure TdxPopupProxy.Popup(X, Y: Integer);
begin
  if (ProxyMenu <> nil) then
    ProxyMenu.Popup(X, Y);
end;

procedure TdxPopupProxy.SetProxyMenu(const Value: TdxBarPopupMenu);
begin
  FProxyMenu := Value;
  if (FProxyMenu <> nil) then
    FProxyMenu.FreeNotification(Self);
end;

end.
