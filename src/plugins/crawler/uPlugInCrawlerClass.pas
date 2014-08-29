unit uPlugInCrawlerClass;

interface

uses
  // Common
  uAppInterface,
  // Plugin
  uPlugInInterface, uPlugInClass;

type
  TCrawlerPlugIn = class(TPlugIn, ICrawlerPlugIn)
  private
    FUseAccount:Boolean;
    FAccountname, FAccountpassword: WideString;
  public
    function GetUseAccount: Boolean;
    procedure SetUseAccount(AUseAccount: Boolean);
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString);
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString);

    function GetAvailableTemplateTypeIDs: Integer; virtual; stdcall; abstract;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; virtual; stdcall; abstract;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean; virtual; stdcall;
      abstract;
    function GetLimitDefaultValue: Integer; virtual; stdcall; abstract;

    property UseAccount: Boolean read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer;
      const AComponentController: IComponentController); virtual; stdcall; abstract;
  end;

implementation

function TCrawlerPlugIn.GetUseAccount: Boolean;
begin
  Result := FUseAccount;
end;

procedure TCrawlerPlugIn.SetUseAccount(AUseAccount: Boolean);
begin
  FUseAccount := AUseAccount;
end;

function TCrawlerPlugIn.GetAccountName: WideString;
begin
  Result := FAccountname;
end;

procedure TCrawlerPlugIn.SetAccountName(AAccountName: WideString);
begin
  FAccountname := AAccountName;
end;

function TCrawlerPlugIn.GetAccountPassword: WideString;
begin
  Result := FAccountpassword;
end;

procedure TCrawlerPlugIn.SetAccountPassword(AAccountPassword: WideString);
begin
  FAccountpassword := AAccountPassword;
end;

end.
