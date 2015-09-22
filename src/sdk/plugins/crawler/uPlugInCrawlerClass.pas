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
    FUseAccount: WordBool;
    FAccountname, FAccountpassword: WideString;
  public
    function GetUseAccount: WordBool; safecall;
    procedure SetUseAccount(AUseAccount: WordBool); safecall;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString); safecall;

    function GetAvailableTemplateTypeIDs: Integer; virtual; safecall; abstract;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; virtual; safecall; abstract;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; virtual; safecall; abstract;
    function GetLimitDefaultValue: Integer; virtual; safecall; abstract;

    property UseAccount: WordBool read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); virtual; safecall; abstract;
  end;

implementation

function TCrawlerPlugIn.GetUseAccount: WordBool;
begin
  Result := FUseAccount;
end;

procedure TCrawlerPlugIn.SetUseAccount(AUseAccount: WordBool);
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
