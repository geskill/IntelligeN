{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn crawler class                                *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInCrawlerClass;

interface

uses
  // Common
  uBaseInterface,
  // Plugin
  uPlugInConst, uPlugInInterface, uPlugInClass;

type
  TCrawlerPlugIn = class(TPlugIn, ICrawlerPlugIn)
  private
    FUseAccount: WordBool;
    FAccountname, FAccountpassword: WideString;
  protected
    function GetUseAccount: WordBool; safecall;
    procedure SetUseAccount(AUseAccount: WordBool); safecall;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString); safecall;
  public
    function GetType: TPlugInType; override; safecall;

    function GetAvailableTypeIDs: Integer; virtual; safecall; abstract;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; virtual; safecall; abstract;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; virtual; safecall; abstract;
    function GetResultsLimitDefaultValue: Integer; virtual; safecall; abstract;

    property UseAccount: WordBool read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; virtual; safecall; abstract;
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

function TCrawlerPlugIn.GetType: TPlugInType;
begin
  Result := ptCrawler;
end;

end.
