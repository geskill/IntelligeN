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
  // Delphi
  Windows,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInConst, uPlugInInterface, uPlugInClass, uPlugInHTTPClasses,
  // Utils
  uURLUtils;

type
  TCrawlerCanUseFunc = reference to function(AControlID: TControlID): WordBool;

  TCrawlerPlugIn = class(TPlugIn, ICrawlerPlugIn)
  private
    FUseAccount: WordBool;
    FAccountname, FAccountpassword: WideString;
  protected
    function GetUseAccount: WordBool; safecall;
    procedure SetUseAccount(AUseAccount: WordBool); safecall;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(const AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(const AAccountPassword: WideString); safecall;

    function InternalGetAvailableTypeIDs: TTypeIDs; virtual; safecall;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; virtual; safecall;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; virtual; safecall;
    function InternalGetDependentControlIDs: TControlIDs; virtual; safecall;

    function GETRequest(const AURL: string; out ARequestID: Double; AHTTPOptions: IHTTPOptions = nil): string;
    function GETFollowUpRequest(const AURL: string; AFollowUp: Double; out ARequestID: Double; AHTTPOptions: IHTTPOptions = nil): string;
    function SimpleGETRequest(const AURL: string; AHTTPOptions: IHTTPOptions = nil): string;

    function InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; virtual; safecall; abstract;
  public
    function GetType: TPlugInType; override; safecall;

    function GetAvailableTypeIDs: Integer; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; safecall;
    function GetDependentControlIDs: Integer; safecall;
    function GetResultsLimitDefaultValue: Integer; virtual; safecall;

    property UseAccount: WordBool read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; safecall;
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

procedure TCrawlerPlugIn.SetAccountName(const AAccountName: WideString);
begin
  FAccountname := AAccountName;
end;

function TCrawlerPlugIn.GetAccountPassword: WideString;
begin
  Result := FAccountpassword;
end;

procedure TCrawlerPlugIn.SetAccountPassword(const AAccountPassword: WideString);
begin
  FAccountpassword := AAccountPassword;
end;

function TCrawlerPlugIn.InternalGetAvailableTypeIDs: TTypeIDs;
begin
  Result := [low(TTypeID) .. high(TTypeID)];
end;

function TCrawlerPlugIn.InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs;
begin
  Result := [low(TControlID) .. high(TControlID)];
end;

function TCrawlerPlugIn.InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool;
begin
  Result := True;
end;

function TCrawlerPlugIn.InternalGetDependentControlIDs: TControlIDs;
begin
  Result := [];
end;

function TCrawlerPlugIn.GETRequest(const AURL: string; out ARequestID: Double; AHTTPOptions: IHTTPOptions = nil): string;
var
  LHTTPRequest: IHTTPRequest;
  LHTTPOptions: IHTTPOptions;
  LRequestID: Double;
begin
  Result := '';

  LHTTPRequest := THTTPRequest.Create(AURL);
  with LHTTPRequest do
  begin
    Referer := ExtractUrlPath(AURL);
  end;

  if not Assigned(AHTTPOptions) then
    LHTTPOptions := TPlugInHTTPOptions.Create(Self)
  else
    LHTTPOptions := AHTTPOptions;

  LRequestID := HTTPManager.Get(LHTTPRequest, LHTTPOptions);

  HTTPManager.WaitFor(LRequestID);

  ARequestID := LRequestID;
  Result := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;
end;

function TCrawlerPlugIn.GETFollowUpRequest(const AURL: string; AFollowUp: Double; out ARequestID: Double; AHTTPOptions: IHTTPOptions = nil): string;
var
  LHTTPOptions: IHTTPOptions;
  LRequestID: Double;
begin
  Result := '';

  if not Assigned(AHTTPOptions) then
    LHTTPOptions := TPlugInHTTPOptions.Create(Self)
  else
    LHTTPOptions := AHTTPOptions;

  LRequestID := HTTPManager.Get(AURL, AFollowUp, LHTTPOptions);

  HTTPManager.WaitFor(LRequestID);

  ARequestID := LRequestID;
  Result := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;
end;

function TCrawlerPlugIn.SimpleGETRequest(const AURL: string; AHTTPOptions: IHTTPOptions = nil): string;
var
  LRequestID: Double;
begin
  Result := GETRequest(AURL, LRequestID, AHTTPOptions);
end;

function TCrawlerPlugIn.GetType: TPlugInType;
begin
  Result := ptCrawler;
end;

function TCrawlerPlugIn.GetAvailableTypeIDs: Integer;
var
  LTypeIDs: TTypeIDs;
begin
  LTypeIDs := InternalGetAvailableTypeIDs();
  Result := Word(LTypeIDs);
end;

function TCrawlerPlugIn.GetAvailableControlIDs(const ATypeID: Integer): Integer;
var
  LControlIDs: TControlIDs;
begin
  LControlIDs := InternalGetAvailableControlIDs(TTypeID(ATypeID));
  Result := LongWord(LControlIDs);
end;

function TCrawlerPlugIn.GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool;
begin
  Result := InternalGetControlIDDefaultValue(TTypeID(ATypeID), TControlID(AControlID));
end;

function TCrawlerPlugIn.GetDependentControlIDs: Integer;
var
  LControlIDs: TControlIDs;
begin
  LControlIDs := InternalGetDependentControlIDs();
  Result := LongWord(LControlIDs);
end;

function TCrawlerPlugIn.GetResultsLimitDefaultValue: Integer;
begin
  Result := 5;
end;

function TCrawlerPlugIn.Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool;
var
  LControlIDs: TControlIDs;
begin
  LongWord(LControlIDs) := AControlIDs;
  Result := InternalExecute(TTypeID(ATypeID), LControlIDs, ALimit, AControlController, { }
    { } function(AControlID: TControlID): WordBool
    { } begin
    { . } Result := Assigned(AControlController.FindControl(AControlID)) and (AControlID in LControlIDs);
    { } end);
end;

end.
