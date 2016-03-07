{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn crawler class                                *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
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
  protected
    function InternalGetAvailableTypeIDs: TTypeIDs; virtual; safecall;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; virtual; safecall;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; virtual; safecall;
    function InternalGetDependentControlIDs: TControlIDs; virtual; safecall;
    function InternalGetRetrieveData(const AAccountData: IAccountData; const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; virtual; safecall; abstract;

    function GETRequest(const AURL: string; out ARequestID: Double; AHTTPOptions: IHTTPOptions = nil): string;
    function GETFollowUpRequest(const AURL: string; AFollowUp: Double; out ARequestID: Double; AHTTPOptions: IHTTPOptions = nil): string;
    function SimpleGETRequest(const AURL: string; AHTTPOptions: IHTTPOptions = nil): string;
  public
    function GetType: TPlugInType; override; safecall;

    function GetAvailableTypeIDs: Integer; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; safecall;
    function GetDependentControlIDs: Integer; safecall;
    function GetResultsLimitDefaultValue: Integer; virtual; safecall;

    function GetRetrieveData(const AAccountData: IAccountData; const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; safecall;
  end;

implementation

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

function TCrawlerPlugIn.GetType;
begin
  Result := ptCrawler;
end;

function TCrawlerPlugIn.GetAvailableTypeIDs;
var
  LTypeIDs: TTypeIDs;
begin
  LTypeIDs := InternalGetAvailableTypeIDs();
  Result := Word(LTypeIDs);
end;

function TCrawlerPlugIn.GetAvailableControlIDs;
var
  LControlIDs: TControlIDs;
begin
  LControlIDs := InternalGetAvailableControlIDs(TTypeID(ATypeID));
  Result := LongWord(LControlIDs);
end;

function TCrawlerPlugIn.GetControlIDDefaultValue;
begin
  Result := InternalGetControlIDDefaultValue(TTypeID(ATypeID), TControlID(AControlID));
end;

function TCrawlerPlugIn.GetDependentControlIDs;
var
  LControlIDs: TControlIDs;
begin
  LControlIDs := InternalGetDependentControlIDs();
  Result := LongWord(LControlIDs);
end;

function TCrawlerPlugIn.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

function TCrawlerPlugIn.GetRetrieveData;
var
  LControlIDs: TControlIDs;
begin
  LongWord(LControlIDs) := AControlIDs;
  Result := InternalGetRetrieveData(AAccountData, TTypeID(ATypeID), LControlIDs, ALimit, AControlController, { }
    { } function(AControlID: TControlID): WordBool
    { } begin
    { . } Result := Assigned(AControlController.FindControl(AControlID)) and (AControlID in LControlIDs);
    { } end);
end;

end.
