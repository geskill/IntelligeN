{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn base class                                   *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInClass;

interface

uses
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin
  uPlugInInterface, uPlugInConst;

type
  TPlugIn = class(TInterfacedObject, IPlugIn)
  private
    FCAPTCHAInput: TCAPTCHAInput;
    FProxy: IProxy;
    FConnectTimeout, FReadTimeout: Integer;
    FHTTPManager: IHTTPManager;
    FErrorMsg: WideString;
  protected
    function GetCAPTCHAInput: TCAPTCHAInput; safecall;
    procedure SetCAPTCHAInput(ACAPTCHAInput: TCAPTCHAInput); safecall;
    function GetHTTPManager: IHTTPManager; safecall;
    procedure SetHTTPManager(AHTTPManager: IHTTPManager); safecall;
    function GetProxy: IProxy; safecall;
    procedure SetProxy(AProxy: IProxy); safecall;
    function GetConnectTimeout: Integer; safecall;
    procedure SetConnectTimeout(AConnectTimeout: Integer); safecall;
    function GetReadTimeout: Integer; safecall;
    procedure SetReadTimeout(AReadTimeout: Integer); safecall;
    function GetErrorMsg: WideString; safecall;
    procedure SetErrorMsg(const AErrorMsg: WideString); safecall;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetName: WideString; virtual; safecall; abstract;
    function GetType: TPlugInType; virtual; safecall; abstract;
    property CAPTCHAInput: TCAPTCHAInput read GetCAPTCHAInput;

    property HTTPManager: IHTTPManager read GetHTTPManager;
    property Proxy: IProxy read GetProxy write SetProxy;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property ErrorMsg: WideString read GetErrorMsg write SetErrorMsg;
  end;

implementation

{ TPlugIn }

function TPlugIn.GetCAPTCHAInput;
begin
  Result := FCAPTCHAInput;
end;

procedure TPlugIn.SetCAPTCHAInput(ACAPTCHAInput: TCAPTCHAInput);
begin
  FCAPTCHAInput := ACAPTCHAInput;
end;

function TPlugIn.GetHTTPManager: IHTTPManager;
begin
  Result := FHTTPManager;
end;

procedure TPlugIn.SetHTTPManager(AHTTPManager: IHTTPManager);
begin
  FHTTPManager := AHTTPManager;
end;

function TPlugIn.GetProxy: IProxy;
begin
  Result := FProxy;
end;

procedure TPlugIn.SetProxy(AProxy: IProxy);
begin
  FProxy := AProxy;
end;

function TPlugIn.GetConnectTimeout: Integer;
begin
  Result := FConnectTimeout;
end;

procedure TPlugIn.SetConnectTimeout(AConnectTimeout: Integer);
begin
  FConnectTimeout := AConnectTimeout;
end;

function TPlugIn.GetReadTimeout: Integer;
begin
  Result := FReadTimeout;
end;

procedure TPlugIn.SetReadTimeout(AReadTimeout: Integer);
begin
  FReadTimeout := AReadTimeout;
end;

function TPlugIn.GetErrorMsg: WideString;
begin
  Result := FErrorMsg;
end;

procedure TPlugIn.SetErrorMsg(const AErrorMsg: WideString);
begin
  FErrorMsg := AErrorMsg;
end;

constructor TPlugIn.Create;
begin
  inherited Create;
  FProxy := TProxy.Create;
end;

destructor TPlugIn.Destroy;
begin
  FProxy := nil;
  FHTTPManager := nil;
  inherited Destroy;
end;

end.
