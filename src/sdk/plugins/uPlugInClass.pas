{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn base class                                   *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInClass;

interface

uses
  // Delphi
  ComObj,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin
  uPlugInInterface, uPlugInConst;

type
  TPlugIn = class(TInterfacedObject, IPlugIn)
  private
    FHTTPManager: IHTTPManager;
    FProxy: IProxy;
    FConnectTimeout, FReadTimeout: Integer;

    FErrorMsg: WideString;
  protected
    function GetHTTPManager: IHTTPManager; safecall;
    function GetProxy: IProxy; safecall;
    function GetConnectTimeout: Integer; safecall;
    function GetReadTimeout: Integer; safecall;

    function GetErrorMsg: WideString; safecall;
    procedure SetErrorMsg(const AErrorMsg: WideString); safecall;
  public
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
    procedure Initialize(const AHTTPManager: IHTTPManager; const AProxy: IProxy; const AConnectTimeout, AReadTimeout: Integer); safecall;
    procedure Uninitialize(); safecall;

    constructor Create; virtual;
    destructor Destroy; override;
  public
    function GetAuthor: WideString; virtual; safecall; abstract;
    function GetAuthorURL: WideString; virtual; safecall; abstract;
    function GetDescription: WideString; virtual; safecall; abstract;
    function GetName: WideString; virtual; safecall; abstract;
    function GetType: TPlugInType; virtual; safecall; abstract;

    property HTTPManager: IHTTPManager read GetHTTPManager;
    property Proxy: IProxy read GetProxy;
    property ConnectTimeout: Integer read GetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout;

    property ErrorMsg: WideString read GetErrorMsg write SetErrorMsg;
  end;

implementation

{ TPlugIn }

function TPlugIn.GetHTTPManager;
begin
  Result := FHTTPManager;
end;

function TPlugIn.GetProxy;
begin
  Result := FProxy;
end;

function TPlugIn.GetConnectTimeout;
begin
  Result := FConnectTimeout;
end;

function TPlugIn.GetReadTimeout;
begin
  Result := FReadTimeout;
end;

function TPlugIn.GetErrorMsg;
begin
  Result := FErrorMsg;
end;

procedure TPlugIn.SetErrorMsg;
begin
  FErrorMsg := AErrorMsg;
end;

function TPlugIn.SafeCallException;
begin
  Result := ComObj.HandleSafeCallException(ExceptObject, ExceptAddr, IUnknown, '', '');
end;

procedure TPlugIn.Initialize;
begin
  FHTTPManager := AHTTPManager;
  FProxy := AProxy;
  FConnectTimeout := AConnectTimeout;
  FReadTimeout := AReadTimeout;
end;

procedure TPlugIn.Uninitialize;
begin
  FProxy := nil;
  FHTTPManager := nil;
end;

constructor TPlugIn.Create;
begin
  inherited Create;
  FProxy := TProxy.Create;
end;

destructor TPlugIn.Destroy;
begin
  Uninitialize;
  inherited Destroy;
end;

end.
