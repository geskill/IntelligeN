unit uPlugInClass;

interface

uses
  // Plugin
  uPlugInInterface, uPlugInConst;

type
  TProxy = class(TInterfacedObject, IProxy)
  private
    FActive: Boolean;
    FType: TProxyType;
    FPort: Integer;
    FServer, FAccountName, FAccountPassword: string;
  protected
    function GetActive: Boolean;
    function GetType: TProxyType;
    function GetServer: WideString; safecall;
    function GetPort: Integer;
    function GetAccountName: WideString; safecall;
    function GetAccountPassword: WideString; safecall;
  public
    constructor Create;
    procedure Activate(AType: TProxyType; AServer: WideString; APort: Integer; AAccountName, AAccountPassword: WideString);
    property Active: Boolean read GetActive;
    property ServerType: TProxyType read GetType;
    property Server: WideString read GetServer;
    property Port: Integer read GetPort;
    property AccountName: WideString read GetAccountName;
    property AccountPassword: WideString read GetAccountPassword;
  end;

  TPlugIn = class(TInterfacedObject, IPlugIn)
  protected
    FCAPTCHAInput: TCAPTCHAInput;
    FProxy: IProxy;
    FConnectTimeout, FReadTimeout: Integer;
    FErrorMsg: WideString;
    function GetCAPTCHAInput: TCAPTCHAInput; safecall;
    procedure SetCAPTCHAInput(ACAPTCHAInput: TCAPTCHAInput);
    function GetProxy: IProxy; safecall;
    procedure SetProxy(AProxy: IProxy);
    function GetConnectTimeout: Integer;
    procedure SetConnectTimeout(AConnectTimeout: Integer); safecall;
    function GetReadTimeout: Integer;
    procedure SetReadTimeout(AReadTimeout: Integer); safecall;
    function GetErrorMsg: WideString; safecall;
    procedure SetErrorMsg(AErrorMsg: WideString);
  public
    constructor Create; virtual;
    function GetName: WideString; virtual; safecall; abstract;
    property CAPTCHAInput: TCAPTCHAInput read GetCAPTCHAInput;

    property Proxy: IProxy read GetProxy write SetProxy;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property ErrorMsg: WideString read GetErrorMsg write SetErrorMsg;
  end;

implementation

{ TProxy }

constructor TProxy.Create;
begin
  FActive := False;
  FPort := 0;
  FServer := '';
  FAccountName := '';
  FAccountPassword := '';
end;

procedure TProxy.Activate(AType: TProxyType; AServer: WideString; APort: Integer; AAccountName, AAccountPassword: WideString);
begin
  FActive := True;
  FType := AType;
  FServer := AServer;
  FPort := APort;
  FAccountName := AAccountName;
  FAccountPassword := AAccountPassword;
end;

function TProxy.GetActive: Boolean;
begin
  Result := FActive;
end;

function TProxy.GetType;
begin
  Result := FType;
end;

function TProxy.GetServer: WideString;
begin
  Result := FServer;
end;

function TProxy.GetPort: Integer;
begin
  Result := FPort;
end;

function TProxy.GetAccountName: WideString;
begin
  Result := FAccountName;
end;

function TProxy.GetAccountPassword: WideString;
begin
  Result := FAccountPassword;
end;

{ TPlugIn }

function TPlugIn.GetCAPTCHAInput;
begin
  Result := FCAPTCHAInput;
end;

procedure TPlugIn.SetCAPTCHAInput(ACAPTCHAInput: TCAPTCHAInput);
begin
  FCAPTCHAInput := ACAPTCHAInput;
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

procedure TPlugIn.SetErrorMsg(AErrorMsg: WideString);
begin
  FErrorMsg := AErrorMsg;
end;

constructor TPlugIn.Create;
begin
  FProxy := TProxy.Create;
end;

end.
