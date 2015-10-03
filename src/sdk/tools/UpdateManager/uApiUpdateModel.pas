unit uApiUpdateModel;

interface

uses
  // Delphi
  SysUtils,
  // Common
  uBase,
  // Api
  uApiUpdateConst, uApiUpdateInterface, uApiUpdateModelBase;

type
  TIUpdateManagerVersion = class(TIFileVersion, IUpdateManagerVersion)
  private
    FID: Integer;
    FActive: WordBool;
  protected
    function GetID: Integer;
    procedure SetID(AID: Integer);
    function GetActive: WordBool;
    procedure SetActive(AActive: WordBool);
  public
    property ID: Integer read GetID write SetID;
    property Active: WordBool read GetActive write SetActive;
  end;

  TIUpdateManagerSystemFileBase = class(TIUpdateSystemFileBase, IUpdateManagerSystemFileBase)
  private
    FID: Integer;
  protected
    function GetID: Integer;
    procedure SetID(AID: Integer);
  public
    constructor Create;

    property ID: Integer read GetID write SetID;
  end;

  (*
  TIUpdateLocalFile = class(TIUpdateFile, IUpdateLocalFile)
  private
    FStatus: WordBool;
    FCondition: TUpdateCondition;
    FAction: TUpdateAction;
    FActions: TUpdateActions;
  protected
    function GetStatus: WordBool;
    procedure SetStatus(AStatus: WordBool);
    function GetCondition: TUpdateCondition;
    procedure SetCondition(ACondition: TUpdateCondition);
    function GetAction: TUpdateAction;
    procedure SetAction(AAction: TUpdateAction);
    function GetActions: TUpdateActions;
    procedure SetActions(AActions: TUpdateActions);
  public
    constructor Create(AFileName: WideString; AStatus: WordBool; ACondition: TUpdateCondition);

    property Status: WordBool read GetStatus write SetStatus;
    property Condition: TUpdateCondition read GetCondition write SetCondition;
    property Action: TUpdateAction read GetAction write SetAction;
    property Actions: TUpdateActions read GetActions write SetActions;
  end;
  *)

  TIFTPServer = class(TInterfacedObject, IFTPServer)
  private
    FName, FPort, FPath, FUsername, FPassword: WideString;
  protected
    function GetName: WideString;
    procedure SetName(AName: WideString);
    function GetPort: WideString;
    procedure SetPort(APort: WideString);
    function GetPath: WideString;
    procedure SetPath(APath: WideString);
    function GetUsername: WideString;
    procedure SetUsername(AUsername: WideString);
    function GetPassword: WideString;
    procedure SetPassword(APassword: WideString);
  public
    constructor Create(AName: WideString);

    property Name: WideString read GetName write SetName;
    property Port: WideString read GetPort write SetPort;
    property Path: WideString read GetPath write SetPath;
    property Username: WideString read GetUsername write SetUsername;
    property Password: WideString read GetPassword write SetPassword;
  end;

implementation

{ TIUpdateManagerVersion }

function TIUpdateManagerVersion.GetID: Integer;
begin
  Result := FID;
end;

procedure TIUpdateManagerVersion.SetID(AID: Integer);
begin
  FID := AID;
end;

function TIUpdateManagerVersion.GetActive: WordBool;
begin
  Result := FActive;
end;

procedure TIUpdateManagerVersion.SetActive(AActive: WordBool);
begin
  FActive := AActive;
end;

{ TIUpdateManagerSystemFileBase }

constructor TIUpdateManagerSystemFileBase.Create;
begin
  inherited Create('');
end;

function TIUpdateManagerSystemFileBase.GetID: Integer;
begin
  Result := FID;
end;

procedure TIUpdateManagerSystemFileBase.SetID(AID: Integer);
begin
  FID := AID;
end;

(*

{ TIUpdateLocalFile }

function TIUpdateLocalFile.GetStatus;
begin
  Result := FStatus;
end;

procedure TIUpdateLocalFile.SetStatus;
begin
  FStatus := AStatus;
end;

function TIUpdateLocalFile.GetCondition;
begin
  Result := FCondition;
end;

procedure TIUpdateLocalFile.SetCondition;
begin
  FCondition := ACondition;
end;

function TIUpdateLocalFile.GetAction;
begin
  Result := FAction;
end;

procedure TIUpdateLocalFile.SetAction;
begin
  FAction := AAction;
end;

function TIUpdateLocalFile.GetActions;
begin
  Result := FActions;
end;

procedure TIUpdateLocalFile.SetActions;
begin
  FActions := AActions;
end;

constructor TIUpdateLocalFile.Create;
begin
  inherited Create(AFileName);

  FStatus := AStatus;
  FCondition := ACondition;
end;

*)

{ TIFTPServer }

constructor TIFTPServer.Create(AName: WideString);
begin
  inherited Create;

  FName := AName;
end;

function TIFTPServer.GetName;
begin
  Result := FName;
end;

procedure TIFTPServer.SetName;
begin
  FName := AName;
end;

function TIFTPServer.GetPort;
begin
  Result := FPort;
end;

procedure TIFTPServer.SetPort;
begin
  FPort := APort;
end;

function TIFTPServer.GetPath;
begin
  Result := FPath;
end;

procedure TIFTPServer.SetPath;
begin
  FPath := APath;
end;

function TIFTPServer.GetUsername;
begin
  Result := FUsername;
end;

procedure TIFTPServer.SetUsername;
begin
  FUsername := AUsername;
end;

function TIFTPServer.GetPassword;
begin
  Result := FPassword;
end;

procedure TIFTPServer.SetPassword;
begin
  FPassword := APassword;
end;

end.
