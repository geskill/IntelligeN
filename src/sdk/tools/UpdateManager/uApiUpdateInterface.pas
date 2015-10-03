unit uApiUpdateInterface;

interface

uses
  // Common
  uBase,
  // Api
  uApiUpdateConst, uApiUpdateInterfaceBase;

type
  IStatusFile = interface(IFile)
    function GetStatus: WordBool;
    procedure SetStatus(AStatus: WordBool);

    property Status: WordBool read GetStatus write SetStatus;
  end;

  (*
    IUpdateLocalFile = interface(IUpdateFile)
    function GetStatus: WordBool;
    procedure SetStatus(AStatus: WordBool);
    function GetCondition: TUpdateCondition;
    procedure SetCondition(ACondition: TUpdateCondition);
    function GetAction: TUpdateAction;
    procedure SetAction(AAction: TUpdateAction);
    function GetActions: TUpdateActions;
    procedure SetActions(AActions: TUpdateActions);

    property Status: WordBool read GetStatus write SetStatus;
    property Condition: TUpdateCondition read GetCondition write SetCondition;
    property Action: TUpdateAction read GetAction write SetAction;
    property Actions: TUpdateActions read GetActions write SetActions;
    end;
    *)

  IUpdateManagerVersion = interface(IFileVersion)
    ['{DDAF7DDA-822F-4EAC-98C5-0E64F35AAD93}']
    function GetID: Integer;
    procedure SetID(AID: Integer);
    function GetActive: WordBool;
    procedure SetActive(AActive: WordBool);

    property ID: Integer read GetID write SetID;
    property Active: WordBool read GetActive write SetActive;
  end;

  IUpdateManagerSystemFileBase = interface(IUpdateSystemFileBase)
    ['{125B2271-7206-4C59-A837-52F55A60F9AA}']
    function GetID: Integer;
    procedure SetID(AID: Integer);

    property ID: Integer read GetID write SetID;
  end;

  IServer = interface
    function GetName: WideString;
    procedure SetName(AName: WideString);

    property Name: WideString read GetName write SetName;
  end;

  IUpdateServer = interface(IServer)
    function GetAccessToken: WideString;
    procedure SetAccessToken(AAccessToken: WideString);

    property AccessToken: WideString read GetAccessToken write SetAccessToken;
  end;

  IFTPServer = interface(IServer)
    function GetPort: WideString;
    procedure SetPort(APort: WideString);
    function GetPath: WideString;
    procedure SetPath(APath: WideString);
    function GetUsername: WideString;
    procedure SetUsername(AUsername: WideString);
    function GetPassword: WideString;
    procedure SetPassword(APassword: WideString);

    property Port: WideString read GetPort write SetPort;
    property Path: WideString read GetPath write SetPath;
    property Username: WideString read GetUsername write SetUsername;
    property Password: WideString read GetPassword write SetPassword;
  end;

implementation

end.
