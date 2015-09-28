unit uApiUpdateInterface;

interface

uses
  // Common
  uBase,
  // Api
  uApiUpdateConst;

type
  IFile = interface(IUnknown)
    function GetFileName: WideString;
    procedure SetFileName(AFileName: WideString);

    property FileName: WideString read GetFileName write SetFileName;
  end;

  IChecksumFile = interface(IFile)
    function GetFileChecksum: WideString;
    procedure SetFileChecksum(AFileChecksum: WideString);

    property FileChecksum: WideString read GetFileChecksum write SetFileChecksum;
  end;

  IStatusFile = interface(IFile)
    function GetStatus: WordBool;
    procedure SetStatus(AStatus: WordBool);

    property Status: WordBool read GetStatus write SetStatus;
  end;

  IFileVersion = interface(IUnknown)
    function GetMajorVersion: Integer;
    procedure SetMajorVersion(AMajorVersion: Integer);
    function GetMinorVersion: Integer;
    procedure SetMinorVersion(AMinorVersion: Integer);
    function GetMajorBuild: Integer;
    procedure SetMajorBuild(AMajorBuild: Integer);
    function GetMinorBuild: Integer;
    procedure SetMinorBuild(AMinorBuild: Integer);

    property MajorVersion: Integer read GetMajorVersion write SetMajorVersion;
    property MinorVersion: Integer read GetMinorVersion write SetMinorVersion;
    property MajorBuild: Integer read GetMajorBuild write SetMajorBuild;
    property MinorBuild: Integer read GetMinorBuild write SetMinorBuild;
  end;

  IUpdateFile = interface(IChecksumFile)

    function GetFileSystem: TFileSystem;
    procedure SetFileSystem(val: TFileSystem);

    function GetFileSizeCompressed: Integer;
    procedure SetFileSizeCompressed(val: Integer);

    function GetFilePathAddition: WideString;
    procedure SetFilePathAddition(val: WideString);

    function GetFileVersion: IFileVersion;
    procedure SetFileVersion(AFileVersion: IFileVersion);

    property FileSystem: TFileSystem read GetFileSystem write SetFileSystem;
    property FileSizeCompressed: Integer read GetFileSizeCompressed write SetFileSizeCompressed;
    property FilePathAddition: WideString read GetFilePathAddition write SetFilePathAddition;

    property FileVersion: IFileVersion read GetFileVersion write SetFileVersion;
  end;

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

  IUpdateServerVersion = interface(IFileVersion)
    function GetID: Integer;
    procedure SetID(AID: Integer);
    function GetNew: WordBool;
    procedure SetNew(ANew: WordBool);

    property ID: Integer read GetID write SetID;
    property New: WordBool read GetNew write SetNew;
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
