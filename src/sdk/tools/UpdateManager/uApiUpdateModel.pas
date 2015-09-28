unit uApiUpdateModel;

interface

uses
  // Delphi
  SysUtils,
  // Common
  uBase,
  // Api
  uApiUpdateInterface, uApiUpdateConst;

type
  TIFile = class(TInterfacedObject, IFile)
  private
    FFileName: WideString;
  protected
    function GetFileName: WideString;
    procedure SetFileName(AFileName: WideString);
  public
    constructor Create(AFileName: WideString); virtual;

    property FileName: WideString read GetFileName write SetFileName;
  end;

  TIChecksumFile = class(TIFile, IChecksumFile)
  private
    FFileChecksum: WideString;
  protected
    function GetFileChecksum: WideString;
    procedure SetFileChecksum(AFileChecksum: WideString);
  public
    property FileChecksum: WideString read GetFileChecksum write SetFileChecksum;
  end;

  TIFileVersion = class(TInterfacedObject, IFileVersion)
  private
    FMajorVersion, FMinorVersion, FMajorBuild, FMinorBuild: Integer;
  protected
    function GetMajorVersion: Integer;
    procedure SetMajorVersion(AMajorVersion: Integer);
    function GetMinorVersion: Integer;
    procedure SetMinorVersion(AMinorVersion: Integer);
    function GetMajorBuild: Integer;
    procedure SetMajorBuild(AMajorBuild: Integer);
    function GetMinorBuild: Integer;
    procedure SetMinorBuild(AMinorBuild: Integer);

  public
    property MajorVersion: Integer read GetMajorVersion write SetMajorVersion;
    property MinorVersion: Integer read GetMinorVersion write SetMinorVersion;
    property MajorBuild: Integer read GetMajorBuild write SetMajorBuild;
    property MinorBuild: Integer read GetMinorBuild write SetMinorBuild;
  end;

  TIUpdateServerVersion = class(TIFileVersion, IUpdateServerVersion)
  private
    FID: Integer;
    FNew: WordBool;
  protected
    function GetID: Integer;
    procedure SetID(AID: Integer);
    function GetNew: WordBool;
    procedure SetNew(ANew: WordBool);
  public
    property ID: Integer read GetID write SetID;
    property New: WordBool read GetNew write SetNew;
  end;

  TIUpdateFile = class(TIChecksumFile, IUpdateFile)
  private
    FFileSystem: TFileSystem;
    FFileSizeCompressed: Integer;
    FFilePathAddition: WideString;

    FFileVersion: IFileVersion;
  protected
    function GetFileSystem: TFileSystem;
    procedure SetFileSystem(val: TFileSystem);

    function GetFileSizeCompressed: Integer;
    procedure SetFileSizeCompressed(val: Integer);

    function GetFilePathAddition: WideString;
    procedure SetFilePathAddition(val: WideString);

    function GetFileVersion: IFileVersion;
    procedure SetFileVersion(AFileVersion: IFileVersion);
  public
    constructor Create(AFileName: WideString); override;

    property FileSystem: TFileSystem read GetFileSystem write SetFileSystem;
    property FileSizeCompressed: Integer read GetFileSizeCompressed write SetFileSizeCompressed;
    property FilePathAddition: WideString read GetFilePathAddition write SetFilePathAddition;

    property FileVersion: IFileVersion read GetFileVersion write SetFileVersion;
  end;

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

{ TIFile }

function TIFile.GetFileName: WideString;
begin
  Result := FFileName;
end;

procedure TIFile.SetFileName(AFileName: WideString);
begin
  FFileName := AFileName;
end;

constructor TIFile.Create(AFileName: WideString);
begin
  FFileName := AFileName;
end;

{ TIChecksumFile }

function TIChecksumFile.GetFileChecksum: WideString;
begin
  Result := FFileChecksum;
end;

procedure TIChecksumFile.SetFileChecksum(AFileChecksum: WideString);
begin
  FFileChecksum := AFileChecksum;
end;

{ TIFileVersion }

function TIFileVersion.GetMajorVersion;
begin
  Result := FMajorVersion;
end;

procedure TIFileVersion.SetMajorVersion;
begin
  FMajorVersion := AMajorVersion;
end;

function TIFileVersion.GetMinorVersion;
begin
  Result := FMinorVersion;
end;

procedure TIFileVersion.SetMinorVersion;
begin
  FMinorVersion := AMinorVersion;
end;

function TIFileVersion.GetMajorBuild;
begin
  Result := FMajorBuild;
end;

procedure TIFileVersion.SetMajorBuild;
begin
  FMajorBuild := AMajorBuild;
end;

function TIFileVersion.GetMinorBuild;
begin
  Result := FMinorBuild;
end;

procedure TIFileVersion.SetMinorBuild;
begin
  FMinorBuild := AMinorBuild;
end;

{ TIUpdateServerVersion }

function TIUpdateServerVersion.GetID: Integer;
begin
  Result := FID;
end;

procedure TIUpdateServerVersion.SetID(AID: Integer);
begin
  FID := AID;
end;

function TIUpdateServerVersion.GetNew: WordBool;
begin
  Result := FNew;
end;

procedure TIUpdateServerVersion.SetNew(ANew: WordBool);
begin
  FNew := ANew;
end;

{ TIUpdateFile }

function TIUpdateFile.GetFileSystem: TFileSystem;
begin
  Result := FFileSystem;
end;

procedure TIUpdateFile.SetFileSystem(val: TFileSystem);
begin
  FFileSystem := val;
end;

function TIUpdateFile.GetFileSizeCompressed: Integer;
begin
  Result := FFileSizeCompressed;
end;

procedure TIUpdateFile.SetFileSizeCompressed(val: Integer);
begin
  FFileSizeCompressed := val;
end;

function TIUpdateFile.GetFilePathAddition: WideString;
begin
  Result := FFilePathAddition;
end;

procedure TIUpdateFile.SetFilePathAddition(val: WideString);
begin
  FFilePathAddition := val;
end;

function TIUpdateFile.GetFileVersion: IFileVersion;
begin
  Result := FFileVersion;
end;

procedure TIUpdateFile.SetFileVersion(AFileVersion: IFileVersion);
begin
  FFileVersion := AFileVersion;
end;

constructor TIUpdateFile.Create(AFileName: WideString);
begin
  inherited Create(AFileName);

  FFileVersion := TIFileVersion.Create;
end;

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
