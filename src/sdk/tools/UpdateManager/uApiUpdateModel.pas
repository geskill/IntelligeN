unit uApiUpdateModel;

interface

uses
  // Delphi
  SysUtils,
  // Common
  uBase,
  // Export
  uDynamicExport,
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

    function GetFullFileName(AIntelligeNFileSystem: TIntelligeNFileSystem): WideString;

    property ID: Integer read GetID write SetID;
  end;

  TIUpdateManagerLocalFile = class(TIFile, IUpdateManagerLocalFile)
  private
    FOnline: WordBool;
    FStatus: WordBool;
    FCondition: TUpdateCondition;
    FAction: TUpdateAction;
    FActions: TUpdateActions;
  protected
    function GetOnline: WordBool;
    procedure SetOnline(AOnline: WordBool);
    function GetStatus: WordBool;
    procedure SetStatus(AStatus: WordBool);
    function GetCondition: TUpdateCondition;
    procedure SetCondition(ACondition: TUpdateCondition);
    function GetAction: TUpdateAction;
    procedure SetAction(AAction: TUpdateAction);
    function GetActions: TUpdateActions;
    procedure SetActions(AActions: TUpdateActions);
  public
    constructor Create(AFileName: WideString); reintroduce;

    property Online: WordBool read GetOnline write SetOnline;
    property Status: WordBool read GetStatus write SetStatus;
    property Condition: TUpdateCondition read GetCondition write SetCondition;
    property Action: TUpdateAction read GetAction write SetAction;
    property Actions: TUpdateActions read GetActions write SetActions;
  end;

  TIUpdateManagerSystemFile = class(TIUpdateSystemFile, IUpdateManagerSystemFile)
  private
    FID: Integer;
    FFileBase: IUpdateManagerSystemFileBase;
    FLocalFile: IUpdateManagerLocalFile;
  protected
    function GetID: Integer;
    procedure SetID(AID: Integer);
    function GetFileBase: IUpdateManagerSystemFileBase; reintroduce; overload;
    procedure SetFileBase(const AFileBase: IUpdateManagerSystemFileBase); reintroduce; overload;
    function GetLocalFile: IUpdateManagerLocalFile;
    procedure SetLocalFile(const ALocalFile: IUpdateManagerLocalFile);
  public
    constructor Create(const ALocalFile: IUpdateManagerLocalFile; const AFileBase: IUpdateManagerSystemFileBase);

    property ID: Integer read GetID write SetID;
    property FileBase: IUpdateManagerSystemFileBase read GetFileBase write SetFileBase;
    property LocalFile: IUpdateManagerLocalFile read GetLocalFile write SetLocalFile;

    destructor Destroy;
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

function TIUpdateManagerSystemFileBase.GetID: Integer;
begin
  Result := FID;
end;

procedure TIUpdateManagerSystemFileBase.SetID(AID: Integer);
begin
  FID := AID;
end;

function TIUpdateManagerSystemFileBase.GetFullFileName(AIntelligeNFileSystem: TIntelligeNFileSystem): WideString;
begin
  Result := IncludeTrailingPathDelimiter(
    { . } IncludeTrailingPathDelimiter(AIntelligeNFileSystem.GetPathFromFileSystemID(FileSystem)) +
    { ... } FilePathAppendix
    { . } ) +
  { . } FileName;
end;

constructor TIUpdateManagerSystemFileBase.Create;
begin
  inherited Create('');
end;

{ TIUpdateManagerLocalFile }

function TIUpdateManagerLocalFile.GetOnline: WordBool;
begin
  Result := FOnline;
end;

procedure TIUpdateManagerLocalFile.SetOnline(AOnline: WordBool);
begin
  FOnline := AOnline;
end;

function TIUpdateManagerLocalFile.GetStatus;
begin
  Result := FStatus;
end;

procedure TIUpdateManagerLocalFile.SetStatus;
begin
  FStatus := AStatus;
end;

function TIUpdateManagerLocalFile.GetCondition;
begin
  Result := FCondition;
end;

procedure TIUpdateManagerLocalFile.SetCondition;
begin
  FCondition := ACondition;
end;

function TIUpdateManagerLocalFile.GetAction;
begin
  Result := FAction;
end;

procedure TIUpdateManagerLocalFile.SetAction;
begin
  FAction := AAction;
end;

function TIUpdateManagerLocalFile.GetActions;
begin
  Result := FActions;
end;

procedure TIUpdateManagerLocalFile.SetActions;
begin
  FActions := AActions;
end;

constructor TIUpdateManagerLocalFile.Create(AFileName: WideString);
begin
  inherited Create(AFileName);

  FOnline := False;
  FStatus := False;
  FCondition := ucNew;
  FAction := uaAddnUpdate;
  FActions := [];
end;

{ TIUpdateManagerSystemFile }

function TIUpdateManagerSystemFile.GetID: Integer;
begin
  Result := FID;
end;

procedure TIUpdateManagerSystemFile.SetID(AID: Integer);
begin
  FID := AID;
end;

function TIUpdateManagerSystemFile.GetFileBase: IUpdateManagerSystemFileBase;
begin
  Result := FFileBase;
end;

procedure TIUpdateManagerSystemFile.SetFileBase(const AFileBase: IUpdateManagerSystemFileBase);
begin
  FFileBase := AFileBase;
end;

function TIUpdateManagerSystemFile.GetLocalFile: IUpdateManagerLocalFile;
begin
  Result := FLocalFile;
end;

procedure TIUpdateManagerSystemFile.SetLocalFile(const ALocalFile: IUpdateManagerLocalFile);
begin
  FLocalFile := ALocalFile;
end;

constructor TIUpdateManagerSystemFile.Create;
begin
  if not Assigned(ALocalFile) then
  begin
    inherited Create('');
    FLocalFile := TIUpdateManagerLocalFile.Create('');
  end
  else
  begin
    inherited Create(ExtractFileName(ALocalFile.FileName));
    FLocalFile := ALocalFile;
  end;
  FID := 0;
  if not Assigned(AFileBase) then
    FFileBase := TIUpdateManagerSystemFileBase.Create
  else
    FFileBase := AFileBase;
end;

destructor TIUpdateManagerSystemFile.Destroy;
begin
  FFileBase := nil;
  FLocalFile := nil;
  inherited Destroy;
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
