unit uApiUpdateModelBase;

interface

uses
  // Delphi
  SysUtils,
  // Common
  uBaseConst,
  // Api
  uApiUpdateInterfaceBase;

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
    constructor Create;
    constructor Clone(const AFileVersion: IFileVersion);

    function IsPreRelease: WordBool;
    function ToString: WideString;

    property MajorVersion: Integer read GetMajorVersion write SetMajorVersion;
    property MinorVersion: Integer read GetMinorVersion write SetMinorVersion;
    property MajorBuild: Integer read GetMajorBuild write SetMajorBuild;
    property MinorBuild: Integer read GetMinorBuild write SetMinorBuild;
  end;

  TIUpdateSystemFileBase = class(TIFile, IUpdateSystemFileBase)
  private
    FFileSystem: TFileSystem;
    FFilePathAppendix: WideString;
  protected
    function GetFileSystem: TFileSystem;
    procedure SetFileSystem(AFileSystem: TFileSystem);

    function GetFilePathAppendix: WideString;
    procedure SetFilePathAppendix(AFilePathAppendix: WideString);
  public
    constructor Create(AFileName: WideString); override;
    constructor Clone(const AUpdateSystemFileBase: IUpdateSystemFileBase);

    function Equals(AValue: IUpdateSystemFileBase): Boolean;

    property FileSystem: TFileSystem read GetFileSystem write SetFileSystem;
    property FilePathAppendix: WideString read GetFilePathAppendix write SetFilePathAppendix;
  end;

  TIUpdateSystemFile = class(TInterfacedObject, IUpdateSystemFile)
  private
    FFileBase: IUpdateSystemFileBase;
    FFileVersion: IFileVersion;
    FFileSizeCompressed: Integer;
    FFileChecksum: WideString;
  protected
    function GetFileBase: IUpdateSystemFileBase; virtual;
    procedure SetFileBase(AFileBase: IUpdateSystemFileBase); virtual;

    function GetFileVersion: IFileVersion;
    procedure SetFileVersion(AFileVersion: IFileVersion);

    function GetFileSizeCompressed: Integer;
    procedure SetFileSizeCompressed(AFileSizeCompressed: Integer);

    function GetFileChecksum: WideString;
    procedure SetFileChecksum(AFileChecksum: WideString);
  public
    constructor Create(const AUpdateSystemFileBase: IUpdateSystemFileBase; const AFileVersion: IFileVersion); overload;
    constructor Create(AFileName: WideString); overload;

    constructor Clone(const AUpdateSystemFile: IUpdateSystemFile);

    property FileBase: IUpdateSystemFileBase read GetFileBase write SetFileBase;
    property FileVersion: IFileVersion read GetFileVersion write SetFileVersion;
    property FileSizeCompressed: Integer read GetFileSizeCompressed write SetFileSizeCompressed;
    property FileChecksum: WideString read GetFileChecksum write SetFileChecksum;

    destructor Destroy; override;
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

constructor TIFileVersion.Create;
begin
  FMajorVersion := 0;
  FMinorVersion := 0;
  FMajorBuild := 0;
  FMinorBuild := 0;
end;

constructor TIFileVersion.Clone(const AFileVersion: IFileVersion);
begin
  Create;

  FMajorVersion := AFileVersion.MajorVersion;
  FMinorVersion := AFileVersion.MinorVersion;
  FMajorBuild := AFileVersion.MajorBuild;
  FMinorBuild := AFileVersion.MinorBuild;
end;

function TIFileVersion.IsPreRelease: WordBool;
begin
  Result := not(MinorBuild = 0)
end;

function TIFileVersion.ToString: WideString;
begin
  Result := IntToStr(MajorVersion) + '.' + IntToStr(MinorVersion) + '.' + IntToStr(MajorBuild) + '.' + IntToStr(MinorBuild);
end;

{ TIUpdateSystemFileBase }

function TIUpdateSystemFileBase.GetFileSystem: TFileSystem;
begin
  Result := FFileSystem;
end;

procedure TIUpdateSystemFileBase.SetFileSystem(AFileSystem: TFileSystem);
begin
  FFileSystem := AFileSystem;
end;

function TIUpdateSystemFileBase.GetFilePathAppendix: WideString;
begin
  Result := FFilePathAppendix;
end;

procedure TIUpdateSystemFileBase.SetFilePathAppendix(AFilePathAppendix: WideString);
begin
  FFilePathAppendix := AFilePathAppendix;
end;

constructor TIUpdateSystemFileBase.Create(AFileName: WideString);
begin
  inherited Create(AFileName);

  FFilePathAppendix := '';
end;

constructor TIUpdateSystemFileBase.Clone(const AUpdateSystemFileBase: IUpdateSystemFileBase);
begin
  Create(AUpdateSystemFileBase.FileName);

  FFileSystem := AUpdateSystemFileBase.FileSystem;
  FFilePathAppendix := AUpdateSystemFileBase.FilePathAppendix;
end;

function TIUpdateSystemFileBase.Equals(AValue: IUpdateSystemFileBase): Boolean;
begin
  Result := (FileSystem = AValue.FileSystem) and (FilePathAppendix = AValue.FilePathAppendix) and (FileName = AValue.FileName);
end;

{ TIUpdateSystemFile }

function TIUpdateSystemFile.GetFileBase: IUpdateSystemFileBase;
begin
  Result := FFileBase;
end;

procedure TIUpdateSystemFile.SetFileBase(AFileBase: IUpdateSystemFileBase);
begin
  FFileBase := AFileBase;
end;

function TIUpdateSystemFile.GetFileVersion: IFileVersion;
begin
  Result := FFileVersion;
end;

procedure TIUpdateSystemFile.SetFileVersion(AFileVersion: IFileVersion);
begin
  FFileVersion := AFileVersion;
end;

function TIUpdateSystemFile.GetFileSizeCompressed: Integer;
begin
  Result := FFileSizeCompressed;
end;

procedure TIUpdateSystemFile.SetFileSizeCompressed(AFileSizeCompressed: Integer);
begin
  FFileSizeCompressed := AFileSizeCompressed;
end;

function TIUpdateSystemFile.GetFileChecksum: WideString;
begin
  Result := FFileChecksum;
end;

procedure TIUpdateSystemFile.SetFileChecksum(AFileChecksum: WideString);
begin
  FFileChecksum := AFileChecksum;
end;

constructor TIUpdateSystemFile.Create(const AUpdateSystemFileBase: IUpdateSystemFileBase; const AFileVersion: IFileVersion);
begin
  inherited Create();

  FFileBase := AUpdateSystemFileBase;
  FFileVersion := AFileVersion;
  FFileSizeCompressed := 0;
  FFileChecksum := '';
end;

constructor TIUpdateSystemFile.Create(AFileName: WideString);
var
  LUpdateSystemFileBase: IUpdateSystemFileBase;
  LFileVersion: IFileVersion;
begin
  LUpdateSystemFileBase := TIUpdateSystemFileBase.Create(AFileName);
  LFileVersion := TIFileVersion.Create;

  Create(LUpdateSystemFileBase, LFileVersion);
end;

constructor TIUpdateSystemFile.Clone(const AUpdateSystemFile: IUpdateSystemFile);
var
  LUpdateSystemFileBase: IUpdateSystemFileBase;
  LFileVersion: IFileVersion;
begin
  LUpdateSystemFileBase := TIUpdateSystemFileBase.Clone(AUpdateSystemFile.FileBase);
  LFileVersion := TIFileVersion.Clone(AUpdateSystemFile.FileVersion);

  Create(LUpdateSystemFileBase, LFileVersion);

  FFileSizeCompressed := AUpdateSystemFile.FileSizeCompressed;
  FFileChecksum := AUpdateSystemFile.FileChecksum;
end;

destructor TIUpdateSystemFile.Destroy;
begin
  FFileVersion := nil;
  FFileBase := nil;

  inherited Destroy();
end;

end.
