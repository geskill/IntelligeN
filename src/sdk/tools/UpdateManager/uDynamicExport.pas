unit uDynamicExport;

interface

uses
  // Delphi
  Windows, SysUtils, Classes,
  // Common
  uBaseConst,
  // Api
  uApiUpdateConst;

type
  TGetRootDir = function: WideString; stdcall;
  TGetPathFromFileSystemID = function(AFileSystem: TFileSystem): WideString; stdcall;
  TGetFileSystemIDFromPath = function(AFileName: WideString): TFileSystem; stdcall;

  TIntelligeNFileSystem = class
  private
  var
    FFrameworkLib: Cardinal;
    FFileSystemLib: Cardinal;

  const
    FRAMEWORK_LIB: string = 'framework.bpl';

  type
    TLibProc = reference to procedure(AAddress: Pointer);
  protected
    procedure Execute(const AProcName: string; ALibProc: TLibProc);
  public
    constructor Create(AFileSystemLib: TFileName);

    function GetRootDir(): TFileName;

    function GetPathFromFileSystemID(AFileSystem: TFileSystem): WideString;
    function GetFileSystemIDFromPath(AFileName: WideString): TFileSystem;

    destructor Destroy; override;
  end;

const
  INTELLIGEN_FILESYSTEM_LIB: string = 'IntelligeN.dll';

implementation

procedure TIntelligeNFileSystem.Execute;
begin
  ALibProc(GetProcAddress(FFileSystemLib, PChar(AProcName)));
end;

constructor TIntelligeNFileSystem.Create(AFileSystemLib: TFileName);
var
  LRootDir: string;
begin
  LRootDir := IncludeTrailingPathDelimiter(ExtractFilePath(AFileSystemLib));

  FFrameworkLib := LoadPackage(PChar(LRootDir + FRAMEWORK_LIB));
  FFileSystemLib := LoadLibrary(PChar(AFileSystemLib));
  if (FFileSystemLib = 0) then
    RaiseLastOSError;
end;

function TIntelligeNFileSystem.GetRootDir(): TFileName;
var
  LResult: TFileName;
begin
  LResult := '';
  Execute('GetRootDir',
    { } procedure(AAddress: Pointer)
    { } var
    { . } MGetRootDir: TGetRootDir;
    { } begin
    { . } @MGetRootDir := AAddress;
    { . } LResult := MGetRootDir();
    { } end);
  Result := LResult;
end;

function TIntelligeNFileSystem.GetPathFromFileSystemID(AFileSystem: TFileSystem): WideString;
var
  LResult: WideString;
begin
  LResult := '';
  Execute('GetPathFromFileSystemID',
    { } procedure(AAddress: Pointer)
    { } var
    { . } MGetPathFromFileSystemID: TGetPathFromFileSystemID;
    { } begin
    { . } @MGetPathFromFileSystemID := AAddress;
    { . } LResult := MGetPathFromFileSystemID(AFileSystem);
    { } end);
  Result := LResult;
end;

function TIntelligeNFileSystem.GetFileSystemIDFromPath(AFileName: WideString): TFileSystem;
var
  LResult: TFileSystem;
begin
  LResult := fsRoot;
  Execute('GetFileSystemIDFromPath',
    { } procedure(AAddress: Pointer)
    { } var
    { . } MGetFileSystemIDFromPath: TGetFileSystemIDFromPath;
    { } begin
    { . } @MGetFileSystemIDFromPath := AAddress;
    { . } LResult := MGetFileSystemIDFromPath(AFileName);
    { } end);
  Result := LResult;
end;

destructor TIntelligeNFileSystem.Destroy;
begin
  FreeLibrary(FFileSystemLib);
  UnloadPackage(FFrameworkLib);
  inherited Destroy;
end;

end.
