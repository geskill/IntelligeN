unit uApiUpdateInterfaceBase;

interface

uses
  // Common
  uBaseConst;

type
  IFile = interface(IUnknown)
    ['{F17455CD-E1F1-430C-9EAD-97F8DA6E3409}']
    function GetFileName: WideString;
    procedure SetFileName(AFileName: WideString);

    property FileName: WideString read GetFileName write SetFileName;
  end;

  IFileVersion = interface(IUnknown)
    ['{23FDA195-BE71-4A16-9760-A9D97FD44525}']
    function GetMajorVersion: Integer;
    procedure SetMajorVersion(AMajorVersion: Integer);
    function GetMinorVersion: Integer;
    procedure SetMinorVersion(AMinorVersion: Integer);
    function GetMajorBuild: Integer;
    procedure SetMajorBuild(AMajorBuild: Integer);
    function GetMinorBuild: Integer;
    procedure SetMinorBuild(AMinorBuild: Integer);

    function IsPreRelease: WordBool;
    function ToString: WideString;

    property MajorVersion: Integer read GetMajorVersion write SetMajorVersion;
    property MinorVersion: Integer read GetMinorVersion write SetMinorVersion;
    property MajorBuild: Integer read GetMajorBuild write SetMajorBuild;
    property MinorBuild: Integer read GetMinorBuild write SetMinorBuild;
  end;

  IUpdateSystemFileBase = interface(IFile)
    ['{8E95B78E-D1E0-4A9F-B372-95B73E886E43}']
    function GetFileSystem: TFileSystem;
    procedure SetFileSystem(AFileSystem: TFileSystem);

    function GetFilePathAppendix: WideString;
    procedure SetFilePathAppendix(AFilePathAppendix: WideString);

    property FileSystem: TFileSystem read GetFileSystem write SetFileSystem;
    property FilePathAppendix: WideString read GetFilePathAppendix write SetFilePathAppendix;
  end;

  IUpdateSystemFile = interface(IUnknown)
    ['{2E682F48-6F50-460C-A4F0-ECEE7069FA02}']
    function GetFileBase: IUpdateSystemFileBase;
    procedure SetFileBase(AFileBase: IUpdateSystemFileBase);

    function GetFileVersion: IFileVersion;
    procedure SetFileVersion(AFileVersion: IFileVersion);

    function GetFileSizeCompressed: Integer;
    procedure SetFileSizeCompressed(AFileSizeCompressed: Integer);

    function GetFileChecksum: WideString;
    procedure SetFileChecksum(AFileChecksum: WideString);

    property FileBase: IUpdateSystemFileBase read GetFileBase write SetFileBase;
    property FileVersion: IFileVersion read GetFileVersion write SetFileVersion;
    property FileSizeCompressed: Integer read GetFileSizeCompressed write SetFileSizeCompressed;
    property FileChecksum: WideString read GetFileChecksum write SetFileChecksum;
  end;

implementation

end.
