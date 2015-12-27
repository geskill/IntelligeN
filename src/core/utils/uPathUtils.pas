unit uPathUtils;

interface

uses
  // Delphi
  Windows, SysUtils, ShlObj, ActiveX;

function GetWindowsFolder: string;
function GetWindowsSystemFolder: string;
function GetCommonDocumentsFolder: string;
function GetCommonAppDataFolder: string;
function GetPersonalFolder: string;
function GetPersonalAppDataFolder: string;
function GetPersonalLocalAppDataFolder: string;
function GetPersonalLocalTempPath: string;

function PathCombineEx(const ABaseName, ARelativePath: string): string;

function IsDirectory(const FileName: string): Boolean;

implementation

// Je nach Delphi Version anpassen!
function PathCombine(lpszDest: PChar; const lpszDir, lpszFile: PChar): PChar; stdcall; external 'shlwapi.dll' name 'PathCombineW';
function PathCombineA(lpszDest: PAnsiChar; const lpszDir, lpszFile: PAnsiChar): PAnsiChar; stdcall; external 'shlwapi.dll';
function PathCombineW(lpszDest: PWideChar; const lpszDir, lpszFile: PWideChar): PWideChar; stdcall; external 'shlwapi.dll';

function GetSpecialFolder(hWindow: HWND; Folder: Integer): string;
var
  pMalloc: IMalloc;
  pidl: PItemIDList;
  Path: PChar;
begin
  // get IMalloc interface pointer
  if (SHGetMalloc(pMalloc) <> S_OK) then
  begin
    MessageBox(hWindow, 'Couldn''t get pointer to IMalloc interface.', 'SHGetMalloc(pMalloc)', 16);
    Exit;
  end;

  // retrieve path
  SHGetSpecialFolderLocation(hWindow, Folder, pidl);
  GetMem(Path, MAX_PATH);
  SHGetPathFromIDList(pidl, Path);
  Result := IncludeTrailingPathDelimiter(Path);
  FreeMem(Path);

  // free memory allocated by SHGetSpecialFolderLocation
  pMalloc.Free(pidl);
end;

function GetWindowsFolder: string;
begin
  // C:\WINDOWS\
  Result := GetSpecialFolder(0, CSIDL_WINDOWS);
end;

function GetWindowsSystemFolder: string;
begin
  // C:\WINDOWS\system32\
  Result := GetSpecialFolder(0, CSIDL_SYSTEM);
end;

function GetCommonDocumentsFolder: string;
begin
  // C:\Documents and Settings\All Users\Documents\
  Result := GetSpecialFolder(0, CSIDL_COMMON_DOCUMENTS);
end;

function GetCommonAppDataFolder: string;
begin
  // C:\Documents and Settings\All Users\Application Data\
  Result := GetSpecialFolder(0, CSIDL_COMMON_APPDATA);
end;

function GetPersonalFolder: string;
begin
  // C:\Documents and Settings\< username >\My Documents\
  Result := GetSpecialFolder(0, CSIDL_PERSONAL);
end;

function GetPersonalAppDataFolder: string;
begin
  // C:\Documents and Settings\< username >\Application Data\
  Result := GetSpecialFolder(0, CSIDL_APPDATA);
end;

function GetPersonalLocalAppDataFolder: string;
begin
  // C:\Documents and Settings\< username >\Local Settings\Application Data\
  Result := GetSpecialFolder(0, CSIDL_LOCAL_APPDATA);
end;

function GetPersonalLocalTempPath: string;
begin
  // C:\Documents and Settings\< username >\Local Settings\Temp\
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'));
end;

function PathCombineEx(const ABaseName, ARelativePath: string): string;
begin
  SetLength(Result, MAX_PATH);
  PathCombine(@Result[1], PChar(ExtractFilePath(ABaseName)), PChar(ARelativePath));
  SetLength(Result, length(PChar(@Result[1])));
end;

function IsDirectory(const FileName: string): Boolean;
var
  R: DWORD;
begin
  R := GetFileAttributes(PChar(FileName));
  Result := (R <> DWORD(-1)) and ((R and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;

end.
