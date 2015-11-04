unit uPathUtils;

interface

uses
  // Delphi
  Windows, SysUtils, ShlObj, ActiveX,
  // Utils
  uStringUtils;

const
  HTTP: string = 'http://';

function GetWindowsFolder: string;
function GetWindowsSystemFolder: string;
function GetCommonDocumentsFolder: string;
function GetCommonAppDataFolder: string;
function GetPersonalFolder: string;
function GetPersonalAppDataFolder: string;
function GetPersonalLocalAppDataFolder: string;
function GetPersonalLocalTempPath: string;

function PathCombineEx(BaseName, RelativePath: string): string;

function BeginsWithHTTP(const AUrl: string): Boolean;
function IsDirectory(const FileName: string): Boolean;
function IsURL(const AUrl: string): Boolean;

function ExtractUrlFileName(const AUrl: string): string;
function ExtractUrlProtocol(const AUrl: string): string;
function ExtractUrlHost(const AUrl: string): string;
function ExtractUrlWebsite(const AUrl: string): string;
function IncludeTrailingUrlDelimiter(const AUrl: string): string;
function ExcludeTrailingUrlDelimiter(const AUrl: string): string;

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

function PathCombineEx(BaseName, RelativePath: string): string;
begin
  SetLength(Result, MAX_PATH);
  PathCombine(@Result[1], PChar(ExtractFilePath(BaseName)), PChar(RelativePath));
  SetLength(Result, length(PChar(@Result[1])));
end;

function BeginsWithHTTP(const AUrl: string): Boolean;
begin
  Result := (copy(LowerCase(AUrl), 1, length(HTTP)) = HTTP);
end;

function IsDirectory(const FileName: string): Boolean;
var
  R: DWORD;
begin
  R := GetFileAttributes(PChar(FileName));
  Result := (R <> DWORD(-1)) and ((R and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;

function IsURL(const AUrl: string): Boolean;
begin
  Result := Pos('://', AUrl) > 0;
end;

function ExtractUrlFileName(const AUrl: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('/', AUrl);
  Result := copy(AUrl, I + 1, length(AUrl) - (I));
end;

function ExtractUrlProtocol(const AUrl: string): string;
var
  LPosition: Integer;
begin
  LPosition := Pos('://', AUrl);
  if (LPosition = 0) then
    Result := ''
  else
    Result := copy(AUrl, 1, LPosition - 1);
end;

function ExtractUrlHost(const AUrl: string): string;
var
  _pos, _pos2: Integer;
  s: string;
begin
  if ExtractUrlProtocol(AUrl) = '' then
  begin
    _pos := Pos(':', AUrl);
    if (_pos = 0) then
      _pos := Pos('/', AUrl);
    if (_pos = 0) then
      _pos := length(AUrl) + 1;
    Result := copy(AUrl, 1, _pos - 1);
  end
  else
  begin
    _pos := Pos('://', AUrl);
    Inc(_pos, 3);
    s := copy(AUrl, _pos, length(AUrl));
    _pos2 := Pos(':', s);
    if (_pos2 = 0) then
      _pos2 := Pos('/', s);
    if (_pos2 = 0) then
      _pos2 := length(s) + 1;
    Result := copy(s, 1, _pos2 - 1);
  end;
end;

function ExtractUrlWebsite(const AUrl: string): string;
var
  LUrl: string;
begin
  if not BeginsWithHTTP(AUrl) then
    LUrl := HTTP + AUrl
  else
    LUrl := AUrl;

  if not(LUrl[length(LUrl)] = '/') then
  begin
    if (CharCount('/', LUrl) < 3) then
      LUrl := IncludeTrailingUrlDelimiter(LUrl)
    else
      LUrl := copy(LUrl, 1, LastDelimiter('/', LUrl))
  end;
  Result := LUrl;
end;

function IncludeTrailingUrlDelimiter(const AUrl: string): string;
const
  UrlDelim = '/';
begin
  Result := AUrl;
  if not IsDelimiter(UrlDelim, Result, length(Result)) then
    Result := Result + UrlDelim;
end;

function ExcludeTrailingUrlDelimiter(const AUrl: string): string;
const
  UrlDelim = '/';
begin
  Result := AUrl;
  if IsDelimiter(UrlDelim, Result, length(Result)) then
    SetLength(Result, length(Result) - 1);
end;

end.
