unit uFileUtils;

interface

uses
  Windows, SysUtils, Classes, DECHash, DECFmt,
{$WARN UNIT_PLATFORM OFF}
  FileCtrl,
{$WARN UNIT_PLATFORM ON}
  ShellAPI;

function TrueFilename(AFileName: string): string;

function GetMD5FromFile(FileName: string): string;

function GetFileSize(const FileName: string): Int64;

procedure GetDirectoriesFromDirectory(Directory: string; const List: TStrings; ClearList: Boolean);

procedure GetFilesInDirectory(Directory: string; const Mask: string; List: TStrings; WithPath, WithSubDirs, WithFileExt, ClearList: Boolean);

function GetFileVersion(const FileName: string): string;
function GetBuildVersion(const FileName: string): string;

function DeleteFile(const AFile: string): Boolean;

implementation

function TrueFilename(AFileName: string): string;
const
  ForbiddenChars: set of AnsiChar = ['/', ':', '*', '<', '>', '|', #39, '\', '?'];
var
  I: Integer;
begin
  for I := length(AFileName) downto 1 do
    if CharInSet(AFileName[I], ForbiddenChars) then
      Delete(AFileName, I, 1);
  Result := AFileName;
end;

function GetMD5FromFile(FileName: string): string;
begin
  Result := string(THash_MD5.CalcFile(FileName, TFormat_HEX));
end;

function GetFileSize(const FileName: string): Int64;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    try
      Result := FileStream.Size;
    except
      Result := 0;
    end;
  finally
    FileStream.Free;
  end;
end;
{$WARN SYMBOL_PLATFORM OFF}

procedure GetDirectoriesFromDirectory(Directory: string; const List: TStrings; ClearList: Boolean);

  function SlashSep(const Path, S: string): string;
  begin
    if AnsiLastChar(Path)^ <> '\' then
      Result := Path + '\' + S
    else
      Result := Path + S;
  end;

var
  Status: Integer;
  SearchRec: TSearchRec;
begin
  List.BeginUpdate;
  try
    if ClearList then
      List.Clear;
    if (Directory = '') then
      Exit;
    if Directory[length(Directory)] <> '\' then
      Directory := Directory + '\';

    Status := FindFirst(SlashSep(Directory, '*.*'), faAnyFile xor faSysFile, SearchRec);
    try
      while Status = 0 do
      begin
        if (SearchRec.Attr and faDirectory = faDirectory) then
        begin
          if (SearchRec.name <> '.') and (SearchRec.name <> '..') then
          begin
            List.Add(SearchRec.name);
          end;
        end;
        Status := FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;

  finally
    List.EndUpdate;
  end;
end;
{$WARN SYMBOL_PLATFORM ON}

procedure GetFilesInDirectory(Directory: string; const Mask: string; List: TStrings; WithPath, WithSubDirs, WithFileExt, ClearList: Boolean);

  procedure ScanDir(const Directory: string);
  var
    SR: TSearchRec;
    FileName: string;
  begin
    if (FindFirst(Directory + Mask, faAnyFile and not faDirectory, SR) = 0) then
      try
        repeat
          FileName := SR.name;
          if WithPath then
            FileName := Directory + FileName;
          if not WithFileExt then
            FileName := ChangeFileExt(FileName, '');
          List.Add(FileName);
        until (FindNext(SR) <> 0);
      finally
        FindClose(SR);
      end;

    if WithSubDirs then
    begin
      if (FindFirst(Directory + Mask, faAnyFile, SR) = 0) then
        try
          repeat
            if ((SR.Attr and faDirectory) = faDirectory) and (SR.name <> '.') and (SR.name <> '..') then
              ScanDir(Directory + SR.name + '\');
          until (FindNext(SR) <> 0);
        finally
          FindClose(SR);
        end;
    end;
  end;

begin
  List.BeginUpdate;
  try
    if ClearList then
      List.Clear;
    if (Directory = '') then
      Exit;
    if Directory[length(Directory)] <> '\' then
      Directory := Directory + '\';
    ScanDir(Directory);
  finally
    List.EndUpdate;
  end;
end;

function GetFileVersion(const FileName: string): string;
var
  VersionInfoSize, VersionInfoValueSize, Zero: DWord;
  VersionInfo, VersionInfoValue: Pointer;
begin
  { Ist Datei nicht vorhanden, dann Hinweis und raus aus Funktion ... }
  if not FileExists(FileName) then
  begin
    Result := '-1'; { alternativ auch 'File not found' oder sonstwas }
    Exit;
  end;

  { sonnst weiter. }
  Result := '';
  VersionInfoSize := GetFileVersionInfoSize(PChar(FileName), Zero);
  if VersionInfoSize = 0 then
    Exit;
  { Bei nicht genug Speicher wird EOutOfMemory-Exception ausgelöst }
  GetMem(VersionInfo, VersionInfoSize);
  try
    if GetFileVersionInfo(PChar(FileName), 0, VersionInfoSize, VersionInfo) and VerQueryValue(VersionInfo, '\'
      { root block } , VersionInfoValue, VersionInfoValueSize) and (0 <> LongInt(VersionInfoValueSize)) then
    begin
      with TVSFixedFileInfo(VersionInfoValue^) do
      begin
        Result := IntToStr(HiWord(dwFileVersionMS));
        Result := Result + '.' + IntToStr(LoWord(dwFileVersionMS));
        Result := Result + '.' + IntToStr(HiWord(dwFileVersionLS));
        Result := Result + '.' + IntToStr(LoWord(dwFileVersionLS));
      end; { with }
    end; { then }
  finally
    FreeMem(VersionInfo);
  end; { try }
end; { GetFileVersion }

function GetBuildVersion(const FileName: string): string;
begin
  Result := GetFileVersion(FileName);
  Result := copy(Result, LastDelimiter('.', Result) + 1);
end;

function DeleteFile(const AFile: string): Boolean;
var
  sh: SHFileOpStruct;
begin
  ZeroMemory(@sh, sizeof(sh));
  with sh do
  begin
    wFunc := fo_Delete;
    pFrom := PChar(AFile + #0);
    fFlags := fof_Silent or fof_NoConfirmation or fof_NoErrorUI;
  end;
  Result := SHFileOperation(sh) = 0;
end;

end.
