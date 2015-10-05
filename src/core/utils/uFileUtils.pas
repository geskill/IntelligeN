unit uFileUtils;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, ShellAPI,
  // DEC
  DECHash, DECFmt;

function TrueFilename(AFileName: string): string;

function GetMD5FromFile(FileName: string): string; deprecated;

function GetFileSize(const FileName: string): Int64; deprecated;

procedure GetDirectoriesFromDirectory(Directory: string; const List: TStrings; ClearList: Boolean); deprecated;

procedure GetFilesInDirectory(Directory: string; const Mask: string; List: TStrings; WithPath, WithSubDirs, WithFileExt, ClearList: Boolean); deprecated;

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

// TODO: FUTURE: replace with Spring.Cryptography.MD5
function GetMD5FromFile(FileName: string): string;
begin
  Result := string(THash_MD5.CalcFile(FileName, TFormat_HEX));
end;

// TODO: FUTURE: replace with Spring.Utils.IO
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

// TODO: FUTURE: replace with Spring.Utils.IO
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

// TODO: FUTURE: replace with Spring.Utils.IO
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
