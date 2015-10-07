unit uApiMain;

interface

uses
  // Delphi
  Windows, SysUtils, Forms, Controls, Classes, StdCtrls, ExtCtrls, Math, NB30,
  // Spring Framework
  Spring.Utils,
  // DLLs
  uExport,
  // Api
  uApiSettings,
  // Utils
  uFileUtils, uPathUtils;

function GetTemplateList: TStrings;

function ExWindows(const AFlag: Word): Boolean;
function GetMACAdress: string;
function GetWindowsUserName: string;
function GetWindowsComputerName: string;
function GetVolumeID: string;

procedure AnalyzeStartupParams; overload;
procedure AnalyzeStartupParams(const StartupParams: array of string); overload;

var
  MINOR_VERSION: Integer;

implementation

uses
  uMain;

function GetTemplateList: TStrings;
begin
  result := TStringList.Create;
  GetFilesInDirectory(GetTemplatesTypeFolder, '*.xml', result, false, false, false, true);
end;

function ExWindows(const AFlag: Word): Boolean;
var
  vi: TOSVersionInfo;
  hToken: THandle;
  tp: TTokenPrivileges;
  h: DWord;
begin
  result := false;

  vi.dwOSVersionInfoSize := SizeOf(vi);

  if GetVersionEx(vi) then
  begin
    if vi.dwPlatformId = VER_PLATFORM_WIN32_NT then
    begin
      // Windows NT
      // Achtung bei Delphi 2 muß @hToken stehen ...
      if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, hToken) then
      begin
        LookupPrivilegeValue(nil, 'SeShutdownPrivilege', tp.Privileges[0].Luid);
        tp.PrivilegeCount := 1;
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        h := 0;
        AdjustTokenPrivileges(hToken, false, tp, 0, PTokenPrivileges(nil)^, h);
        CloseHandle(hToken);
        result := ExitWindowsEx(AFlag, 0);
      end;
    end
    else
    begin // Windows 95
      result := ExitWindowsEx(AFlag, 0);
    end;
  end;
end;

function GetMACAdress: string;
const
  NO_MAC_ADDRESS = '00-00-00-00-00-00';
var
  NCB: PNCB;
  Adapter: PAdapterStatus;

  RetCode: Ansichar;
  I: integer;
  Lenum: PlanaEnum;
  _SystemID: string;
begin
  result := NO_MAC_ADDRESS;
  _SystemID := '';
  Getmem(NCB, SizeOf(TNCB));
  Fillchar(NCB^, SizeOf(TNCB), 0);

  Getmem(Lenum, SizeOf(TLanaEnum));
  Fillchar(Lenum^, SizeOf(TLanaEnum), 0);

  Getmem(Adapter, SizeOf(TAdapterStatus));
  Fillchar(Adapter^, SizeOf(TAdapterStatus), 0);

  Lenum.Length := chr(0);
  NCB.ncb_command := chr(NCBENUM);
  NCB.ncb_buffer := Pointer(Lenum);
  NCB.ncb_length := SizeOf(Lenum);
  RetCode := Netbios(NCB);

  I := 0;
  repeat
    Fillchar(NCB^, SizeOf(TNCB), 0);
    NCB.ncb_command := chr(NCBRESET);
    NCB.ncb_lana_num := Lenum.lana[I];
    RetCode := Netbios(NCB);

    Fillchar(NCB^, SizeOf(TNCB), 0);
    NCB.ncb_command := chr(NCBASTAT);
    NCB.ncb_lana_num := Lenum.lana[I];
    // Must be 16
    NCB.ncb_callname := '*               ';

    NCB.ncb_buffer := Pointer(Adapter);

    NCB.ncb_length := SizeOf(TAdapterStatus);
    RetCode := Netbios(NCB);
    // ---- calc _systemId from mac-address[2-5] XOR mac-address[1]...
    if (RetCode = chr(0)) or (RetCode = chr(6)) then
    begin
      _SystemID := IntToHex(Ord(Adapter.adapter_address[0]), 2) + '-' + IntToHex(Ord(Adapter.adapter_address[1]), 2) + '-' + IntToHex
        (Ord(Adapter.adapter_address[2]), 2) + '-' + IntToHex(Ord(Adapter.adapter_address[3]), 2) + '-' + IntToHex(Ord(Adapter.adapter_address[4]), 2)
        + '-' + IntToHex(Ord(Adapter.adapter_address[5]), 2);
    end;
    Inc(I);
  until (I >= Ord(Lenum.Length)) or (_SystemID <> NO_MAC_ADDRESS);
  FreeMem(NCB);
  FreeMem(Adapter);
  FreeMem(Lenum);
  if not(_SystemID = '') then
    result := _SystemID;
end;

function GetWindowsUserName: string;
var
  Buffer: array [0 .. MAX_PATH + 1] of Char;
  Size: DWord;
begin
  Size := 1024;
  GetUserName(Buffer, Size);
  result := StrPas(Buffer);
end;

function GetWindowsComputerName: string;
var
  len: DWord;
begin
  len := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(result, len);
  if not Windows.GetComputerName(PChar(result), len) then
    RaiseLastOSError;
  SetLength(result, len);
end;

function GetVolumeID: string;
var
  pRootPathName: PChar;
  pVolumeNameBuffer: array [0 .. MAX_PATH] of Char;
  iVolumeSerialNumber: Cardinal;
  iMaximumComponentLength, iFileSystemFlags: DWord;
begin
  iVolumeSerialNumber := 0;
  pRootPathName := PChar(GetEnvironmentVariable('systemdrive'));
  GetVolumeInformation(pRootPathName, pVolumeNameBuffer, SizeOf(pVolumeNameBuffer) - 1, @iVolumeSerialNumber, iMaximumComponentLength, iFileSystemFlags, nil,
    0);
  result := Format('%.8x', [iVolumeSerialNumber]);
end;

procedure AnalyzeStartupParams;
var
  I: integer;
  Params: array of string;
begin
  for I := 1 to ParamCount do
  begin
    SetLength(Params, I);
    Params[I - 1] := ParamStr(I);
  end;

  AnalyzeStartupParams(Params);
end;

procedure AnalyzeStartupParams(const StartupParams: array of string);
const
  Params: array [0 .. 2] of string = ('openfile', 'deletesettings', 'close');
var
  I, J: integer;
begin

  for I := 0 to Length(StartupParams) - 1 do
    for J := 0 to Length(Params) - 1 do
      if (StartupParams[I] = '/' + Params[J]) or (StartupParams[I] = '-' + Params[J]) or (StartupParams[I] = Params[J]) then
        case J of
          0:
            begin
              if ((I + 1) < Length(StartupParams)) then
                Main.fMain.OpenToNewTab(StartupParams[I + 1])
              else
                Main.fMain.OpenToNewTab();
            end;
          1:
            SettingsManager.DeleteSettings;
          2:
            Application.Terminate;
        end;
end;

initialization
  MINOR_VERSION := TFileVersionInfo.GetVersionInfo(ParamStr(0)).FileVersionNumber.Minor;

end.
