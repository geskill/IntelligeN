library IntelligeN;

uses
  // Delphi
  Windows, SysUtils, Classes, Variants,
  // Common
  uBaseConst,
  // Utils
  uPathUtils, uSystemUtils;

{$R *.res}

const
  ConfigurationFolder: string = 'configuration';
  DocumentsFolder: string = 'documents';
  PluginsFolder: string = 'plugins';
  SettingsFolder: string = 'settings';
  TemplatesFolder: string = 'templates';

function IsPortable: WordBool; stdcall; export;
begin
  Result := {$IFDEF PORTABLE} True {$ELSE} false {$ENDIF};
end;

function GetRootDir: WideString; stdcall; export;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(GetModulePath));
end;

function GetCommonDataDir: WideString; stdcall; export;
begin
  Result := {$IFDEF PORTABLE} GetRootDir {$ELSE} IncludeTrailingPathDelimiter(GetCommonDocumentsFolder + ProgrammName){$ENDIF};
end;

function GetPersonalDataDir: WideString; stdcall; export;
begin
  Result := {$IFDEF PORTABLE} GetRootDir {$ELSE} IncludeTrailingPathDelimiter(GetPersonalFolder + ProgrammName){$ENDIF};
end;

function GetHiddenDataDir: WideString; stdcall; export;
begin
  Result := {$IFDEF PORTABLE} GetPersonalDataDir {$ELSE} IncludeTrailingPathDelimiter(GetPersonalLocalTempPath + ProgrammName){$ENDIF};
end;

function GetConfigurationFolder: WideString; stdcall; export;
begin
  Result := IncludeTrailingPathDelimiter(GetCommonDataDir + ConfigurationFolder);
end;

function GetDocumentsFolder: WideString; stdcall; export;
begin
  Result := IncludeTrailingPathDelimiter(GetPersonalDataDir + DocumentsFolder);
end;

function GetPluginFolder: WideString; stdcall; export;
begin
  Result := IncludeTrailingPathDelimiter(GetCommonDataDir + PluginsFolder);
end;

function GetSettingsFolder: WideString; stdcall; export;
begin
  Result := IncludeTrailingPathDelimiter(GetPersonalDataDir + SettingsFolder);
end;

function GetTemplatesCMSFolder: WideString; stdcall; export;
begin
  Result := IncludeTrailingPathDelimiter(GetCommonDataDir + TemplatesFolder + '_cms');
end;

function GetTemplatesSiteFolder: WideString; stdcall; export;
begin
  Result := IncludeTrailingPathDelimiter(GetCommonDataDir + TemplatesFolder + '_site');
end;

function GetTemplatesTypeFolder: WideString; stdcall; export;
begin
  Result := IncludeTrailingPathDelimiter(GetCommonDataDir + TemplatesFolder + '_type');
end;

{ ****************************************************************************** }

function GetPathFromFileSystemID(const AFileSystem: TFileSystem): WideString; stdcall; export;
begin
  case AFileSystem of
    fsRoot:
      Result := GetRootDir;
    fsConfig:
      Result := GetConfigurationFolder;
    fsPlugins:
      Result := GetPluginFolder;
    fsSettings:
      Result := GetSettingsFolder;
    fsCMS:
      Result := GetTemplatesCMSFolder;
    fsCMSSubject:
      Result := GetTemplatesCMSFolder + 'subject\';
    fsCMSMessage:
      Result := GetTemplatesCMSFolder + 'message\';
    fsSite:
      Result := GetTemplatesSiteFolder;
    fsType:
      Result := GetTemplatesTypeFolder;
  end;
end;

function GetFileSystemIDFromPath(const AFileName: WideString): TFileSystem; stdcall; export;
var
  LFilePath: string;
begin
  LFilePath := IncludeTrailingPathDelimiter(ExtractFilePath(AFileName));

  if Pos(string(GetTemplatesCMSFolder + 'subject\'), LFilePath) > 0 then
    Result := fsCMSSubject
  else if Pos(string(GetTemplatesCMSFolder + 'message\'), LFilePath) > 0 then
    Result := fsCMSMessage
  else if Pos(string(GetTemplatesCMSFolder), LFilePath) > 0 then
    Result := fsCMS
  else if Pos(string(GetTemplatesSiteFolder), LFilePath) > 0 then
    Result := fsSite
  else if Pos(string(GetTemplatesTypeFolder), LFilePath) > 0 then
    Result := fsType
  else if Pos(string(GetSettingsFolder), LFilePath) > 0 then
    Result := fsSettings
  else if Pos(string(GetPluginFolder), LFilePath) > 0 then
    Result := fsPlugins
  else if Pos(string(GetConfigurationFolder), LFilePath) > 0 then
    Result := fsConfig
  else
    Result := fsRoot;
end;

exports { . }
  IsPortable name 'IsPortable', { . }
  GetRootDir name 'GetRootDir', { . }
  GetCommonDataDir name 'GetCommonDataDir', { . }
  GetPersonalDataDir name 'GetPersonalDataDir', { . }
  GetHiddenDataDir name 'GetHiddenDataDir', { . }
  GetConfigurationFolder name 'GetConfigurationFolder', { . }
  GetPluginFolder name 'GetPluginFolder', { . }
  GetDocumentsFolder name 'GetDocumentsFolder', { . }
  GetSettingsFolder name 'GetSettingsFolder', { . }
  GetTemplatesCMSFolder name 'GetTemplatesCMSFolder', { . }
  GetTemplatesSiteFolder name 'GetTemplatesSiteFolder', { . }
  GetTemplatesTypeFolder name 'GetTemplatesTypeFolder', { . }
  GetPathFromFileSystemID name 'GetPathFromFileSystemID', { . }
  GetFileSystemIDFromPath name 'GetFileSystemIDFromPath';

begin

end.
