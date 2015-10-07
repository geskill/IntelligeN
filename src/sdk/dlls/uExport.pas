unit uExport;

interface

uses
  // Common
  uBaseConst, uAppConst;

const
  ProgrammLibrary = Programm + '.dll';

// Exports
//
// IntelligeN
function IsPortable: WordBool; stdcall; external ProgrammLibrary name 'IsPortable';

function GetRootDir: WideString; stdcall; external ProgrammLibrary name 'GetRootDir';

function GetCommonDataDir: WideString; stdcall; external ProgrammLibrary name 'GetCommonDataDir';
function GetPersonalDataDir: WideString; stdcall; external ProgrammLibrary name 'GetPersonalDataDir';
function GetHiddenDataDir: WideString; stdcall; external ProgrammLibrary name 'GetHiddenDataDir';

function GetConfigurationFolder: WideString; stdcall; external ProgrammLibrary name 'GetConfigurationFolder';
function GetPluginFolder: WideString; stdcall; external ProgrammLibrary name 'GetPluginFolder';
function GetSettingsFolder: WideString; stdcall; external ProgrammLibrary name 'GetSettingsFolder';
function GetTemplatesCMSFolder: WideString; stdcall; external ProgrammLibrary name 'GetTemplatesCMSFolder';
function GetTemplatesSiteFolder: WideString; stdcall; external ProgrammLibrary name 'GetTemplatesSiteFolder';
function GetTemplatesTypeFolder: WideString; stdcall; external ProgrammLibrary name 'GetTemplatesTypeFolder';

function GetPathFromFileSystemID(AFileSystem: TFileSystem): WideString; stdcall; external ProgrammLibrary name 'GetPathFromFileSystemID';
function GetFileSystemIDFromPath(AFileName: WideString): TFileSystem; stdcall; external ProgrammLibrary name 'GetFileSystemIDFromPath';

// IXML
function GetFileInfo(AFileName: WideString): RTemplateFileInfo; stdcall; external 'IXML.dll' name 'GetFileInfo';

implementation

end.
