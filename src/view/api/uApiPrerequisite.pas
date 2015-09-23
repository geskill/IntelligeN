unit uApiPrerequisite;

interface

uses
  // Delphi
  SysUtils, Dialogs,
  // DLLs
  uExport,
  // Utils
  uPathUtils;

function GenerateFolderSystem: Boolean;

implementation

function GenerateFolderSystem: Boolean;
begin
  result := ForceDirectories(GetSettingsFolder) and ForceDirectories(GetTemplatesCMSFolder) and ForceDirectories(GetTemplatesSiteFolder) and ForceDirectories
    (GetTemplatesTypeFolder);
end;

end.
