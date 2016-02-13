unit uCustomScriptSettings;

interface

uses
  // Delphi
  SysUtils, Classes,
  // JsonDataObjects
  JsonDataObjects,
  // Plugin system
  uPlugInAppSettingsBase;

type
  TCustomScriptSettings = class(TPlugInAppSettingsBase)
  protected
    FScriptFileName: string;
    FOverrideValue: Boolean;
  public
    procedure LoadDefaultSettings; override;
  published
    property ScriptFileName: string read FScriptFileName write FScriptFileName;
    property OverrideValue: Boolean read FOverrideValue write FOverrideValue;
  end;

implementation

{ TCustomScriptSettings }

procedure TCustomScriptSettings.LoadDefaultSettings;
begin
  FScriptFileName := '';
  FOverrideValue := True;
end;

end.
