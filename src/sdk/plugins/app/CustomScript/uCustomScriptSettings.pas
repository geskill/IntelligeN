unit uCustomScriptSettings;

interface

uses
  // Delphi
  SysUtils, Classes, IniFiles;

type
  TCustomScriptSettings = class(TPersistent)
  private
    FSettingsFileName: string;
    FIniFile: TIniFile;
  protected

    FScriptFileName: string;
    FOverrideValue: Boolean;
  public
    constructor Create(const ASettingsFileName: TFileName);
    destructor Destroy; override;

    procedure LoadSettings;
    procedure SaveSettings;
  published
    property ScriptFileName: string read FScriptFileName write FScriptFileName;
    property OverrideValue: Boolean read FOverrideValue write FOverrideValue;
  end;

implementation

{ TCustomScriptSettings }

constructor TCustomScriptSettings.Create(const ASettingsFileName: TFileName);
begin
  inherited Create;
  FIniFile := TIniFile.Create(ASettingsFileName);
  FSettingsFileName := ASettingsFileName;
  FScriptFileName := '';
  FOverrideValue := True;

  LoadSettings;
end;

destructor TCustomScriptSettings.Destroy;
begin
  FIniFile.Free;
  inherited Destroy;
end;

procedure TCustomScriptSettings.LoadSettings;
begin
  ScriptFileName := FIniFile.ReadString('SETTINGS', 'ScriptFileName', '');
  OverrideValue := FIniFile.ReadBool('SETTINGS', 'OverrideValue', True);
end;

procedure TCustomScriptSettings.SaveSettings;
begin
  FIniFile.WriteString('SETTINGS', 'ScriptFileName', ScriptFileName);
  FIniFile.WriteBool('SETTINGS', 'OverrideValue', OverrideValue);
end;

end.
