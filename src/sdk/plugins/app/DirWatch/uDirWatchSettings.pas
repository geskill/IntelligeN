unit uDirWatchSettings;

interface

uses
  // Delphi
  SysUtils, Classes, IniFiles;

type
  TDirWatchSettings = class(TPersistent)
  private
    FSettingsFileName: string;
    FIniFile: TIniFile;
  protected
    FDirWatchPath: string;
    FWatchSubdirectories: Boolean;
  public
    constructor Create(const ASettingsFileName: TFileName);
    destructor Destroy; override;

    procedure LoadSettings;
    procedure SaveSettings;
  published
    property DirWatchPath: string read FDirWatchPath write FDirWatchPath;
    property WatchSubdirectories: Boolean read FWatchSubdirectories write FWatchSubdirectories;
  end;

implementation

{ TDirWatchSettings }

constructor TDirWatchSettings.Create(const ASettingsFileName: TFileName);
begin
  inherited Create;
  FIniFile := TIniFile.Create(ASettingsFileName);
  FSettingsFileName := ASettingsFileName;
  FDirWatchPath := '';
  FWatchSubdirectories := True;

  LoadSettings;
end;

destructor TDirWatchSettings.Destroy;
begin
  FIniFile.Free;
  inherited Destroy;
end;

procedure TDirWatchSettings.LoadSettings;
begin
  DirWatchPath := FIniFile.ReadString('SETTINGS', 'DirWatchPath', '');
  WatchSubdirectories := FIniFile.ReadBool('SETTINGS', 'WatchSubdirectories', True);
end;

procedure TDirWatchSettings.SaveSettings;
begin
  FIniFile.WriteString('SETTINGS', 'DirWatchPath', DirWatchPath);
  FIniFile.WriteBool('SETTINGS', 'WatchSubdirectories', WatchSubdirectories);
end;

end.
