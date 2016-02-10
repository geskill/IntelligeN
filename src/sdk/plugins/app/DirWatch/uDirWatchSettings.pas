unit uDirWatchSettings;

interface

uses
  // Delphi
  SysUtils, Classes,
  // JsonDataObjects
  JsonDataObjects;

type
  TDirWatchSettings = class(TPersistent)
  private
    FSettingsFileName: string;
  protected
    FDirWatchPath, FPublishCustomCheckScriptFile: string;
    FLoadFilesOnlyOnce, FLoadAlreadyExistingFiles, FWatchSubdirectories, { . }
    FLoadOnyXMLFiles, FLoadOnlyIntelligeNXML2Files, { . }
    FRunCrawlers, FRunCrypters, FRunSave, FRunPublish, FRunPublishOnlyWithCustomCheck: Boolean;
  public
    constructor Create(const ASettingsFileName: TFileName);
    destructor Destroy; override;

    procedure LoadSettings;
    procedure LoadDefaultSettings;
    procedure SaveSettings;
  published
    property DirWatchPath: string read FDirWatchPath write FDirWatchPath;
    property LoadFilesOnlyOnce: Boolean read FLoadFilesOnlyOnce write FLoadFilesOnlyOnce;
    property LoadAlreadyExistingFiles: Boolean read FLoadAlreadyExistingFiles write FLoadAlreadyExistingFiles;
    property WatchSubdirectories: Boolean read FWatchSubdirectories write FWatchSubdirectories;
    property LoadOnyXMLFiles: Boolean read FLoadOnyXMLFiles write FLoadOnyXMLFiles;
    property LoadOnlyIntelligeNXML2Files: Boolean read FLoadOnlyIntelligeNXML2Files write FLoadOnlyIntelligeNXML2Files;
    property RunCrawlers: Boolean read FRunCrawlers write FRunCrawlers;
    property RunCrypters: Boolean read FRunCrypters write FRunCrypters;
    property RunSave: Boolean read FRunSave write FRunSave;
    property RunPublish: Boolean read FRunPublish write FRunPublish;
    property RunPublishOnlyWithCustomCheck: Boolean read FRunPublishOnlyWithCustomCheck write FRunPublishOnlyWithCustomCheck;
    property PublishCustomCheckScriptFile: string read FPublishCustomCheckScriptFile write FPublishCustomCheckScriptFile;
  end;

implementation

{ TDirWatchSettings }

constructor TDirWatchSettings.Create(const ASettingsFileName: TFileName);
begin
  inherited Create;
  FSettingsFileName := ASettingsFileName;

  LoadSettings;
end;

destructor TDirWatchSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TDirWatchSettings.LoadSettings;
var
  LJsonObject: TJsonObject;
begin
  if FileExists(FSettingsFileName) then
  begin
    LJsonObject := TJsonObject.Create;
    try
      LJsonObject.LoadFromFile(FSettingsFileName);
      LJsonObject.ToSimpleObject(Self);
    finally
      LJsonObject.Free;
    end;
  end
  else
  begin
    LoadDefaultSettings;
  end;
end;

procedure TDirWatchSettings.LoadDefaultSettings;
begin
  FDirWatchPath := '';
  FLoadFilesOnlyOnce := True;
  FLoadAlreadyExistingFiles := True;
  FWatchSubdirectories := True;
  FLoadOnyXMLFiles := True;
  FLoadOnlyIntelligeNXML2Files := True;
  FRunCrawlers := True;
  FRunCrypters := True;
  FRunSave := True;
  FRunPublish := True;
  FRunPublishOnlyWithCustomCheck := False;
  FPublishCustomCheckScriptFile := '';
end;

procedure TDirWatchSettings.SaveSettings;
var
  LJsonObject: TJsonObject;
begin
  LJsonObject := TJsonObject.Create;
  try
    LJsonObject.FromSimpleObject(Self);
    LJsonObject.SaveToFile(FSettingsFileName, False);
  finally
    LJsonObject.Free;
  end;
end;

end.
