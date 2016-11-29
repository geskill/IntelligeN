unit uDirWatchSettings;

interface

uses
  // Delphi
  SysUtils, Classes,
  // JsonDataObjects
  JsonDataObjects,
  // Plugin system
  uPlugInAppSettingsBase;

type
  TDirWatchSettings = class(TPlugInAppSettingsBase)
  protected
    FDirWatchPath, FPublishCustomCheckScriptFile: string;
    FLoadFilesOnlyOnce, FLoadAlreadyExistingFiles, FWatchSubdirectories, { . }
    FLoadOnyXMLFiles, FLoadOnlyIntelligeNXML2Files, FCloseTabAfterPublish, { . }
    FRunCrawlers, FRunCrypters, FRunSave, FRunPublish, FRunPublishOnlyWithCustomCheck: Boolean;
  public
    procedure LoadDefaultSettings; override;
  published
    property DirWatchPath: string read FDirWatchPath write FDirWatchPath;
    property LoadFilesOnlyOnce: Boolean read FLoadFilesOnlyOnce write FLoadFilesOnlyOnce;
    property LoadAlreadyExistingFiles: Boolean read FLoadAlreadyExistingFiles write FLoadAlreadyExistingFiles;
    property WatchSubdirectories: Boolean read FWatchSubdirectories write FWatchSubdirectories;
    property LoadOnyXMLFiles: Boolean read FLoadOnyXMLFiles write FLoadOnyXMLFiles;
    property LoadOnlyIntelligeNXML2Files: Boolean read FLoadOnlyIntelligeNXML2Files write FLoadOnlyIntelligeNXML2Files;
    property CloseTabAfterPublish: Boolean read FCloseTabAfterPublish write FCloseTabAfterPublish;
    property RunCrawlers: Boolean read FRunCrawlers write FRunCrawlers;
    property RunCrypters: Boolean read FRunCrypters write FRunCrypters;
    property RunSave: Boolean read FRunSave write FRunSave;
    property RunPublish: Boolean read FRunPublish write FRunPublish;
    property RunPublishOnlyWithCustomCheck: Boolean read FRunPublishOnlyWithCustomCheck write FRunPublishOnlyWithCustomCheck;
    property PublishCustomCheckScriptFile: string read FPublishCustomCheckScriptFile write FPublishCustomCheckScriptFile;
  end;

implementation

{ TDirWatchSettings }

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

end.
