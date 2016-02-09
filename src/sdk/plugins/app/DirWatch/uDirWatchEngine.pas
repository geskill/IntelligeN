unit uDirWatchEngine;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants, Dialogs, Messages,
  // DirectoryWatch
  Cromis.DirectoryWatch,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // API
  uApiIScriptParser,
  // Utils
  uPathUtils,
  //
  uDirWatchSettings;

type
  TDirWatchEngine = class
  private
    FDirectoryWatch: TDirectoryWatch;
    FActive: Boolean;
    FPageController: IPageController;
    FDirWatchSettings: TDirWatchSettings;
    FCurrentPath: string;
  protected
    procedure OnError(const Sender: TObject; const ErrorCode: Integer; const ErrorMessage: string);
    procedure OnNotify(const Sender: TObject; const Action: TWatchAction; const FileName: string);
    procedure SetActive(AActive: Boolean);
  public
    constructor Create(const APageController: IPageController; const ADirWatchSettings: TDirWatchSettings);
    destructor Destroy; override;

    property Active: Boolean read FActive write SetActive;
  end;

implementation

function GetModulePath: string;
var
  QueryRes: TMemoryBasicInformation;
  LBuffer: string;
begin
  VirtualQuery(@GetModulePath, QueryRes, SizeOf(QueryRes));
  SetLength(LBuffer, MAX_PATH);
  SetLength(LBuffer, GetModuleFileName(Cardinal(QueryRes.AllocationBase), PChar(LBuffer), Length(LBuffer)));
  Result := LBuffer;
end;

{ TDirWatchEngine }

procedure TDirWatchEngine.OnError(const Sender: TObject; const ErrorCode: Integer; const ErrorMessage: string);
begin
  //
end;

procedure TDirWatchEngine.OnNotify(const Sender: TObject; const Action: TWatchAction; const FileName: string);
begin
  if Action in [waAdded] then
  begin
    FPageController.OpenToNewTab(IncludeTrailingPathDelimiter(FCurrentPath) + FileName);
  end;
end;

procedure TDirWatchEngine.SetActive(AActive: Boolean);
begin
  if not(AActive = FActive) then
  begin

    if not AActive then
    begin
      FDirectoryWatch.Stop;
    end
    else
    begin
      FCurrentPath := PathCombineEx(GetModulePath, FDirWatchSettings.DirWatchPath);

      if IsDirectory(FCurrentPath) then
      begin
        FDirectoryWatch.WatchSubTree := FDirWatchSettings.WatchSubdirectories;
        FDirectoryWatch.Directory := FCurrentPath;
        FDirectoryWatch.Start;
      end
      else
      begin
        MessageDlg('The defined path is not a directory! Not possible to start the watching.', mtError, [mbOK], 0);
        Exit;
      end;

    end;

    FActive := AActive;
  end;
end;

constructor TDirWatchEngine.Create(const APageController: IPageController; const ADirWatchSettings: TDirWatchSettings);
begin
  inherited Create;

  FDirectoryWatch := TDirectoryWatch.Create;
  FDirectoryWatch.OnNotify := OnNotify;
  FDirectoryWatch.OnError := OnError;

  FActive := False;

  FPageController := APageController;
  FDirWatchSettings := ADirWatchSettings;
end;

destructor TDirWatchEngine.Destroy;
begin
  Active := False;

  FDirWatchSettings := nil;
  FPageController := nil;
  FreeAndNil(FDirectoryWatch);
  inherited Destroy;
end;

end.
