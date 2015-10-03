unit uApiUpdateSettings;

interface

uses
  // Delphi
  SysUtils, Classes, Generics.Collections,
  // Api
  uApiSettingsManager, uApiSettingsInterfacedCollectionItem, uApiUpdateInterface;

type
  TUpdateFileSystemCollectionItem = class(TCollectionItem)
  private
    FLibraryFile: string;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property LibraryFile: string read FLibraryFile write FLibraryFile;
  end;

  TUpdateServerCollectionItem = class(TInterfacedCollectionItem, IUpdateServer)
  private
    FName, FAccessToken: WideString;
  protected
    function GetName: WideString;
    procedure SetName(AName: WideString);
    function GetAccessToken: WideString;
    procedure SetAccessToken(AAccessToken: WideString);
  public

  published
    property Name: WideString read GetName write SetName;
    property AccessToken: WideString read GetAccessToken write SetAccessToken;
  end;

  TUpdateSettings = class(TPersistent)
  private
    FFileSystems, FUpdateServers: TCollection;
  public
    constructor Create;
    function FindFileSystem(ALibraryFile: string): TUpdateFileSystemCollectionItem;
    function GetLibraryFiles: string;
    function FindServer(AServerURL: string): TUpdateServerCollectionItem;
    function GetUpdateServers: string;
    destructor Destroy; override;
  published
    property FileSystems: TCollection read FFileSystems write FFileSystems;
    property UpdateServers: TCollection read FUpdateServers write FUpdateServers;
  end;

var
  SettingsManager: TSettingsManager<TUpdateSettings>;

implementation

{ TUpdateFileSystemCollectionItem }

constructor TUpdateFileSystemCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TUpdateFileSystemCollectionItem.Destroy;
begin
  inherited Destroy;
end;

{ TUpdateServerCollectionItem }

function TUpdateServerCollectionItem.GetName;
begin
  Result := FName;
end;

procedure TUpdateServerCollectionItem.SetName;
begin
  FName := AName;
end;

function TUpdateServerCollectionItem.GetAccessToken;
begin
  Result := FAccessToken;
end;

procedure TUpdateServerCollectionItem.SetAccessToken;
begin
  FAccessToken := AAccessToken;
end;

{ TUpdateSettings }

constructor TUpdateSettings.Create;
begin
  inherited Create;
  FFileSystems := TCollection.Create(TUpdateFileSystemCollectionItem);
  FUpdateServers := TCollection.Create(TUpdateServerCollectionItem);
end;

function TUpdateSettings.FindFileSystem(ALibraryFile: string): TUpdateFileSystemCollectionItem;
var
  LCollectionItem: TCollectionItem;
begin
  Result := nil;
  for LCollectionItem in FFileSystems do
    if SameText(ALibraryFile, TUpdateFileSystemCollectionItem(LCollectionItem).LibraryFile) then
      Exit(TUpdateFileSystemCollectionItem(LCollectionItem));
end;

function TUpdateSettings.GetLibraryFiles: string;
var
  LCollectionItem: TCollectionItem;
begin
  with TStringList.Create do
    try
      for LCollectionItem in FFileSystems do
        Add(TUpdateFileSystemCollectionItem(LCollectionItem).LibraryFile);
      Result := Text;
    finally
      Free;
    end;
end;

function TUpdateSettings.FindServer(AServerURL: string): TUpdateServerCollectionItem;
var
  LCollectionItem: TCollectionItem;
begin
  Result := nil;
  for LCollectionItem in FUpdateServers do
    if SameText(AServerURL, TUpdateServerCollectionItem(LCollectionItem).Name) then
      Exit(TUpdateServerCollectionItem(LCollectionItem));
end;

function TUpdateSettings.GetUpdateServers: string;
var
  LCollectionItem: TCollectionItem;
begin
  with TStringList.Create do
    try
      for LCollectionItem in FUpdateServers do
        Add(TUpdateServerCollectionItem(LCollectionItem).Name);
      Result := Text;
    finally
      Free;
    end;
end;

destructor TUpdateSettings.Destroy;
begin
  FUpdateServers.Free;
  FFileSystems.Free;
  inherited Destroy;
end;

initialization

SettingsManager := TSettingsManager<TUpdateSettings>.Create(ExtractFilePath(ParamStr(0)));
SettingsManager.LoadSettings;

finalization

SettingsManager.SaveSettings;
SettingsManager.Free;

end.
