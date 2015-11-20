unit uApiSettingsManager;

interface

uses
  // Delphi
  SysUtils, Classes, ActiveX,
  // DragonSoft XMLSerializer
  uXMLSerializer,
  // Utils
  uFileUtils;

type
  TSettingsManager<T: TPersistent, constructor> = class
  private
  var
    FFirstStart: Boolean;
    FSettingsFolder: TFileName;
    FSettings: T;

  type
    TXMLSerializer = class(uXMLSerializer.TXMLSerializer)
    public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

      procedure LoadObject(const aInstance: TObject);
      procedure SaveObject(const aInstance: TObject);
    end;

  const
    SETTINGS_FILENAME = 'settings.xml';

    function SettingsFileName: TFileName;
    function SettingsFileExists: Boolean;
  protected
    procedure ResetInternalSettings;
    procedure LoadDefaultSettings; virtual;
  public
    constructor Create(ASettingsFolder: TFileName);

    property Settings: T read FSettings write FSettings;

    procedure LoadSettings; virtual;

    procedure SaveSettings;

    procedure DeleteSettings;

    property FirstStart: Boolean read FFirstStart;

    destructor Destroy; override;
  end;

implementation

{ TSettingsManager }

constructor TSettingsManager<T>.TXMLSerializer.Create(AOwner: TComponent);
begin
  CoInitialize(nil);
  inherited Create(AOwner);
end;

destructor TSettingsManager<T>.TXMLSerializer.Destroy;
begin
  inherited Destroy;
  CoUninitialize;
end;

procedure TSettingsManager<T>.TXMLSerializer.LoadObject(const aInstance: TObject);
begin
  inherited LoadObject(aInstance, aInstance.ClassName);
end;

procedure TSettingsManager<T>.TXMLSerializer.SaveObject(const aInstance: TObject);
begin
  inherited SaveObject(aInstance, aInstance.ClassName);
end;

function TSettingsManager<T>.SettingsFileName: TFileName;
begin
  Result := IncludeTrailingPathDelimiter(FSettingsFolder) + SETTINGS_FILENAME;
end;

function TSettingsManager<T>.SettingsFileExists: Boolean;
begin
  Result := FileExists(SettingsFileName);
end;

procedure TSettingsManager<T>.ResetInternalSettings;
begin
  FSettings.Free;
  FSettings := T.Create;
end;

procedure TSettingsManager<T>.LoadDefaultSettings;
begin
  //
end;

constructor TSettingsManager<T>.Create(ASettingsFolder: TFileName);
begin
  inherited Create;
  FFirstStart := False;
  FSettingsFolder := ASettingsFolder;
  ResetInternalSettings;
end;

procedure TSettingsManager<T>.LoadSettings;
var
  XMLSerializer: TXMLSerializer;
begin
  if SettingsFileExists then
  begin
    XMLSerializer := TXMLSerializer.Create(nil);
    try
      with XMLSerializer do
      begin
        // XMLSettings.WellFormated := true;
        SpecialClasses := [scTCollection];
        StorageOptions := StorageOptions + [soIgnoreSetPropertyError];
        LoadFromFile(SettingsFileName);
        LoadObject(Settings);
      end;
    finally
      XMLSerializer.Free;
    end;
  end
  else
  begin
    FFirstStart := True;
    LoadDefaultSettings;
  end;
end;

procedure TSettingsManager<T>.SaveSettings;
var
  XMLSerializer: TXMLSerializer;
begin
  XMLSerializer := TXMLSerializer.Create(nil);
  try
    with XMLSerializer do
    begin
      XMLSerializer.XMLSettings.WellFormated := True;
      // StorageOptions := [soIncludeObjectLinks, soSortProperties];
      SpecialClasses := [scTCollection];
      SaveObject(Settings);
      SaveToFile(SettingsFileName);
    end;
  finally
    XMLSerializer.Free;
  end;
end;

procedure TSettingsManager<T>.DeleteSettings;
begin
  if SettingsFileExists then
    DeleteFile(SettingsFileName);

  LoadSettings;
end;

destructor TSettingsManager<T>.Destroy;
begin
  FSettings.Free;
  inherited Destroy;
end;

end.
