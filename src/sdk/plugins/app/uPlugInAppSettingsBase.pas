{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn application settings base class              *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInAppSettingsBase;

interface

uses
  // Delphi
  SysUtils, Classes,
  // JsonDataObjects
  JsonDataObjects,
  // Utils
  uSystemUtils;

type
  TPlugInAppSettingsBase = class(TPersistent)
  private
    FFileName: string;
  public
    constructor Create(const AFileName: TFileName = '');
    destructor Destroy; override;

    procedure LoadSettings;
    procedure LoadDefaultSettings; virtual; abstract;
    procedure SaveSettings;
  published
  end;

implementation

{ TCustomScriptSettings }

constructor TPlugInAppSettingsBase.Create(const AFileName: TFileName);
begin
  inherited Create;
  if SameStr('', AFileName) then
    FFileName := ExtractFilePath(GetModulePath) + ChangeFileExt(ExtractFileName(GetModulePath), '.json')
  else
    FFileName := AFileName;

  LoadSettings;
end;

destructor TPlugInAppSettingsBase.Destroy;
begin
  inherited Destroy;
end;

procedure TPlugInAppSettingsBase.LoadSettings;
var
  LJsonObject: TJsonObject;
begin
  if FileExists(FFileName) then
  begin
    LJsonObject := TJsonObject.Create;
    try
      LJsonObject.LoadFromFile(FFileName);
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

procedure TPlugInAppSettingsBase.SaveSettings;
var
  LJsonObject: TJsonObject;
begin
  LJsonObject := TJsonObject.Create;
  try
    LJsonObject.FromSimpleObject(Self);
    LJsonObject.SaveToFile(FFileName, False);
  finally
    LJsonObject.Free;
  end;
end;

end.
