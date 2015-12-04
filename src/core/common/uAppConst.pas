{ ********************************************************
  *                                     IntelligeN CORE  *
  *  Application constants                               *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uAppConst;

interface

uses
  // Delphi
  SysUtils,
  // Spring Framework
  Spring.SystemUtils,
  // Common
  uBaseConst;

const
  Homepage = 'http://intelligen2009.com/';

  BackupFilename = 'releases.sdb';

type
  RTemplateFileInfo = packed record
    TemplateType: TTypeID;
    FileName: WideString;
    Checksum: WideString;
  end;

  RIScriptResult = packed record
    CompiledText: WideString;
    HasError: Boolean;
    X, Y: Integer;
    ErrorMessage, ErrorUnit: WideString;
    procedure Init;
  end;

  TPictureInfo = packed record
    Picture: Variant;
    Downloaded: WordBool;
    Size, Width, Height: Integer;

    procedure Clear;
  end;
  { ****************************************************************************** }
{$REGION 'Documentation'}
  /// <remarks>
  /// Expects AStringTypeID in format "Audio" not "cAudio"
  /// </remarks>
{$ENDREGION}
function StringInTypeID(AStringTypeID: string): Boolean;
{$REGION 'Documentation'}
/// <remarks>
/// Expects AStringTypeID in format "Audio" not "cAudio"
/// </remarks>
{$ENDREGION}
function StringToTypeID(AStringTypeID: string): TTypeID;
{$REGION 'Documentation'}
/// <remarks>
/// Returns TTypeID in format "Audio" not "cAudio"
/// </remarks>
{$ENDREGION}
function TypeIDToString(ATypeID: TTypeID): string;
{$REGION 'Documentation'}
/// <remarks>
/// Expects AStringContentStatus in format "NotChecked" not "csNotChecked"
/// </remarks>
{$ENDREGION}
function StringToContentStatus(AStringContentStatus: string): TContentStatus;
{$REGION 'Documentation'}
/// <remarks>
///   Returns TContentStatus in format "NotChecked" not "csNotChecked" <br />
/// </remarks>
{$ENDREGION}
function ContentStatusToString(AContentStatus: TContentStatus): string;
{$REGION 'Documentation'}
/// <remarks>
/// Expects AStringControlID in format "IReleaseName" not "cReleaseName"
/// </remarks>
{$ENDREGION}
function StringInControlID(AStringControlID: string): Boolean;
{$REGION 'Documentation'}
/// <remarks>
/// Expects AStringControlID in format "IReleaseName" not "cReleaseName"
/// </remarks>
{$ENDREGION}
function StringToControlID(AStringControlID: string): TControlID;
{$REGION 'Documentation'}
/// <remarks>
/// Returns TControlID in format "IReleaseName" not "cReleaseName"
/// </remarks>
{$ENDREGION}
function ControlIDToString(AControlID: TControlID): string;
{$REGION 'Documentation'}
/// <remarks>
/// Returns TControlID in format "ReleaseName" not "IReleaseName" or "cReleaseName"
/// </remarks>
{$ENDREGION}
function ControlIDToReadableString(AControlID: TControlID): string;

implementation

function StringInTypeID(AStringTypeID: string): Boolean;
var
  LTypeID: TTypeID;
begin
  Result := TEnum.TryParse<TTypeID>('c' + AStringTypeID, LTypeID);
end;

function StringToTypeID(AStringTypeID: string): TTypeID;
begin
  if not TEnum.TryParse<TTypeID>('c' + AStringTypeID, Result) then
    raise Exception.Create('Unknown type id');
end;

function TypeIDToString(ATypeID: TTypeID): string;
begin
  Result := copy(TEnum.GetName<TTypeID>(ATypeID), 2, MaxInt);
end;

function StringToContentStatus(AStringContentStatus: string): TContentStatus;
begin
  if not TEnum.TryParse<TContentStatus>('cs' + AStringContentStatus, Result) then
    raise Exception.Create('Unknown content status id');
end;

function ContentStatusToString(AContentStatus: TContentStatus): string;
begin
  Result := copy(TEnum.GetName<TContentStatus>(AContentStatus), 3, MaxInt);
end;

function StringInControlID(AStringControlID: string): Boolean;
var
  LControlID: TControlID;
begin
  Result := TEnum.TryParse<TControlID>('c' + copy(AStringControlID, 2, MaxInt), LControlID);
end;

function StringToControlID(AStringControlID: string): TControlID;
begin
  if not TEnum.TryParse<TControlID>('c' + copy(AStringControlID, 2, MaxInt), Result) then
    raise Exception.Create('Unknown control id');
end;

function ControlIDToString(AControlID: TControlID): string;
begin
  Result := 'I' + copy(TEnum.GetName<TControlID>(AControlID), 2, MaxInt);
end;

function ControlIDToReadableString(AControlID: TControlID): string;
begin
  Result := copy(ControlIDToString(AControlID), 2, MaxInt);
end;

{ RIScriptResult }

procedure RIScriptResult.Init;
begin
  CompiledText := '';
  HasError := False;
  X := 0;
  Y := 0;
  ErrorMessage := '';
  ErrorUnit := '';
end;

{ TPictureInfo }

procedure TPictureInfo.Clear;
begin
  Finalize(Self);
  FillChar(Self, SizeOf(Self), 0);
end;

end.
