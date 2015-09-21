unit uConst;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils;

type
  TViewType = (vtData, vtCode, vtPreview);

  TTemplateTypeID = (cAudio, cGameCube, cMovie, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cSoftware, cWii, cXbox, cXbox360,
    cXXX, cOther);

  TTemplateTypeIDs = set of TTemplateTypeID;

const
  cGames: TTemplateTypeIDs = [cGameCube, cNintendoDS, cPCGames, cPlayStation2, cPlayStation3, cPlayStationPortable, cWii, cXbox, cXbox360];
  TStringTemplateTypeID: array [0 .. 13] of string = ('Audio', 'GameCube', 'Movie', 'NintendoDS', 'PCGames', 'PlayStation2', 'PlayStation3',
    'PlayStationPortable', 'Software', 'Wii', 'Xbox', 'Xbox360', 'XXX', 'Other');

type
  TComponentID = (cReleaseName, cReleaseDate, cTags, cTitle, cArtist, cPicture, cTrailer, cSample, cNotes, cPassword, cAudioBitrate, cAudioBitrateType,
    cAudioEncoder, cAudioSamplingRate, cAudioStream, cGenre, cLanguage, cRuntime, cVideoCodec, cVideoStream, cVideoSystem, cNFO, cDescription);

  TComponentIDs = set of TComponentID;

const
  TStringComponentID: array [0 .. 22] of string = ('IReleaseName', 'IReleaseDate', 'ITags', 'ITitle', 'IArtist', 'IPicture', 'ITrailer', 'ISample', 'INotes',
    'IPassword', 'IAudioBitrate', 'IAudioBitrateType', 'IAudioEncoder', 'IAudioSamplingRate', 'IAudioStream', 'IGenre', 'ILanguage', 'IRuntime', 'IVideoCodec',
    'IVideoStream', 'IVideoSystem', 'INFO', 'IDescription');

type
  RIScirptResult = packed record
    CompiledText: WideString;
    HasError: Boolean;
    X, Y: Integer;
    ErrorMessage, ErrorUnit: WideString;
    procedure Init;
  end;

{$IFDEF MAINAPP}

  TPictureInfo = packed record
    Picture: Variant;
    Downloaded: WordBool;
    Size, Width, Height: Integer;

    procedure Clear;
  end;
{$ENDIF}
  { ****************************************************************************** }

function StringToTTemplateTypeID(StringTemplateTypeID: string): TTemplateTypeID;
function TTemplateTypeIDToString(TemplateTypeID: TTemplateTypeID): string;

function StringToTComponentID(StringComponentID: string): TComponentID;
function TComponentIDToString(ComponentID: TComponentID): string;
function TComponentIDToReadableStringComponentID(ComponentID: TComponentID): string;

implementation

function StringToTTemplateTypeID(StringTemplateTypeID: string): TTemplateTypeID;
var
  I: Integer;
begin
  I := IndexStr(StringTemplateTypeID, TStringTemplateTypeID);

  if not(I = -1) then
    result := TTemplateTypeID(I)
  else
    raise Exception.Create('Unknown template type id');
end;

function TTemplateTypeIDToString(TemplateTypeID: TTemplateTypeID): string;
begin
  result := TStringTemplateTypeID[Integer(TemplateTypeID)];
end;

function StringToTComponentID(StringComponentID: string): TComponentID;
begin
  try
    result := TComponentID(IndexStr(StringComponentID, TStringComponentID));
  except
    raise Exception.Create('Unknown component id');
  end;
end;

function TComponentIDToString(ComponentID: TComponentID): string;
begin
  result := TStringComponentID[Integer(ComponentID)];
end;

function TComponentIDToReadableStringComponentID(ComponentID: TComponentID): string;
begin
  result := copy(TStringComponentID[Integer(ComponentID)], 2, MaxInt);
end;

{ RIScirptResult }

procedure RIScirptResult.Init;
begin
  CompiledText := '';
  HasError := False;
  X := 0;
  Y := 0;
  ErrorMessage := '';
  ErrorUnit := '';
end;

{$IFDEF MAINAPP}
{ TPictureInfo }

procedure TPictureInfo.Clear;
begin
  Finalize(Self);
  FillChar(Self, SizeOf(Self), 0);
end;
{$ENDIF}

end.
