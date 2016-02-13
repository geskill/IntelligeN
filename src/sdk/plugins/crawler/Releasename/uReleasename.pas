unit uReleasename;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes,
  // Common
  uBaseConst, uBaseInterface,
  // Utils
  uStringUtils,
  // Plugin system
  uPlugInCrawlerClass;

type
  TReleasename = class(TCrawlerPlugIn)
  private
    procedure RemoveDotsOrUnderscores(var AName: string);
    procedure RemoveGroupName(var AName: string);
    procedure RemoveSceneNames(var AName: string);
  protected
    function InSceneString(AStrings: array of string; const AName: string): Boolean;
  public
    function GetName: WideString; override; safecall;

    function InternalGetAvailableTypeIDs: TTypeIDs; override; safecall;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override; safecall;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override; safecall;
    function InternalGetDependentControlIDs: TControlIDs; override; safecall;

    function InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override; safecall;

    function GetResultsLimitDefaultValue: Integer; override; safecall;
  end;

implementation

procedure TReleasename.RemoveDotsOrUnderscores(var AName: string);
begin
  if CharCount('_', AName) > CharCount('.', AName) then
    AName := StringReplace(AName, '_', ' ', [rfReplaceAll])
  else
    AName := StringReplace(AName, '.', ' ', [rfReplaceAll]);
end;

procedure TReleasename.RemoveGroupName(var AName: string);
var
  LPosition: Integer;
begin
  LPosition := LastDelimiter('-', AName);
  if (LPosition > 0) then
    Delete(AName, LPosition, Length(AName) - LPosition + 1);
end;

procedure TReleasename.RemoveSceneNames(var AName: string);

  function LoadStringsFromResourceByName(const AResourceName: string): TStrings;
  var
    LResourceStream: TResourceStream;
  begin
    Result := TStringList.Create;

    LResourceStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
    try
      try
        Result.LoadFromStream(LResourceStream);
      except
        raise ;
      end;
    finally
      LResourceStream.Free;
    end;
  end;

var
  LIndex, LNameLength, LSzeneNameLength, LLastMatch, LLastMatchEnd: Integer;
  LSzeneName: string;
begin
  with LoadStringsFromResourceByName('SceneNames') do
    try
      for LIndex := 0 to Count - 1 do
      begin
        LNameLength := Length(AName);

        // Assume that SzeneNames are all in upper case
        LSzeneName := Strings[LIndex];
        LSzeneNameLength := Length(LSzeneName);

        LLastMatch := ReversePos(' ' + LSzeneName, UpperCase(AName));
        LLastMatchEnd := LLastMatch + LSzeneNameLength;

        if (LLastMatch > 0) and ((LLastMatchEnd = LNameLength) or ((LLastMatchEnd < LNameLength) and SameStr(' ', AName[LLastMatchEnd + 1]))) then
          AName := copy(AName, 1, LLastMatch);
      end;
    finally
      Free;
    end;
end;

function TReleasename.InSceneString(AStrings: array of string; const AName: string): Boolean;
var
  LArrayIndex: Integer;
begin
  Result := False;

  for LArrayIndex := low(AStrings) to High(AStrings) do
  begin
    if not(Pos(AStrings[LArrayIndex], AName) > 0) then // Optimization
      Continue;

    if (Pos('_' + AStrings[LArrayIndex] + '_', AName) > 0) or (Pos('_' + AStrings[LArrayIndex] + '-', AName) > 0) or
    { . } (Pos('.' + AStrings[LArrayIndex] + '.', AName) > 0) or (Pos('.' + AStrings[LArrayIndex] + '-', AName) > 0) then
      Exit(True);
  end;
end;

function TReleasename.GetName;
begin
  Result := 'Releasename';
end;

function TReleasename.InternalGetAvailableTypeIDs;
begin
  Result := [ low(TTypeID) .. high(TTypeID)];
end;

function TReleasename.InternalGetAvailableControlIDs;
begin
  Result := [cDirector, cTitle, cLanguage, cNotes];

  if (ATypeID = cMovie) or (ATypeID = cXXX) then
    Result := Result + [cAudioStream, cVideoCodec, cVideoStream];

  if (ATypeID in cConsole) then
    Result := Result + [cVideoSystem];
end;

function TReleasename.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TReleasename.InternalGetDependentControlIDs;
begin
  Result := [cReleaseName];
end;

function TReleasename.InternalExecute;
var
  LReleasename, LTitle: string;
  LIndex: Integer;
begin
  LReleasename := AControlController.FindControl(cReleaseName).Value;

  if ACanUse(cTitle) then
  begin
    LTitle := LReleasename;

    RemoveDotsOrUnderscores(LTitle);

    RemoveGroupName(LTitle);

    if (ATypeID = cAudio) then
    begin
      // Remove all hyphen except the first occurrence
      for LIndex := Pos('-', LTitle) + 1 to Length(LTitle) do
        if (LTitle[LIndex] = '-') then
          LTitle[LIndex] := ' ';

      // Space between artist and song title
      if Pos('-', LTitle) > 1 then
      begin
        if not(LTitle[Pos('-', LTitle) - 1] = ' ') then
          Insert(' ', LTitle, Pos('-', LTitle));
        if not(LTitle[Pos('-', LTitle) + 1] = ' ') then
          Insert(' ', LTitle, Pos('-', LTitle) + 1);
      end;

      // For SAMPLER
      if (copy(LTitle, 1, 5) = 'VA - ') then
        Delete(LTitle, 1, 5)
      else if (copy(LTitle, 1, 4) = 'VA--') then
        Delete(LTitle, 1, 4)
      else if (copy(LTitle, 1, 3) = 'VA-') then
        Delete(LTitle, 1, 3);
    end;

    // TODO: Only for german stuff useful. Detect german language or ignore?
    // https://github.com/shuyo/language-detection
    (*
      LTitle := StringReplace(LTitle, 'ae', 'ä', [rfReplaceAll]);
      LTitle := StringReplace(LTitle, 'Ae', 'Ä', [rfReplaceAll]);
      LTitle := StringReplace(LTitle, 'oe', 'ö', [rfReplaceAll]);
      LTitle := StringReplace(LTitle, 'Oe', 'Ö', [rfReplaceAll]);
      LTitle := StringReplace(LTitle, 'ue', 'ü', [rfReplaceAll]);
      LTitle := StringReplace(LTitle, 'Ue', 'Ü', [rfReplaceAll]);
      *)

    RemoveSceneNames(LTitle);

    if Length(LTitle) > 0 then
      if (LTitle[Length(LTitle)] in [' ', '-', '.', '_']) then
        Delete(LTitle, Length(LTitle), 1);

    if not(ATypeID = cSoftware) then
      if (Length(LTitle) > 4) then
        if (LTitle[Length(LTitle)] in ['0' .. '9']) and (LTitle[Length(LTitle) - 1] in ['0' .. '9']) and (LTitle[Length(LTitle) - 2] in ['0' .. '9']) and (LTitle[Length(LTitle) - 3] in ['1' .. '9']) and (LTitle[Length(LTitle) - 4] = ' ') then
          LTitle := copy(LTitle, 1, Length(LTitle) - 5);

    for LIndex := Length(LTitle) - 1 downto 1 do
      if (LTitle[LIndex] = ' ') and (LTitle[LIndex - 1] in ['0' .. '9']) and (LTitle[LIndex + 1] in ['0' .. '9']) then
        LTitle[LIndex] := '.';

    // ReduceCapitals not worth

    AControlController.FindControl(cTitle).AddProposedValue(GetName, LTitle);
  end;

  if (ATypeID = cAudio) and ACanUse(cCreator) then
  begin
    AControlController.FindControl(cCreator).AddProposedValue(GetName, Trim(copy(LTitle, 1, Pos('-', LTitle) - 1)));
  end;

  LReleasename := LowerCase(LReleasename);

  if ACanUse(cLanguage) then
  begin
    if (ATypeID = cMovie) and (Pos('german', LReleasename) > 0) and (Pos('.dl', LReleasename) > 0) then
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'german;english')

    else if Pos('german', LReleasename) > 0 then
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'german')

    else if Pos('english', LReleasename) > 0 then
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'english')

    else if Pos('spanish', LReleasename) > 0 then
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'spanish')

    else if (Pos('japanese', LReleasename) > 0) or ((ATypeID = cXXX) and (Pos('jav', LReleasename) > 0)) then
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'japanese')

    else
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'english')
  end;

  if ACanUse(cNotes) then
  begin
    if InSceneString(['uncut'], LReleasename) then
      AControlController.FindControl(cNotes).AddProposedValue(GetName, 'uncut')

    else if InSceneString(['unrated'], LReleasename) then
      AControlController.FindControl(cNotes).AddProposedValue(GetName, 'unrated')

    else if InSceneString(['dc', 'directors.cut'], LReleasename) then
      AControlController.FindControl(cNotes).AddProposedValue(GetName, 'director''s cut')

    else if (ATypeID = cMovie) and InSceneString(['extended'], LReleasename) then
      AControlController.FindControl(cNotes).AddProposedValue(GetName, 'extended')

    else if (ATypeID = cMovie) and InSceneString(['theatrical'], LReleasename) then
      AControlController.FindControl(cNotes).AddProposedValue(GetName, 'theatrical')
  end;

  if ACanUse(cAudioStream) then
  begin
    if InSceneString(['mic', 'md', 'ac3md'], LReleasename) then
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'mic')

    else if InSceneString(['line', 'ld', 'ac3ld'], LReleasename) then
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'line')

    else if InSceneString(['aac'], LReleasename) then
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'aac')

    else if InSceneString(['ac3', 'ac3d', 'ac3.dubbed', 'dd51', 'dd5.1'], LReleasename) then
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'ac3')

    else if InSceneString(['dts', 'dtsd', 'dtshd', 'dts-hd', 'bluray'], LReleasename) then
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'dts')

    else
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'ac3')
  end;

  if ACanUse(cVideoCodec) then
  begin
    if InSceneString(['xvid'], LReleasename) then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'xvid')

    else if InSceneString(['divx'], LReleasename) then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'divx')

    else if InSceneString(['x264', 'h264'], LReleasename) then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'x264')

    else if InSceneString(['x265', 'h265'], LReleasename) then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'x265')

    else if InSceneString(['dvdr'], LReleasename) then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'dvdr')

    else if InSceneString(['complete.bluray'], LReleasename) then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'vc-1')

    else if InSceneString(['svcd'], LReleasename) then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'svcd')

    else if InSceneString(['vcd'], LReleasename) then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'vcd')
  end;

  if ACanUse(cVideoStream) then
  begin
    if InSceneString(['cam', 'hdcam'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'cam')

    else if InSceneString(['bluray'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'bluray')

    else if InSceneString(['bdrip', 'bd.rip', 'bd-rip', 'hdrip', 'hd.rip', 'hd-rip'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'bdrip')

    else if InSceneString(['bdscr', 'bd.scr', 'bd-scr', 'hdscr', 'hd.scr', 'hd-scr'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'bdscr')

    else if InSceneString(['dvd5', 'dvdr'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'dvd5')

    else if InSceneString(['dvd9'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'dvd9')

    else if InSceneString(['dvdrip'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'dvdrip')

    else if InSceneString(['dvdscr', 'dvd-scr'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'dvdscr')

    else if InSceneString(['ppv', 'ppvrip'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'ppvrip')

    else if InSceneString(['r1'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'r1')
    else if InSceneString(['r2'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'r2')
    else if InSceneString(['r3'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'r3')
    else if InSceneString(['r4'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'r4')
    else if InSceneString(['r5'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'r5')
    else if InSceneString(['r6'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'r6')

    else if InSceneString(['scr', 'screener'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'scr')

    else if InSceneString(['tc', 'telecine'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'tc')

    else if InSceneString(['ts', 'telesync', 'hdts', 'hd-ts'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'ts')

    else if InSceneString(['hdtv'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'hdtv')

    else if InSceneString(['vhs'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'vhs')

    else if InSceneString(['webdl', 'web-dl'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'webdl')

    else if InSceneString(['webrip', 'web.rip', 'web-rip'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'webrip')

    else if InSceneString(['workprint'], LReleasename) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'workprint')
  end;

  if ACanUse(cVideoSystem) then
  begin
    if InSceneString(['pal'], LReleasename) then
      AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'pal')

    else if InSceneString(['ntsc'], LReleasename) then
      AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'ntsc')

    else if InSceneString(['secam'], LReleasename) then
      AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'secam');

    if (ATypeID in cConsole) then
    begin
      if InSceneString(['rf'], LReleasename) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'rf')

      else if InSceneString(['eur', 'ger', 'fra'], LReleasename) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'eur')

      else if InSceneString(['usa'], LReleasename) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'usa')

      else if InSceneString(['jpn', 'jap'], LReleasename) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'jpn')
    end;
  end;

  Result := True;
end;

function TReleasename.GetResultsLimitDefaultValue;
begin
  Result := 1;
end;

end.
