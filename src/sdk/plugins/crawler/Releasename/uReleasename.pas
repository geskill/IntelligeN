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
    procedure RemoveSceneNames(var AName: string);
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

procedure TReleasename.RemoveSceneNames(var AName: string);

  function ReversePos(SubStr, S: string): Integer;
  begin
    SubStr := ReverseString(SubStr);
    S := ReverseString(S);
    Result := Pos(SubStr, S);
    if 0 <> Result then
      Result := Length(S) - Length(SubStr) - Result + 2;
  end;

const
  SzeneNames: array [0 .. 111] of string = ('2CD', '3CD', '3DS', '480p', '720p', '1080p', 'AC3', 'AC3D', 'AC3LD', 'BDRIP', 'BLURAY', 'BRRIP', 'CAM', 'CENSORED', 'CLONEDVD', 'COMPLETE', 'CRACKED', 'DC', 'DISC', 'DISC1', 'DISC2', 'DL', 'DTS', 'DUAL',
    'Dubbed', 'DVD', 'DVDR', 'DVDRiP', 'DVDSCR', 'DVDSCREENER', 'EMUDVD', 'ENG', 'EUR', 'EXTENDED', 'FLV', 'FRA', 'FRE', 'GBA', 'GER', 'German', 'H264', 'HDRIP', 'HDTV', 'INTEGRATED', 'INTERNAL', 'JAV', 'JPN', 'JTAG', 'LD', 'Line', 'KOR', 'MAC',
    'MACOSX', 'MAG', 'MD', 'MOV', 'MP4', 'MULTI', 'MULTi2', 'MULTi3', 'MULTi4', 'MULTi5', 'MULTi6', 'MULTiLANGUAGE', 'NDS', 'NFO', 'NGC', 'NTSC', 'PAL', 'PPVRIP', 'PROPER', 'PROMO', 'PDVD', 'PS2', 'PS3', 'PS4', 'PSP', 'R3', 'R5', 'READNFO',
    'REGION', 'REMUX', 'REPACK', 'RF', 'SCR', 'SCREENER', 'SUBBED', 'TELECINE', 'TC', 'TELESYNC', 'TS', 'UNCUT', 'UNTOUCHED', 'UNRATED', 'USA', 'VC1', 'VINYL', 'WEB', 'WEBRIP', 'WII', 'WIIU', 'WINALL', 'WORKPRINT', 'WS', 'x264', 'X360', 'XBLA',
    'XBOX', 'XBOX360', 'XBOXONE', 'XViD', 'XXX');
var
  LIndex, LastMatch, LastMatchEnd, NameLength: Integer;
begin
  for LIndex := low(SzeneNames) to high(SzeneNames) do
  begin
    LastMatch := ReversePos(' ' + LowerCase(SzeneNames[LIndex]), LowerCase(AName));
    LastMatchEnd := LastMatch + Length(SzeneNames[LIndex]);
    NameLength := Length(AName);
    if (LastMatch > 0) and ((LastMatchEnd = NameLength) or ((LastMatchEnd < NameLength) and SameText(AName[LastMatchEnd + 1], ' '))) then
      AName := copy(AName, 1, LastMatch);
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
  Result := [cDirector, cTitle, cLanguage];

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
    if CharCount('_', LReleasename) > CharCount('.', LReleasename) then
      LTitle := StringReplace(LReleasename, '_', ' ', [rfReplaceAll])
    else
      LTitle := StringReplace(LReleasename, '.', ' ', [rfReplaceAll]);
    if (LastDelimiter('-', LTitle) > 0) then
      Delete(LTitle, LastDelimiter('-', LTitle), Length(LTitle) - LastDelimiter('-', LTitle) + 1);

    if (TTypeID(ATypeID) = cAudio) then
    begin
      // Alle bis auf den ersten Bindestrich entfernen
      for LIndex := Pos('-', LTitle) + 1 to Length(LTitle) do
        if (LTitle[LIndex] = '-') then
          LTitle[LIndex] := ' ';

      // Leerzeichen zwischen Interpret und Songtitel
      if Pos('-', LTitle) > 1 then
      begin
        if not(LTitle[Pos('-', LTitle) - 1] = ' ') then
          Insert(' ', LTitle, Pos('-', LTitle));
        if not(LTitle[Pos('-', LTitle) + 1] = ' ') then
          Insert(' ', LTitle, Pos('-', LTitle) + 1);
      end;

      // Für SAMPLER
      if (copy(LTitle, 1, 5) = 'VA - ') then
        Delete(LTitle, 1, 5)
      else if (copy(LTitle, 1, 4) = 'VA--') then
        Delete(LTitle, 1, 4)
      else if (copy(LTitle, 1, 3) = 'VA-') then
        Delete(LTitle, 1, 3);
    end;

    LTitle := StringReplace(LTitle, 'ae', 'ä', [rfReplaceAll]);
    LTitle := StringReplace(LTitle, 'Ae', 'Ä', [rfReplaceAll]);
    LTitle := StringReplace(LTitle, 'oe', 'ö', [rfReplaceAll]);
    LTitle := StringReplace(LTitle, 'Oe', 'Ö', [rfReplaceAll]);
    LTitle := StringReplace(LTitle, 'ue', 'ü', [rfReplaceAll]);
    LTitle := StringReplace(LTitle, 'Ue', 'Ü', [rfReplaceAll]);

    RemoveSceneNames(LTitle);

    if Length(LTitle) > 0 then
      if (LTitle[Length(LTitle)] in [' ', '-', '.', '_']) then
        Delete(LTitle, Length(LTitle), 1);

    if not(TTypeID(ATypeID) = cSoftware) then
      if Length(LTitle) > 4
      { 11 } then
        if (LTitle[Length(LTitle)] in ['0' .. '9']) and (LTitle[Length(LTitle) - 1] in ['0' .. '9']) and (LTitle[Length(LTitle) - 2] in ['0' .. '9']) and (LTitle[Length(LTitle) - 3] in ['1' .. '9']) and (LTitle[Length(LTitle) - 4] = ' ') then
          LTitle := copy(LTitle, 1, Length(LTitle) - 5);

    for LIndex := Length(LTitle) - 1 downto 1 do
      if (LTitle[LIndex] = ' ') and (LTitle[LIndex - 1] in ['0' .. '9']) and (LTitle[LIndex + 1] in ['0' .. '9']) then
        LTitle[LIndex] := '.';

    { ReduceCapitals eher schlecht
      for I := Length(s) downto 1 do
      if (s[I] in ['A' .. 'Z']) and (s[I - 1] in ['A' .. 'Z']) then
      s[I] := LowerCase(s)[I];
      }

    if (ATypeID = cAudio) then
    begin
      if (AControlController.FindControl(cCreator) <> nil) and (cCreator in AControlIDs) then
        AControlController.FindControl(cCreator).AddProposedValue(GetName, Trim(copy(LTitle, 1, Pos('-', LTitle) - 1)));
    end;

    AControlController.FindControl(cTitle).AddProposedValue(GetName, LTitle);
  end;

  LReleasename := LowerCase(LReleasename);

  if ACanUse(cLanguage) then
  begin
    if (TTypeID(ATypeID) = cMovie) and (Pos('german', LReleasename) > 0) and (Pos('.dl', LReleasename) > 0) then
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'german;english')
    else if Pos('german', LReleasename) > 0 then
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'german')
    else if Pos('english', LReleasename) > 0 then
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'english')
    else if Pos('spanish', LReleasename) > 0 then
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'spanish')
    else if (Pos('japanese', LReleasename) > 0) or ((Pos('jav', LReleasename) > 0) and (TTypeID(ATypeID) = cXXX)) then
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'japanese')
    else
      // if (TTypeID(ATypeID) = cMovie) or (TTypeID(ATypeID) = cPCGames) or (TTypeID(ATypeID) = cSoftware) or (TTypeID(ATypeID) = cXXX) then
      AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'english')
  end;

  if ACanUse(cAudioStream) then
  begin
    if Pos('.ac3', LReleasename) > 0 then
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'ac3')
    else if Pos('.dts', LReleasename) > 0 then
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'dts')
    else if Pos('.mic', LReleasename) > 0 then
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'mic')
    else if Pos('.md', LReleasename) > 0 then
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'mic')
    else if Pos('.ld', LReleasename) > 0 then
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'line')
    else if (TTypeID(ATypeID) = cMovie) or (TTypeID(ATypeID) = cXXX) then
      AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'stereo')
  end;

  if ACanUse(cVideoCodec) then
  begin
    if Pos('.xvid', LReleasename) > 0 then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'xvid')
    else if Pos('.divx', LReleasename) > 0 then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'divx')
    else if Pos('.dvdr', LReleasename) > 0 then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'dvdr')
    else if Pos('.svcd', LReleasename) > 0 then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'svcd')
    else if Pos('.x264', LReleasename) > 0 then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'x264')
    else if Pos('.complete.bluray', LReleasename) > 0 then
      AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'vc-1')
  end;

  if ACanUse(cVideoStream) then
  begin
    if (Pos('.screener.', LReleasename) > 0) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'screener')
    else if (Pos('.dvdscr.', LReleasename) > 0) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'dvdscr')
    else if Pos('.dvdrip.', LReleasename) > 0 then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'dvdrip')
    else if Pos('.dvdr.', LReleasename) > 0 then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'dvdr')
    else if (Pos('.bdrip.', LReleasename) > 0) or (Pos('.hdrip.', LReleasename) > 0) then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'bdrip')
    else if Pos('.bluray.', LReleasename) > 0 then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'bluray')
    else if Pos('.ts.', LReleasename) > 0 then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'ts')
    else if Pos('.telesync.', LReleasename) > 0 then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'telesync')
    else if Pos('.r3.', LReleasename) > 0 then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'r3')
    else if Pos('.r5.', LReleasename) > 0 then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'r5')
    else if Pos('.hdtv.', LReleasename) > 0 then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'hdtv')
    else if Pos('.workprint.', LReleasename) > 0 then
      AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'workprint')
  end;

  if ACanUse(cVideoSystem) then
  begin
    if (ATypeID = cMovie) then
    begin
      if (Pos('.pal.', LReleasename) > 0) or (Pos('_pal_', LReleasename) > 0) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'PAL')
      else if (Pos('.ntsc.', LReleasename) > 0) or (Pos('_ntsc_', LReleasename) > 0) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'NTSC')
    end
    else if (ATypeID in cConsole) then
    begin
      if (Pos('.rf.', LReleasename) > 0) or (Pos('_rf_', LReleasename) > 0) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'REGION FREE')
      else if (Pos('.pal.', LReleasename) > 0) or (Pos('_pal_', LReleasename) > 0) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'PAL')
      else if (Pos('.ntsc.', LReleasename) > 0) or (Pos('_ntsc_', LReleasename) > 0) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'NTSC')
      else if (Pos('.eur.', LReleasename) > 0) or (Pos('_eur_', LReleasename) > 0) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'EUR')
      else if (Pos('.ger.', LReleasename) > 0) or (Pos('_ger_', LReleasename) > 0) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'EUR')
      else if (Pos('.fra.', LReleasename) > 0) or (Pos('_fra_', LReleasename) > 0) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'EUR')
      else if (Pos('.usa.', LReleasename) > 0) or (Pos('_usa_', LReleasename) > 0) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'USA')
      else if (Pos('.jpn.', LReleasename) > 0) or (Pos('_jpn_', LReleasename) > 0) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'JPN')
      else if (Pos('.jap.', LReleasename) > 0) or (Pos('_jap_', LReleasename) > 0) then
        AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'JPN')
    end;
  end;

  Result := True;
end;

function TReleasename.GetResultsLimitDefaultValue;
begin
  Result := 1;
end;

end.
