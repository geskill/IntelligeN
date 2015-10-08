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

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
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
  SzeneNames: array [0 .. 111] of string = ('2CD', '3CD', '3DS', '480p', '720p', '1080p', 'AC3', 'AC3D', 'AC3LD', 'BDRIP', 'BLURAY', 'BRRIP', 'CAM', 'CENSORED', 'CLONEDVD',
    'COMPLETE', 'CRACKED', 'DC', 'DISC', 'DISC1', 'DISC2', 'DL', 'DTS', 'DUAL', 'Dubbed', 'DVD', 'DVDR', 'DVDRiP', 'DVDSCR', 'DVDSCREENER', 'EMUDVD', 'ENG', 'EUR',
    'EXTENDED', 'FLV', 'FRA', 'FRE', 'GBA', 'GER', 'German', 'H264', 'HDRIP', 'HDTV', 'INTEGRATED', 'INTERNAL', 'JAV', 'JPN', 'JTAG', 'LD', 'Line', 'KOR', 'MAC', 'MACOSX',
    'MAG', 'MD', 'MOV', 'MP4', 'MULTI', 'MULTi2', 'MULTi3', 'MULTi4', 'MULTi5', 'MULTi6', 'MULTiLANGUAGE', 'NDS', 'NFO', 'NGC', 'NTSC', 'PAL', 'PPVRIP', 'PROPER', 'PROMO',
    'PDVD', 'PS2', 'PS3', 'PS4', 'PSP', 'R3', 'R5', 'READNFO', 'REGION', 'REMUX', 'REPACK', 'RF', 'SCR', 'SCREENER', 'SUBBED', 'TELECINE', 'TC', 'TELESYNC', 'TS', 'UNCUT',
    'UNTOUCHED', 'UNRATED', 'USA', 'VC1', 'VINYL', 'WEB', 'WEBRIP', 'WII', 'WIIU', 'WINALL', 'WORKPRINT', 'WS', 'x264', 'X360', 'XBLA', 'XBOX', 'XBOX360', 'XBOXONE', 'XViD',
    'XXX');
var
  I, LastMatch, LastMatchEnd, NameLength: Integer;
begin
  for I := low(SzeneNames) to high(SzeneNames) do
  begin
    LastMatch := ReversePos(' ' + LowerCase(SzeneNames[I]), LowerCase(AName));
    LastMatchEnd := LastMatch + Length(SzeneNames[I]);
    NameLength := Length(AName);
    if (LastMatch > 0) and ((LastMatchEnd = NameLength) or ((LastMatchEnd < NameLength) and SameText(AName[LastMatchEnd + 1], ' '))) then
      AName := copy(AName, 1, LastMatch);
  end;
end;

function TReleasename.GetName;
begin
  Result := 'Releasename';
end;

function TReleasename.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTypeID) .. high(TTypeID)];
  Result := Word(_TemplateTypeIDs);
end;

function TReleasename.GetAvailableControlIDs;
var
  _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
begin
  _TemplateTypeID := TTypeID(ATypeID);

  _ComponentIDs := [cArtist, cTitle, cLanguage];

  if (_TemplateTypeID = cMovie) or (_TemplateTypeID = cXXX) then
    _ComponentIDs := _ComponentIDs + [cAudioStream, cVideoCodec, cVideoStream];

  if (_TemplateTypeID in [cNintendoDS, cPlayStation3, cWii, cXbox, cXbox360]) then
    _ComponentIDs := _ComponentIDs + [cVideoSystem];

  Result := LongWord(_ComponentIDs);
end;

function TReleasename.GetControlIDDefaultValue;
begin
  Result := True;
end;

function TReleasename.GetResultsLimitDefaultValue;
begin
  Result := 1;
end;

function TReleasename.Exec;
var
  _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
  _Releasename, S: string;
  I: Integer;
begin
  _TemplateTypeID := TTypeID(ATypeID);
  LongWord(_ComponentIDs) := AControlIDs;
  if not(AControlController.FindControl(cReleaseName) = nil) then
  begin
    _Releasename := AControlController.FindControl(cReleaseName).Value;

    if (AControlController.FindControl(cTitle) <> nil) and (cTitle in _ComponentIDs) then
    begin
      if CharCount('_', _Releasename) > CharCount('.', _Releasename) then
        S := StringReplace(_Releasename, '_', ' ', [rfReplaceAll])
      else
        S := StringReplace(_Releasename, '.', ' ', [rfReplaceAll]);
      if (LastDelimiter('-', S) > 0) then
        Delete(S, LastDelimiter('-', S), Length(S) - LastDelimiter('-', S) + 1);

      if (TTypeID(ATypeID) = cAudio) then
      begin
        // Alle bis auf den ersten Bindestrich entfernen
        for I := Pos('-', S) + 1 to Length(S) do
          if (S[I] = '-') then
            S[I] := ' ';

        // Leerzeichen zwischen Interpret und Songtitel
        if Pos('-', S) > 1 then
        begin
          if not(S[Pos('-', S) - 1] = ' ') then
            Insert(' ', S, Pos('-', S));
          if not(S[Pos('-', S) + 1] = ' ') then
            Insert(' ', S, Pos('-', S) + 1);
        end;

        // Für SAMPLER
        if (copy(S, 1, 5) = 'VA - ') then
          Delete(S, 1, 5)
        else if (copy(S, 1, 4) = 'VA--') then
          Delete(S, 1, 4)
        else if (copy(S, 1, 3) = 'VA-') then
          Delete(S, 1, 3);
      end;

      S := StringReplace(S, 'ae', 'ä', [rfReplaceAll]);
      S := StringReplace(S, 'Ae', 'Ä', [rfReplaceAll]);
      S := StringReplace(S, 'oe', 'ö', [rfReplaceAll]);
      S := StringReplace(S, 'Oe', 'Ö', [rfReplaceAll]);
      S := StringReplace(S, 'ue', 'ü', [rfReplaceAll]);
      S := StringReplace(S, 'Ue', 'Ü', [rfReplaceAll]);

      RemoveSceneNames(S);

      if Length(S) > 0 then
        if (S[Length(S)] in [' ', '-', '.', '_']) then
          Delete(S, Length(S), 1);

      if not(TTypeID(ATypeID) = cSoftware) then
        if Length(S) > 4
        { 11 } then
          if (S[Length(S)] in ['0' .. '9']) and (S[Length(S) - 1] in ['0' .. '9']) and (S[Length(S) - 2] in ['0' .. '9']) and
            (S[Length(S) - 3] in ['1' .. '9']) and (S[Length(S) - 4] = ' ') then
            S := copy(S, 1, Length(S) - 5);

      for I := Length(S) - 1 downto 1 do
        if (S[I] = ' ') and (S[I - 1] in ['0' .. '9']) and (S[I + 1] in ['0' .. '9']) then
          S[I] := '.';

      { ReduceCapitals eher schlecht
        for I := Length(s) downto 1 do
        if (s[I] in ['A' .. 'Z']) and (s[I - 1] in ['A' .. 'Z']) then
        s[I] := LowerCase(s)[I];
        }

      if (AControlController.FindControl(cArtist) <> nil) and (cArtist in _ComponentIDs) then
        AControlController.FindControl(cArtist).AddProposedValue(GetName, Trim(copy(S, 1, Pos('-', S) - 1)));

      AControlController.FindControl(cTitle).AddProposedValue(GetName, S);
    end;

    _Releasename := LowerCase(_Releasename);

    if (AControlController.FindControl(cLanguage) <> nil) and (cLanguage in _ComponentIDs) then
    begin
      if (TTypeID(ATypeID) = cMovie) and (Pos('german', _Releasename) > 0) and (Pos('.dl', _Releasename) > 0) then
        AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'german;english', GetName)
      else if Pos('german', _Releasename) > 0 then
        AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'german', GetName)
      else if Pos('english', _Releasename) > 0 then
        AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'english', GetName)
      else if Pos('spanish', _Releasename) > 0 then
        AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'spanish', GetName)
      else if (Pos('japanese', _Releasename) > 0) or ((Pos('jav', _Releasename) > 0) and (TTypeID(ATypeID) = cXXX)) then
        AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'japanese', GetName)
      else
        // if (TTypeID(ATypeID) = cMovie) or (TTypeID(ATypeID) = cPCGames) or (TTypeID(ATypeID) = cSoftware) or (TTypeID(ATypeID) = cXXX) then
        AControlController.FindControl(cLanguage).AddProposedValue(GetName, 'english', GetName)
    end;

    if (AControlController.FindControl(cAudioStream) <> nil) and (cAudioStream in _ComponentIDs) then
    begin
      if Pos('.ac3', _Releasename) > 0 then
        AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'ac3', GetName)
      else if Pos('.dts', _Releasename) > 0 then
        AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'dts', GetName)
      else if Pos('.mic', _Releasename) > 0 then
        AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'mic', GetName)
      else if Pos('.md', _Releasename) > 0 then
        AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'mic', GetName)
      else if Pos('.ld', _Releasename) > 0 then
        AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'line', GetName)
      else if (TTypeID(ATypeID) = cMovie) or (TTypeID(ATypeID) = cXXX) then
        AControlController.FindControl(cAudioStream).AddProposedValue(GetName, 'stereo', GetName)
    end;

    if (AControlController.FindControl(cVideoCodec) <> nil) and (cVideoCodec in _ComponentIDs) then
    begin
      if Pos('.xvid', _Releasename) > 0 then
        AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'xvid', GetName)
      else if Pos('.divx', _Releasename) > 0 then
        AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'divx', GetName)
      else if Pos('.dvdr', _Releasename) > 0 then
        AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'dvdr', GetName)
      else if Pos('.svcd', _Releasename) > 0 then
        AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'svcd', GetName)
      else if Pos('.x264', _Releasename) > 0 then
        AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'x264', GetName)
      else if Pos('.complete.bluray', _Releasename) > 0 then
        AControlController.FindControl(cVideoCodec).AddProposedValue(GetName, 'vc-1', GetName)
    end;

    if (AControlController.FindControl(cVideoStream) <> nil) and (cVideoStream in _ComponentIDs) then
    begin
      if (Pos('.screener.', _Releasename) > 0) then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'screener', GetName)
      else if (Pos('.dvdscr.', _Releasename) > 0) then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'dvdscr', GetName)
      else if Pos('.dvdrip.', _Releasename) > 0 then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'dvdrip', GetName)
      else if Pos('.dvdr.', _Releasename) > 0 then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'dvdr', GetName)
      else if (Pos('.bdrip.', _Releasename) > 0) or (Pos('.hdrip.', _Releasename) > 0) then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'bdrip', GetName)
      else if Pos('.bluray.', _Releasename) > 0 then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'bluray', GetName)
      else if Pos('.ts.', _Releasename) > 0 then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'ts', GetName)
      else if Pos('.telesync.', _Releasename) > 0 then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'telesync', GetName)
      else if Pos('.r3.', _Releasename) > 0 then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'r3', GetName)
      else if Pos('.r5.', _Releasename) > 0 then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'r5', GetName)
      else if Pos('.hdtv.', _Releasename) > 0 then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'hdtv', GetName)
      else if Pos('.workprint.', _Releasename) > 0 then
        AControlController.FindControl(cVideoStream).AddProposedValue(GetName, 'workprint', GetName)
    end;

    if (AControlController.FindControl(cVideoSystem) <> nil) and (cVideoSystem in _ComponentIDs) then
    begin
      if (_TemplateTypeID = cMovie) then
      begin
        if (Pos('.pal.', _Releasename) > 0) or (Pos('_pal_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'PAL', GetName)
        else if (Pos('.ntsc.', _Releasename) > 0) or (Pos('_ntsc_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'NTSC', GetName)
      end
      else if (_TemplateTypeID = cNintendoDS) then
      begin
        if (Pos('.eur.', _Releasename) > 0) or (Pos('_eur_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'EUR', GetName)
        else if (Pos('.ger.', _Releasename) > 0) or (Pos('_ger_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'EUR', GetName)
        else if (Pos('.fra.', _Releasename) > 0) or (Pos('_fra_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'EUR', GetName)
        else if (Pos('.usa.', _Releasename) > 0) or (Pos('_usa_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'USA', GetName)
        else if (Pos('.jpn.', _Releasename) > 0) or (Pos('_jpn_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'JPN', GetName)
        else if (Pos('.jap.', _Releasename) > 0) or (Pos('_jap_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'JPN', GetName)
      end
      else if (_TemplateTypeID = cXbox) or (_TemplateTypeID = cWii) then
      begin
        if (Pos('.pal.', _Releasename) > 0) or (Pos('_pal_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'PAL', GetName)
        else if (Pos('.ntsc.', _Releasename) > 0) or (Pos('_ntsc_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'NTSC', GetName)
        else if (Pos('.usa.', _Releasename) > 0) or (Pos('_usa_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'NTSC', GetName)
      end
      else if (_TemplateTypeID = cPlayStation3) or (_TemplateTypeID = cXbox360) then
      begin
        if (Pos('.rf.', _Releasename) > 0) or (Pos('_rf_', _Releasename) > 0) then // nur x360
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'REGION FREE', GetName)
        else if (Pos('.pal.', _Releasename) > 0) or (Pos('_pal_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'PAL', GetName)
        else if (Pos('.ntsc.', _Releasename) > 0) or (Pos('_ntsc_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'NTSC', GetName)
        else if (Pos('.eur.', _Releasename) > 0) or (Pos('_eur_', _Releasename) > 0) then // nur PS3
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'PAL', GetName)
        else if (Pos('.usa.', _Releasename) > 0) or (Pos('_usa_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'NTSC', GetName)
        else if (Pos('.jpn.', _Releasename) > 0) or (Pos('_jpn_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'NTSC-J', GetName)
        else if (Pos('.jap.', _Releasename) > 0) or (Pos('_jap_', _Releasename) > 0) then
          AControlController.FindControl(cVideoSystem).AddProposedValue(GetName, 'NTSC-J', GetName)
      end;
    end;
  end;
end;

end.
