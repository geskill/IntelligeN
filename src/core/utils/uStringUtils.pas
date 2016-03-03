unit uStringUtils;

interface

uses
  // Delphi
  Windows, Character, Classes, Variants, SysUtils, StrUtils,
  // Spring Framework
  Spring.Cryptography,
  // RegEx
  RegExpr;

function CompareTextByMD5(const AText, AComparison: string): Boolean;

function MatchTextMask(const Mask, S: WideString; CaseSensitive: Boolean = False): Boolean;

function StringListSplit(const AStrings: TStrings; ADelimiter: string): string;

function SplittString(splitt: Char; const S: string; AExact: Boolean = False): TStrings;

function StringReplaceMultiple(const Source: string; const OldPatterns, NewPatterns: array of string; CaseSensitive: Boolean = True): string;

function IsLineBreak(const AStr: string; AIndex: Integer): Boolean;

function IsNumber(AVariant: Variant): Boolean;

function ExtractTextBetween(const Str: string; const Delim1, Delim2: string): string;

function RemoveTextBetween(const Str: string; const Delim1, Delim2: string): string;

function CharCount(const SubStr, S: string): Integer;

function RegExpEscape(const AStr: string): string;

function ReversePos(SubStr, S: string): Integer;

function ReduceCapitals(const Str: string): string;

function ReduceWhitespace(const AStr: string): string;

function RemoveWhitespace(const AStr: string): string;

function RemoveW(const AHost: string): string;

function Trim(const S: string; const C: Char): string; overload;

function TrimLeft(const S: string; const C: Char): string; overload;

function TrimRight(const S: string; const C: Char): string; overload;

function PadLeft(const S: string; Ch: Char; Len: Integer): string;

function PadRight(const S: string; Ch: Char; Len: Integer): string;

implementation

function CompareTextByMD5(const AText, AComparison: string): Boolean;
var
  LTrim1, LTrim2: string;
  LHash1, LHash2: string;
begin
  Result := False;

  LTrim1 := Trim(AText);
  LTrim2 := Trim(AComparison);

  if (length(LTrim1) = length(LTrim2)) then
  begin
    LHash1 := CreateMD5.ComputeHash(LTrim1).ToHexString;
    LHash2 := CreateMD5.ComputeHash(LTrim2).ToHexString;

    Result := SameStr(LHash1, LHash2);
  end;
end;

function MatchTextMask(const Mask, S: WideString; CaseSensitive: Boolean = False): Boolean;
var
  Mp, Me, Mm, Sp, Se, Sm: PWideChar;
  Ml, Sl: WideString;

label LMask;
begin
  Result := False;
  if CaseSensitive then
  begin
    Mp := PWideChar(Mask);
    Sp := PWideChar(S);
  end
  else
  begin
    Ml := Mask;
    Sl := S;
    UniqueString(Ml);
    UniqueString(Sl);
    Mp := PWideChar(Ml);
    Sp := PWideChar(Sl);
    CharLowerBuffW(Mp, Length(Ml));
    CharLowerBuffW(Sp, Length(Sl));
  end;
  Me := Mp + Length(Mask);
  Se := Sp + Length(S);
  Mm := nil;
  Sm := Se;
  while (Mp < Me) or (Sp < Se) do
  begin
    case Mp^ of
      '*', '%':
        begin
          while (Mp^ = '*') or (Mp^ = '%') do
            Inc(Mp);
          Mm := Mp;
          Sm := Sp + 1;
          Continue;
        LMask :
          Mp := Mm;
          Sp := Sm;
          Inc(Sm);
          if (Mp < Me) and (Sp >= Se) then
            Exit;
          Continue;
        end;
      '?':
        ;
      '\':
        begin
{$IF SizeOf(Mp^) > 1}
          // If ((Mp + 1)^ = '*') or ((Mp + 1)^ = '?') or ((Mp + 1)^ = '\') Then Inc(Mp);
          case (Mp + 1)^ of
            '*', '%', '?', '\':
              Inc(Mp);
          end;
{$ELSE}
          if (Mp + 1)^ in ['*', '%', '?', '\'] then
            Inc(Mp);
{$IFEND}
          if Mp^ <> Sp^ then
            goto LMask;
        end;
    else
      if Mp^ <> Sp^ then
        goto LMask;
    end;
    if (Mp >= Me) or (Sp >= Se) then
      goto LMask;
    Inc(Mp);
    Inc(Sp);
  end;
  Result := True;
end;

function StringListSplit(const AStrings: TStrings; ADelimiter: string): string;
var
  LEndPos: Integer;
begin
  Result := StringReplace(AStrings.Text, sLineBreak, ADelimiter, [rfReplaceAll]);
  LEndPos := Length(Result) + 1 - Length(ADelimiter);
  if copy(Result, LEndPos, Length(ADelimiter)) = ADelimiter then
    Delete(Result, LEndPos, Length(ADelimiter));
end;

function SplittString(splitt: Char; const S: string; AExact: Boolean = False): TStrings;
var
  I: Integer;
begin
  Result := TStringlist.Create;
  Result.Add('');
  for I := 1 to Length(S) do
    if (S[I] <> splitt) then
      Result.Strings[Result.Count - 1] := Result.Strings[Result.Count - 1] + S[I]
    else if AExact xor ((not AExact) and (Result.Strings[Result.Count - 1] <> '')) then
      Result.Add('');
end;

function StringReplaceMultiple(const Source: string; const OldPatterns, NewPatterns: array of string; CaseSensitive: Boolean = True): string;
// Replace every occurrence

type
  TFoundPos = record
    Position: Integer;
    PatternNum: Integer;
  end;

var
  C: Integer;
  FoundCount: Integer;
  SourcePosition: Integer;
  PatternCount: Integer;
  Positions: array of TFoundPos;
  PositionLength: Integer;

  PatternNum: Integer;
  SourceLength: Integer;
  OldLengths, NewLengths: array of Integer;
  DeltaOld: Integer;

  Delta: Integer;

  PSource, PDest, PNew: PChar;

  SearchSource: string;
  CasePatterns: array of string;
begin
  if (Source = '') or (Length(OldPatterns) <> Length(NewPatterns)) then
  begin
    Result := Source;
    Exit;
  end;

  try
    // Initialize some variables
    PatternCount := Length(OldPatterns);
    SourceLength := Length(Source);
    SetLength(OldLengths, PatternCount);
    SetLength(NewLengths, PatternCount);
    Delta := 0;
    DeltaOld := 0;
    for C := 0 to PatternCount - 1 do
    begin
      OldLengths[C] := Length(OldPatterns[C]);
      NewLengths[C] := Length(NewPatterns[C]);
      Inc(DeltaOld, OldLengths[C]);
    end;
    DeltaOld := Round(DeltaOld / PatternCount);

    SetLength(CasePatterns, PatternCount);
    if CaseSensitive then
    begin
      SearchSource := Source;
      for C := 0 to PatternCount - 1 do
        CasePatterns[C] := OldPatterns[C];
    end
    else
    begin
      SearchSource := LowerCase(Source);
      for C := 0 to PatternCount - 1 do
        CasePatterns[C] := LowerCase(OldPatterns[C]);
    end;

    FoundCount := 0;

    // ----------------------------------
    // Check the amount of replaces
    // ----------------------------------

    // We *should* range check here, but who has strings > 2GB ?
    PositionLength := SourceLength div DeltaOld + 1;
    SetLength(Positions, PositionLength);

    C := 1;
    while C <= SourceLength do
    begin
      for PatternNum := 0 to PatternCount - 1 do
      begin
        if (SearchSource[C]) = (CasePatterns[PatternNum][1]) then // Check first char before we waste a jump to CompareMem
        begin
          if CompareMem(@SearchSource[C], @CasePatterns[PatternNum][1], OldLengths[PatternNum] * SizeOf(Char)) then
          begin
            if FoundCount >= PositionLength then
            begin
              Inc(PositionLength, 4);
              SetLength(Positions, PositionLength);
            end;

            Positions[FoundCount].Position := C; // Store the found position
            Positions[FoundCount].PatternNum := PatternNum;
            Inc(FoundCount);
            Inc(C, OldLengths[PatternNum] - 1); // Jump to after OldPattern
            Inc(Delta, NewLengths[PatternNum] - OldLengths[PatternNum]);
            Break;
          end;
        end;
      end;
      Inc(C);
    end;

    SetLength(CasePatterns, 0);

    // ----------------------------------
    // Actual replace
    // ----------------------------------

    if FoundCount > 0 then // Have we found anything?
    begin
      // We know the length of the result
      // Again, we *should* range check here...
      SetLength(Result, SourceLength + Delta);

      // Initialize some variables
      SourcePosition := 1;
      PSource := PChar(Source);
      PDest := PChar(Result);

      // Replace...

      for C := 0 to FoundCount - 1 do
      begin
        // Copy original and advance resultpos
        PNew := PChar(NewPatterns[Positions[C].PatternNum]);

        Move(PSource^, PDest^, (Positions[C].Position - SourcePosition) * SizeOf(Char));
        Inc(PDest, Positions[C].Position - SourcePosition);

        // Append NewPattern and advance resultpos
        Move(PNew^, PDest^, (NewLengths[Positions[C].PatternNum]) * SizeOf(Char));
        Inc(PDest, NewLengths[Positions[C].PatternNum]);

        // Jump to after OldPattern
        Inc(PSource, Positions[C].Position - SourcePosition + OldLengths[Positions[C].PatternNum]);
        SourcePosition := Positions[C].Position + OldLengths[Positions[C].PatternNum];
      end;

      // Append characters after last OldPattern
      Move(PSource^, PDest^, (SourceLength - SourcePosition + 1) * SizeOf(Char));
    end
    else
      Result := Source; // Nothing to replace

    // Clean up
    Finalize(Positions);
  except
  end;
end;

function IsLineBreak(const AStr: string; AIndex: Integer): Boolean;
begin
  Result := CharInSet(AStr[AIndex], [#10, #13]);
end;

{$HINTS OFF}
function IsNumber(AVariant: Variant): Boolean;
var
  LValue, LCode: Integer;
begin
  val(VarToStrDef(AVariant, ''), LValue, LCode);
  Result := (LCode = 0);
end;
{$HINTS ON}

function ExtractTextBetween(const Str: string; const Delim1, Delim2: string): string;
var
  pos1, pos2: Integer;
begin
  Result := '';
  pos1 := Pos(Delim1, Str);
  if pos1 > 0 then
  begin
    pos2 := PosEx(Delim2, Str, pos1 + 1);
    if pos2 > 0 then
      Result := copy(Str, pos1 + 1, pos2 - pos1 - 1);
  end;
end;

function RemoveTextBetween(const Str: string; const Delim1, Delim2: string): string;
var
  pos1, pos2: Integer;
begin
  Result := Str;
  pos1 := Pos(Delim1, Str);
  if pos1 > 0 then
  begin
    pos2 := PosEx(Delim2, Str, pos1 + 1);
    if pos2 > 0 then
    begin
      Result := copy(Str, 1, pos1 - 1) + copy(Str, pos2 + Length(Delim2));
    end;
  end;
end;

function CharCount(const SubStr, S: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  I := 1;
  while PosEx(SubStr, S, I) > 0 do
  begin
    I := PosEx(SubStr, S, I) + 1;
    Inc(Result);
  end;
end;

function RegExpEscape(const AStr: string): string;
begin
  Result := ReplaceRegExpr('([.+*?\[\]\(\){}\\])', AStr, '\\$1', True);
end;

function ReversePos(SubStr, S: string): Integer;
begin
  SubStr := ReverseString(SubStr);
  S := ReverseString(S);
  Result := Pos(SubStr, S);
  if 0 <> Result then
    Result := Length(S) - Length(SubStr) - Result + 2;
end;

function ReduceCapitals(const Str: string): string;
var
  LStringIndex: Integer;
begin
  Result := Str;

  for LStringIndex := Length(Str) downto 2 do
    if CharInSet(Str[LStringIndex], ['A' .. 'Z']) and CharInSet(Str[LStringIndex - 1], ['A' .. 'Z']) then
    begin
      Result[LStringIndex] := LowerCase(Str[LStringIndex])[1];
    end;
end;

function ReduceWhitespace(const AStr: string): string;
var
  LCharIndex, LWhiteSpaceCount: Integer;
  LHasLineBreak: Boolean;
begin
  Result := AStr;
  LCharIndex := Length(AStr);
  LWhiteSpaceCount := 0;
  LHasLineBreak := False;
  while LCharIndex > 0 do
  begin
    if IsWhiteSpace(AStr, LCharIndex) then
    begin
      Inc(LWhiteSpaceCount);
      if IsLineBreak(AStr, LCharIndex) then
        LHasLineBreak := True;
    end
    else
    begin
      if LWhiteSpaceCount > 1 then
      begin
        Delete(Result, LCharIndex + 1, LWhiteSpaceCount);
        if LHasLineBreak then
        begin
          Insert(#10#13, Result, LCharIndex + 1);
          LHasLineBreak := False;
        end
        else
        begin
          Insert(' ', Result, LCharIndex + 1);
        end;
      end;
      LWhiteSpaceCount := 0;
    end;

    Dec(LCharIndex);
  end;
  if LWhiteSpaceCount > 1 then
    Delete(Result, LCharIndex + 1, LWhiteSpaceCount);
end;

function RemoveWhitespace(const AStr: string): string;
begin
  with TRegExpr.Create do
    try
      Result := ReplaceRegExpr('\s+', AStr, '', False);
    finally
      Free;
    end;
end;

function RemoveW(const AHost: string): string;
begin
  with TRegExpr.Create do
    try
      InputString := AHost;
      Expression := 'www\d{0,2}\.';

      if Exec(InputString) then
        Result := copy(AHost, Pos(string(Match[0]), AHost) + Length(Match[0]))
      else
        Result := AHost;
    finally
      Free;
    end;
end;

function Trim(const S: string; const C: Char): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and ((S[I] <= ' ') or (S[I] = C)) do
    Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while ((S[L] <= ' ') or (S[L] = C)) do
      Dec(L);
    Result := copy(S, I, L - I + 1);
  end;
end;

function TrimLeft(const S: string; const C: Char): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and ((S[I] <= ' ') or (S[I] = C)) do
    Inc(I);
  Result := copy(S, I, Maxint);
end;

function TrimRight(const S: string; const C: Char): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and ((S[I] <= ' ') or ((S[I] = C))) do
    Dec(I);
  Result := copy(S, 1, I);
end;

function PadLeft(const S: string; Ch: Char; Len: Integer): string;
var
  RestLen: Integer;
begin
  Result := S;
  RestLen := Len - Length(S);
  if RestLen < 1 then
    Exit;
  Result := StringOfChar(Ch, RestLen) + S;
end;

function PadRight(const S: string; Ch: Char; Len: Integer): string;
var
  RestLen: Integer;
begin
  Result := S;
  RestLen := Len - Length(S);
  if RestLen < 1 then
    Exit;
  Result := S + StringOfChar(Ch, RestLen);
end;

end.
