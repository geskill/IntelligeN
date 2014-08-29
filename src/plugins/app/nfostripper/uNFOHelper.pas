unit uNFOHelper;

interface

uses
  // Delphi
  Classes, SysUtils;

type
  TNFOHelper = class
  private
    function IsAscii(C: Char): Boolean;
    function IsLineBreak(C: Char): Boolean;

    function LastPos(const SubStr, S: string): Integer;
    function FindLastNotOf(const SubStr, S: string; const AStartPos: Integer = 1): Integer;

    function TrimRight(const SubStr, S: string): string;
  public
    function AsStrippedText(const ANFO: string): string;
  end;

implementation

function TNFOHelper.IsAscii(C: Char): Boolean;
begin
  Result := Integer(C) <= $7F;
end;

function TNFOHelper.IsLineBreak(C: Char): Boolean;
begin
  Result := Ord(C) in [10, 13];
end;

function TNFOHelper.LastPos(const SubStr, S: string): Integer;
var
  I, J, K: Integer;
begin
  Result := 0;
  I := Length(S);
  K := Length(SubStr);
  if (K = 0) or (K > I) then
    Exit;
  while (Result = 0) and (I >= K) do
  begin
    J := K;
    if S[I] = SubStr[J] then
    begin
      while (J > 1) and (S[I + J - K - 1] = SubStr[J - 1]) do
        Dec(J);
      if J = 1 then
        Result := I - K + 1;
    end;
    Dec(I);
  end;
end;

function TNFOHelper.FindLastNotOf(const SubStr, S: string; const AStartPos: Integer = 1): Integer;
var
  I, Pos: Integer;
begin
  Result := 0;

  if Length(SubStr) = 0 then
  begin
    Result := AStartPos;
    Exit;
  end;

  for I := Length(S) downto 1 do
  begin
    Pos := AStartPos + I;
    if LastPos(S[I], SubStr) = 0 then
    begin
      Result := Pos;
      Exit;
    end;
  end;
end;

function TNFOHelper.TrimRight(const SubStr, S: string): string;
var
  I: Integer;
begin
  I := FindLastNotOf(SubStr, S);
  if not(I = 0) then
  begin
    Result := copy(S, 1, I - 1);
  end
  else
  begin
    Result := '';
  end;
end;

function TNFOHelper.AsStrippedText(const ANFO: string): string;
var
  InitialNFO, NFO: string;
  I: Integer;
begin
  InitialNFO := AdjustLineBreaks(ANFO, tlbsLF);

  // Filter for ASCII chars
  // Remove all unneeded spaces at the right side
  for I := 1 to Length(InitialNFO) do
  begin
    if IsAscii(InitialNFO[I]) then
    begin
      if IsLineBreak(InitialNFO[I]) then
      begin
        NFO := TrimRight(' ', NFO);
      end;

      NFO := NFO + InitialNFO[I];
    end
    else
    begin
      NFO := NFO + ' ';
    end;
  end;

  I := 1;
  while I < (Length(NFO)) do
  begin
    if (IsLineBreak(NFO[I]) and (I < Length(NFO) - 2) and IsLineBreak(NFO[I + 1])) then
    begin
      Inc(I, 2);

      while ((I < Length(NFO)) and IsLineBreak(NFO[I])) do
      begin
        Delete(NFO, I, 1);
      end;
    end
    else
    begin
      Inc(I, 1);
    end;
  end;

  NFO := AdjustLineBreaks(NFO, tlbsCRLF);

  with TStringList.Create() do
  try
    Text := NFO;
    for I := 0 to Count - 1 do
      Strings[I] := TrimLeft(Strings[I]);
    NFO := Text;
  finally
    Free;
  end;

  NFO := Trim(NFO);

  Result := NFO;
end;

end.
