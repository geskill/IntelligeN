unit uApiMody;

interface

uses
  // Delphi
  Classes, SysUtils, Dialogs,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Utils
  uPathUtils;

type
  TModyResult = record
    RemovedDouble: TStrings;
    MissingParts: TStrings;
  end;

  TMody = class
  public
    class function Mody(var AStringList: TStrings): TModyResult; overload;
    class function Mody(ADirectlinksMirror: IDirectlinksMirror): TModyResult; overload;
  end;

implementation

uses
  // Api
  uApiSettings;

class function TMody.Mody(var AStringList: TStrings): TModyResult;

  procedure DoSort;
  var
    I, J: Integer;
    Line, LineEx: string;
  begin
    for I := 1 to AStringList.Count - 1 do
      for J := AStringList.Count - 1 downto I do
      begin
        Line := ExtractUrlFileName(AStringList.Strings[J]);
        LineEx := ExtractUrlFileName(AStringList.Strings[J - 1]);
        if (Line < LineEx) then
        begin
          LineEx := AStringList.Strings[J - 1];
          AStringList.Strings[J - 1] := AStringList.Strings[J];
          AStringList.Strings[J] := LineEx;
        end;
      end;
  end;

  function DoRemoveDouble: TStrings;
  var
    StringList: TStringList;
    I, J: Integer;
    Line: string;
    found: Boolean;
  begin
    StringList := TStringList.Create;
    result := TStringList.Create;
    try
      StringList.Text := AStringList.Text;
      AStringList.Clear;

      for I := 0 to StringList.Count - 1 do
      begin
        Line := ExtractUrlFileName(StringList.Strings[I]);
        found := false;

        for J := 0 to AStringList.Count - 1 do
          if (Line = ExtractUrlFileName(AStringList.Strings[J])) then
            found := true;

        if found then
          result.Add(StringList.Strings[I])
        else
          AStringList.Add(StringList.Strings[I]);
      end;

      StringList.Clear;
    finally
      StringList.Free;
    end;
  end;

  function DoFindMissing: TStrings;
  var
    StringList: TStringList;
    RegExpr: TRegExpr;
    I, Y, Z: Integer;
    found: Boolean;
  begin
    result := TStringList.Create;

    StringList := TStringList.Create;
    RegExpr := TRegExpr.Create;
    try
      for I := 0 to AStringList.Count - 1 do
        StringList.Add(ExtractUrlFileName(AStringList.Strings[I]));

      with RegExpr do
      begin
        InputString := StringList.Text;
        Expression := '(7z\.|part|r|[.][0-9])([0-9]+)';

        StringList.Clear;
        if Exec(InputString) then
        begin
          repeat
            StringList.Add(Match[2]);
          until not ExecNext;
        end;
      end;
      StringList.Sort;

      if (StringList.Count > 0) then
        if not TryStrToInt(copy(StringList.Strings[StringList.Count - 1], length(StringList.Strings[StringList.Count - 1]) - 2, 3), I) then
          if not TryStrToInt(copy(StringList.Strings[StringList.Count - 1], length(StringList.Strings[StringList.Count - 1]) - 1, 2), I) then
            TryStrToInt(copy(StringList.Strings[StringList.Count - 1], length(StringList.Strings[StringList.Count - 1]), 1), I);

      for Y := 0 to I do
      begin
        found := false;
        for Z := 0 to StringList.Count - 1 do
          if StrToIntDef(StringList.Strings[Z], -1) = Y then
            found := true;
        if not found then
          result.Add(IntToStr(Y));
      end;
    finally
      RegExpr.Free;
      StringList.Free;
    end;
  end;

begin
  with SettingsManager.Settings.Mody do
  begin
    if Sort then
      DoSort;

    if RemoveDouble then
      result.RemovedDouble := DoRemoveDouble
    else
      result.RemovedDouble := TStringList.Create;

    if FindMissing then
      result.MissingParts := DoFindMissing
    else
      result.MissingParts := TStringList.Create;
  end;
end;

class function TMody.Mody(ADirectlinksMirror: IDirectlinksMirror): TModyResult;
var
  ReturnLinks: TStringList;

  procedure DoSort;
  var
    I, J: Integer;
    Line, LineEx: string;
  begin
    for I := 1 to ReturnLinks.Count - 1 do
      for J := ReturnLinks.Count - 1 downto I do
      begin
        Line := ADirectlinksMirror.GetPartName(ReturnLinks.Strings[J]);
        if SameText(Line, '') then
          Line := ExtractUrlFileName(ReturnLinks.Strings[J]);

        LineEx := ADirectlinksMirror.GetPartName(ReturnLinks.Strings[J - 1]);
        if SameText(LineEx, '') then
          LineEx := ExtractUrlFileName(ReturnLinks.Strings[J - 1]);
        if (Line < LineEx) then
        begin
          LineEx := ReturnLinks.Strings[J - 1];
          ReturnLinks.Strings[J - 1] := ReturnLinks.Strings[J];
          ReturnLinks.Strings[J] := LineEx;
        end;
      end;
  end;

  function DoRemoveDouble: TStrings;
  var
    StringList: TStringList;
    I, J: Integer;
    Line, LineEx: string;
    found: Boolean;
  begin
    StringList := TStringList.Create;
    result := TStringList.Create;
    try
      StringList.Text := ReturnLinks.Text;
      ReturnLinks.Clear;

      for I := 0 to StringList.Count - 1 do
      begin
        Line := ADirectlinksMirror.GetPartName(StringList.Strings[I]);
        if SameText(Line, '') then
          Line := ExtractUrlFileName(StringList.Strings[I]);

        found := false;

        for J := 0 to ReturnLinks.Count - 1 do
        begin
          LineEx := ADirectlinksMirror.GetPartName(ReturnLinks.Strings[J]);
          if SameText(LineEx, '') then
            LineEx := ExtractUrlFileName(ReturnLinks.Strings[J]);
          if (Line = LineEx) then
          begin
            found := true;
            break;
          end;
        end;

        if found then
          result.Add(StringList.Strings[I])
        else
          ReturnLinks.Add(StringList.Strings[I]);
      end;

      StringList.Clear;
    finally
      StringList.Free;
    end;
  end;

  function DoFindMissing: TStrings;
  var
    StringList: TStringList;
    RegExpr: TRegExpr;
    Line: string;
    I, Y, Z: Integer;
    found: Boolean;
  begin
    result := TStringList.Create;

    StringList := TStringList.Create;
    RegExpr := TRegExpr.Create;
    try
      for I := 0 to ReturnLinks.Count - 1 do
      begin
        Line := ADirectlinksMirror.GetPartName(ReturnLinks.Strings[I]);
        if SameText(Line, '') then
          Line := ExtractUrlFileName(ReturnLinks.Strings[I]);

        StringList.Add(Line);
      end;

      with RegExpr do
      begin
        InputString := StringList.Text;
        Expression := '(7z\.|part|r|[.][0-9])([0-9]+)';

        StringList.Clear;
        if Exec(InputString) then
        begin
          repeat
            StringList.Add(Match[2]);
          until not ExecNext;
        end;
      end;
      StringList.Sort;

      if (StringList.Count > 0) then
        if not TryStrToInt(copy(StringList.Strings[StringList.Count - 1], length(StringList.Strings[StringList.Count - 1]) - 2, 3), I) then
          if not TryStrToInt(copy(StringList.Strings[StringList.Count - 1], length(StringList.Strings[StringList.Count - 1]) - 1, 2), I) then
            TryStrToInt(copy(StringList.Strings[StringList.Count - 1], length(StringList.Strings[StringList.Count - 1]), 1), I);

      for Y := 0 to I do
      begin
        found := false;
        for Z := 0 to StringList.Count - 1 do
          if StrToIntDef(StringList.Strings[Z], -1) = Y then
            found := true;
        if not found then
          result.Add(IntToStr(Y));
      end;
    finally
      RegExpr.Free;
      StringList.Free;
    end;
  end;

begin
  ReturnLinks := TStringList.Create;
  try
    with SettingsManager.Settings.Mody do
    begin
      ReturnLinks.Text := ADirectlinksMirror.Value;

      if Sort then
        DoSort;

      if RemoveDouble then
        result.RemovedDouble := DoRemoveDouble
      else
        result.RemovedDouble := TStringList.Create;

      if FindMissing then
        result.MissingParts := DoFindMissing
      else
        result.MissingParts := TStringList.Create;
    end;
    ADirectlinksMirror.Value := ReturnLinks.Text;
  finally
    ReturnLinks.Free;
  end;
end;

end.
