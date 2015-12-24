unit uApiCodeTag;

interface

uses
  // Delphi
  SysUtils, Classes,
  // Utils
  uStringUtils;

const
  CodeTagsCount: Integer = 18;
  TCodeTags: array [0 .. 18 - 1] of string = ('bold', 'italic', 'underline', 'strikethrough', 'size', 'color', 'left', 'center', 'right', 'image', 'email',
    'youtube', 'url', 'list', 'quote', 'code', 'spoiler', 'hide');

type
  TCodeTag = record
    Name: string;
    Value: string;
    Params: TStringList;
    ParamValues: TStringList;
  end;

  TCodeParam = class
  private
    FName, FDescription, FValue: string;
    FIndex: Integer;
    FWasSelected: Boolean;
    FDefaultValues: TStringList;
  public
    constructor Create(AIndex: Integer);
    destructor Destroy; override;

    property Name: string read FName;
    property Index: Integer read FIndex write FIndex;

    property Description: string read FDescription write FDescription;
    property Value: string read FValue write FValue;

    property WasSelected: Boolean read FWasSelected write FWasSelected;

    function HasDefaultValues: Boolean;
    procedure SetDefaultValues(const ADefaultValues, ADefaultValuesDescription: string);
    property DefaultValues: TStringList read FDefaultValues;
    function NameFromValue(const AValue: string): string;
    function GetDefaultTextValues: TStrings;
  end;

implementation

{ TCodeParam }

constructor TCodeParam.Create(AIndex: Integer);
begin
  FName := 'param' + IntToStr(AIndex);
  FIndex := AIndex;
  FWasSelected := False;
  FDefaultValues := TStringList.Create;
end;

destructor TCodeParam.Destroy;
begin
  FDefaultValues.Free;
  inherited Destroy;
end;

function TCodeParam.HasDefaultValues: Boolean;
begin
  Result := not(FDefaultValues.Count = 0);
end;

procedure TCodeParam.SetDefaultValues(const ADefaultValues, ADefaultValuesDescription: string);
var
  I: Integer;
begin
  with SplittString('|', ADefaultValues, True) do
    try
      for I := 0 to Count - 1 do
        FDefaultValues.Add(Strings[I] + NameValueSeparator);
    finally
      Free;
    end;

  with SplittString('|', ADefaultValuesDescription, True) do
    try
      for I := 0 to Count - 1 do
        FDefaultValues.ValueFromIndex[I] := Strings[I];
    finally
      Free;
    end;
end;

function TCodeParam.NameFromValue(const AValue: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FDefaultValues.Count - 1 do
    if SameText(AValue,  FDefaultValues.ValueFromIndex[I]) then
      Exit(FDefaultValues.Names[I]);
end;

function TCodeParam.GetDefaultTextValues: TStrings;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to FDefaultValues.Count - 1 do
    Result.Add(FDefaultValues.ValueFromIndex[I]);
end;

end.
