unit uMyAdvmJScriptStyler;

interface

uses
  // Delphi
  Classes, Graphics,
  // AdvMemo
  AdvMemo,
  // Common
  uConst;

const
  AllJScriptKeyWords = '"FOR","IMPORT","IF","THIS","DO","WHILE","BREAK","{","}","(",")","SWITCH","CASE","DEFAULT","ELSE","FUNCTION",";","RESULT","VAR"';

type
  TAdvJScriptMemoStyler = class(TAdvCustomMemoStyler)
  private
    FVersion: string;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BlockStart;
    property BlockEnd;
    property EscapeChar;
    property LineComment;
    property MultiCommentLeft;
    property MultiCommentRight;
    property CommentStyle;
    property NumberStyle;
    property HighlightStyle;
    property AllStyles;
    property AutoCompletion;
    property HintParameter;

    property Version: string read FVersion;
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
  end;

implementation

{ TAdvJScriptMemoStyler }

constructor TAdvJScriptMemoStyler.Create(AOwner: TComponent);
var
  itm: TElementStyle;
  I: Integer;
begin
  inherited;
  FVersion := '3.0';
  Description := 'JavaScript';
  Filter := 'Javascript Files (*.js)|*.js';
  DefaultExtension := '.js';
  StylerName := 'JavaScript';
  Extensions := 'js';
  EscapeChar := '\';

  LineComment := '//';
  MultiCommentLeft := '/*';
  MultiCommentRight := '*/';
  BlockStart := '{';
  BlockEnd := '}';
  CommentStyle.TextColor := clSilver;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clBlue;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];

  // ------------Script Standard Default----------------
  itm := AllStyles.Add;
  itm.Info := 'Script Standard Default';
  itm.Font.Color := $00A00000;
  itm.Font.Style := [fsBold];
  itm.KeyWords.CommaText := AllJScriptKeyWords;
  // ------------Simple Quote ' '----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Simple Quote';
  itm.Font.Color := clBlue;
  itm.Font.Style := [];
  itm.BracketStart := #39;
  itm.BracketEnd := #39;
  // ------------Double Quote " "----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Double Quote';
  itm.Font.Color := clBlue;
  itm.Font.Style := [];
  itm.BracketStart := '"';
  itm.BracketEnd := '"';
  // ----------SYMBOL --------------
  itm := AllStyles.Add;
  itm.StyleType := stSymbol;
  itm.Info := 'Symbols Delimiters';
  itm.Font.Color := clBlack;
  itm.Font.Style := [];
  itm.Symbols := #32',;:.()[]=-*/^%<>#'#13 + #10;
  // ----------Javascript functions --------------
  itm := AllStyles.Add;
  itm.Info := 'JScript Standard Functions';
  itm.Font.Color := clPurple;
  itm.Font.Style := [];
  itm.KeyWords.CommaText := '"PRINT"';
  // ----------Javascript functions --------------
  itm := AllStyles.Add;
  itm.Info := 'JScript Functions';
  itm.Font.Color := clGreen;
  itm.Font.Style := [];
  itm.KeyWords.CommaText := '"INTTOSTR","FLOATTOSTR","BOOLTOSTR","VARTOSTR","STRTOINT","STRTOINT64","STRTOFLOAT","STRTOBOOL","FORMAT","FORMATFLOAT",' +
    '"LENGTH","COPY","POS","DELETE","DELETESTR","INSERT","UPPERCASE","LOWERCASE","TRIM","NAMECASE","COMPARETEXT","CHR","ORD","SETLENGTH",' +
    '"ROUND","TRUNC","INT","FRAC","SQRT","ABS","SIN","COS","ARCTAN","TAN","EXP","LN","PI","INC","DEC",' +
    '"MATCHTEXT","REPLACEREGEXPR","CHARCOUNT","POSEX","EXTRACTURLFILENAME","EXTRACTURLPROTOCOL","EXTRACTURLHOST"';

  with HintParameter.Parameters do
  begin
    Add('for(initialization; termination; increment)');

    Add('IntToStr(i: Integer): string');
    Add('FloatToStr(e: Extended): string');
    Add('BoolToStr(B: Boolean): string');
    Add('VarToStr(v: Variant): string');

    Add('StrToInt(s: string): Integer');
    Add('StrToInt64(s: string): Integer');
    Add('StrToFloat(s: string): Extended');
    Add('StrToBool(const S: string): Boolean');

    Add('Format(Fmt: string, Args: array): string');
    Add('FormatFloat(Fmt: string, Value: Extended): string');

    Add('Length(s: Variant): Integer');
    Add('Copy(s: string, from: Integer, count: Integer): string');
    Add('Pos(substr: string, s: string): Integer');
    Add('Delete(var s: string, from: Integer, count: Integer)');
    Add('DeleteStr(var s: string, from: Integer, count: Integer)');
    Add('Insert(s: string, var s2: string, pos: Integer)');
    Add('Uppercase(s: string): string');
    Add('Lowercase(s: string): string');
    Add('Trim(s: string): string');
    Add('NameCase(s: string): string');
    Add('CompareText(s: string, s1: string): Integer');
    Add('Chr(i: Integer): Char');
    Add('Ord(ch: Char): Integer');
    Add('SetLength(var S: Variant, L: Integer)');

    Add('Round(e: Extended): Integer');
    Add('Trunc(e: Extended): Integer');
    Add('Int(e: Extended): Integer');
    Add('Frac(X: Extended): Extended');
    Add('Sqrt(e: Extended): Extended');
    Add('Abs(e: Extended): Extended');
    Add('Sin(e: Extended): Extended');
    Add('Cos(e: Extended): Extended');
    Add('ArcTan(X: Extended): Extended');
    Add('Tan(X: Extended): Extended');
    Add('Exp(X: Extended): Extended');
    Add('Ln(X: Extended): Extended');
    Add('Pi: Extended');

    Add('Inc(var i: Integer, incr: Integer = 1)');
    Add('Dec(var i: Integer, decr: Integer = 1)');

    Add('MatchText(const Mask: string, S: string, CaseSensitive: Boolean = False): Boolean');
    Add('ReplaceRegExpr(const ARegExpr: string, AInputStr: string, AReplaceStr: string, AUseSubstitution: Boolean = False): string');

    Add('print(Msg: string)');

    Add('CharCount(const SubStr: string, S: string): Integer');
    Add('PosEx(const SubStr: string, S: string, Offset: Integer = 1): Integer');

    Add('ExtractUrlFileName(const AUrl: string): string');
    Add('ExtractUrlProtocol(const AUrl: string): string');
    Add('ExtractUrlHost(const AUrl: string): string');
  end;

  with AutoCompletion do
  begin
    Add('function IntToStr(i: Integer): string');
    Add('function FloatToStr(e: Extended): string');
    Add('function BoolToStr(B: Boolean): string');
    Add('function VarToStr(v: Variant): string');

    Add('function StrToInt(s: string): Integer');
    Add('function StrToInt64(s: string): Integer');
    Add('function StrToFloat(s: string): Extended');
    Add('function StrToBool(const S: string): Boolean');

    Add('function Format(Fmt: string, Args: array): string');
    Add('function FormatFloat(Fmt: string, Value: Extended): string');

    Add('function Length(s: Variant): Integer');
    Add('function Copy(s: string, from: Integer, count: Integer): string');
    Add('function Pos(substr: string, s: string): Integer');
    Add('procedure Delete(var s: string, from: Integer, count: Integer)');
    Add('procedure DeleteStr(var s: string, from: Integer, count: Integer)');
    Add('procedure Insert(s: string, var s2: string, pos: Integer)');
    Add('function Uppercase(s: string): string');
    Add('function Lowercase(s: string): string');
    Add('function Trim(s: string): string');
    Add('function NameCase(s: string): string');
    Add('function CompareText(s: string, s1: string): Integer');
    Add('function Chr(i: Integer): Char');
    Add('function Ord(ch: Char): Integer');
    Add('procedure SetLength(var S: Variant, L: Integer)');

    Add('function Round(e: Extended): Integer');
    Add('function Trunc(e: Extended): Integer');
    Add('function Int(e: Extended): Integer');
    Add('function Frac(X: Extended): Extended');
    Add('function Sqrt(e: Extended): Extended');
    Add('function Abs(e: Extended): Extended');
    Add('function Sin(e: Extended): Extended');
    Add('function Cos(e: Extended): Extended');
    Add('function ArcTan(X: Extended): Extended');
    Add('function Tan(X: Extended): Extended');
    Add('function Exp(X: Extended): Extended');
    Add('function Ln(X: Extended): Extended');
    Add('function Pi: Extended');

    Add('procedure Inc(var i: Integer, incr: Integer = 1)');
    Add('procedure Dec(var i: Integer, decr: Integer = 1)');

    Add('function MatchText(const Mask: string, S: string, CaseSensitive: Boolean = False): Boolean');
    Add('function ReplaceRegExpr(const ARegExpr: string, AInputStr: string, AReplaceStr: string, AUseSubstitution: Boolean = False): string');

    Add('procedure print(Msg: string)');

    Add('function CharCount(const SubStr: string, S: string): Integer');
    Add('function PosEx(const SubStr: string, S: string, Offset: Integer = 1): Integer');

    Add('function ExtractUrlFileName(const AUrl: string): string');
    Add('function ExtractUrlProtocol(const AUrl: string): string');
    Add('function ExtractUrlHost(const AUrl: string): string');

    Add('const IType: string');
    Add('const ICMS: string');
    Add('const IWebsite: string');

    for I := 0 to length(TStringComponentID) - 1 do
      Add('const ' + TStringComponentID[I] + ': string');

    Add('property IMirror: [const IndexOrName: OleVariant]: IMirrorContainer');
  end;

end;

end.
