unit EZTexturePanel;

{ -------------------------------------------------------------------------------
  TEZTexturePanel v1.4

  Included files: EZTextureReg.pas, EZTextureReg.dfm, EZJTexturePanelD6.dpk and
  EZJTexturePanelD6.res.

  TEZTexturePanel is a panel with additional properties. Properties included are:
  gradient background, texture background, moveable, sizeable, custom shape,
  shades all objects with TEZTexturePanel as parent.

  "Author" herein refers to Eric Z. Jordens (the creator of EZTexturePanel).
  "Software" refers to all files included with EZJTexturePanel distribution
  package.

  EZTexturePanel is distributed as a freeware. You are free to use EZTexturePanel
  as part of your application for any purpose including freeware, commercial and
  shareware applications, provided an explicit credit is given to the author in
  application's about box and/or accompanying documentation.

  This software is provided 'as-is', without warranty of any kind, either
  expressed or implied. In no event shall the author be held liable for any
  damages arising from the use of this software.

  Eric Z. Jordens 24-02-04

  -------------------------------------------------------------------------------

  minor changes by geskill 23.06.2011:

  TShadeObject: uses now full strings not limited to 255 chars
  NoBlendeShade: FindComponent <> nil
  CheckShadeAction: FindComponent <> nil
  ------------------------------------------------------------------------------- }

interface

{$WARNINGS OFF}

uses
  Windows, Forms, Types, Messages, SysUtils, Classes, Controls, ComCtrls, ExtCtrls, Graphics, Math, StdCtrls, RichEdit;

// , Printers, ,
const
  MaxKernelSize = 100;
  MaxPixelCount = 32768;
  Version = 'EZT14';

type
  TMouseEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TRGBArray = array [0 .. 32767] of TRGBTriple;
  pRGBArray = ^TRGBArray;
  PRow = ^TRow;
  TRow = array [0 .. 1000000] of TRGBTriple;
  PPRows = ^TPRows;
  TPRows = array [0 .. 1000000] of PRow;
  TKernelSize = 1 .. MaxKernelSize;

  TKernel = record
    Size: TKernelSize;
    Weights: array [-MaxKernelSize .. MaxKernelSize] of single;
  end;

  TTextureType = (ttNone, ttCenter, ttTile, ttStrech, ttGradient, ttBlend);
  TShadeAction = (saNone, saWindow, saText, saShape);
  TGradType = (gtTopBottom, gtLeftRight, gtCircle, gtHorizontal, gtVertical, gtSquare);

  TShadeObject = record
    name: string;
    Act: TShadeAction;
    Visible: byte;
    L, T, W, H: Integer;
    Inherit: string;
  end;

  TShadeObjects = array of TShadeObject;

  TEZShades = class(TMemoryStream);

    TEZTextureSettings = class(TPersistent)private FShadeHeight: Integer;
    FShadeDiffusion: Integer;
    FShadeAngle: Integer;
    FShadeColor: TColor;
    FSmooth: boolean;
    FTexture: TBitmap;
    FTType: TTextureType;
    FScale: double;
    FGrad1: TColor;
    FGrad2: TColor;
    FMove: boolean;
    FGradType: TGradType;
    FX0: Integer;
    FY0: Integer;
    FShadeShow: boolean;
    FEZShape: TBitmap;
    FSize: boolean;
    EZShadeOb: TShadeObjects;
    FPaint: TNotifyEvent;
    FBCol: TColor;
    FBlendCol: TColor;
    FBlend: byte;
    OldS: boolean;
    OldT: TTextureType;
  public
    BlockRepaint: boolean;
    FRegion: THandle;
    Hw: HWND;
    function CreateRegion(Bmp: TBitmap): THandle;
    procedure SetGradType(Value: TGradType);
    procedure SetOffsetX(Value: Integer);
    procedure SetOffsety(Value: Integer);
    procedure SetSize(Value: boolean);
    procedure SetShadeHeight(Value: Integer);
    procedure SetShadeDiffusion(Value: Integer);
    procedure SetShadeAngle(Value: Integer);
    procedure SetShadeColor(Value: TColor);
    procedure SetShadeShow(Value: boolean);
    procedure SetShape(Value: TBitmap);
    procedure SetTexture(Value: TBitmap);
    procedure SetTType(Value: TTextureType);
    procedure SetSmooth(Value: boolean);
    procedure SetScale(Value: double);
    procedure SetGrad1(Value: TColor);
    procedure SetGrad2(Value: TColor);
    procedure SetBCol(Value: TColor);
    procedure SetBlendCol(Value: TColor);
    procedure SetBlend(Value: byte);
    procedure Region;
    constructor Create;
    destructor Destroy; override;
  published
    property AlphaBlendColor: TColor read FBlendCol write SetBlendCol;
    property AlphaBlend: byte read FBlend write SetBlend;
    property BorderColor: TColor read FBCol write SetBCol;
    property Moveable: boolean read FMove write FMove;
    property Sizeable: boolean read FSize write SetSize;
    property GradientColor1: TColor read FGrad1 write SetGrad1;
    property GradientColor2: TColor read FGrad2 write SetGrad2;
    property GradientType: TGradType read FGradType write SetGradType;
    property GradientX0: Integer read FX0 write SetOffsetX;
    property GradientY0: Integer read FY0 write SetOffsety;
    property Texture: TBitmap read FTexture write SetTexture;
    property TextureType: TTextureType read FTType write SetTType;
    property TextureScale: double read FScale write SetScale;
    property TextureSmooth: boolean read FSmooth write SetSmooth;
    property ShadeAngle: Integer read FShadeAngle write SetShadeAngle;
    property ShadeColor: TColor read FShadeColor write SetShadeColor;
    property ShadeDiffusion: Integer read FShadeDiffusion write SetShadeDiffusion;
    property ShadeHeight: Integer read FShadeHeight write SetShadeHeight;
    property ShadeShow: boolean read FShadeShow write SetShadeShow;
    property Shape: TBitmap read FEZShape write SetShape;
    property OnChange: TNotifyEvent read FPaint write FPaint;
  end;

  TEZTexturePanel = class(TPanel)
  private
    Shades: TEZShades;
    TempBmp: TBitmap;
    Temp: TBitmap;
    FMouseD: TMouseEvent;
    FMouseU: TMouseEvent;
    FBIn: TPanelBevel;
    FBOut: TPanelBevel;
    IsSizeing: boolean;
    Pos: TPoint;
    FCtl3D: boolean;
    FCaption: string;
    First: boolean;
    Count: Integer;
    FColor: TColor;
    FSettings: TEZTextureSettings;
    BackCopy: boolean;
    function GetShades: TEZShades;
    procedure SetShades(Value: TEZShades);
    procedure SetBevelIn(Value: TPanelBevel);
    procedure SetBevelOut(Value: TPanelBevel);
    procedure SetCtl3D(Value: boolean);
    procedure SetCaption(Value: string);
    procedure SetColor(Value: TColor);
    procedure DrawBorder;
    procedure WriteData(Stream: TStream);
    procedure ReadData(Stream: TStream);
    procedure DrawWindowShade(Obj: TObject);
    procedure DrawTextShade(Obj: TObject);
    procedure DrawShapeShade(Obj: TObject);
    procedure DrawShapeOwn(Obj: TObject);
    procedure DrawShade;
    procedure ApplyShade(L, T: Integer; Bmp: TBitmap);
    procedure DrawGrad(Col1, Col2: TColor);
    procedure DrawChange(Sender: TObject);
    procedure BackToBmp;
    procedure BlendIt(Col: TColor; Alpha: byte; BlendBmp: TBitmap);
  protected
    procedure WMEraseBkgnd(var message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure DefineProperties(Filer: TFiler); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Redraw;
    function UpdateShadeObjects: boolean;
    procedure UpdateTexture;
    procedure UpdateOffspring;
    procedure SaveSettings(Stream: TMemoryStream);
    procedure LoadSettings(Stream: TMemoryStream);
    function SetShade(CompName: string; Shaded: boolean): boolean;
    function IsShaded(CompName: string): boolean;
  published
    property Shadeobjects: TEZShades read GetShades write SetShades;
    property Settings: TEZTextureSettings read FSettings write FSettings;
    property BevelInner: TPanelBevel read FBIn write SetBevelIn;
    property BevelOuter: TPanelBevel read FBOut write SetBevelOut;
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
    property Ctl3D: boolean read FCtl3D write SetCtl3D;
    property OnMouseDown: TMouseEvent read FMouseD write FMouseD;
    property OnMouseUp: TMouseEvent read FMouseU write FMouseU;
  end;

procedure LoadShades(var Shadeobjects: TShadeObjects; Shades: TEZShades);
procedure SaveShades(var Shades: TEZShades; Shadeobjects: TShadeObjects);

implementation

/// /////////////////////////////////////////////////////////////////////////////
function Norm24BitCol(Col: TColor): TColor; // Returns the corresponding 24-bit color if Col is a system color (negative value)
begin
{$WARNINGS OFF}
  if (Col < 0) then
    Result := GetSysColor(Col + $80000000)
  else
    Result := Col;
{$WARNINGS ON}
end;

// ------------------------------------------------------------------------------
procedure StreamToBitmap(Bitmap: TBitmap; Stream: TMemoryStream);
var
  Sz, N: Integer;
begin
  Stream.read(Sz, SizeOf(Integer));
  N := Stream.Position + Sz; // Get Position after loading bitmap
  if Sz > 0 then
  begin
    Bitmap.LoadFromStream(Stream);
    Stream.Position := N; // Set to good position
  end;
end;

// ------------------------------------------------------------------------------
procedure BitmapToStream(Bitmap: TBitmap; Stream: TMemoryStream);
var
  Temp: TMemoryStream;
  Sz: Integer;
begin
  Temp := TMemoryStream.Create;
  Bitmap.SaveToStream(Temp);
  Sz := Temp.Size;
  Temp.Free;
  Stream.write(Sz, SizeOf(Integer));
  if Sz > 0 then
    Bitmap.SaveToStream(Stream);
end;

// ------------------------------------------------------------------------------
procedure StringToStream(Str: string; Stream: TMemoryStream); overload;
var
  C, I: byte;
begin
  C := Length(Str);
  Stream.write(C, SizeOf(byte));
  for I := 1 to C do
    Stream.write(Str[I], SizeOf(char));
end;

// ------------------------------------------------------------------------------
function StreamToString(Stream: TMemoryStream): string; overload;
var
  C, I: byte;
  Ch: char;
begin
  Result := '';
  Stream.read(C, SizeOf(byte));
  for I := 1 to C do
  begin
    Stream.read(Ch, SizeOf(char));
    Result := Result + Ch;
  end;
end;

// ------------------------------------------------------------------------------
function NewPos(X, Y, X0, Y0: double; Angle: double): TPoint;
var
  X1, Y1: double;
  a: double;
begin
  a := -DegToRad(Angle);
  X1 := X - X0;
  Y1 := Y - Y0;
  Result.X := Round(Cos(a) * (X1) + Sin(a) * (Y1) + X0);
  Result.Y := Round(-Sin(a) * (X1) + Cos(a) * (Y1) + Y0);
end;

// ------------------------------------------------------------------------------
procedure StringToStream(Str: string; Stream: TEZShades); overload;
var
  C, I: byte;
begin
  C := Length(Str);
  Stream.write(C, SizeOf(byte));
  for I := 1 to C do
    Stream.write(Str[I], SizeOf(char));
end;

// ------------------------------------------------------------------------------
function StreamToString(Stream: TEZShades): string; overload;
var
  C, I: byte;
  Ch: char;
begin
  Result := '';
  Stream.read(C, SizeOf(byte));
  for I := 1 to C do
  begin
    Stream.read(Ch, SizeOf(char));
    Result := Result + Ch;
  end;
end;

// ------------------------------------------------------------------------------
procedure LoadShades(var Shadeobjects: TShadeObjects; Shades: TEZShades);
var
  I, J: Integer;
  C: array [0 .. 5] of char;
  B: boolean;
begin
  Shades.Position := 0;

  Shades.read(C, SizeOf(C));
  if (C = Version) then
    B := True
  else
    B := False;
  if not B then
    Shades.Position := 0;

  Shades.read(J, SizeOf(Integer));
  SetLength(Shadeobjects, J);
  if J > 0 then
    for I := 0 to J - 1 do
    begin
      Shadeobjects[I].name := StreamToString(Shades);
      Shades.read(Shadeobjects[I].Act, SizeOf(TShadeAction));
      Shades.read(Shadeobjects[I].Visible, SizeOf(byte));
      Shades.read(Shadeobjects[I].L, SizeOf(Integer));
      Shades.read(Shadeobjects[I].T, SizeOf(Integer));
      Shades.read(Shadeobjects[I].W, SizeOf(Integer));
      Shades.read(Shadeobjects[I].H, SizeOf(Integer));
      if B then
        Shadeobjects[I].Inherit := StreamToString(Shades);
    end;
end;

// ------------------------------------------------------------------------------
procedure SaveShades(var Shades: TEZShades; Shadeobjects: TShadeObjects);
var
  I, J: Integer;
  C: array [0 .. 5] of char;
begin
  Shades.Clear;
  Shades.Position := 0;
  C := Version;
  J := Length(Shadeobjects);
  Shades.write(C, SizeOf(C));
  Shades.write(J, SizeOf(Integer));
  if J > 0 then
    for I := 0 to J - 1 do
    begin
      StringToStream(Shadeobjects[I].name, Shades);
      Shades.write(Shadeobjects[I].Act, SizeOf(TShadeAction));
      Shades.write(Shadeobjects[I].Visible, SizeOf(byte));
      Shades.write(Shadeobjects[I].L, SizeOf(Integer));
      Shades.write(Shadeobjects[I].T, SizeOf(Integer));
      Shades.write(Shadeobjects[I].W, SizeOf(Integer));
      Shades.write(Shadeobjects[I].H, SizeOf(Integer));
      StringToStream(Shadeobjects[I].Inherit, Shades);
    end;
end;

// ------------------------------------------------------------------------------
function TrimInt(I, Min, Max: Integer): Integer;
begin
  if I > Max then
    Result := Max
  else if I < Min then
    Result := Min
  else
    Result := I;
end;

procedure MakeGaussianKernel(var K: TKernel; radius: double; MaxData, DataGranularity: double);
var
  J: Integer;
  Temp, delta: double;
  KernelSize: TKernelSize;
begin
  for J := low(K.Weights) to high(K.Weights) do
  begin
    Temp := J / radius;
    K.Weights[J] := exp(-Temp * Temp / 2);
  end;
  Temp := 0;
  for J := low(K.Weights) to high(K.Weights) do
    Temp := Temp + K.Weights[J];
  for J := low(K.Weights) to high(K.Weights) do
    K.Weights[J] := K.Weights[J] / Temp;
  KernelSize := MaxKernelSize;
  delta := DataGranularity / (2 * MaxData);
  Temp := 0;
  while (Temp < delta) and (KernelSize > 1) do
  begin
    Temp := Temp + 2 * K.Weights[KernelSize];
    dec(KernelSize);
  end;
  K.Size := KernelSize;
  Temp := 0;
  for J := -K.Size to K.Size do
    Temp := Temp + K.Weights[J];
  for J := -K.Size to K.Size do
    K.Weights[J] := K.Weights[J] / Temp;
end;

{ "Gaussian Blur in Delphi" from The Delphi Pool }
function TrimReal(Lower, Upper: Integer; X: double): Integer;
begin
  if (X < Upper) and (X >= Lower) then
    Result := trunc(X)
  else if X > Upper then
    Result := Upper
  else
    Result := Lower;
end;

{ "Gaussian Blur in Delphi" from The Delphi Pool }
procedure BlurRow(var theRow: array of TRGBTriple; K: TKernel; P: PRow);
var
  J, N: Integer;
  tr, tg, tb: double;
  W: double;
begin
  for J := 0 to high(theRow) do
  begin
    tb := 0;
    tg := 0;
    tr := 0;
    for N := -K.Size to K.Size do
    begin
      W := K.Weights[N];
      with theRow[TrimInt(J - N, 0, high(theRow))] do
      begin
        tb := tb + W * rgbtBlue;
        tg := tg + W * rgbtGreen;
        tr := tr + W * rgbtRed;
      end;
    end;
    with P[J] do
    begin
      rgbtBlue := TrimReal(0, 255, tb);
      rgbtGreen := TrimReal(0, 255, tg);
      rgbtRed := TrimReal(0, 255, tr);
    end;
  end;
  Move(P[0], theRow[0], ( high(theRow) + 1) * SizeOf(TRGBTriple));
end;

{ "Gaussian Blur in Delphi" from The Delphi Pool }
procedure GBlur(theBitmap: TBitmap; radius: double);
var
  Row, Col: Integer;
  theRows: PPRows;
  K: TKernel;
  ACol: PRow;
  P: PRow;
begin
  if (radius > 0.001) then
  begin
    if (theBitmap.HandleType <> bmDIB) or (theBitmap.PixelFormat <> pf24Bit) then
      raise exception.Create('GBlur only works for 24-bit bitmaps');
    MakeGaussianKernel(K, radius, 255, 1);
    GetMem(theRows, theBitmap.Height * SizeOf(PRow));
    GetMem(ACol, theBitmap.Height * SizeOf(TRGBTriple));
    { record the location of the bitmap data: }
    for Row := 0 to theBitmap.Height - 1 do
      theRows[Row] := theBitmap.Scanline[Row];
    { blur each row: }
    P := AllocMem(theBitmap.Width * SizeOf(TRGBTriple));
    for Row := 0 to theBitmap.Height - 1 do
      BlurRow(Slice(theRows[Row]^, theBitmap.Width), K, P);
    { now blur each column }
    ReAllocMem(P, theBitmap.Height * SizeOf(TRGBTriple));
    for Col := 0 to theBitmap.Width - 1 do
    begin
      { first read the column into a TRow: }
      for Row := 0 to theBitmap.Height - 1 do
        ACol[Row] := theRows[Row][Col];
      BlurRow(Slice(ACol^, theBitmap.Height), K, P);
      { now put that row, um, column back into the data: }
      for Row := 0 to theBitmap.Height - 1 do
        theRows[Row][Col] := ACol[Row];
    end;
    FreeMem(theRows);
    FreeMem(ACol);
    ReAllocMem(P, 0);
  end;
end;

// ------------------------------------------------------------------------------
procedure IntensityToGradient(var Pix: TRGBTriple; Intensity: double; Min, Max: TColor);
begin
  Pix.rgbtRed := trunc(GetRValue(Min) * (1 - Intensity) + GetRValue(Max) * Intensity);
  Pix.rgbtGreen := trunc(GetGValue(Min) * (1 - Intensity) + GetGValue(Max) * Intensity);
  Pix.rgbtBlue := trunc(GetBValue(Min) * (1 - Intensity) + GetBValue(Max) * Intensity);
end;

// ------------------------------------------------------------------------------// SmoothResize has been obtained from the library janFX.pas, written by Jan Verhoeven (2-july-2000). Source: http://www.torry.net section Graphics - Effects
procedure SmoothResize(Src, Dst: TBitmap);
var
  X, Y, xP, yP, yP2, xP2, T, t3, t13, z, z2, iz2, w1, w2, w3, w4: Integer;
  read, Read2, pc: PByteArray;
  Col1r, col1g, col1b, Col2r, col2g, col2b: byte;
begin
  if (Src.PixelFormat <> pf24Bit) or (Dst.PixelFormat <> pf24Bit) then
    Exit; // small modification
  xP2 := ((Src.Width - 1) shl 15) div Dst.Width;
  yP2 := ((Src.Height - 1) shl 15) div Dst.Height;
  yP := 0;
  for Y := 0 to Dst.Height - 1 do
  begin
    xP := 0;
    read := Src.Scanline[yP shr 15];
    if yP shr 16 < Src.Height - 1 then
      Read2 := Src.Scanline[yP shr 15 + 1]
    else
      Read2 := Src.Scanline[yP shr 15];
    pc := Dst.Scanline[Y];
    z2 := yP and $7FFF;
    iz2 := $8000 - z2;
    for X := 0 to Dst.Width - 1 do
    begin
      T := xP shr 15;
      t3 := T * 3;
      t13 := t3 + 3;
      Col1r := read[t3];
      col1g := read[t3 + 1];
      col1b := read[t3 + 2];
      Col2r := Read2[t3];
      col2g := Read2[t3 + 1];
      col2b := Read2[t3 + 2];
      z := xP and $7FFF;
      w2 := (z * iz2) shr 15;
      w1 := iz2 - w2;
      w4 := (z * z2) shr 15;
      w3 := z2 - w4;
      pc[X * 3 + 2] := (col1b * w1 + read[t13 + 2] * w2 + col2b * w3 + Read2[t13 + 2] * w4) shr 15;
      pc[X * 3 + 1] := (col1g * w1 + read[t13 + 1] * w2 + col2g * w3 + Read2[t13 + 1] * w4) shr 15;
      pc[X * 3] := (Col1r * w1 + Read2[t13] * w2 + Col2r * w3 + Read2[t13] * w4) shr 15;
      Inc(xP, xP2);
    end;
    Inc(yP, yP2);
  end;
end;

// ------------------------------------------------------------------------------
procedure OutputLabel2Canv(Owner: TWinControl; TxtLabel: TLabel; ImageHolder: TCanvas; Offset: Integer);
var
  richedit_outputarea: TRect;
  fmtRange: TFormatRange;
  RichHolder: TRichEdit;
  Bmp: TBitmap;
begin
  if (TxtLabel.Caption <> '') then
  begin
    Bmp := TBitmap.Create;
    Bmp.PixelFormat := pf24Bit;
    Bmp.Width := TxtLabel.Width;
    Bmp.Height := TxtLabel.Height;
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.Brush.Style := bsSolid;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
    if TxtLabel.WordWrap then
    begin
      RichHolder := TRichEdit.Create(Owner);
      RichHolder.Left := -4000;
      RichHolder.Visible := False;
      RichHolder.Parent := Owner;

      RichHolder.Lines.Add(TxtLabel.Caption);
      RichHolder.SelectAll;
      RichHolder.SelAttributes.Color := clBlack;
      RichHolder.SelAttributes.Style := TxtLabel.Font.Style;
      RichHolder.SelAttributes.name := TxtLabel.Font.name;
      RichHolder.SelAttributes.Size := TxtLabel.Font.Size;

      RichHolder.WordWrap := True;
      RichHolder.Width := TxtLabel.Width;
      RichHolder.Height := TxtLabel.Height;

      FillChar(fmtRange, SizeOf(TFormatRange), 0);
      richedit_outputarea := Rect(0, 0, TxtLabel.Width * 1440 div Screen.PixelsPerInch, TxtLabel.Height * 1440 div Screen.PixelsPerInch);
      fmtRange.hDC := Bmp.Canvas.Handle;
      fmtRange.hdcTarget := Bmp.Canvas.Handle;
      fmtRange.rc := richedit_outputarea;
      fmtRange.rcPage := richedit_outputarea;
      fmtRange.chrg.cpMin := 0;
      fmtRange.chrg.cpMax := -1;
      SetMapMode(Bmp.Canvas.Handle, MM_TEXT);
      RichHolder.Perform(EM_FORMATRANGE, 1, Longint(@fmtRange));
      RichHolder.Perform(EM_FORMATRANGE, 0, 0);

      RichHolder.Free;
    end
    else
    begin
      Bmp.Canvas.Font.Size := TxtLabel.Font.Size;
      Bmp.Canvas.Font.Style := TxtLabel.Font.Style;
      Bmp.Canvas.Font.name := TxtLabel.Font.name;
      Bmp.Canvas.Font.Color := clBlack;
      Bmp.Canvas.Brush.Color := clWhite;
      Bmp.Canvas.TextOut(0, 0, TxtLabel.Caption);
    end;
    ImageHolder.Draw(Offset, Offset, Bmp);
    Bmp.Free;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
constructor TEZTextureSettings.Create;
begin
  inherited Create;
  FTexture := TBitmap.Create;
  FEZShape := TBitmap.Create;
  FEZShape.PixelFormat := pf24Bit;
  FTType := ttGradient;
  FGrad1 := clGray;
  FGrad2 := clBtnFace;
  FBCol := clWindowFrame;
  FMove := False;
  FSize := False;
  FScale := 1;
  FShadeHeight := 5;
  FShadeDiffusion := 3;
  FShadeAngle := 315;
  FShadeColor := clBlack;
  FShadeShow := True;
  FGradType := gtTopBottom;
  FBlendCol := clBlack;
  FBlend := 128;
  BlockRepaint := False;
  SetLength(EZShadeOb, 0);
end;

// ------------------------------------------------------------------------------
destructor TEZTextureSettings.Destroy;
begin
  FTexture.Free;
  FEZShape.Free;
  if (FRegion <> 0) then
    DeleteObject(FRegion);
  inherited Destroy;
end;

// ------------------------------------------------------------------------------
function TEZTextureSettings.CreateRegion(Bmp: TBitmap): THandle;
var
  X, Y, StartX: Integer;
  Excl: THandle;
  Row: pRGBArray;
  TransparentColor: TRGBTriple;
begin
  Bmp.PixelFormat := pf24Bit;

  Result := CreateRectRGN(0, 0, Bmp.Width, Bmp.Height);

  for Y := 0 to Bmp.Height - 1 do
  begin
    Row := Bmp.Scanline[Y];

    StartX := -1;

    if Y = 0 then
      TransparentColor := Row[0];

    for X := 0 to Bmp.Width - 1 do
    begin
      if (Row[X].rgbtRed = TransparentColor.rgbtRed) and (Row[X].rgbtGreen = TransparentColor.rgbtGreen) and (Row[X].rgbtBlue = TransparentColor.rgbtBlue) then
      begin
        if StartX = -1 then
          StartX := X;
      end
      else
      begin
        if StartX > -1 then
        begin
          Excl := CreateRectRGN(StartX, Y, X + 1, Y + 1);
          try
            CombineRGN(Result, Result, Excl, RGN_DIFF);
            StartX := -1;
          finally
            DeleteObject(Excl);
          end;
        end;
      end;
    end;

    if StartX > -1 then
    begin
      Excl := CreateRectRGN(StartX, Y, Bmp.Width, Y + 1);
      try
        CombineRGN(Result, Result, Excl, RGN_DIFF);
      finally
        DeleteObject(Excl);
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.Region;
begin
  if not FEZShape.Empty then
  begin
    if (GetWindowRgn(Hw, FRegion) <> 0) then
      DeleteObject(FRegion);
    FRegion := CreateRegion(FEZShape);
    SetWindowRGN(Hw, FRegion, True);
  end
  else if (FEZShape.Empty) or (FRegion <> 0) then
  begin
    DeleteObject(FRegion);
    SetWindowRGN(Hw, 0, True);
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetOffsetX(Value: Integer);
begin
  FX0 := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetOffsety(Value: Integer);
begin
  FY0 := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetGradType(Value: TGradType);
begin
  FGradType := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetTexture(Value: TBitmap);
begin
  FTexture.Assign(Value);
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetTType(Value: TTextureType);
begin
  OldT := FTType;
  OldS := FShadeShow;
  if (Value = ttBlend) then
    FShadeShow := False
  else
    FShadeShow := OldS;

  FTType := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetSmooth(Value: boolean);
begin
  FSmooth := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetGrad1(Value: TColor);
begin
  FGrad1 := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetBCol(Value: TColor);
begin
  FBCol := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetBlendCol(Value: TColor);
begin
  FBlendCol := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetBlend(Value: byte);
begin
  FBlend := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetGrad2(Value: TColor);
begin
  FGrad2 := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetScale(Value: double);
begin
  if (Value < 0.2) then
    FScale := 0.2
  else if (Value > 5) then
    FScale := 5
  else
    FScale := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetShadeHeight(Value: Integer);
begin
  if (Value > -1) and (Value < 100) then
    FShadeHeight := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetShadeDiffusion(Value: Integer);
begin
  if (Value > -1) and (Value < 10) then
    FShadeDiffusion := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetShadeAngle(Value: Integer);
begin
  if (Value > -1) and (Value < 360) then
    FShadeAngle := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetShadeColor(Value: TColor);
begin
  FShadeColor := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetShape(Value: TBitmap);
begin
  FEZShape.Assign(Value);
  Region;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetSize(Value: boolean);
begin
  FSize := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

// ------------------------------------------------------------------------------
procedure TEZTextureSettings.SetShadeShow(Value: boolean);
begin
  if (FTType = ttBlend) then
    FShadeShow := False
  else
    FShadeShow := Value;
  if Assigned(FPaint) then
    FPaint(Self);
end;

/// /////////////////////////////////////////////////////////////////////////////
constructor TEZTexturePanel.Create(AOwner: TComponent);
var
  J: Integer;
begin
  inherited Create(AOwner);

  TempBmp := TBitmap.Create;
  TempBmp.PixelFormat := pf24Bit;
  Temp := TBitmap.Create;
  Temp.PixelFormat := pf24Bit;

  Shades := TEZShades.Create;
  Shades.Clear;
  J := 0;
  Shades.write(J, SizeOf(Integer));
  Height := 100;
  Width := 228;
  DoubleBuffered := False;
  FBIn := bvNone;
  FBOut := bvRaised;
  FCtl3D := True;
  FCaption := name;
  FColor := clBtnFace;
  FSettings := TEZTextureSettings.Create;
  FSettings.FX0 := Width div 2;
  FSettings.FY0 := Height div 2;
  FSettings.OnChange := DrawChange;
  First := True;
  Count := 0;
  BackCopy := True;
end;

// ------------------------------------------------------------------------------
destructor TEZTexturePanel.Destroy;
begin
  TempBmp.Free;
  Temp.Free;
  Shades.Free;
  FSettings.Free;
  inherited Destroy;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.WMEraseBkgnd(var message: TWMEraseBkgnd);
begin
  message.Result := LRESULT(False);
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// -------
  procedure DoEvent;
  begin
    if FSettings.FSize and (X >= Width - 8) and (Y >= Height - 8) then
    begin
      IsSizeing := True;
      Pos := Point(X, Y);
    end;
    if Assigned(FMouseD) then
      FMouseD(Self, Button, Shift, X, Y);
  end;

// --------
const
  SC_DragMove = $F012;
begin
  if FSettings.FMove and not(FSettings.FSize and (X >= Width - 8) and (Y >= Height - 8)) then
  begin
    ReleaseCapture;
    Perform(WM_SysCommand, SC_DragMove, 0);
    DoEvent;
    DrawChange(Self);
    UpdateOffspring;
    if Assigned(FMouseU) then
      FMouseU(Self, Button, Shift, X, Y);
  end
  else
    DoEvent;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FSettings.FSize and IsSizeing then
  begin
    if (Width + X - Pos.X > 0) then
      Width := Width + X - Pos.X;
    if (Height + Y - Pos.Y > 0) then
      Height := Height + Y - Pos.Y;
    IsSizeing := False;
    TWinControl(Parent).Refresh;
    DrawChange(Self);
  end;
  if Assigned(FMouseU) then
    FMouseU(Self, Button, Shift, X, Y);
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.WriteData;
var
  ASize: Longint;
begin
  ASize := Shades.Size;
  Stream.write(ASize, SizeOf(ASize));
  if ASize > 0 then
    Stream.write((Shades as TEZShades).Memory^, ASize);
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.DefineProperties;
begin
  inherited;
  Filer.DefineBinaryProperty('TheData', ReadData, WriteData, True);
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.ReadData;
var
  ASize: Longint;
begin
  Stream.read(ASize, SizeOf(ASize));
  if ASize > 0 then
  begin (Shades as TEZShades)
    .SetSize(ASize);
    Stream.read((Shades as TEZShades).Memory^, ASize);
  end;
end;

// ------------------------------------------------------------------------------
function TEZTexturePanel.GetShades: TEZShades;
begin
  Result := Shades;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.SetShades(Value: TEZShades);
begin
  if (csDesigning in ComponentState) then
  begin
    if (Value <> nil) then (Shades as TEZShades)
      .LoadFromStream(Value)
    else (Shades as TEZShades)
      .SetSize(0);
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.SetColor(Value: TColor);
begin
  FColor := Value;
  UpdateTexture;
  Invalidate;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.SetBevelIn(Value: TPanelBevel);
begin
  FBIn := Value;
  UpdateTexture;
  Invalidate;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.SetBevelOut(Value: TPanelBevel);
begin
  FBOut := Value;
  UpdateTexture;
  Invalidate;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.SetCtl3D(Value: boolean);
begin
  FCtl3D := Value;
  UpdateTexture;
  Invalidate;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.SetCaption(Value: string);
begin
  FCaption := Value;
  UpdateTexture;
  Invalidate;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.Resize;
begin
  if not IsSizeing then
  begin
    UpdateTexture;
    Invalidate;
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.Redraw;
begin
  UpdateTexture;
  Invalidate;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.DrawChange(Sender: TObject);
begin
  // with Owner as TControl do begin Redraw; end;
  if (FSettings.FTType = ttBlend) and (FSettings.OldT <> FSettings.FTType) then
    BackCopy := True
  else
    BackCopy := False;
  FSettings.OldT := FSettings.FTType;
  if not First and not FSettings.BlockRepaint then
  begin
    UpdateTexture;
    Invalidate;
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.Paint;
begin
  Inc(Count);
  if (FSettings.Hw <> Handle) then
  begin
    FSettings.Hw := Handle;
    FSettings.Region;
  end
  else
    FSettings.Hw := Handle;
  if First or UpdateShadeObjects then
  begin
    Invalidate;
    UpdateShadeObjects;
    UpdateTexture;
  end
  else
  begin
    Canvas.Draw(0, 0, Temp);
  end;
  First := False;
end;

// ------------------------------------------------------------------------------
function TEZTexturePanel.UpdateShadeObjects: boolean;
// ------
  function Shadable(S: string): boolean;
  begin
    if (S = 'TMenuItem') or (S = 'TMenu') or (S = 'TMainMenu') or (S = 'TSplitter') or (S = 'TBevel') then
      Result := False
    else
      Result := True;
  end;
// ------
  function UseInherit(Comp: TComponent; ClassN: string): string;
  begin
    Result := 'None';
    if Shadable(ClassN) then
    begin
      if Comp.InheritsFrom(TShape) then
        Result := 'Shape'
      else if Comp.InheritsFrom(TLabel) then
        Result := 'Label'
      else if Comp.InheritsFrom(TControl) then
        Result := 'Window';
      if (Comp.ClassName = 'TEZTexturePanel') then
        Result := 'EZTex';
    end;
  end;
// ------
  procedure NoBlendeShade(TS: TShadeObjects);
  var
    I: Integer;
    TT: TTextureType;
    C: TComponent;
  begin
    for I := 0 to Length(TS) - 1 do
    begin
      C := Owner.FindComponent(TS[I].name);
      if Assigned(C) and (C.ClassName = 'TEZTexturePanel') then
      begin
        with Owner.FindComponent(TS[I].name) as TEZTexturePanel do
          TT := Settings.TextureType;
        if (TT = ttBlend) then
          TS[I].Act := saNone;
      end;
    end;
  end;
// ------
  function CheckShadeAction(CompName: string; CurrAct: TShadeAction): TShadeAction;
  var
    C: TComponent;
    S: string;
  begin
    C := Owner.FindComponent(CompName);
    if Assigned(C) then
    begin
      S := C.ClassName;
      if (CurrAct = saWindow) or (CurrAct = saNone) then
        Result := CurrAct
      else
        Result := saNone;
      if (S = 'TEZTexturePanel') then
        if (CurrAct <> saText) then
          Result := CurrAct
        else
          Result := saNone;
      if (S = 'TShape') then
        if (CurrAct <> saText) then
          Result := CurrAct
        else
          Result := saNone;
      if (S = 'TLabel') then
        if (CurrAct <> saShape) then
          Result := CurrAct
        else
          Result := saNone;
    end
    else
    begin
      Result := saNone;
    end;
  end;

// ------
var
  I, J: Integer;
  S1, S2, S3: string;
  TS: TShadeObjects;
begin
  Result := False;
  LoadShades(FSettings.EZShadeOb, Shades);
  SetLength(TS, 0);
  for I := 1 to Owner.ComponentCount - 1 do
  begin
    S3 := Owner.Components[I].ClassName;
    if Owner.Components[I].HasParent then
    begin
      S1 := Owner.Components[I].name;
      with Owner.Components[I] as TControl do
        S2 := Parent.name;
      if (S2 = Self.name) then
        with Owner.Components[I] as TControl do
        begin
          J := Length(TS);
          SetLength(TS, J + 1);
          TS[J].name := S1;
          TS[J].Act := saNone;
          if Visible then
            TS[J].Visible := 1
          else
            TS[J].Visible := 0;
          TS[J].L := Left;
          TS[J].T := Top;
          TS[J].W := Width;
          TS[J].H := Height;
          TS[J].Inherit := UseInherit(Owner.Components[I], S3);
        end;
    end;
  end;

  if (Length(FSettings.EZShadeOb) <> Length(TS)) then
    Result := True;

  for I := 0 to Length(FSettings.EZShadeOb) - 1 do
    for J := 0 to Length(TS) - 1 do
      if (FSettings.EZShadeOb[I].name = TS[J].name) then
      begin
        TS[J].Act := CheckShadeAction(FSettings.EZShadeOb[I].name, FSettings.EZShadeOb[I].Act);
        if (FSettings.EZShadeOb[I].Act <> saNone) then
        begin
          if (TS[J].Visible <> FSettings.EZShadeOb[I].Visible) then
            Result := True;
          if (TS[J].L <> FSettings.EZShadeOb[I].L) then
            Result := True;
          if (TS[J].T <> FSettings.EZShadeOb[I].T) then
            Result := True;
          if (TS[J].W <> FSettings.EZShadeOb[I].W) then
            Result := True;
          if (TS[J].H <> FSettings.EZShadeOb[I].H) then
            Result := True;
        end;
      end;
  NoBlendeShade(TS);
  SaveShades(Shades, TS);
  LoadShades(FSettings.EZShadeOb, Shades);
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.BackToBmp;
var
  BlendBmp: TBitmap;
  DC: hDC;
  W, H: Integer;
  Hw: HWND;
  Vis: boolean;
begin
  if not(csDesigning in ComponentState) or BackCopy then
  begin
    BackCopy := False;
    W := Width;
    H := Height;

    BlendBmp := TBitmap.Create;
    BlendBmp.PixelFormat := pf24Bit;
    BlendBmp.Width := W;
    BlendBmp.Height := H;
    BlendBmp.Canvas.Brush.Style := bsSolid;
    BlendBmp.Canvas.Brush.Color := clBlack;
    BlendBmp.Canvas.FillRect(Rect(0, 0, Width, Height));

    Hw := TWinControl(Parent).Handle;
    SetActiveWindow(Hw);
    GetDC(Hw);
    Vis := Visible;
    Visible := False;
    TWinControl(Parent).Refresh;
    DC := GetDC(Handle);
    BitBlt(BlendBmp.Canvas.Handle, 0, 0, W, H, DC, 0, 0, SrcCopy);
    ReleaseDC(Handle, DC);
    Visible := Vis;
    BlendIt(FSettings.FBlendCol, 255 - FSettings.FBlend, BlendBmp);
    BlendBmp.Free;
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.BlendIt(Col: TColor; Alpha: byte; BlendBmp: TBitmap);
var
  Bl, Tm: pRGBArray;
  X, Y: Integer;
begin
  Temp.Width := Width;
  Temp.Height := Height;
  for Y := 0 to Height - 1 do
  begin
    Bl := BlendBmp.Scanline[Y];
    Tm := Temp.Scanline[Y];
    for X := 0 to Width - 1 do
    begin
      Tm[X].rgbtRed := (Alpha * Bl[X].rgbtRed + (255 - Alpha) * GetRValue(Col)) div 255;
      Tm[X].rgbtGreen := (Alpha * Bl[X].rgbtGreen + (255 - Alpha) * GetGValue(Col)) div 255;
      Tm[X].rgbtBlue := (Alpha * Bl[X].rgbtBlue + (255 - Alpha) * GetBValue(Col)) div 255;
    end
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.DrawBorder;
var
  Re1, Re2: TRect;
  Col1, Col2, Col3, Col4: TColor;
  W: Integer;
begin
  Canvas.Pen.Style := psSolid;
  W := BevelWidth;
  Col1 := FColor;
  Col2 := FColor;
  Col3 := FColor;
  Col4 := FColor;
  case BevelOuter of
    bvNone:
      case BevelInner of
        bvNone:
          begin
            Col1 := FColor;
            Col2 := FColor;
            Col3 := FColor;
            Col4 := FColor
          end;
        bvLowered:
          begin
            Col1 := clBtnShadow;
            Col2 := clBtnHighlight;
            Col3 := FColor;
            Col4 := FColor
          end;
        bvSpace:
          begin
            Col1 := clBtnHighlight;
            Col2 := clBtnShadow;
            Col3 := FColor;
            Col4 := FColor
          end;
        bvRaised:
          begin
            Col1 := clBtnHighlight;
            Col2 := clBtnShadow;
            Col3 := FColor;
            Col4 := FColor
          end;
      end;
    bvLowered:
      case BevelInner of
        bvNone:
          begin
            Col1 := clBtnShadow;
            Col2 := clBtnHighlight;
            Col3 := FColor;
            Col4 := FColor
          end;
        bvLowered:
          begin
            Col1 := clBtnShadow;
            Col2 := clBtnHighlight;
            Col3 := clBtnShadow;
            Col4 := clBtnHighlight
          end;
        bvSpace:
          begin
            Col1 := clBtnShadow;
            Col2 := clBtnHighlight;
            Col3 := clBtnHighlight;
            Col4 := clBtnShadow
          end;
        bvRaised:
          begin
            Col1 := clBtnShadow;
            Col2 := clBtnHighlight;
            Col3 := clBtnHighlight;
            Col4 := clBtnShadow
          end;
      end;
    bvSpace:
      case BevelInner of
        bvNone:
          begin
            Col1 := clBtnHighlight;
            Col2 := clBtnShadow;
            Col3 := FColor;
            Col4 := FColor
          end;
        bvLowered:
          begin
            Col1 := clBtnHighlight;
            Col2 := clBtnShadow;
            Col3 := clBtnShadow;
            Col4 := clBtnHighlight
          end;
        bvSpace:
          begin
            Col1 := clBtnHighlight;
            Col2 := clBtnShadow;
            Col3 := clBtnHighlight;
            Col4 := clBtnShadow
          end;
        bvRaised:
          begin
            Col1 := clBtnHighlight;
            Col2 := clBtnShadow;
            Col3 := clBtnHighlight;
            Col4 := clBtnShadow
          end;
      end;
    bvRaised:
      case BevelInner of
        bvNone:
          begin
            Col1 := clBtnHighlight;
            Col2 := clBtnShadow;
            Col3 := FColor;
            Col4 := FColor
          end;
        bvLowered:
          begin
            Col1 := clBtnHighlight;
            Col2 := clBtnShadow;
            Col3 := clBtnShadow;
            Col4 := clBtnHighlight
          end;
        bvSpace:
          begin
            Col1 := clBtnHighlight;
            Col2 := clBtnShadow;
            Col3 := clBtnHighlight;
            Col4 := clBtnShadow
          end;
        bvRaised:
          begin
            Col1 := clBtnHighlight;
            Col2 := clBtnShadow;
            Col3 := clBtnHighlight;
            Col4 := clBtnShadow
          end;
      end;
  end;

  Re1 := Rect(0, 0, Width, Height);
  Re2 := Rect(W, W, Width - W, Height - W);
  if (BevelOuter <> bvNone) and (Col3 = FColor) and (Col4 = FColor) then
    Frame3D(Temp.Canvas, Re1, Col1, Col2, W);
  if (Col3 <> FColor) and (Col4 <> FColor) then
    Frame3D(Temp.Canvas, Re2, Col3, Col4, W);
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.UpdateOffspring;
var
  I: Integer;
  TT: TTextureType;
begin
  if (FSettings.FTType = ttBlend) then
  begin
    for I := 1 to Owner.ComponentCount - 1 do
    begin
      if (Owner.FindComponent(Owner.Components[I].name).ClassName = 'TEZTexturePanel') then
      begin
        with Owner.FindComponent(Owner.Components[I].name) as TEZTexturePanel do
          TT := Settings.TextureType;
        if (TT = ttBlend) then
          with Owner.FindComponent(Owner.Components[I].name) as TEZTexturePanel do
            UpdateTexture;
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.UpdateTexture;
var
  X, Y: Integer;
  Re1: TRect;
begin
  if (Count > 0) then
  begin
    Temp.Width := Width;
    Temp.Height := Height;
    FSettings.Region;
    if (not Assigned(FSettings.FTexture) or (FSettings.FTexture.Empty)) and ((FSettings.FTType <> ttGradient) and (FSettings.FTType <> ttBlend)) then
      FSettings.FTType := ttNone;
    if (FSettings.FTexture.PixelFormat <> pf24Bit) then
      FSettings.FTexture.PixelFormat := pf24Bit;
    if (TempBmp.PixelFormat <> pf24Bit) then
      TempBmp.PixelFormat := pf24Bit;
    if (FSettings.FTType <> ttNone) and (FSettings.FTType <> ttGradient) then
    begin
      if (FSettings.FScale = 1) then
      begin
        TempBmp.Width := FSettings.FTexture.Width;
        TempBmp.Height := FSettings.FTexture.Height;
        TempBmp.Canvas.StretchDraw(Rect(0, 0, FSettings.FTexture.Width, FSettings.FTexture.Height), FSettings.FTexture);
      end
      else if FSettings.FSmooth then
      begin
        TempBmp.Width := trunc(FSettings.FTexture.Width * FSettings.FScale);
        TempBmp.Height := trunc(FSettings.FTexture.Height * FSettings.FScale);
        SmoothResize(FSettings.FTexture, TempBmp);
      end
      else
      begin
        TempBmp.Width := trunc(FSettings.FTexture.Width * FSettings.FScale);
        TempBmp.Height := trunc(FSettings.FTexture.Height * FSettings.FScale);
        TempBmp.Canvas.StretchDraw(Rect(0, 0, TempBmp.Width, TempBmp.Height), FSettings.FTexture);
      end;
    end;
    case FSettings.FTType of
      ttTile:
        begin
          if (TempBmp.Width > 0) and (TempBmp.Height > 0) then
            for X := 0 to 1 + Width div TempBmp.Width do
              for Y := 0 to 1 + Height div TempBmp.Height do
                Temp.Canvas.Draw(X * TempBmp.Width, Y * TempBmp.Height, TempBmp);
        end;
      ttCenter:
        begin
          Temp.Canvas.Brush.Style := bsSolid;
          Temp.Canvas.Brush.Color := FColor;
          Temp.Canvas.FillRect(Rect(0, 0, Width, Height));
          Temp.Canvas.Draw((Width - TempBmp.Width) div 2, (Height - TempBmp.Height) div 2, TempBmp);
        end;
      ttStrech:
        begin
          if FSettings.FSmooth then
          begin
            SmoothResize(FSettings.FTexture, Temp);
          end
          else
            Temp.Canvas.StretchDraw(Rect(0, 0, Width, Height), FSettings.FTexture);
        end;
      ttNone:
        begin
          Temp.Canvas.Brush.Style := bsSolid;
          Temp.Canvas.Brush.Color := FColor;
          Temp.Canvas.FillRect(Rect(0, 0, Width, Height));
        end;
      ttGradient:
        DrawGrad(FSettings.FGrad1, FSettings.FGrad2);
      ttBlend:
        if not(csDesigning in ComponentState) then
          BackToBmp
        else
        begin
          Temp.Canvas.Brush.Style := bsSolid;
          Temp.Canvas.Brush.Color := clBtnFace;
          Temp.Canvas.Rectangle(0, 0, Width, Height);
          Temp.Canvas.Brush.Style := bsDiagCross;
          Temp.Canvas.Brush.Color := FSettings.FBlendCol;
          Temp.Canvas.Rectangle(0, 0, Width, Height);
        end;
    end;

    Temp.Canvas.Brush.Style := bsClear;
    Temp.Canvas.Font := Font;
    Temp.Canvas.TextOut((Width - Canvas.TextWidth(Caption)) div 2, (Height - Canvas.TextHeight(Caption)) div 2, Caption);

    Temp.Canvas.Pen.Color := FSettings.FBCol;
    if Ctl3D then
      DrawBorder
    else
      Temp.Canvas.Rectangle(Rect(0, 0, Width, Height));
    if FSettings.FSize then
    begin
      Temp.Canvas.Brush.Style := bsSolid;
      Temp.Canvas.Brush.Color := clBtnFace;
      Temp.Canvas.FillRect(Rect(Width - 8, Height - 8, Width, Height));
      Temp.Canvas.Pen.Color := FSettings.FBCol;
      Re1 := Rect(Width - 8, Height - 8, Width, Height);
      if Ctl3D then
        Frame3D(Temp.Canvas, Re1, clBtnHighlight, clBtnShadow, 1)
      else
        Temp.Canvas.Rectangle(Re1);
    end;

    if FSettings.FShadeShow then
      DrawShade;
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.DrawGrad(Col1, Col2: TColor);
// ---------
  function SquareDis(X, Y: double): double;
  // ---------
    function SideDis(Xa, Ya, Xb, Yb, Xc, Yc: double): double;
    var
      L: double;
    begin
      Result := 0;
      L := SQRT(((Xb - Xa) * (Xb - Xa) + (Yb - Ya) * (Yb - Ya)));
      if (L <> 0) then
        Result := ((Ya - Yc) * (Xb - Xa) - (Xa - Xc) * (Yb - Ya)) / (L * L);
    end;
  // ---------
    function SaveAbsDiv(C, D: double): double;
    begin
      if (abs(D) < 1E-3) then
        Result := 0
      else
        Result := abs(C / D);
    end;

  // ---------
  var
    D1, D2, D3, D4: double;
  begin
    D1 := SideDis(0, 0, FSettings.FX0, FSettings.FY0, X, Y);
    D2 := SideDis(Width, 0, FSettings.FX0, FSettings.FY0, X, Y);
    D3 := SideDis(0, Height, FSettings.FX0, FSettings.FY0, X, Y);
    D4 := SideDis(Width, Height, FSettings.FX0, FSettings.FY0, X, Y);

    Result := 0;
    if (D1 >= 0) and (D2 <= 0) then
      Result := 1 - SaveAbsDiv(Y, (FSettings.FY0))
    else if (D1 <= 0) and (D3 >= 0) then
      Result := 1 - SaveAbsDiv(X, (FSettings.FX0))
    else if (D2 >= 0) and (D4 <= 0) then
      Result := SaveAbsDiv((FSettings.FX0 - X), (Width - FSettings.FX0))
    else if (D3 <= 0) and (D4 >= 0) then
      Result := SaveAbsDiv((FSettings.FY0 - Y), (Height - FSettings.FY0));
  end;
// ---------
  function MaxDis: Integer;
  var
    D: array [1 .. 4] of double;
    I: Integer;
  begin
    D[1] := SQRT(sqr(0 - FSettings.FX0) + sqr(0 - FSettings.FY0));
    D[2] := SQRT(sqr(Width - FSettings.FX0) + sqr(0 - FSettings.FY0));
    D[3] := SQRT(sqr(0 - FSettings.FX0) + sqr(Height - FSettings.FY0));
    D[4] := SQRT(sqr(Width - FSettings.FX0) + sqr(Height - FSettings.FY0));
    Result := 0;
    for I := 1 to 4 do
      if (Round(D[I]) > Result) then
        Result := Round(D[I]);
  end;

// ---------
var
  X, Y, XD, YD, M: Integer;
  Row: pRGBArray;
  Col: TRGBTriple;
begin
  if (Col1 < 0) then
    Col1 := Norm24BitCol(Col1);
  if (Col2 < 0) then
    Col2 := Norm24BitCol(Col2);
  XD := FSettings.FX0;
  if (XD < Width div 2) then
    XD := Width - FSettings.FX0;
  YD := FSettings.FY0;
  if (YD < Height div 2) then
    YD := Height - FSettings.FY0;
  M := MaxDis;
  for Y := 0 to Temp.Height - 1 do
  begin
    Row := Temp.Scanline[Y];
    for X := 0 to Temp.Width - 1 do
    begin
      case FSettings.FGradType of
        gtLeftRight:
          IntensityToGradient(Col, X / Width, Col1, Col2);
        gtTopBottom:
          IntensityToGradient(Col, Y / Height, Col1, Col2);
        gtCircle:
          IntensityToGradient(Col, SQRT(sqr(X - FSettings.FX0) + sqr(Y - FSettings.FY0)) / M, Col2, Col1);
        gtHorizontal:
          IntensityToGradient(Col, abs(Y - FSettings.FY0) / (YD), Col2, Col1);
        gtVertical:
          IntensityToGradient(Col, abs(X - FSettings.FX0) / (XD), Col2, Col1);
        gtSquare:
          IntensityToGradient(Col, SquareDis(X, Y), Col2, Col1);
      end;
      Row[X] := Col;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.DrawShade;
var
  I: Integer;
begin
  for I := 0 to Length(FSettings.EZShadeOb) - 1 do
  begin
    if (FSettings.EZShadeOb[I].Visible = 1) then
      case FSettings.EZShadeOb[I].Act of
        saWindow:
          DrawWindowShade(Owner.FindComponent(FSettings.EZShadeOb[I].name));
        saText:
          DrawTextShade(Owner.FindComponent(FSettings.EZShadeOb[I].name));
        saShape:
          if (Owner.FindComponent(FSettings.EZShadeOb[I].name).ClassName = 'TEZTexturePanel') then
            DrawShapeOwn(Owner.FindComponent(FSettings.EZShadeOb[I].name))
          else
            DrawShapeShade(Owner.FindComponent(FSettings.EZShadeOb[I].name));
      end;
  end;
end;

// ------------------------------------------------------------------------------
function TEZTexturePanel.SetShade(CompName: string; Shaded: boolean): boolean;
var
  I, C: Integer;
  a: TShadeAction;
  // -------
  function GetAct(index: Integer): TShadeAction;
  var
    S: string;
  begin
    S := FSettings.EZShadeOb[index].Inherit;
    if (FSettings.EZShadeOb[C].Visible = 1) then
    begin
      if (S = 'Window') then
        Result := saWindow
      else if (S = 'Shape') then
        Result := saShape
      else if (S = 'Label') then
        Result := saText
      else if (S = 'EZTex') then
        Result := saShape
      else
        Result := saNone;
    end
    else
      Result := saNone;
  end;

// -------
begin
  LoadShades(FSettings.EZShadeOb, Shades);
  C := -1;
  for I := 0 to Length(FSettings.EZShadeOb) - 1 do
    if FSettings.EZShadeOb[I].name = CompName then
      C := I;

  if (C > -1) then
  begin
    a := GetAct(C);
    if (a = saNone) then
      Result := False
    else
      Result := True;
    if Shaded then
      FSettings.EZShadeOb[C].Act := a
    else
      FSettings.EZShadeOb[C].Act := saNone;
  end
  else
    Result := False;

  SaveShades(Shades, FSettings.EZShadeOb);
  Redraw;
end;

// ------------------------------------------------------------------------------
function TEZTexturePanel.IsShaded(CompName: string): boolean;
var
  I, C: Integer;
begin
  C := -1;
  Result := False;

  for I := 0 to Length(FSettings.EZShadeOb) - 1 do
    if FSettings.EZShadeOb[I].name = CompName then
      C := I;
  if (C > -1) then
    if (FSettings.EZShadeOb[C].Act <> saNone) then
      Result := True;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.ApplyShade(L, T: Integer; Bmp: TBitmap);
var
  X, Y, Br: Integer;
  Pos: TPoint;
  Row, RowT: pRGBArray;
  Am, An: double;
  Col: TColor;
begin
  Br := Round(FSettings.FShadeDiffusion) * 2;
  if (FSettings.FShadeHeight > 0) then
    Pos := NewPos(0, FSettings.FShadeHeight, 0, 0, FSettings.FShadeAngle)
  else
    Pos := Point(0, 0);
  Col := Norm24BitCol(FSettings.FShadeColor);
  for Y := 0 to Bmp.Height - 1 do
    if (Y + T + Pos.Y - Br >= 0) and (Y + T + Pos.Y - Br < Height) then
    begin
      Row := Bmp.Scanline[Y];
      RowT := Self.Temp.Scanline[Y + T + Pos.Y - Br];
      for X := 0 to Bmp.Width - 1 do
        if (X + L + Pos.X - Br >= 0) and (X + L + Pos.X - Br < Width) then
        begin
          Am := Row[X].rgbtRed / 255;
          An := 1 - Row[X].rgbtRed / 255;
          with RowT[X + L + Pos.X - Br] do
          begin
            rgbtRed := trunc(rgbtRed * Am) + trunc(GetRValue(Col) * An);
            rgbtGreen := trunc(rgbtGreen * Am) + trunc(GetGValue(Col) * An);
            rgbtBlue := trunc(rgbtBlue * Am) + trunc(GetBValue(Col) * An);
          end;
        end;
    end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.DrawWindowShade(Obj: TObject);
var
  Br, X2, Y2, X1, Y1: Integer;
  L, T, W, H: Integer;
  Pos: TPoint;
  Bmp1, Bmp2: TBitmap;
begin
  if (Obj <> nil) then
  begin
    with Obj as TControl do
    begin
      L := Left;
      T := Top;
      W := Width;
      H := Height;
    end;

    Br := Round(FSettings.FShadeDiffusion) * 2;
    if (FSettings.FShadeHeight > 0) then
      Pos := NewPos(0, FSettings.FShadeHeight, 0, 0, FSettings.FShadeAngle)
    else
      Pos := Point(0, 0);
    X1 := Br;
    Y1 := Br;
    X2 := W + Br;
    Y2 := H + Br;

    Bmp1 := TBitmap.Create;
    Bmp1.PixelFormat := pf24Bit;
    Bmp1.Canvas.Brush.Style := bsSolid;
    Bmp1.Width := W + 2 * Br;
    Bmp1.Height := H + 2 * Br;
    Bmp1.Canvas.Brush.Color := clWhite;
    Bmp1.Canvas.FillRect(Rect(0, 0, W + Br * 2, H + Br * 2));
    Bmp1.Canvas.Brush.Color := clBlack;
    Bmp1.Canvas.FillRect(Rect(Br, Br, W + Br, H + Br));

    Temp.Canvas.Brush.Color := Norm24BitCol(FSettings.FShadeColor);
    Temp.Canvas.FillRect(Rect(L + Pos.X + Br, T + Pos.Y + Br, L + W + Pos.X - Br, T + H + Pos.Y - Br));

    Bmp2 := TBitmap.Create;
    Bmp2.PixelFormat := pf24Bit;
    Bmp2.Canvas.Brush.Style := bsSolid;

    // Left
    if (X1 + Br >= 0) and (X1 - Br <= Width) then
    begin
      Bmp2.Width := Br * 2;
      Bmp2.Height := H + Br * 2;
      Bmp2.Canvas.FillRect(Rect(0, 0, Br * 2, H + Br * 2));
      Bmp2.Canvas.CopyRect(Rect(0, 0, Br * 2, H + Br * 2), Bmp1.Canvas, Rect(X1 - Br, Y1 - Br, X1 + Br, Y2 + Br));
      GBlur(Bmp2, FSettings.FShadeDiffusion);
      ApplyShade(X1 - Br + L, Y1 - Br + T, Bmp2);
    end;

    // Right
    if (X2 + Br >= 0) and (X2 - Br + L <= Width) then
    begin
      Bmp2.Width := Br * 2;
      Bmp2.Height := H + Br * 2;
      Bmp2.Canvas.FillRect(Rect(0, 0, Br * 2, H + Br * 2));
      Bmp2.Canvas.CopyRect(Rect(0, 0, Br * 2, H + Br * 2), Bmp1.Canvas, Rect(X2 - Br, Y1 - Br, X2 + Br, Y2 + Br));
      GBlur(Bmp2, FSettings.FShadeDiffusion);
      ApplyShade(X2 - Br + L, Y1 - Br + T, Bmp2);
    end;

    // Top
    if (Y1 + Br >= 0) and (Y1 - Br <= Height) then
    begin
      Bmp2.Width := W - 2 * Br;
      Bmp2.Height := Br * 2;
      Bmp2.Canvas.FillRect(Rect(0, 0, W, Br * 2));
      Bmp2.Canvas.CopyRect(Rect(0, 0, W, Br * 2), Bmp1.Canvas, Rect(X1 + Br, Y1 - Br, X2 - Br, Y1 + Br));
      GBlur(Bmp2, FSettings.FShadeDiffusion);
      ApplyShade(X1 + Br + L, Y1 - Br + T, Bmp2);
    end;

    // Bottom
    if (Y2 + Br >= 0) and (Y2 - Br + T <= Height) then
    begin
      Bmp2.Width := W - 2 * Br;
      Bmp2.Height := Br * 2;
      Bmp2.Canvas.FillRect(Rect(0, 0, W, Br * 2));
      Bmp2.Canvas.CopyRect(Rect(0, 0, W, Br * 2), Bmp1.Canvas, Rect(X1 + Br, Y2 - Br, X2 - Br, Y2 + Br));
      GBlur(Bmp2, FSettings.FShadeDiffusion);
      ApplyShade(X1 + Br + L, Y2 - Br + T, Bmp2);
    end;

    Bmp1.Free;
    Bmp2.Free;
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.DrawTextShade(Obj: TObject);
var
  Br: Integer;
  Bmp1: TBitmap;
  L, T, W, H: Integer;
  Text: string;
  Wind: boolean;
begin
  if (Obj <> nil) then
  begin
    with Obj as TLabel do
    begin
      L := Left;
      T := Top;
      W := Width;
      H := Height;
      Text := Caption;
      Wind := Transparent;
    end;

    if not Wind then
      DrawWindowShade(Obj)
    else
    begin
      Br := Round(FSettings.FShadeDiffusion) * 2;

      Bmp1 := TBitmap.Create;
      Bmp1.PixelFormat := pf24Bit;
      Bmp1.Canvas.Brush.Style := bsSolid;
      Bmp1.Width := W + 2 * Br;
      Bmp1.Height := H + 2 * Br;
      Bmp1.Canvas.Brush.Color := clWhite;
      Bmp1.Canvas.FillRect(Rect(0, 0, W + Br * 2, H + Br * 2));
      OutputLabel2Canv(Self, TLabel(Obj), Bmp1.Canvas, Br);
      GBlur(Bmp1, FSettings.FShadeDiffusion);
      ApplyShade(L, T, Bmp1);

      Bmp1.Free;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.DrawShapeShade(Obj: TObject);
var
  Br, cS: Integer;
  Bmp1: TBitmap;
  L, T, W, H: Integer;
  Bru: TBrush;
  P: TPen;
  Sh: TShapeType;
begin
  if (Obj <> nil) then
  begin
    with Obj as TShape do
    begin
      L := Left;
      T := Top;
      W := Width;
      H := Height;
      Bru := Brush;
      P := Pen;
      Sh := Shape;
    end;

    if (Bru.Style = bsSolid) and (Sh = stRectangle) then
    begin
      DrawWindowShade(Obj);
    end
    else
    begin
      Br := Round(FSettings.FShadeDiffusion) * 2;
      Bmp1 := TBitmap.Create;
      Bmp1.PixelFormat := pf24Bit;
      Bmp1.Canvas.Brush.Style := bsSolid;
      Bmp1.Width := W + 2 * Br;
      Bmp1.Height := H + 2 * Br;
      Bmp1.Canvas.Brush.Color := clWhite;
      Bmp1.Canvas.FillRect(Rect(0, 0, W + Br * 2, H + Br * 2));

      Bmp1.Canvas.Pen.Color := clBlack;
      Bmp1.Canvas.Brush.Color := clBlack;
      Bmp1.Canvas.Pen.Style := P.Style;
      Bmp1.Canvas.Pen.Width := P.Width;
      Bmp1.Canvas.Brush.Style := Bru.Style;
      if (W > H) then
        cS := H
      else
        cS := W; ;

      case Sh of
        stCircle:
          Bmp1.Canvas.Ellipse(Bmp1.Width div 2 - cS div 2, Bmp1.Height div 2 - cS div 2, Bmp1.Width div 2 + cS div 2, Bmp1.Height div 2 + cS div 2);
        stEllipse:
          Bmp1.Canvas.Ellipse(Bmp1.Width div 2 - W div 2, Bmp1.Height div 2 - H div 2, Bmp1.Width div 2 + W div 2, Bmp1.Height div 2 + H div 2);
        stRectangle:
          Bmp1.Canvas.Rectangle(Bmp1.Width div 2 - W div 2, Bmp1.Height div 2 - H div 2, Bmp1.Width div 2 + W div 2, Bmp1.Height div 2 + H div 2);
        stSquare:
          Bmp1.Canvas.Rectangle(Bmp1.Width div 2 - cS div 2, Bmp1.Height div 2 - cS div 2, Bmp1.Width div 2 + cS div 2, Bmp1.Height div 2 + cS div 2);
        stRoundSquare:
          Bmp1.Canvas.RoundRect(Bmp1.Width div 2 - cS div 2, Bmp1.Height div 2 - cS div 2, Bmp1.Width div 2 + cS div 2, Bmp1.Height div 2 + cS div 2, 10, 10);
        stRoundRect:
          Bmp1.Canvas.RoundRect(Bmp1.Width div 2 - W div 2, Bmp1.Height div 2 - H div 2, Bmp1.Width div 2 + W div 2, Bmp1.Height div 2 + H div 2, cS div 4,
            cS div 4);
      end;

      GBlur(Bmp1, FSettings.FShadeDiffusion);
      ApplyShade(L, T, Bmp1);
      Bmp1.Free;
    end;
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.DrawShapeOwn(Obj: TObject);
var
  Br, X, Y: Integer;
  Bmp1: TBitmap;
  Row, RowT: pRGBArray;
  Col: TColor;
  L, T, W, H: Integer;
  ShapeBmp: TBitmap;
  B: boolean;
  TT: TTextureType;
  // ----------
  procedure GetVar;
  begin
    with Obj as TEZTexturePanel do
      if FSettings.Shape.Empty or (FSettings.Shape = nil) then
        B := False
      else
        B := True;
    with Obj as TEZTexturePanel do
    begin
      L := Left;
      T := Top;
      W := Width;
      H := Height;
      TT := Settings.TextureType;
      if B then
      begin
        ShapeBmp.PixelFormat := FSettings.Shape.PixelFormat;
        ShapeBmp.Width := FSettings.Shape.Width;
        ShapeBmp.Height := FSettings.Shape.Height;
        ShapeBmp.Canvas.Draw(0, 0, FSettings.Shape);
        B := True;
      end;
    end;
  end;

// ----------
begin
  if (Obj <> nil) then
  begin
    ShapeBmp := TBitmap.Create;
    GetVar;
    if not(TT = ttBlend) then
    begin
      if not B then
      begin
        DrawWindowShade(Obj);
      end
      else
      begin
        Br := Round(FSettings.FShadeDiffusion) * 2;
        Bmp1 := TBitmap.Create;
        Bmp1.PixelFormat := pf24Bit;
        Bmp1.Canvas.Brush.Style := bsSolid;
        if (ShapeBmp.Width > W) then
          Bmp1.Width := W + 2 * Br
        else
          Bmp1.Width := ShapeBmp.Width + 2 * Br;
        if (ShapeBmp.Height > H) then
          Bmp1.Height := H + 2 * Br
        else
          Bmp1.Height := ShapeBmp.Height + 2 * Br;

        Col := ShapeBmp.Canvas.Pixels[0, 0];
        for Y := 0 to Bmp1.Height - 1 - Br * 2 do
        begin
          Row := ShapeBmp.Scanline[Y];
          RowT := Bmp1.Scanline[Y + Br];
          for X := 0 to Bmp1.Width - 1 - Br * 2 do
          begin
            if (Row[X].rgbtBlue = GetBValue(Col)) and (Row[X].rgbtGreen = GetGValue(Col)) and (Row[X].rgbtRed = GetRValue(Col)) then
            begin
              RowT[X + Br].rgbtBlue := 255;
              RowT[X + Br].rgbtGreen := 255;
              RowT[X + Br].rgbtRed := 255;
            end
            else
            begin
              RowT[X + Br].rgbtBlue := 0;
              RowT[X + Br].rgbtGreen := 0;
              RowT[X + Br].rgbtRed := 0;
            end;
          end;
        end;
        GBlur(Bmp1, FSettings.FShadeDiffusion);
        ApplyShade(L, T, Bmp1);
        Bmp1.Free;
      end;
    end;
    ShapeBmp.Free;
  end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.SaveSettings(Stream: TMemoryStream);
var
  C: array [0 .. 2] of char;
  I, J: Integer;
begin
  C := 'ET3';
  // ET1
  Stream.write(C, SizeOf(C));
  Stream.write(FColor, SizeOf(TColor));
  Stream.write(FBIn, SizeOf(byte));
  Stream.write(FBOut, SizeOf(byte));
  StringToStream(FCaption, Stream);
  Stream.write(FSettings.FBCol, SizeOf(TColor));
  Stream.write(FCtl3D, SizeOf(byte));
  Stream.write(FSettings.FMove, SizeOf(byte));
  Stream.write(FSettings.FSize, SizeOf(byte));
  Stream.write(FSettings.FGrad1, SizeOf(TColor));
  Stream.write(FSettings.FGrad2, SizeOf(TColor));
  BitmapToStream(FSettings.FTexture, Stream);
  Stream.write(FSettings.FTType, SizeOf(byte));
  Stream.write(FSettings.FScale, SizeOf(double));
  Stream.write(FSettings.FSmooth, SizeOf(byte));
  Stream.write(FSettings.FShadeAngle, SizeOf(Integer));
  Stream.write(FSettings.FShadeColor, SizeOf(TColor));
  Stream.write(FSettings.FShadeDiffusion, SizeOf(double));
  Stream.write(FSettings.FShadeHeight, SizeOf(Integer));
  Stream.write(FSettings.FShadeShow, SizeOf(byte));
  BitmapToStream(FSettings.FEZShape, Stream);
  // ET2
  Stream.write(FSettings.FBlendCol, SizeOf(TColor));
  Stream.write(FSettings.FBlend, SizeOf(byte));
  Stream.write(FSettings.FGradType, SizeOf(byte));
  Stream.write(FSettings.FX0, SizeOf(Integer));
  Stream.write(FSettings.FY0, SizeOf(Integer));
  // ET3
  J := Length(FSettings.EZShadeOb);
  Stream.write(J, SizeOf(Integer));
  if J > 0 then
    for I := 0 to J - 1 do
    begin
      StringToStream(FSettings.EZShadeOb[I].name, Stream);
      Stream.write(FSettings.EZShadeOb[I].Act, SizeOf(TShadeAction));
      Stream.write(FSettings.EZShadeOb[I].Visible, SizeOf(byte));
      Stream.write(FSettings.EZShadeOb[I].L, SizeOf(Integer));
      Stream.write(FSettings.EZShadeOb[I].T, SizeOf(Integer));
      Stream.write(FSettings.EZShadeOb[I].W, SizeOf(Integer));
      Stream.write(FSettings.EZShadeOb[I].H, SizeOf(Integer));
      StringToStream(FSettings.EZShadeOb[I].Inherit, Stream);
    end;
end;

// ------------------------------------------------------------------------------
procedure TEZTexturePanel.LoadSettings(Stream: TMemoryStream);
var
  C: array [0 .. 2] of char;
  I, J: Integer;
begin
  Stream.read(C, SizeOf(C));
  if (C = 'ET2') or (C = 'ET1') or (C = 'ET3') then
  begin
    Stream.read(FColor, SizeOf(TColor));
    Stream.read(FBIn, SizeOf(byte));
    Stream.read(FBOut, SizeOf(byte));
    FCaption := StreamToString(Stream);
    Stream.read(FSettings.FBCol, SizeOf(TColor));
    Stream.read(FCtl3D, SizeOf(byte));
    Stream.read(FSettings.FMove, SizeOf(byte));
    Stream.read(FSettings.FSize, SizeOf(byte));
    Stream.read(FSettings.FGrad1, SizeOf(TColor));
    Stream.read(FSettings.FGrad2, SizeOf(TColor));
    StreamToBitmap(FSettings.FTexture, Stream);
    Stream.read(FSettings.FTType, SizeOf(byte));
    Stream.read(FSettings.FScale, SizeOf(double));
    Stream.read(FSettings.FSmooth, SizeOf(byte));
    Stream.read(FSettings.FShadeAngle, SizeOf(Integer));
    Stream.read(FSettings.FShadeColor, SizeOf(TColor));
    Stream.read(FSettings.FShadeDiffusion, SizeOf(double));
    Stream.read(FSettings.FShadeHeight, SizeOf(Integer));
    Stream.read(FSettings.FShadeShow, SizeOf(byte));
    StreamToBitmap(FSettings.FEZShape, Stream);
    if (C = 'ET2') or (C = 'ET3') then
    begin
      Stream.read(FSettings.FBlendCol, SizeOf(TColor));
      Stream.read(FSettings.FBlend, SizeOf(byte));
      Stream.read(FSettings.FGradType, SizeOf(byte));
      Stream.read(FSettings.FX0, SizeOf(Integer));
      Stream.read(FSettings.FY0, SizeOf(Integer));
    end;
    if (C = 'ET3') then
    begin
      Stream.read(J, SizeOf(Integer));
      SetLength(FSettings.EZShadeOb, J);
      if J > 0 then
        for I := 0 to J - 1 do
        begin
          FSettings.EZShadeOb[I].name := StreamToString(Stream);
          Stream.read(FSettings.EZShadeOb[I].Act, SizeOf(TShadeAction));
          Stream.read(FSettings.EZShadeOb[I].Visible, SizeOf(byte));
          Stream.read(FSettings.EZShadeOb[I].L, SizeOf(Integer));
          Stream.read(FSettings.EZShadeOb[I].T, SizeOf(Integer));
          Stream.read(FSettings.EZShadeOb[I].W, SizeOf(Integer));
          Stream.read(FSettings.EZShadeOb[I].H, SizeOf(Integer));
          FSettings.EZShadeOb[I].Inherit := StreamToString(Stream);
        end;
    end;
    Redraw;
  end;
end;

// ------------------------------------------------------------------------------
end.
