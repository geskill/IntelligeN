unit uCAPTCHA;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, jpeg, GIFImg,
  // Dev Express
  dxGDIPlusClasses,
  // API
  uApiSettings,
  // Utils
  uImageUtils;

type
  TCAPTCHA = class(TForm)
    iCAPTCHA: TImage;
    eCAPTCHA: TEdit;
    bAccept: TButton;
    bCancel: TButton;
    mCAPTCHA: TMemo;
    tTimer: TTimer;
    procedure tTimerTimer(Sender: TObject);
  private
    function GetCAPTCHA: string;
  protected
    procedure Display;
  public
    constructor Create(AOwner: TComponent; AMaxWaitMs: Cardinal = INFINITE);

    procedure DisplayCAPTCHAImage(ACAPTCHAMemoryStream: TMemoryStream);
    procedure DisplayCAPTCHAText(ACAPTCHAText: string);

    function ShowModal: Integer; override;

    property CAPTCHA: string read GetCAPTCHA;
  end;

implementation

{$R *.dfm}
{ TCAPTCHA }

procedure TCAPTCHA.tTimerTimer(Sender: TObject);
begin
  tTimer.Enabled := False;
  Close;
end;

function TCAPTCHA.GetCAPTCHA: string;
begin
  Result := eCAPTCHA.Text;
end;

procedure TCAPTCHA.Display;
var
  LCAPTCHAPosition: TCAPTCHAPosition;
begin
  // TODO: Move this into constructor via parameter. (thread-safety)
  LCAPTCHAPosition := SettingsManager.Settings.CAPTCHAPosition;

  if not(LCAPTCHAPosition = cpCentered) then
  begin
    // if (ACAPTCHAPosition = cpBottomLeft) or (ACAPTCHAPosition = cpTopLeft) then
    Left := 0;
    if (LCAPTCHAPosition = cpBottomRight) or (LCAPTCHAPosition = cpBottomRight) then
      Left := Screen.WorkAreaWidth - Width;
    // if (ACAPTCHAPosition = cpTopLeft) or (ACAPTCHAPosition = cpTopRight) then
    Top := 0;
    if (LCAPTCHAPosition = cpBottomLeft) or (LCAPTCHAPosition = cpBottomRight) then
      Top := Screen.WorkAreaHeight - Height;
  end
  else
  begin
    Position := poScreenCenter;
  end;
end;

constructor TCAPTCHA.Create(AOwner: TComponent; AMaxWaitMs: Cardinal);
begin
  inherited Create(AOwner);

  tTimer.Enabled := False;
  tTimer.Interval := AMaxWaitMs;
end;

procedure TCAPTCHA.DisplayCAPTCHAImage(ACAPTCHAMemoryStream: TMemoryStream);
var
  _img_jpeg: TJPEGImage;
  _img_png: TdxPNGImage;
  _img_gif: TGIFImage;
  _img_tif: TWicImage;
begin
  if IsJPEG(ACAPTCHAMemoryStream) then
  begin
    _img_jpeg := TJPEGImage.Create;
    try
      _img_jpeg.LoadFromStream(ACAPTCHAMemoryStream);
      iCAPTCHA.Picture.Bitmap.Assign(_img_jpeg);
    finally
      _img_jpeg.Free;
    end;
  end
  else if IsPNG(ACAPTCHAMemoryStream) then
  begin
    _img_png := TdxPNGImage.Create;
    try
      _img_png.LoadFromStream(ACAPTCHAMemoryStream);
      iCAPTCHA.Picture.Assign(_img_png);
    finally
      _img_png.Free;
    end;
  end
  else if IsGIF(ACAPTCHAMemoryStream) then
  begin
    _img_gif := TGIFImage.Create;
    try
      _img_gif.LoadFromStream(ACAPTCHAMemoryStream);
      _img_gif.Animate := True;
      iCAPTCHA.Picture.Graphic := _img_gif;
    finally
      _img_gif.Free;
    end;
  end
  else if IsTIF(ACAPTCHAMemoryStream) then
  begin
    _img_tif := TWicImage.Create;
    try
      _img_tif.LoadFromStream(ACAPTCHAMemoryStream);
      iCAPTCHA.Picture.Graphic := _img_tif;
    finally
      _img_tif.Free;
    end;
  end
  else if IsBMP(ACAPTCHAMemoryStream) then
    iCAPTCHA.Picture.Bitmap.LoadFromStream(ACAPTCHAMemoryStream)
  else
  begin
    with TStringStream.Create do
      try
        LoadFromStream(ACAPTCHAMemoryStream);
        DisplayCAPTCHAText(DataString);
      finally
        Free;
      end;

    Exit;
  end;
  ClientWidth := iCAPTCHA.Width + 16;
  ClientHeight := iCAPTCHA.Height + 66 + 12;
  iCAPTCHA.Visible := True;

  Display;
end;

procedure TCAPTCHA.DisplayCAPTCHAText(ACAPTCHAText: string);
begin
  mCAPTCHA.Text := ACAPTCHAText;
  mCAPTCHA.Visible := True;

  Display;
end;

function TCAPTCHA.ShowModal: Integer;
begin
  tTimer.Enabled := True;
  Result := inherited ShowModal;
end;

end.
