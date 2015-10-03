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
  private
    function GetCAPTCHA: string;
  protected
    procedure Display;
  public
    procedure DisplayCAPTCHAImage(ACAPTCHAMemoryStream: TMemoryStream);
    procedure DisplayCAPTCHAText(ACAPTCHAText: string);

    property CAPTCHA: string read GetCAPTCHA;
  end;

implementation

{$R *.dfm}
{ TCAPTCHA }

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

procedure TCAPTCHA.DisplayCAPTCHAImage(ACAPTCHAMemoryStream: TMemoryStream);
var
  _img_jpeg: TJPEGImage;
  _img_png: TdxPNGImage;
  _img_gif: TGIFImage;
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

end.
