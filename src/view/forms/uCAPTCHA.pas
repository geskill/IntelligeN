unit uCAPTCHA;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, jpeg, GIFImg,
  // Dev Express
  dxGDIPlusClasses,
  // API
  uApiConst, uApiHTTP, uApiSettings,
  // Utils
  uImageUtils,
  // Plugin
  uPlugInConst;

type
  TCAPTCHA = class(TForm)
    iCAPTCHA: TImage;
    eCAPTCHA: TEdit;
    bAccept: TButton;
    bCancel: TButton;
    mCAPTCHA: TMemo;
  private
    function GetCAPTCHA: string;
  public
    constructor Create(ACAPTCHAType: TCAPTCHAType; ACAPTCHA: string; var ACookies: string); reintroduce;
    destructor Destroy; override;
    property CAPTCHA: string read GetCAPTCHA;
  end;

implementation

{$R *.dfm}
{ TCAPTCHA }

function TCAPTCHA.GetCAPTCHA: string;
begin
  Result := eCAPTCHA.Text;
end;

constructor TCAPTCHA.Create(ACAPTCHAType: TCAPTCHAType; ACAPTCHA: string; var ACookies: string);
var
  _CAPTCHAPosition: TCAPTCHAPosition;
  _MemoryStream: TMemoryStream;

  _img_jpeg: TJPEGImage;
  _img_png: TdxPNGImage;
  _img_gif: TGIFImage;
begin
  inherited Create(nil);

  if (ACAPTCHAType = ctImage) then
  begin
    with TApiHTTP.Create(psaCMS) do
      try
        _MemoryStream := TMemoryStream.Create;
        try
          try
            Request.Referer := ACAPTCHA; // i.e. for DLE
            CookieList := ACookies;
            Get(ACAPTCHA, _MemoryStream);
            ACookies := CookieList;
          except
            on E: Exception do
            begin

            end;
          end;

          _MemoryStream.Position := 0;

          if IsJPEG(_MemoryStream) then
          begin
            _img_jpeg := TJPEGImage.Create;
            try
              _img_jpeg.LoadFromStream(_MemoryStream);
              iCAPTCHA.Picture.Bitmap.Assign(_img_jpeg);
            finally
              _img_jpeg.Free;
            end;
          end
          else if IsPNG(_MemoryStream) then
          begin
            _img_png := TdxPNGImage.Create;
            try
              _img_png.LoadFromStream(_MemoryStream);
              iCAPTCHA.Picture.Assign(_img_png);
            finally
              _img_png.Free;
            end;
          end
          else if IsGIF(_MemoryStream) then
          begin
            _img_gif := TGIFImage.Create;
            try
              _img_gif.LoadFromStream(_MemoryStream);
              _img_gif.Animate := True;
              iCAPTCHA.Picture.Graphic := _img_gif;
            finally
              _img_gif.Free;
            end;
          end
          else if IsBMP(_MemoryStream) then
            iCAPTCHA.Picture.Bitmap.LoadFromStream(_MemoryStream)
          else
          begin
            with TStringStream.Create do
              try
                LoadFromStream(_MemoryStream);
                mCAPTCHA.Text := DataString;
              finally
                Free;
              end;
            mCAPTCHA.Visible := True;
          end;
        finally
          _MemoryStream.Free;
        end;
      finally
        Free;
      end;
    if not mCAPTCHA.Visible then
    begin
      ClientWidth := iCAPTCHA.Width + 16;
      ClientHeight := iCAPTCHA.Height + 66 + 12;
      iCAPTCHA.Visible := True;
    end;
  end
  else if (ACAPTCHAType = ctText) then
  begin
    mCAPTCHA.Text := ACAPTCHA;
    mCAPTCHA.Visible := True;
  end;

  _CAPTCHAPosition := SettingsManager.Settings.CAPTCHAPosition;

  if not(_CAPTCHAPosition = cpCentered) then
  begin
    // if (ACAPTCHAPosition = cpBottomLeft) or (ACAPTCHAPosition = cpTopLeft) then
    Left := 0;
    if (_CAPTCHAPosition = cpBottomRight) or (_CAPTCHAPosition = cpBottomRight) then
      Left := Screen.WorkAreaWidth - Width;
    // if (ACAPTCHAPosition = cpTopLeft) or (ACAPTCHAPosition = cpTopRight) then
    Top := 0;
    if (_CAPTCHAPosition = cpBottomLeft) or (_CAPTCHAPosition = cpBottomRight) then
      Top := Screen.WorkAreaHeight - Height;
  end
  else
    Position := poScreenCenter;
end;

destructor TCAPTCHA.Destroy;
begin
  inherited;
end;

end.
