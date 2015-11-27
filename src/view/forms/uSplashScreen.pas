unit uSplashScreen;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, jpeg,
  // Dev Express
  dxGDIPlusClasses, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxLabel,
  // Common
  uBaseConst,
  // Api
  uApiMain;

type
  TSplashScreen = class(TForm)
    iSplash: TImage;
    iLogo: TImage;
    cxLCopyright: TcxLabel;
    cxLProgrammName: TcxLabel;
    cxLProgrammBuild: TcxLabel;
    cxLPleaseDonate: TcxLabel;    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  SplashScreen: TSplashScreen;

implementation

{$R *.dfm}

procedure TSplashScreen.FormCreate(Sender: TObject);
var
  LRandomID: Integer;
  LResourceStream: TResourceStream;
  LJPEGImage: TJPEGImage;

  LPNGImage: TdxPNGImage;
begin
  LRandomID := Random(3);
  LResourceStream := TResourceStream.Create(hInstance, 'JpgImage' + IntToStr(LRandomID), RT_RCDATA);
  try
    LJPEGImage := TJPEGImage.Create;
    try
      LJPEGImage.LoadFromStream(LResourceStream);
      iSplash.Picture.Graphic := LJPEGImage;
    finally
      LJPEGImage.Free;
    end;
  finally
    LResourceStream.Free;
  end;

  LPNGImage := TdxPNGImage.Create;
  try
    LPNGImage.LoadFromResource(hInstance, 'logo', RT_RCDATA);
    iLogo.Picture.Graphic := LPNGImage;
  finally
    LPNGImage.Free;
  end;

  cxLProgrammName.Caption := ProgrammName;
  cxLProgrammBuild.Caption := 'BUILD ' + IntToStr(MINOR_VERSION);

  ShowWindow(Application.Handle, SW_HIDE);
  SetWindowLong(Application.Handle, GWL_EXSTYLE, GetWindowLong(Application.Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);
  ShowWindow(Application.Handle, SW_SHOW);
end;

procedure TSplashScreen.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  SplashScreen := nil;
end;

end.
