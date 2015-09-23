unit uApiWebImageloader;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Graphics, jpeg, GIFImg, ActiveX, AxCtrls,
  // DevExpress
  dxGDIPlusClasses,
  // Interface
  uAppInterface, uConst,
  // API
  uApiCAPTCHA, uApiHTTP, uApiPlugins, uApiSettings,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Utils
  uImageUtils;

type
  TWebImageDownloader = class(TThread)
  private
    FPicture: IPicture;
    FImageLink: string;
    FImageIndex: Integer;

    FGraphic: TGraphic;

    procedure WriteToImage();
  protected
    procedure Execute; override;
  public
    constructor Create(APicture: IPicture); virtual;
    property ImageLink: string read FImageLink write FImageLink;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
  end;

  TWebImageUploader = class(TThread)
  private
    FPicture: IPicture;
    FUploadImage: string;
    FImageHoster: TImageHosterCollectionItem;
    FCAPTCHAResult: Boolean;
    FCAPTCHAImageUrl, FCAPTCHAName, FCAPTCHAText, FCAPTCHACookies: WideString;
    procedure CAPTCHAInputThreaded;
    function CAPTCHAInputSynchronizer(const AImageUrl, AName: WideString; out AText: WideString; var ACookies: WideString): Boolean; stdcall;
  public
    constructor Create(APicture: IPicture; AImageHoster: TImageHosterCollectionItem); virtual;

    procedure Execute; override;
  end;

implementation

{$REGION 'TWebImageDownloader'}

procedure TWebImageDownloader.WriteToImage;
begin
  //
end;

procedure TWebImageDownloader.Execute;
var
  HTTPManager: IHTTPManager;
  HTTPOptions: IHTTPOptions;
  RequestID: Double;
  HTTPProcess: IHTTPProcess;
  OleStream: TOleStream;
  Dummy: Int64;
  MemoryStream: TMemoryStream;
  PictureInfo: TPictureInfo;
  valid: Boolean;



  function GetBitmap(const Graphic: TGraphic): TBitmap;
  begin
    Result := TBitmap.Create;
    if Assigned(Graphic) and not Graphic.Empty then
    begin
      Result.SetSize(Graphic.Width, Graphic.Height);
      with Result.Canvas do
        try
          Draw(0, 0, Graphic);
        except

        end;
    end;
  end;

  function convert(Picture: TGraphic): Variant;
  var
    StringStream: TStringStream;
  begin
    StringStream := TStringStream.Create('');
    try
      with GetBitmap(Picture) do
        try
          SaveToStream(StringStream);
        finally
          Free;
        end;
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  end;

begin
  with PictureInfo do
  begin
    Picture := '';
    Size := 0;
    Width := 0;
    Height := 0;
  end;

  MemoryStream := TMemoryStream.Create;
  try
    OutputDebugString(PChar(ImageLink));

    valid := True;

    HTTPManager := TApiHTTPManager.GetInstance;

    HTTPOptions := THTTPOptions.Create(SettingsManager.Settings.HTTP.GetProxy(psaCrawler));

    HTTPOptions.ConnectTimeout := SettingsManager.Settings.HTTP.ConnectTimeout;
    HTTPOptions.ReadTimeout := SettingsManager.Settings.HTTP.ReadTimeout;

    RequestID := HTTPManager.Get(THTTPRequest.Create(ImageLink), HTTPOptions);

    repeat
      sleep(50);
    until HTTPManager.HasResult(RequestID);

    HTTPProcess := HTTPManager.GetResult(RequestID);

    OleStream := TOleStream.Create(HTTPProcess.HTTPResult.HTTPResponse.ContentStream);
    try
      HTTPProcess.HTTPResult.HTTPResponse.ContentStream.Seek(0, STREAM_SEEK_SET, Dummy);
      OleStream.Seek(0, STREAM_SEEK_SET);
      MemoryStream.CopyFrom(OleStream, OleStream.Size);
    finally
      OleStream.Free;
    end;

    HTTPProcess := nil;

    // HTTPManager.GetResult(RequestID).HTTPResult.HTTPResponse.ContentStream.CopyTo(bytesRead64,bytesWritten64)

    HTTPOptions := nil;
    HTTPManager := nil;

    {
      with TApiHTTP.Create do
      try
      try
      Get(ImageLink, MemoryStream);
      except
      on E: Exception do
      valid := False;
      end;
      finally
      Free;
      end;
      }

    if valid then
    begin
      sleep(100);

      PictureInfo.Size := MemoryStream.Size;

      FGraphic := GetTGraphicType(MemoryStream).Create;
      try
        try
          MemoryStream.Position := 0;

          FGraphic.LoadFromStream(MemoryStream);

          if FGraphic.InheritsFrom(TJPEGImage) then
            with TJPEGImage(FGraphic) do
              DIBNeeded;

          with PictureInfo do
          begin
            Picture := convert(FGraphic);
            Width := FGraphic.Width;
            Height := FGraphic.Height;
          end;

          FPicture.SetValuePicture(ImageIndex, PictureInfo);
        except

        end;
      finally
        FGraphic.Free;
      end;
    end;

  finally
    MemoryStream.Free;
  end;
  PictureInfo.Clear;
end;

constructor TWebImageDownloader.Create(APicture: IPicture);
begin
  inherited Create(True);

  FPicture := APicture;
  FreeOnTerminate := True;
end;
{$ENDREGION}
{ TWebImageUploader }

procedure TWebImageUploader.CAPTCHAInputThreaded;
begin
  FCAPTCHAResult := TCAPTCHAClass.CAPTCHAInput(FCAPTCHAImageUrl, FCAPTCHAName, FCAPTCHAText, FCAPTCHACookies);
end;

function TWebImageUploader.CAPTCHAInputSynchronizer(const AImageUrl, AName: WideString; out AText: WideString; var ACookies: WideString): Boolean;
begin
  FCAPTCHAImageUrl := AImageUrl;
  FCAPTCHAName := AName;
  FCAPTCHACookies := ACookies;
  Synchronize(CAPTCHAInputThreaded);
  AText := FCAPTCHAText;
  ACookies := FCAPTCHACookies;
  Result := FCAPTCHAResult;
end;

constructor TWebImageUploader.Create;
begin
  inherited Create(True);

  FPicture := APicture;
  FUploadImage := FPicture.Value;
  FImageHoster := AImageHoster;

  FreeOnTerminate := True;
end;

procedure TWebImageUploader.Execute;
var
  _ImageValue: string;
begin
  try
    _ImageValue := TApiPlugin.UploadToImageHoster(FImageHoster, FUploadImage, CAPTCHAInputSynchronizer);

    Synchronize( procedure begin FPicture.Value := _ImageValue end);
  except

  end;

  FPicture := nil;
end;

end.
