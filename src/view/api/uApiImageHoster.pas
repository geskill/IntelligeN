unit uApiImageHoster;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes,
  // OmniThreadLibrary
  OtlTaskControl, OtlThreadPool,
  // Common
  uAppInterface,
  // Api
  uApiCAPTCHA, uApiPlugins, uApiSettings;

const
  MSG_SLEEP = 3;

type
  TImageHosterUploadThread = class(TOmniWorker)
  private
    FCAPTCHAResult: Boolean;
    FCAPTCHAImageUrl, FCAPTCHAName, FCAPTCHAText, FCAPTCHACookies: WideString;
    procedure CAPTCHAInputThreaded;
    function CAPTCHAInputSynchronizer(const AImageUrl, AName: WideString; out AText: WideString; var ACookies: WideString): Boolean; stdcall;
  protected
    FPictureMirror: IPictureMirror;
    FErrorMsg: string;
    FImageHosterCollectionItem: TImageHosterCollectionItem;
    procedure InternalErrorHandler(AErrorMsg: string);
  public
    constructor Create(const APictureMirror: IPictureMirror);
    procedure SleepTask(var msg: TMessage); message MSG_SLEEP;
    destructor Destroy; override;
  end;

  TImageHosterLocalUploadThread = class(TImageHosterUploadThread)
  protected
    FLocalPath: string;
  public
    constructor Create(const APictureMirror: IPictureMirror; const ALocalPath: WideString);
    function Initialize: Boolean; override;
  end;

  TImageHosterRemoteUploadThread = class(TImageHosterUploadThread)
  public
    function Initialize: Boolean; override;
  end;

  TImageHosterManager = class(TInterfacedObject, IImageHosterManager)
  public
    procedure AddLocalUploadJob(const APictureMirror: IPictureMirror; const ALocalPath: WideString);
    procedure AddRemoteUploadJob(const APictureMirror: IPictureMirror);
  end;

implementation

{ TImageHosterUploadThread }

procedure TImageHosterUploadThread.CAPTCHAInputThreaded;
begin
  FCAPTCHAResult := TCAPTCHAClass.CAPTCHAInput(FCAPTCHAImageUrl, FCAPTCHAName, FCAPTCHAText, FCAPTCHACookies);
end;

function TImageHosterUploadThread.CAPTCHAInputSynchronizer(const AImageUrl, AName: WideString; out AText: WideString; var ACookies: WideString): Boolean;
begin
  FCAPTCHAImageUrl := AImageUrl;
  FCAPTCHAName := AName;
  FCAPTCHACookies := ACookies;
  task.Invoke(CAPTCHAInputThreaded);
  AText := FCAPTCHAText;
  ACookies := FCAPTCHACookies;
  Result := FCAPTCHAResult;
end;

procedure TImageHosterUploadThread.InternalErrorHandler(AErrorMsg: string);
begin
  FErrorMsg := AErrorMsg;
end;

constructor TImageHosterUploadThread.Create(const APictureMirror: IPictureMirror);
begin
  inherited Create;

  FPictureMirror := APictureMirror;
  with SettingsManager.Settings.Plugins do
    FImageHosterCollectionItem := TImageHosterCollectionItem(FindPlugInCollectionItemFromCollection(FPictureMirror.Name, ImageHoster));
  FErrorMsg := '';
end;

procedure TImageHosterUploadThread.SleepTask(var msg: TMessage);
begin
  task.SetTimer(0);
  Sleep(1000);
  task.Terminate;
end;

destructor TImageHosterUploadThread.Destroy;
begin
  FPictureMirror := nil;
  inherited Destroy;
end;

{ TImageHosterLocalUploadThread }

constructor TImageHosterLocalUploadThread.Create(const APictureMirror: IPictureMirror; const ALocalPath: WideString);
begin
  inherited Create(APictureMirror);

  FLocalPath := ALocalPath;
end;

function TImageHosterLocalUploadThread.Initialize: Boolean;
var
  LUploadURL: string;
begin
  LUploadURL := TApiPlugin.ImageHosterLocalUpload(FImageHosterCollectionItem, FLocalPath, InternalErrorHandler, CAPTCHAInputSynchronizer);

  with FPictureMirror do
  begin
    if not SameStr('', LUploadURL) then
    begin
      Value := LUploadURL;
      if SameStr('', OriginalValue) then
        Picture.Value := LUploadURL;
    end;
    ErrorMsg := FErrorMsg;
  end;

  task.SetTimer(1, 1, MSG_SLEEP);

  Result := True;
end;

{ TImageHosterRemoteUploadThread }

function TImageHosterRemoteUploadThread.Initialize: Boolean;
begin
  with FPictureMirror do
  begin
    Value := TApiPlugin.ImageHosterRemoteUpload(FImageHosterCollectionItem, OriginalValue, InternalErrorHandler, CAPTCHAInputSynchronizer);
    ErrorMsg := FErrorMsg;
  end;

  task.SetTimer(1, 1, MSG_SLEEP);

  Result := True;
end;

{ TImageHosterManager }

procedure TImageHosterManager.AddLocalUploadJob(const APictureMirror: IPictureMirror; const ALocalPath: WideString);
begin
  CreateTask(TImageHosterLocalUploadThread.Create(APictureMirror, ALocalPath)).Unobserved.Run;
end;

procedure TImageHosterManager.AddRemoteUploadJob(const APictureMirror: IPictureMirror);
begin
  CreateTask(TImageHosterRemoteUploadThread.Create(APictureMirror)).Unobserved.Run;
end;

end.
