unit uApiImageHosterManager;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes,
  // OmniThreadLibrary
  OtlTaskControl,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiThreadManager, uApiPlugins, uApiSettings;

type
  TImageHosterData = class(TThreadWorkData)
  protected
    FPictureMirror: IPictureMirror;
    FImageHosterCollectionItem: TImageHosterCollectionItem;
  public
    constructor Create; override;
    destructor Destroy; override;

    property PictureMirror: IPictureMirror read FPictureMirror write FPictureMirror;
    property ImageHosterCollectionItem: TImageHosterCollectionItem read FImageHosterCollectionItem write FImageHosterCollectionItem;
  end;

  TImageHosterUploadThread = class(TThreadWorker<TImageHosterData>)
  protected
    FPictureURL: string;
    procedure DefaultErrorHandler(AErrorMsg: string); override;
  public
    constructor Create(const APictureMirror: IPictureMirror); reintroduce;
    destructor Destroy; override;
  end;

  TImageHosterLocalUploadThread = class(TImageHosterUploadThread)
  protected
    FLocalPath: string;
  public
    constructor Create(const APictureMirror: IPictureMirror; const ALocalPath: WideString);

    procedure Execute; override;
  end;

  TImageHosterRemoteUploadThread = class(TImageHosterUploadThread)
  protected
    FRemotePath: string;
  public
    constructor Create(const APictureMirror: IPictureMirror; const ARemoteUrl: WideString);
    procedure Execute; override;
  end;

  TImageHosterManager = class(TThreadManager<TImageHosterData>, IImageHosterManager)
  public
    procedure AddLocalUploadJob(const APictureMirror: IPictureMirror; const ALocalPath: WideString);
    procedure AddRemoteUploadJob(const APictureMirror: IPictureMirror; const ARemoteUrl: WideString);
    procedure RemoveUploadJob(const APictureMirror: IPictureMirror);
  end;

implementation

{ TImageHosterData }

constructor TImageHosterData.Create;
begin
  inherited Create;
end;

destructor TImageHosterData.Destroy;
begin
  FPictureMirror := nil;
  FImageHosterCollectionItem := nil;
  inherited Destroy;
end;

{ TImageHosterUploadThread }

procedure TImageHosterUploadThread.DefaultErrorHandler(AErrorMsg: string);
begin
  task.Invoke(
    { } procedure
    { } begin
    { . } Data.PictureMirror.ErrorMsg := AErrorMsg;
    { } end);
end;

constructor TImageHosterUploadThread.Create(const APictureMirror: IPictureMirror);
begin
  inherited Create;

  Data.TabSheetController := APictureMirror.Picture.ControlController.TabSheetController;

  Data.PictureMirror := APictureMirror;

  with SettingsManager.Settings.Plugins do
    Data.ImageHosterCollectionItem := TImageHosterCollectionItem(FindPlugInCollectionItemFromCollection(APictureMirror.Name, ImageHoster));
end;

destructor TImageHosterUploadThread.Destroy;
begin
  inherited Destroy;
end;

{ TImageHosterLocalUploadThread }

constructor TImageHosterLocalUploadThread.Create(const APictureMirror: IPictureMirror; const ALocalPath: WideString);
begin
  inherited Create(APictureMirror);

  FLocalPath := ALocalPath;
end;

procedure TImageHosterLocalUploadThread.Execute;
var
  LUploadedURL: WideString;
begin
  with TApiThreadedPlugin.Create(task, DefaultErrorHandler) do
    try
      if ImageHosterLocalUpload(Data.ImageHosterCollectionItem, FLocalPath, LUploadedURL) then
      begin
        task.Invoke(
          { } procedure
          { } begin
          { . } Data.PictureMirror.Value := LUploadedURL;
          { . } if SameStr('', Data.PictureMirror.OriginalValue) then
          { . } begin
          { ... } Data.PictureMirror.Picture.Value := LUploadedURL;
          { . } end;
          { . } Finish;
          { } end);
      end
      else
      begin
        Finish;
      end;
    finally
      Free;
    end;
end;

{ TImageHosterRemoteUploadThread }

constructor TImageHosterRemoteUploadThread.Create(const APictureMirror: IPictureMirror; const ARemoteUrl: WideString);
begin
  inherited Create(APictureMirror);

  FRemotePath := ARemoteUrl;
end;

procedure TImageHosterRemoteUploadThread.Execute;
var
  LUploadedURL: WideString;
begin
  with TApiThreadedPlugin.Create(task, DefaultErrorHandler) do
    try
      if ImageHosterRemoteUpload(Data.ImageHosterCollectionItem, FRemotePath, LUploadedURL) then
      begin
        task.Invoke(
          { } procedure
          { } begin
          { . } Data.PictureMirror.Value := LUploadedURL;

          { . } Finish;
          { } end);
      end
      else
      begin
        Finish;
      end;
    finally
      Free;
    end;
end;

{ TImageHosterManager }

procedure TImageHosterManager.AddLocalUploadJob(const APictureMirror: IPictureMirror; const ALocalPath: WideString);
var
  LImageHosterUploadThread: TImageHosterUploadThread;
begin
  LImageHosterUploadThread := TImageHosterLocalUploadThread.Create(APictureMirror, ALocalPath);
  AddJob(LImageHosterUploadThread.Data);
  CreateTask(LImageHosterUploadThread).MonitorWith(FOmniEM).Run(@TImageHosterLocalUploadThread.Execute);
end;

procedure TImageHosterManager.AddRemoteUploadJob(const APictureMirror: IPictureMirror; const ARemoteUrl: WideString);
var
  LImageHosterUploadThread: TImageHosterUploadThread;
begin
  LImageHosterUploadThread := TImageHosterRemoteUploadThread.Create(APictureMirror, ARemoteUrl);
  AddJob(LImageHosterUploadThread.Data);
  CreateTask(LImageHosterUploadThread).MonitorWith(FOmniEM).Run(@TImageHosterRemoteUploadThread.Execute);
end;

procedure TImageHosterManager.RemoveUploadJob(const APictureMirror: IPictureMirror);
var
  LListIndex: Integer;
begin
  for LListIndex := 0 to FInList.Count - 1 do
  begin
    if (APictureMirror = FInList[LListIndex].PictureMirror) then
    begin
      RemoveJob(FInList[LListIndex]);
    end;
  end;
end;

end.
