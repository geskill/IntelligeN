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
    constructor Create(const APictureMirror: IPictureMirror);
    procedure Execute; override;
  end;

  TImageHosterManager = class(TThreadManager<TImageHosterData>, IImageHosterManager)
  public
    procedure AddLocalUploadJob(const APictureMirror: IPictureMirror; const ALocalPath: WideString);
    procedure AddRemoteUploadJob(const APictureMirror: IPictureMirror);
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
  LUploadURL: string;
begin
  with TApiThreadedPlugin.Create(task, DefaultErrorHandler) do
    try
      LUploadURL := ImageHosterLocalUpload(Data.ImageHosterCollectionItem, FLocalPath);
      if not SameStr('', LUploadURL) then
      begin
        task.Invoke(
          { } procedure
          { } begin
          { . } Data.PictureMirror.Value := LUploadURL;
          { . } if SameStr('', Data.PictureMirror.OriginalValue) then
          { . } begin
          { ... } Data.PictureMirror.Picture.Value := LUploadURL;
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

constructor TImageHosterRemoteUploadThread.Create(const APictureMirror: IPictureMirror);
begin
  inherited Create(APictureMirror);

  FRemotePath := APictureMirror.OriginalValue;
end;

procedure TImageHosterRemoteUploadThread.Execute;
var
  LUploadURL: string;
begin
  with TApiThreadedPlugin.Create(task, DefaultErrorHandler) do
    try
      LUploadURL := ImageHosterRemoteUpload(Data.ImageHosterCollectionItem, FRemotePath);
      if not SameStr('', LUploadURL) then
      begin
        task.Invoke(
          { } procedure
          { } begin
          { . } Data.PictureMirror.Value := LUploadURL;

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

procedure TImageHosterManager.AddRemoteUploadJob(const APictureMirror: IPictureMirror);
var
  LImageHosterUploadThread: TImageHosterUploadThread;
begin
  LImageHosterUploadThread := TImageHosterRemoteUploadThread.Create(APictureMirror);
  AddJob(LImageHosterUploadThread.Data);
  CreateTask(LImageHosterUploadThread).MonitorWith(FOmniEM).Run(@TImageHosterRemoteUploadThread.Execute);
end;

end.