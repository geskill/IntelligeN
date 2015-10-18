unit uApiThreadManager;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Messages, Dialogs,
  // Spring Framework
  Spring.Collections.Lists,
  // OmniThreadLibrary
  OtlComm, OtlCommon, OtlEventMonitor, OtlTaskControl,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface;

const
  WM_MSG_FINISH = WM_USER + 2;

type
  TThreadWorkData = class
  protected
    FTabSheetController: ITabSheetController;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property TabSheetController: ITabSheetController read FTabSheetController write FTabSheetController;
  end;

  TThreadWorker<T: TThreadWorkData, constructor> = class(TOmniWorker)
  protected
    FData: T;
    procedure DoFinish(var msg: TMessage); message WM_MSG_FINISH;
    procedure DefaultErrorHandler(AErrorMsg: string);
    function InBlackList: Boolean;
    function Initialize: Boolean; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; virtual; abstract;
    procedure Finish;

    property Data: T read FData;
  end;

  TThreadManager<T: TThreadWorkData, constructor> = class(TInterfacedObject, IThreadManager)
  protected
    FOmniEM: TOmniEventMonitor;
    FInList, FBlackList: TList<T>;
    function InAnyList(const ATabSheetController: ITabSheetController): Boolean;
    function InList(const AJobWorkData: T): Boolean;
    function InBlackList(const AJobWorkData: T): Boolean;

    procedure OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); virtual;

    function CanAddJob(const AJobWorkData: T): Boolean;
    function AddJob(const AJobWorkData: T): Boolean;
    function CanRemoveJob(const AJobWorkData: T): Boolean;
    function RemoveJob(const AJobWorkData: T): Boolean;
  public
    constructor Create();

    function InUse(const ATabSheetController: ITabSheetController): WordBool;
    function IsIdle: WordBool;

    destructor Destroy; override;
  end;

const
  MSG_TASK_BLACKLISTED = 0;
  MSG_CONTINUE_TASK = 1;
  MSG_QUIT_TASK = 2;
  MSG_TASK_QUIT = 3;

implementation

{ TThreadWorkData<T> }

constructor TThreadWorkData.Create;
begin
  inherited Create;
end;

destructor TThreadWorkData.Destroy;
begin
  FTabSheetController := nil;

  inherited Destroy;
end;

{ TThreadWorker<T> }

procedure TThreadWorker<T>.DoFinish(var msg: TMessage);
begin
  task.ClearTimer(1);
  Sleep(1000);
  task.Terminate;
end;

procedure TThreadWorker<T>.DefaultErrorHandler(AErrorMsg: string);
begin
  task.Invoke(
    { } procedure
    { } begin
    { . } MessageDlg(AErrorMsg, mtError, [mbOK], 0);
    { } end);
end;

function TThreadWorker<T>.InBlackList: Boolean;
var
  LOmniValue: TOmniValue;
  LMessage: TOmniMessage;
  LFound: Boolean;
begin
  Result := False;

  LOmniValue := TOmniValue.CastFrom<T>(Data);
  task.Comm.Send(MSG_TASK_BLACKLISTED, [task.UniqueID, LOmniValue.AsObject]);

  repeat
    task.Comm.ReceiveWait(LMessage, INFINITE);

    LFound := (((LMessage.MsgID = MSG_CONTINUE_TASK) or (LMessage.MsgID = MSG_QUIT_TASK)) and (LMessage.MsgData[0].AsInt64 = task.UniqueID));

    if not LFound then
      task.Comm.OtherEndpoint.Send(LMessage);

  until LFound;

  if (LMessage.MsgID = MSG_QUIT_TASK) then
  begin
    Finish;
    Result := True;
  end;
end;

function TThreadWorker<T>.Initialize: Boolean;
begin
  Result := inherited Initialize;
  if not Result or InBlackList then
    Exit;
  Result := True;
end;

constructor TThreadWorker<T>.Create;
begin
  inherited Create;
  FData := T.Create();
end;

destructor TThreadWorker<T>.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TThreadWorker<T>.Finish;
var
  LOmniValue: TOmniValue;
begin
  LOmniValue := TOmniValue.CastFrom<T>(Data);
  task.Comm.Send(MSG_TASK_QUIT, [task.UniqueID, LOmniValue.AsObject]);
  task.SetTimer(1, 1, WM_MSG_FINISH);
end;

{ TThreadManager<T> }

function TThreadManager<T>.InAnyList(const ATabSheetController: ITabSheetController): Boolean;
var
  LListIndex: Integer;
begin
  Result := False;
  for LListIndex := 0 to FInList.Count - 1 do
  begin
    if ATabSheetController = FInList[LListIndex].TabSheetController then
    begin
      Result := True;
      Break;
    end;
  end;
  for LListIndex := 0 to FBlackList.Count - 1 do
  begin
    if ATabSheetController = FBlackList[LListIndex].TabSheetController then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TThreadManager<T>.InList(const AJobWorkData: T): Boolean;
begin
  Result := FInList.Contains(AJobWorkData);
end;

function TThreadManager<T>.InBlackList(const AJobWorkData: T): Boolean;
begin
  Result := FBlackList.Contains(AJobWorkData);
end;

procedure TThreadManager<T>.OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
var
  LJobWorkData: T;
begin
  LJobWorkData := T(msg.MsgData[1].AsObject);

  case msg.MsgID of
    MSG_TASK_BLACKLISTED:
      begin
        if InBlackList(LJobWorkData) then
        begin
          FBlackList.Remove(LJobWorkData);
          task.Comm.Send(MSG_QUIT_TASK, [msg.MsgData[0].AsInt64]);
        end
        else
        begin
          task.Comm.Send(MSG_CONTINUE_TASK, [msg.MsgData[0].AsInt64]);
        end;
      end;
    MSG_TASK_QUIT:
      begin
        FInList.Remove(LJobWorkData);
        if FBlackList.Contains(LJobWorkData) then
          FBlackList.Remove(LJobWorkData);
      end;
  end;
end;

function TThreadManager<T>.CanAddJob(const AJobWorkData: T): Boolean;
begin
  Result := not InList(AJobWorkData);
end;

function TThreadManager<T>.AddJob(const AJobWorkData: T): Boolean;
begin
  if CanAddJob(AJobWorkData) then
  begin
    FInList.Add(AJobWorkData);
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function TThreadManager<T>.CanRemoveJob(const AJobWorkData: T): Boolean;
begin
  Result := InList(AJobWorkData);
end;

function TThreadManager<T>.RemoveJob(const AJobWorkData: T): Boolean;
begin
  if CanRemoveJob(AJobWorkData) then
  begin
    if not InBlackList(AJobWorkData) then
    begin
      FBlackList.Add(AJobWorkData);
    end;
  end
  else
  begin
    Result := False;
  end;
end;

constructor TThreadManager<T>.Create;
begin
  inherited Create;
  FOmniEM := TOmniEventMonitor.Create(nil);
  with FOmniEM do
  begin
    OnTaskMessage := OmniEMTaskMessage;
  end;

  FInList := TList<T>.Create;
  FBlackList := TList<T>.Create;
end;

function TThreadManager<T>.InUse(const ATabSheetController: ITabSheetController): WordBool;
begin
  Result := InAnyList(ATabSheetController);
end;

function TThreadManager<T>.IsIdle: WordBool;
begin
  Result := (FInList.Count = 0);
end;

destructor TThreadManager<T>.Destroy;
begin
  FInList.Free;
  FBlackList.Free;
  FOmniEM.Free;
  inherited Destroy;
end;

end.
