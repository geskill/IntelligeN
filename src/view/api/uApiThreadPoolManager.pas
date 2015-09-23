unit uApiThreadPoolManager;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Messages,
  // OmniThreadLibrary
  OtlComm, OtlEventMonitor, OtlTaskControl, OtlThreadPool;

const
  MSG_SLEEP = WM_USER + 2;

type
  TMyOmniWorker = class(TOmniWorker)
  public
    function CheckforBlacklist(AInterfaceComponent: IInterface): Boolean;
    procedure SleepTask(var msg: TMessage); message MSG_SLEEP;
  end;

  TThreadPoolManager = class(TInterfacedObject)
  protected
    FOmniTED: TOmniEventMonitor;
    FThreadPool: IOmniThreadPool;

    FInList, FBlackList: TInterfaceList;

    procedure OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); virtual;
  public
    constructor Create(AThreadPoolName: string; AMaxExecuting: Integer); virtual;

    function IsIdle: WordBool;

    destructor Destroy; override;
  end;

const
  MSG_TASK_BLACKLISTED = 0;
  MSG_CONTINUE_TASK = 1;
  MSG_QUIT_TASK = 2;
  MSG_TASK_QUIT = 3;

implementation

{ TMyOmniWorker }

function TMyOmniWorker.CheckforBlacklist(AInterfaceComponent: IInterface): Boolean;
var
  msg: TOmniMessage;
begin
  Result := False;

  task.Comm.Send(MSG_TASK_BLACKLISTED, [task.UniqueID, AInterfaceComponent]);

  repeat
    task.Comm.ReceiveWait(msg, INFINITE);
  until (((msg.MsgID = MSG_CONTINUE_TASK) or (msg.MsgID = MSG_QUIT_TASK)) and (msg.MsgData.AsInt64 = task.UniqueID));

  if (msg.MsgID = MSG_QUIT_TASK) then
  begin
    task.SetTimer(1, 1, MSG_SLEEP);
    Result := True;
  end;
end;

procedure TMyOmniWorker.SleepTask(var msg: TMessage);
begin
  task.ClearTimer(1);
  Sleep(1000);
  task.Terminate;
end;

{ TThreadPool }

procedure TThreadPoolManager.OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
var
  indx: Integer;
begin
  case msg.MsgID of
    MSG_TASK_BLACKLISTED:
      begin
        indx := FBlackList.IndexOf(msg.MsgData[1].AsInterface);
        if indx <> -1 then
        begin
          FBlackList.Delete(indx);
          task.Comm.Send(MSG_QUIT_TASK, msg.MsgData[0].AsInt64);
        end
        else
          task.Comm.Send(MSG_CONTINUE_TASK, msg.MsgData[0].AsInt64);
      end;
    MSG_TASK_QUIT:
      begin
        FInList.Remove(msg.MsgData.AsInterface);
        indx := FBlackList.IndexOf(msg.MsgData.AsInterface);
        if indx <> -1 then
          FBlackList.Delete(indx);
      end;
  end;
end;

constructor TThreadPoolManager.Create(AThreadPoolName: string; AMaxExecuting: Integer);
begin
  FOmniTED := TOmniEventMonitor.Create(nil);
  with FOmniTED do
    OnTaskMessage := OmniTEDTaskMessage;

  FThreadPool := CreateThreadPool(AThreadPoolName);

  with FThreadPool do
  begin
    MaxExecuting := AMaxExecuting;
    MaxQueued := 0;
  end;

  FInList := TInterfaceList.Create;
  FBlackList := TInterfaceList.Create;
end;

function TThreadPoolManager.IsIdle: WordBool;
begin
  Result := FThreadPool.IsIdle;
end;

destructor TThreadPoolManager.Destroy;
begin
  FInList.Free;
  FBlackList.Free;
  if Assigned(FThreadPool) then
  begin
    FThreadPool.CancelAll;
    FThreadPool := nil;
  end;
  FOmniTED.Free;
  inherited Destroy;
end;

end.
