{.$DEFINE DEBUG_THREADWORKER}
unit uApiThreadManager;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Math, Messages, Dialogs,
  // Spring Framework
  Spring.Collections.Lists,
  // OmniThreadLibrary
  OtlComm, OtlCommon, OtlEventMonitor, OtlTaskControl,
  // Generic TThreadList
  hThreadList,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface;

type
  TThreadWorkData = class
  protected
    // TODO: FUTURE: Replace ITabSheetController instance with UniqueID of every tab.
    FTabSheetController: ITabSheetController;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property TabSheetController: ITabSheetController read FTabSheetController write FTabSheetController;
  end;

  TThreadWorker<T: TThreadWorkData, constructor> = class(TOmniWorker)
  protected
    FData: T;
    procedure DefaultErrorHandler(const AErrorMsg: string); virtual;
    function InBlackList: Boolean;
    function Initialize: Boolean; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; virtual; abstract;
    procedure Finish(const AByItSelf: Boolean = False);

    property Data: T read FData;
  end;

  TThreadManagerRemoveProc<T> = reference to procedure(const AJobWorkData: T; var ARemove: Boolean);

  TThreadManager<T: TThreadWorkData, constructor> = class(TInterfacedObject, IThreadManager)
  private
    FInList, FBlackList: TThreadList<T>;
  protected
    FOmniEM: TOmniEventMonitor;
    function InAnyList(const ATabSheetController: ITabSheetController): Boolean; virtual;
    function InList(const AJobWorkData: T): Boolean;
    function InBlackList(const AJobWorkData: T): Boolean;

    procedure OmniEMTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage); virtual;

    function CanAddJob(const AJobWorkData: T): Boolean;
    function AddJob(const AJobWorkData: T): Boolean;
    function CanRemoveJob(const AJobWorkData: T): Boolean;
    function RemoveJob(const AJobWorkData: T): Boolean;
    function RemoveAllJobs: Boolean;

    function RemoveIterator(ARemoveProc: TThreadManagerRemoveProc<T>): Boolean;
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
  MSG_TASK_QUIT_BY_ITSELF = 4;

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

procedure TThreadWorker<T>.DefaultErrorHandler(const AErrorMsg: string);
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
    // Do not do the Finish() call here!!
    Result := True;
  end;
end;

function TThreadWorker<T>.Initialize: Boolean;
begin
  Result := inherited Initialize;
  if not Result or InBlackList then
  begin
    // Make the Finish() call but indicate that the task will be quit by itself
    Finish(True);
    Exit(False);
  end;
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

procedure TThreadWorker<T>.Finish(const AByItSelf: Boolean = False);
var
  LOmniValue: TOmniValue;
begin
  LOmniValue := TOmniValue.CastFrom<T>(Data);
  task.Comm.Send(IfThen(AByItSelf, MSG_TASK_QUIT_BY_ITSELF, MSG_TASK_QUIT), [task.UniqueID, LOmniValue.AsObject]);
{$IFDEF DEBUG_THREADWORKER}
  OutputDebugString(PChar(ClassName + ' Send: MSG_TASK_QUIT'));
{$ENDIF}
end;

{ TThreadManager<T> }

function TThreadManager<T>.InAnyList(const ATabSheetController: ITabSheetController): Boolean;
var
  LList: TThreadList<T>.TListObj;
  LListIndex: Integer;
  LListItem: T;
begin
  Result := False;

  LList := FInList.LockList;
  try
    for LListIndex := 0 to LList.Count - 1 do
    begin
      LListItem := LList[LListIndex];

      if ATabSheetController = LListItem.TabSheetController then
      begin
        Exit(True);
      end;
    end;
  finally
    FInList.UnlockList;
  end;

  LList := FBlackList.LockList;
  try
    for LListIndex := 0 to LList.Count - 1 do
    begin
      LListItem := LList[LListIndex];

      if ATabSheetController = LListItem.TabSheetController then
      begin
        Exit(True);
      end;
    end;
  finally
    FBlackList.UnlockList;
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
  (*
    task.Comm.Send( <Message ID>, [<UniqueID>, <TThreadWorkData>, <Additional Parameter 1>, <Additional Parameter 2>, ...])
    *)
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
    MSG_TASK_QUIT, MSG_TASK_QUIT_BY_ITSELF:
      begin
{$IFDEF DEBUG_THREADWORKER}
        OutputDebugString(PChar(ClassName + ' Retrieve: MSG_TASK_QUIT'));
{$ENDIF}
        FInList.Remove(LJobWorkData);
{$IFDEF DEBUG_THREADWORKER}
        OutputDebugString(PChar(ClassName + ' Removed from worklist:' + IntToStr(FInList.Count)));
{$ENDIF}
        if FBlackList.Contains(LJobWorkData) then
        begin
          FBlackList.Remove(LJobWorkData);
{$IFDEF DEBUG_THREADWORKER}
          OutputDebugString('Removed from blacklist');
{$ENDIF}
        end;

        if (msg.MsgID = MSG_TASK_QUIT) then
        begin
          task.Terminate;
{$IFDEF DEBUG_THREADWORKER}
          OutputDebugString('task.Terminate');
{$ENDIF}
        end;
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
{$IFDEF DEBUG_THREADWORKER}
    OutputDebugString(PChar(ClassName + 'Added to worklist'));
{$ENDIF}
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
  Result := True;
  if CanRemoveJob(AJobWorkData) then
  begin
    if not InBlackList(AJobWorkData) then
    begin
      FBlackList.Add(AJobWorkData);
{$IFDEF DEBUG_THREADWORKER}
      OutputDebugString(PChar(ClassName + 'Added to blacklist'));
{$ENDIF}
    end;
  end
  else
  begin
    Result := False;
  end;
end;

function TThreadManager<T>.RemoveAllJobs: Boolean;
begin
  Result := RemoveIterator(
    { } procedure(const AJobWorkData: T; var ARemove: Boolean)
    { } begin
    { . } ARemove := True;
    { } end);
end;

function TThreadManager<T>.RemoveIterator(ARemoveProc: TThreadManagerRemoveProc<T>): Boolean;
var
  LList: TThreadList<T>.TListObj;
  LListIndex: Integer;
  LListItem: T;
  LRemove: Boolean;
  LRemoveList: TThreadList<T>;
begin
  Result := True;
  LRemoveList := TThreadList<T>.Create(False);
  try
    LList := FInList.LockList;
    try
      for LListIndex := 0 to LList.Count - 1 do
      begin
        LListItem := LList[LListIndex];
        LRemove := False;
        ARemoveProc(LListItem, LRemove);
        if LRemove then
          LRemoveList.Add(LList[LListIndex]);
      end;
    finally
      FInList.UnlockList;
    end;
    for LListItem in LRemoveList do
      Result := Result and RemoveJob(LListItem);
  finally
    LRemoveList.Free;
  end;
end;

constructor TThreadManager<T>.Create;
begin
  inherited Create;

  FInList := TThreadList<T>.Create(False);
  FBlackList := TThreadList<T>.Create(False);

  FOmniEM := TOmniEventMonitor.Create(nil);
  with FOmniEM do
  begin
    OnTaskMessage := OmniEMTaskMessage;
  end;
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
  FBlackList.Free;
  FInList.Free;
  FOmniEM.Free;
  inherited Destroy;
end;

end.
