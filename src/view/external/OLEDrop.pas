unit OLEDrop;

// -----------------------------------------------------------------------------
// Dezember 2011 - Tonic1024
// Eine einfache OLE Drag'n'Drop Klasse. So simpel wie möglich gehalten.
// Der Ruhm gebührt lbccaleb aus der DelphiPraxis. Der hat die meisste
// Arbeit gemacht. ;)
// -----------------------------------------------------------------------------

interface

uses
  Windows, Forms, Messages, ActiveX, ComObj;

type
  TTextDrop = procedure(AText: PChar) of object;

type
  TOleDrop = class(TObject, IDropTarget)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private
    DoTextDrop: TTextDrop;
    FDropzone: THandle;

    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
  public
    constructor Create(ADropzone: THandle);
    destructor Destroy; override;

    property OnTextDropped: TTextDrop write DoTextDrop;
  end;

implementation

constructor TOleDrop.Create(ADropzone: THandle);
begin
  inherited Create;
  FDropzone := aDropzone;
  OleCheck(RegisterDragDrop(FDropzone, Self));
end;

destructor TOleDrop.Destroy;
begin
  RevokeDragDrop(FDropzone);

  inherited Destroy;
end;

function TOleDrop.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TOleDrop._AddRef: Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TOleDrop._Release: Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
end;

function TOleDrop.DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  dwEffect := DROPEFFECT_COPY;
  Result := S_OK;
end;

function TOleDrop.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  dwEffect := DROPEFFECT_COPY;
  Result := S_OK;
end;

function TOleDrop.DragLeave: HResult;
begin
  Result := S_OK;
end;

function TOleDrop.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  FmtEtc: TFormatEtc;
  StgMed: TStgMedium;
  pData: PChar;
begin
  if (dataObj = nil) then
    MessageBox(Application.Handle, 'No valid pointer on IDataObject', 'OLEDrop Error', MB_OK or MB_ICONERROR);

  with FmtEtc do
  begin
    cfFormat := CF_UNICODETEXT;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;

  OleCheck(dataObj.GetData(FmtEtc, StgMed));
  try
    pData := GlobalLock(StgMed.hGlobal);
    if Assigned(DoTextDrop) then
      DoTextDrop(pData);
  finally
    GlobalUnlock(StgMed.hGlobal);
    ReleaseStgMedium(StgMed);
  end;
  Result := S_OK;
end;

initialization

OleInitialize(nil);

finalization

OleUnInitialize;

end.
