unit uApiTabSheetItem;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ImgList, ActnList, StdCtrls,
  ShellAPI, Generics.Collections, Math, StrUtils, ExtCtrls, Clipbrd, ActiveX, AxCtrls,
  // Dev Express
  cxControls, dxBar, cxScrollBox, cxDropDownEdit, cxImageComboBox, cxButtons, cxSplitter,
  // OmniThreadLibrary
  OtlParallel, OtlTaskControl,
  // HtmlViewer
  HtmlGlobals, HtmlView, htmlun2,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiConst, uApiCodeTag, uApiComponentController, uApiIScriptFormatter, uApiMirrorController, uApiPublishController, uApiMultiCastEvent, uApiSettings,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPManager,
  // MultiEvent
  Generics.MultiEvents.Handler, Generics.MultiEvents.NotifyHandler,
  // Utils
  uPathUtils, uStringUtils,
  // Plugin
  uPlugInEvent,
  // Forms
  uSelectDialog,
  // Frames
  ufIScriptDesigner;

type
  TPopupMenuChangeMethod = procedure(const Sender: Integer) of object;

  TIPopupMenuChangeEventHandler = class(TGenericEventHandler<TPopupMenuChangeMethod>, IPopupMenuChange)
  public
    procedure Invoke(const Sender: Integer); safecall;
  end;

  TTabSheetItem = class(TcxScrollBox)
  public
    constructor Create(AOwner: TComponent; ATabSheetController: ITabSheetController); reintroduce; virtual;
  end;

  TDataTabSheetItem = class(TTabSheetItem)
  private
    FComponentController: IControlController;
    FISpaceMouseDown: TINotifyEventHandler;
    FIPopupMenuChange: TIPopupMenuChangeEventHandler;
    FIControlEnter: TIControlEventHandler;
    FMirrorController: IMirrorController;
    procedure FScrollBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    procedure MoveWorkWhileHoldingLeftMouse;
  protected
    procedure SpaceMouseDown(const Sender: IUnknown);

    procedure ControlEnter(const Sender: IBasic);

    procedure PopupMenuChange(const Sender: Integer);
  public
    constructor Create(AOwner: TComponent; ATabSheetController: ITabSheetController); override;
    procedure CreateInner(ATabSheetController: ITabSheetController);

    property ControlController: IControlController read FComponentController write FComponentController;
    property MirrorController: IMirrorController read FMirrorController write FMirrorController;

    procedure DestroyInner;
    destructor Destroy; override;
  end;

  TDesignTabSheetItem = class(TTabSheetItem)
  private
    FTabSheetController: ITabSheetController;
    FPublishController: IPublishController;

    FActiveDesigner: TIScriptDesigner;

    // website data
    FActiveWebsiteData: ICMSWebsiteContainer;
    FActiveCMSCollectionItem: TCMSCollectionItem;
    FSubjectChange: TICMSItemChangeEventHandler;
    FMessageChange: TICMSItemChangeEventHandler;

    // code + preview
    FWebsitePanel: TPanel;
    FWebsite: TcxImageComboBox;

    // code
    FCodePanel: TPanel;
    FSubjectDesigner: TIScriptDesigner;
    FcxSplitter: TcxSplitter;
    FMessageDesigner: TIScriptDesigner;

    // preview
    FPreviewPanel: TPanel;
    FHtmlView: THtmlViewer;
    FCopySubjectToClipboardButton, FCopyMessageToClipboardButton: TcxButton;

    function GetCMSWebsiteContainer: ICMSWebsiteContainer;

    procedure SubjectUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex, AParam: Integer);
    procedure MessageUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex, AParam: Integer);

    procedure FWebsiteChange(Sender: TObject);
    procedure FHtmlViewHotSpotClick(Sender: TObject; const URL: string; var Handled: Boolean);
    procedure FHtmlViewHotSpotChange(Sender: TObject; const URL: ThtString);
    procedure FHtmlViewImageOver(Sender, Obj: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FHtmlViewImageRequest(Sender: TObject; const SRC: ThtString; var Stream: TStream);
    procedure FCheckSubjectScriptButtonClick(Sender: TObject);
    procedure FFormatSubjectScriptButtonClick(Sender: TObject);
    procedure FCopySubjectToClipboardButtonClick(Sender: TObject);
    procedure FCheckMessageScriptButtonClick(Sender: TObject);
    procedure FFormatMessageScriptButtonClick(Sender: TObject);
    procedure FCopyMessageToClipboardButtonClick(Sender: TObject);

    procedure FSubjectMemoEnter(Sender: TObject);
    procedure FMessageMemoEnter(Sender: TObject);
    procedure FMemoExit(Sender: TObject);

    procedure CheckScript(AIScriptDesigner: TIScriptDesigner);
    procedure ViewChange(const NewViewType: TTabViewType);
  protected
    FIViewChangeEvent: IViewChangeEventHandler;

    function GetActiveWebsite: WideString;
    procedure SetActiveWebsite(AWebsite: WideString);
    function GetActiveWebsiteData: ICMSWebsiteContainer;
    procedure SetActiveWebsiteData(AData: ICMSWebsiteContainer);
    procedure RegisterWebsite;
    procedure DeregisterWebsite;
    procedure UpdateActiveWebsite;

    function GetViewType: TTabViewType;
    procedure SetViewType(AViewType: TTabViewType);

    procedure UpdateHTMLView(ASubject, AMessage: RIScriptResult);
    procedure RenderHTMLView;
  public
    constructor Create(AOwner: TComponent; ATabSheetController: ITabSheetController); override;

    property ActiveWebsite: WideString read GetActiveWebsite write SetActiveWebsite;
    property ActiveWebsiteData: ICMSWebsiteContainer read GetActiveWebsiteData;

    property PublishController: IPublishController read FPublishController write FPublishController;

    property ViewType: TTabViewType read GetViewType write SetViewType;

    procedure InsertTextBetweenSelected(ACodeTag: TCodeTag);

    procedure UpdateCMSList(const Sender: IPublishController);
    procedure UpdateCMSWebsiteList(const Sender: ICMSContainer; CMSIndex: Integer);

    destructor Destroy; override;
  end;

implementation

uses
  uMain;

{ TIPopupMenuChangeEventHandler }

procedure TIPopupMenuChangeEventHandler.Invoke(const Sender: Integer);
begin
  if (@FHandler <> nil) then
    FHandler(Sender);
end;

{ TTabSheetItem }

constructor TTabSheetItem.Create;
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  Align := alClient;
  BorderStyle := cxcbsNone;
  // Color := clWhite;

  HorzScrollBar.Visible := False;
  VertScrollBar.Visible := False;

  Transparent := True;

  Visible := False;
end;

{ TDataTabSheetItem }

procedure TDataTabSheetItem.FScrollBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  MoveWorkWhileHoldingLeftMouse;
end;

procedure TDataTabSheetItem.MoveWorkWhileHoldingLeftMouse;
begin
  if SettingsManager.Settings.MoveWorkWhileHoldingLeftMouse and (GetAsyncKeyState(VK_LBUTTON) <> 0) then
  begin
    ReleaseCapture;
    SendMessage(Main.Handle, WM_SYSCOMMAND, SC_MOVE + HTCAPTION, 0);
  end;
end;

procedure TDataTabSheetItem.SpaceMouseDown(const Sender: IInterface);
begin
  MoveWorkWhileHoldingLeftMouse;
end;

procedure TDataTabSheetItem.ControlEnter(const Sender: IBasic);
begin
  Main.fControlEditor.Control := Sender;
end;

procedure TDataTabSheetItem.PopupMenuChange(const Sender: Integer);
begin
  Main.SetEditMenu(TdxBarItemLinks(Sender));
end;

constructor TDataTabSheetItem.Create;
begin
  inherited Create(AOwner, ATabSheetController);
  VertScrollBar.Visible := True;
  VertScrollBar.Tracking := True;

  OnMouseDown := FScrollBoxMouseDown;

  Visible := True;

  Main.fControlEditor.Control := nil;

  FISpaceMouseDown := TINotifyEventHandler.Create(SpaceMouseDown);

  FIPopupMenuChange := TIPopupMenuChangeEventHandler.Create(PopupMenuChange);

  FIControlEnter := TIControlEventHandler.Create(ControlEnter);

  CreateInner(ATabSheetController);
end;

procedure TDataTabSheetItem.CreateInner(ATabSheetController: ITabSheetController);
begin
  ControlController := TComponentController.Create(Self);
  with ControlController do
  begin
    TabSheetController := ATabSheetController;
    ATypeID := ATabSheetController.ATypeID;

    OnSpaceMouseDown := FISpaceMouseDown;

    OnControlEnter := FIControlEnter;

    OnPopupMenuChange := FIPopupMenuChange;
  end;

  MirrorController := TMirrorController.Create(Self);
  with MirrorController do
  begin
    TabSheetController := ATabSheetController;

    OnSpaceMouseDown := FISpaceMouseDown;

    OnPopupMenuChange := FIPopupMenuChange;
  end;
end;

procedure TDataTabSheetItem.DestroyInner;
var
  I, J: Integer;
  Picture: IPicture;
begin
  with MirrorController do
  begin
    OnSpaceMouseDown := nil;

    OnPopupMenuChange := nil;

    for I := MirrorCount - 1 downto 0 do
      Remove(I);

    TabSheetController := nil;
  end;
  MirrorController := nil;

  with ControlController do
  begin
    for I := 0 to ControlCount - 1 do
    begin
      if Control[I].QueryInterface(IPicture, Picture) = 0 then
      begin
        for J := 0 to Picture.MirrorCount - 1 do
          Picture.Mirror[J].Picture := nil;
        Picture := nil;
      end;
      Control[I].ControlController := nil;
    end;

    OnPopupMenuChange := nil;

    OnControlEnter := nil;

    OnSpaceMouseDown := nil;

    OnReleaseNameChange := nil;

    TabSheetController := nil;
  end;
  ControlController := nil;
end;

destructor TDataTabSheetItem.Destroy;
begin
  with Main do
  begin
    SetEditMenu(nil);
    fControlEditor.Control := nil;
  end;

  DestroyInner;

  FMirrorController := nil;
  FIControlEnter := nil;
  FIPopupMenuChange := nil;
  FISpaceMouseDown := nil;
  FComponentController := nil;

  inherited Destroy;
end;

{ ****************************************************************************** }

{ TDesignTabSheetItem }

function TDesignTabSheetItem.GetCMSWebsiteContainer: ICMSWebsiteContainer;

  function GetImageComboBoxValue(AValue: string): string;
  begin
    Result := copy(AValue, 1, LastDelimiter('=', AValue) - 1);
  end;

var
  CMSName: string;
  CMSWebsiteIndex: Integer;
begin
  if (FWebsite.ItemIndex = -1) then
    Exit(nil);

  with FWebsite.Properties.Items[FWebsite.ItemIndex] do
  begin
    CMSName := GetImageComboBoxValue(VarToStr(Value));
    CMSWebsiteIndex := Tag;
  end;

  Result := PublishController.CMS[CMSName].Website[CMSWebsiteIndex];
end;

procedure TDesignTabSheetItem.SubjectUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex, AParam: Integer);
var
  LCMSWebsitesCollectionItem: TCMSWebsitesCollectionItem;
begin
  if (ACMSItemChangeType = cctChange) then
  begin
    LCMSWebsitesCollectionItem := TCMSWebsitesCollectionItem(FActiveCMSCollectionItem.Websites.Items[AIndex]);

    if SameStr(ActiveWebsite, LCMSWebsitesCollectionItem.Website) then
    begin
      if Assigned(ActiveWebsiteData) then
        FSubjectDesigner.SetFileName(ActiveWebsiteData.SubjectFileName);
      RenderHTMLView;
    end;
  end;
end;

procedure TDesignTabSheetItem.MessageUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex, AParam: Integer);
var
  LCMSWebsitesCollectionItem: TCMSWebsitesCollectionItem;
begin
  if (ACMSItemChangeType = cctChange) then
  begin
    LCMSWebsitesCollectionItem := TCMSWebsitesCollectionItem(FActiveCMSCollectionItem.Websites.Items[AIndex]);

    if SameStr(ActiveWebsite, LCMSWebsitesCollectionItem.Website) then
    begin
      if Assigned(ActiveWebsiteData) then
        FMessageDesigner.SetFileName(ActiveWebsiteData.MessageFileName);
      RenderHTMLView;
    end;
  end;
end;

procedure TDesignTabSheetItem.FWebsiteChange(Sender: TObject);
begin
  UpdateActiveWebsite;
end;

procedure TDesignTabSheetItem.FHtmlViewHotSpotClick(Sender: TObject; const URL: string; var Handled: Boolean);
begin
  ShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_SHOW);
  Handled := True;
end;

procedure TDesignTabSheetItem.FHtmlViewHotSpotChange(Sender: TObject; const URL: ThtString);
begin
  if not SameStr('', URL) then
    FHtmlView.Hint := URL
  else if not SameStr('', FHtmlView.TitleAttr) then
    FHtmlView.Hint := FHtmlView.TitleAttr
  else
    FHtmlView.Hint := '';
end;

procedure TDesignTabSheetItem.FHtmlViewImageOver(Sender, Obj: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not SameStr('', FHtmlView.TitleAttr) then
    FHtmlView.Hint := FHtmlView.TitleAttr
  else
    FHtmlView.Hint := '';
end;

procedure TDesignTabSheetItem.FHtmlViewImageRequest(Sender: TObject; const SRC: ThtString; var Stream: TStream);
var
  HTTPRequest: IHTTPRequest;
  HTTPOptions: IHTTPOptions;
  RequestID: Double;
begin
  HTTPRequest := THTTPRequest.Create(SRC);
  HTTPRequest.Referer := SRC;

  HTTPOptions := THTTPOptions.Create(SettingsManager.Settings.http.GetProxy(psaMain));
  HTTPOptions.ConnectTimeout := SettingsManager.Settings.http.ConnectTimeout;
  HTTPOptions.ReadTimeout := SettingsManager.Settings.http.ReadTimeout;

  RequestID := THTTPManager.Instance().Get(HTTPRequest, HTTPOptions);

  Stream := WaitStream;

  Parallel.Async(
    { } procedure
    { } begin
    { . } THTTPManager.Wait(RequestID);
    { } end,
    { } Parallel.TaskConfig.OnTerminated(
      { } procedure(const task: IOmniTaskControl)
      { } var
      { . } HTTPProcess: IHTTPProcess;
      { . } OleStream: TOleStream;
      { . } Dummy: Int64;
      { } begin
      { . } HTTPProcess := THTTPManager.Instance.GetResult(RequestID);
      { . } if not HTTPProcess.HTTPResult.HasError then
      { . } begin
      { ... } OleStream := TOleStream.Create(HTTPProcess.HTTPResult.HTTPResponse.ContentStream);
      { ... } try
      { ..... } HTTPProcess.HTTPResult.HTTPResponse.ContentStream.Seek(0, STREAM_SEEK_SET, Dummy);
      { ..... } OleStream.Seek(0, STREAM_SEEK_SET);
      { ..... } while not FHtmlView.InsertImage(SRC, OleStream) do sleep(50);
      { ... } finally
      { ..... } OleStream.Free;
      { ... } end;
      { . } end;
      { } end
      { } ));
end;

procedure TDesignTabSheetItem.FCheckSubjectScriptButtonClick(Sender: TObject);
begin
  CheckScript(FSubjectDesigner);
end;

procedure TDesignTabSheetItem.FFormatSubjectScriptButtonClick(Sender: TObject);
begin
  with FSubjectDesigner do
    Data := TIScriptFormatter.Format(Data);
end;

procedure TDesignTabSheetItem.FCopySubjectToClipboardButtonClick(Sender: TObject);
begin
  if Assigned(ActiveWebsiteData) then
    Clipboard.AsText := ActiveWebsiteData.ParseIScript(FSubjectDesigner.Data).CompiledText;
end;

procedure TDesignTabSheetItem.FCheckMessageScriptButtonClick(Sender: TObject);
begin
  CheckScript(FMessageDesigner);
end;

procedure TDesignTabSheetItem.FFormatMessageScriptButtonClick(Sender: TObject);
begin
  with FMessageDesigner do
    Data := TIScriptFormatter.Format(Data);
end;

procedure TDesignTabSheetItem.FCopyMessageToClipboardButtonClick(Sender: TObject);
begin
  if Assigned(ActiveWebsiteData) then
    Clipboard.AsText := ActiveWebsiteData.ParseIScript(FMessageDesigner.Data).CompiledText;
end;

procedure TDesignTabSheetItem.FSubjectMemoEnter(Sender: TObject);
begin
  FActiveDesigner := FSubjectDesigner;
  Main.fMain.SwitchDesignView(True);
end;

procedure TDesignTabSheetItem.FMessageMemoEnter(Sender: TObject);
begin
  FActiveDesigner := FMessageDesigner;
  Main.fMain.SwitchDesignView(True);
end;

procedure TDesignTabSheetItem.FMemoExit(Sender: TObject);
begin
  FActiveDesigner := nil;
  Main.fMain.SwitchDesignView(False);
end;

procedure TDesignTabSheetItem.CheckScript(AIScriptDesigner: TIScriptDesigner);
var
  LIScirptResult: RIScriptResult;
begin
  if Assigned(ActiveWebsiteData) then
    with AIScriptDesigner do
    begin
      LIScirptResult := ActiveWebsiteData.CheckIScript(Data);
      if LIScirptResult.HasError then
      begin
        if SameStr('', LIScirptResult.ErrorUnit) then
          MessageDlg(LIScirptResult.ErrorMessage, mtError, [mbOK], 0)
        else
          MessageDlg(LIScirptResult.ErrorUnit + ': ' + LIScirptResult.ErrorMessage, mtError, [mbOK], 0);

        if AdvMemo.CanFocus then
          AdvMemo.SetFocus;
        AdvMemo.SetCursor(LIScirptResult.X, LIScirptResult.Y);
      end
      else
        MessageDlg('The IScript compiled successful.', mtInformation, [mbOK], 0);
    end;
end;

procedure TDesignTabSheetItem.ViewChange(const NewViewType: TTabViewType);
begin
  if FTabSheetController.IsTabActive then
  begin
    Main.fMain.SwitchDesignView((NewViewType = vtData) and Assigned(FActiveDesigner));
    if (vtPreview = NewViewType) then
    begin
      RenderHTMLView;
    end;
  end;
end;

function TDesignTabSheetItem.GetActiveWebsite: WideString;
begin
  Result := IfThen(Assigned(ActiveWebsiteData), ActiveWebsiteData.Name);
end;

procedure TDesignTabSheetItem.SetActiveWebsite(AWebsite: WideString);
var
  WebsiteDescription: string;
  NewWebsiteIndex, Index: Integer;
begin
  WebsiteDescription := RemoveW(ExtractUrlHost(AWebsite));

  with FWebsite do
  begin
    for Index := 0 to Properties.Items.Count - 1 do
      if SameText(WebsiteDescription, Properties.Items[Index].Description) then
      begin
        ItemIndex := Index;
        break;
      end;
  end;
end;

function TDesignTabSheetItem.GetActiveWebsiteData: ICMSWebsiteContainer;
begin
  result := FActiveWebsiteData;
end;

procedure TDesignTabSheetItem.SetActiveWebsiteData(AData: ICMSWebsiteContainer);
begin
  FActiveWebsiteData := AData;
end;

procedure TDesignTabSheetItem.RegisterWebsite;
begin
  FActiveCMSCollectionItem := TCMSCollectionItem(SettingsManager.Settings.Plugins.FindPlugInCollectionItemFromCollection(ActiveWebsiteData.CMS, SettingsManager.Settings.Plugins.CMS));
  FSubjectChange := TICMSItemChangeEventHandler.Create(SubjectUpdate);
  FActiveCMSCollectionItem.OnSubjectsChange.Add(FSubjectChange);
  FMessageChange := TICMSItemChangeEventHandler.Create(MessageUpdate);
  FActiveCMSCollectionItem.OnMessagesChange.Add(FMessageChange);
end;

procedure TDesignTabSheetItem.DeregisterWebsite;
begin
  if Assigned(FActiveCMSCollectionItem) then
  begin
    FActiveCMSCollectionItem.OnSubjectsChange.Remove(FSubjectChange);
    FActiveCMSCollectionItem.OnMessagesChange.Remove(FMessageChange);
    FSubjectChange := nil;
    FMessageChange := nil;
  end;
end;

procedure TDesignTabSheetItem.UpdateActiveWebsite;
begin
  SetActiveWebsiteData(GetCMSWebsiteContainer);

  if Assigned(ActiveWebsiteData) then
  begin
    DeregisterWebsite;
    RegisterWebsite;

    FSubjectDesigner.SetFileName(ActiveWebsiteData.SubjectFileName);
    FMessageDesigner.SetFileName(ActiveWebsiteData.MessageFileName);

    RenderHTMLView;
  end;
end;

function TDesignTabSheetItem.GetViewType: TTabViewType;
begin
  Result := vtData;
  if FCodePanel.Visible then
    Exit(vtCode)
  else if FPreviewPanel.Visible then
    Exit(vtPreview);
end;

procedure TDesignTabSheetItem.SetViewType(AViewType: TTabViewType);
begin
  FWebsitePanel.Visible := (AViewType = vtCode) or (AViewType = vtPreview);

  FCodePanel.Visible := (AViewType = vtCode);

  FPreviewPanel.Visible := (AViewType = vtPreview);
end;

procedure TDesignTabSheetItem.UpdateHTMLView(ASubject, AMessage: RIScriptResult);

  function IsBBCode(AIScriptOutput: string): Boolean;
  begin
    Result := (CharCount('[', AIScriptOutput) > CharCount('<', AIScriptOutput)) and (CharCount(']', AIScriptOutput) > CharCount('>', AIScriptOutput));
  end;

  function BBCodeToHTML(ABBCode: string): string;
  var
    BasicHTML: string;

    function OpenBB(ATag: string): string;
    begin
      Result := '[' + ATag + ']';
    end;

    function CloseBB(ATag: string): string;
    begin
      Result := '[/' + ATag + ']';
    end;

    function OpenHTML(ATag: string): string;
    begin
      Result := '<' + ATag + '>';
    end;

    function CloseHTML(ATag: string): string;
    begin
      Result := '</' + ATag + '>';
    end;

  begin
    BasicHTML := StringReplaceMultiple(ABBCode, { }
      [sLineBreak, #10, OpenBB('b'), CloseBB('b'), OpenBB('i'), CloseBB('i'), OpenBB('u'), CloseBB('u'), OpenBB('s'), CloseBB('s'), OpenBB('left'), CloseBB('left'), OpenBB('center'),
      CloseBB('center'), OpenBB('right'), CloseBB('right'), OpenBB('align=left'), OpenBB('align=center'), OpenBB('align=right'), CloseBB('align'), CloseBB('size'), CloseBB('color')], { }
      [OpenHTML('br'), OpenHTML('br'), OpenHTML('strong'), CloseHTML('strong'), OpenHTML('em'), CloseHTML('em'), OpenHTML('ins'), CloseHTML('ins'), OpenHTML('del'), CloseHTML('del'),
      OpenHTML('div align="left"'), CloseHTML('div'), OpenHTML('div align="center"'), CloseHTML('div'), OpenHTML('div align="right"'), CloseHTML('div'), OpenHTML('div align="left"'),
      OpenHTML('div align="center"'), OpenHTML('div align="right"'), CloseHTML('div'), CloseHTML('font'), CloseHTML('font')], { }
      False);

    with TRegExpr.Create do
      try
        ModifierI := True;

        Expression := '\[size=?[''"]?(.*?)[''"]?\]';
        BasicHTML := Replace(BasicHTML, '<font size="$1">', True);

        Expression := '\[color=?[''"]?(.*?)[''"]?\]';
        BasicHTML := Replace(BasicHTML, '<font color="$1">', True);

        Expression := '\[img\](.*?)\[\/img\]';
        BasicHTML := Replace(BasicHTML, '<img src="$1" alt="" />', True);

        Expression := '\[email\](.*?)\[\/email\]';
        BasicHTML := Replace(BasicHTML, '<a href="mailto:$1"></a>', True);

        Expression := '\[youtube\](.*?)\[\/youtube\]';
        BasicHTML := Replace(BasicHTML,
          '<object width="425" height="350"><param name="movie" value="$1"></param><param name="wmode" value="transparent"></param><embed src="$1" type="application/x-shockwave-flash" wmode="transparent" width="425" height="350"></embed></object>', True);

        Expression := '\[url\](.*?)\[\/url\]';
        BasicHTML := Replace(BasicHTML, '<a href="$1">$1</a>', True);
        Expression := '\[url=[''"]?(.*?)[''"]?\](.*?)\[\/url\]';
        BasicHTML := Replace(BasicHTML, '<a href="$1">$2</a>', True);

        Expression := '\[quote=?[''"]?(.*?)[''"]?\](.*?)\[\/quote\]';
        BasicHTML := Replace(BasicHTML, '<blockquote cite="$1">$2</blockquote>', True);

        Expression := '\[code(=php)?\](.*?)\[\/code\]';
        BasicHTML := Replace(BasicHTML, '<pre>$2</pre>', True);

        (*
          BasicHTML := ReplaceRegExpr('\[list\](.*?)\[\/list\]', BasicHTML, '<ul>$1</ul>', True);
          BasicHTML := ReplaceRegExpr('\[list\](.*?)\[\/list\]', BasicHTML, '<ul>$1</ul>', True);

          BasicHTML := ReplaceRegExpr('\[list=1\](.*?)\[\/list\]', BasicHTML, '<ol style="list-style-type: decimal">$2</ol>', True);

          ModifierI := False;

          BasicHTML := ReplaceRegExpr('\[\*\](.*?)\[\*\]', BasicHTML, '<li>$1</li>[*]', True);
          BasicHTML := ReplaceRegExpr('\[\*\](.*?)\[\/list\]', BasicHTML, '<li>$1</li>[/list]', True);

          BasicHTML := ReplaceRegExpr('\[list=A\](.*?)\[\/list\]', BasicHTML, '<ol style="list-style-type: upper-alpha">$2</ol>', True);

          BasicHTML := ReplaceRegExpr('\[list=a\](.*?)\[\/list\]', BasicHTML, '<ol style="list-style-type: lower-alpha">$2</ol>', True);

          BasicHTML := ReplaceRegExpr('\[list=i\](.*?)\[\/list\]', BasicHTML, '<ol style="list-style-type: lower-roman">$2</ol>', True);

          BasicHTML := ReplaceRegExpr('\[list=I\](.*?)\[\/list\]', BasicHTML, '<ol style="list-style-type: upper-roman">$2</ol>', True);
          *)
      finally
        Free;
      end;

    Result := BasicHTML;
  end;

const
  HTML_TITLE = '<h2>%s</h2>';
  HTML_ERROR = '&laquo;Error: %s&raquo;';

var
  Subject, Message, HTML: string;
begin

  if ASubject.HasError then
    Subject := Format(HTML_ERROR, [ASubject.ErrorMessage])
  else if SameStr('', ASubject.CompiledText) then
    Subject := Format(HTML_ERROR, ['Subject is empty'])
  else
    Subject := ASubject.CompiledText;

  if AMessage.HasError then
    Message := '<p>' + Format(HTML_ERROR, [AMessage.ErrorMessage]) + '</p>'
  else if SameStr('', AMessage.CompiledText) then
    Message := '<p>' + Format(HTML_ERROR, ['Message is empty']) + '</p>'
  else if IsBBCode(AMessage.CompiledText) then
    Message := BBCodeToHTML(AMessage.CompiledText)
  else
    Message := AMessage.CompiledText;

  HTML := Format(HTML_TITLE, [Subject]);
  HTML := HTML + Message;

  FHtmlView.LoadFromString(HTML);
end;

procedure TDesignTabSheetItem.RenderHTMLView;
begin
  SetActiveWebsiteData(GetCMSWebsiteContainer);

  if Assigned(ActiveWebsiteData) then
  begin
    UpdateHTMLView(ActiveWebsiteData.ParseIScript(FSubjectDesigner.Data), ActiveWebsiteData.ParseIScript(FMessageDesigner.Data));
  end;
end;

constructor TDesignTabSheetItem.Create;
begin
  inherited Create(AOwner, ATabSheetController);

  FTabSheetController := ATabSheetController;

  FPublishController := TIPublishController.Create(ATabSheetController);

  // code + preview
  FWebsitePanel := TPanel.Create(Self);
  with FWebsitePanel do
  begin
    ParentBackground := False;

    Parent := TWinControl(Self);

    Align := alTop;
    Anchors := [akLeft, akTop, akRight];

    Height := 8 + 21 + 6;

    BevelOuter := bvNone;

    Caption := '';
    Color := clWhite;
  end;

  FWebsite := TcxImageComboBox.Create(FWebsitePanel);
  with FWebsite do
  begin
    Parent := TWinControl(FWebsitePanel);

    Anchors := [akLeft, akTop, akRight];

    Left := 8;
    Top := 8;
    Width := Parent.Width - 8 - 8;

    with Properties do
    begin
      DropDownListStyle := lsEditFixedList;

      OnChange := FWebsiteChange;
    end;
  end;

  // code
  FCodePanel := TPanel.Create(Self);
  with FCodePanel do
  begin
    ParentBackground := False;

    Parent := TWinControl(Self);

    Align := alClient;
    Anchors := [akLeft, akTop, akRight, akBottom];

    BevelOuter := bvNone;

    Caption := '';
    Color := clWhite;
  end;

  FSubjectDesigner := TIScriptDesigner.Create(FCodePanel);
  with FSubjectDesigner do
  begin
    Align := alTop;

    Height := 75;

    cxBCheckScript.OnClick := FCheckSubjectScriptButtonClick;
    cxBFormatScript.OnClick := FFormatSubjectScriptButtonClick;

    with AdvMemo do
    begin
      OnEnter := FSubjectMemoEnter;
      OnExit := FMemoExit;
    end;
  end;

  FcxSplitter := TcxSplitter.Create(FCodePanel);
  with FcxSplitter do
  begin
    Parent := TWinControl(FCodePanel);

    AlignSplitter := salTop;

    AutoSnap := True;

    HotZone := TcxXPTaskBarStyle.Create(FcxSplitter);

    MinSize := 30;

    Control := FSubjectDesigner;
  end;

  FMessageDesigner := TIScriptDesigner.Create(FCodePanel);
  with FMessageDesigner do
  begin
    Align := alClient;

    cxBCheckScript.OnClick := FCheckMessageScriptButtonClick;
    cxBFormatScript.OnClick := FFormatMessageScriptButtonClick;

    with AdvMemo do
    begin
      OnEnter := FMessageMemoEnter;
      OnExit := FMemoExit;
    end;
  end;

  // preview
  FPreviewPanel := TPanel.Create(Self);
  with FPreviewPanel do
  begin
    ParentBackground := False;

    Parent := TWinControl(Self);

    Align := alClient;
    Anchors := [akLeft, akTop, akRight, akBottom];

    BevelOuter := bvNone;

    Caption := '';
    Color := clWhite;
  end;

  FHtmlView := THtmlViewer.Create(FPreviewPanel);
  with FHtmlView do
  begin
    Parent := TWinControl(FPreviewPanel);

    Anchors := [akLeft, akTop, akRight, akBottom];

    Left := 0;
    Top := 0;
    Width := Parent.Width;
    Height := Parent.Height - Top - 6 - 25 - 8;

    DefBackground := clWhite;

    ShowHint := True;

    OnHotSpotClick := FHtmlViewHotSpotClick;
    OnHotSpotCovered := FHtmlViewHotSpotChange;
    OnImageOver := FHtmlViewImageOver;
    OnImageRequest := FHtmlViewImageRequest;
  end;

  FCopySubjectToClipboardButton := TcxButton.Create(FPreviewPanel);
  with FCopySubjectToClipboardButton do
  begin
    Parent := TWinControl(FPreviewPanel);

    Anchors := [akRight, akBottom];

    Left := FHtmlView.Width - 8 - 121 - 6 - 121;
    Top := FHtmlView.Top + FHtmlView.Height + 6;
    Width := 121;

    Caption := 'copy subject';

    OnClick := FCopySubjectToClipboardButtonClick;
  end;

  FCopyMessageToClipboardButton := TcxButton.Create(FPreviewPanel);
  with FCopyMessageToClipboardButton do
  begin
    Parent := TWinControl(FPreviewPanel);

    Anchors := [akRight, akBottom];

    Left := FHtmlView.Width - 8 - 121;
    Top := FHtmlView.Top + FHtmlView.Height + 6;
    Width := 121;

    Caption := 'copy message';

    OnClick := FCopyMessageToClipboardButtonClick;
  end;

  FIViewChangeEvent := TIViewChangeEventHandler.Create(ViewChange);
  FTabSheetController.PageController.OnViewChange.Add(FIViewChangeEvent);
end;

procedure TDesignTabSheetItem.InsertTextBetweenSelected(ACodeTag: TCodeTag);
var
  TextSelected: Boolean;
  InputQueryStr, RealValue: string;
  SelectedText: TStringList;
  ParamList: TList<TCodeParam>;
  I, FirstUnanswered, RelCursorPos: Integer;

  function InList(AIndex: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to ParamList.Count - 1 do
      if (AIndex = ParamList.Items[I].Index) then
        Exit(True);
  end;

  function TrimOnceRight(AValue: string): string;
  begin
    Result := AValue;
    if EndsText(sLineBreak, AValue) then
      Result := copy(AValue, 1, length(AValue) - length(sLineBreak));
  end;

  function GetParamIndex(AValue: string): Integer;
  begin
    Result := -1;
    with TRegExpr.Create do
      try
        InputString := AValue;
        Expression := '\$param(\d+)';

        if Exec(InputString) then
          Result := StrToInt(Match[1]);
      finally
        Free;
      end;
  end;

  function FindItemAtIndex(AIndex: Integer; aList: TList<TCodeParam>): TCodeParam;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to aList.Count - 1 do
      if AIndex = aList.Items[I].Index then
        Exit(aList.Items[I]);
  end;

  procedure MinIfNotNull(var AValue: Integer; ANewValue: Integer);
  begin
    if (AValue > 0) then
      AValue := Min(AValue, ANewValue)
    else
      AValue := I;
  end;

begin
  ParamList := TList<TCodeParam>.Create;

  with TRegExpr.Create do
    try
      InputString := ACodeTag.Value;
      Expression := '\{param(\d+)(|=(.*?))\}';

      if Exec(InputString) then
      begin
        repeat
          if InList(StrToInt(Match[1])) then
            Continue;

          ParamList.Add(TCodeParam.Create(StrToInt(Match[1])));

          ParamList.Last.Description := ACodeTag.Params.Values[ParamList.Last.Name];

          if not SameText('', Match[2]) then
            ParamList.Last.SetDefaultValues(Match[3], ACodeTag.ParamValues.Values[ParamList.Last.Name + 'value']);

        until not ExecNext;
      end;
    finally
      Free;
    end;

  RealValue := ACodeTag.Value;

  TextSelected := not(FActiveDesigner.AdvMemo.SelLength = 0);

  if TextSelected then
  begin
    SelectedText := TStringList.Create;
    try
      SelectedText.Text := FActiveDesigner.AdvMemo.Selection;

      with TRegExpr.Create do
        try
          InputString := ACodeTag.Value;
          Expression := '\{selection=(.*?)\}';

          if Exec(InputString) then
            if (Pos('$lineX', string(Match[1])) > 0) then
            begin
              for I := 0 to SelectedText.Count - 1 do
                SelectedText.Strings[I] := StringReplace(Match[1], '$lineX', SelectedText.Strings[I], []);
            end
            else if (Pos('$param', string(Match[1])) > 0) then
            begin
              SelectedText.Text := StringReplace(Match[1], '$param' + IntToStr(GetParamIndex(Match[1])), SelectedText.Text, []);
              FindItemAtIndex(GetParamIndex(Match[1]), ParamList).WasSelected := True;
            end;

          RealValue := ReplaceRegExpr('\{selection=(.*?)\}', RealValue, TrimOnceRight(SelectedText.Text), False);
        finally
          Free;
        end;

    finally
      SelectedText.Free;
    end;
  end
  else
    with TRegExpr.Create do
      try
        RealValue := ReplaceRegExpr('\{selection=(.*?)\}', RealValue, '', False);
      finally
        Free;
      end;

  FirstUnanswered := 0;
  for I := 0 to ParamList.Count - 1 do
    if not ParamList.Items[I].WasSelected then
      if ParamList.Items[I].HasDefaultValues then
      begin
        with TSelectDialog.Create(nil) do
          try
            Caption := 'Caption';
            Description := 'Please select a value for the ' + ParamList.Items[I].Description + ':';

            with ParamList.Items[I].GetDefaultTextValues do
              try
                Items.Text := Text;
              finally
                Free;
              end;

            if Execute(False) then
              ParamList.Items[I].Value := ParamList.Items[I].NameFromValue(SelectedItem)
            else
              MinIfNotNull(FirstUnanswered, I);
          finally
            Free;
          end;

      end
      else
      begin
        InputQueryStr := '';
        if InputQuery('Caption', 'Please enter a value for the ' + ParamList.Items[I].Description + ':', InputQueryStr) then
          ParamList.Items[I].Value := InputQueryStr
        else
          MinIfNotNull(FirstUnanswered, I);
      end;

  with TRegExpr.Create do
    try
      InputString := RealValue;
      Expression := '\{param(\d+)(|=(.*?))\}';

      if Exec(InputString) then
      begin
        repeat

          RealValue := StringReplace(RealValue, Match[0], FindItemAtIndex(StrToInt(Match[1]), ParamList).Value, []);

        until not ExecNext;
      end;

    finally
      Free;
    end;

  for I := 0 to ParamList.Count - 1 do
    ParamList.Items[I].Free;
  ParamList.Free;

  RelCursorPos := Pos('{cursor}', RealValue);

  RealValue := StringReplace(RealValue, '{cursor}', '', []);

  if (RelCursorPos = 0) then
    RelCursorPos := length(RealValue);

  (*
    A Nichts wird in die Eingabefelder eingegeben
    -> Cursor wo??
    ->> Cursor beim ersten nicht ausgefüllten Feld

    B Werte werden in die Eingabefelder eingegeben
    -> Cursor nach Snippet

    C Text wird markiert
    -> Cursor nach Makierung
    *)

  FActiveDesigner.InsertText(RealValue);
end;

procedure TDesignTabSheetItem.UpdateCMSList(const Sender: IPublishController);
var
  I: Integer;
begin
  if Assigned(Sender) then
  begin
    for I := 0 to Sender.Count - 1 do
      UpdateCMSWebsiteList(Sender.CMS[I], I);

    with FWebsite do
      if (ItemIndex = -1) and (Properties.Items.Count > 0) then
        ItemIndex := 0;
  end;
end;

procedure TDesignTabSheetItem.UpdateCMSWebsiteList(const Sender: ICMSContainer; CMSIndex: Integer);

  function GetImageComboBoxValue(AValue: string): string;
  begin
    Result := copy(AValue, 1, LastDelimiter('=', AValue) - 1);
  end;

var
  I: Integer;
begin
  with FWebsite.Properties do
  begin
    BeginUpdate;
    try
      for I := Items.Count - 1 downto 0 do
        if SameText(Sender.Name, GetImageComboBoxValue(VarToStr(Items[I].Value))) then
          Items[I].Free;

      // Items.Clear;

      for I := 0 to Sender.Count - 1 do
        with Items.Add do
        begin
          Description := RemoveW(ExtractUrlHost(Sender.Website[I].Name));
          Tag := Sender.Website[I].Index;
          Value := Sender.Name + '=' + IntToStr(Sender.Website[I].Index);

        end;
    finally
      EndUpdate;
    end;
  end;
end;

destructor TDesignTabSheetItem.Destroy;
begin
  DeregisterWebsite;

  FTabSheetController.PageController.OnViewChange.Remove(FIViewChangeEvent);
  FIViewChangeEvent := nil;

  FCopyMessageToClipboardButton.Free;
  FCopySubjectToClipboardButton.Free;
  FHtmlView.Free;
  FPreviewPanel.Free;

  FMessageDesigner.Free;
  FcxSplitter.Free;
  FSubjectDesigner.Free;
  FCodePanel.Free;

  FWebsite.Free;
  FWebsitePanel.Free;

  with FPublishController do
  begin
    Active := False;
    // Disabled, because PublishController.Destroy() uses
    // TabSheetController.IsTabActive to Invoke updating the
    // CMS list only if the current tab was visible.
    // TabSheetController := nil;
  end;

  FPublishController := nil;

  FTabSheetController := nil;

  inherited Destroy;
end;

{ ****************************************************************************** }

end.
