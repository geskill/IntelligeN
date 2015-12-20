unit ufIScriptDesigner;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Controls, Forms, StrUtils, FileCtrl, ShellAPI, ExtCtrls, Menus, StdCtrls,
  // DevExpress
  dxBar, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxControls, cxContainer, cxEdit, cxLabel, cxButtons,
  // AdvMemo
  AdvMemo, AdvFindDialogForm, AdvReplaceDialogForm,
  // RegEx
  RegExpr,
  // Mods
  uMyAdvmJScriptStyler,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Utils
  uStringUtils;

type
  TIScriptDesigner = class(TFrame, IIScriptController)
    AdvReplaceDialog: TAdvReplaceDialog;
    AdvFindDialog: TAdvFindDialog;
    AdvMemoFindDialog: TAdvMemoFindDialog;
    AdvMemoFindReplaceDialog: TAdvMemoFindReplaceDialog;
    AdvMemo: TAdvMemo;
    pTopControls: TPanel;
    cxBSave: TcxButton;
    cxBResetChanges: TcxButton;
    cxLFileName: TcxLabel;
    cxBCheckScript: TcxButton;
    cxBFormatScript: TcxButton;
    procedure FrameResize(Sender: TObject);
    procedure AdvMemoChange(Sender: TObject);
    procedure AdvMemoGetAutoCompletionList(Sender: TObject; AToken: string; AList: TStringList);
    procedure cxBSaveClick(Sender: TObject);
    procedure cxBResetChangesClick(Sender: TObject);
    procedure cxLFileNameDblClick(Sender: TObject);
  private
    FDataChanged: Boolean;
    FIScriptData: ICMSWebsiteIScriptData;
    FAdvJScriptMemoStyler: TAdvJScriptMemoStyler;

    procedure ToogleEnabledStatus(AStatus: Boolean);

    procedure UpdateDisplayFileName;

    function GetDataChanged: WordBool;
    procedure SetDataChanged(const ADataChanged: WordBool);

    function GetData: WideString;
    procedure SetData(const AData: WideString);
  public
    constructor Create(AOwner: TComponent); override;

    property DataChanged: WordBool read GetDataChanged write SetDataChanged;

    property Data: WideString read GetData write SetData;

    procedure InsertText(const AText: WideString);
    function EscapeText(const AText: string): string;

    procedure SetIScriptData(const AIScriptData: ICMSWebsiteIScriptData);

    destructor Destroy; override;
  end;

implementation

uses
  uMain;
{$R *.dfm}
{ TIScriptDesigner }

procedure TIScriptDesigner.FrameResize(Sender: TObject);
begin
  UpdateDisplayFileName;
end;

procedure TIScriptDesigner.AdvMemoChange(Sender: TObject);
begin
  if Assigned(FIScriptData) then
  begin
    FIScriptData.Code := AdvMemo.Lines.Text;
  end;
  DataChanged := True;
end;

procedure TIScriptDesigner.AdvMemoGetAutoCompletionList(Sender: TObject; AToken: string; AList: TStringList);
begin
  with AList do
  begin
    with TRegExpr.Create do
      try
        InputString := AToken;
        Expression := '.*?\.DIRECTLINK\[\w+\]\.$';

        if Exec(InputString) then
        begin
          Add('property Size: Extended');
          Add('property PartSize: Extended');
          Add('property Hoster: string');
          Add('property HosterShort: string');
          Add('property Parts: Integer');
          Add('property Value: string');
        end
        else
        begin
          Expression := '.*?\.CRYPTER\[[^\]]+\]\.$';

          if Exec(InputString) then
          begin
            Add('property Name: string');
            Add('property Size: Extended');
            Add('property PartSize: Extended');
            Add('property Hoster: string');
            Add('property HosterShort: string');
            Add('property Parts: Integer');
            Add('property Value: string');
            Add('property StatusImage: string');
            Add('property StatusImageText: string');
          end
          else
          begin
            Expression := '.*?MIRROR\[[^\]]+\]\.$';

            if Exec(InputString) then
            begin
              Add('property Size: Extended');
              Add('property PartSize: Extended');
              Add('property Hoster: string');
              Add('property HosterShort: string');
              Add('property Parts: Integer');

              Add('property Crypter: [const IndexOrName: OleVariant]: ICrypter');
              Add('property CrypterCount: Integer');

              Add('property Directlink: [const Index: Integer]: IDirectlink');
              Add('property DirectlinkCount: Integer');
            end
            else
            begin
              Expression := '.*?IMIRROR\.$';

              if Exec(InputString) then
              begin
                Add('property Mirror: [I: Integer]: TMirrorControl');
                Add('property Count: Integer');
              end;
            end
          end;
        end;

      finally
        Free;
      end;
  end;
end;

procedure TIScriptDesigner.cxBSaveClick(Sender: TObject);
var
  I: Integer;
begin
  AdvMemo.Lines.SaveToFile(FIScriptData.FileName);
  for I := 0 to AdvMemo.Lines.Count - 1 do
    if AdvMemo.LineModifiedInt[I] = lmModified then
      AdvMemo.LineModifiedInt[I] := lmSaved;

  DataChanged := False;
end;

procedure TIScriptDesigner.cxBResetChangesClick(Sender: TObject);
var
  I: Integer;
begin
  AdvMemo.Lines.LoadFromFile(FIScriptData.FileName);
  for I := 0 to AdvMemo.Lines.Count - 1 do
    AdvMemo.LineModifiedInt[I] := lmUnmodified;

  DataChanged := False;
end;

procedure TIScriptDesigner.cxLFileNameDblClick(Sender: TObject);
begin
  if Assigned(FIScriptData) then
  begin
    ShellExecute(Handle, nil, PChar(ExtractFilePath(FIScriptData.FileName)), nil, nil, SW_SHOW);
  end;
end;

procedure TIScriptDesigner.ToogleEnabledStatus(AStatus: Boolean);
begin
  AdvMemo.Enabled := AStatus;

  cxBSave.Enabled := AStatus;
  cxBResetChanges.Enabled := AStatus;
  cxBCheckScript.Enabled := AStatus;
  cxBFormatScript.Enabled := AStatus;
end;

procedure TIScriptDesigner.UpdateDisplayFileName;
var
  LFileName: TFileName;
begin
  LFileName := '';
  if Assigned(FIScriptData) then
  begin
    LFileName := FIScriptData.FileName + IfThen(DataChanged and AdvMemo.Enabled, '*');
  end;
  cxLFileName.Caption := MinimizeName(LFileName, cxLFileName.Canvas.Canvas, cxLFileName.Width - 5);
end;

function TIScriptDesigner.GetDataChanged: WordBool;
begin
  Result := FDataChanged;
end;

procedure TIScriptDesigner.SetDataChanged(const ADataChanged: WordBool);
begin
  FDataChanged := ADataChanged;
  UpdateDisplayFileName;
end;

function TIScriptDesigner.GetData: WideString;
begin
  Result := AdvMemo.Lines.Text;
end;

procedure TIScriptDesigner.SetData(const AData: WideString);
begin
  AdvMemo.Lines.Text := AData;
  DataChanged := True;
end;

constructor TIScriptDesigner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Parent := TWinControl(AOwner);

  Name := '';

  cxBSave.OptionsImage.Images := Main.ImageList;
  cxBResetChanges.OptionsImage.Images := Main.ImageList;
  cxBCheckScript.OptionsImage.Images := Main.ImageList;
  cxBFormatScript.OptionsImage.Images := Main.ImageList;

  FDataChanged := False;
  FIScriptData := nil;

  FAdvJScriptMemoStyler := TAdvJScriptMemoStyler.Create(nil);
  AdvMemo.SyntaxStyles := FAdvJScriptMemoStyler;

  ToogleEnabledStatus(False);
end;

procedure TIScriptDesigner.InsertText(const AText: WideString);
var
  LBeforeInsert, LAfterInsert, LInputString: string;
  LBeforeQuoteCount, LAfterQuoteCount, LPosition: Integer;
  LNeedFinalQuote: Boolean;

  function CharCountEx(const SubStr, s: string): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    I := 1;
    while PosEx(SubStr, s, I) > 0 do
    begin
      if I > 1 then
      begin
        if not(s[I - 1] = '\') then
          Inc(Result);
      end
      else
        Inc(Result);

      I := PosEx(SubStr, s, I) + 1;
    end;
  end;

begin
  AdvMemo.TextFromPos(AdvMemo.CurX, AdvMemo.CurY, LPosition);

  LBeforeInsert := copy(AdvMemo.Lines.Text, 1, LPosition);
  LAfterInsert := copy(AdvMemo.Lines.Text, LPosition + 1);
  LBeforeQuoteCount := CharCountEx('"', LBeforeInsert);
  LAfterQuoteCount := CharCountEx('"', LAfterInsert);
  LNeedFinalQuote := (LAfterQuoteCount mod 2) = 0;
  if (LBeforeQuoteCount mod 2) = 0 then
  begin
    LInputString := copy(LBeforeInsert, LastDelimiter('"', LBeforeInsert));

    if SameStr('"', LInputString) then
      AdvMemo.InsertText(' + "' + EscapeText(AText) + IfThen(LNeedFinalQuote, '"'))
    else
      with TRegExpr.Create do
        try
          InputString := LInputString;
          Expression := '\"([\+\s+]*)';

          if (Exec(InputString) and (Pos('+', Match[1]) > 0)) or (LBeforeQuoteCount = 0) or SameStr('', Match[1]) then
            AdvMemo.InsertText('"' + EscapeText(AText) + IfThen(LNeedFinalQuote, '"'))
          else
            AdvMemo.InsertText('+ "' + EscapeText(AText) + IfThen(LNeedFinalQuote, '"'));
        finally
          Free;
        end;
  end
  else
    AdvMemo.InsertText(EscapeText(AText) + IfThen(LNeedFinalQuote, '"'));
end;

function TIScriptDesigner.EscapeText(const AText: string): string;
begin
  Result := StringReplaceMultiple(AText, ['"', '\'], ['\"', '\\']);
end;

procedure TIScriptDesigner.SetIScriptData(const AIScriptData: ICMSWebsiteIScriptData);
var
  LFileExists: Boolean;
begin
  if not Assigned(AIScriptData) then
  begin
    FIScriptData := nil;

    ToogleEnabledStatus(False);
    AdvMemo.Lines.Clear;

    DataChanged := False;
  end
  else if not Assigned(FIScriptData) or not SameFileName(AIScriptData.FileName, FIScriptData.FileName) then
  begin
    FIScriptData := AIScriptData;

    LFileExists := FileExists(FIScriptData.FileName);

    ToogleEnabledStatus(LFileExists);

    if LFileExists then
      AdvMemo.Lines.Text := FIScriptData.Code
    else
      AdvMemo.Lines.Clear;

    DataChanged := not CompareTextByMD5(FIScriptData.Code, FIScriptData.OriginalCode);
  end;
end;

destructor TIScriptDesigner.Destroy;
begin
  FIScriptData := nil;
  FAdvJScriptMemoStyler.Free;
  inherited Destroy;
end;

end.
