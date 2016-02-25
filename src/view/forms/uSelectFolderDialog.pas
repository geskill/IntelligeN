unit uSelectFolderDialog;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ComCtrls, ShlObj,
  // Dev Express
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxShellCommon, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, cxShellTreeView, cxLabel, cxButtons, cxShellControls, cxTreeView, cxShellListView,
  // RegEx
  RegExpr;

type
  TSelectFolderDialog = class(TForm)
    cxLDescription: TcxLabel;
    cxShellTreeView: TcxShellTreeView;
    cxLFileFormat: TcxLabel;
    cxCOBFileFormat: TcxComboBox;
    cxBCancel: TcxButton;
    cxBOk: TcxButton;
    cxBNewFolder: TcxButton;
    procedure cxBNewFolderClick(Sender: TObject);
    procedure cxBCancelClick(Sender: TObject);
    procedure cxBOkClick(Sender: TObject);
  private
    FFilter: string;
    function GetDescription: string;
    procedure SetDescription(const ADescription: string);
    procedure SetFilter(const AFilter: string);
    function GetFilterIndex: Integer;
    function GetPath: string;
    procedure SetPath(const APath: string);
  public
    property Description: string read GetDescription write SetDescription;
    property Filter: string read FFilter write SetFilter;
    property FilterIndex: Integer read GetFilterIndex;
    property Path: string read GetPath write SetPath;
    function Execute: Boolean;
  end;

var
  SelectFolderDialog: TSelectFolderDialog;

implementation

{$R *.dfm}

type
  TcxShellTreeViewAccess = class(TcxShellTreeView);

procedure TSelectFolderDialog.cxBNewFolderClick(Sender: TObject);
var
  LNewFolderName, LNewFolderPath: string;
  LNewFolderRelativePidl: PItemIDList;
  LShellFolder: IShellFolder;
begin
  LNewFolderName := 'New folder';
  if InputQuery('Create New Folder', 'New Folder Name', LNewFolderName) then
  begin
    LNewFolderPath := IncludeTrailingPathDelimiter(cxShellTreeView.Path) + LNewFolderName;
    if not ForceDirectories(LNewFolderPath) then
      raise exception.createFmt('New Folder "%s" could not be created.', [LNewFolderName]);
    cxShellTreeView.UpdateContent;
    Application.ProcessMessages;
    LShellFolder := cxShellTreeView.Folders[cxShellTreeView.InnerTreeView.Selected.AbsoluteIndex].ShellFolder;
    LNewFolderRelativePidl := InternalParseDisplayName(LShellFolder, LNewFolderName, TcxShellTreeViewAccess(cxShellTreeView).GetViewOptions);
    cxShellTreeView.AbsolutePIDL := ConcatenatePidls(cxShellTreeView.AbsolutePIDL, LNewFolderRelativePidl);
    cxShellTreeView.SetFocus;
  end;
end;

procedure TSelectFolderDialog.cxBCancelClick(Sender: TObject);
begin
  CloseModal;
end;

procedure TSelectFolderDialog.cxBOkClick(Sender: TObject);
begin
  CloseModal;
end;

function TSelectFolderDialog.GetDescription;
begin
  Result := cxLDescription.Caption;
end;

procedure TSelectFolderDialog.SetDescription(const ADescription: string);
begin
  cxLDescription.Caption := ADescription;
end;

procedure TSelectFolderDialog.SetFilter(const AFilter: string);
begin
  FFilter := AFilter;

  cxCOBFileFormat.Properties.Items.Clear;
  with TRegExpr.Create do
    try
      ModifierS := False;
      InputString := FFilter;
      Expression := '(.*?)\|.*?\|';

      if Exec(InputString) then
      begin
        repeat
          cxCOBFileFormat.Properties.Items.Add(Match[1]);
        until not ExecNext;
      end;
    finally
      Free;
    end;
end;

function TSelectFolderDialog.GetFilterIndex;
begin
  Result := cxCOBFileFormat.ItemIndex + 1;
end;

function TSelectFolderDialog.GetPath;
begin
  Result := IncludeTrailingPathDelimiter(cxShellTreeView.Path);
end;

procedure TSelectFolderDialog.SetPath(const APath: string);
begin
  cxShellTreeView.Path := APath;
end;

function TSelectFolderDialog.Execute: Boolean;
begin
  if cxCOBFileFormat.Properties.Items.Count > 0 then
    cxCOBFileFormat.ItemIndex := 0;
  ShowModal;
  Result := (ModalResult = mrOK);
end;

end.
