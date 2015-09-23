unit ufLinklist;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ShellAPI, ComCtrls, Menus,
  ImgList,
  // Dev Express
  cxControls, cxListView, cxContainer, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters,
  // Api
  uApiConst;

type
  TfLinklist = class(TFrame)
    lfLinklist: TcxListView;
    pmAddLink: TPopupMenu;
    nAddLink: TMenuItem;
    nEditLink: TMenuItem;
    nRemoveLink: TMenuItem;
    procedure FrameResize(Sender: TObject);
    procedure lfLinklistClick(Sender: TObject);
    procedure lfLinklistMouseLeave(Sender: TObject);
    procedure pmAddLinkPopup(Sender: TObject);
    procedure nAddLinkClick(Sender: TObject);
    procedure nEditLinkClick(Sender: TObject);
    procedure nRemoveLinkClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
  end;

implementation

{$R *.dfm}

procedure TfLinklist.FrameResize(Sender: TObject);
begin
  lfLinklist.Column[0].Width := lfLinklist.Width - 2;
end;

procedure TfLinklist.lfLinklistClick(Sender: TObject);
begin
  if (lfLinklist.SelCount = 1) then
    ShellExecute(Handle, 'open', PChar(lfLinklist.Selected.Caption), nil, nil, SW_SHOW);
end;

procedure TfLinklist.lfLinklistMouseLeave(Sender: TObject);
var
  p: TPoint;
begin
  p := Mouse.CursorPos;
  p := lfLinklist.ScreenToClient(p);
  if (lfLinklist.GetItemAt(p.X, p.Y) = nil) then
    lfLinklist.Selected := nil;
end;

procedure TfLinklist.pmAddLinkPopup(Sender: TObject);
var
  Enabled: Boolean;
begin
  Enabled := (lfLinklist.SelCount = 1);

  nEditLink.Enabled := Enabled;
  nRemoveLink.Enabled := Enabled;
end;

procedure TfLinklist.nAddLinkClick(Sender: TObject);
var
  newLink: string;
begin
  newLink := '';
  if InputQuery('<!>', '<!>', newLink) and (Pos(http, newLink) > 0) then
    with lfLinklist.Items.Add do
      Caption := newLink;
end;

procedure TfLinklist.nEditLinkClick(Sender: TObject);
var
  Link: string;
begin
  Link := lfLinklist.Selected.Caption;
  if InputQuery('<!>', '<!>', Link) and (Pos(http, Link) > 0) then
    lfLinklist.Selected.Caption := Link;
end;

procedure TfLinklist.nRemoveLinkClick(Sender: TObject);
begin
  lfLinklist.Selected.Free;
end;

end.
