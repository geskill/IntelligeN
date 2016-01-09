unit ufLogin;

interface

uses
  // Delphi
  Windows, SysUtils, Forms, Menus, Controls, StdCtrls, Classes, ExtCtrls, ShellAPI, DateUtils,
  IdMultipartFormData, DECCipher, DECHash, DECFmt, XMLDoc, XMLIntf, Variants, Dialogs, Types, Math,
  // Dev Express
  cxButtons, cxLookAndFeelPainters, cxCheckBox, cxControls, cxContainer, cxEdit, cxTextEdit, cxGraphics,
  cxLookAndFeels, cxLabel, dxBar,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiConst, uApiMain,
  // Utils
  uFileUtils;

type
  TfLogin = class(TFrame)
    pLogout: TPanel;
    caLAccount: TcxLabel;
    cxLAccountValue: TcxLabel;
    cxbUserCP: TcxButton;
    cxbLogout: TcxButton;
    pLogin: TPanel;
    cxbLogin: TcxButton;
    cxbRegister: TcxButton;
    eLoginname: TcxTextEdit;
    eLoginpassword: TcxTextEdit;
    cbLoginSaveData: TcxCheckBox;
    cbLoginAutoLogin: TcxCheckBox;
    pPassword: TPanel;
    cxLOldPassword: TcxLabel;
    cxTEOldPassword: TcxTextEdit;
    cxLNewPassword: TcxLabel;
    cxTENewPassword: TcxTextEdit;
    cxBCancelPassword: TcxButton;
    cxBChangePassword: TcxButton;
    cxTENewPassword2: TcxTextEdit;
    cxLNewPassword2: TcxLabel;
    procedure ActivateSaveData(Sender: TObject);
    procedure cbLoginSaveDataClick(Sender: TObject);
    procedure cbLoginAutoLoginClick(Sender: TObject);
    procedure cxbRegisterClick(Sender: TObject);
    procedure cxbLoginClick(Sender: TObject);
    procedure cxbLogoutClick(Sender: TObject);
    procedure cxbUserCPMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cxTEOldPasswordPropertiesChange(Sender: TObject);
    procedure cxTENewPasswordPropertiesChange(Sender: TObject);
    procedure cxBCancelPasswordClick(Sender: TObject);
    procedure cxBChangePasswordClick(Sender: TObject);
  private
    FbpmUserCP: TdxBarPopupMenu;
    procedure bpmUserCPPopup(Sender: TObject);
    procedure FmiChangePasswordClick(Sender: TObject);
    procedure FmiRenewLicenseClick(Sender: TObject);
    procedure A;
  public
    FmiChangePassword, FmiRenewLicense: TdxBarButton;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  uMain, uSettings, uRegister,
  // Api
  uApiSettings;

{$R *.dfm}

procedure TfLogin.ActivateSaveData(Sender: TObject);
begin
  with SettingsManager.Settings.Login do
  begin
    AccountName := eLoginname.Text;
    if SaveLoginData then
      AccountPassword := eLoginpassword.Text;
  end;

  if not((length(eLoginname.Text) > 1) and (length(eLoginpassword.Text) > 1)) then
    cbLoginSaveData.Checked := False;
  A;
end;

procedure TfLogin.cbLoginSaveDataClick(Sender: TObject);
begin
  with SettingsManager.Settings.Login do
  begin
    SaveLoginData := cbLoginSaveData.Checked;

    AccountName := eLoginname.Text;
    if SaveLoginData then
      AccountPassword := eLoginpassword.Text;
  end;

  if not cbLoginSaveData.Checked then
    cbLoginAutoLogin.Checked := False;
  cbLoginAutoLogin.Enabled := cbLoginSaveData.Checked;
end;

procedure TfLogin.cbLoginAutoLoginClick(Sender: TObject);
begin
  SettingsManager.Settings.Login.AutoLogin := cbLoginAutoLogin.Checked;
end;

procedure TfLogin.cxbRegisterClick(Sender: TObject);
begin
  if not Assigned(register) then
    Application.CreateForm(TRegister, register);
  register.Show;
end;

procedure TfLogin.cxbLoginClick(Sender: TObject);
begin
  if Now < EncodeDate(2016, 1, 1) then
    MessageDlg('Your Windows datetime is incorrect. You have to use the correct datetime for login!', mtError, [mbOK], 0)
  else
  begin
    pLogin.Visible := False;

    pLogout.Visible := True;

    FbpmUserCP := TdxBarPopupMenu.Create(Main);
    with FbpmUserCP do
      OnPopup := bpmUserCPPopup;
    FmiChangePassword := TdxBarButton.Create(FbpmUserCP);
    with FmiChangePassword do
    begin
      Caption := 'change password';
      ImageIndex := 19;
      OnClick := FmiChangePasswordClick;
    end;

    with FbpmUserCP.ItemLinks.Add do
      Item := FmiChangePassword;
    FmiRenewLicense := TdxBarButton.Create(FbpmUserCP);
    with FmiRenewLicense do
    begin
      Caption := 'renew licence';
      ImageIndex := 20;
      OnClick := FmiRenewLicenseClick;
    end;
    with FbpmUserCP.ItemLinks.Add do
      Item := FmiRenewLicense;

    cxLAccountValue.Caption := eLoginname.Text;
  end;
end;

procedure TfLogin.cxbLogoutClick(Sender: TObject);
begin
  FbpmUserCP.Free;

  pLogout.Visible := False;
  pLogin.Visible := True;
end;

procedure TfLogin.cxbUserCPMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  cxDropDownButtonWidth = 15;
var
  R: TRect;
  P: TPoint;
  ABounds: TRect;
begin
  if mbLeft = Button then
  begin
    with TcxButton(Sender) do
    begin
      R := ClientRect;
      R.Left := R.Right - cxDropDownButtonWidth;
      // if PtInRect(R, Point(X, Y)) then
      // begin
      P := Parent.ClientToScreen(BoundsRect.BottomRight);
      ABounds := BoundsRect;
      FbpmUserCP.PopupEx(P.X, P.Y, Width, Height, False, @ABounds);
      // end;
    end;
  end;
end;

procedure TfLogin.cxTEOldPasswordPropertiesChange(Sender: TObject);
begin
  cxBChangePassword.Enabled := (length(cxTEOldPassword.Text) >= 6);
end;

procedure TfLogin.cxTENewPasswordPropertiesChange(Sender: TObject);
begin
  cxBChangePassword.Enabled := (length(cxTENewPassword.Text) >= 6) and (length(cxTENewPassword2.Text) >= 6);
end;

procedure TfLogin.cxBCancelPasswordClick(Sender: TObject);
begin
  if (cxBChangePassword.Caption = 'OK') then
  begin
    cxBCancelPassword.Caption := 'Cancel';
    cxBChangePassword.Caption := 'Next';
    cxLNewPassword.Visible := False;
    cxTENewPassword.Visible := False;
    cxLNewPassword2.Visible := False;
    cxTENewPassword2.Visible := False;
    cxLOldPassword.Visible := True;
    cxTEOldPassword.Visible := True;
    cxTEOldPassword.SetFocus;
    cxBChangePassword.Enabled := (length(cxTEOldPassword.Text) >= 6);
  end
  else
  begin
    pPassword.Visible := False;
    pLogout.Visible := True;
  end;
end;

procedure TfLogin.cxBChangePasswordClick(Sender: TObject);
var
  Params: TIdMultiPartFormDataStream;
  ReplyData: TStringStream;
  XMLDoc: IXMLDocument;
  _a, _b, _errormsg: string;
begin

  if (cxBChangePassword.Caption = 'OK') then
  begin
    if not(cxTENewPassword.Text = cxTENewPassword2.Text) then
    begin
      MessageDlg('Your new passwords does not match', mtError, [mbOK], 0);
      Exit;
    end;
    _a := THash_SHA512.CalcBinary(eLoginpassword.Text, TFormat_MIME64);
    _errormsg := 'Unknown error';
    with TCipher_Rijndael.Create do
      try
        Mode := cmCFB8;
        Init(THash_MD5.CalcBinary(_a, TFormat_HEXL));
        _b := EncodeBinary(cxTENewPassword.Text, TFormat_MIME64);
      finally
        Free;
      end;
    {
      with TApiHTTP.CreateAccounting do
      try
      Params := TIdMultiPartFormDataStream.Create;
      try
      Params.AddFormField('action', 'password_v1', 'utf-8');

      Params.AddObject('username', '', '', TStringStream.Create(eLoginname.Text, TEncoding.UTF8));
      Params.AddObject('password', '', '', TStringStream.Create(_a, TEncoding.UTF8));
      Params.AddObject('checksum', '', '', TStringStream.Create(_b, TEncoding.UTF8));

      Params.Position := 0;

      ReplyData := TStringStream.Create('', CP_UTF8);
      try
      XMLDoc := NewXMLDocument;
      with XMLDoc do
      begin
      try
      Post(Homepage + u + copy(u, 2, 1) + '.php', Params, ReplyData);
      // Post('http://geskill.bplaced.net/intelligen2009/' + u + copy(u, 2, 1) + '.php', Params, ReplyData);
      except
      on E: Exception do
      begin
      try
      Post('http://geskill.bplaced.net/intelligen2009/' + u + copy(u, 2, 1) + '.php', Params, ReplyData);
      except
      on E: Exception do
      begin
      MessageDlg(_errormsg, mtError, [mbOK], 0);
      Exit;
      end;
      end;
      end;
      end;
      if ReplyData.DataString = '' then
      Exit;
      try
      LoadFromXML(ReplyData.DataString);
      Active := True;
      except
      on E: Exception do
      begin
      MessageDlg(_errormsg, mtError, [mbOK], 0);
      Exit;
      end;
      end;
      end;
      with XMLDoc.DocumentElement do
      if HasChildNodes then
      begin
      with ChildNodes.Nodes['msg'] do
      _errormsg := VarToStr(NodeValue);
      with ChildNodes.Nodes['code'] do
      begin
      case NodeValue of
      0:
      MessageDlg('Your current password is incorrect', mtError, [mbOK], 0);
      1:
      begin
      eLoginpassword.Text := cxTENewPassword.Text;
      MessageDlg('Your password has been changed', mtInformation, [mbOK], 0);
      cxBCancelPassword.Caption := 'Cancel';
      cxBChangePassword.Caption := 'Next';
      cxLNewPassword.Visible := False;
      cxTENewPassword.Visible := False;
      cxLNewPassword2.Visible := False;
      cxTENewPassword2.Visible := False;
      cxLOldPassword.Visible := True;
      cxTEOldPassword.Visible := True;
      pPassword.Visible := False;
      pLogout.Visible := True;
      end;
      255:
      MessageDlg(_errormsg, mtError, [mbOK], 0);
      end;
      end;
      end;
      finally
      ReplyData.Free;
      end;
      finally
      Params.Free;
      end;
      finally
      Free;
      end;
      }
  end
  else
  begin
    if not(eLoginpassword.Text = cxTEOldPassword.Text) then
      MessageDlg('Your current password is incorrect', mtError, [mbOK], 0)
    else
    begin
      cxLOldPassword.Visible := False;
      cxTEOldPassword.Visible := False;
      cxBChangePassword.Enabled := False;
      cxBChangePassword.Caption := 'OK';
      cxBCancelPassword.Caption := 'Back';
      cxLNewPassword.Visible := True;
      cxTENewPassword.Visible := True;
      cxTENewPassword.Clear;
      cxLNewPassword2.Visible := True;
      cxTENewPassword2.Visible := True;
      cxTENewPassword2.Clear;
      cxTENewPassword.SetFocus;
    end;
  end;
end;

procedure TfLogin.bpmUserCPPopup(Sender: TObject);
var
  R: TRect;
  P: TPoint;
begin
  R := GetWorkArea(Point(0, 0));
  with TdxBarPopupMenu(Sender).SubMenuControl do
  begin
    HandleNeeded;
    with cxbUserCP do
      P := Parent.ClientToScreen(BoundsRect.BottomRight);
    if (P.X - Width > 0) then
      Left := P.X - cxbUserCP.Width;
  end;
end;

procedure TfLogin.FmiChangePasswordClick(Sender: TObject);
begin
  pLogout.Visible := False;
  cxTEOldPassword.Clear;
  cxTENewPassword.Clear;
  pPassword.Visible := True;
  cxTEOldPassword.SetFocus;
end;

procedure TfLogin.FmiRenewLicenseClick(Sender: TObject);
begin
  if not Assigned(register) then
    Application.CreateForm(TRegister, register);
  register.Show;
end;

procedure TfLogin.A;
var
  Value: Boolean;
begin
  Value := (length(eLoginname.Text) > 1) and (length(eLoginpassword.Text) > 1);

  cbLoginSaveData.Enabled := Value;
  cxbLogin.Enabled := Value;
end;

constructor TfLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  pLogin.DoubleBuffered := True;

  with SettingsManager.Settings.Login do
  begin
    eLoginname.Text := AccountName;
    if SaveLoginData then
      eLoginpassword.Text := AccountPassword;

    cbLoginSaveData.Checked := SaveLoginData;
    cbLoginAutoLogin.Enabled := SaveLoginData;
    cbLoginAutoLogin.Checked := AutoLogin;
  end;

  A;
end;

end.
