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
  uBase,
  // Api
  uApiConst, uApiHTTP, uApiMain, uApiSettings,
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
    procedure FmiLicenseHistoryClick(Sender: TObject);
    procedure A;
    procedure B(A: string);
    function C(_: Boolean = True): string;
    function D: string;
    function E: string;
    procedure Sort;

    // Prüft Lizenz
  const
    u = 'acc/';
  public
    FmiChangePassword, FmiRenewLicense, FmiLicenseHistory: TdxBarButton;
    procedure Check; // Aktiviert/Deaktiviert Funktionen entsprechend der Lizenz
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  uMain, uSettings, uRegister, uLicenceInfo;
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
  if Now < EncodeDate(2012, 3, 1) then
    MessageDlg('Your Windows datetime is incorrect. You have to use the correct datetime for login!', mtError, [mbOK], 0)
  else
  begin

    Sort;

    cxLAccountValue.Caption := eLoginname.Text;

    Check;
  end;
end;

procedure TfLogin.cxbLogoutClick(Sender: TObject);
begin
  FbpmUserCP.Free;

  Main.V := 0;

  pLogout.Visible := False;
  pLogin.Visible := True;

  Check;
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

procedure TfLogin.FmiLicenseHistoryClick(Sender: TObject);
begin
  if not Assigned(LicenceInfo) then
    Application.CreateForm(TLicenceInfo, LicenceInfo);
  if LicenceInfo.cxGridCardView1.DataController.RecordCount = 0 then
    LicenceInfo.RefreshAccountInfo;
  LicenceInfo.Show;
end;

procedure TfLogin.A;
var
  Value: Boolean;
begin
  Value := (length(eLoginname.Text) > 1) and (length(eLoginpassword.Text) > 1);

  cbLoginSaveData.Enabled := Value;
  cxbLogin.Enabled := Value;
end;

procedure TfLogin.B;
var
  _FileStream: TFileStream;
  _buffer, _b: string;
  XMLDoc: IXMLDocument;
begin
  _b := THash_MD5.CalcBinary(THash_MD5.CalcBinary(THash_MD5.CalcBinary(DateTimeToStr(Now), TFormat_HEXL), TFormat_HEXL), TFormat_HEXL);
  _FileStream := TFileStream.Create(ExtractFilePath(ParamStr(0)) + '.licence', fmCreate);
  with _FileStream do
    try
      XMLDoc := NewXMLDocument;
      with XMLDoc do
      begin
        Options := Options + [doNodeAutoIndent];
        DocumentElement := CreateElement('xml', '');
        NodeIndentStr := #9;
        Active := True;
        try
          with DocumentElement do
          begin
            with AddChild('datetime') do
            begin
              NodeValue := DateTimeToStr(Now);
              Attributes['d'] := copy(_b, 25, 8);
            end;
            with AddChild('latetime') do
              NodeValue := A;
            with AddChild('msg') do
              Attributes['m'] := copy(_b, 1, 8);
            with AddChild('status') do
            begin
              NodeValue := Main.V;
              Attributes['s'] := copy(_b, 9, 8);
            end;
            with AddChild('code') do
            begin
              NodeValue := 1;
              Attributes['c'] := copy(_b, 17, 8);
            end;
          end;
          SaveToXML(_buffer);
        finally
          XMLDoc := nil;
        end;
      end;

      with TCipher_Rijndael.Create do
        try
          Mode := cmCFB8;
          Init(D, 'FFFFFFFFFF');
          _buffer := EncodeBinary(_buffer, TFormat_Copy);
        finally
          Free;
        end;

      _FileStream.Position := 0;
      with TStringStream.Create('') do
        try
          WriteString(_buffer);
          SaveToStream(_FileStream);
        finally
          Free;
        end;

    finally
      Free;
    end;
end;

function TfLogin.C;
var
  _FileStream: TFileStream;
  _buffer: string;
  XMLDoc: IXMLDocument;
begin
  Result := '';
  _FileStream := TFileStream.Create(ExtractFilePath(ParamStr(0)) + '.licence', fmShareDenyNone);
  with _FileStream do
    try
      _FileStream.Position := 0;
      with TStringStream.Create('') do
        try
          LoadFromStream(_FileStream);
          _buffer := DataString;
        finally
          Free;
        end;

      with TCipher_Rijndael.Create do
        try
          Mode := cmCFB8;
          Init(D, 'FFFFFFFFFF');
          _buffer := DecodeBinary(_buffer, TFormat_Copy);
        finally
          Free;
        end;

      if not(Pos('xml', _buffer) = 0) then
      begin
        XMLDoc := NewXMLDocument;
        with XMLDoc do
        begin
          LoadFromXML(_buffer);
          Active := True;
          try
            case _ of
              True:
                if (CompareDateTime(Now - StrToDateTimeDef(DocumentElement.ChildNodes.Nodes['datetime'].NodeValue, Now), OneHour * 24 * 5) = GreaterThanValue)
                  then
                  Exit;
              False:
                if (CompareDateTime(Now - StrToDateTimeDef(DocumentElement.ChildNodes.Nodes['datetime'].NodeValue, Now), OneMinute * 15) = GreaterThanValue)
                  then
                  Exit;
            end;

          finally
            XMLDoc := nil;
          end;
        end;

        Result := _buffer;
      end;
    finally
      Free;
    end;
end;

function TfLogin.D;
begin
  Result := THash_MD5.CalcBinary(THash_MD5.CalcBinary(GetVolumeID, TFormat_HEXL) + GetMD5FromFile(ExtractFilePath(ParamStr(0)) + 'framework.bpl'),
    TFormat_HEXL);
end;

function TfLogin.E: string;
var
  _buffer: string;
  XMLDoc: IXMLDocument;
begin
  Result := '';
  _buffer := C;

  if not(Pos('xml', _buffer) = 0) then
    try
      XMLDoc := NewXMLDocument;
      try
        with XMLDoc do
        begin
          LoadFromXML(_buffer);
          Active := True;

          with DocumentElement.ChildNodes do
            if IndexOf('latetime') <> -1 then
              Result := Nodes['latetime'].NodeValue;
        end;
      finally
        XMLDoc := nil;
      end;
    except

    end;
end;

procedure TfLogin.Sort;
var
  Params: TIdMultiPartFormDataStream;
  ReplyData: TStringStream;
  XMLDoc: IXMLDocument;
  // _FormatSettings: TFormatSettings;
  _password, _hashedpassword, _username, _errormsg, _e, _x, _y, _z: string;
  _i, _j, _k: Integer;
  _did_c: Boolean;
begin
  _password := eLoginname.Text;
  _hashedpassword := eLoginpassword.Text;

  _did_c := False;

  _i := -1;
  _k := _i;
  _i := _k;

  // START CRACK DETECTION
  if Now < EncodeDate(2012, 1, 31) then
    Halt;
  // END CRACK DETECTION

  with TApiHTTP.CreateAccounting do
    try
      Params := TIdMultiPartFormDataStream.Create;
      try
        Params.AddFormField('action', 'login_v1', 'UTF-8');
        with TCipher_Rijndael.Create do
          try
            Mode := cmCFB8;
            Init(THash_MD5.CalcBinary(THash_SHA512.CalcBinary(_hashedpassword, TFormat_MIME64), TFormat_HEXL));
            _username := EncodeBinary(_password, TFormat_MIME64);
          finally
            Free;
          end;
        with TCipher_Rijndael.Create do
          try
            Mode := cmCFB8;
            Init(THash_MD5.CalcBinary(_password, TFormat_HEXL));
            _x := EncodeBinary(GetWindowsUserName, TFormat_MIME64);
          finally
            Free;
          end;
        with TCipher_Rijndael.Create do
          try
            Mode := cmCFB8;
            Init(THash_MD5.CalcBinary(GetWindowsUserName, TFormat_HEXL));
            _y := EncodeBinary(GetWindowsComputerName, TFormat_MIME64);
          finally
            Free;
          end;
        with TCipher_Rijndael.Create do
          try
            Mode := cmCFB8;
            Init(THash_MD5.CalcBinary(GetWindowsComputerName, TFormat_HEXL));
            _z := EncodeBinary(GetMACAdress, TFormat_MIME64);
          finally
            Free;
          end;

        ReplyData := TStringStream.Create('', CP_UTF8);
        try
          with Params do
          begin
            AddFormField('username', THash_SHA512.CalcBinary(_hashedpassword, TFormat_MIME64), 'UTF-8').ContentTransfer := 'binary';
            AddFormField('password', _username, 'UTF-8').ContentTransfer := 'binary';

            AddFormField('v', MINOR_VERSION, 'UTF-8').ContentTransfer := 'binary';
            AddFormField('x', _y, 'UTF-8').ContentTransfer := 'binary';
            AddFormField('y', _z, 'UTF-8').ContentTransfer := 'binary';
            AddFormField('z', _x, 'UTF-8').ContentTransfer := 'binary';

            Position := 0;
          end;

          XMLDoc := NewXMLDocument;
          with XMLDoc do
          begin
            if FileExists(ExtractFilePath(ParamStr(0)) + '.licence') and not SameStr('', C(False)) then
            begin
              if _did_c then
                Exit;
              _did_c := True;
              ReplyData.Clear;
              if SameStr('', C(False)) then
                Halt;
              ReplyData.WriteString(C);
            end
            else
            begin
              try
                Post(Homepage + u + copy(u, 2, 1) + '.php', Params, ReplyData);
              except
                on E: Exception do
                begin
                  try
                    Post('http://geskill.bplaced.net/intelligen2009/' + u + copy(u, 2, 1) + '.php', Params, ReplyData);
                  except
                    on E: Exception do
                    begin
                      if FileExists(ExtractFilePath(ParamStr(0)) + '.licence') then
                      begin
                        if _did_c then
                          Exit;
                        _did_c := True;
                        ReplyData.Clear;
                        ReplyData.WriteString(C);
                      end
                      else
                      begin
                        Main.V := 0;
                        Exit;
                      end;
                    end;
                  end;
                end;
              end;
            end;
            // START CRACK DETECTION
            if (Pos('Win32', Response.Server) > 0) then
              Exit;
            // END CRACK DETECTION
            if ReplyData.DataString = '' then
              Exit;
            // START CRACK DETECTION
            if not FileExists(ExtractFilePath(ParamStr(0)) + '.licence') and _did_c then
              Halt;
            // END CRACK DETECTION
            try
              LoadFromXML(ReplyData.DataString);
              Active := True;
            except
              on E: Exception do
              begin
                Main.V := 0;
                Exit;
              end;
            end;
            // START CRACK DETECTION
            if (Pos('Win32', Response.Server) > 0) and (Random(25) = 14) then
              Halt;
            // END CRACK DETECTION
          end;
          with XMLDoc.DocumentElement do
            if HasChildNodes then
            begin
              with ChildNodes.Nodes['datetime'] do
              begin
                _e := THash_MD5.CalcBinary(THash_MD5.CalcBinary(THash_MD5.CalcBinary(NodeValue, TFormat_HEXL), TFormat_HEXL), TFormat_HEXL);
                // START CRACK DETECTION
                if not SameText(Attributes['d'], copy(_e, 25, 8)) and (DayOfTheWeek(Now) = 5) then
                  Exit;
                // END CRACK DETECTION
              end;
              // START CRACK DETECTION
              if FileExists(ExtractFilePath(ParamStr(0)) + '.licence') and not _did_c then
                if SameText(_e, E) then
                  Exit;
              // END CRACK DETECTION
              with ChildNodes.Nodes['msg'] do
              begin
                _errormsg := VarToStr(NodeValue);
                // START CRACK DETECTION
                if not SameText(Attributes['m'], copy(_e, 1, 8)) and (DayOfTheWeek(Now) = 2) then
                  Exit;
                // END CRACK DETECTION
              end;
              with ChildNodes.Nodes['status'] do
              begin
                // START CRACK DETECTION
                if (Random(4) = 3) then
                  _i := NodeValue;

                _j := NodeValue;
                // END CRACK DETECTION
                // START CRACK DETECTION
                if not SameText(Attributes['s'], copy(_e, 9, 8)) and (Random(15) = 3) then
                  Exit;
                // END CRACK DETECTION
                Main.V := NodeValue;
                // START CRACK DETECTION
                _k := NodeValue;
                // END CRACK DETECTION
              end;
              with ChildNodes.Nodes['code'] do
              begin
                case NodeValue of
                  0:
                    if (Main.V = 0) then
                      MessageDlg('Your username or password is invalid', mtError, [mbOK], 0);
                  1:
                    if (Main.V = 0) then
                      MessageDlg('Your account awaits activation (check your mails)', mtError, [mbOK], 0);
                  2:
                    if (Main.V = 0) then
                      MessageDlg('Your account has been expired', mtError, [mbOK], 0);
                  255:
                    begin
                      if (Main.V = 0) then
                        MessageDlg(_errormsg, mtError, [mbOK], 0)
                      else
                        MessageDlg(_errormsg, mtWarning, [mbOK], 0);
                    end;
                end;
                // START CRACK DETECTION
                if not SameText(Attributes['c'], copy(_e, 17, 8)) then
                  Halt;
                // END CRACK DETECTION
                // START CRACK DETECTION
                if FileExists(ExtractFilePath(ParamStr(0)) + '.licence') and not _did_c then
                  if SameText(_e, E) and (DayOfTheWeek(Now) = 6) then
                    Halt;
                // END CRACK DETECTION
              end;
              // START CRACK DETECTION
              if not(_j = Main.V) then
                Halt;
              // END CRACK DETECTION
            end;
        finally
          ReplyData.Free;
        end;
      finally
        Params.Free;
      end;
      // START CRACK DETECTION
      if (Pos('Win32', Response.Server) > 0) and (DayOfTheWeek(Now) = 1) then
        Halt;
      // END CRACK DETECTION
    finally
      Free;
    end;

  if (Main.V = 0) then

  else
  begin
    // START CRACK DETECTION
    if (Main.V = 0) then
      Halt;
    // END CRACK DETECTION
    if not _did_c then
      B(_e);

    pLogin.Visible := False;
    // START CRACK DETECTION
    if not(_k = Main.V) then
      Halt;
    // END CRACK DETECTION
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
    // START CRACK DETECTION
    if not(_i = -1) and not(_i = Main.V) then
      Halt;
    // END CRACK DETECTION
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
    FmiLicenseHistory := TdxBarButton.Create(FbpmUserCP);
    with FmiLicenseHistory do
    begin
      Caption := 'licence history';
      ImageIndex := 21;
      OnClick := FmiLicenseHistoryClick;
    end;
    // START CRACK DETECTION
    if Now < EncodeDate(2011, 11, 24) then
      Halt;
    // END CRACK DETECTION
    with FbpmUserCP.ItemLinks.Add do
      Item := FmiLicenseHistory;
    // START CRACK DETECTION
    if (Main.V = 0) and (Random(20) = 16) then
      Halt;
    // END CRACK DETECTION
  end;
end;

procedure TfLogin.Check;
var
  I, J: Integer;
  X: Boolean;
begin
  with Main do
  begin
    X := (Main.V < 1);
    Caption := ProgrammName + ' ' + ProgrammVersion[Min(length(ProgrammVersion) - 1, V)];

    nSeries.Enabled := (V > 0);

    aWindowDatabase.Visible := (V > 1);
    dxDPDatabase.Visible := (V > 1);
    Settings.cxTSDatabase.TabVisible := (V > 1);

    if (V > 0) then
    begin
      nSeries.Visible := ivAlways;
      nSeriesAutoCompletion.Visible := ivAlways;
      nSeriesCrypterCrypt.Visible := ivAlways;
      nSeriesCrypterCheck.Visible := ivAlways;
    end
    else
    begin
      nSeries.Visible := ivNever;
      nSeriesAutoCompletion.Visible := ivNever;
      nSeriesCrypterCrypt.Visible := ivNever;
      nSeriesCrypterCheck.Visible := ivNever;
    end;

    with SettingsManager.Settings.Plugins do
    begin
      with CMS do
      begin
        if (Main.V < 1) then
        begin
          J := 0;
          for I := 0 to Count - 1 do
          begin
            if (J >= 2) and TPlugInCollectionItem(Items[I]).Enabled then
            begin
              TPlugInCollectionItem(Items[I]).Enabled := False;
              TPlugInCollectionItem(Items[I]).PreEnabled := True;
            end;
            if TPlugInCollectionItem(Items[I]).Enabled then
              Inc(J);
          end;
        end
        else
        begin
          // START CRACK DETECTION
          if (Main.V < 1) and (DayOfTheWeek(Now) = 7) then
            Exit;
          // END CRACK DETECTION
          for I := 0 to Count - 1 do
            with TPlugInCollectionItem(Items[I]) do
              Enabled := Enabled or PreEnabled;
          // START CRACK DETECTION
          if (Main.V = 0) and (DayOfTheWeek(Now) = 2) then
            Halt;
          // END CRACK DETECTION
        end;
        // Main.fPublish.GenerateColumns;
      end;
      with Crawler do
      begin
        if (Main.V < 1) then
        begin
          J := 0;
          for I := 0 to Count - 1 do
          begin
            if (J >= 3) and TPlugInCollectionItem(Items[I]).Enabled then
            begin
              TPlugInCollectionItem(Items[I]).Enabled := False;
              TPlugInCollectionItem(Items[I]).PreEnabled := True;
            end;
            if TPlugInCollectionItem(Items[I]).Enabled then
              Inc(J);
          end;
        end
        else
        begin
          for I := 0 to Count - 1 do
            with TPlugInCollectionItem(Items[I]) do
              Enabled := Enabled or PreEnabled;
          // START CRACK DETECTION
          if (Main.V < 1) and (DayOfTheWeek(Now) = 5) then
            Halt;
          // END CRACK DETECTION
        end;
      end;
      with Crypter do
      begin
        if (Main.V < 1) then
        begin
          J := 0;
          for I := 0 to Count - 1 do
          begin
            if (J >= 1) and TPlugInCollectionItem(Items[I]).Enabled then
            begin
              TPlugInCollectionItem(Items[I]).Enabled := False;
              TPlugInCollectionItem(Items[I]).PreEnabled := True;
            end;
            if TPlugInCollectionItem(Items[I]).Enabled then
              Inc(J);
          end;
        end
        else
        begin
          // START CRACK DETECTION
          if (Main.V < 1) and (DayOfTheWeek(Now) = 3) then
            Halt;
          // END CRACK DETECTION
          for I := 0 to Count - 1 do
            with TPlugInCollectionItem(Items[I]) do
              Enabled := Enabled or PreEnabled;
        end;
      end;
    end;
    Settings.SetComponentStatusFromSettings(True);
  end;
  // START CRACK DETECTION
  if X and (Main.V >= 1) then
    Halt;
  // END CRACK DETECTION
end;

constructor TfLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // START CRACK DETECTION
  if not(Main.V = 0) then
  begin
    Beep;
    Sleep(100);
    Beep;
    Sleep(100);
    Beep;
    Sleep(100);
    Beep;
    Sleep(1000);
  end;
  // END CRACK DETECTION
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

  // START CRACK DETECTION
  if not(Main.V = 0) then
    Halt;
  // END CRACK DETECTION
  A;
  // START CRACK DETECTION
  if (Main.V > 0) and (DayOfTheWeek(Now) = 6) then
    Halt;
  // END CRACK DETECTION
end;

end.
