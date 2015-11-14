unit uRegister;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ShellAPI, Dialogs, XMLDoc, XMLIntf,
  // Dev Express
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, Menus, StdCtrls, cxButtons, cxRadioGroup, ExtCtrls, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxLabel,
  // Api
  uApiConst;

type
  TRegister = class(TForm)
    pDo: TPanel;
    pBuy: TPanel;
    pActivate: TPanel;
    cxRBActivate: TcxRadioButton;
    cxBDoNext: TcxButton;
    cxRBPayPal: TcxRadioButton;
    cxTEActivationKey: TcxTextEdit;
    cxTEEMailAddress: TcxTextEdit;
    cxTEAccountname: TcxTextEdit;
    cxTEPassword_A: TcxTextEdit;
    cxTEPassword_B: TcxTextEdit;
    cxRBBuy: TcxRadioButton;
    cxBBack: TcxButton;
    cxRBNewAccount: TcxRadioButton;
    cxRBExistingAccount: TcxRadioButton;
    pMonths: TPanel;
    cxRB2Months: TcxRadioButton;
    cxRB6Months: TcxRadioButton;
    cxLActivationKey: TcxLabel;
    cxLEMailAddress: TcxLabel;
    cxLAccountname: TcxLabel;
    cxLPassword_A: TcxLabel;
    cxLPassword_B: TcxLabel;
    cxRB12Months: TcxRadioButton;
    cxRBAlertPay: TcxRadioButton;
    procedure cxBBackClick(Sender: TObject);
    procedure cxBDoNextClick(Sender: TObject);
    procedure cxRBNewAccountClick(Sender: TObject);
    procedure cxRBExistingAccountClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  register: TRegister;

implementation

uses
  // Api
  uApiSettings;

{$R *.dfm}

procedure TRegister.cxBBackClick(Sender: TObject);
begin
  if pMonths.Visible then
  begin
    pMonths.Visible := False;
    pDo.Visible := True;
    cxBBack.Enabled := False;
  end
  else if pBuy.Visible then
  begin
    pBuy.Visible := False;
    pMonths.Visible := True;
    cxBDoNext.Caption := 'Next';
  end
  else if pActivate.Visible then
  begin
    pActivate.Visible := False;
    pDo.Visible := True;
    cxBDoNext.Caption := 'Next';
    cxBBack.Enabled := False;
  end
end;

procedure TRegister.cxBDoNextClick(Sender: TObject);
const
  paypalurl = 'https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=HJGRSGGSULBHN&os0=';
var
  // Params: TIdMultiPartFormDataStream;
  ReplyData: TStringStream;
  XMLDoc: IXMLDocument;
  _errormsg: string;
begin
  if pDo.Visible then
  begin
    if cxRBBuy.Checked then
    begin
      pMonths.Visible := True;
      cxBBack.Enabled := True;
    end
    else if cxRBActivate.Checked then
    begin
      pActivate.Visible := True;
      cxBDoNext.Caption := 'Activate';
      cxBBack.Enabled := True;
    end;
    pDo.Visible := False;
  end
  else if pMonths.Visible then
  begin
    pMonths.Visible := False;
    cxBDoNext.Caption := 'Buy';
    pBuy.Visible := True;
  end
  else if pBuy.Visible then
  begin
    if cxRBPayPal.Checked then
      if cxRB2Months.Checked then
        ShellExecute(Handle, 'open', paypalurl + '2%20months', nil, nil, SW_SHOW)
      else if cxRB6Months.Checked then
        ShellExecute(Handle, 'open', paypalurl + '6%20months', nil, nil, SW_SHOW)
      else if cxRB12Months.Checked then
        ShellExecute(Handle, 'open', paypalurl + '12%20months', nil, nil, SW_SHOW);

    pBuy.Visible := False;
    pDo.Visible := True;
    cxBDoNext.Caption := 'Next';
    cxBBack.Enabled := False;
    cxRBActivate.Checked := True;
  end
  else if pActivate.Visible then
  begin
    _errormsg := '';
    if (cxTEActivationKey.Text = '') then
      _errormsg := 'Enter your activation key'
    else if (length(cxTEActivationKey.Text) < 6) then
      _errormsg := 'Your activation key seems invalid'
    else if (cxTEEMailAddress.Text = '') then
      _errormsg := 'Enter your E-Mail-Address'
    else if (Pos('@', cxTEEMailAddress.Text) = 0) or (length(cxTEEMailAddress.Text) < 5) then
      _errormsg := 'Your E-Mail-Address seems invalid'
    else if (cxTEAccountname.Text = '') then
      _errormsg := 'Enter your new account name'
    else if (length(cxTEAccountname.Text) < 2) then
      _errormsg := 'Your account name is to short';

    if cxRBNewAccount.Checked and (_errormsg = '') then
    begin
      if not(cxTEPassword_A.Text = cxTEPassword_B.Text) then
        _errormsg := 'Your passwords does not match'
      else if (length(cxTEPassword_A.Text) < 6) then
        _errormsg := 'Your password name is to short';
    end;
    if not(_errormsg = '') then
      MessageDlg(_errormsg, mtError, [mbOK], 0)
    else
    begin
      {
      with TApiHTTP.CreateAccounting do
        try
          Params := TIdMultiPartFormDataStream.Create;
          try
            Params.AddFormField('action', 'activation_v1', 'utf-8');

            if cxRBNewAccount.Checked then
              Params.AddFormField('type', 'new_account', 'utf-8')
            else
              Params.AddFormField('type', 'use_account', 'utf-8');

            Params.AddObject('activation_key', '', '', TStringStream.Create(cxTEActivationKey.Text, TEncoding.UTF8));
            Params.AddObject('emailaddress', '', '', TStringStream.Create(cxTEEMailAddress.Text, TEncoding.UTF8));
            Params.AddObject('username', '', '', TStringStream.Create(cxTEAccountname.Text, TEncoding.UTF8));
            if cxRBNewAccount.Checked then
              Params.AddObject('password', '', '', TStringStream.Create(cxTEPassword_A.Text, TEncoding.UTF8));

            Params.Position := 0;

            _errormsg := 'Unknown error';

            ReplyData := TStringStream.Create('', CP_UTF8);
            try
              XMLDoc := NewXMLDocument;
              with XMLDoc do
              begin
                try
                  Post(Homepage + 'buy/', Params, ReplyData);
                except
                  on E: Exception do
                  begin
                    try
                      Post('http://geskill.bplaced.net/intelligen2009/buy/', Params, ReplyData);
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
                  with ChildNodes.Nodes['code'] do
                  begin
                    case NodeValue of
                      0:
                        _errormsg := 'Your activation key seems invalid';
                      1:
                        _errormsg := 'The activation process was successful, we''ve send you a eMail to activate the key';
                      2:
                        _errormsg := 'Your E-Mail-Address or user name seems invalid or already in use';
                      3:
                        _errormsg := 'Your account is still vaild, please activate the new key not before your old is expired';
                      255:
                        begin
                        end;
                    end;
                  end;
                  with ChildNodes.Nodes['status'] do
                  begin
                    if (NodeValue = 1) then
                    begin
                      MessageDlg(_errormsg, mtInformation, [mbOK], 0);
                      Close;
                    end
                    else
                      MessageDlg(_errormsg, mtError, [mbOK], 0);
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
    end;
  end
end;

procedure TRegister.cxRBExistingAccountClick(Sender: TObject);
begin
  cxTEPassword_A.Enabled := cxRBNewAccount.Checked;
  cxTEPassword_B.Enabled := cxRBNewAccount.Checked;
end;

procedure TRegister.cxRBNewAccountClick(Sender: TObject);
begin
  cxTEPassword_A.Enabled := cxRBNewAccount.Checked;
  cxTEPassword_B.Enabled := cxRBNewAccount.Checked;
end;

end.
