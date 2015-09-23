object fLogin: TfLogin
  Left = 0
  Top = 0
  Width = 200
  Height = 135
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  TabOrder = 0
  DesignSize = (
    200
    135)
  object pLogin: TPanel
    Left = 8
    Top = 8
    Width = 184
    Height = 119
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    DesignSize = (
      184
      119)
    object cxbLogin: TcxButton
      Left = 0
      Top = 99
      Width = 65
      Height = 20
      Caption = 'Login'
      Default = True
      Enabled = False
      TabOrder = 4
      OnClick = cxbLoginClick
    end
    object cxbRegister: TcxButton
      Left = 119
      Top = 99
      Width = 65
      Height = 20
      Anchors = [akTop, akRight]
      Caption = 'Register'
      TabOrder = 5
      OnClick = cxbRegisterClick
    end
    object eLoginname: TcxTextEdit
      Left = 0
      Top = 0
      Anchors = [akLeft, akTop, akRight]
      Properties.OnChange = ActivateSaveData
      TabOrder = 0
      TextHint = 'Login name'
      Width = 184
    end
    object cbLoginSaveData: TcxCheckBox
      Left = 0
      Top = 54
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Save password'
      Enabled = False
      TabOrder = 2
      Transparent = True
      OnClick = cbLoginSaveDataClick
    end
    object cbLoginAutoLogin: TcxCheckBox
      Left = 7
      Top = 75
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Auto login'
      Enabled = False
      TabOrder = 3
      Transparent = True
      OnClick = cbLoginAutoLoginClick
    end
    object eLoginpassword: TcxTextEdit
      Left = 0
      Top = 27
      Anchors = [akLeft, akTop, akRight]
      Properties.EchoMode = eemPassword
      Properties.OnChange = ActivateSaveData
      TabOrder = 1
      TextHint = 'Login password'
      Width = 184
    end
  end
  object pLogout: TPanel
    Left = 8
    Top = 8
    Width = 184
    Height = 55
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 0
    Visible = False
    object cxbUserCP: TcxButton
      Left = 71
      Top = 35
      Width = 65
      Height = 20
      Caption = 'UserCP'
      TabOrder = 1
      OnMouseDown = cxbUserCPMouseDown
    end
    object cxbLogout: TcxButton
      Left = 0
      Top = 35
      Width = 65
      Height = 20
      Caption = 'Logout'
      TabOrder = 0
      OnClick = cxbLogoutClick
    end
    object caLAccount: TcxLabel
      Left = 0
      Top = -2
      Caption = 'Account:'
      Transparent = True
    end
    object cxLAccountValue: TcxLabel
      Left = 0
      Top = 12
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = [fsBold]
      Style.IsFontAssigned = True
      Transparent = True
    end
  end
  object pPassword: TPanel
    Left = 8
    Top = 8
    Width = 184
    Height = 119
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object cxLOldPassword: TcxLabel
      Left = 0
      Top = 0
      Caption = 'Current password:'
      Transparent = True
    end
    object cxTEOldPassword: TcxTextEdit
      Left = 0
      Top = 18
      Properties.EchoMode = eemPassword
      Properties.OnChange = cxTEOldPasswordPropertiesChange
      TabOrder = 1
      Width = 184
    end
    object cxLNewPassword: TcxLabel
      Left = 0
      Top = 0
      Caption = 'New password:'
      Transparent = True
      Visible = False
    end
    object cxTENewPassword: TcxTextEdit
      Left = 0
      Top = 18
      Properties.EchoMode = eemPassword
      Properties.OnChange = cxTENewPasswordPropertiesChange
      TabOrder = 3
      Visible = False
      Width = 184
    end
    object cxBCancelPassword: TcxButton
      Left = 48
      Top = 92
      Width = 65
      Height = 20
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 5
      OnClick = cxBCancelPasswordClick
    end
    object cxBChangePassword: TcxButton
      Left = 119
      Top = 92
      Width = 65
      Height = 20
      Caption = 'Next'
      Default = True
      Enabled = False
      TabOrder = 7
      OnClick = cxBChangePasswordClick
    end
    object cxTENewPassword2: TcxTextEdit
      Left = 0
      Top = 65
      Properties.EchoMode = eemPassword
      Properties.OnChange = cxTENewPasswordPropertiesChange
      TabOrder = 4
      Visible = False
      Width = 184
    end
    object cxLNewPassword2: TcxLabel
      Left = 0
      Top = 45
      Caption = 'New password:'
      Transparent = True
      Visible = False
    end
  end
end
