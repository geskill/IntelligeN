object Register: TRegister
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Register'
  ClientHeight = 232
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    361
    232)
  PixelsPerInch = 96
  TextHeight = 13
  object pDo: TPanel
    Left = 8
    Top = 8
    Width = 345
    Height = 185
    BevelOuter = bvNone
    TabOrder = 0
    object cxRBBuy: TcxRadioButton
      Left = 116
      Top = 68
      Width = 113
      Height = 17
      Caption = 'Buy'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object cxRBActivate: TcxRadioButton
      Left = 116
      Top = 100
      Width = 113
      Height = 17
      Caption = 'Activate'
      TabOrder = 1
    end
  end
  object pBuy: TPanel
    Left = 8
    Top = 8
    Width = 345
    Height = 185
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object cxRBPayPal: TcxRadioButton
      Left = 116
      Top = 95
      Width = 113
      Height = 17
      Caption = 'PayPal'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object cxRBAlertPay: TcxRadioButton
      Left = 116
      Top = 72
      Width = 113
      Height = 17
      Caption = 'AlertPay'
      Enabled = False
      TabOrder = 0
    end
  end
  object pActivate: TPanel
    Left = 8
    Top = 8
    Width = 345
    Height = 185
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object cxTEActivationKey: TcxTextEdit
      Left = 110
      Top = 52
      TabOrder = 2
      TextHint = 'Activation key'
      Width = 185
    end
    object cxTEEMailAddress: TcxTextEdit
      Left = 110
      Top = 79
      TabOrder = 3
      TextHint = 'E-Mail-Address'
      Width = 185
    end
    object cxTEAccountname: TcxTextEdit
      Left = 110
      Top = 106
      TabOrder = 4
      TextHint = 'Account name'
      Width = 185
    end
    object cxTEPassword_A: TcxTextEdit
      Left = 110
      Top = 133
      Properties.EchoMode = eemPassword
      TabOrder = 5
      TextHint = 'Password'
      Width = 185
    end
    object cxTEPassword_B: TcxTextEdit
      Left = 110
      Top = 160
      Properties.EchoMode = eemPassword
      TabOrder = 6
      TextHint = 're-type Password'
      Width = 185
    end
    object cxRBNewAccount: TcxRadioButton
      Left = 45
      Top = 6
      Width = 220
      Height = 17
      Caption = 'create new account'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = cxRBNewAccountClick
    end
    object cxRBExistingAccount: TcxRadioButton
      Left = 45
      Top = 29
      Width = 219
      Height = 17
      Caption = 'use existing account'
      TabOrder = 1
      OnClick = cxRBExistingAccountClick
    end
    object cxLActivationKey: TcxLabel
      Left = 28
      Top = 53
      Caption = 'Activation key:'
    end
    object cxLEMailAddress: TcxLabel
      Left = 25
      Top = 80
      Caption = 'E-Mail-Address:'
    end
    object cxLAccountname: TcxLabel
      Left = 28
      Top = 107
      Caption = 'Account name:'
    end
    object cxLPassword_A: TcxLabel
      Left = 50
      Top = 134
      Caption = 'Password:'
    end
    object cxLPassword_B: TcxLabel
      Left = 11
      Top = 161
      Caption = 're-type Password:'
    end
  end
  object pMonths: TPanel
    Left = 8
    Top = 8
    Width = 345
    Height = 185
    BevelOuter = bvNone
    TabOrder = 5
    Visible = False
    object cxRB2Months: TcxRadioButton
      Left = 110
      Top = 57
      Width = 139
      Height = 17
      Caption = '2 Months (10,00 EUR)'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object cxRB6Months: TcxRadioButton
      Left = 110
      Top = 80
      Width = 139
      Height = 17
      Caption = '6 Months (25,00 EUR)'
      TabOrder = 1
    end
    object cxRB12Months: TcxRadioButton
      Left = 110
      Top = 103
      Width = 139
      Height = 17
      Caption = '12 Months (50,00 EUR)'
      TabOrder = 2
    end
  end
  object cxBDoNext: TcxButton
    Left = 278
    Top = 199
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Next'
    TabOrder = 4
    OnClick = cxBDoNextClick
  end
  object cxBBack: TcxButton
    Left = 197
    Top = 199
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Back'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = cxBBackClick
  end
end
