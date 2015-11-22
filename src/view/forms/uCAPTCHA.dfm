object CAPTCHA: TCAPTCHA
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'CAPTCHA'
  ClientHeight = 195
  ClientWidth = 371
  Color = clBtnFace
  Constraints.MinWidth = 180
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    371
    195)
  PixelsPerInch = 96
  TextHeight = 13
  object iCAPTCHA: TImage
    Left = 8
    Top = 8
    Width = 355
    Height = 121
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = True
    Center = True
    Visible = False
    ExplicitWidth = 619
  end
  object eCAPTCHA: TEdit
    Left = 8
    Top = 135
    Width = 355
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object bAccept: TButton
    Left = 207
    Top = 162
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Accept'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object bCancel: TButton
    Left = 288
    Top = 162
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object mCAPTCHA: TMemo
    Left = 8
    Top = 8
    Width = 355
    Height = 121
    ReadOnly = True
    TabOrder = 0
    Visible = False
  end
  object tTimer: TTimer
    Enabled = False
    OnTimer = tTimerTimer
    Left = 16
    Top = 16
  end
end
