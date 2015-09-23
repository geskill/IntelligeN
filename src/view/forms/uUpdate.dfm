object Update: TUpdate
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Update'
  ClientHeight = 126
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    400
    126)
  PixelsPerInch = 96
  TextHeight = 13
  object cxLMessage: TcxLabel
    Left = 32
    Top = 32
  end
  object cxPBUpdateStatus: TcxProgressBar
    Left = 32
    Top = 52
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Properties.SolidTextColor = True
    TabOrder = 1
    Visible = False
    Height = 35
    Width = 336
  end
  object bRestartProgram: TButton
    Left = 212
    Top = 93
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Restart'
    TabOrder = 2
    Visible = False
    OnClick = bRestartProgramClick
  end
  object bClose: TButton
    Left = 293
    Top = 93
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = bCloseClick
  end
  object cxREUpdateInfo: TcxRichEdit
    Left = 32
    Top = 136
    Anchors = [akLeft, akTop, akRight]
    Properties.AutoURLDetect = True
    Properties.MemoMode = True
    Properties.ReadOnly = True
    Properties.ScrollBars = ssBoth
    TabOrder = 4
    Height = 0
    Width = 336
  end
  object cxLShowInfo: TcxLabel
    Left = 32
    Top = 101
    Cursor = crHandPoint
    Caption = '[+] Show update details'
    Visible = False
    OnClick = cxLShowInfoClick
  end
end
