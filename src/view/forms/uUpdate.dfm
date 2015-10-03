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
  OnShow = FormShow
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
    TabOrder = 3
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
    TabOrder = 5
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
    TabOrder = 6
    OnClick = bCloseClick
  end
  object cxRBUpgrade: TcxRadioButton
    Left = 45
    Top = 52
    Width = 323
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Visible = False
    OnClick = cxRBClick
  end
  object cxRBUpdate: TcxRadioButton
    Left = 45
    Top = 70
    Width = 323
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Visible = False
    OnClick = cxRBClick
  end
  object bDownload: TButton
    Left = 32
    Top = 93
    Width = 75
    Height = 25
    Caption = 'Download'
    TabOrder = 4
    Visible = False
    OnClick = bDownloadClick
  end
end
