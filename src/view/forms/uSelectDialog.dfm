object SelectDialog: TSelectDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'SelectDialog'
  ClientHeight = 108
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    294
    108)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Label1'
    Transparent = True
  end
  object cxCOBSelect: TcxComboBox
    Left = 24
    Top = 32
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Width = 246
  end
  object cxButton1: TcxButton
    Left = 69
    Top = 65
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = cxButton1Click
  end
  object cxButton2: TcxButton
    Left = 150
    Top = 65
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = cxButton2Click
  end
end
