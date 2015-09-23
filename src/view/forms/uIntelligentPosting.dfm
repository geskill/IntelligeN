object IntelligentPosting: TIntelligentPosting
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'intelligent_posting-Helper'
  ClientHeight = 226
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    400
    226)
  PixelsPerInch = 96
  TextHeight = 13
  object cxLSearchValue: TcxLabel
    Left = 8
    Top = 8
    Caption = 'Search value:'
  end
  object cxTESearchValue: TcxTextEdit
    Left = 8
    Top = 30
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Width = 303
  end
  object cxBNewSearch: TcxButton
    Left = 317
    Top = 28
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Search'
    ModalResult = 4
    TabOrder = 2
  end
  object cxLSearchResults: TcxLabel
    Left = 8
    Top = 57
    Caption = 'Search results:'
  end
  object cxLBSearchResults: TcxListBox
    Left = 8
    Top = 80
    Width = 384
    Height = 107
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnMouseMove = cxLBSearchResultsMouseMove
  end
  object cxBAccept: TcxButton
    Left = 236
    Top = 193
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Accept'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object cxBCancel: TcxButton
    Left = 317
    Top = 193
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end
