object IntelligentPosting: TIntelligentPosting
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'intelligent_posting-Helper'
  ClientHeight = 220
  ClientWidth = 390
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    390
    220)
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
    Width = 293
  end
  object cxBNewSearch: TcxButton
    Left = 307
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
    Width = 374
    Height = 101
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnMouseMove = cxLBSearchResultsMouseMove
  end
  object cxBAccept: TcxButton
    Left = 226
    Top = 187
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Accept'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object cxBCancel: TcxButton
    Left = 307
    Top = 187
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end
