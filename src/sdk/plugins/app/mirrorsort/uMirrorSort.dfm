object fMirrorSort: TfMirrorSort
  Left = 0
  Top = 0
  Caption = 'MirrorSort Plugin'
  ClientHeight = 282
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  DesignSize = (
    418
    282)
  PixelsPerInch = 96
  TextHeight = 13
  object bFilter: TButton
    Left = 304
    Top = 249
    Width = 106
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Filter'
    TabOrder = 1
    OnClick = bFilterClick
  end
  object pcsubtabs: TPageControl
    Left = 8
    Top = 8
    Width = 402
    Height = 235
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiLine = True
    TabOrder = 2
    Visible = False
  end
  object cbMultiUpload: TCheckBox
    Left = 8
    Top = 257
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'MultiUpload'
    TabOrder = 0
  end
  object pmLinks: TPopupMenu
    OnPopup = pmLinksPopup
    Left = 96
    Top = 48
    object nCut: TMenuItem
      Caption = 'Cut'
      OnClick = nCutClick
    end
    object nCopy: TMenuItem
      Caption = 'Copy'
      OnClick = nCopyClick
    end
    object nPaste: TMenuItem
      Caption = 'Paste'
      OnClick = nPasteClick
    end
  end
end
