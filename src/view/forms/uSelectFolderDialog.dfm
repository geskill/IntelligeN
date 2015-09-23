object SelectFolderDialog: TSelectFolderDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Search folder'
  ClientHeight = 372
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
    372)
  PixelsPerInch = 96
  TextHeight = 13
  object cxLDescription: TcxLabel
    Left = 8
    Top = 8
    Caption = 'Search folder...'
  end
  object cxShellTreeView: TcxShellTreeView
    Left = 8
    Top = 31
    Width = 278
    Height = 240
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    RightClickSelect = True
    TabOrder = 1
  end
  object cxLFileFormat: TcxLabel
    Left = 8
    Top = 277
    Anchors = [akLeft, akBottom]
    Caption = 'Select file format...'
  end
  object cxCOBFileFormat: TcxComboBox
    Left = 8
    Top = 299
    Anchors = [akLeft, akRight]
    Properties.DropDownListStyle = lsFixedList
    TabOrder = 3
    Width = 278
  end
  object cxCancel: TcxButton
    Left = 130
    Top = 339
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    OnClick = cxCancelClick
  end
  object cxBOk: TcxButton
    Left = 211
    Top = 339
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = cxBOkClick
  end
end
