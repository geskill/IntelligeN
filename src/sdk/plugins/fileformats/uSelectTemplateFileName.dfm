object SelectTemplateFileName: TSelectTemplateFileName
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select Templatefilename'
  ClientHeight = 80
  ClientWidth = 161
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lTemplateFile: TLabel
    Left = 8
    Top = 8
    Width = 48
    Height = 13
    Caption = 'Template:'
  end
  object cobTemplateFileName: TComboBox
    Left = 8
    Top = 24
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cobTemplateFileNameChange
  end
  object bAccept: TButton
    Left = 96
    Top = 51
    Width = 57
    Height = 21
    Caption = 'Accept'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = bAcceptClick
  end
end
