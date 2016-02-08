object fCustomScriptSettingsForm: TfCustomScriptSettingsForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'CustomScript Settings'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbScriptFile: TGroupBox
    Left = 8
    Top = 8
    Width = 619
    Height = 113
    Caption = 'File'
    TabOrder = 0
    object sbCreateEmptyScriptFile: TSpeedButton
      Left = 21
      Top = 24
      Width = 134
      Height = 21
      Caption = 'Create empty script file ...'
      Flat = True
      OnClick = sbCreateEmptyScriptFileClick
    end
    object sbSearchFile: TSpeedButton
      Left = 581
      Top = 72
      Width = 21
      Height = 21
      Caption = '...'
      Flat = True
      OnClick = sbSearchFileClick
    end
    object lScriptFilePath: TLabel
      Left = 21
      Top = 56
      Width = 45
      Height = 13
      Caption = 'File path:'
    end
    object eScriptFilePath: TEdit
      Left = 21
      Top = 72
      Width = 554
      Height = 21
      TabOrder = 0
    end
  end
  object gbSettings: TGroupBox
    Left = 8
    Top = 127
    Width = 619
    Height = 134
    Caption = 'Settings'
    TabOrder = 1
    object cbUseFoundAsValue: TCheckBox
      Left = 21
      Top = 24
      Width = 220
      Height = 17
      Caption = 'Use found value as first control value'
      TabOrder = 0
    end
  end
  object bSave: TButton
    Left = 471
    Top = 267
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 2
    OnClick = bSaveClick
  end
  object bClose: TButton
    Left = 552
    Top = 267
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 3
    OnClick = bCloseClick
  end
end
