object fDirWatchSettingsForm: TfDirWatchSettingsForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'DirWatch Settings'
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
  object gbDirectoryFile: TGroupBox
    Left = 8
    Top = 8
    Width = 619
    Height = 81
    Caption = 'File'
    TabOrder = 0
    object sbSearchDirectory: TSpeedButton
      Left = 581
      Top = 40
      Width = 21
      Height = 21
      Caption = '...'
      Flat = True
      OnClick = sbSearchDirectoryClick
    end
    object lDirectoryPath: TLabel
      Left = 21
      Top = 24
      Width = 73
      Height = 13
      Caption = 'Directory path:'
    end
    object eDirectoryPath: TEdit
      Left = 21
      Top = 40
      Width = 554
      Height = 21
      TabOrder = 0
    end
  end
  object gbSettings: TGroupBox
    Left = 8
    Top = 95
    Width = 619
    Height = 166
    Caption = 'Settings'
    TabOrder = 1
    object cbWatchSubdirectories: TCheckBox
      Left = 21
      Top = 24
      Width = 164
      Height = 17
      Caption = 'Watch subdirectories'
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
