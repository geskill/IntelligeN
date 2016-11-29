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
    object sbSearchCustomCheckScriptFile: TSpeedButton
      Left = 581
      Top = 139
      Width = 21
      Height = 21
      Caption = '...'
      Enabled = False
      Flat = True
      OnClick = sbSearchCustomCheckScriptFileClick
    end
    object sbCreateEmptyCustomCheckScriptFile: TSpeedButton
      Left = 465
      Top = 114
      Width = 137
      Height = 21
      Caption = 'Create empty script file ...'
      Enabled = False
      Flat = True
      OnClick = sbCreateEmptyCustomCheckScriptFileClick
    end
    object cbWatchSubdirectories: TCheckBox
      Left = 21
      Top = 70
      Width = 234
      Height = 17
      Caption = 'Watch subdirectories'
      TabOrder = 2
    end
    object cbLoadOnlyIntelligeNXML2Files: TCheckBox
      Left = 40
      Top = 116
      Width = 215
      Height = 17
      Caption = 'Load only intelligen.xml.2 files'
      Enabled = False
      TabOrder = 4
    end
    object cbLoadOnyXMLFiles: TCheckBox
      Left = 21
      Top = 93
      Width = 234
      Height = 17
      Caption = 'Load only xml files'
      TabOrder = 3
      OnClick = cbLoadOnyXMLFilesClick
    end
    object cbLoadAlreadyExistingFiles: TCheckBox
      Left = 21
      Top = 47
      Width = 234
      Height = 17
      Caption = 'Load already existing files'
      TabOrder = 1
    end
    object cbLoadFilesOnlyOnce: TCheckBox
      Left = 21
      Top = 24
      Width = 234
      Height = 17
      Caption = 'Load files only once'
      TabOrder = 0
    end
    object cbRunCrawlers: TCheckBox
      Left = 261
      Top = 24
      Width = 234
      Height = 17
      Caption = 'Run crawlers'
      TabOrder = 6
    end
    object cbRunCrypters: TCheckBox
      Left = 261
      Top = 47
      Width = 234
      Height = 17
      Caption = 'Run crypters'
      TabOrder = 7
    end
    object cbRunSave: TCheckBox
      Left = 261
      Top = 70
      Width = 234
      Height = 17
      Caption = 'Run save under same file name'
      TabOrder = 8
    end
    object cbRunPublish: TCheckBox
      Left = 261
      Top = 93
      Width = 234
      Height = 17
      Caption = 'Run publish'
      TabOrder = 9
      OnClick = cbRunPublishClick
    end
    object cbRunPublishOnlyWithCustomCheck: TCheckBox
      Left = 280
      Top = 116
      Width = 179
      Height = 17
      Caption = 'Only with custom check'
      Enabled = False
      TabOrder = 10
      OnClick = cbRunPublishOnlyWithCustomCheckClick
    end
    object ePublishCustomCheckScriptFile: TEdit
      Left = 280
      Top = 139
      Width = 295
      Height = 21
      Enabled = False
      TabOrder = 11
    end
    object cbCloseTabAfterPublish: TCheckBox
      Left = 21
      Top = 139
      Width = 234
      Height = 17
      Caption = 'Close tab after publish'
      TabOrder = 5
    end
  end
  object bSave: TButton
    Left = 471
    Top = 267
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 3
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
    TabOrder = 4
    OnClick = bCloseClick
  end
  object bReset: TButton
    Left = 390
    Top = 267
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 2
    OnClick = bResetClick
  end
end
