object fPluginWizard: TfPluginWizard
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Plugin Wizard'
  ClientHeight = 217
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lFullName: TLabel
    Left = 8
    Top = 8
    Width = 50
    Height = 13
    Caption = 'Full Name:'
  end
  object lBasicName: TLabel
    Left = 8
    Top = 48
    Width = 58
    Height = 13
    Caption = 'Basic Name:'
  end
  object lCompanyName: TLabel
    Left = 248
    Top = 8
    Width = 79
    Height = 13
    Caption = 'Company Name:'
  end
  object lMajorVer: TLabel
    Left = 248
    Top = 96
    Width = 69
    Height = 13
    Caption = 'Major Version:'
  end
  object lMinorVer: TLabel
    Left = 368
    Top = 96
    Width = 68
    Height = 13
    Caption = 'Minor Version:'
  end
  object lRelease: TLabel
    Left = 248
    Top = 137
    Width = 42
    Height = 13
    Caption = 'Release:'
  end
  object lBuild: TLabel
    Left = 368
    Top = 137
    Width = 26
    Height = 13
    Caption = 'Build:'
  end
  object lCopyright: TLabel
    Left = 248
    Top = 48
    Width = 51
    Height = 13
    Caption = 'Copyright:'
  end
  object lWebsite: TLabel
    Left = 8
    Top = 96
    Width = 43
    Height = 13
    Caption = 'Website:'
  end
  object eBasicName: TEdit
    Left = 8
    Top = 61
    Width = 225
    Height = 21
    ReadOnly = True
    TabOrder = 1
  end
  object seMajorVer: TSpinEdit
    Left = 248
    Top = 109
    Width = 105
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 2
  end
  object seMinorVer: TSpinEdit
    Left = 368
    Top = 109
    Width = 105
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 6
    Value = 129
  end
  object seRelease: TSpinEdit
    Left = 248
    Top = 150
    Width = 105
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 7
    Value = 2
  end
  object seBuild: TSpinEdit
    Left = 368
    Top = 150
    Width = 105
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 8
    Value = 0
  end
  object bCreate: TButton
    Left = 398
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Create'
    Default = True
    Enabled = False
    TabOrder = 9
    OnClick = bCreateClick
  end
  object cobCompanyName: TComboBox
    Left = 248
    Top = 21
    Width = 225
    Height = 21
    TabOrder = 3
    Items.Strings = (
      'Sebastian Klatte')
  end
  object cobCopyright: TComboBox
    Left = 248
    Top = 61
    Width = 225
    Height = 21
    TabOrder = 4
    Items.Strings = (
      '(c) 2016 Sebastian Klatte')
  end
  object eWebsite: TEdit
    Left = 8
    Top = 109
    Width = 225
    Height = 21
    TabOrder = 2
    Text = 'http://www.intelligen2009.com/'
  end
  object eFullName: TcxMaskEdit
    Left = 8
    Top = 21
    Properties.MaskKind = emkRegExpr
    Properties.EditMask = '([A-Za-z_]{1}\w{2,15})'
    Properties.OnChange = eFullNamePropertiesChange
    TabOrder = 0
    Width = 225
  end
end
