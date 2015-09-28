object Main: TMain
  Left = 0
  Top = 0
  Caption = 'IntelligeN OLE Client Demo'
  ClientHeight = 193
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    635
    193)
  PixelsPerInch = 96
  TextHeight = 13
  object lFileName: TLabel
    Left = 111
    Top = 42
    Width = 46
    Height = 13
    Caption = 'Filename:'
  end
  object spSearchFile: TSpeedButton
    Left = 506
    Top = 58
    Width = 23
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    Flat = True
    OnClick = spSearchFileClick
  end
  object lCrawlerActive: TLabel
    Left = 111
    Top = 85
    Width = 41
    Height = 13
    Caption = 'Crawler:'
  end
  object lHosterStatus: TLabel
    Left = 238
    Top = 85
    Width = 36
    Height = 13
    Caption = 'Hoster:'
  end
  object lPublishStatus: TLabel
    Left = 365
    Top = 85
    Width = 37
    Height = 13
    Caption = 'Publish:'
  end
  object bStartIntelligeN: TButton
    Left = 8
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Start IntelligeN'
    TabOrder = 0
    OnClick = bStartIntelligeNClick
  end
  object bCloseIntelligeN: TButton
    Left = 530
    Top = 8
    Width = 97
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Close IntelligeN'
    TabOrder = 1
    OnClick = bCloseIntelligeNClick
  end
  object bLoadFromFile: TButton
    Left = 30
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Load XML'
    TabOrder = 2
    OnClick = bLoadFromFileClick
  end
  object eFileName: TEdit
    Left = 111
    Top = 58
    Width = 389
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object bUpdateStatus: TButton
    Left = 30
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Status'
    TabOrder = 4
    OnClick = bUpdateStatusClick
  end
  object eStatusCrawler: TEdit
    Left = 111
    Top = 98
    Width = 121
    Height = 21
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
  end
  object eStatusHoster: TEdit
    Left = 238
    Top = 98
    Width = 121
    Height = 21
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 6
  end
  object eStatusPublish: TEdit
    Left = 365
    Top = 98
    Width = 121
    Height = 21
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 7
  end
  object bStartCrawler: TButton
    Left = 30
    Top = 160
    Width = 83
    Height = 25
    Caption = 'Start Crawler'
    TabOrder = 8
    OnClick = bStartCrawlerClick
  end
  object bStartImageUpload: TButton
    Left = 119
    Top = 160
    Width = 130
    Height = 25
    Caption = 'Remote Image Upload'
    TabOrder = 9
    OnClick = bStartImageUploadClick
  end
  object bDirectlinksHosterCheck: TButton
    Left = 255
    Top = 160
    Width = 130
    Height = 25
    Caption = 'Directlinks Hoster Check'
    TabOrder = 10
    OnClick = bDirectlinksHosterCheckClick
  end
  object bStartCrypter: TButton
    Left = 391
    Top = 160
    Width = 83
    Height = 25
    Caption = 'Start Crypter'
    TabOrder = 11
    OnClick = bStartCrypterClick
  end
  object bStartPublish: TButton
    Left = 480
    Top = 160
    Width = 89
    Height = 25
    Caption = 'Start Publish'
    TabOrder = 12
    OnClick = bStartPublishClick
  end
  object bSave: TButton
    Left = 575
    Top = 160
    Width = 52
    Height = 25
    Caption = 'Save'
    TabOrder = 13
    OnClick = bSaveClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.xml'
    Filter = '.xml'
    Left = 568
    Top = 56
  end
end
