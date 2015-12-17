object Settings: TSettings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  ClientHeight = 374
  ClientWidth = 606
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    606
    374)
  PixelsPerInch = 96
  TextHeight = 13
  object cxBSaveSettings: TcxButton
    Left = 523
    Top = 341
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save Settings'
    Default = True
    TabOrder = 3
    OnClick = cxBSaveSettingsClick
  end
  object cxPCMain: TcxPageControl
    Left = 0
    Top = 0
    Width = 606
    Height = 335
    Anchors = [akLeft, akTop, akRight, akBottom]
    Focusable = False
    TabOrder = 0
    Properties.ActivePage = cxTSControlAligner
    Properties.CustomButtons.Buttons = <>
    Properties.HotTrack = True
    ClientRectBottom = 331
    ClientRectLeft = 4
    ClientRectRight = 602
    ClientRectTop = 24
    object cxTSGeneral: TcxTabSheet
      Caption = 'Desktop'
      ImageIndex = 0
      object cxcbNativeStyle: TcxCheckBox
        Left = 16
        Top = 16
        Caption = 'Native Style'
        Properties.OnChange = cxcbNativeStylePropertiesChange
        TabOrder = 0
        Transparent = True
      end
      object cxcbCheckForUpdates: TcxCheckBox
        Left = 16
        Top = 70
        Caption = 'Check for updates'
        Properties.OnChange = cxcbCheckForUpdatesPropertiesChange
        TabOrder = 2
        Transparent = True
      end
      object cxcbUseSkins: TcxCheckBox
        Left = 16
        Top = 43
        Caption = 'Use Skins'
        Properties.OnChange = cxcbUseSkinsPropertiesChange
        TabOrder = 1
        Transparent = True
        Visible = False
      end
      object cxCOBDefaultSkin: TcxComboBox
        Left = 167
        Top = 43
        Properties.DropDownListStyle = lsFixedList
        Properties.OnChange = cxCOBDefaultSkinPropertiesChange
        TabOrder = 3
        Visible = False
        Width = 121
      end
      object cxLCAPTCHAPosition: TcxLabel
        Left = 16
        Top = 97
        Caption = 'CAPTCHA position:'
        Transparent = True
      end
      object cxCOBCAPTCHAPosition: TcxComboBox
        Left = 16
        Top = 117
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'bottom left'
          'bottom right'
          'centered'
          'top left'
          'top right')
        Properties.OnChange = cxCOBCAPTCHAPositionPropertiesChange
        TabOrder = 5
        Width = 121
      end
    end
    object cxTSPlugins: TcxTabSheet
      Caption = 'Plugins'
      ImageIndex = 3
      DesignSize = (
        598
        307)
      object cxPCPlugins: TcxPageControl
        Left = 3
        Top = 3
        Width = 592
        Height = 301
        Anchors = [akLeft, akTop, akRight, akBottom]
        Focusable = False
        TabOrder = 0
        Properties.ActivePage = cxTSCrawler
        Properties.CustomButtons.Buttons = <>
        Properties.HotTrack = True
        ClientRectBottom = 297
        ClientRectLeft = 4
        ClientRectRight = 588
        ClientRectTop = 24
        object cxTSApp: TcxTabSheet
          Caption = 'App'
          ImageIndex = 0
        end
        object cxTSCAPTCHA: TcxTabSheet
          Caption = 'CAPTCHA'
          ImageIndex = 0
        end
        object cxTSCMS: TcxTabSheet
          Caption = 'CMS'
          ImageIndex = 0
          DesignSize = (
            584
            273)
          inline fAddWebsiteWizard: TfAddWebsiteWizard
            Left = 151
            Top = 16
            Width = 430
            Height = 254
            Anchors = [akLeft, akTop, akRight, akBottom]
            TabOrder = 1
            ExplicitLeft = 151
            ExplicitTop = 16
            ExplicitWidth = 430
            ExplicitHeight = 254
            inherited pNewWebsite: TPanel
              Width = 430
              ExplicitWidth = 430
              inherited cxTEPageURL: TcxTextEdit
                ExplicitWidth = 414
                Width = 414
              end
              inherited cxTEFormattedURL: TcxTextEdit
                ExplicitWidth = 387
                Width = 387
              end
              inherited cxCBEditFormattedURL: TcxCheckBox
                Left = 401
                ExplicitLeft = 401
                ExplicitWidth = 21
              end
              inherited cxCOBURLCMS: TcxComboBox
                ExplicitWidth = 235
                Width = 235
              end
              inherited cxBDetectCMS: TcxButton
                Left = 380
                ExplicitLeft = 380
              end
              inherited cxLPageURLInfo: TcxLabel
                ExplicitWidth = 410
                Width = 410
              end
              inherited cxLEncoding: TcxLabel
                Left = 249
                ExplicitLeft = 249
              end
              inherited cxCOBEncoding: TcxComboBox
                Left = 249
                ExplicitLeft = 249
              end
            end
            inherited pImport: TPanel
              Width = 430
              Height = 191
              ExplicitWidth = 430
              ExplicitHeight = 191
              inherited cxGImport: TcxGrid
                Width = 430
                Height = 166
                ExplicitWidth = 430
                ExplicitHeight = 166
              end
              inherited cxRBImportCopy: TcxRadioButton
                Left = 78
                Top = 172
                ExplicitLeft = 78
                ExplicitTop = 172
              end
              inherited cxRBImportReplace: TcxRadioButton
                Left = 238
                Top = 172
                ExplicitLeft = 238
                ExplicitTop = 172
              end
            end
            inherited cxBCancel: TcxButton
              Left = 271
              Top = 226
              ExplicitLeft = 271
              ExplicitTop = 226
            end
            inherited cxBNext: TcxButton
              Left = 352
              Top = 226
              ExplicitLeft = 352
              ExplicitTop = 226
            end
            inherited cxLAddWebsiteWizard: TcxLabel
              Style.IsFontAssigned = True
            end
            inherited pStart: TPanel
              Width = 430
              Height = 191
              ExplicitWidth = 430
              ExplicitHeight = 191
              inherited cxRBImport: TcxRadioButton
                Top = 112
                Width = 255
                ExplicitTop = 112
                ExplicitWidth = 255
              end
              inherited cxRBNew: TcxRadioButton
                Top = 62
                Width = 255
                ExplicitTop = 62
                ExplicitWidth = 255
              end
              inherited cxRBLoad: TcxRadioButton
                Top = 87
                Width = 255
                ExplicitTop = 87
                ExplicitWidth = 255
              end
            end
            inherited pLoad: TPanel
              Width = 430
              Height = 191
              ExplicitWidth = 430
              ExplicitHeight = 191
              inherited cxGLoad: TcxGrid
                Width = 430
                Height = 191
                ExplicitWidth = 430
                ExplicitHeight = 191
              end
            end
          end
          object pCMSSettings: TPanel
            Left = 151
            Top = 16
            Width = 430
            Height = 254
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelOuter = bvNone
            TabOrder = 0
            Visible = False
            DesignSize = (
              430
              254)
            object cxGCMS: TcxGrid
              Left = 0
              Top = 24
              Width = 430
              Height = 211
              Anchors = [akLeft, akTop, akRight, akBottom]
              TabOrder = 1
              object cxGCMSTableView1: TcxGridTableView
                Navigator.Buttons.CustomButtons = <>
                Navigator.Buttons.First.Visible = True
                Navigator.Buttons.PriorPage.Visible = True
                Navigator.Buttons.Prior.Visible = True
                Navigator.Buttons.Next.Visible = True
                Navigator.Buttons.NextPage.Visible = True
                Navigator.Buttons.Last.Visible = True
                Navigator.Buttons.Insert.Visible = True
                Navigator.Buttons.Append.Visible = False
                Navigator.Buttons.Delete.Visible = True
                Navigator.Buttons.Edit.Visible = True
                Navigator.Buttons.Post.Visible = True
                Navigator.Buttons.Cancel.Visible = True
                Navigator.Buttons.Refresh.Visible = True
                Navigator.Buttons.SaveBookmark.Visible = True
                Navigator.Buttons.GotoBookmark.Visible = True
                Navigator.Buttons.Filter.Visible = True
                FilterBox.CustomizeDialog = False
                FilterBox.Visible = fvNever
                OnFocusedRecordChanged = cxGCMSTableView1FocusedRecordChanged
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                DataController.OnDataChanged = cxGCMSTableView1DataControllerDataChanged
                OptionsBehavior.CellHints = True
                OptionsBehavior.FocusCellOnTab = True
                OptionsView.ShowEditButtons = gsebAlways
                OptionsView.ColumnAutoWidth = True
                OptionsView.GroupByBox = False
                object cxGCMSTableView1Column1: TcxGridColumn
                  Caption = 'Enabled'
                  DataBinding.ValueType = 'Boolean'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Properties.ImmediatePost = True
                  Width = 25
                end
                object cxGCMSTableView1Column2: TcxGridColumn
                  Caption = 'Name'
                  PropertiesClassName = 'TcxLabelProperties'
                  Width = 79
                end
                object cxGCMSTableView1Column3: TcxGridColumn
                  Caption = 'Accountname'
                  PropertiesClassName = 'TcxComboBoxProperties'
                  Properties.OnInitPopup = cxGCMSTableView1Column3PropertiesInitPopup
                  Width = 79
                end
                object cxGCMSTableView1Column4: TcxGridColumn
                  Caption = 'Accountpassword'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Properties.EchoMode = eemPassword
                  Width = 80
                end
                object cxGCMSTableView1Column5: TcxGridColumn
                  Caption = 'Subject'
                  PropertiesClassName = 'TcxButtonEditProperties'
                  Properties.Buttons = <
                    item
                      Default = True
                      Hint = 'Browse...'
                      Kind = bkEllipsis
                    end>
                  Properties.OnButtonClick = cxGCMSTableView1Column5PropertiesButtonClick
                  Options.ShowEditButtons = isebAlways
                  Width = 79
                end
                object cxGCMSTableView1Column6: TcxGridColumn
                  Caption = 'Message'
                  PropertiesClassName = 'TcxButtonEditProperties'
                  Properties.Buttons = <
                    item
                      Default = True
                      Hint = 'Browse...'
                      Kind = bkEllipsis
                    end>
                  Properties.OnButtonClick = cxGCMSTableView1Column6PropertiesButtonClick
                  Options.ShowEditButtons = isebAlways
                  Width = 79
                end
                object cxGCMSTableView1Column7: TcxGridColumn
                  Caption = 'Website settings'
                  PropertiesClassName = 'TcxButtonEditProperties'
                  Properties.Buttons = <
                    item
                      Default = True
                      Hint = 'Website Editor'
                      Kind = bkEllipsis
                    end>
                  Properties.ViewStyle = vsButtonsOnly
                  Properties.OnButtonClick = cxGCMSTableView1Column7PropertiesButtonClick
                  Options.Filtering = False
                  Options.HorzSizing = False
                  Width = 20
                end
              end
              object cxGCMSLevel1: TcxGridLevel
                GridView = cxGCMSTableView1
              end
            end
            object cxCBCMSAll: TcxCheckBox
              Left = 0
              Top = 0
              Caption = 'enable/disable all'
              Properties.ValueChecked = 1
              Properties.ValueGrayed = 2
              Properties.ValueUnchecked = 0
              Properties.OnChange = cxCBCMSAllPropertiesChange
              State = cbsGrayed
              TabOrder = 0
              Transparent = True
            end
            object cxBAddCMSWebsite: TcxButton
              Left = 0
              Top = 238
              Width = 50
              Height = 16
              Anchors = [akLeft, akBottom]
              Caption = 'Add'
              TabOrder = 2
              OnClick = cxBAddCMSWebsiteClick
            end
            object cxBRemoveCMSWebsite: TcxButton
              Left = 380
              Top = 238
              Width = 50
              Height = 16
              Anchors = [akRight, akBottom]
              Caption = 'Remove'
              Enabled = False
              TabOrder = 3
              OnClick = cxBRemoveCMSWebsiteClick
            end
          end
        end
        object cxTSCrawler: TcxTabSheet
          Caption = 'Crawler'
          ImageIndex = 0
          DesignSize = (
            584
            273)
          object pCrawlerSettings: TPanel
            Left = 151
            Top = 16
            Width = 430
            Height = 254
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelOuter = bvNone
            TabOrder = 0
            Visible = False
            DesignSize = (
              430
              254)
            object lLimit: TLabel
              Left = 387
              Top = 0
              Width = 25
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Limit:'
              Transparent = True
              ExplicitLeft = 403
            end
            object cxSELimit: TcxSpinEdit
              Left = 387
              Top = 16
              Anchors = [akTop, akRight]
              Properties.OnChange = cxSELimitPropertiesChange
              TabOrder = 1
              Width = 41
            end
            object cxGCrawler: TcxGrid
              Left = 0
              Top = 0
              Width = 381
              Height = 254
              Anchors = [akLeft, akTop, akRight, akBottom]
              TabOrder = 0
              object cxGCrawlerTableView1: TcxGridTableView
                Navigator.Buttons.CustomButtons = <>
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                DataController.OnDataChanged = cxGCrawlerTableView1DataControllerDataChanged
                OptionsView.ColumnAutoWidth = True
                OptionsView.GroupByBox = False
                object cxGCrawlerTableView1Column1: TcxGridColumn
                  Caption = 'Type'
                  PropertiesClassName = 'TcxLabelProperties'
                end
              end
              object cxGCrawlerTableView2: TcxGridTableView
                Navigator.Buttons.CustomButtons = <>
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                DataController.OnDataChanged = cxGCrawlerTableView2DataControllerDataChanged
                OptionsView.ColumnAutoWidth = True
                OptionsView.GroupByBox = False
                object cxGCrawlerTableView2Column1: TcxGridColumn
                  Caption = 'Control'
                  PropertiesClassName = 'TcxLabelProperties'
                end
                object cxGCrawlerTableView2Column2: TcxGridColumn
                  Caption = 'Status'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Properties.ImmediatePost = True
                end
              end
              object cxGCrawlerLevel1: TcxGridLevel
                GridView = cxGCrawlerTableView1
                object cxGCrawlerLevel2: TcxGridLevel
                  GridView = cxGCrawlerTableView2
                end
              end
            end
          end
        end
        object cxTSCrypter: TcxTabSheet
          Caption = 'Crypter'
          ImageIndex = 0
          DesignSize = (
            584
            273)
          object pCrypterSettings: TPanel
            Left = 151
            Top = 16
            Width = 430
            Height = 254
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelOuter = bvNone
            TabOrder = 0
            Visible = False
            object lCrypterAccountName: TLabel
              Left = 0
              Top = 0
              Width = 119
              Height = 13
              Caption = 'Account name / API key:'
              Transparent = True
            end
            object lCrypterAccountPassword: TLabel
              Left = 127
              Top = 0
              Width = 92
              Height = 13
              Caption = 'Account password:'
              Transparent = True
            end
            object lCrypterVisitorpassword: TLabel
              Left = 154
              Top = 215
              Width = 82
              Height = 13
              Caption = 'Visitor password:'
              Transparent = True
            end
            object lCrypterAdminpassword: TLabel
              Left = 0
              Top = 215
              Width = 82
              Height = 13
              Caption = 'Admin password:'
              Transparent = True
            end
            object lCrypterEMailforStatusNotice: TLabel
              Left = 0
              Top = 172
              Width = 110
              Height = 13
              Caption = 'EMail for status notice:'
              Transparent = True
            end
            object lCrypterFolderName: TLabel
              Left = 0
              Top = 86
              Width = 63
              Height = 13
              Caption = 'Folder name:'
              Transparent = True
            end
            object lCrypterWebseiteLink: TLabel
              Left = 0
              Top = 129
              Width = 67
              Height = 13
              Caption = 'Webseite link:'
              Transparent = True
            end
            object lCrypterCheckDelay: TLabel
              Left = 308
              Top = 215
              Width = 62
              Height = 13
              Caption = 'Check delay:'
            end
            object cxTECrypterAccountName: TcxTextEdit
              Left = 0
              Top = 16
              Properties.OnChange = cxTECrypterAccountNamePropertiesChange
              TabOrder = 0
              Width = 121
            end
            object cxTECrypterAccountPassword: TcxTextEdit
              Left = 127
              Top = 16
              Properties.EchoMode = eemPassword
              Properties.PasswordChar = '*'
              Properties.ValidateOnEnter = True
              Properties.OnChange = cxTECrypterAccountPasswordPropertiesChange
              TabOrder = 1
              Width = 121
            end
            object cxTECrypterVisitorpassword: TcxTextEdit
              Left = 154
              Top = 231
              Properties.OnChange = cxTECrypterVisitorpasswordPropertiesChange
              TabOrder = 17
              Width = 121
            end
            object cxCBCrypterUseAccount: TcxCheckBox
              Left = 254
              Top = 16
              Properties.OnChange = cxCBCrypterUseAccountPropertiesChange
              State = cbsChecked
              TabOrder = 2
              Transparent = True
            end
            object cxCBCrypterUseVisitorpassword: TcxCheckBox
              Left = 281
              Top = 231
              Properties.OnChange = cxCBCrypterUseVisitorpasswordPropertiesChange
              State = cbsChecked
              TabOrder = 18
              Transparent = True
            end
            object cxTECrypterAdminpassword: TcxTextEdit
              Left = 0
              Top = 231
              Properties.OnChange = cxTECrypterAdminpasswordPropertiesChange
              TabOrder = 15
              Width = 121
            end
            object cxTECrypterEMailforStatusNotice: TcxTextEdit
              Left = 0
              Top = 188
              Properties.OnChange = cxTECrypterEMailforStatusNoticePropertiesChange
              TabOrder = 13
              Width = 121
            end
            object cxCBCrypterUseAdminpassword: TcxCheckBox
              Left = 127
              Top = 231
              Properties.OnChange = cxCBCrypterUseAdminpasswordPropertiesChange
              State = cbsChecked
              TabOrder = 16
              Transparent = True
            end
            object cxCBCrypterUseEMailforStatusNotice: TcxCheckBox
              Left = 127
              Top = 188
              Properties.OnChange = cxCBCrypterUseEMailforStatusNoticePropertiesChange
              State = cbsChecked
              TabOrder = 14
              Transparent = True
            end
            object cxCOBCrypterFolderName: TcxComboBox
              Left = 0
              Top = 102
              Properties.DropDownListStyle = lsFixedList
              Properties.Items.Strings = (
                'Filename'
                'Releasename'
                'Title')
              Properties.OnChange = cxCOBCrypterFolderNamePropertiesChange
              TabOrder = 6
              Text = 'Title'
              Width = 121
            end
            object cxCBCrypterUseCoverLink: TcxCheckBox
              Left = 154
              Top = 129
              Caption = 'CoverLink'
              Properties.OnChange = cxCBCrypterUseCoverLinkPropertiesChange
              State = cbsChecked
              TabOrder = 10
              Transparent = True
            end
            object cxCBCrypterUseDescription: TcxCheckBox
              Left = 154
              Top = 156
              Caption = 'Description'
              Properties.OnChange = cxCBCrypterUseDescriptionPropertiesChange
              State = cbsChecked
              TabOrder = 11
              Transparent = True
            end
            object cxTECrypterWebseiteLink: TcxTextEdit
              Left = 0
              Top = 145
              Properties.OnChange = cxTECrypterWebseiteLinkPropertiesChange
              TabOrder = 8
              Width = 121
            end
            object cxCBCrypterUseWebseiteLink: TcxCheckBox
              Left = 127
              Top = 145
              Properties.OnChange = cxCBCrypterUseWebseiteLinkPropertiesChange
              State = cbsChecked
              TabOrder = 9
              Transparent = True
            end
            object cxCGCrypterFoldertypes: TcxCheckGroup
              Left = 0
              Top = 43
              Caption = 'Folder types:'
              Properties.Columns = 3
              Properties.Items = <
                item
                  Caption = 'Web'
                end
                item
                  Caption = 'Plain'
                end
                item
                  Caption = 'Container'
                end>
              Properties.OnChange = cxCGCrypterFoldertypesPropertiesChange
              Style.BorderStyle = ebsNone
              Style.TransparentBorder = True
              TabOrder = 4
              Transparent = True
              Height = 37
              Width = 248
            end
            object cxCGCrypterContainerTypes: TcxCheckGroup
              Left = 254
              Top = 43
              Caption = 'Container types:'
              Properties.Columns = 3
              Properties.Items = <
                item
                  Caption = 'CCF'
                end
                item
                  Caption = 'DLC'
                end
                item
                  Caption = 'RSDF'
                end>
              Properties.OnChange = cxCGCrypterContainerTypesPropertiesChange
              Style.BorderStyle = ebsNone
              TabOrder = 5
              Transparent = True
              Height = 37
              Width = 195
            end
            object cxCBCrypterUseCaptcha: TcxCheckBox
              Left = 127
              Top = 102
              Caption = 'Captcha'
              Properties.OnChange = cxCBCrypterUseCaptchaPropertiesChange
              State = cbsChecked
              TabOrder = 7
              Transparent = True
            end
            object cxPCCrypterAdvertisment: TcxPageControl
              Left = 254
              Top = 86
              Width = 160
              Height = 123
              Focusable = False
              TabOrder = 19
              Properties.ActivePage = cxTSCrypterAdvertismentLink
              Properties.CustomButtons.Buttons = <>
              OnChange = cxPCCrypterAdvertismentChange
              ClientRectBottom = 119
              ClientRectLeft = 4
              ClientRectRight = 156
              ClientRectTop = 24
              object cxTSCrypterAdvertismentLayer: TcxTabSheet
                Caption = 'Layer'
                ImageIndex = 0
                DesignSize = (
                  152
                  95)
                object cxLCrypterAdvertismentLayerName: TcxLabel
                  Left = 3
                  Top = 3
                  Caption = 'Layer name:'
                  Transparent = True
                end
                object cxTECrypterAdvertismentLayerName: TcxTextEdit
                  Left = 3
                  Top = 23
                  Anchors = [akLeft, akTop, akRight]
                  Properties.OnChange = cxTECrypterAdvertismentLayerNamePropertiesChange
                  TabOrder = 1
                  Width = 146
                end
                object cxLCrypterAdvertismentLayerValue: TcxLabel
                  Left = 3
                  Top = 50
                  Caption = 'Layer value:'
                  Transparent = True
                end
                object cxTECrypterAdvertismentLayerValue: TcxTextEdit
                  Left = 3
                  Top = 70
                  Anchors = [akLeft, akTop, akRight]
                  Properties.OnChange = cxTECrypterAdvertismentLayerValuePropertiesChange
                  TabOrder = 3
                  Width = 146
                end
              end
              object cxTSCrypterAdvertismentLink: TcxTabSheet
                Caption = 'Link'
                ImageIndex = 1
                object cxLCrypterAdvertismentLink: TcxLabel
                  Left = 3
                  Top = 3
                  Caption = 'Link:'
                  Transparent = True
                end
                object cxTECrypterAdvertismentLink: TcxTextEdit
                  Left = 3
                  Top = 23
                  Properties.OnChange = cxTECrypterAdvertismentLinkPropertiesChange
                  TabOrder = 1
                  Width = 121
                end
                object cxCBCrypterAdvertismentLink: TcxCheckBox
                  Left = 130
                  Top = 23
                  Properties.OnChange = cxCBCrypterAdvertismentLinkPropertiesChange
                  State = cbsChecked
                  TabOrder = 2
                  Transparent = True
                end
              end
              object cxTSCrypterAdvertismentBanner: TcxTabSheet
                Caption = 'Banner'
                ImageIndex = 2
                object cxLCrypterAdvertismentBannerLink: TcxLabel
                  Left = 3
                  Top = 3
                  Caption = 'Link:'
                  Transparent = True
                end
                object cxTECrypterAdvertismentBannerLink: TcxTextEdit
                  Left = 3
                  Top = 23
                  Properties.OnChange = cxTECrypterAdvertismentBannerLinkPropertiesChange
                  TabOrder = 0
                  Width = 121
                end
                object cxCBCrypterAdvertismentBannerLink: TcxCheckBox
                  Left = 130
                  Top = 23
                  Properties.OnChange = cxCBCrypterAdvertismentBannerLinkPropertiesChange
                  State = cbsChecked
                  TabOrder = 2
                  Transparent = True
                end
                object cxLCrypterAdvertismentBannerPicture: TcxLabel
                  Left = 3
                  Top = 50
                  Caption = 'Picture:'
                  Transparent = True
                end
                object cxTECrypterAdvertismentBannerPicture: TcxTextEdit
                  Left = 3
                  Top = 70
                  Properties.OnChange = cxTECrypterAdvertismentBannerPicturePropertiesChange
                  TabOrder = 4
                  Width = 121
                end
                object cxCBCrypterAdvertismentBannerPicture: TcxCheckBox
                  Left = 130
                  Top = 70
                  Properties.OnChange = cxCBCrypterAdvertismentBannerPicturePropertiesChange
                  State = cbsChecked
                  TabOrder = 5
                  Transparent = True
                end
              end
            end
            object cxTBCrypterCheckDelay: TcxTrackBar
              Left = 300
              Top = 231
              ParentShowHint = False
              Position = 250
              Properties.Frequency = 250
              Properties.Max = 10000
              Properties.OnChange = cxTBCrypterCheckDelayPropertiesChange
              ShowHint = True
              Style.Edges = [bLeft, bTop, bRight, bBottom]
              Style.Shadow = False
              TabOrder = 20
              Height = 21
              Width = 129
            end
            object cxCBCrypterUseCNL: TcxCheckBox
              Left = 335
              Top = 16
              Caption = 'Click'#39'n'#39'load'
              Properties.OnChange = cxCBCrypterUseCNLPropertiesChange
              State = cbsChecked
              TabOrder = 3
              Transparent = True
            end
            object cxCBCrypterUseFilePassword: TcxCheckBox
              Left = 154
              Top = 183
              Caption = 'File password'
              Properties.OnChange = cxCBCrypterUseFilePasswordPropertiesChange
              State = cbsChecked
              TabOrder = 12
              Transparent = True
            end
          end
        end
        object cxTSFileFormats: TcxTabSheet
          Caption = 'File Formats'
          ImageIndex = 0
          DesignSize = (
            584
            273)
          object pFileFormatsSettings: TPanel
            Left = 151
            Top = 16
            Width = 432
            Height = 254
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelOuter = bvNone
            TabOrder = 0
            Visible = False
            object cxCBFileFormatsForceAddCrypter: TcxCheckBox
              Left = 0
              Top = 0
              Caption = 'force add crypter'
              Properties.OnChange = cxCBFileFormatsForceAddCrypterPropertiesChange
              TabOrder = 0
              Transparent = True
            end
            object cxCBFileFormatsForceAddImageMirror: TcxCheckBox
              Left = 0
              Top = 27
              Caption = 'force add image mirror'
              Properties.OnChange = cxCBFileFormatsForceAddImageMirrorPropertiesChange
              TabOrder = 1
              Transparent = True
            end
          end
        end
        object cxTSFileHoster: TcxTabSheet
          Caption = 'File Hoster'
          ImageIndex = 0
        end
        object cxTSImageHoster: TcxTabSheet
          Caption = 'Image Hoster'
          ImageIndex = 0
          DesignSize = (
            584
            273)
          object pImageHosterSettings: TPanel
            Left = 151
            Top = 16
            Width = 432
            Height = 254
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelOuter = bvNone
            TabOrder = 0
            Visible = False
            object lImageHosterAccountName: TLabel
              Left = 0
              Top = 0
              Width = 72
              Height = 13
              Caption = 'Account name:'
              Transparent = True
            end
            object lImageHosterAccountPassword: TLabel
              Left = 127
              Top = 0
              Width = 92
              Height = 13
              Caption = 'Account password:'
              Transparent = True
            end
            object cxTEImageHosterAccountName: TcxTextEdit
              Left = 0
              Top = 16
              Properties.OnChange = cxTEImageHosterAccountNamePropertiesChange
              TabOrder = 0
              Width = 121
            end
            object cxTEImageHosterAccountPassword: TcxTextEdit
              Left = 127
              Top = 16
              Properties.EchoMode = eemPassword
              Properties.PasswordChar = '*'
              Properties.ValidateOnEnter = True
              Properties.OnChange = cxTEImageHosterAccountPasswordPropertiesChange
              TabOrder = 1
              Width = 121
            end
            object cxCBImageHosterUseAccount: TcxCheckBox
              Left = 254
              Top = 16
              Properties.OnChange = cxCBImageHosterUseAccountPropertiesChange
              State = cbsChecked
              TabOrder = 2
              Transparent = True
            end
            object cxCOBImageHosterResize: TcxComboBox
              Left = 0
              Top = 63
              Properties.DropDownListStyle = lsFixedList
              Properties.Items.Strings = (
                'None'
                '320x240'
                '450x338'
                '640x480'
                '800x600')
              Properties.OnChange = cxCOBImageHosterResizePropertiesChange
              TabOrder = 3
              Text = 'None'
              Width = 121
            end
            object cxLImageHosterResize: TcxLabel
              Left = 0
              Top = 43
              Caption = 'Image resize:'
              Transparent = True
            end
            object cxCBImageHosterUploadAfterCrawling: TcxCheckBox
              Left = 127
              Top = 63
              Hint = 'upload to this image hoster after crawling'
              AutoSize = False
              Caption = 'Upload after crawling'
              ParentShowHint = False
              Properties.OnChange = cxCBImageHosterDirectUploadPropertiesChange
              ShowHint = True
              TabOrder = 5
              Transparent = True
              Height = 21
              Width = 148
            end
            object cxLImageHosterUploadAfterCrawling: TcxLabel
              Left = 127
              Top = 43
              Caption = 'Direct upload:'
              Transparent = True
            end
          end
        end
      end
    end
    object cxTSControlAligner: TcxTabSheet
      Caption = 'ControlAligner'
      ImageIndex = 2
      object cxSEMirrorCount: TcxSpinEdit
        Left = 16
        Top = 36
        Properties.MaxValue = 99.000000000000000000
        Properties.MinValue = 1.000000000000000000
        Properties.OnChange = cxSEMirrorCountPropertiesChange
        TabOrder = 0
        Value = 1
        Width = 49
      end
      object cxSEMirrorColumns: TcxSpinEdit
        Left = 16
        Top = 83
        Properties.ImmediatePost = True
        Properties.MaxValue = 3.000000000000000000
        Properties.MinValue = 1.000000000000000000
        Properties.OnChange = cxSEMirrorColumnsPropertiesChange
        TabOrder = 1
        Value = 1
        Width = 49
      end
      object cxCOBMirrorPosition: TcxComboBox
        Left = 16
        Top = 177
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'Bottom'
          'Top')
        Properties.OnChange = cxCOBMirrorPositionPropertiesChange
        TabOrder = 3
        Width = 49
      end
      object cxLDefaultMirrorCount: TcxLabel
        Left = 16
        Top = 16
        Caption = 'Mirror count:'
        Transparent = True
      end
      object cxLMirrorColumns: TcxLabel
        Left = 16
        Top = 63
        Caption = 'Mirror columns:'
        Transparent = True
      end
      object cxLMirrorPosition: TcxLabel
        Left = 16
        Top = 157
        Caption = 'Mirror position:'
        Transparent = True
      end
      object cxLMirrorHeight: TcxLabel
        Left = 16
        Top = 110
        Caption = 'Mirror height:'
        Transparent = True
      end
      object cxSEMirrorHeight: TcxSpinEdit
        Left = 16
        Top = 130
        Properties.Increment = 10.000000000000000000
        Properties.LargeIncrement = 50.000000000000000000
        Properties.MaxValue = 500.000000000000000000
        Properties.MinValue = 50.000000000000000000
        Properties.UseCtrlIncrement = True
        Properties.OnChange = cxSEMirrorHeightPropertiesChange
        TabOrder = 2
        Value = 50
        Width = 49
      end
      object cxGBDefaultStartup: TcxGroupBox
        Left = 226
        Top = 16
        Caption = 'Default startup'
        TabOrder = 13
        Transparent = True
        Height = 158
        Width = 222
        object cxCBDefaultStartupAActive: TcxCheckBox
          Left = 16
          Top = 20
          Caption = 'Active'
          Properties.OnChange = cxCBDefaultStartupActivePropertiesChange
          TabOrder = 0
        end
        object cxCBDefaultStartupBActive: TcxCheckBox
          Tag = 1
          Left = 16
          Top = 47
          Caption = 'Active'
          Properties.OnChange = cxCBDefaultStartupActivePropertiesChange
          TabOrder = 2
        end
        object cxCBDefaultStartupCActive: TcxCheckBox
          Tag = 2
          Left = 16
          Top = 74
          Caption = 'Active'
          Properties.OnChange = cxCBDefaultStartupActivePropertiesChange
          TabOrder = 4
        end
        object cxCBDefaultStartupDActive: TcxCheckBox
          Tag = 3
          Left = 16
          Top = 101
          Caption = 'Active'
          Properties.OnChange = cxCBDefaultStartupActivePropertiesChange
          TabOrder = 6
        end
        object cxCBDefaultStartupEActive: TcxCheckBox
          Tag = 4
          Left = 16
          Top = 128
          Caption = 'Active'
          Properties.OnChange = cxCBDefaultStartupActivePropertiesChange
          TabOrder = 8
        end
        object cxCOBDefaultStartupAType: TcxComboBox
          Left = 87
          Top = 20
          Enabled = False
          Properties.DropDownListStyle = lsFixedList
          Properties.OnChange = cxCOBDefaultStartupTypePropertiesChange
          Properties.OnInitPopup = cxCOBDefaultStartupTypePropertiesInitPopup
          TabOrder = 1
          Width = 121
        end
        object cxCOBDefaultStartupBType: TcxComboBox
          Tag = 1
          Left = 87
          Top = 47
          Enabled = False
          Properties.DropDownListStyle = lsFixedList
          Properties.OnChange = cxCOBDefaultStartupTypePropertiesChange
          Properties.OnInitPopup = cxCOBDefaultStartupTypePropertiesInitPopup
          TabOrder = 3
          Width = 121
        end
        object cxCOBDefaultStartupCType: TcxComboBox
          Tag = 2
          Left = 87
          Top = 74
          Enabled = False
          Properties.DropDownListStyle = lsFixedList
          Properties.OnChange = cxCOBDefaultStartupTypePropertiesChange
          Properties.OnInitPopup = cxCOBDefaultStartupTypePropertiesInitPopup
          TabOrder = 5
          Width = 121
        end
        object cxCOBDefaultStartupDType: TcxComboBox
          Tag = 3
          Left = 87
          Top = 101
          Enabled = False
          Properties.DropDownListStyle = lsFixedList
          Properties.OnChange = cxCOBDefaultStartupTypePropertiesChange
          Properties.OnInitPopup = cxCOBDefaultStartupTypePropertiesInitPopup
          TabOrder = 7
          Width = 121
        end
        object cxCOBDefaultStartupEType: TcxComboBox
          Tag = 4
          Left = 87
          Top = 128
          Enabled = False
          Properties.DropDownListStyle = lsFixedList
          Properties.OnChange = cxCOBDefaultStartupTypePropertiesChange
          Properties.OnInitPopup = cxCOBDefaultStartupTypePropertiesInitPopup
          TabOrder = 9
          Width = 121
        end
      end
      object cxLDefaultMirrorTabIndex: TcxLabel
        Left = 99
        Top = 16
        Caption = 'Default Mirror tab index:'
        Transparent = True
      end
      object cxcobDefaultMirrorTabIndex: TcxComboBox
        Left = 99
        Top = 36
        Properties.DropDownListStyle = lsFixedList
        Properties.OnChange = cxcobDefaultMirrorTabIndexPropertiesChange
        Properties.OnInitPopup = cxcobDefaultMirrorTabIndexPropertiesInitPopup
        TabOrder = 9
        Width = 121
      end
      object cxCBSwichAfterCrypt: TcxCheckBox
        Left = 99
        Top = 90
        Caption = 'Switch after crypt'
        Properties.OnChange = cxCBSwichAfterCryptPropertiesChange
        TabOrder = 11
        Transparent = True
      end
      object cxCBModyBeforeCrypt: TcxCheckBox
        Left = 99
        Top = 63
        Caption = 'Mody before crypt'
        Properties.OnChange = cxCBModyBeforeCryptPropertiesChange
        TabOrder = 10
        Transparent = True
      end
      object cxLDirectlinksView: TcxLabel
        Left = 16
        Top = 204
        Caption = 'Directlinks view:'
        Transparent = True
      end
      object cxCOBDirectlinksView: TcxComboBox
        Left = 16
        Top = 224
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'Grid'
          'Icon')
        Properties.OnChange = cxCOBDirectlinksViewPropertiesChange
        TabOrder = 4
        Width = 49
      end
    end
    object cxTSDatabase: TcxTabSheet
      Caption = 'Database'
      ImageIndex = 3
      TabVisible = False
      DesignSize = (
        598
        307)
      object lAddDatabase: TLabel
        Left = 17
        Top = 291
        Width = 31
        Height = 13
        Cursor = crHandPoint
        Anchors = [akLeft, akBottom]
        Caption = 'Add...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        Transparent = True
        OnClick = lAddDatabaseClick
        ExplicitTop = 269
      end
      object lRemoveDatabase: TLabel
        Left = 98
        Top = 291
        Width = 39
        Height = 13
        Cursor = crHandPoint
        Anchors = [akLeft, akBottom]
        Caption = 'Remove'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        Transparent = True
        OnClick = lRemoveDatabaseClick
        ExplicitTop = 271
      end
      object cxCLBDatabase: TcxCheckListBox
        Left = 16
        Top = 16
        Width = 121
        Height = 269
        Anchors = [akLeft, akTop, akBottom]
        Items = <>
        TabOrder = 0
        OnClick = cxCLBDatabaseClick
        OnClickCheck = cxCLBDatabaseClickCheck
      end
      object pDatabaseSettings: TPanel
        Left = 143
        Top = 16
        Width = 452
        Height = 288
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
        object lDatabaseType: TLabel
          Left = 127
          Top = 0
          Width = 64
          Height = 13
          Caption = 'Connectivity:'
          Transparent = True
        end
        object lDatabaseName: TLabel
          Left = 0
          Top = 0
          Width = 31
          Height = 13
          Caption = 'Name:'
          Transparent = True
        end
        object lDatabaseServer: TLabel
          Left = 0
          Top = 43
          Width = 36
          Height = 13
          Caption = 'Server:'
          Transparent = True
        end
        object lDatabaseDatabase: TLabel
          Left = 127
          Top = 43
          Width = 50
          Height = 13
          Caption = 'Database:'
          Transparent = True
        end
        object lDatabaseUsername: TLabel
          Left = 0
          Top = 86
          Width = 52
          Height = 13
          Caption = 'Username:'
          Transparent = True
        end
        object lDatabasePassword: TLabel
          Left = 127
          Top = 86
          Width = 50
          Height = 13
          Caption = 'Password:'
          Transparent = True
        end
        object lDatabasePort: TLabel
          Left = 254
          Top = 43
          Width = 24
          Height = 13
          Caption = 'Port:'
          Transparent = True
        end
        object cxTEDatabaseName: TcxTextEdit
          Left = 0
          Top = 16
          Properties.OnChange = cxTEDatabaseNamePropertiesChange
          TabOrder = 0
          Width = 121
        end
        object cxCOBDatabaseType: TcxComboBox
          Left = 127
          Top = 16
          Properties.DropDownListStyle = lsFixedList
          Properties.OnChange = cxCOBDatabaseTypePropertiesChange
          TabOrder = 1
          Width = 121
        end
        object cxTEDatabaseServer: TcxTextEdit
          Left = 0
          Top = 59
          Properties.OnChange = cxTEDatabaseServerPropertiesChange
          TabOrder = 2
          Width = 121
        end
        object cxTEDatabaseDatabase: TcxTextEdit
          Left = 127
          Top = 59
          Properties.OnChange = cxTEDatabaseDatabasePropertiesChange
          TabOrder = 3
          Width = 121
        end
        object cxTEDatabaseUsername: TcxTextEdit
          Left = 0
          Top = 102
          Properties.OnChange = cxTEDatabaseUsernamePropertiesChange
          TabOrder = 5
          Width = 121
        end
        object cxTEDatabasePassword: TcxTextEdit
          Left = 127
          Top = 102
          Properties.EchoMode = eemPassword
          Properties.OnChange = cxTEDatabasePasswordPropertiesChange
          TabOrder = 6
          Width = 121
        end
        object cxCOBDatabasePort: TcxComboBox
          Left = 254
          Top = 59
          Properties.DropDownListStyle = lsEditFixedList
          Properties.Items.Strings = (
            '3306'
            '3307')
          Properties.MaskKind = emkRegExpr
          Properties.EditMask = '\d+'
          Properties.MaxLength = 0
          Properties.OnChange = cxCOBDatabasePortPropertiesChange
          TabOrder = 4
          Width = 121
        end
      end
    end
    object cxTSControls: TcxTabSheet
      Caption = 'Controls'
      ImageIndex = 4
      DesignSize = (
        598
        307)
      object cxPCControls: TcxPageControl
        Left = 3
        Top = 3
        Width = 592
        Height = 301
        Anchors = [akLeft, akTop, akRight, akBottom]
        Focusable = False
        TabOrder = 0
        Properties.ActivePage = cxTSControls_
        Properties.CustomButtons.Buttons = <>
        Properties.HotTrack = True
        ClientRectBottom = 297
        ClientRectLeft = 4
        ClientRectRight = 588
        ClientRectTop = 24
        object cxTSControls_: TcxTabSheet
          Caption = 'Controls'
          ImageIndex = 0
          DesignSize = (
            584
            273)
          object cxTCControls: TcxTabControl
            Left = 0
            Top = 0
            Width = 584
            Height = 273
            Anchors = [akLeft, akTop, akRight, akBottom]
            Focusable = False
            TabOrder = 0
            Properties.CustomButtons.Buttons = <>
            Properties.HotTrack = True
            Properties.NavigatorPosition = npRightBottom
            Properties.Options = [pcoAlwaysShowGoDialogButton, pcoFixedTabWidthWhenRotated, pcoGradient, pcoGradientClientArea, pcoRedrawOnResize]
            Properties.Rotate = True
            Properties.TabPosition = tpLeft
            OnChange = cxTCControlsChange
            DesignSize = (
              584
              273)
            ClientRectBottom = 269
            ClientRectLeft = 4
            ClientRectRight = 580
            ClientRectTop = 4
            object pControlsSettings: TPanel
              Left = 110
              Top = 99
              Width = 471
              Height = 171
              Anchors = [akLeft, akTop, akRight, akBottom]
              BevelOuter = bvNone
              DoubleBuffered = True
              ParentDoubleBuffered = False
              TabOrder = 1
              Visible = False
              DesignSize = (
                471
                171)
              object lControlsHelp: TLabel
                Left = 0
                Top = 43
                Width = 48
                Height = 13
                Caption = 'Help text:'
                Transparent = True
              end
              object lControlsTitle: TLabel
                Left = 0
                Top = 0
                Width = 24
                Height = 13
                Caption = 'Title:'
                Transparent = True
              end
              object lControlsValue: TLabel
                Left = 0
                Top = 86
                Width = 30
                Height = 13
                Caption = 'Value:'
                Transparent = True
              end
              object lControlsItems: TLabel
                Left = 0
                Top = 129
                Width = 31
                Height = 13
                Caption = 'Items:'
                Transparent = True
              end
              object lControlsItemsSort: TLabel
                Left = 37
                Top = 129
                Width = 20
                Height = 13
                Cursor = crHandPoint
                Caption = 'Sort'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = [fsUnderline]
                ParentFont = False
                Transparent = True
                Visible = False
                OnClick = lControlsItemsSortClick
              end
              object lControlsItemsAdd: TLabel
                Left = 90
                Top = 129
                Width = 16
                Height = 13
                Cursor = crHandPoint
                Hint = 'Add'
                Caption = '[+]'
                ParentShowHint = False
                ShowHint = True
                Transparent = True
                OnClick = lControlsItemsAddClick
              end
              object lControlsItemsEdit: TLabel
                Left = 112
                Top = 129
                Width = 20
                Height = 13
                Cursor = crHandPoint
                Hint = 'Edit'
                Caption = '[...]'
                ParentShowHint = False
                ShowHint = True
                Transparent = True
                OnClick = lControlsItemsEditClick
              end
              object lControlsItemsRemove: TLabel
                Left = 138
                Top = 129
                Width = 12
                Height = 13
                Cursor = crHandPoint
                Hint = 'Remove'
                Caption = '[-]'
                ParentShowHint = False
                ShowHint = True
                OnClick = lControlsItemsRemoveClick
              end
              object cxTEControlsTitle: TcxTextEdit
                Left = 0
                Top = 16
                Anchors = [akLeft, akTop, akRight]
                Properties.OnValidate = cxTEControlsTitlePropertiesValidate
                TabOrder = 0
                Width = 471
              end
              object cxBEControlsHelp: TcxBlobEdit
                Left = 0
                Top = 59
                Anchors = [akLeft, akTop, akRight]
                Properties.BlobEditKind = bekMemo
                Properties.BlobPaintStyle = bpsText
                Properties.MemoScrollBars = ssBoth
                Properties.OnInitPopup = cxBEControlsHelpPropertiesInitPopup
                Properties.OnValidate = cxBEControlsHelpPropertiesValidate
                TabOrder = 1
                Width = 471
              end
              object cxTEControlsValue: TcxTextEdit
                Left = 0
                Top = 102
                Anchors = [akLeft, akTop, akRight]
                Properties.OnValidate = cxTEControlsValuePropertiesValidate
                TabOrder = 2
                Width = 471
              end
              object cxMControlsItemAlsoKnownAs: TcxMemo
                Left = 156
                Top = 145
                Anchors = [akLeft, akTop, akRight, akBottom]
                Properties.OnValidate = cxMControlsItemAlsoKnownAsPropertiesValidate
                TabOrder = 3
                Visible = False
                Height = 26
                Width = 315
              end
              object cxLBControlsItems: TcxListBox
                Left = 0
                Top = 145
                Width = 150
                Height = 26
                Anchors = [akLeft, akTop, akBottom]
                ItemHeight = 13
                TabOrder = 4
                OnClick = cxLBControlsItemsClick
                OnDblClick = cxLBControlsItemsDblClick
                OnDragOver = cxLBControlsItemsDragOver
                OnEndDrag = cxLBControlsItemsEndDrag
              end
            end
            object pControlTemplateType: TPanel
              Left = 110
              Top = 3
              Width = 471
              Height = 90
              Anchors = [akLeft, akTop, akRight]
              BevelOuter = bvNone
              DoubleBuffered = True
              ParentDoubleBuffered = False
              TabOrder = 0
              Visible = False
              DesignSize = (
                471
                90)
              object lControlsTemplateTypeSelectAll: TLabel
                Left = 455
                Top = 0
                Width = 16
                Height = 13
                Cursor = crHandPoint
                Hint = 'Select all'
                Anchors = [akTop, akRight]
                Caption = '[+]'
                ParentShowHint = False
                ShowHint = True
                Transparent = True
                OnClick = lControlsTemplateTypeSelectAllClick
                OnMouseEnter = lControlsTemplateTypeSelectAllMouseEnter
                OnMouseLeave = lControlsTemplateTypeSelectAllMouseLeave
                ExplicitLeft = 464
              end
              object lControlsTemplateTypeSelectNone: TLabel
                Left = 455
                Top = 40
                Width = 12
                Height = 13
                Cursor = crHandPoint
                Hint = 'Select none'
                Anchors = [akTop, akRight]
                Caption = '[-]'
                ParentShowHint = False
                ShowHint = True
                Transparent = True
                OnClick = lControlsTemplateTypeSelectNoneClick
                OnMouseEnter = lControlsTemplateTypeSelectNoneMouseEnter
                OnMouseLeave = lControlsTemplateTypeSelectNoneMouseLeave
                ExplicitLeft = 464
              end
              object lControlsTemplateTypeSwitch: TLabel
                Left = 455
                Top = 19
                Width = 12
                Height = 13
                Cursor = crHandPoint
                Hint = 'Invert'
                Anchors = [akTop, akRight]
                Caption = '[/]'
                ParentShowHint = False
                ShowHint = True
                Transparent = True
                OnClick = lControlsTemplateTypeSwitchClick
                OnMouseEnter = lControlsTemplateTypeSwitchMouseEnter
                OnMouseLeave = lControlsTemplateTypeSwitchMouseLeave
                ExplicitLeft = 464
              end
              object cxLVControlsTemplateType: TcxListView
                Left = 0
                Top = 0
                Width = 449
                Height = 90
                Anchors = [akLeft, akTop, akRight, akBottom]
                Columns = <>
                HideSelection = False
                MultiSelect = True
                ReadOnly = True
                ShowWorkAreas = True
                TabOrder = 0
                ViewStyle = vsSmallIcon
                OnSelectItem = cxLVControlsTemplateTypeSelectItem
              end
            end
          end
        end
        object cxTSControlsSettings: TcxTabSheet
          Caption = 'Settings'
          ImageIndex = 1
          object cxCBControlsIRichEditWrapText: TcxCheckBox
            Left = 16
            Top = 16
            Caption = 'IRichEdit wrap text'
            Properties.OnChange = cxCBControlsIRichEditWrapTextPropertiesChange
            TabOrder = 0
          end
          object cxBResetControls: TcxButton
            Left = 16
            Top = 90
            Width = 121
            Height = 25
            Caption = 'Reset'
            TabOrder = 3
            OnClick = cxBResetControlsClick
          end
          object cxSEDropDownRows: TcxSpinEdit
            Left = 16
            Top = 63
            Properties.MinValue = 1.000000000000000000
            Properties.OnChange = cxSEDropDownRowsPropertiesChange
            TabOrder = 1
            Value = 8
            Width = 41
          end
          object cxLDropDownRows: TcxLabel
            Left = 16
            Top = 43
            Caption = 'DropDown rows:'
            Transparent = True
          end
        end
      end
    end
    object cxTSHTTP: TcxTabSheet
      Caption = 'HTTP'
      ImageIndex = 5
      object cxGBProxy: TcxGroupBox
        Left = 196
        Top = 16
        Caption = 'Proxy'
        TabOrder = 2
        Height = 273
        Width = 399
        object cxCGEnableProxyAt: TcxCheckGroup
          Left = 16
          Top = 155
          Caption = 'Use Proxy for:'
          Properties.Columns = 2
          Properties.EditValueFormat = cvfInteger
          Properties.Items = <
            item
              Caption = 'Main'
            end
            item
              Caption = 'CMS'
            end
            item
              Caption = 'Crawler'
            end
            item
              Caption = 'Crypter'
            end
            item
              Caption = 'File Hoster'
            end
            item
              Caption = 'Image Hoster'
            end>
          Properties.OnChange = cxCGEnableProxyAtPropertiesChange
          Style.BorderStyle = ebsUltraFlat
          TabOrder = 4
          Height = 91
          Width = 201
        end
        object cxLProxyServerName: TcxLabel
          Left = 103
          Top = 16
          Caption = 'Server name:'
          Transparent = True
        end
        object cxLProxyServerPort: TcxLabel
          Left = 16
          Top = 63
          Caption = 'Server port:'
          Transparent = True
        end
        object cxTEProxyServername: TcxTextEdit
          Left = 103
          Top = 36
          Properties.OnChange = cxTEProxyServernamePropertiesChange
          TabOrder = 1
          Width = 248
        end
        object cxLProxyServerType: TcxLabel
          Left = 16
          Top = 16
          Caption = 'Server type:'
          Transparent = True
        end
        object cxCOBProxyServerType: TcxComboBox
          Left = 16
          Top = 36
          Properties.DropDownListStyle = lsFixedList
          Properties.Items.Strings = (
            'HTTP'
            'SOCKS 4'
            'SOCKS 4 (a)'
            'SOCKS 5')
          Properties.OnChange = cxCOBProxyServerTypePropertiesChange
          TabOrder = 0
          Width = 81
        end
        object cxCOBProxyServerPort: TcxComboBox
          Left = 16
          Top = 83
          Properties.Items.Strings = (
            '80'
            '1080'
            '3128'
            '8080'
            '27977')
          Properties.MaskKind = emkRegExpr
          Properties.EditMask = '\d+'
          Properties.MaxLength = 0
          Properties.OnChange = cxCOBProxyServerPortPropertiesChange
          TabOrder = 2
          Width = 81
        end
        object cxGBProxyRequireAuthentication: TcxGroupBox
          Left = 103
          Top = 67
          Caption = '                                         '
          TabOrder = 5
          Height = 74
          Width = 280
        end
        object cxCGBProxyRequireAuthentication: TdxCheckGroupBox
          Left = 103
          Top = 67
          Caption = 'Require authentication'
          Properties.OnChange = cxCGBProxyRequireAuthenticationPropertiesChange
          TabOrder = 3
          Height = 74
          Width = 282
          object cxLProxyAccountName: TcxLabel
            Left = 16
            Top = 16
            Caption = 'Account name:'
            Transparent = True
          end
          object cxLProxyAccountPassword: TcxLabel
            Left = 142
            Top = 16
            Caption = 'Account password:'
            Transparent = True
          end
          object cxTEProxyAccountName: TcxTextEdit
            Left = 15
            Top = 36
            Properties.OnChange = cxTEProxyAccountNamePropertiesChange
            TabOrder = 2
            Width = 121
          end
          object cxTEProxyAccountPassword: TcxTextEdit
            Left = 142
            Top = 36
            Properties.EchoMode = eemPassword
            Properties.PasswordChar = '*'
            Properties.OnChange = cxTEProxyAccountPasswordPropertiesChange
            TabOrder = 3
            Width = 121
          end
        end
      end
      object cxGBTimeout: TcxGroupBox
        Left = 3
        Top = 95
        Caption = 'Timeout'
        TabOrder = 1
        Height = 120
        Width = 187
        object cxLConnectTimeout: TcxLabel
          Left = 16
          Top = 16
          Caption = 'Connect timeout:'
          Transparent = True
        end
        object cxLReadTimeout: TcxLabel
          Left = 16
          Top = 63
          Caption = 'Read timeout:'
          Transparent = True
        end
        object cxSEConnectTimeout: TcxSpinEdit
          Left = 16
          Top = 36
          Properties.AssignedValues.MinValue = True
          Properties.Increment = 1000.000000000000000000
          Properties.LargeIncrement = 5000.000000000000000000
          Properties.MaxValue = 60000.000000000000000000
          Properties.OnChange = cxSEConnectTimeoutPropertiesChange
          TabOrder = 1
          Width = 57
        end
        object cxLConnectTimeoutMSec: TcxLabel
          Left = 79
          Top = 37
          Caption = '[msec, 0 = infinitely]'
          Transparent = True
        end
        object cxSEReadTimeout: TcxSpinEdit
          Left = 16
          Top = 83
          Properties.AssignedValues.MinValue = True
          Properties.Increment = 1000.000000000000000000
          Properties.LargeIncrement = 5000.000000000000000000
          Properties.MaxValue = 60000.000000000000000000
          Properties.OnChange = cxSEReadTimeoutPropertiesChange
          TabOrder = 4
          Width = 57
        end
        object cxLReadTimeoutMSec: TcxLabel
          Left = 79
          Top = 84
          Caption = '[msec, 0 = infinitely]'
          Transparent = True
        end
      end
      object cxGBManager: TcxGroupBox
        Left = 3
        Top = 16
        Caption = 'Manager'
        TabOrder = 0
        Height = 73
        Width = 187
        object cxLMaxSimultaneousConnections: TcxLabel
          Left = 16
          Top = 16
          Caption = 'Max simultaneous connections:'
          Transparent = True
        end
        object cxSPMaxSimultaneousConnections: TcxSpinEdit
          Left = 16
          Top = 36
          Properties.MaxValue = 99.000000000000000000
          Properties.MinValue = 1.000000000000000000
          Properties.OnChange = cxSPMaxSimultaneousConnectionsPropertiesChange
          TabOrder = 1
          Value = 1
          Width = 49
        end
      end
    end
    object cxTSPublish: TcxTabSheet
      Caption = 'Publish'
      ImageIndex = 4
      object cxLPublishMaxCount: TcxLabel
        Left = 16
        Top = 16
        Caption = 'Maximum publish rate:'
        Transparent = True
      end
      object cxLPublishDelaybetweenUploads: TcxLabel
        Left = 16
        Top = 63
        Caption = 'Delay between uploads:'
        Transparent = True
      end
      object cxLRetryCount: TcxLabel
        Left = 16
        Top = 110
        Caption = 'Retry count:'
        Transparent = True
      end
      object cxSEPublishMaxCount: TcxSpinEdit
        Left = 16
        Top = 36
        Properties.LargeIncrement = 5.000000000000000000
        Properties.MaxValue = 25.000000000000000000
        Properties.MinValue = 1.000000000000000000
        Properties.OnChange = cxSEPublishMaxCountPropertiesChange
        TabOrder = 1
        Value = 2
        Width = 65
      end
      object cxSEPublishDelaybetweenUploads: TcxSpinEdit
        Left = 16
        Top = 83
        ParentShowHint = False
        Properties.AssignedValues.MinValue = True
        Properties.Increment = 10000.000000000000000000
        Properties.LargeIncrement = 30000.000000000000000000
        Properties.MaxValue = 1800000.000000000000000000
        Properties.OnChange = cxSEPublishDelaybetweenUploadsPropertiesChange
        ShowHint = True
        TabOrder = 3
        Width = 65
      end
      object cxLPublishDelaybetweenUploadsMSec: TcxLabel
        Left = 87
        Top = 84
        Caption = '[msec, 0 = disabled]'
        Transparent = True
      end
      object cxSERetryCount: TcxSpinEdit
        Left = 16
        Top = 130
        Properties.AssignedValues.MinValue = True
        Properties.LargeIncrement = 5.000000000000000000
        Properties.MaxValue = 10.000000000000000000
        Properties.OnChange = cxSERetryCountPropertiesChange
        TabOrder = 6
        Value = 2
        Width = 65
      end
    end
    object cxTSLog: TcxTabSheet
      Caption = 'Log'
      ImageIndex = 6
      object cxGBLog: TcxGroupBox
        Left = 3
        Top = 16
        Caption = 'Application log'
        TabOrder = 0
        Height = 73
        Width = 187
        object cxLMaxLogEntries: TcxLabel
          Left = 16
          Top = 16
          Caption = 'Max log entries'
          Transparent = True
        end
        object cxSEMaxLogEntries: TcxSpinEdit
          Left = 16
          Top = 36
          Properties.AssignedValues.MinValue = True
          Properties.MaxValue = 500.000000000000000000
          Properties.OnChange = cxSEMaxLogEntriesPropertiesChange
          TabOrder = 1
          Value = 200
          Width = 49
        end
        object cxLMaxLogEntriesInfo: TcxLabel
          Left = 71
          Top = 37
          Caption = '[0 = disabled]'
          Transparent = True
        end
      end
      object cxBGHTTPLog: TcxGroupBox
        Left = 3
        Top = 95
        Caption = 'HTTP log'
        TabOrder = 1
        Height = 73
        Width = 187
        object cxLMaxHTTPLogEntries: TcxLabel
          Left = 16
          Top = 16
          Caption = 'Max log entries'
          Transparent = True
        end
        object cxSEMaxHTTPLogEntries: TcxSpinEdit
          Left = 16
          Top = 36
          Properties.AssignedValues.MinValue = True
          Properties.MaxValue = 500.000000000000000000
          Properties.OnChange = cxSEMaxHTTPLogEntriesPropertiesChange
          TabOrder = 1
          Value = 15
          Width = 49
        end
        object cxLMaxHTTPLogEntriesInfo: TcxLabel
          Left = 71
          Top = 37
          Caption = '[0 = disabled]'
          Transparent = True
        end
      end
    end
  end
  object cxCBSaveOnClose: TcxCheckBox
    Left = 8
    Top = 345
    Anchors = [akLeft, akBottom]
    Caption = 'Save On Close'
    TabOrder = 1
    Transparent = True
    OnClick = cxCBSaveOnCloseClick
  end
  object cxBExportSettings: TcxButton
    Left = 442
    Top = 341
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Export'
    TabOrder = 2
    OnClick = cxBExportSettingsClick
  end
end
