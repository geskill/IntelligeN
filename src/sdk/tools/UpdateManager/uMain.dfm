object fMain: TfMain
  Left = 0
  Top = 0
  ActiveControl = JvWizardInteriorPagePublish
  Caption = 'Update Manager'
  ClientHeight = 338
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object JvWizard: TJvWizard
    Left = 0
    Top = 0
    Width = 635
    Height = 338
    ActivePage = JvWizardInteriorPagePublish
    ButtonBarHeight = 42
    ButtonStart.Caption = 'To &Start Page'
    ButtonStart.NumGlyphs = 1
    ButtonStart.Width = 85
    ButtonLast.Caption = 'To &Last Page'
    ButtonLast.NumGlyphs = 1
    ButtonLast.Width = 85
    ButtonBack.Caption = '< &Back'
    ButtonBack.NumGlyphs = 1
    ButtonBack.Width = 75
    ButtonNext.Caption = '&Next >'
    ButtonNext.NumGlyphs = 1
    ButtonNext.Width = 75
    ButtonFinish.Caption = '&Finish'
    ButtonFinish.NumGlyphs = 1
    ButtonFinish.Width = 75
    ButtonCancel.Caption = '&Cancel'
    ButtonCancel.NumGlyphs = 1
    ButtonCancel.ModalResult = 2
    ButtonCancel.Width = 75
    ButtonHelp.Caption = '&Help'
    ButtonHelp.NumGlyphs = 1
    ButtonHelp.Width = 75
    ShowRouteMap = True
    OnCancelButtonClick = JvWizardCancelButtonClick
    DesignSize = (
      635
      338)
    object JvWizardWelcomePage: TJvWizardWelcomePage
      Header.Title.Color = clNone
      Header.Title.Text = 'Welcome'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 
        'Select your IntelligeN developer edition which will be used for ' +
        'making the update from those files.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      EnabledButtons = [bkStart, bkLast, bkBack, bkFinish, bkCancel, bkHelp]
      OnPage = JvWizardWelcomePagePage
      OnNextButtonClick = JvWizardWelcomePageNextButtonClick
      WaterMark.Visible = False
      DesignSize = (
        490
        296)
      object sbSelectRootDir: TSpeedButton
        Left = 446
        Top = 111
        Width = 21
        Height = 21
        Hint = 'select root dir'
        Anchors = [akTop, akRight]
        Caption = '...'
        Enabled = False
        Flat = True
        OnClick = sbSelectRootDirClick
      end
      object eRootDir: TEdit
        Left = 64
        Top = 111
        Width = 376
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 1
        OnChange = eRootDirChange
      end
      object rbAddNewPath: TRadioButton
        Left = 48
        Top = 88
        Width = 145
        Height = 17
        Caption = 'Add new FileSystem'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbSelectFileSystem
      end
      object rbSelectExisting: TRadioButton
        Left = 48
        Top = 152
        Width = 145
        Height = 17
        Caption = 'Select existing FileSystem'
        TabOrder = 2
        TabStop = True
        OnClick = rbSelectFileSystem
      end
      object lbSelectPath: TListBox
        Left = 64
        Top = 175
        Width = 404
        Height = 74
        Anchors = [akLeft, akTop, akRight, akBottom]
        Enabled = False
        ItemHeight = 13
        TabOrder = 3
        OnClick = lbSelectPathClick
      end
    end
    object JvWizardInteriorPageServer: TJvWizardInteriorPage
      Header.Title.Color = clNone
      Header.Title.Text = 'Server'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'Select a Server which will manage the update.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      EnabledButtons = [bkStart, bkLast, bkBack, bkFinish, bkCancel, bkHelp]
      OnPage = JvWizardInteriorPageServerPage
      OnNextButtonClick = JvWizardInteriorPageServerNextButtonClick
      DesignSize = (
        490
        296)
      object lServerAccessToken: TLabel
        Left = 392
        Top = 98
        Width = 69
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Access Token:'
      end
      object rbAddNewServer: TRadioButton
        Left = 48
        Top = 88
        Width = 145
        Height = 17
        Caption = 'Add new Server'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbSelectServer
      end
      object eServerDir: TEdit
        Left = 64
        Top = 111
        Width = 322
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 1
        Text = 'http://'
        OnChange = eServerDirChange
      end
      object rbSelectExistingServer: TRadioButton
        Left = 48
        Top = 152
        Width = 145
        Height = 17
        Caption = 'Select existing Server'
        TabOrder = 3
        TabStop = True
        OnClick = rbSelectServer
      end
      object lbSelectServer: TListBox
        Left = 64
        Top = 175
        Width = 404
        Height = 74
        Anchors = [akLeft, akTop, akRight, akBottom]
        Enabled = False
        ItemHeight = 13
        TabOrder = 4
        OnClick = lbSelectServerClick
      end
      object eServerAccessToken: TEdit
        Left = 392
        Top = 111
        Width = 76
        Height = 21
        Anchors = [akTop, akRight]
        Enabled = False
        TabOrder = 2
        TextHint = '(optional)'
      end
    end
    object JvWizardInteriorPageServerInfo: TJvWizardInteriorPage
      Header.Title.Color = clNone
      Header.Title.Text = 'Server Info'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 
        'Gathering required informations from the server for making the u' +
        'pdate. Just kick back for a moment.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      EnabledButtons = [bkStart, bkLast, bkBack, bkFinish, bkCancel, bkHelp]
      OnPage = JvWizardInteriorPageServerInfoPage
      DesignSize = (
        490
        296)
      object JvLEDConnectToServer: TJvLED
        Left = 24
        Top = 88
        ColorOff = clSilver
        Status = False
      end
      object lConnectToServer: TLabel
        Left = 47
        Top = 90
        Width = 117
        Height = 13
        Caption = 'Connecting to Server ...'
      end
      object JvLEDRecivingUpdateVersions: TJvLED
        Left = 24
        Top = 120
        ColorOff = clSilver
        Status = False
      end
      object lRecivingUpdateVersions: TLabel
        Left = 47
        Top = 122
        Width = 135
        Height = 13
        Caption = 'Reciving update versions ...'
      end
      object JvLEDRecivingFTPServer: TJvLED
        Left = 24
        Top = 152
        ColorOff = clSilver
        Status = False
      end
      object lRecivingFTPServer: TLabel
        Left = 47
        Top = 154
        Width = 230
        Height = 13
        Caption = 'Reciving FTP store path and login credentials ...'
      end
      object JvLEDRecivingUpdateFiles: TJvLED
        Left = 24
        Top = 184
        ColorOff = clSilver
        Status = False
      end
      object lRecivingUpdateFiles: TLabel
        Left = 47
        Top = 186
        Width = 188
        Height = 13
        Caption = 'Reciving information for update files ...'
      end
      object lServerInfoError: TLabel
        Left = 24
        Top = 264
        Width = 28
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Error:'
        Visible = False
      end
      object eServerInfoError: TEdit
        Left = 58
        Top = 264
        Width = 255
        Height = 13
        Anchors = [akLeft, akBottom]
        BorderStyle = bsNone
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
        Visible = False
      end
    end
    object JvWizardInteriorPageLocalFiles: TJvWizardInteriorPage
      Header.Title.Color = clNone
      Header.Title.Text = 'Local Files'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'Select the files you wish to keep tracking and updating on.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      OnPage = JvWizardInteriorPageLocalFilesPage
      OnExitPage = JvWizardInteriorPageLocalFilesExitPage
      OnNextButtonClick = JvWizardInteriorPageLocalFilesNextButtonClick
      object lFileSystem: TLabel
        AlignWithMargins = True
        Left = 5
        Top = 282
        Width = 480
        Height = 13
        Cursor = crHandPoint
        Margins.Left = 5
        Margins.Top = 1
        Margins.Right = 5
        Margins.Bottom = 1
        Align = alBottom
        OnClick = lFileSystemClick
        ExplicitWidth = 3
      end
      object cxGLocalFiles: TcxGrid
        AlignWithMargins = True
        Left = 3
        Top = 91
        Width = 484
        Height = 187
        Margins.Top = 21
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        TabOrder = 1
        object cxGLocalFilesTableView: TcxGridTableView
          Navigator.Buttons.CustomButtons = <>
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          DataController.OnDataChanged = cxGLocalFilesTableViewDataControllerDataChanged
          OptionsView.ColumnAutoWidth = True
          object cxGLocalFilesTableViewColumn1: TcxGridColumn
            Caption = 'Status'
            PropertiesClassName = 'TcxCheckBoxProperties'
            Properties.ImmediatePost = True
            Properties.NullStyle = nssUnchecked
            Width = 45
          end
          object cxGLocalFilesTableViewColumn2: TcxGridColumn
            Caption = 'Condition'
            PropertiesClassName = 'TcxLabelProperties'
            OnCustomDrawCell = cxGLocalFilesTableViewColumn2CustomDrawCell
            Width = 45
          end
          object cxGLocalFilesTableViewColumn3: TcxGridColumn
            Caption = 'FileName'
            PropertiesClassName = 'TcxLabelProperties'
            Width = 216
          end
          object cxGLocalFilesTableViewColumn4: TcxGridColumn
            Caption = 'Version'
            PropertiesClassName = 'TcxLabelProperties'
          end
          object cxGLocalFilesTableViewColumn5: TcxGridColumn
            Caption = 'Action'
            PropertiesClassName = 'TcxComboBoxProperties'
            Properties.DropDownListStyle = lsFixedList
            Properties.ImmediateDropDownWhenActivated = True
            Properties.ImmediatePost = True
            Properties.ImmediateUpdateText = True
            Properties.ReadOnly = True
            Visible = False
            OnGetPropertiesForEdit = cxGLocalFilesTableViewColumn5GetPropertiesForEdit
          end
          object cxGLocalFilesTableViewColumn6: TcxGridColumn
            Caption = 'Actions'
            PropertiesClassName = 'TcxMemoProperties'
            Visible = False
          end
        end
        object cxGLocalFilesLevel: TcxGridLevel
          GridView = cxGLocalFilesTableView
        end
      end
      object cxCBLocalFilesEnableDisableAll: TcxCheckBox
        Left = 3
        Top = 72
        Caption = 'enable/disable all'
        Properties.ValueChecked = 1
        Properties.ValueGrayed = 2
        Properties.ValueUnchecked = 0
        Properties.OnChange = cxCBLocalFilesEnableDisableAllPropertiesChange
        State = cbsGrayed
        TabOrder = 0
        Transparent = True
      end
    end
    object JvWizardInteriorPageUpdateFiles: TJvWizardInteriorPage
      Header.Title.Color = clNone
      Header.Title.Text = 'Update Files'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'Here you can verify the list of files that will be updated.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      OnPage = JvWizardInteriorPageUpdateFilesPage
      OnNextButtonClick = JvWizardInteriorPageUpdateFilesNextButtonClick
      object cxGUpdateFiles: TcxGrid
        AlignWithMargins = True
        Left = 3
        Top = 72
        Width = 484
        Height = 221
        Margins.Top = 2
        Align = alClient
        Anchors = []
        BevelInner = bvNone
        BevelOuter = bvNone
        TabOrder = 0
        object cxGUpdateFilesTableView: TcxGridTableView
          Navigator.Buttons.CustomButtons = <>
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          DataController.OnDataChanged = cxGLocalFilesTableViewDataControllerDataChanged
          OptionsView.ColumnAutoWidth = True
          object cxGUpdateFilesTableViewColumn1: TcxGridColumn
            Caption = 'Status'
            PropertiesClassName = 'TcxCheckBoxProperties'
            Properties.ImmediatePost = True
            Properties.NullStyle = nssUnchecked
            Visible = False
            Width = 45
          end
          object cxGUpdateFilesTableViewColumn2: TcxGridColumn
            Caption = 'Condition'
            PropertiesClassName = 'TcxLabelProperties'
            Visible = False
            OnCustomDrawCell = cxGLocalFilesTableViewColumn2CustomDrawCell
            Width = 45
          end
          object cxGUpdateFilesTableViewColumn3: TcxGridColumn
            Caption = 'FileName'
            PropertiesClassName = 'TcxLabelProperties'
            Width = 216
          end
          object cxGUpdateFilesTableViewColumn4: TcxGridColumn
            Caption = 'Version'
            PropertiesClassName = 'TcxLabelProperties'
            Visible = False
          end
          object cxGUpdateFilesTableViewColumn5: TcxGridColumn
            Caption = 'Action'
            PropertiesClassName = 'TcxComboBoxProperties'
            Properties.DropDownListStyle = lsFixedList
            Properties.ImmediateDropDownWhenActivated = True
            Properties.ImmediatePost = True
            Properties.ImmediateUpdateText = True
            OnCustomDrawCell = cxGUpdateFilesTableViewColumn5CustomDrawCell
            OnGetPropertiesForEdit = cxGUpdateFilesTableViewColumn6GetPropertiesForEdit
          end
          object cxGUpdateFilesTableViewColumn6: TcxGridColumn
            Caption = 'Actions'
            PropertiesClassName = 'TcxMemoProperties'
            Visible = False
            OnGetPropertiesForEdit = cxGUpdateFilesTableViewColumn6GetPropertiesForEdit
          end
        end
        object cxGUpdateFilesLevel: TcxGridLevel
          GridView = cxGUpdateFilesTableView
        end
      end
    end
    object JvWizardInteriorPageUpdateVersion: TJvWizardInteriorPage
      Header.Title.Color = clNone
      Header.Title.Text = 'Update Version'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'Select a Version which will contain the update.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      EnabledButtons = [bkStart, bkLast, bkBack, bkFinish, bkCancel, bkHelp]
      OnPage = JvWizardInteriorPageUpdateVersionPage
      DesignSize = (
        490
        296)
      object lPreRelease: TLabel
        Left = 252
        Top = 114
        Width = 74
        Height = 13
        Caption = '(PRE-RELEASE)'
        Visible = False
      end
      object rbAddNewVersion: TRadioButton
        Left = 48
        Top = 88
        Width = 145
        Height = 17
        Caption = 'Add new Version'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbSelectVersion
      end
      object rbSelectExistingVersion: TRadioButton
        Left = 48
        Top = 152
        Width = 145
        Height = 17
        Caption = 'Select existing Version'
        TabOrder = 5
        TabStop = True
        OnClick = rbSelectVersion
      end
      object lbSelectVersion: TListBox
        Left = 64
        Top = 175
        Width = 404
        Height = 74
        Anchors = [akLeft, akTop, akRight, akBottom]
        Enabled = False
        ItemHeight = 13
        TabOrder = 6
        OnClick = lbSelectVersionClick
      end
      object cxSEMajorVersion: TcxSpinEdit
        Left = 64
        Top = 111
        Properties.AssignedValues.MinValue = True
        Properties.ValueType = vtInt
        Properties.OnChange = cxSEMajorVersionPropertiesChange
        TabOrder = 1
        Value = 2
        Width = 41
      end
      object cxSEMinorVersion: TcxSpinEdit
        Left = 111
        Top = 111
        Properties.AssignedValues.MinValue = True
        Properties.ValueType = vtInt
        Properties.OnChange = cxSEMinorVersionPropertiesChange
        TabOrder = 2
        Value = 129
        Width = 41
      end
      object cxSEMajorBuild: TcxSpinEdit
        Left = 158
        Top = 111
        Properties.AssignedValues.MinValue = True
        Properties.ValueType = vtInt
        Properties.OnChange = cxSEMajorBuildPropertiesChange
        TabOrder = 3
        Width = 41
      end
      object cxSEMinorBuild: TcxSpinEdit
        Left = 205
        Top = 111
        Properties.AssignedValues.MinValue = True
        Properties.ValueType = vtInt
        Properties.OnChange = cxSEMinorBuildPropertiesChange
        TabOrder = 4
        Width = 41
      end
    end
    object JvWizardInteriorPageUploadFiles: TJvWizardInteriorPage
      Header.Title.Color = clNone
      Header.Title.Text = 'Upload Files'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 
        'Your files are now getting uploaded to the server. Just wait a m' +
        'oment.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      EnabledButtons = [bkStart, bkLast, bkFinish, bkCancel, bkHelp]
      OnPage = JvWizardInteriorPageUploadFilesPage
      DesignSize = (
        490
        296)
      object JvLEDAddVersion: TJvLED
        Left = 24
        Top = 88
        ColorOff = clSilver
        Status = False
      end
      object lAddVersion: TLabel
        Left = 47
        Top = 90
        Width = 208
        Height = 13
        Caption = 'Adding the new version to the database ...'
      end
      object JvLEDAddSystems: TJvLED
        Left = 24
        Top = 120
        ColorOff = clSilver
        Status = False
      end
      object lAddingTheNewSystems: TLabel
        Left = 47
        Top = 122
        Width = 212
        Height = 13
        Caption = 'Adding the new systems to the database ...'
      end
      object JvLEDCompressLocalFiles: TJvLED
        Left = 24
        Top = 152
        ColorOff = clSilver
        Status = False
      end
      object lCompressingLocalFiles: TLabel
        Left = 47
        Top = 154
        Width = 122
        Height = 13
        Caption = 'Compressing local files ...'
      end
      object JvLEDUploadLocalFiles: TJvLED
        Left = 24
        Top = 184
        ColorOff = clSilver
        Status = False
      end
      object lUploadingLocalFiles: TLabel
        Left = 47
        Top = 186
        Width = 108
        Height = 13
        Caption = 'Uploading local files ...'
      end
      object lUploadInfoError: TLabel
        Left = 24
        Top = 264
        Width = 28
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Error:'
        Visible = False
      end
      object pbUploadProgress: TProgressBar
        Left = 47
        Top = 207
        Width = 242
        Height = 17
        TabOrder = 0
        Visible = False
      end
      object eUploadInfoError: TEdit
        Left = 58
        Top = 264
        Width = 255
        Height = 13
        Anchors = [akLeft, akBottom]
        BorderStyle = bsNone
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
        Visible = False
      end
    end
    object JvWizardInteriorPagePublish: TJvWizardInteriorPage
      Header.Title.Color = clNone
      Header.Title.Text = 'Publish'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'The update is ready. Approve now to activate the update.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      VisibleButtons = [bkBack, bkFinish, bkCancel]
      OnFinishButtonClick = JvWizardInteriorPagePublishFinishButtonClick
    end
    object JvWizardRouteMapNodes: TJvWizardRouteMapNodes
      Left = 0
      Top = 0
      Width = 145
      Height = 296
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
  end
end
