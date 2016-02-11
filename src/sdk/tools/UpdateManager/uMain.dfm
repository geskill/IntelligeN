object fMain: TfMain
  Left = 0
  Top = 0
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
  DesignSize = (
    635
    338)
  PixelsPerInch = 96
  TextHeight = 13
  object WizardControl: TdxWizardControl
    Left = 0
    Top = 0
    Width = 635
    Height = 338
    Buttons.CustomButtons.Buttons = <>
    Buttons.Help.Visible = False
    Buttons.Next.Enabled = False
    InfoPanel.Visible = False
    OnButtonClick = WizardControlButtonClick
    OnPageChanging = WizardControlPageChanging
    object wcpWelcomePage: TdxWizardControlPage
      Header.Description = 
        'Select your IntelligeN developer edition which will be used for ' +
        'making the update from those files.'
      Header.Title = 'Welcome'
      DesignSize = (
        613
        202)
      object sbSelectRootDir: TSpeedButton
        Left = 589
        Top = 26
        Width = 21
        Height = 21
        Hint = 'select root dir'
        Anchors = [akTop, akRight]
        Caption = '...'
        Flat = True
        OnClick = sbSelectRootDirClick
      end
      object eRootDir: TEdit
        Left = 19
        Top = 26
        Width = 564
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = eRootDirChange
      end
      object rbAddNewPath: TRadioButton
        Left = 3
        Top = 3
        Width = 145
        Height = 17
        Caption = 'Add new FileSystem'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbSelectFileSystem
      end
      object rbSelectExisting: TRadioButton
        Left = 3
        Top = 67
        Width = 145
        Height = 17
        Caption = 'Select existing FileSystem'
        TabOrder = 2
        TabStop = True
        OnClick = rbSelectFileSystem
      end
      object lbSelectPath: TListBox
        AlignWithMargins = True
        Left = 19
        Top = 90
        Width = 591
        Height = 109
        Margins.Left = 19
        Margins.Top = 90
        Align = alClient
        Enabled = False
        ItemHeight = 13
        TabOrder = 3
        OnClick = lbSelectPathClick
      end
    end
    object wcpPageServer: TdxWizardControlPage
      Header.Description = 'Select a Server, which will manage the update.'
      Header.Title = 'Server'
      DesignSize = (
        613
        202)
      object lServerAccessToken: TLabel
        Left = 534
        Top = 13
        Width = 69
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Access Token:'
      end
      object rbAddNewServer: TRadioButton
        Left = 3
        Top = 3
        Width = 145
        Height = 17
        Caption = 'Add new Server'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbSelectServer
      end
      object eServerDir: TEdit
        Left = 19
        Top = 26
        Width = 509
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'http://'
        OnChange = eServerDirChange
      end
      object rbSelectExistingServer: TRadioButton
        Left = 3
        Top = 67
        Width = 145
        Height = 17
        Caption = 'Select existing Server'
        TabOrder = 3
        TabStop = True
        OnClick = rbSelectServer
      end
      object lbSelectServer: TListBox
        AlignWithMargins = True
        Left = 19
        Top = 90
        Width = 591
        Height = 109
        Margins.Left = 19
        Margins.Top = 90
        Align = alClient
        Enabled = False
        ItemHeight = 13
        TabOrder = 4
        OnClick = lbSelectServerClick
      end
      object eServerAccessToken: TEdit
        Left = 534
        Top = 26
        Width = 76
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 2
        TextHint = '(optional)'
      end
    end
    object wcpPageServerInfo: TdxWizardControlPage
      Header.Description = 
        'Gathering required informations from the server for making the u' +
        'pdate. Just kick back for a moment.'
      Header.Title = 'Server Info'
      DesignSize = (
        613
        202)
      object lConnectToServer: TLabel
        Left = 47
        Top = 3
        Width = 117
        Height = 13
        Caption = 'Connecting to Server ...'
      end
      object lRecivingUpdateVersions: TLabel
        Left = 47
        Top = 35
        Width = 135
        Height = 13
        Caption = 'Reciving update versions ...'
      end
      object lRecivingFTPServer: TLabel
        Left = 47
        Top = 67
        Width = 230
        Height = 13
        Caption = 'Reciving FTP store path and login credentials ...'
      end
      object lRecivingUpdateFiles: TLabel
        Left = 47
        Top = 99
        Width = 188
        Height = 13
        Caption = 'Reciving information for update files ...'
      end
      object lServerInfoError: TLabel
        Left = 24
        Top = 125
        Width = 28
        Height = 13
        Margins.Top = 190
        Anchors = [akLeft, akBottom]
        Caption = 'Error:'
        Visible = False
        ExplicitTop = 190
      end
      object eServerInfoError: TEdit
        Left = 58
        Top = 125
        Width = 255
        Height = 13
        Margins.Top = 190
        Anchors = [akLeft, akBottom]
        BorderStyle = bsNone
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
        Visible = False
      end
    end
    object wcpLocalFiles: TdxWizardControlPage
      Header.Description = 'Select the files you wish to keep tracking and updating on.'
      Header.Title = 'Local Files'
      object lFileSystem: TLabel
        AlignWithMargins = True
        Left = 5
        Top = 188
        Width = 603
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
        Top = 23
        Width = 607
        Height = 161
        Margins.Top = 23
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
        Top = 3
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
    object wcpUpdateFiles: TdxWizardControlPage
      Header.Description = 'Here you can verify the list of files that will be updated.'
      Header.Title = 'Update Files'
      object cxGUpdateFiles: TcxGrid
        AlignWithMargins = True
        Left = 3
        Top = 2
        Width = 607
        Height = 197
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
    object wcpUpdateVersion: TdxWizardControlPage
      Header.Description = 'Select a Version, which will contain the update.'
      Header.Title = 'Update Version'
      object lPreRelease: TLabel
        Left = 207
        Top = 29
        Width = 74
        Height = 13
        Caption = '(PRE-RELEASE)'
        Visible = False
      end
      object rbAddNewVersion: TRadioButton
        Left = 3
        Top = 3
        Width = 145
        Height = 15
        Caption = 'Add new Version'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbSelectVersion
      end
      object rbSelectExistingVersion: TRadioButton
        Left = 3
        Top = 67
        Width = 145
        Height = 15
        Caption = 'Select existing Version'
        TabOrder = 5
        TabStop = True
        OnClick = rbSelectVersion
      end
      object lbSelectVersion: TListBox
        AlignWithMargins = True
        Left = 19
        Top = 90
        Width = 591
        Height = 109
        Margins.Left = 19
        Margins.Top = 90
        Align = alClient
        Enabled = False
        ItemHeight = 13
        TabOrder = 6
        OnClick = lbSelectVersionClick
      end
      object cxSEMajorVersion: TcxSpinEdit
        Left = 19
        Top = 26
        Properties.AssignedValues.MinValue = True
        Properties.ValueType = vtInt
        Properties.OnChange = cxSEMajorVersionPropertiesChange
        TabOrder = 1
        Value = 2
        Width = 41
      end
      object cxSEMinorVersion: TcxSpinEdit
        Left = 66
        Top = 26
        Properties.AssignedValues.MinValue = True
        Properties.ValueType = vtInt
        Properties.OnChange = cxSEMinorVersionPropertiesChange
        TabOrder = 2
        Value = 129
        Width = 41
      end
      object cxSEMajorBuild: TcxSpinEdit
        Left = 113
        Top = 26
        Properties.AssignedValues.MinValue = True
        Properties.ValueType = vtInt
        Properties.OnChange = cxSEMajorBuildPropertiesChange
        TabOrder = 3
        Width = 41
      end
      object cxSEMinorBuild: TcxSpinEdit
        Left = 160
        Top = 26
        Properties.AssignedValues.MinValue = True
        Properties.ValueType = vtInt
        Properties.OnChange = cxSEMinorBuildPropertiesChange
        TabOrder = 4
        Width = 41
      end
    end
    object wcpUpdateServer: TdxWizardControlPage
      Header.Description = 'Your files are now verified by the server. Just wait a moment...'
      Header.Title = 'Update Server'
      DesignSize = (
        613
        202)
      object lUpdateInfoError: TLabel
        Left = 24
        Top = 82
        Width = 28
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Error:'
        Visible = False
        ExplicitTop = 264
      end
      object lAddingTheNewSystems: TLabel
        Left = 47
        Top = 35
        Width = 212
        Height = 13
        Caption = 'Adding the new systems to the database ...'
      end
      object lAddVersion: TLabel
        Left = 47
        Top = 3
        Width = 208
        Height = 13
        Caption = 'Adding the new version to the database ...'
      end
      object lRetrieveFilesFromServer: TLabel
        Left = 47
        Top = 67
        Width = 231
        Height = 13
        Caption = 'Retrieve files for this version from the server ...'
      end
      object eUpdateInfoError: TEdit
        Left = 58
        Top = 82
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
    object wcpUploadFiles: TdxWizardControlPage
      Header.Description = 
        'Your files are now getting uploaded to the server. Just wait a m' +
        'oment...'
      Header.Title = 'Upload Files'
      DesignSize = (
        613
        202)
      object lCompressingLocalFiles: TLabel
        Left = 47
        Top = 3
        Width = 122
        Height = 13
        Caption = 'Compressing local files ...'
      end
      object lUploadingLocalFiles: TLabel
        Left = 47
        Top = 35
        Width = 108
        Height = 13
        Caption = 'Uploading local files ...'
      end
      object lUploadInfoError: TLabel
        Left = 24
        Top = 82
        Width = 28
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Error:'
        Visible = False
        ExplicitTop = 264
      end
      object pbUploadProgress: TProgressBar
        Left = 66
        Top = 53
        Width = 242
        Height = 17
        TabOrder = 0
        Visible = False
      end
      object eUploadInfoError: TEdit
        Left = 58
        Top = 82
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
    object wcpPublish: TdxWizardControlPage
      Header.Description = 'The update is ready! Approve now to activate the update.'
      Header.Title = 'Publish'
    end
  end
  object bShowHTTPLogger: TButton
    Left = 8
    Top = 302
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'HTTP Logger'
    TabOrder = 1
    Visible = False
    OnClick = bShowHTTPLoggerClick
  end
end
