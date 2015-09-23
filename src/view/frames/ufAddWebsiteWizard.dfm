object fAddWebsiteWizard: TfAddWebsiteWizard
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  DesignSize = (
    320
    240)
  object pNewWebsite: TPanel
    Left = 0
    Top = 29
    Width = 320
    Height = 177
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 0
    Visible = False
    DesignSize = (
      320
      177)
    object cxLWebsiteURL: TcxLabel
      Left = 8
      Top = 0
      Caption = 'Page URL:'
      Transparent = True
    end
    object cxTEPageURL: TcxTextEdit
      Left = 8
      Top = 20
      Anchors = [akLeft, akTop, akRight]
      Properties.OnChange = cxTEPageURLPropertiesChange
      TabOrder = 1
      Width = 304
    end
    object cxLFormattedURL: TcxLabel
      Left = 8
      Top = 87
      Caption = 'Formatted URL:'
      Transparent = True
    end
    object cxTEFormattedURL: TcxTextEdit
      Left = 8
      Top = 107
      Anchors = [akLeft, akTop, akRight]
      Properties.ReadOnly = True
      Properties.OnChange = cxTEFormattedURLPropertiesChange
      TabOrder = 3
      Width = 277
    end
    object cxCBEditFormattedURL: TcxCheckBox
      Left = 291
      Top = 107
      Hint = 'Activate to edit the URL manually'
      Anchors = [akTop, akRight]
      ParentShowHint = False
      Properties.OnChange = cxCBEditFormattedURLPropertiesChange
      ShowHint = True
      TabOrder = 5
      Transparent = True
    end
    object cxLURLCMS: TcxLabel
      Left = 8
      Top = 134
      Caption = 'CMS:'
      Transparent = True
    end
    object cxCOBURLCMS: TcxComboBox
      Left = 8
      Top = 154
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cxCOBURLCMSPropertiesChange
      TabOrder = 6
      Width = 125
    end
    object cxBDetectCMS: TcxButton
      Left = 270
      Top = 154
      Width = 42
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'detect'
      TabOrder = 9
      OnClick = cxBDetectCMSClick
    end
    object cxLPageURLInfo: TcxLabel
      Left = 10
      Top = 47
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Enter here the URL of the website you like to add, be sure to no' +
        't link on the enter page only!'
      Properties.WordWrap = True
      Transparent = True
      Height = 34
      Width = 300
    end
    object cxLEncoding: TcxLabel
      Left = 139
      Top = 134
      Anchors = [akTop, akRight]
      Caption = 'Encoding:'
      Transparent = True
    end
    object cxCOBEncoding: TcxComboBox
      Left = 139
      Top = 154
      Anchors = [akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cxCOBEncodingPropertiesChange
      TabOrder = 7
      Width = 125
    end
  end
  object pImport: TPanel
    Left = 0
    Top = 29
    Width = 320
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 6
    Visible = False
    DesignSize = (
      320
      177)
    object cxGImport: TcxGrid
      Left = 0
      Top = 0
      Width = 320
      Height = 152
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      object cxGImportTableView1: TcxGridTableView
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
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsView.ShowEditButtons = gsebAlways
        OptionsView.ColumnAutoWidth = True
        OptionsView.GroupByBox = False
        object cxGImportTableView1Column1: TcxGridColumn
          Caption = 'Name'
          PropertiesClassName = 'TcxHyperLinkEditProperties'
        end
        object cxGImportTableView1Column2: TcxGridColumn
          Caption = 'Type'
          PropertiesClassName = 'TcxComboBoxProperties'
          Properties.DropDownListStyle = lsFixedList
          Properties.ImmediatePost = True
          Properties.ImmediateUpdateText = True
          Properties.OnEditValueChanged = cxGImportTableView1Column2PropertiesEditValueChanged
        end
        object cxGImportTableView1Column3: TcxGridColumn
          Caption = 'Filename'
          Visible = False
        end
      end
      object cxGImportLevel1: TcxGridLevel
        GridView = cxGImportTableView1
      end
    end
    object cxRBImportCopy: TcxRadioButton
      Left = 44
      Top = 158
      Width = 113
      Height = 17
      Anchors = [akBottom]
      Caption = 'Copy'
      Checked = True
      TabOrder = 1
      TabStop = True
      Transparent = True
    end
    object cxRBImportReplace: TcxRadioButton
      Left = 163
      Top = 158
      Width = 113
      Height = 17
      Anchors = [akBottom]
      Caption = 'Replace'
      TabOrder = 2
      Transparent = True
    end
  end
  object cxBCancel: TcxButton
    Left = 161
    Top = 212
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    Visible = False
    OnClick = cxBCancelClick
  end
  object cxBNext: TcxButton
    Left = 242
    Top = 212
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Next'
    Default = True
    TabOrder = 2
    OnClick = cxBNextClick
  end
  object cxLAddWebsiteWizard: TcxLabel
    Left = 0
    Top = 0
    Caption = 'Add Website Wizard'
    ParentFont = False
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -16
    Style.Font.Name = 'Tahoma'
    Style.Font.Style = []
    Style.IsFontAssigned = True
    Transparent = True
  end
  object pStart: TPanel
    Left = 0
    Top = 29
    Width = 320
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      320
      177)
    object cxRBImport: TcxRadioButton
      Left = 88
      Top = 103
      Width = 145
      Height = 17
      Anchors = [akLeft, akRight]
      Caption = 'Import'
      TabOrder = 2
    end
    object cxRBNew: TcxRadioButton
      Left = 88
      Top = 57
      Width = 145
      Height = 17
      Anchors = [akLeft, akRight]
      Caption = 'New'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object cxRBLoad: TcxRadioButton
      Left = 88
      Top = 80
      Width = 145
      Height = 17
      Anchors = [akLeft, akRight]
      Caption = 'Load'
      TabOrder = 1
    end
  end
  object pLoad: TPanel
    Left = 0
    Top = 29
    Width = 320
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    DesignSize = (
      320
      177)
    object cxGLoad: TcxGrid
      Left = 0
      Top = 0
      Width = 320
      Height = 177
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      object cxGLoadTableView1: TcxGridTableView
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
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsView.ShowEditButtons = gsebAlways
        OptionsView.ColumnAutoWidth = True
        OptionsView.GroupByBox = False
        object cxGLoadTableView1Column1: TcxGridColumn
          Caption = 'Name'
          PropertiesClassName = 'TcxHyperLinkEditProperties'
        end
        object cxGLoadTableView1Column2: TcxGridColumn
          Caption = 'Type'
          PropertiesClassName = 'TcxComboBoxProperties'
          Properties.DropDownListStyle = lsFixedList
          Properties.ImmediatePost = True
          Properties.ImmediateUpdateText = True
          Properties.OnEditValueChanged = cxGLoadTableView1Column2PropertiesEditValueChanged
        end
        object cxGLoadTableView1Column3: TcxGridColumn
          Caption = 'FileName'
          Visible = False
        end
      end
      object cxGLoadLevel1: TcxGridLevel
        GridView = cxGLoadTableView1
      end
    end
  end
end
