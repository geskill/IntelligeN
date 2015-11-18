object fPublishQueue: TfPublishQueue
  Left = 0
  Top = 0
  Width = 200
  Height = 240
  TabOrder = 0
  object pTop: TPanel
    Left = 0
    Top = 0
    Width = 200
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      200
      25)
    object cxBStop: TcxButton
      Left = 50
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Stop'
      Caption = 'Stop'
      Enabled = False
      OptionsImage.ImageIndex = 51
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = cxBStopClick
    end
    object cxBStart: TcxButton
      Left = 0
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Start'
      Caption = 'Start'
      Enabled = False
      OptionsImage.ImageIndex = 50
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = cxBStartClick
    end
    object cxBPause: TcxButton
      Left = 25
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Pause'
      Caption = 'Pause'
      OptionsImage.ImageIndex = 49
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = cxBPauseClick
    end
    object cxPBPublishOverallProgress: TcxProgressBar
      Left = 81
      Top = 2
      Anchors = [akLeft, akTop, akRight]
      Properties.SolidTextColor = True
      TabOrder = 3
      Width = 113
    end
  end
  object cxGPublishQueue: TcxGrid
    Left = 0
    Top = 25
    Width = 200
    Height = 215
    Align = alClient
    TabOrder = 1
    object cxGPublishQueueTableView: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.CellHints = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object cxGPublishQueueTableViewColumnPosition: TcxGridColumn
        Caption = 'Position'
        PropertiesClassName = 'TcxLabelProperties'
        Width = 20
      end
      object cxGPublishQueueTableViewColumnDescription: TcxGridColumn
        Caption = 'Description'
        PropertiesClassName = 'TcxLabelProperties'
        Width = 93
      end
      object cxGPublishQueueTableViewColumnProgress: TcxGridColumn
        Caption = 'Progress'
        RepositoryItem = cxERProgressBarOK
        OnGetCellHint = cxGPublishQueueTableViewColumnProgressGetCellHint
        OnGetProperties = cxGPublishQueueTableViewColumnProgressGetProperties
      end
      object cxGPublishQueueTableViewColumnCanel: TcxGridColumn
        Caption = 'Cancel'
        PropertiesClassName = 'TcxButtonEditProperties'
        Properties.Buttons = <
          item
            Caption = 'Cancel'
            Default = True
            ImageIndex = 48
            Hint = 'Cancel'
            Kind = bkGlyph
          end>
        Properties.ViewStyle = vsButtonsOnly
        Properties.OnButtonClick = cxGPublishQueueTableViewColumnCanelPropertiesButtonClick
        MinWidth = 24
        Options.Filtering = False
        Options.ShowEditButtons = isebAlways
        Options.HorzSizing = False
        Options.Moving = False
        Width = 24
      end
      object cxGPublishQueueTableViewColumnHint: TcxGridColumn
        Visible = False
      end
      object cxGPublishQueueTableViewColumnErrorMsg: TcxGridColumn
        Visible = False
      end
    end
    object cxGPublishQueueLevel: TcxGridLevel
      GridView = cxGPublishQueueTableView
    end
  end
  object cxERProgressBar: TcxEditRepository
    Left = 88
    Top = 64
    object cxERProgressBarOK: TcxEditRepositoryProgressBar
      Properties.BarStyle = cxbsAnimation
      Properties.BeginColor = 54056
      Properties.SolidTextColor = True
    end
    object cxERProgressBarError: TcxEditRepositoryProgressBar
      Properties.BarStyle = cxbsAnimation
      Properties.BeginColor = clRed
      Properties.SolidTextColor = True
    end
  end
end
