object fControlEditor: TfControlEditor
  Left = 0
  Top = 0
  Width = 210
  Height = 268
  TabOrder = 0
  object cxGrid: TcxGrid
    Left = 0
    Top = 0
    Width = 210
    Height = 268
    Align = alClient
    BorderStyle = cxcbsNone
    TabOrder = 0
    object cxGridTableView: TcxGridTableView
      OnMouseMove = cxGridTableViewMouseMove
      Navigator.Buttons.CustomButtons = <>
      OnCellDblClick = cxGridTableViewCellDblClick
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsSelection.CellSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.DataRowHeight = 18
      OptionsView.GroupByBox = False
      object cxGridTableViewColumn1: TcxGridColumn
        Caption = 'Crawler'
        PropertiesClassName = 'TcxLabelProperties'
        Width = 68
      end
      object cxGridTableViewColumn2: TcxGridColumn
        Caption = 'Value'
        PropertiesClassName = 'TcxImageProperties'
        Width = 133
      end
    end
    object cxGridLevel: TcxGridLevel
      GridView = cxGridTableView
    end
  end
  object HintTimer: TTimer
    Enabled = False
    Interval = 30
    OnTimer = HintTimerTimer
    Left = 32
    Top = 88
  end
  object cxHintStyleController: TcxHintStyleController
    Global = False
    HintStyleClassName = 'TcxHintStyle'
    HintStyle.CaptionFont.Charset = DEFAULT_CHARSET
    HintStyle.CaptionFont.Color = clWindowText
    HintStyle.CaptionFont.Height = -11
    HintStyle.CaptionFont.Name = 'Tahoma'
    HintStyle.CaptionFont.Style = []
    HintStyle.Font.Charset = DEFAULT_CHARSET
    HintStyle.Font.Color = clWindowText
    HintStyle.Font.Height = -11
    HintStyle.Font.Name = 'Tahoma'
    HintStyle.Font.Style = []
    HintStyle.Standard = True
    Left = 72
    Top = 88
  end
end
