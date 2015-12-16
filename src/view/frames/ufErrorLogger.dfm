object fErrorLogger: TfErrorLogger
  Left = 0
  Top = 0
  Width = 618
  Height = 510
  TabOrder = 0
  object cxGrid: TcxGrid
    Left = 0
    Top = 0
    Width = 618
    Height = 510
    Align = alClient
    TabOrder = 0
    object tvLog: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.CellHints = True
      OptionsSelection.CellSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object tvLogColumnTime: TcxGridColumn
        Caption = 'Time'
        PropertiesClassName = 'TcxLabelProperties'
        Width = 75
      end
      object tvLogColumnMessage: TcxGridColumn
        Caption = 'Message'
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.ReadOnly = True
        Width = 543
      end
    end
    object Log: TcxGridLevel
      GridView = tvLog
    end
  end
end
