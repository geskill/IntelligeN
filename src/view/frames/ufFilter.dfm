object fFilter: TfFilter
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object cxGrid: TcxGrid
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    TabOrder = 0
    object tvSections: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      OptionsView.Header = False
      object tvSectionsColumn1: TcxGridColumn
        PropertiesClassName = 'TcxLabelProperties'
      end
    end
    object cvValues: TcxGridCardView
      OnMouseMove = cvValuesMouseMove
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.CellHints = True
      OptionsView.CardAutoWidth = True
      OptionsView.CardIndent = 7
      object cvValuesRow1: TcxGridCardViewRow
        Caption = 'Website'
        PropertiesClassName = 'TcxLabelProperties'
        Kind = rkCategory
        Position.BeginsLayer = True
      end
      object cvValuesRow2: TcxGridCardViewRow
        Caption = 'Enabled'
        PropertiesClassName = 'TcxCheckBoxProperties'
        Properties.AllowGrayed = True
        Properties.ImmediatePost = True
        Position.BeginsLayer = True
      end
      object cvValuesRow3: TcxGridCardViewRow
        Caption = 'Categories'
        PropertiesClassName = 'TcxCheckComboBoxProperties'
        Properties.Items = <>
        Position.BeginsLayer = True
      end
      object cvValuesRow4: TcxGridCardViewRow
        Caption = 'Filter'
        PropertiesClassName = 'TcxButtonEditProperties'
        Properties.Buttons = <
          item
            Default = True
            Hint = 'Edit'
            Kind = bkEllipsis
          end>
        Properties.OnButtonClick = cvValuesRow4PropertiesButtonClick
        Options.ShowEditButtons = isebAlways
        Position.BeginsLayer = True
      end
    end
    object cxGridLevel1: TcxGridLevel
      GridView = tvSections
      object cxGridLevel2: TcxGridLevel
        GridView = cvValues
      end
    end
  end
end
