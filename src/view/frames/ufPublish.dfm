object fPublish: TfPublish
  Left = 0
  Top = 0
  Width = 200
  Height = 240
  TabOrder = 0
  object cxGrid: TcxGrid
    Left = 0
    Top = 0
    Width = 200
    Height = 240
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = cxcbsNone
    TabOrder = 0
    object tvSections: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.ColumnHorzSizing = False
      OptionsSelection.CellSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.ExpandButtonsForEmptyDetails = False
      OptionsView.GroupByBox = False
      OptionsView.Header = False
      object tvSectionsColumn: TcxGridColumn
        PropertiesClassName = 'TcxLabelProperties'
        Options.Editing = False
        Options.Focusing = False
        Width = 20
        IsCaptionAssigned = True
      end
    end
    object tvValues: TcxGridTableView
      OnMouseMove = tvValuesMouseMove
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      DataController.Data = {
        710000000F00000044617461436F6E74726F6C6C657231020000001200000054
        6378537472696E6756616C75655479706512000000546378537472696E675661
        6C75655479706502000000445855464D54000004000000540072007500650001
        445855464D54000004000000540072007500650001}
      OptionsBehavior.CellHints = True
      OptionsView.ShowEditButtons = gsebAlways
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object tvValuesColumn1: TcxGridColumn
        Caption = 'Active'
        PropertiesClassName = 'TcxCheckBoxProperties'
        Properties.ReadOnly = True
        MinWidth = 36
        Options.HorzSizing = False
        Width = 36
      end
      object tvValuesColumn2: TcxGridColumn
        Caption = 'Website'
        PropertiesClassName = 'TcxButtonEditProperties'
        Properties.Buttons = <
          item
            Caption = 'Options'
            Default = True
            Hint = 'Options'
          end>
        Properties.ReadOnly = True
        Properties.OnButtonClick = tvValuesColumn2PropertiesButtonClick
        SortIndex = 0
        SortOrder = soDescending
        Width = 647
      end
    end
    object Sections: TcxGridLevel
      GridView = tvSections
      object Values: TcxGridLevel
        GridView = tvValues
      end
    end
  end
end
