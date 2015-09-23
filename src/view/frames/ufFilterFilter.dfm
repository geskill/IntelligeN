object fFilterFilter: TfFilterFilter
  Left = 0
  Top = 0
  Width = 200
  Height = 200
  TabOrder = 0
  object cxGrid: TcxGrid
    Left = 0
    Top = 0
    Width = 200
    Height = 200
    TabOrder = 0
    object cxGridTableView1: TcxGridTableView
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
      NewItemRow.Visible = True
      OptionsBehavior.CellHints = True
      OptionsBehavior.FocusCellOnTab = True
      OptionsView.ShowEditButtons = gsebAlways
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object cxGridTableView1Column1: TcxGridColumn
        Caption = 'Name'
        PropertiesClassName = 'TcxComboBoxProperties'
        Properties.OnChange = cxGridTableView1Column1PropertiesChange
      end
      object cxGridTableView1Column2: TcxGridColumn
        Caption = 'Value'
        PropertiesClassName = 'TcxComboBoxProperties'
        Properties.OnInitPopup = cxGridTableView1Column2PropertiesInitPopup
      end
      object cxGridTableView1Column3: TcxGridColumn
        Caption = 'Add/Delete'
        PropertiesClassName = 'TcxButtonEditProperties'
        Properties.Buttons = <
          item
            Default = True
            Kind = bkEllipsis
          end>
        Properties.ViewStyle = vsButtonsOnly
        Properties.OnButtonClick = cxGridTableView1Column3PropertiesButtonClick
        OnGetCellHint = cxGridTableView1Column3GetCellHint
        Options.Filtering = False
        Options.HorzSizing = False
        Width = 20
      end
    end
    object cxGridLevel1: TcxGridLevel
      GridView = cxGridTableView1
    end
  end
end
