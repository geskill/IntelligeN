object fDatabase: TfDatabase
  Left = 0
  Top = 0
  Width = 274
  Height = 259
  TabOrder = 0
  DesignSize = (
    274
    259)
  object cxGrid: TcxGrid
    Left = 0
    Top = 21
    Width = 274
    Height = 238
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object cxGridDBTableView: TcxGridDBTableView
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
      FilterBox.Visible = fvAlways
      DataController.DataSource = DataSource
      DataController.Filter.Active = True
      DataController.Filter.AutoDataSetFilter = True
      DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.CellHints = True
      OptionsView.ColumnAutoWidth = True
    end
    object cxGridLevel: TcxGridLevel
      GridView = cxGridDBTableView
    end
  end
  object cxCOBActiveDatabaseName: TcxComboBox
    Left = 0
    Top = 0
    Anchors = [akLeft, akTop, akRight]
    Properties.DropDownListStyle = lsFixedList
    Properties.OnChange = cxCOBActiveDatabaseNamePropertiesChange
    Properties.OnInitPopup = cxCOBActiveDatabaseNamePropertiesInitPopup
    TabOrder = 1
    Width = 274
  end
  object DataSource: TDataSource
    Left = 40
    Top = 80
  end
end
