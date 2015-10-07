object HTTPLogger: THTTPLogger
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'HTTPLogger'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  DesignSize = (
    635
    300)
  PixelsPerInch = 96
  TextHeight = 13
  object cxGrid: TcxGrid
    Left = 8
    Top = 8
    Width = 619
    Height = 284
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tvHTTPProcess: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.CellHints = True
      OptionsSelection.CellSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object tvHTTPProcessUniqueID: TcxGridColumn
        Caption = 'ID'
        PropertiesClassName = 'TcxLabelProperties'
        MinWidth = 30
        Options.HorzSizing = False
        Width = 30
      end
      object tvHTTPProcessMethod: TcxGridColumn
        Caption = 'Method'
        PropertiesClassName = 'TcxLabelProperties'
        MinWidth = 50
        Options.HorzSizing = False
        Width = 50
      end
      object tvHTTPProcessURI: TcxGridColumn
        Caption = 'URI'
        PropertiesClassName = 'TcxTextEditProperties'
        Width = 413
      end
      object tvHTTPProcessStatusCode: TcxGridColumn
        Caption = 'Status code'
        PropertiesClassName = 'TcxLabelProperties'
        MinWidth = 64
        Options.HorzSizing = False
      end
    end
    object tvHTTPRequest: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.CellHints = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object tvHTTPRequestName: TcxGridColumn
        Caption = 'Name'
        PropertiesClassName = 'TcxLabelProperties'
        MinWidth = 100
        Options.HorzSizing = False
        Width = 100
      end
      object tvHTTPRequestValue: TcxGridColumn
        Caption = 'Value'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekMemo
        Properties.BlobPaintStyle = bpsText
        Properties.ImmediateDropDownWhenActivated = False
        Properties.ImmediateDropDownWhenKeyPressed = False
        Properties.MemoScrollBars = ssBoth
        Properties.PopupWidth = 400
        Properties.ReadOnly = True
      end
    end
    object tvHTTPParams: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.CellHints = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object tvHTTPParamsColumnName: TcxGridColumn
        Caption = 'Name'
        PropertiesClassName = 'TcxTextEditProperties'
      end
      object tvHTTPParamsColumnValue: TcxGridColumn
        Caption = 'Value'
        PropertiesClassName = 'TcxTextEditProperties'
      end
    end
    object tvHTTPResponse: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.CellHints = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object tvHTTPResponseName: TcxGridColumn
        Caption = 'Name'
        PropertiesClassName = 'TcxLabelProperties'
        MinWidth = 100
        Options.HorzSizing = False
        Width = 100
      end
      object tvHTTPResponseValue: TcxGridColumn
        Caption = 'Value'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekMemo
        Properties.BlobPaintStyle = bpsText
        Properties.ImmediateDropDownWhenActivated = False
        Properties.ImmediateDropDownWhenKeyPressed = False
        Properties.MemoScrollBars = ssBoth
        Properties.PopupWidth = 450
        Properties.ReadOnly = True
      end
    end
    object HTTPProcess: TcxGridLevel
      Caption = 'HTTPProcess'
      GridView = tvHTTPProcess
      Options.DetailTabsPosition = dtpTop
      object HTTPRequest: TcxGridLevel
        Caption = 'Request'
        GridView = tvHTTPRequest
      end
      object HTTPParams: TcxGridLevel
        Caption = 'Params'
        GridView = tvHTTPParams
      end
      object HTTPResponse: TcxGridLevel
        Caption = 'Response'
        GridView = tvHTTPResponse
      end
    end
  end
end
