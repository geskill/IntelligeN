object LicenceInfo: TLicenceInfo
  Left = 0
  Top = 0
  Caption = 'licence history'
  ClientHeight = 331
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  DesignSize = (
    417
    331)
  PixelsPerInch = 96
  TextHeight = 13
  object cxGrid: TcxGrid
    Left = 8
    Top = 8
    Width = 401
    Height = 284
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object cxGridCardView1: TcxGridCardView
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsSelection.CellSelect = False
      OptionsSelection.HideSelection = True
      OptionsView.CardAutoWidth = True
      OptionsView.CardIndent = 7
      OptionsView.CardWidth = 218
      object cxGridCardView1Row1: TcxGridCardViewRow
        Caption = 'Key'
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.ReadOnly = True
        Kind = rkCategory
        Options.Filtering = False
        Position.BeginsLayer = True
      end
      object cxGridCardView1Row2: TcxGridCardViewRow
        Caption = 'Date'
        PropertiesClassName = 'TcxLabelProperties'
        Options.Filtering = False
        Position.BeginsLayer = True
      end
      object cxGridCardView1Row3: TcxGridCardViewRow
        Caption = 'Days'
        PropertiesClassName = 'TcxLabelProperties'
        Options.Filtering = False
        Position.BeginsLayer = True
      end
      object cxGridCardView1Row4: TcxGridCardViewRow
        Caption = 'Type'
        PropertiesClassName = 'TcxLabelProperties'
        Options.Filtering = False
        Position.BeginsLayer = True
      end
    end
    object cxGridLevel1: TcxGridLevel
      GridView = cxGridCardView1
    end
  end
  object cxBCancel: TcxButton
    Left = 253
    Top = 298
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = cxBCancelClick
  end
  object cxBRefresh: TcxButton
    Left = 334
    Top = 298
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Refresh'
    TabOrder = 2
    OnClick = cxBRefreshClick
  end
end
