object fLinklist: TfLinklist
  Left = 0
  Top = 0
  Width = 200
  Height = 350
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Padding.Left = 8
  Padding.Top = 5
  Padding.Right = 8
  Padding.Bottom = 5
  TabOrder = 0
  OnResize = FrameResize
  object lfLinklist: TcxListView
    Left = 8
    Top = 5
    Width = 184
    Height = 340
    Align = alClient
    Columns = <
      item
        MinWidth = 100
        Width = 180
      end>
    HideSelection = False
    HotTrack = True
    HoverTime = 0
    ParentColor = True
    PopupMenu = pmAddLink
    RowSelect = True
    ShowColumnHeaders = False
    Style.HotTrack = True
    Style.TransparentBorder = True
    StyleFocused.TextStyle = []
    StyleHot.TextStyle = []
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = lfLinklistClick
  end
  object pmAddLink: TPopupMenu
    OnPopup = pmAddLinkPopup
    Left = 16
    Top = 320
    object nAddLink: TMenuItem
      Caption = 'Add link'
      ImageIndex = 0
      OnClick = nAddLinkClick
    end
    object nEditLink: TMenuItem
      Caption = 'Edit link'
      Enabled = False
      ImageIndex = 1
      OnClick = nEditLinkClick
    end
    object nRemoveLink: TMenuItem
      Caption = 'Remove link'
      Enabled = False
      ImageIndex = 2
      OnClick = nRemoveLinkClick
    end
  end
end
