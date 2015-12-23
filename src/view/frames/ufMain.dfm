object fMain: TfMain
  Left = 0
  Top = 0
  Width = 618
  Height = 510
  TabOrder = 0
  OnResize = FrameResize
  object dxStatusBar: TdxStatusBar
    Left = 0
    Top = 490
    Width = 618
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarContainerPanelStyle'
        PanelStyle.Container = dxStatusBarContainer3
        Bevel = dxpbNone
        MinWidth = 20
        Width = 125
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        MinWidth = 1
        Width = 1
      end
      item
        PanelStyleClassName = 'TdxStatusBarContainerPanelStyle'
        PanelStyle.Container = dxStatusBarContainer6
        Bevel = dxpbNone
        Width = 138
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        MinWidth = 1
        Width = 1
      end
      item
        PanelStyleClassName = 'TdxStatusBarKeyboardStatePanelStyle'
        PanelStyle.KeyboardStates = [dxksCapsLock, dxksNumLock, dxksInsert]
        PanelStyle.CapsLockKeyAppearance.ActiveCaption = 'CAPS'
        PanelStyle.CapsLockKeyAppearance.InactiveCaption = 'CAPS'
        PanelStyle.NumLockKeyAppearance.ActiveCaption = 'NUM'
        PanelStyle.NumLockKeyAppearance.InactiveCaption = 'NUM'
        PanelStyle.ScrollLockKeyAppearance.ActiveCaption = 'SCRL'
        PanelStyle.ScrollLockKeyAppearance.InactiveCaption = 'SCRL'
        PanelStyle.InsertKeyAppearance.ActiveCaption = 'OVR'
        PanelStyle.InsertKeyAppearance.InactiveCaption = 'INS'
        Bevel = dxpbNone
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    object dxStatusBarContainer3: TdxStatusBarContainerControl
      Left = 0
      Top = 2
      Width = 127
      Height = 18
      object cxPBAutocompletion: TcxProgressBar
        Left = 0
        Top = 0
        Cursor = crHelp
        Hint = 'Autocompletion'
        Align = alClient
        ParentShowHint = False
        Properties.BeginColor = clSkyBlue
        Properties.SolidTextColor = True
        ShowHint = True
        TabOrder = 0
        Width = 127
      end
    end
    object dxStatusBarContainer6: TdxStatusBarContainerControl
      Left = 134
      Top = 2
      Width = 140
      Height = 18
      object cxTCView: TcxTabControl
        Left = 0
        Top = 0
        Width = 140
        Height = 18
        Align = alClient
        Enabled = False
        Focusable = False
        TabOrder = 0
        Properties.CustomButtons.Buttons = <>
        Properties.HotTrack = True
        Properties.TabIndex = 0
        Properties.TabPosition = tpBottom
        Properties.Tabs.Strings = (
          'Data'
          'Design'
          'Preview')
        OnChange = cxTCViewChange
        ClientRectRight = 0
        ClientRectTop = 0
      end
    end
  end
  object pcMain: TcxPageControl
    Left = 0
    Top = 0
    Width = 618
    Height = 490
    Align = alClient
    Focusable = False
    TabOrder = 0
    Properties.AllowTabDragDrop = True
    Properties.CloseButtonMode = cbmActiveAndHoverTabs
    Properties.CustomButtons.Buttons = <>
    Properties.HotTrack = True
    Properties.Images = Main.ILTypeIDs
    Properties.MultiLine = True
    Properties.ShowTabHints = True
    OnCanCloseEx = pcMainCanCloseEx
    OnChange = pcMainChange
    OnGetTabHint = pcMainGetTabHint
    OnMouseDown = pcMainMouseDown
    OnNewTabButtonClick = pcMainNewTabButtonClick
    OnNewTabCreate = pcMainNewTabCreate
    ClientRectBottom = 486
    ClientRectLeft = 4
    ClientRectRight = 614
    ClientRectTop = 4
  end
  object tResize: TTimer
    Enabled = False
    Interval = 350
    OnTimer = tResizeTimer
    Left = 568
    Top = 432
  end
end
