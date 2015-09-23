object fDesignObjectInspector: TfDesignObjectInspector
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
  object pComponentSettings: TPanel
    Left = 8
    Top = 5
    Width = 184
    Height = 340
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      184
      340)
    object lComponentName: TLabel
      Left = 0
      Top = 6
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object eComponentName: TEdit
      Left = 0
      Top = 22
      Width = 184
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
end
