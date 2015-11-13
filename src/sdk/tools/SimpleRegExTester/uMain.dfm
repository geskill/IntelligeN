object Main: TMain
  Left = 0
  Top = 0
  Caption = 'Simple RegEx Tester'
  ClientHeight = 241
  ClientWidth = 792
  Color = clBtnFace
  Constraints.MinHeight = 279
  Constraints.MinWidth = 808
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    792
    241)
  PixelsPerInch = 96
  TextHeight = 13
  object lRegularExpression: TLabel
    Left = 8
    Top = 8
    Width = 92
    Height = 13
    Caption = 'Regular expression'
  end
  object lTestString: TLabel
    Left = 8
    Top = 72
    Width = 51
    Height = 13
    Caption = 'Test string'
  end
  object lSubstitution: TLabel
    Left = 8
    Top = 198
    Width = 57
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Substitution'
  end
  object lMatch: TLabel
    Left = 399
    Top = 8
    Width = 29
    Height = 13
    Caption = 'Match'
  end
  object lClearTestString: TLabel
    Left = 383
    Top = 72
    Width = 7
    Height = 13
    Cursor = crHandPoint
    Caption = 'X'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = lClearTestStringClick
  end
  object eRegularExpression: TEdit
    Left = 8
    Top = 22
    Width = 385
    Height = 21
    TabOrder = 0
    OnChange = OnChange
  end
  object mTestString: TMemo
    Left = 8
    Top = 86
    Width = 385
    Height = 106
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
    OnChange = OnChange
    OnKeyPress = mTestStringKeyPress
  end
  object eSubstitution: TEdit
    Left = 8
    Top = 212
    Width = 385
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 8
    OnChange = OnChange
  end
  object cbModifierI: TCheckBox
    Left = 16
    Top = 49
    Width = 33
    Height = 17
    Hint = 'insensitive. Case insensitive match (ignores case of [a-zA-Z])'
    Caption = 'I'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = OnChange
  end
  object cbModifierR: TCheckBox
    Left = 55
    Top = 49
    Width = 33
    Height = 17
    Hint = 'syntax extended for russian characters.'
    Caption = 'R'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 3
    OnClick = OnChange
  end
  object cbModifierS: TCheckBox
    Left = 94
    Top = 49
    Width = 33
    Height = 17
    Hint = 'single line. Dot matches newline characters'
    Caption = 'S'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 4
    OnClick = OnChange
  end
  object cbModifierG: TCheckBox
    Left = 133
    Top = 49
    Width = 33
    Height = 17
    Hint = 
      'ungreedy. The match becomes lazy by default. Now a ? following a' +
      ' quantifier makes it greedy'
    Caption = 'G'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 5
    OnClick = OnChange
  end
  object cbModifierM: TCheckBox
    Left = 172
    Top = 49
    Width = 33
    Height = 17
    Hint = 
      'multi-line. Causes ^ and $ to match the begin/end of each line (' +
      'not only begin/end of string)'
    Caption = 'M'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = OnChange
  end
  object cbModifierX: TCheckBox
    Left = 211
    Top = 49
    Width = 33
    Height = 17
    Hint = 'extended. Spaces and text after a # in the pattern are ignored'
    Caption = 'X'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = OnChange
  end
  object tbMatches: TTabControl
    Left = 399
    Top = 24
    Width = 385
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 9
    OnChange = tbMatchesChange
    object mMatch: TMemo
      Left = 4
      Top = 6
      Width = 377
      Height = 199
      Align = alClient
      Enabled = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
end
