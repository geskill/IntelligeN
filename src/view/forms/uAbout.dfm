object About: TAbout
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'About'
  ClientHeight = 290
  ClientWidth = 549
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  DesignSize = (
    549
    290)
  PixelsPerInch = 96
  TextHeight = 13
  object lVersion: TLabel
    Left = 8
    Top = 8
    Width = 41
    Height = 13
    Caption = 'Version_'
    Transparent = True
  end
  object lVersionValue: TLabel
    Left = 8
    Top = 27
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object lCopyright: TLabel
    Left = 8
    Top = 46
    Width = 533
    Height = 236
    Anchors = [akLeft, akTop, akBottom]
    AutoSize = False
    Caption = 
      'Copyright_'#13'(c) 2007 - 2015 Sebastian Klatte                     ' +
      '                                                             Pho' +
      'ne +49 151 280 59 557'#13#13'Portions of this software are Copyright_'#13 +
      '(c) 1993 - 2004, Chad Z. Hower (Kudzu) and the Indy Pit Crew [Th' +
      'e Indy Project] http://www.indyproject.org/'#13'(c) 1997 - 2011, Tur' +
      'boPower Software [TurboPower Abbrevia] http://sourceforge.net/pr' +
      'ojects/tpabbrevia/'#13'(c) 1999 - 2004, Andrey V. Sorokin [Delphi Re' +
      'gular Expressions] http://RegExpStudio.com'#13'(c) 2000 - 2009, Drag' +
      'onSoft [XML Class Serializer] http://www.dragonsoft.us/'#13'(c) 2004' +
      ' - 2009, Pierre le Riche [Fast Memory Manager] http://sourceforg' +
      'e.net/projects/fastmm/'#13'(c) 2009, Daniel Wischnewski [Windows 7 C' +
      'omponent Library] http://www.gumpi.com/blog/'#13'(c) 2009, Iztok Kac' +
      'in [DirectoryWatch] http://www.cromis.net/blog/downloads/directo' +
      'ry-watch/ '#13'(c) 2012, Primoz Gabrijelcic [OmniThreadLibrary] http' +
      '://otl.17slon.com/'#13#13'Portions of this software use_'#13'EurekaLog (Eu' +
      'rekaLab)  ExpressPack (DevExpress)  FastScript (Fast Reports)  R' +
      'eal Vista icons (Iconshock)'#13#13'Special thanks goes to_'#13'Members of ' +
      'Delphi-PRAXiS and Indy Team'#9'Families and Friends'#9'All software te' +
      'sters'
    Transparent = True
    WordWrap = True
    ExplicitHeight = 235
  end
end
