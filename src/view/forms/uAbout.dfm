object About: TAbout
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'About'
  ClientHeight = 422
  ClientWidth = 625
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
    625
    422)
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
    Top = 51
    Width = 609
    Height = 366
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 
      'Copyright_'#10'(c) 2007 - %s Sebastian Klatte                       ' +
      '                                                                ' +
      '                    Phone +49 151 280 59 557'#10#10'Portions of this s' +
      'oftware are copyright_'#10'(c) 1993 - 2004, Chad Z. Hower (Kudzu) an' +
      'd the Indy Pit Crew [The Indy Project] http://www.indyproject.or' +
      'g/'#10'(c) 1995 - 2011, L. David Baldwin et al. [HtmlViewer] https:/' +
      '/github.com/BerndGabriel/HtmlViewer/'#10'(c) 1997 - 2011, TurboPower' +
      ' Software [TurboPower Abbrevia] http://sourceforge.net/projects/' +
      'tpabbrevia/'#10'(c) 1999 - 2004, Andrey V. Sorokin [Delphi Regular E' +
      'xpressions] http://RegExpStudio.com'#10'(c) 2000 - 2009, DragonSoft ' +
      '[XML Class Serializer] http://dragonsoft.us/'#10'(c) 2004, Eric Z. J' +
      'ordens [TEZTexturePanel] http://ez-j.com/maindelphi4.htm'#10'(c) 200' +
      '4 - 2009, Pierre le Riche [Fast Memory Manager] http://sourcefor' +
      'ge.net/projects/fastmm/'#10'(c) 2006 - 2008, Hagen Reddmann et al. [' +
      'Delphi Encryption Compendium] http://michael-puff.de/Programmier' +
      'ung/Delphi/DEC/'#10'(c) 2006 - 2009, Leonid Koninin [JSON delphi lib' +
      'rary] http://sourceforge.net/projects/lkjson/'#10'(c) 2009 - 2014, S' +
      'pring4D Team [Spring Framework for Delphi] http://spring4d.org/'#10 +
      '(c) 2009 - 2015, Benjamin Rosseaux [BESEN] https://github.com/be' +
      'ro1985/besen/'#10'(c) 2009, Iztok Kacin [DirectoryWatch] http://crom' +
      'is.net/blog/downloads/directory-watch/'#10'(c) 2011, Frank Semmling ' +
      '[hThreadList] http://geheimniswelten.de/tipps/codes/threadlist/'#10 +
      '(c) 2015, Primoz Gabrijelcic [OmniThreadLibrary] http://otl.17sl' +
      'on.com/'#10#10'Portions of this icons use are copyright_'#10'(c) 2015, Fre' +
      'epik licensed under CC BY 3.0 [http://www.flaticon.com/]'#10#10'Portio' +
      'ns of this software use_'#10'EurekaLog (EurekaLab)  VCL Subscription' +
      ' (DevExpress)  FastScript (Fast Reports)  TAdvMemo (TMS Software' +
      ')'#10'Real Vista icons (Iconshock)'#10#10'Special thanks goes to_'#10'Members ' +
      'of Delphi-PRAXiS and Indy Team'#9'Families and Friends'#9'All software' +
      ' testers'
    Transparent = True
    WordWrap = True
    ExplicitHeight = 360
  end
  object bDonatePP: TButton
    Left = 542
    Top = 20
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Donate PP'
    TabOrder = 1
    OnClick = bDonatePPClick
  end
  object bDonateBC: TButton
    Left = 461
    Top = 20
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Donate BC'
    TabOrder = 0
    OnClick = bDonateBCClick
  end
end
