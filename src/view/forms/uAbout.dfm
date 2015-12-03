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
      'Copyright_'#10'(c) 2007 - 2015 Sebastian Klatte                     ' +
      '                                                                ' +
      '                      Phone +49 151 280 59 557'#10#10'Portions of this' +
      ' software are copyright_'#10'(c) 1993 - 2004, Chad Z. Hower (Kudzu) ' +
      'and the Indy Pit Crew [The Indy Project] http://www.indyproject.' +
      'org/'#10'(c) 1995 - 2011, L. David Baldwin et al. [HtmlViewer] https' +
      '://github.com/BerndGabriel/HtmlViewer/'#10'(c) 1997 - 2011, TurboPow' +
      'er Software [TurboPower Abbrevia] http://sourceforge.net/project' +
      's/tpabbrevia/'#10'(c) 1999 - 2004, Andrey V. Sorokin [Delphi Regular' +
      ' Expressions] http://RegExpStudio.com'#10'(c) 2000 - 2009, DragonSof' +
      't [XML Class Serializer] http://dragonsoft.us/'#10'(c) 2004, Eric Z.' +
      ' Jordens [TEZTexturePanel] http://ez-j.com/maindelphi4.htm'#10'(c) 2' +
      '004 - 2009, Pierre le Riche [Fast Memory Manager] http://sourcef' +
      'orge.net/projects/fastmm/'#10'(c) 2006 - 2008, Hagen Reddmann et al.' +
      ' [Delphi Encryption Compendium] http://michael-puff.de/Programmi' +
      'erung/Delphi/DEC/'#10'(c) 2006 - 2009, Leonid Koninin [JSON delphi l' +
      'ibrary] http://sourceforge.net/projects/lkjson/'#10'(c) 2009 - 2014,' +
      ' Spring4D Team [Spring Framework for Delphi] http://spring4d.org' +
      '/'#10'(c) 2009 - 2015, Benjamin Rosseaux [BESEN] https://github.com/' +
      'bero1985/besen/'#10'(c) 2009, Iztok Kacin [DirectoryWatch] http://cr' +
      'omis.net/blog/downloads/directory-watch/'#10'(c) 2011, Frank Semmlin' +
      'g [hThreadList] http://geheimniswelten.de/tipps/codes/threadlist' +
      '/'#10'(c) 2015, Primoz Gabrijelcic [OmniThreadLibrary] http://otl.17' +
      'slon.com/'#10#10'Portions of this icons use are copyright_'#10'(c) 2015, F' +
      'reepik licensed under CC BY 3.0 [http://www.flaticon.com/]'#10#10'Port' +
      'ions of this software use_'#10'EurekaLog (EurekaLab)  VCL Subscripti' +
      'on (DevExpress)  FastScript (Fast Reports)  TAdvMemo (TMS Softwa' +
      're)'#10'Real Vista icons (Iconshock)'#10#10'Special thanks goes to_'#10'Member' +
      's of Delphi-PRAXiS and Indy Team'#9'Families and Friends'#9'All softwa' +
      're testers'
    Transparent = True
    WordWrap = True
    ExplicitHeight = 360
  end
  object bDonate: TButton
    Left = 542
    Top = 20
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Donate'
    TabOrder = 0
    OnClick = bDonateClick
  end
end
