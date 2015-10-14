{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn constants                                    *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInConst;

interface

uses
  // Common
  uBaseConst;

type
  TPlugInType = (ptNone, ptApp, ptCAPTCHA, ptCMS, ptCrawler, ptCrypter, ptFileFormats, ptFileHoster, ptImageHoster);
  TCAPTCHAInput = function(const ACAPTCHA: WideString; const ACAPTCHAName: WideString; out ACAPTCHASolution: WideString; var ACookies: WideString): WordBool of object; safecall;
  TCAPTCHAType = (ctImage, ctText);
  TIntelligentPostingHelper = function(var ASearchValue: WideString; const ASearchResults: WideString; var ASearchIndex: Integer; out ARedoSearch: WordBool): WordBool of object; safecall;
  TCMSType = (cmsBoard, cmsBlog, cmsFormbased);
  TCMSIDType = (citCategory, citPrefix, citIcon);
  TFoldertype = (ftWeb, ftPlain, ftContainer);
  TFoldertypes = set of TFoldertype;
  TContainertype = (ctDLC, ctCCF, ctRSDF);
  TContainertypes = set of TContainertype;
  TAdvertismenttype = (atLayer, atLink, atBanner);
  TImageHostResize = (irNone, ir320x240, ir450x338, ir640x480, ir800x600);
  TLinkStatus = csOffline..csTemporaryOffline;
  TChecksumType = (ctMD5, ctSHA512);

  TIDInfo = packed record
    ID, Path: WideString;
  end;

  TLinkInfo = packed record
    Link: WideString;
    Status: TLinkStatus;
    Size: Double; { in Bytes }
    FileName, Checksum: WideString;
    ChecksumType: TChecksumType;
  end;

  TLinksInfo = packed record
    Status: TContentStatus;
    Size: Double;
    PartSize: Double;
    Links: array of TLinkInfo;
  end;

  TCrypterFolderInfo = packed record
    Link: WideString;
    Status: TContentStatus;
    Size: Double; { in Megabytes }
    PartSize: Double; { in Megabytes }
    Hoster: WideString; { Uploaded.to or Uploaded or up.to (will be converted in MAIN APP) }
    HosterShort: WideString;
    Parts: Integer;
    StatusImage, StatusImageText: WideString;
  end;
  // PCrypterFolderInfo = ^TCrypterFolderInfo;

implementation

end.
