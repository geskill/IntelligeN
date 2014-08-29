unit uPlugInConst;

interface

type
  TProxyType = (ptHTTP, ptSOCKS4, ptSOCKS5);
  TCAPTCHAInput = function(const AImageUrl, AName: WideString; out AText: WideString; var ACookies: WideString): Boolean of object; stdcall;
  TCAPTCHAType = (ctImage, ctText);
  TIntelligentPostingHelper = function(var ASearchValue: WideString; const ASearchResults: WideString; var ASearchIndex: Integer;
    out ARedoSearch: Boolean): Boolean of object; stdcall;
  TCMSType = (cmsBoard, cmsBlog, cmsFormbased);
  TFoldertype = (ftWeb, ftPlain, ftContainer);
  TFoldertypes = set of TFoldertype;
  TContainertype = (ctDLC, ctCCF, ctRSDF);
  TContainertypes = set of TContainertype;
  TAdvertismenttype = (atLayer, atLink, atBanner);
  TImageHostResize = (irNone, ir320x240, ir450x338, ir640x480, ir800x600);
  TLinkStatus = (lsOffline, lsOnline, lsUnknown, lsTemporaryOffline);
  TChecksumType = (ctMD5);

  TIDInfo = packed record
    ID, Path: WideString;
  end;

  TLinkInfo = packed record
    Link: WideString;
    Status: TLinkStatus;
    Size: Int64; { in Bytes }
    FileName, Checksum: WideString;
    ChecksumType: TChecksumType;
  end;

  TLinksInfo = packed record
    Status: Byte; { 0=offline|1=online|2=unknown;3=notyet;4=mixed;255=noinfo }
    Size: Extended;
    PartSize: Extended;
    Links: array of TLinkInfo;
  end;

  TCrypterFolderInfo = packed record
    Status: Byte; { 0=offline|1=online|2=unknown;3=notyet;4=mixed;255=noinfo }
    Size: Extended; { in Megabytes }
    Hoster: WideString; { Uploaded.to or Uploaded or up.to }
    HosterShort: WideString;
    Parts: Integer;
    StatusImage, StatusImageText: WideString;
  end;
  // PCrypterFolderInfo = ^TCrypterFolderInfo;

implementation

end.
