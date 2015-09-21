unit uWebsiteInterface;

interface

uses
  // Common
  uConst;

type
  IMirrorData = interface
  ['{BD6E30EB-EC6F-476C-8C57-51ADEBB75156}']
    function GetSize: Extended; safecall;
    function GetPartSize: Extended; safecall;
    function GetHoster: WideString; safecall;
    function GetHosterShort: WideString; safecall;
    function GetParts: Integer; safecall;

    property Size: Extended read GetSize;
    property PartSize: Extended read GetPartSize;
    property Hoster: WideString read GetHoster;
    property HosterShort: WideString read GetHosterShort;
    property Parts: Integer read GetParts;
  end;

  IControlContainer = interface
  ['{CE74BD5F-80B2-4D4F-AB2D-F29AA0773F58}']
    function GetComponentID: TComponentID; safecall;
    function GetValue: WideString; safecall;

    property ComponentID: TComponentID read GetComponentID;
    property Value: WideString read GetValue;
  end;

  IMirrorItem = interface(IMirrorData)
  ['{E1096EE1-D38F-4BCB-9341-417550DDAD38}']
    function GetValue: WideString; safecall;

    property Value: WideString read GetValue;
  end;

  ICrypter = interface(IMirrorItem)
  ['{10541298-AE56-42F5-ACE2-7EA93E93AF26}']
    function GetName: WideString; safecall;
    function GetStatusImage: WideString; safecall;
    function GetStatusImageText: WideString; safecall;

    property Name: WideString read GetName;
    property StatusImage: WideString read GetStatusImage;
    property StatusImageText: WideString read GetStatusImageText;
  end;

  IDirectlink = interface(IMirrorItem)
  ['{F8592097-8D2E-4EC6-AE2F-B74092641096}']
  end;

  IMirrorContainer = interface(IMirrorData)
  ['{0FC2EC0F-D5E8-42A7-8C75-0F4BBDD48089}']
    function GetCrypter(const IndexOrName: OleVariant): ICrypter; safecall;
    function GetCrypterCount: Integer; safecall;
    function GetDirectlink(const Index: Integer): IDirectlink; safecall;
    function GetDirectlinkCount: Integer; safecall;

    property Crypter[const IndexOrName: OleVariant]: ICrypter read GetCrypter;
    property CrypterCount: Integer read GetCrypterCount;
    property Directlink[const Index: Integer]: IDirectlink read GetDirectlink;
    property DirectlinkCount: Integer read GetDirectlinkCount;
  end;

  ICMSWebsiteData = interface
  ['{68C4F907-A032-408E-9A68-2C4ADA0262D5}']
    function GetTemplateTypeID: TTemplateTypeID; safecall;

    function GetControl(const IndexOrName: OleVariant): IControlContainer; safecall;
    function GetControlCount: Integer; safecall;
    function GetMirror(const IndexOrName: OleVariant): IMirrorContainer; safecall;
    function GetMirrorCount: Integer; safecall;

    property TemplateTypeID: TTemplateTypeID read GetTemplateTypeID;

    function FindControl(ComponentID: TComponentID): IControlContainer; safecall;

    property Control[const IndexOrName: OleVariant]: IControlContainer read GetControl;
    property ControlCount: Integer read GetControlCount;
    property Mirror[const IndexOrName: OleVariant]: IMirrorContainer read GetMirror;
    property MirrorCount: Integer read GetMirrorCount;
  end;

implementation

end.
