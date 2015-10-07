unit uBaseInterface;

interface

uses
  // Common
  uBaseConst;

type
  IValueItem = interface
    ['{E1096EE1-D38F-4BCB-9341-417550DDAD38}']
    function GetValue: WideString; safecall;

    property Value: WideString read GetValue;
  end;

  IMirrorData = interface(IValueItem)
    ['{BD6E30EB-EC6F-476C-8C57-51ADEBB75156}']
    function GetSize: Double; safecall;
    function GetPartSize: Double; safecall;
    function GetHoster: WideString; safecall;
    function GetHosterShort: WideString; safecall;
    function GetParts: Integer; safecall;

    property Size: Double read GetSize;
    property PartSize: Double read GetPartSize;
    property Hoster: WideString read GetHoster;
    property HosterShort: WideString read GetHosterShort;
    property Parts: Integer read GetParts;
  end;

  IControlData = interface(IValueItem)
    ['{CE74BD5F-80B2-4D4F-AB2D-F29AA0773F58}']
    function GetControlID: TControlID; safecall;

    property ControlID: TControlID read GetControlID;
  end;

  ICrypter = interface(IMirrorData)
    ['{10541298-AE56-42F5-ACE2-7EA93E93AF26}']
    function GetName: WideString; safecall;
    function GetStatusImage: WideString; safecall;
    function GetStatusImageText: WideString; safecall;

    property Name: WideString read GetName;
    property StatusImage: WideString read GetStatusImage;
    property StatusImageText: WideString read GetStatusImageText;
  end;

  IDirectlink = interface(IMirrorData)
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

  IControlBase = interface(IControlData)
    ['{06C360E4-BBFB-462E-A2AD-54BE875635AE}']
    procedure AddProposedValue(const ASender: WideString; AValue: WideString; ATitle: WideString = ''); safecall;
    function GetProposedValue(const AIndex: Integer): WideString; safecall;
    function GetProposedValueSender(const AIndex: Integer): WideString; safecall;
    function GetProposedCount: Integer; safecall;

    property ProposedCount: Integer read GetProposedCount;
  end;

  IControlControllerBase = interface
    ['{CC51899A-01B3-43A0-ABEB-C90350956977}']
    function GetControl(const IndexOrName: OleVariant): IControlBase; safecall;
    function GetControlCount: Integer; safecall;

    function FindControl(const AControlID: TControlID): IControlBase; safecall;

    property Control[const IndexOrName: OleVariant]: IControlBase read GetControl;
    property ControlCount: Integer read GetControlCount;
  end;

  IMirrorControllerBase = interface
    ['{723BD89D-867F-49DF-8E89-98EA81C6B3EA}']
    function GetMirror(const IndexOrName: OleVariant): IMirrorContainer; safecall;
    function GetMirrorCount: Integer; safecall;

    property Mirror[const IndexOrName: OleVariant]: IMirrorContainer read GetMirror;
    property MirrorCount: Integer read GetMirrorCount;
  end;

  ITabSheetData = interface
    ['{68C4F907-A032-408E-9A68-2C4ADA0262D5}']
    function GetTypeID: TTypeID; safecall;

    function GetControl(const IndexOrName: OleVariant): IControlData; safecall;
    function GetControlCount: Integer; safecall;
    function GetMirror(const IndexOrName: OleVariant): IMirrorContainer; safecall;
    function GetMirrorCount: Integer; safecall;

    property TypeID: TTypeID read GetTypeID;

    function FindControl(const AControlID: TControlID): IControlData; safecall;

    property Control[const IndexOrName: OleVariant]: IControlData read GetControl;
    property ControlCount: Integer read GetControlCount;
    property Mirror[const IndexOrName: OleVariant]: IMirrorContainer read GetMirror;
    property MirrorCount: Integer read GetMirrorCount;
  end;

  IWebsiteEditor = interface
    ['{75F63C46-C88E-48C5-BDC1-C7D81BEFEC1A}']
    function GetCustomFields: WordBool; safecall;
    procedure SetCustomFields(ACustomFields: WordBool); safecall;

    procedure AddEdit(AName: WideString; ADefaultValue: WideString = ''; ATopValue: WordBool = False); safecall;
    procedure AddCheckbox(AName: WideString; ADefaultValue: WordBool = False; ATopValue: WordBool = False); safecall;
    procedure AddCategoryTab(AName: WideString); safecall;

    property CustomFields: WordBool read GetCustomFields write SetCustomFields;

    function ShowModal: Integer; safecall;
  end;

implementation

end.
