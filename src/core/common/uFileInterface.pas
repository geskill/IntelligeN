{ ********************************************************
  *                                     IntelligeN CORE  *
  *  File interface                                      *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uFileInterface;

interface

uses
  // Delphi
  Classes, Generics.Collections,
  // Common
  uBaseConst;

type
  IChangeable = interface
    ['{D350C7D6-9EA6-4A73-A2CF-FBC70CFFBD01}']
    function GetChanged: WordBool;
    procedure SetChanged(AChanged: WordBool);

    procedure Change;

    property Changed: WordBool read GetChanged write SetChanged;
  end;

  ISubType = interface(IChangeable)
    ['{4F712A84-D730-450E-9DF0-3DB206B2ADE9}']
    function GetControlName: WideString;
    procedure SetGetControlName(AGetControlName: WideString); overload;
    procedure SetGetControlName(AComponentID: TControlID); overload;
    function GetControlValue: WideString;
    procedure SetGetControlValue(AGetControlValue: WideString);
    function GetID: WideString;
    procedure SetID(AID: WideString);
    function GetSubTypes: TList<ISubType>;

    property ControlName: WideString read GetControlName write SetGetControlName;
    property ControlValue: WideString read GetControlValue write SetGetControlValue;
    property ID: WideString read GetID write SetID;
    property SubTypes: TList<ISubType>read GetSubTypes;
  end;

  IType = interface(IChangeable)
    ['{8B2D299F-C826-4B05-B8AE-CE512DF322E2}']
    function GetName: WideString;
    procedure SetName(AName: WideString); overload;
    procedure SetName(ATypeID: TTypeID); overload;
    function GetID: WideString;
    procedure SetID(AID: WideString);
    function GetSubTypes: TList<ISubType>;

    property Name: WideString read GetName write SetName;
    property ID: WideString read GetID write SetID;
    property SubTypes: TList<ISubType>read GetSubTypes;
  end;

  IID = interface(IChangeable)
    ['{3DBB07DE-D75F-4EFA-926A-EF5BF8690010}']
    function GetName: WideString;
    procedure SetName(AName: WideString);
    function GetTypes: TList<IType>;

    property Name: WideString read GetName write SetName;
    property Types: TList<IType>read GetTypes;
  end;

  IWebsiteConfigurationFile = interface(IChangeable)
    ['{C86CF3AA-62CC-4FD0-BF13-9D62EA43A221}']
    function GetWebsiteURL: WideString;
    procedure SetWebsiteURL(AWebsiteURL: WideString);
    function GetWebsiteType: WideString;
    procedure SetWebsiteType(AWebsiteType: WideString);
    function GetWebsiteCharset: WideString;
    procedure SetWebsiteCharset(AWebsiteCharset: WideString);
    function GetIDs: TList<IID>;

    property WebsiteURL: WideString read GetWebsiteURL write SetWebsiteURL;
    property WebsiteType: WideString read GetWebsiteType write SetWebsiteType;
    property WebsiteCharset: WideString read GetWebsiteCharset write SetWebsiteCharset;
    property IDs: TList<IID>read GetIDs;
  end;

  IControl = interface(IChangeable)
    ['{491D7F7E-82EC-4EC5-8982-E2B9CFE5BBCD}']
    function GetName: WideString;
    procedure SetName(AName: WideString);
    function GetRelation: WideString;
    procedure SetRelation(ARelation: WideString);
    function GetValue: WideString;
    procedure SetValue(AValue: WideString);

    property Name: WideString read GetName write SetName;
    property Relation: WideString read GetRelation write SetRelation;
    property Value: WideString read GetValue write SetValue;
  end;

  IHoster = interface(IChangeable)
    ['{276A9292-86DD-415A-A110-BCF4687CA2BC}']
    function GetName: WideString;
    procedure SetName(AName: WideString);
    function GetRanked: WordBool;
    procedure SetRanked(ARanked: WordBool);
    function GetBlacklist: TStringList;
    function GetWhitelist: TStringList;

    property Name: WideString read GetName write SetName;
    property Ranked: WordBool read GetRanked write SetRanked;
    property Blacklist: TStringList read GetBlacklist;
    property Whitelist: TStringList read GetWhitelist;
  end;

  IFilter = interface(IChangeable)
    ['{43CF3341-9761-4840-B7AD-076657AA74AD}']
    function GetActive: WordBool;
    procedure SetActive(AActive: WordBool);
    function GetCategories: WideString;
    function GetCategoriesAsTTemplateTypeIDs: TTypeIDs;
    procedure SetCategories(ACategories: WideString);
    function GetControls: TList<IControl>;
    function GetHoster: TList<IHoster>;

    property Active: WordBool read GetActive write SetActive;
    property Categories: WideString read GetCategories write SetCategories;
    property Controls: TList<IControl>read GetControls;
    property Hosters: TList<IHoster>read GetHoster;
  end;

  IIntelligeNConfigurationFile = interface(IWebsiteConfigurationFile)
    ['{83C439E8-0FD3-4E56-8FF1-F54387180693}']

    function GetWebsiteFilter: IFilter;

    property WebsiteFilter: IFilter read GetWebsiteFilter;
  end;

implementation

end.
