unit uApiServerInterface;

interface

uses
  // Delphi
  Generics.Collections,
  // Api
  uApiUpdateInterface;

type
  IBasicServerResponse = interface(IUnknown)
    ['{5308E9AC-31F2-429F-A5AC-F96837A67A32}']
    function GetStatus: Integer;
    procedure SetStatus(AStatus: Integer);
    function GetCode: Integer;
    procedure SetCode(ACode: Integer);
    function GetMsg: WideString;
    procedure SetMsg(AMsg: WideString);

    property Status: Integer read GetStatus write SetStatus;
    property Code: Integer read GetCode write SetCode;
    property Msg: WideString read GetMsg write SetMsg;

    function HasError: WordBool;
  end;

  IVersionsResponse = interface(IBasicServerResponse)
    ['{75DD6A08-3C92-4617-97CF-ACD2AE6A1AAB}']
    function GetVersions: TList<IUpdateManagerVersion>;

    property Versions: TList<IUpdateManagerVersion>read GetVersions;
  end;

  ISystemsResponse = interface(IBasicServerResponse)
    ['{AC2D03AE-8BA3-4C26-A6E4-AF7D3B4D395F}']
    function GetSystems: TList<IUpdateManagerSystemFileBase>;

    property Systems: TList<IUpdateManagerSystemFileBase>read GetSystems;
  end;

  IFTPServerResponse = interface(IBasicServerResponse)
    ['{8AE9C42F-54FC-4E00-B199-12982F65546A}']
    function GetFTPServer: IFTPServer;

    property Server: IFTPServer read GetFTPServer;
  end;

implementation

end.
