unit uApiServerClasses;

interface

uses
  // Delphi
  SysUtils, Classes, Generics.Collections,
  // Api
  uApiUpdateInterface, uApiServerInterface, uApiUpdateModel;

type
  TIBasicServerResponse = class(TInterfacedObject, IBasicServerResponse)
  private
    FStatus, FCode: Integer;
    FMsg: WideString;
  protected
    function GetStatus: Integer;
    procedure SetStatus(AStatus: Integer);
    function GetCode: Integer;
    procedure SetCode(ACode: Integer);
    function GetMsg: WideString;
    procedure SetMsg(AMsg: WideString);
  public
    constructor Create; reintroduce; virtual;

    property Status: Integer read GetStatus write SetStatus;
    property Code: Integer read GetCode write SetCode;
    property Msg: WideString read GetMsg write SetMsg;

    function HasError: WordBool;
  end;

  TIVersionsResponse = class(TIBasicServerResponse, IVersionsResponse)
  private
    FList: TList<IUpdateManagerVersion>;
  protected
    function GetVersions: TList<IUpdateManagerVersion>;
  public
    constructor Create; override;

    property Versions: TList<IUpdateManagerVersion>read GetVersions;

    destructor Destroy; override;
  end;

  TISystemsResponse = class(TIBasicServerResponse, ISystemsResponse)
  private
    FList: TList<IUpdateManagerSystemFileBase>;
  protected
    function GetSystems: TList<IUpdateManagerSystemFileBase>;
  public
    constructor Create; override;

    property Systems: TList<IUpdateManagerSystemFileBase>read GetSystems;

    destructor Destroy; override;
  end;

  TIFTPServerResponse = class(TIBasicServerResponse, IFTPServerResponse)
  private
    FFTPServer: IFTPServer;
  protected
    function GetFTPServer: IFTPServer;
  public
    constructor Create; override;

    property Server: IFTPServer read GetFTPServer;

    destructor Destroy; override;
  end;

  TIFilesToVersionResponse = class(TIBasicServerResponse, IFilesToVersionResponse)
  private
    FList: TList<IUpdateManagerOnlineSystemFile>;
  protected
    function GetFiles: TList<IUpdateManagerOnlineSystemFile>;
  public
    constructor Create; override;

    property Files: TList<IUpdateManagerOnlineSystemFile>read GetFiles;

    destructor Destroy; override;
  end;

  TIVersionAddResponse = class(TIBasicServerResponse, IVersionAddResponse)
  private
    FVersionID: Integer;
  protected
    function GetVersionID: Integer;
    procedure SetVersionID(AVersionID: Integer);
  public
    property VersionID: Integer read GetVersionID write SetVersionID;
  end;

implementation

{ TIBasicServerResponse }

function TIBasicServerResponse.GetStatus;
begin
  Result := FStatus;
end;

procedure TIBasicServerResponse.SetStatus;
begin
  FStatus := AStatus;
end;

function TIBasicServerResponse.GetCode;
begin
  Result := FCode;
end;

procedure TIBasicServerResponse.SetCode;
begin
  FCode := ACode;
end;

function TIBasicServerResponse.GetMsg;
begin
  Result := FMsg;
end;

procedure TIBasicServerResponse.SetMsg;
begin
  FMsg := AMsg;
end;

constructor TIBasicServerResponse.Create;
begin
  inherited Create;
end;

function TIBasicServerResponse.HasError: WordBool;
begin
  Result := (FStatus = 0)
end;

{ TIVersionsResponse }

constructor TIVersionsResponse.Create;
begin
  inherited Create;
  FList := TList<IUpdateManagerVersion>.Create();
end;

function TIVersionsResponse.GetVersions: TList<IUpdateManagerVersion>;
begin
  Result := FList;
end;

destructor TIVersionsResponse.Destroy;
begin
  FList := nil; // .Free;
  inherited Destroy;
end;

{ TISystemsResponse }

constructor TISystemsResponse.Create;
begin
  inherited Create;
  FList := TList<IUpdateManagerSystemFileBase>.Create();
end;

function TISystemsResponse.GetSystems: TList<IUpdateManagerSystemFileBase>;
begin
  Result := FList;
end;

destructor TISystemsResponse.Destroy;
begin
  FList := nil; // .Free;
  inherited Destroy;
end;

{ TIFTPServerResponse }

constructor TIFTPServerResponse.Create;
begin
  inherited Create;
  FFTPServer := TIFTPServer.Create('');
end;

function TIFTPServerResponse.GetFTPServer;
begin
  Result := FFTPServer;
end;

destructor TIFTPServerResponse.Destroy;
begin
  FFTPServer := nil;
  inherited Destroy;
end;

{ TIFilesToVersionResponse }

constructor TIFilesToVersionResponse.Create;
begin
  inherited Create;
  FList := TList<IUpdateManagerOnlineSystemFile>.Create();
end;

function TIFilesToVersionResponse.GetFiles: TList<IUpdateManagerOnlineSystemFile>;
begin
  Result := FList;
end;

destructor TIFilesToVersionResponse.Destroy;
begin
  FList := nil; // .Free;
  inherited Destroy;
end;

{ TIVersionAddResponse }

function TIVersionAddResponse.GetVersionID: Integer;
begin
  Result := FVersionID;
end;

procedure TIVersionAddResponse.SetVersionID(AVersionID: Integer);
begin
  FVersionID := AVersionID;
end;

end.
