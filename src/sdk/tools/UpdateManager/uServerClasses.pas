unit uServerClasses;

interface

uses
  // Delphi
  SysUtils, Classes, Generics.Collections,
  // Common
  uApiUpdateInterface, uServerInterface, uApiUpdateModel;

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
    FList: TList<IUpdateServerVersion>;
  protected
    function GetVersions: TList<IUpdateServerVersion>;
  public
    constructor Create; override;

    property Versions: TList<IUpdateServerVersion>read GetVersions;

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
  FList := TList<IUpdateServerVersion>.Create();
end;

function TIVersionsResponse.GetVersions: TList<IUpdateServerVersion>;
begin
  Result := FList;
end;

destructor TIVersionsResponse.Destroy;
begin
  FList := nil; //.Free;
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

end.
