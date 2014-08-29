unit uPlugInImageHosterClass;

interface

uses
  // Plugin
  uPlugInInterface, uPlugInClass, uPlugInConst;

type

  TImageHosterPlugIn = class(TPlugIn, IImageHosterPlugIn)
  private
    FUseAccount: Boolean;

    FAccountName, FAccountPassword: WideString;

    FImageHostResize: TImageHostResize;
    function GetUseAccount: Boolean;
    procedure SetUseAccount(AUseAccount: Boolean);
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString);
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString);
    function GetImageHostResize: TImageHostResize;
    procedure SetImageHostResize(AImageHostResize: TImageHostResize);
  public
    property UseAccount: Boolean read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    function RemoteUpload(AImageUrl: WideString): WideString; virtual; abstract;
    property ImageHostResize: TImageHostResize read GetImageHostResize write SetImageHostResize;
  end;

implementation

{ TImageHosterPlugIn }

function TImageHosterPlugIn.GetUseAccount: Boolean;
begin
  Result := FUseAccount;
end;

procedure TImageHosterPlugIn.SetUseAccount(AUseAccount: Boolean);
begin
  FUseAccount := AUseAccount;
end;

function TImageHosterPlugIn.GetAccountName: WideString;
begin
  Result := FAccountName;
end;

procedure TImageHosterPlugIn.SetAccountName(AAccountName: WideString);
begin
  FAccountName := AAccountName;
end;

function TImageHosterPlugIn.GetAccountPassword: WideString;
begin
  Result := FAccountPassword;
end;

procedure TImageHosterPlugIn.SetAccountPassword(AAccountPassword: WideString);
begin
  FAccountPassword := AAccountPassword;
end;

function TImageHosterPlugIn.GetImageHostResize: TImageHostResize;
begin
  Result := FImageHostResize;
end;

procedure TImageHosterPlugIn.SetImageHostResize(AImageHostResize: TImageHostResize);
begin
  FImageHostResize := AImageHostResize;
end;

end.
