unit uPlugInImageHosterClass;

interface

uses
  // Plugin
  uPlugInInterface, uPlugInClass, uPlugInConst;

type

  TImageHosterPlugIn = class(TPlugIn, IImageHosterPlugIn)
  private
    FUseAccount: WordBool;

    FAccountName, FAccountPassword: WideString;

    FImageHostResize: TImageHostResize;
    function GetUseAccount: WordBool; safecall;
    procedure SetUseAccount(AUseAccount: WordBool); safecall;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString); safecall;
    function GetImageHostResize: TImageHostResize; safecall;
    procedure SetImageHostResize(AImageHostResize: TImageHostResize); safecall;
  public
    property UseAccount: WordBool read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    function LocalUpload(ALocalPath: WideString): WideString; virtual; safecall; abstract;
    function RemoteUpload(AImageUrl: WideString): WideString; virtual; safecall; abstract;

    property ImageHostResize: TImageHostResize read GetImageHostResize write SetImageHostResize;
  end;

implementation

{ TImageHosterPlugIn }

function TImageHosterPlugIn.GetUseAccount: WordBool;
begin
  Result := FUseAccount;
end;

procedure TImageHosterPlugIn.SetUseAccount(AUseAccount: WordBool);
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
