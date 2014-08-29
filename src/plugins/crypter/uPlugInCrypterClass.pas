unit uPlugInCrypterClass;

interface

uses
  // Interface
  uAppInterface,
  // Plugin
  uPlugInInterface, uPlugInClass, uPlugInConst;

type
  TCrypterPlugIn = class(TPlugIn, ICrypterPlugIn)
  private
    FUseAccount, FUseCaptcha, FUseAdvertismentLink, FUseAdvertismentPicture, FUseCoverLink, FUseDescription, FUseCNL, FUseWebseiteLink,
      FUseEMailforStatusNotice, FUseFilePassword, FUseAdminPassword, FUseVisitorPassword: Boolean;

    FAccountName, FAccountPassword, FFoldername, FAdvertismentLayerName, FAdvertismentLayerValue, FAdvertismentLink, FAdvertismentPicture, FDescription,
      FCoverLink, FWebseiteLink, FEMailforStatusNotice, FFilePassword, FAdminPassword, FVisitorPassword: WideString;

    FFoldertypes, FContainerTypes, FAdvertismentTyp: Integer;
  public
    function GetUseAccount: Boolean;
    procedure SetUseAccount(AUseAccount: Boolean);
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString);
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString);

    function GetUseCoverLink: Boolean;
    procedure SetUseCoverLink(AUseCoverLink: Boolean);
    function GetCoverLink: WideString; safecall;
    procedure SetCoverLink(ACoverLink: WideString);
    function GetUseDescription: Boolean;
    procedure SetUseDescription(AUseDescription: Boolean);
    function GetDescription: WideString; safecall;
    procedure SetDescription(ADescription: WideString);
    function GetUseCNL: Boolean;
    procedure SetUseCNL(AUseCNL: Boolean);
    function GetUseWebseiteLink: Boolean;
    procedure SetUseWebseiteLink(AUseWebseiteLink: Boolean);
    function GetWebseiteLink: WideString; safecall;
    procedure SetWebseiteLink(AWebseiteLink: WideString);

    function GetFoldertypes: Integer;
    procedure SetFoldertypes(AFoldertypes: Integer);
    function GetContainerTypes: Integer;
    procedure SetContainerTypes(AContainerTypes: Integer);
    function GetUseCaptcha: Boolean;
    procedure SetUseCaptcha(AUseCaptcha: Boolean);
    function GetFolderName: WideString; safecall;
    procedure SetFolderName(AFolderName: WideString);

    function GetAdvertismentType: Integer;
    procedure SetAdvertismentType(AAdvertismentType: Integer);
    function GetAdvertismentLayerName: WideString; safecall;
    procedure SetAdvertismentLayerName(AAdvertismentLayerName: WideString);
    function GetAdvertismentLayerValue: WideString; safecall;
    procedure SetAdvertismentLayerValue(AAdvertismentLayerValue: WideString);
    function GetUseAdvertismentLink: Boolean;
    procedure SetUseAdvertismentLink(AUseAdvertismentLink: Boolean);
    function GetAdvertismentLink: WideString; safecall;
    procedure SetAdvertismentLink(AAdvertismentLink: WideString);
    function GetUseAdvertismentPicture: Boolean;
    procedure SetUseAdvertismentPicture(AUseAdvertismentPicture: Boolean);
    function GetAdvertismentPicture: WideString; safecall;
    procedure SetAdvertismentPicture(AAdvertismentPicture: WideString);

    function GetUseEMailforStatusNotice: Boolean;
    procedure SetUseEMailforStatusNotice(AUseEMailforStatusNotice: Boolean);
    function GetEMailforStatusNotice: WideString; safecall;
    procedure SetEMailforStatusNotice(AEMailforStatusNotice: WideString);
    function GetUseFilePassword: Boolean;
    procedure SetUseFilePassword(AUseFilePassword: Boolean);
    function GetFilePassword: WideString; safecall;
    procedure SetFilePassword(AFilePassword: WideString);
    function GetUseAdminPassword: Boolean;
    procedure SetUseAdminPassword(AUseAdminPassword: Boolean);
    function GetAdminPassword: WideString; safecall;
    procedure SetAdminPassword(AAdminPassword: WideString);
    function GetUseVisitorPassword: Boolean;
    procedure SetUseVisitorPassword(AUseVisitorPassword: Boolean);
    function GetVisitorPassword: WideString; safecall;
    procedure SetVisitorPassword(AVisitorPassword: WideString);

    property UseAccount: Boolean read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    property Foldertypes: Integer read GetFoldertypes write SetFoldertypes;
    property ContainerTypes: Integer read GetContainerTypes write SetContainerTypes;
    property UseCaptcha: Boolean read GetUseCaptcha write SetUseCaptcha;
    property FolderName: WideString read GetFolderName write SetFolderName;

    property AdvertismentType: Integer read GetAdvertismentType write SetAdvertismentType;
    property AdvertismentLayerName: WideString read GetAdvertismentLayerName write SetAdvertismentLayerName;
    property AdvertismentLayerValue: WideString read GetAdvertismentLayerValue write SetAdvertismentLayerValue;
    property UseAdvertismentLink: Boolean read GetUseAdvertismentLink write SetUseAdvertismentLink;
    property AdvertismentLink: WideString read GetAdvertismentLink write SetAdvertismentLink;
    property UseAdvertismentPicture: Boolean read GetUseAdvertismentPicture write SetUseAdvertismentPicture;
    property AdvertismentPicture: WideString read GetAdvertismentPicture write SetAdvertismentPicture;

    property UseCoverLink: Boolean read GetUseCoverLink write SetUseCoverLink;
    property CoverLink: WideString read GetCoverLink write SetCoverLink;
    property UseDescription: Boolean read GetUseDescription write SetUseDescription;
    property Description: WideString read GetDescription write SetDescription;
    property UseCNL: Boolean read GetUseCNL write SetUseCNL;
    property UseWebseiteLink: Boolean read GetUseWebseiteLink write SetUseWebseiteLink;
    property WebseiteLink: WideString read GetWebseiteLink write SetWebseiteLink;

    property UseEMailforStatusNotice: Boolean read GetUseEMailforStatusNotice write SetUseEMailforStatusNotice;
    property EMailforStatusNotice: WideString read GetEMailforStatusNotice write SetEMailforStatusNotice;

    property UseFilePassword: Boolean read GetUseFilePassword write SetUseFilePassword;
    property FilePassword: WideString read GetFilePassword write SetFilePassword;
    property UseAdminPassword: Boolean read GetUseAdminPassword write SetUseAdminPassword;
    property AdminPassword: WideString read GetAdminPassword write SetAdminPassword;
    property UseVisitorPassword: Boolean read GetUseVisitorPassword write SetUseVisitorPassword;
    property Visitorpassword: WideString read GetVisitorPassword write SetVisitorPassword;

    function GenerateFolder(MirrorController: IMirrorControl): WideString; virtual; abstract;
    function GetFolderInfo(FolderURL: WideString): TCrypterFolderInfo; virtual; abstract;
    procedure GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: Boolean = True); virtual; stdcall; abstract;
  end;

implementation

{ TCrypterPlugIn }

function TCrypterPlugIn.GetUseAccount: Boolean;
begin
  Result := FUseAccount;
end;

procedure TCrypterPlugIn.SetUseAccount(AUseAccount: Boolean);
begin
  FUseAccount := AUseAccount;
end;

function TCrypterPlugIn.GetAccountName: WideString;
begin
  Result := FAccountName;
end;

procedure TCrypterPlugIn.SetAccountName(AAccountName: WideString);
begin
  FAccountName := AAccountName;
end;

function TCrypterPlugIn.GetAccountPassword: WideString;
begin
  Result := FAccountPassword;
end;

procedure TCrypterPlugIn.SetAccountPassword(AAccountPassword: WideString);
begin
  FAccountPassword := AAccountPassword;
end;

function TCrypterPlugIn.GetUseCoverLink: Boolean;
begin
  Result := FUseCoverLink;
end;

procedure TCrypterPlugIn.SetUseCoverLink(AUseCoverLink: Boolean);
begin
  FUseCoverLink := AUseCoverLink;
end;

function TCrypterPlugIn.GetCoverLink: WideString;
begin
  Result := FCoverLink;
end;

procedure TCrypterPlugIn.SetCoverLink(ACoverLink: WideString);
begin
  FCoverLink := ACoverLink;
end;

function TCrypterPlugIn.GetUseDescription: Boolean;
begin
  Result := FUseDescription;
end;

procedure TCrypterPlugIn.SetUseDescription(AUseDescription: Boolean);
begin
  FUseDescription := AUseDescription;
end;

function TCrypterPlugIn.GetDescription: WideString;
begin
  Result := FDescription;
end;

procedure TCrypterPlugIn.SetDescription(ADescription: WideString);
begin
  FDescription := ADescription;
end;

function TCrypterPlugIn.GetUseCNL: Boolean;
begin
  Result := FUseCNL;
end;

procedure TCrypterPlugIn.SetUseCNL(AUseCNL: Boolean);
begin
  FUseCNL := AUseCNL;
end;

function TCrypterPlugIn.GetUseWebseiteLink: Boolean;
begin
  Result := FUseWebseiteLink;
end;

procedure TCrypterPlugIn.SetUseWebseiteLink(AUseWebseiteLink: Boolean);
begin
  FUseWebseiteLink := AUseWebseiteLink;
end;

function TCrypterPlugIn.GetWebseiteLink: WideString;
begin
  Result := FWebseiteLink;
end;

procedure TCrypterPlugIn.SetWebseiteLink(AWebseiteLink: WideString);
begin
  FWebseiteLink := AWebseiteLink;
end;

function TCrypterPlugIn.GetFoldertypes: Integer;
begin
  Result := FFoldertypes;
end;

procedure TCrypterPlugIn.SetFoldertypes(AFoldertypes: Integer);
begin
  FFoldertypes := AFoldertypes;
end;

function TCrypterPlugIn.GetContainerTypes: Integer;
begin
  Result := FContainerTypes;
end;

procedure TCrypterPlugIn.SetContainerTypes(AContainerTypes: Integer);
begin
  FContainerTypes := AContainerTypes;
end;

function TCrypterPlugIn.GetUseCaptcha: Boolean;
begin
  Result := FUseCaptcha;
end;

procedure TCrypterPlugIn.SetUseCaptcha(AUseCaptcha: Boolean);
begin
  FUseCaptcha := AUseCaptcha;
end;

function TCrypterPlugIn.GetFolderName: WideString;
begin
  Result := FFoldername;
end;

procedure TCrypterPlugIn.SetFolderName(AFolderName: WideString);
begin
  FFoldername := AFolderName;
end;

function TCrypterPlugIn.GetAdvertismentType;
begin
  Result := FAdvertismentTyp;
end;

procedure TCrypterPlugIn.SetAdvertismentType(AAdvertismentType: Integer);
begin
  FAdvertismentTyp := AAdvertismentType;
end;

function TCrypterPlugIn.GetAdvertismentLayerName;
begin
  Result := FAdvertismentLayerName;
end;

procedure TCrypterPlugIn.SetAdvertismentLayerName(AAdvertismentLayerName: WideString);
begin
  FAdvertismentLayerName := AAdvertismentLayerName;
end;

function TCrypterPlugIn.GetAdvertismentLayerValue;
begin
  Result := FAdvertismentLayerValue;
end;

procedure TCrypterPlugIn.SetAdvertismentLayerValue(AAdvertismentLayerValue: WideString);
begin
  FAdvertismentLayerValue := AAdvertismentLayerValue;
end;

function TCrypterPlugIn.GetUseAdvertismentLink;
begin
  Result := FUseAdvertismentLink;
end;

procedure TCrypterPlugIn.SetUseAdvertismentLink(AUseAdvertismentLink: Boolean);
begin
  FUseAdvertismentLink := AUseAdvertismentLink;
end;

function TCrypterPlugIn.GetAdvertismentLink;
begin
  Result := FAdvertismentLink;
end;

procedure TCrypterPlugIn.SetAdvertismentLink(AAdvertismentLink: WideString);
begin
  FAdvertismentLink := AAdvertismentLink;
end;

function TCrypterPlugIn.GetUseAdvertismentPicture;
begin
  Result := FUseAdvertismentPicture;
end;

procedure TCrypterPlugIn.SetUseAdvertismentPicture(AUseAdvertismentPicture: Boolean);
begin
  FUseAdvertismentPicture := AUseAdvertismentPicture;
end;

function TCrypterPlugIn.GetAdvertismentPicture;
begin
  Result := FAdvertismentPicture;
end;

procedure TCrypterPlugIn.SetAdvertismentPicture(AAdvertismentPicture: WideString);
begin
  FAdvertismentPicture := AAdvertismentPicture;
end;

function TCrypterPlugIn.GetUseEMailforStatusNotice: Boolean;
begin
  Result := FUseEMailforStatusNotice;
end;

procedure TCrypterPlugIn.SetUseEMailforStatusNotice(AUseEMailforStatusNotice: Boolean);
begin
  FUseEMailforStatusNotice := AUseEMailforStatusNotice;
end;

function TCrypterPlugIn.GetEMailforStatusNotice: WideString;
begin
  Result := FEMailforStatusNotice;
end;

procedure TCrypterPlugIn.SetEMailforStatusNotice(AEMailforStatusNotice: WideString);
begin
  FEMailforStatusNotice := AEMailforStatusNotice;
end;

function TCrypterPlugIn.GetUseFilePassword: Boolean;
begin
  Result := FUseFilePassword;
end;

procedure TCrypterPlugIn.SetUseFilePassword(AUseFilePassword: Boolean);
begin
  FUseFilePassword := AUseFilePassword;
end;

function TCrypterPlugIn.GetFilePassword: WideString;
begin
  Result := FFilePassword;
end;

procedure TCrypterPlugIn.SetFilePassword(AFilePassword: WideString);
begin
  FFilePassword := AFilePassword;
end;

function TCrypterPlugIn.GetUseAdminPassword: Boolean;
begin
  Result := FUseAdminPassword;
end;

procedure TCrypterPlugIn.SetUseAdminPassword(AUseAdminPassword: Boolean);
begin
  FUseAdminPassword := AUseAdminPassword;
end;

function TCrypterPlugIn.GetAdminPassword: WideString;
begin
  Result := FAdminPassword;
end;

procedure TCrypterPlugIn.SetAdminPassword(AAdminPassword: WideString);
begin
  FAdminPassword := AAdminPassword;
end;

function TCrypterPlugIn.GetUseVisitorPassword: Boolean;
begin
  Result := FUseVisitorPassword;
end;

procedure TCrypterPlugIn.SetUseVisitorPassword(AUseVisitorPassword: Boolean);
begin
  FUseVisitorPassword := AUseVisitorPassword;
end;

function TCrypterPlugIn.GetVisitorPassword: WideString;
begin
  Result := FVisitorPassword;
end;

procedure TCrypterPlugIn.SetVisitorPassword(AVisitorPassword: WideString);
begin
  FVisitorPassword := AVisitorPassword;
end;

end.
