unit uPlugInCrypterClass;

interface

uses
  // Common
  uBaseInterface,
  // Plugin
  uPlugInConst, uPlugInInterface, uPlugInClass;

type
  TCrypterPlugIn = class(TPlugIn, ICrypterPlugIn)
  private
    FUseAccount, FUseCaptcha, FUseAdvertismentLink, FUseAdvertismentPicture, FUseCoverLink, FUseDescription, FUseCNL, FUseWebseiteLink,
      FUseEMailforStatusNotice, FUseFilePassword, FUseAdminPassword, FUseVisitorPassword: WordBool;

    FAccountName, FAccountPassword, FFoldername, FAdvertismentLayerName, FAdvertismentLayerValue, FAdvertismentLink, FAdvertismentPicture, FDescription,
      FCoverLink, FWebseiteLink, FEMailforStatusNotice, FFilePassword, FAdminPassword, FVisitorPassword: WideString;

    FFoldertypes, FContainerTypes, FAdvertismentTyp: Integer;
  protected
    function GetUseAccount: WordBool; safecall;
    procedure SetUseAccount(AUseAccount: WordBool); safecall;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString); safecall;

    function GetUseCoverLink: WordBool; safecall;
    procedure SetUseCoverLink(AUseCoverLink: WordBool); safecall;
    function GetCoverLink: WideString; safecall;
    procedure SetCoverLink(ACoverLink: WideString); safecall;
    function GetUseDescription: WordBool; safecall;
    procedure SetUseDescription(AUseDescription: WordBool); safecall;
    function GetDescription: WideString; safecall;
    procedure SetDescription(ADescription: WideString); safecall;
    function GetUseCNL: WordBool; safecall;
    procedure SetUseCNL(AUseCNL: WordBool); safecall;
    function GetUseWebseiteLink: WordBool; safecall;
    procedure SetUseWebseiteLink(AUseWebseiteLink: WordBool); safecall;
    function GetWebseiteLink: WideString; safecall; safecall;
    procedure SetWebseiteLink(AWebseiteLink: WideString); safecall;

    function GetFoldertypes: Integer; safecall;
    procedure SetFoldertypes(AFoldertypes: Integer); safecall;
    function GetContainerTypes: Integer; safecall;
    procedure SetContainerTypes(AContainerTypes: Integer); safecall;
    function GetUseCaptcha: WordBool; safecall;
    procedure SetUseCaptcha(AUseCaptcha: WordBool); safecall;
    function GetFolderName: WideString; safecall;
    procedure SetFolderName(AFolderName: WideString); safecall;

    function GetAdvertismentType: Integer; safecall;
    procedure SetAdvertismentType(AAdvertismentType: Integer); safecall;
    function GetAdvertismentLayerName: WideString; safecall;
    procedure SetAdvertismentLayerName(AAdvertismentLayerName: WideString); safecall;
    function GetAdvertismentLayerValue: WideString; safecall;
    procedure SetAdvertismentLayerValue(AAdvertismentLayerValue: WideString); safecall;
    function GetUseAdvertismentLink: WordBool; safecall;
    procedure SetUseAdvertismentLink(AUseAdvertismentLink: WordBool); safecall;
    function GetAdvertismentLink: WideString; safecall;
    procedure SetAdvertismentLink(AAdvertismentLink: WideString); safecall;
    function GetUseAdvertismentPicture: WordBool; safecall;
    procedure SetUseAdvertismentPicture(AUseAdvertismentPicture: WordBool); safecall;
    function GetAdvertismentPicture: WideString; safecall;
    procedure SetAdvertismentPicture(AAdvertismentPicture: WideString); safecall;

    function GetUseEMailforStatusNotice: WordBool; safecall;
    procedure SetUseEMailforStatusNotice(AUseEMailforStatusNotice: WordBool); safecall;
    function GetEMailforStatusNotice: WideString; safecall;
    procedure SetEMailforStatusNotice(AEMailforStatusNotice: WideString); safecall;
    function GetUseFilePassword: WordBool; safecall;
    procedure SetUseFilePassword(AUseFilePassword: WordBool); safecall;
    function GetFilePassword: WideString; safecall;
    procedure SetFilePassword(AFilePassword: WideString); safecall;
    function GetUseAdminPassword: WordBool; safecall;
    procedure SetUseAdminPassword(AUseAdminPassword: WordBool); safecall;
    function GetAdminPassword: WideString; safecall;
    procedure SetAdminPassword(AAdminPassword: WideString); safecall;
    function GetUseVisitorPassword: WordBool; safecall;
    procedure SetUseVisitorPassword(AUseVisitorPassword: WordBool); safecall;
    function GetVisitorPassword: WideString; safecall;
    procedure SetVisitorPassword(AVisitorPassword: WideString); safecall;
  public
    function GetType: TPlugInType; override; safecall;

    property UseAccount: WordBool read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    property Foldertypes: Integer read GetFoldertypes write SetFoldertypes;
    property ContainerTypes: Integer read GetContainerTypes write SetContainerTypes;
    property UseCaptcha: WordBool read GetUseCaptcha write SetUseCaptcha;
    property FolderName: WideString read GetFolderName write SetFolderName;

    property AdvertismentType: Integer read GetAdvertismentType write SetAdvertismentType;
    property AdvertismentLayerName: WideString read GetAdvertismentLayerName write SetAdvertismentLayerName;
    property AdvertismentLayerValue: WideString read GetAdvertismentLayerValue write SetAdvertismentLayerValue;
    property UseAdvertismentLink: WordBool read GetUseAdvertismentLink write SetUseAdvertismentLink;
    property AdvertismentLink: WideString read GetAdvertismentLink write SetAdvertismentLink;
    property UseAdvertismentPicture: WordBool read GetUseAdvertismentPicture write SetUseAdvertismentPicture;
    property AdvertismentPicture: WideString read GetAdvertismentPicture write SetAdvertismentPicture;

    property UseCoverLink: WordBool read GetUseCoverLink write SetUseCoverLink;
    property CoverLink: WideString read GetCoverLink write SetCoverLink;
    property UseDescription: WordBool read GetUseDescription write SetUseDescription;
    property Description: WideString read GetDescription write SetDescription;
    property UseCNL: WordBool read GetUseCNL write SetUseCNL;
    property UseWebseiteLink: WordBool read GetUseWebseiteLink write SetUseWebseiteLink;
    property WebseiteLink: WideString read GetWebseiteLink write SetWebseiteLink;

    property UseEMailforStatusNotice: WordBool read GetUseEMailforStatusNotice write SetUseEMailforStatusNotice;
    property EMailforStatusNotice: WideString read GetEMailforStatusNotice write SetEMailforStatusNotice;

    property UseFilePassword: WordBool read GetUseFilePassword write SetUseFilePassword;
    property FilePassword: WideString read GetFilePassword write SetFilePassword;
    property UseAdminPassword: WordBool read GetUseAdminPassword write SetUseAdminPassword;
    property AdminPassword: WideString read GetAdminPassword write SetAdminPassword;
    property UseVisitorPassword: WordBool read GetUseVisitorPassword write SetUseVisitorPassword;
    property Visitorpassword: WideString read GetVisitorPassword write SetVisitorPassword;

    function AddFolder(const AMirrorContainer: IDirectlinkContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; virtual; safecall; abstract;
    function EditFolder(const AMirrorContainer: IDirectlinkContainer; var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; virtual; safecall; abstract;
    function DeleteFolder(AFolderIdentifier: WideString): WordBool; virtual; safecall; abstract;
    function GetFolder(AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; virtual; safecall; abstract;
  end;

implementation

{ TCrypterPlugIn }

function TCrypterPlugIn.GetUseAccount: WordBool;
begin
  Result := FUseAccount;
end;

procedure TCrypterPlugIn.SetUseAccount(AUseAccount: WordBool);
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

function TCrypterPlugIn.GetUseCoverLink: WordBool;
begin
  Result := FUseCoverLink;
end;

procedure TCrypterPlugIn.SetUseCoverLink(AUseCoverLink: WordBool);
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

function TCrypterPlugIn.GetUseDescription: WordBool;
begin
  Result := FUseDescription;
end;

procedure TCrypterPlugIn.SetUseDescription(AUseDescription: WordBool);
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

function TCrypterPlugIn.GetUseCNL: WordBool;
begin
  Result := FUseCNL;
end;

procedure TCrypterPlugIn.SetUseCNL(AUseCNL: WordBool);
begin
  FUseCNL := AUseCNL;
end;

function TCrypterPlugIn.GetUseWebseiteLink: WordBool;
begin
  Result := FUseWebseiteLink;
end;

procedure TCrypterPlugIn.SetUseWebseiteLink(AUseWebseiteLink: WordBool);
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

function TCrypterPlugIn.GetUseCaptcha: WordBool;
begin
  Result := FUseCaptcha;
end;

procedure TCrypterPlugIn.SetUseCaptcha(AUseCaptcha: WordBool);
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

procedure TCrypterPlugIn.SetUseAdvertismentLink(AUseAdvertismentLink: WordBool);
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

procedure TCrypterPlugIn.SetUseAdvertismentPicture(AUseAdvertismentPicture: WordBool);
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

function TCrypterPlugIn.GetUseEMailforStatusNotice: WordBool;
begin
  Result := FUseEMailforStatusNotice;
end;

procedure TCrypterPlugIn.SetUseEMailforStatusNotice(AUseEMailforStatusNotice: WordBool);
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

function TCrypterPlugIn.GetUseFilePassword: WordBool;
begin
  Result := FUseFilePassword;
end;

procedure TCrypterPlugIn.SetUseFilePassword(AUseFilePassword: WordBool);
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

function TCrypterPlugIn.GetUseAdminPassword: WordBool;
begin
  Result := FUseAdminPassword;
end;

procedure TCrypterPlugIn.SetUseAdminPassword(AUseAdminPassword: WordBool);
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

function TCrypterPlugIn.GetUseVisitorPassword: WordBool;
begin
  Result := FUseVisitorPassword;
end;

procedure TCrypterPlugIn.SetUseVisitorPassword(AUseVisitorPassword: WordBool);
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

function TCrypterPlugIn.GetType: TPlugInType;
begin
  Result := ptCrypter;
end;

end.
