unit uApiPluginsClasses;

interface

uses
  // Delphi
  SysUtils, Classes,
  // Common
  uBaseConst, uBaseInterface,
  // Plugin system
  uPlugInConst, uPlugInInterface;

type
  TIAccountData = class(TInterfacedObject, IAccountData)
  private
    FUseAccount: WordBool;
    FAccountName, FAccountPassword: WideString;
  protected
    function GetUseAccount: WordBool; safecall;
    procedure SetUseAccount(AUseAccount: WordBool); safecall;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(const AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(const AAccountPassword: WideString); safecall;
  public
    property UseAccount: WordBool read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;
  end;

  TICMSData = class(TIAccountData, ICMSData)
  private
    FSettingsFileName, FWebsite: WideString;
  protected
    function GetSettingsFileName: WideString; safecall;
    procedure SetSettingsFileName(const ASettingsFileName: WideString); safecall;

    function GetWebsite: WideString; safecall;
    procedure SetWebsite(const AWebsite: WideString); safecall;
  public
    property AccountName;
    property AccountPassword;

    property SettingsFileName: WideString read GetSettingsFileName write SetSettingsFileName;

    property Website: WideString read GetWebsite write SetWebsite;
  end;

  TICrypterData = class(TIAccountData, ICrypterData)
  private
    FUseCaptcha, FUseAdvertismentLink, FUseAdvertismentPicture, FUseCoverLink, FUseDescription, FUseCNL, FUseWebseiteLink, FUseEMailforStatusNotice, FUseFilePassword, FUseAdminPassword, FUseVisitorPassword: WordBool;

    FFoldername, FAdvertismentLayerName, FAdvertismentLayerValue, FAdvertismentLink, FAdvertismentPicture, FDescription, FCoverLink, FWebseiteLink, FEMailforStatusNotice, FFilePassword, FAdminPassword, FVisitorPassword: WideString;

    FFoldertypes, FContainerTypes, FAdvertismentTyp: Integer;
  protected
    function GetUseCoverLink: WordBool; safecall;
    procedure SetUseCoverLink(AUseCoverLink: WordBool); safecall;
    function GetCoverLink: WideString; safecall;
    procedure SetCoverLink(const ACoverLink: WideString); safecall;
    function GetUseDescription: WordBool; safecall;
    procedure SetUseDescription(AUseDescription: WordBool); safecall;
    function GetDescription: WideString; safecall;
    procedure SetDescription(const ADescription: WideString); safecall;
    function GetUseCNL: WordBool; safecall;
    procedure SetUseCNL(AUseCNL: WordBool); safecall;
    function GetUseWebseiteLink: WordBool; safecall;
    procedure SetUseWebseiteLink(AUseWebseiteLink: WordBool); safecall;
    function GetWebseiteLink: WideString; safecall; safecall;
    procedure SetWebseiteLink(const AWebseiteLink: WideString); safecall;

    function GetFoldertypes: Integer; safecall;
    procedure SetFoldertypes(AFoldertypes: Integer); safecall;
    function GetContainerTypes: Integer; safecall;
    procedure SetContainerTypes(AContainerTypes: Integer); safecall;
    function GetUseCaptcha: WordBool; safecall;
    procedure SetUseCaptcha(AUseCaptcha: WordBool); safecall;
    function GetFolderName: WideString; safecall;
    procedure SetFolderName(const AFolderName: WideString); safecall;

    function GetAdvertismentType: Integer; safecall;
    procedure SetAdvertismentType(AAdvertismentType: Integer); safecall;
    function GetAdvertismentLayerName: WideString; safecall;
    procedure SetAdvertismentLayerName(const AAdvertismentLayerName: WideString); safecall;
    function GetAdvertismentLayerValue: WideString; safecall;
    procedure SetAdvertismentLayerValue(const AAdvertismentLayerValue: WideString); safecall;
    function GetUseAdvertismentLink: WordBool; safecall;
    procedure SetUseAdvertismentLink(AUseAdvertismentLink: WordBool); safecall;
    function GetAdvertismentLink: WideString; safecall;
    procedure SetAdvertismentLink(const AAdvertismentLink: WideString); safecall;
    function GetUseAdvertismentPicture: WordBool; safecall;
    procedure SetUseAdvertismentPicture(AUseAdvertismentPicture: WordBool); safecall;
    function GetAdvertismentPicture: WideString; safecall;
    procedure SetAdvertismentPicture(const AAdvertismentPicture: WideString); safecall;

    function GetUseEMailforStatusNotice: WordBool; safecall;
    procedure SetUseEMailforStatusNotice(AUseEMailforStatusNotice: WordBool); safecall;
    function GetEMailforStatusNotice: WideString; safecall;
    procedure SetEMailforStatusNotice(const AEMailforStatusNotice: WideString); safecall;
    function GetUseFilePassword: WordBool; safecall;
    procedure SetUseFilePassword(AUseFilePassword: WordBool); safecall;
    function GetFilePassword: WideString; safecall;
    procedure SetFilePassword(const AFilePassword: WideString); safecall;
    function GetUseAdminPassword: WordBool; safecall;
    procedure SetUseAdminPassword(AUseAdminPassword: WordBool); safecall;
    function GetAdminPassword: WideString; safecall;
    procedure SetAdminPassword(const AAdminPassword: WideString); safecall;
    function GetUseVisitorPassword: WordBool; safecall;
    procedure SetUseVisitorPassword(AUseVisitorPassword: WordBool); safecall;
    function GetVisitorPassword: WideString; safecall;
    procedure SetVisitorPassword(const AVisitorPassword: WideString); safecall;
  public
    property UseAccount;
    property AccountName;
    property AccountPassword;

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
  end;

  TIFileFormatPlugIn = class(TInterfacedObject, IFileFormatPlugIn)
  private
    FForceAddCrypter, FForceAddImageMirror: WordBool;
  protected
    function GetForceAddCrypter: WordBool; safecall;
    procedure SetForceAddCrypter(const AForceAddCrypter: WordBool); safecall;
    function GetForceAddImageMirror: WordBool; safecall;
    procedure SetForceAddImageMirror(const AForceAddImageMirror: WordBool); safecall;
  public
    property ForceAddCrypter: WordBool read GetForceAddCrypter write SetForceAddCrypter;
    property ForceAddImageMirror: WordBool read GetForceAddImageMirror write SetForceAddImageMirror;
  end;

  TIImageHosterData = class(TIAccountData, IImageHosterData)
  private
    FImageHostResize: TImageHostResize;
  protected
    function GetImageHostResize: TImageHostResize; safecall;
    procedure SetImageHostResize(AImageHostResize: TImageHostResize); safecall;
  public
    property UseAccount;
    property AccountName;
    property AccountPassword;

    property ImageHostResize: TImageHostResize read GetImageHostResize write SetImageHostResize;
  end;

implementation

{ TIAccountData }

function TIAccountData.GetAccountName: WideString;
begin
  Result := FAccountName;
end;

procedure TIAccountData.SetAccountName(const AAccountName: WideString);
begin
  FAccountName := AAccountName;
end;

function TIAccountData.GetAccountPassword: WideString;
begin
  Result := FAccountPassword;
end;

procedure TIAccountData.SetAccountPassword(const AAccountPassword: WideString);
begin
  FAccountPassword := AAccountPassword;
end;

{ TICMSData }

function TICMSData.GetSettingsFileName: WideString;
begin
  Result := FSettingsFileName;
end;

procedure TICMSData.SetSettingsFileName(const ASettingsFileName: WideString);
begin
  FSettingsFileName := ASettingsFileName;
end;

function TICMSData.GetWebsite: WideString;
begin
  Result := FWebsite;
end;

procedure TICMSData.SetWebsite(const AWebsite: WideString);
begin
  FWebsite := AWebsite;
end;

{ TICrypterData }

function TICrypterData.GetUseAccount: WordBool;
begin
  Result := FUseAccount;
end;

procedure TICrypterData.SetUseAccount(AUseAccount: WordBool);
begin
  FUseAccount := AUseAccount;
end;

function TICrypterData.GetUseCoverLink: WordBool;
begin
  Result := FUseCoverLink;
end;

procedure TICrypterData.SetUseCoverLink(AUseCoverLink: WordBool);
begin
  FUseCoverLink := AUseCoverLink;
end;

function TICrypterData.GetCoverLink: WideString;
begin
  Result := FCoverLink;
end;

procedure TICrypterData.SetCoverLink(const ACoverLink: WideString);
begin
  FCoverLink := ACoverLink;
end;

function TICrypterData.GetUseDescription: WordBool;
begin
  Result := FUseDescription;
end;

procedure TICrypterData.SetUseDescription(AUseDescription: WordBool);
begin
  FUseDescription := AUseDescription;
end;

function TICrypterData.GetDescription: WideString;
begin
  Result := FDescription;
end;

procedure TICrypterData.SetDescription(const ADescription: WideString);
begin
  FDescription := ADescription;
end;

function TICrypterData.GetUseCNL: WordBool;
begin
  Result := FUseCNL;
end;

procedure TICrypterData.SetUseCNL(AUseCNL: WordBool);
begin
  FUseCNL := AUseCNL;
end;

function TICrypterData.GetUseWebseiteLink: WordBool;
begin
  Result := FUseWebseiteLink;
end;

procedure TICrypterData.SetUseWebseiteLink(AUseWebseiteLink: WordBool);
begin
  FUseWebseiteLink := AUseWebseiteLink;
end;

function TICrypterData.GetWebseiteLink: WideString;
begin
  Result := FWebseiteLink;
end;

procedure TICrypterData.SetWebseiteLink(const AWebseiteLink: WideString);
begin
  FWebseiteLink := AWebseiteLink;
end;

function TICrypterData.GetFoldertypes: Integer;
begin
  Result := FFoldertypes;
end;

procedure TICrypterData.SetFoldertypes(AFoldertypes: Integer);
begin
  FFoldertypes := AFoldertypes;
end;

function TICrypterData.GetContainerTypes: Integer;
begin
  Result := FContainerTypes;
end;

procedure TICrypterData.SetContainerTypes(AContainerTypes: Integer);
begin
  FContainerTypes := AContainerTypes;
end;

function TICrypterData.GetUseCaptcha: WordBool;
begin
  Result := FUseCaptcha;
end;

procedure TICrypterData.SetUseCaptcha(AUseCaptcha: WordBool);
begin
  FUseCaptcha := AUseCaptcha;
end;

function TICrypterData.GetFolderName: WideString;
begin
  Result := FFoldername;
end;

procedure TICrypterData.SetFolderName(const AFolderName: WideString);
begin
  FFoldername := AFolderName;
end;

function TICrypterData.GetAdvertismentType;
begin
  Result := FAdvertismentTyp;
end;

procedure TICrypterData.SetAdvertismentType(AAdvertismentType: Integer);
begin
  FAdvertismentTyp := AAdvertismentType;
end;

function TICrypterData.GetAdvertismentLayerName;
begin
  Result := FAdvertismentLayerName;
end;

procedure TICrypterData.SetAdvertismentLayerName(const AAdvertismentLayerName: WideString);
begin
  FAdvertismentLayerName := AAdvertismentLayerName;
end;

function TICrypterData.GetAdvertismentLayerValue;
begin
  Result := FAdvertismentLayerValue;
end;

procedure TICrypterData.SetAdvertismentLayerValue(const AAdvertismentLayerValue: WideString);
begin
  FAdvertismentLayerValue := AAdvertismentLayerValue;
end;

function TICrypterData.GetUseAdvertismentLink;
begin
  Result := FUseAdvertismentLink;
end;

procedure TICrypterData.SetUseAdvertismentLink(AUseAdvertismentLink: WordBool);
begin
  FUseAdvertismentLink := AUseAdvertismentLink;
end;

function TICrypterData.GetAdvertismentLink;
begin
  Result := FAdvertismentLink;
end;

procedure TICrypterData.SetAdvertismentLink(const AAdvertismentLink: WideString);
begin
  FAdvertismentLink := AAdvertismentLink;
end;

function TICrypterData.GetUseAdvertismentPicture;
begin
  Result := FUseAdvertismentPicture;
end;

procedure TICrypterData.SetUseAdvertismentPicture(AUseAdvertismentPicture: WordBool);
begin
  FUseAdvertismentPicture := AUseAdvertismentPicture;
end;

function TICrypterData.GetAdvertismentPicture;
begin
  Result := FAdvertismentPicture;
end;

procedure TICrypterData.SetAdvertismentPicture(const AAdvertismentPicture: WideString);
begin
  FAdvertismentPicture := AAdvertismentPicture;
end;

function TICrypterData.GetUseEMailforStatusNotice: WordBool;
begin
  Result := FUseEMailforStatusNotice;
end;

procedure TICrypterData.SetUseEMailforStatusNotice(AUseEMailforStatusNotice: WordBool);
begin
  FUseEMailforStatusNotice := AUseEMailforStatusNotice;
end;

function TICrypterData.GetEMailforStatusNotice: WideString;
begin
  Result := FEMailforStatusNotice;
end;

procedure TICrypterData.SetEMailforStatusNotice(const AEMailforStatusNotice: WideString);
begin
  FEMailforStatusNotice := AEMailforStatusNotice;
end;

function TICrypterData.GetUseFilePassword: WordBool;
begin
  Result := FUseFilePassword;
end;

procedure TICrypterData.SetUseFilePassword(AUseFilePassword: WordBool);
begin
  FUseFilePassword := AUseFilePassword;
end;

function TICrypterData.GetFilePassword: WideString;
begin
  Result := FFilePassword;
end;

procedure TICrypterData.SetFilePassword(const AFilePassword: WideString);
begin
  FFilePassword := AFilePassword;
end;

function TICrypterData.GetUseAdminPassword: WordBool;
begin
  Result := FUseAdminPassword;
end;

procedure TICrypterData.SetUseAdminPassword(AUseAdminPassword: WordBool);
begin
  FUseAdminPassword := AUseAdminPassword;
end;

function TICrypterData.GetAdminPassword: WideString;
begin
  Result := FAdminPassword;
end;

procedure TICrypterData.SetAdminPassword(const AAdminPassword: WideString);
begin
  FAdminPassword := AAdminPassword;
end;

function TICrypterData.GetUseVisitorPassword: WordBool;
begin
  Result := FUseVisitorPassword;
end;

procedure TICrypterData.SetUseVisitorPassword(AUseVisitorPassword: WordBool);
begin
  FUseVisitorPassword := AUseVisitorPassword;
end;

function TICrypterData.GetVisitorPassword: WideString;
begin
  Result := FVisitorPassword;
end;

procedure TICrypterData.SetVisitorPassword(const AVisitorPassword: WideString);
begin
  FVisitorPassword := AVisitorPassword;
end;

{ TIFileFormatPlugIn }

function TIFileFormatPlugIn.GetForceAddCrypter: WordBool;
begin
  Result := FForceAddCrypter;
end;

procedure TIFileFormatPlugIn.SetForceAddCrypter(const AForceAddCrypter: WordBool);
begin
  FForceAddCrypter := AForceAddCrypter;
end;

function TIFileFormatPlugIn.GetForceAddImageMirror: WordBool;
begin
  Result := FForceAddImageMirror;
end;

procedure TIFileFormatPlugIn.SetForceAddImageMirror(const AForceAddImageMirror: WordBool);
begin
  FForceAddImageMirror := AForceAddImageMirror;
end;

{ TIImageHosterData }

function TIImageHosterData.GetUseAccount: WordBool;
begin
  Result := FUseAccount;
end;

procedure TIImageHosterData.SetUseAccount(AUseAccount: WordBool);
begin
  FUseAccount := AUseAccount;
end;

function TIImageHosterData.GetImageHostResize: TImageHostResize;
begin
  Result := FImageHostResize;
end;

procedure TIImageHosterData.SetImageHostResize(AImageHostResize: TImageHostResize);
begin
  FImageHostResize := AImageHostResize;
end;

end.
