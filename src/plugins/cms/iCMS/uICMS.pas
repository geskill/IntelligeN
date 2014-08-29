unit uICMS;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Controls, Variants, XMLDoc, XMLIntf, ActiveX,
  // Indy
  IdMultipartFormData,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInConst, uPlugInCMSSettingsHelper, uIdHTTPHelper;

type
  TICMS = class(TCMSPlugIn)
  private
    ICMSSettings: TCMSPlugInSettings;
  protected
    function LoadSettings(AComponentController: IComponentController = nil): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetName: WideString; override; safecall;
    function CMSType: TCMSType; override;
    function DefaultCharset: WideString; override;
    function BelongsTo(AWebsiteSourceCode: WideString): Boolean; override;
    function GetIDs: Integer; override;
    function Exec(ComponentController: IComponentController; MirrorController: IMirrorController): Boolean; override;
    function ShowWebsiteSettingsEditor(AWebsiteEditor: IWebsiteEditor): Boolean; override;
  end;

implementation

function TICMS.LoadSettings(AComponentController: IComponentController): Boolean;
begin
  Result := True;
  TPlugInCMSSettingsHelper.LoadSettingsToClass(SettingsFileName, ICMSSettings, AComponentController);
  with ICMSSettings do
  begin
    if SameStr('', Charset) then
      Charset := DefaultCharset;
  end;
end;

constructor TICMS.Create;
begin
  inherited Create;
  ICMSSettings := TCMSPlugInSettings.Create;
end;

destructor TICMS.Destroy;
begin
  ICMSSettings.Free;
  inherited Destroy;
end;

function TICMS.GetName;
begin
  Result := 'ICMS';
end;

function TICMS.CMSType;
begin
  Result := cmsFormbased;
end;

function TICMS.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TICMS.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := False;
end;

function TICMS.GetIDs: Integer;
begin
  Result := 0;
end;

function TICMS.Exec;
var
  XMLDoc: IXMLDocument;
  I, J: Integer;
  Params: TIdMultiPartFormDataStream;
  MirrorPrefix, MirrorCrypterPrefix: string;
  ResponseStr: string;
begin
  Result := False;

  LoadSettings(ComponentController);

  with TIdHTTPHelper.Create(Self) do
    try
      Params := TIdMultiPartFormDataStream.Create;
      try
        with Params do
        begin
          if not(AccountName = '') then
          begin
            AddFormField('account_username', AccountName, ICMSSettings.Charset).ContentTransfer := 'binary';
            AddFormField('account_password', AccountPassword, ICMSSettings.Charset).ContentTransfer := 'binary';
          end;

          AddFormField('action', 'add_entry', ICMSSettings.Charset);

          AddFormField('IType', TTemplateTypeIDToString(ComponentController.TemplateTypeID), ICMSSettings.Charset).ContentTransfer := 'binary';
          AddFormField('ISubject', Subject, ICMSSettings.Charset).ContentTransfer := 'binary';
          AddFormField('ITags', Tags, ICMSSettings.Charset).ContentTransfer := 'binary';
          AddFormField('IMessage', Message, ICMSSettings.Charset).ContentTransfer := 'binary';

          for I := 0 to ComponentController.ControlCount - 1 do
            with ComponentController.Control[I] do
              AddFormField(TComponentIDToString(ComponentID), Value, ICMSSettings.Charset).ContentTransfer := 'binary';

          AddFormField('MirrorCount', IntToStr(MirrorController.MirrorCount), ICMSSettings.Charset);
          for I := 0 to MirrorController.MirrorCount - 1 do
            with MirrorController.Mirror[I] do
            begin
              MirrorPrefix := 'Mirror_' + IntToStr(I) + '_';
              AddFormField(MirrorPrefix + 'Size', FloatToStr(Size), ICMSSettings.Charset).ContentTransfer := 'binary';
              AddFormField(MirrorPrefix + 'PartSize', FloatToStr(PartSize), ICMSSettings.Charset).ContentTransfer := 'binary';
              AddFormField(MirrorPrefix + 'Hoster', Hoster, ICMSSettings.Charset).ContentTransfer := 'binary';
              AddFormField(MirrorPrefix + 'HosterShort', GetHoster(True), ICMSSettings.Charset).ContentTransfer := 'binary';
              AddFormField(MirrorPrefix + 'Parts', IntToStr(Parts), ICMSSettings.Charset).ContentTransfer := 'binary';

              AddFormField(MirrorPrefix + 'DirectlinksMirrorCount', IntToStr(DirectlinksMirrorCount), ICMSSettings.Charset).ContentTransfer := 'binary';
              for J := 0 to DirectlinksMirrorCount - 1 do
                AddFormField(MirrorPrefix + 'DirectlinksMirror_' + IntToStr(J), DirectlinksMirror[J], ICMSSettings.Charset).ContentTransfer := 'binary';

              AddFormField(MirrorPrefix + 'CrypterCount', IntToStr(CrypterCount), ICMSSettings.Charset).ContentTransfer := 'binary';
              for J := 0 to CrypterCount - 1 do
              begin
                MirrorCrypterPrefix := 'Mirror_' + IntToStr(I) + '_Crypter_' + IntToStr(J) + '_';
                AddFormField(MirrorCrypterPrefix + 'Name', Crypter[J].name, ICMSSettings.Charset).ContentTransfer := 'binary';
                AddFormField(MirrorCrypterPrefix + 'Link', Crypter[J].Link, ICMSSettings.Charset).ContentTransfer := 'binary';
                AddFormField(MirrorCrypterPrefix + 'Size', FloatToStr(Crypter[J].Size), ICMSSettings.Charset).ContentTransfer := 'binary';
                AddFormField(MirrorCrypterPrefix + 'Hoster', Crypter[J].Hoster, ICMSSettings.Charset).ContentTransfer := 'binary';
                AddFormField(MirrorCrypterPrefix + 'HosterShort', Crypter[J].HosterShort, ICMSSettings.Charset).ContentTransfer := 'binary';
                AddFormField(MirrorCrypterPrefix + 'Parts', IntToStr(Crypter[J].Parts), ICMSSettings.Charset).ContentTransfer := 'binary';
                AddFormField(MirrorCrypterPrefix + 'StatusImage', Crypter[J].StatusImage, ICMSSettings.Charset).ContentTransfer := 'binary';
                AddFormField(MirrorCrypterPrefix + 'StatusImageText', Crypter[J].StatusImageText, ICMSSettings.Charset).ContentTransfer := 'binary';
              end;
            end;

          try
            ResponseStr := Post(Website + 'icms_add_entry.php', Params);
          except
            on E: Exception do
            begin
              ErrorMsg := E.message;
              Exit;
            end;
          end;

          try
            OleInitialize(nil);
            try
              XMLDoc := NewXMLDocument;
              try
                with XMLDoc do
                begin
                  LoadFromXML(ResponseStr);
                  Active := True;
                end;
                with XMLDoc.DocumentElement do
                  if HasChildNodes then
                  begin
                    with ChildNodes.Nodes['code'] do
                      Result := NodeValue;
                    with ChildNodes.Nodes['msg'] do
                      ErrorMsg := VarToWideStr(NodeValue);
                  end;
              finally
                XMLDoc := nil;
              end;
            finally
              OleUninitialize;
            end;
          except
            ErrorMsg := 'error parsing ICMS-RESULT-XML (' + SysErrorMessage(GetLastError()) + ')';
            Exit;
          end;
        end;
      finally
        Params.Free;
      end;
    finally
      Free;
    end;
end;

function TICMS.ShowWebsiteSettingsEditor;
begin
  TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(SettingsFileName, TCMSPlugInSettings, AWebsiteEditor);
  Result := IsPositiveResult(AWebsiteEditor.ShowModal);
end;

end.
