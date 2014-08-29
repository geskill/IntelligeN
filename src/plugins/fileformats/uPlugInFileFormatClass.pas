unit uPlugInFileFormatClass;

interface

uses
  // Common
  uAppInterface,
  // Plugin system
  uPlugInInterface, uPlugInClass;

type
  TFileFormatPlugIn = class(TPlugIn, IFileFormatPlugIn)
  private
    FForceAddCrypter: Boolean;
  protected
    function GetForceAddCrypter: Boolean;
    procedure SetForceAddCrypter(AForceAddCrypter: Boolean);
  public
    function GetFileFormatName: WideString; virtual; safecall; abstract;
    function CanSaveControls: Boolean; virtual; stdcall; abstract;
    procedure SaveControls(AFileName, ATemplateFileName: WideString; const ATabSheetController: ITabSheetController); virtual; stdcall; abstract;
    function CanLoadControls: Boolean; virtual; stdcall; abstract;
    function LoadControls(AFileName, ATemplateDirectory: WideString; const APageController: IPageController): Boolean; virtual; stdcall; abstract;
    property ForceAddCrypter: Boolean read GetForceAddCrypter write SetForceAddCrypter;
  end;

implementation

{ TFileFormatPlugIn }

function TFileFormatPlugIn.GetForceAddCrypter: Boolean;
begin
  Result := FForceAddCrypter;
end;

procedure TFileFormatPlugIn.SetForceAddCrypter(AForceAddCrypter: Boolean);
begin
  FForceAddCrypter := AForceAddCrypter;
end;

end.
