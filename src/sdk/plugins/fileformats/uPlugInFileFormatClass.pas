unit uPlugInFileFormatClass;

interface

uses
  // Common
  uAppInterface,
  // Plugin system
  uPlugInInterfaceAdv, uPlugInClass;

type
  TFileFormatPlugIn = class(TPlugIn, IFileFormatPlugIn)
  private
    FForceAddCrypter, FForceAddImageMirror: WordBool;
  protected
    function GetForceAddCrypter: WordBool; safecall;
    procedure SetForceAddCrypter(AForceAddCrypter: WordBool); safecall;
    function GetForceAddImageMirror: WordBool; safecall;
    procedure SetForceAddImageMirror(AForceAddImageMirror: WordBool); safecall;
  public
    function GetFileFormatName: WideString; virtual; safecall; abstract;
    function CanSaveControls: WordBool; virtual; safecall; abstract;
    procedure SaveControls(AFileName, ATemplateFileName: WideString; const ATabSheetController: ITabSheetController); virtual; safecall; abstract;
    function CanLoadControls: WordBool; virtual; safecall; abstract;
    function LoadControls(AFileName, ATemplateDirectory: WideString; const APageController: IPageController): Integer; virtual; safecall; abstract;
    property ForceAddCrypter: WordBool read GetForceAddCrypter write SetForceAddCrypter;
    property ForceAddImageMirror: WordBool read GetForceAddImageMirror write SetForceAddImageMirror;
  end;

implementation

{ TFileFormatPlugIn }

function TFileFormatPlugIn.GetForceAddCrypter: WordBool;
begin
  Result := FForceAddCrypter;
end;

procedure TFileFormatPlugIn.SetForceAddCrypter(AForceAddCrypter: WordBool);
begin
  FForceAddCrypter := AForceAddCrypter;
end;

function TFileFormatPlugIn.GetForceAddImageMirror: WordBool;
begin
  Result := FForceAddImageMirror;
end;

procedure TFileFormatPlugIn.SetForceAddImageMirror(AForceAddImageMirror: WordBool);
begin
  FForceAddImageMirror := AForceAddImageMirror;
end;

end.
