unit uPlugInInterfaceAdv;

interface

uses
  // Common
  uBaseInterface, uAppInterface,
  // HTTPManager
  uHTTPInterface,
  // Plugin
  uPlugInConst, uPlugInInterface;

type
  IAppPlugIn = interface(IPlugIn)
    ['{DB81AD44-5514-4F6E-BF24-663E8A0AD66A}']
    function Start(const AAppController: IAppController): WordBool; safecall;
    procedure Stop; safecall;
  end;

  IFileFormatPlugIn = interface(IPlugIn)
    ['{8A7373D1-51F3-4A98-912F-D3480274A715}']
    function GetForceAddCrypter: WordBool; safecall;
    procedure SetForceAddCrypter(AForceAddCrypter: WordBool); safecall;
    function GetForceAddImageMirror: WordBool; safecall;
    procedure SetForceAddImageMirror(AForceAddImageMirror: WordBool); safecall;

    function GetFileFormatName: WideString; safecall;
    function CanSaveControls: WordBool; safecall;
    procedure SaveControls(AFileName, ATemplateFileName: WideString; const ATabSheetController: ITabSheetController); safecall;
    function CanLoadControls: WordBool; safecall;
    function LoadControls(AFileName, ATemplateDirectory: WideString; const APageController: IPageController): Integer; safecall;
    property ForceAddCrypter: WordBool read GetForceAddCrypter write SetForceAddCrypter;
    property ForceAddImageMirror: WordBool read GetForceAddImageMirror write SetForceAddImageMirror;
  end;

  TLoadAppPlugIn = function(var APlugIn: IAppPlugIn): WordBool; safecall;
  TLoadFileFormatPlugIn = function(var APlugIn: IFileFormatPlugIn): WordBool; safecall;

implementation

end.
