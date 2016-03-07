{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn interface advanced                           *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
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
    function Stop: WordBool; safecall;
  end;

  IFileFormatData = interface(IUnknown)
    ['{379BE81E-60DC-4D15-9B73-1A99D803C5DF}']
    function GetForceAddCrypter: WordBool; safecall;
    procedure SetForceAddCrypter(const AForceAddCrypter: WordBool); safecall;
    function GetForceAddImageMirror: WordBool; safecall;
    procedure SetForceAddImageMirror(const AForceAddImageMirror: WordBool); safecall;

    property ForceAddCrypter: WordBool read GetForceAddCrypter write SetForceAddCrypter;
    property ForceAddImageMirror: WordBool read GetForceAddImageMirror write SetForceAddImageMirror;
  end;

  IFileFormatPlugIn = interface(IPlugIn)
    ['{8A7373D1-51F3-4A98-912F-D3480274A715}']
    function GetFileExtension: WideString; safecall;
    function GetFileFilter: WideString; safecall;

    function CanSaveFiles: WordBool; safecall;
    function SaveFile(const AFileName: WideString; const ATabSheetController: ITabSheetController): WordBool; safecall;

    function CanLoadFiles: WordBool; safecall;
    function LoadFile(const AFileFormatData: IFileFormatData; const AFileName: WideString; const APageController: IPageController): Integer; safecall;
  end;

  TLoadAppPlugIn = function(var APlugIn: IAppPlugIn): WordBool; safecall;
  TLoadFileFormatPlugIn = function(var APlugIn: IFileFormatPlugIn): WordBool; safecall;

implementation

end.
