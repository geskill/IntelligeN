{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn file formats class                           *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInFileFormatClass;

interface

uses
  // Common
  uAppInterface,
  // Plugin system
  uPlugInConst, uPlugInInterfaceAdv, uPlugInClass;

type
  TFileFormatPlugIn = class(TPlugIn, IFileFormatPlugIn)
  public
    function GetType: TPlugInType; override; safecall;

    function GetFileExtension: WideString; virtual; safecall; abstract;
    function GetFileFilter: WideString; virtual; safecall; abstract;

    function CanSaveFiles: WordBool; virtual; safecall; abstract;
    function SaveFile(const AFileName: WideString; const ATabSheetController: ITabSheetController): WordBool; virtual; safecall; abstract;

    function CanLoadFiles: WordBool; virtual; safecall; abstract;
    function LoadFile(const AFileFormatData: IFileFormatData; const AFileName: WideString; const APageController: IPageController): Integer; virtual; safecall; abstract;
  end;

implementation

{ TFileFormatPlugIn }

function TFileFormatPlugIn.GetType: TPlugInType;
begin
  Result := ptFileFormats;
end;

end.
