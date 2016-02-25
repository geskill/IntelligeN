{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn file formats class                           *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
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
  private
    FForceAddCrypter, FForceAddImageMirror: WordBool;
  protected
    function GetForceAddCrypter: WordBool; safecall;
    procedure SetForceAddCrypter(const AForceAddCrypter: WordBool); safecall;
    function GetForceAddImageMirror: WordBool; safecall;
    procedure SetForceAddImageMirror(const AForceAddImageMirror: WordBool); safecall;
  public
    function GetType: TPlugInType; override; safecall;

    function GetFileExtension: WideString; virtual; safecall; abstract;
    function GetFileFilter: WideString; virtual; safecall; abstract;

    function CanSaveFiles: WordBool; virtual; safecall; abstract;
    function SaveFile(const AFileName: WideString; const ATabSheetController: ITabSheetController): WordBool; virtual; safecall; abstract;

    function CanLoadFiles: WordBool; virtual; safecall; abstract;
    function LoadFile(const AFileName: WideString; const APageController: IPageController): Integer; virtual; safecall; abstract;

    property ForceAddCrypter: WordBool read GetForceAddCrypter write SetForceAddCrypter;
    property ForceAddImageMirror: WordBool read GetForceAddImageMirror write SetForceAddImageMirror;
  end;

implementation

{ TFileFormatPlugIn }

function TFileFormatPlugIn.GetForceAddCrypter: WordBool;
begin
  Result := FForceAddCrypter;
end;

procedure TFileFormatPlugIn.SetForceAddCrypter(const AForceAddCrypter: WordBool);
begin
  FForceAddCrypter := AForceAddCrypter;
end;

function TFileFormatPlugIn.GetForceAddImageMirror: WordBool;
begin
  Result := FForceAddImageMirror;
end;

procedure TFileFormatPlugIn.SetForceAddImageMirror(const AForceAddImageMirror: WordBool);
begin
  FForceAddImageMirror := AForceAddImageMirror;
end;

function TFileFormatPlugIn.GetType: TPlugInType;
begin
  Result := ptFileFormats;
end;

end.
