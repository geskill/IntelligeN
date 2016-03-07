{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn crypter class                                *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInCrypterClass;

interface

uses
  // Common
  uBaseInterface,
  // Plugin
  uPlugInConst, uPlugInInterface, uPlugInClass;

type
  TCrypterPlugIn = class(TPlugIn, ICrypterPlugIn)
  public
    function GetType: TPlugInType; override; safecall;

    function GetServiceRequiresAccess: TCrypterAccess; virtual; safecall; abstract;

    function AddFolder(const ACrypterData: ICrypterData; const AMirrorContainer: IDirectlinkContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; virtual; safecall; abstract;
    function EditFolder(const ACrypterData: ICrypterData; const AMirrorContainer: IDirectlinkContainer; var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; virtual; safecall; abstract;
    function DeleteFolder(const AAccountData: IAccountData; const AFolderIdentifier: WideString): WordBool; virtual; safecall; abstract;
    function GetFolder(const AAccountData: IAccountData; const AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; virtual; safecall; abstract;
  end;

implementation

{ TCrypterPlugIn }

function TCrypterPlugIn.GetType: TPlugInType;
begin
  Result := ptCrypter;
end;

end.
