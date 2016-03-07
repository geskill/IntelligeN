{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn image hoster class                           *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInImageHosterClass;

interface

uses
  // Plugin
  uPlugInConst, uPlugInInterface, uPlugInClass;

type
  TImageHosterPlugIn = class(TPlugIn, IImageHosterPlugIn)
  public
    function GetType: TPlugInType; override; safecall;

    function AddLocalImage(const AImageHosterData: IImageHosterData; const ALocalPath: WideString; out AUrl: WideString): WordBool; virtual; safecall; abstract;
    function AddWebImage(const AImageHosterData: IImageHosterData; const ARemoteUrl: WideString; out AUrl: WideString): WordBool; virtual; safecall; abstract;
  end;

implementation

{ TImageHosterPlugIn }

function TImageHosterPlugIn.GetType: TPlugInType;
begin
  Result := ptImageHoster;
end;

end.
