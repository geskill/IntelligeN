{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn image hoster class                           *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInImageHosterClass;

interface

uses
  // Plugin
  uPlugInConst, uPlugInInterface, uPlugInClass;

type

  TImageHosterPlugIn = class(TPlugIn, IImageHosterPlugIn)
  private
    FUseAccount: WordBool;

    FAccountName, FAccountPassword: WideString;

    FImageHostResize: TImageHostResize;
  protected
    function GetUseAccount: WordBool; safecall;
    procedure SetUseAccount(AUseAccount: WordBool); safecall;
    function GetAccountName: WideString; safecall;
    procedure SetAccountName(AAccountName: WideString); safecall;
    function GetAccountPassword: WideString; safecall;
    procedure SetAccountPassword(AAccountPassword: WideString); safecall;
    function GetImageHostResize: TImageHostResize; safecall;
    procedure SetImageHostResize(AImageHostResize: TImageHostResize); safecall;
  public
    function GetType: TPlugInType; override; safecall;

    property UseAccount: WordBool read GetUseAccount write SetUseAccount;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property AccountPassword: WideString read GetAccountPassword write SetAccountPassword;

    function LocalUpload(ALocalPath: WideString; out AUrl: WideString): WordBool; virtual; safecall; abstract;
    function RemoteUpload(ARemoteUrl: WideString; out AUrl: WideString): WordBool; virtual; safecall; abstract;

    property ImageHostResize: TImageHostResize read GetImageHostResize write SetImageHostResize;
  end;

implementation

{ TImageHosterPlugIn }

function TImageHosterPlugIn.GetUseAccount: WordBool;
begin
  Result := FUseAccount;
end;

procedure TImageHosterPlugIn.SetUseAccount(AUseAccount: WordBool);
begin
  FUseAccount := AUseAccount;
end;

function TImageHosterPlugIn.GetAccountName: WideString;
begin
  Result := FAccountName;
end;

procedure TImageHosterPlugIn.SetAccountName(AAccountName: WideString);
begin
  FAccountName := AAccountName;
end;

function TImageHosterPlugIn.GetAccountPassword: WideString;
begin
  Result := FAccountPassword;
end;

procedure TImageHosterPlugIn.SetAccountPassword(AAccountPassword: WideString);
begin
  FAccountPassword := AAccountPassword;
end;

function TImageHosterPlugIn.GetImageHostResize: TImageHostResize;
begin
  Result := FImageHostResize;
end;

procedure TImageHosterPlugIn.SetImageHostResize(AImageHostResize: TImageHostResize);
begin
  FImageHostResize := AImageHostResize;
end;

function TImageHosterPlugIn.GetType: TPlugInType;
begin
  Result := ptImageHoster;
end;

end.
