{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn file hoster classes                          *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInFileHosterClasses;

interface

uses
  // Delphi
  SysUtils, Classes, Variants,
  // Common
  uBaseConst,
  // Plugin
  uPlugInConst, uPlugInInterface, uPlugInClass, uPlugInClasses;

type
  TLinkInfo = class(TPlugInInfoObject, ILinkInfo)
  private
    FLink: WideString;
    FStatus: TLinkStatus;
    FSize: UInt64;
    FFileName, FChecksum: WideString;
    FChecksumType: TChecksumType;
  protected
    function GetLink: WideString; safecall;
    procedure SetLink(const ALink: WideString); safecall;
    function GetStatus: TLinkStatus; safecall;
    procedure SetStatus(const AStatus: TLinkStatus); safecall;
    function GetSize: UInt64; safecall;
    procedure SetSize(const ASize: UInt64); safecall;
    function GetFileName: WideString; safecall;
    procedure SetFileName(const AFileName: WideString); safecall;
    function GetChecksum: WideString; safecall;
    procedure SetChecksum(const AChecksum: WideString); safecall;
    function GetChecksumType: TChecksumType; safecall;
    procedure SetChecksumType(const AChecksumType: TChecksumType); safecall;
  public
    constructor Create; overload;
    constructor Create(const ALink, AFileName: string; const AStatus: TLinkStatus; const ASize: UInt64; const AChecksum: string = ''; const AChecksumType: TChecksumType = ctMD5); overload;
    destructor Destroy; override;

    property Link: WideString read GetLink write SetLink;
    property Status: TLinkStatus read GetStatus write SetStatus;
    property Size: UInt64 read GetSize write SetSize;
    property FileName: WideString read GetFileName write SetFileName;
    property Checksum: WideString read GetChecksum write SetChecksum;
    property ChecksumType: TChecksumType read GetChecksumType write SetChecksumType;
  end;

  TLinksInfo = class(TPlugInInfoObjectList, ILinksInfo)
  protected
    function GetLink(const AIndex: Integer): ILinkInfo; safecall;
  public
    function AddLink(const ALink: ILinkInfo): Integer; overload;
    function AddLink(const ALink, AFileName: string; const AStatus: TLinkStatus; const ASize: UInt64; const AChecksum: string = ''; const AChecksumType: TChecksumType = ctMD5): Integer; overload;

    property Count: Integer read GetCount;
    property Link[const AIndex: Integer]: ILinkInfo read GetLink;
  end;

implementation

{ TLinkInfo }

function TLinkInfo.GetLink: WideString;
begin
  Result := FLink;
end;

procedure TLinkInfo.SetLink(const ALink: WideString);
begin
  FLink := ALink;
end;

function TLinkInfo.GetStatus: TLinkStatus;
begin
  Result := FStatus;
end;

procedure TLinkInfo.SetStatus(const AStatus: TLinkStatus);
begin
  FStatus := AStatus;
end;

function TLinkInfo.GetSize: UInt64;
begin
  Result := FSize;
end;

procedure TLinkInfo.SetSize(const ASize: UInt64);
begin
  FSize := ASize;
end;

function TLinkInfo.GetFileName: WideString;
begin
  Result := FFileName;
end;

procedure TLinkInfo.SetFileName(const AFileName: WideString);
begin
  FFileName := AFileName;
end;

function TLinkInfo.GetChecksum: WideString;
begin
  Result := FChecksum;
end;

procedure TLinkInfo.SetChecksum(const AChecksum: WideString);
begin
  FChecksum := AChecksum;
end;

function TLinkInfo.GetChecksumType: TChecksumType;
begin
  Result := FChecksumType;
end;

procedure TLinkInfo.SetChecksumType(const AChecksumType: TChecksumType);
begin
  FChecksumType := AChecksumType;
end;

constructor TLinkInfo.Create;
begin
  Create('', '', csUnknown, 0);
end;

constructor TLinkInfo.Create(const ALink, AFileName: string; const AStatus: TLinkStatus; const ASize: UInt64; const AChecksum: string = ''; const AChecksumType: TChecksumType = ctMD5);
begin
  inherited Create;
  FLink := ALink;
  FStatus := AStatus;
  FSize := ASize;
  FFileName := AFileName;
  FChecksum := AChecksum;
  FChecksumType := AChecksumType;
end;

destructor TLinkInfo.Destroy;
begin
  inherited Destroy;
end;

{ TLinksInfo }

function TLinksInfo.AddLink(const ALink: ILinkInfo): Integer;
begin
  Result := AddElement(ALink);
end;

function TLinksInfo.AddLink(const ALink, AFileName: string; const AStatus: TLinkStatus; const ASize: UInt64; const AChecksum: string = ''; const AChecksumType: TChecksumType = ctMD5): Integer;
var
  LLinkInfo: ILinkInfo;
begin
  LLinkInfo := TLinkInfo.Create(ALink, AFileName, AStatus, ASize, AChecksum, AChecksumType);
  Result := AddLink(LLinkInfo);
end;

function TLinksInfo.GetLink(const AIndex: Integer): ILinkInfo;
begin
  Result := GetElement(AIndex) as ILinkInfo;
end;

end.
