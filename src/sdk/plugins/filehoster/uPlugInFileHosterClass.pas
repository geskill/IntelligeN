unit uPlugInFileHosterClass;

interface

uses
  // Delphi
  SysUtils, Classes, Generics.Collections,
  // Plugin
  uPlugInInterface, uPlugInClass, uPlugInConst;

type
  TFileHosterPlugIn = class(TPlugIn, IFileHosterPlugIn)
  protected
    FCheckedLinksList: TList<TLinkInfo>;
    procedure AddLink(ALink, AFileName: string; AStatus: TLinkStatus; ASize: Int64; AChecksum: string = ''; AChecksumType: TChecksumType = ctMD5);
  public
    constructor Create; override;
    function CheckLink(AFile: WideString): TLinkInfo; virtual; safecall; abstract;
    function CheckLinks(AFiles: WideString): Integer; virtual; safecall;
    function CheckedLink(AIndex: Integer): TLinkInfo; safecall;
    destructor Destroy; override;
  end;

implementation

{ TFileHosterPlugIn }

procedure TFileHosterPlugIn.AddLink(ALink, AFileName: string; AStatus: TLinkStatus; ASize: Int64; AChecksum: string = ''; AChecksumType: TChecksumType = ctMD5);
var
  LinkInfo: TLinkInfo;
begin
  with LinkInfo do
  begin
    Link := ALink;
    Status := AStatus;
    Size := ASize;
    FileName := AFileName;
    Checksum := AChecksum;
    ChecksumType := AChecksumType;
  end;

  FCheckedLinksList.Add(LinkInfo);
end;

constructor TFileHosterPlugIn.Create;
begin
  inherited Create;
  FCheckedLinksList := TList<TLinkInfo>.Create;
end;

function TFileHosterPlugIn.CheckLinks(AFiles: WideString): Integer;
var
  _FileIndex: Integer;
begin
  FCheckedLinksList.Clear;
  with TStringList.Create do
    try
      Text := AFiles;

      for _FileIndex := 0 to Count - 1 do
        FCheckedLinksList.Add(CheckLink(Strings[_FileIndex]));

    finally
      Free;
    end;
  Result := FCheckedLinksList.Count;
end;

function TFileHosterPlugIn.CheckedLink(AIndex: Integer): TLinkInfo;
begin
  Result := FCheckedLinksList.Items[AIndex];
end;

destructor TFileHosterPlugIn.Destroy;
begin
  FCheckedLinksList.Free;
  inherited Destroy;
end;

end.
