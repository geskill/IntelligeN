unit uWcryptIn;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Variants, HTTPApp, Math,
  // RegEx
  RegExpr,
  // LkJSON
  uLkJSON,
  // Common
  uBaseConst, uBaseInterface,
  // plugin system
  uPlugInCrypterClass, uPlugInConst;

type
  TWcryptIn = class(TCrypterPlugIn)
  private
    function GetFolderID(AFolderURL: string): string;
  public
    function GetName: WideString; override;

    function AddFolder(const AMirrorContainer: IDirectlinkContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function EditFolder(const AMirrorContainer: IDirectlinkContainer; var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function DeleteFolder(AFolderIdentifier: WideString): WordBool; override; safecall;
    function GetFolder(AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
  end;

implementation

{ TWcryptIn }

function TWcryptIn.GetFolderID(AFolderURL: string): string;
begin
  with TRegExpr.Create do
    try
      InputString := AFolderURL;
      Expression := 'folder\/(.*?)\/';

      if Exec(InputString) then
        Result := Match[1];
    finally
      Free;
    end;
end;

function TWcryptIn.GetName;
begin
  Result := 'wCrypt.in';
end;

function TWcryptIn.AddFolder;
var
  _lkJSONobject: TlkJSONobject;
  _params, _result: TStringStream;
  _Foldertypes: TFoldertypes;
  _Containertypes: TContainertypes;
begin
  _Foldertypes := TFoldertypes(TFoldertype(Foldertypes));
  _Containertypes := TContainertypes(TContainertype(ContainerTypes));

  {

    with TIdHTTPHelper.Create(Self) do
    try
    _lkJSONobject := TlkJSONobject.Create;
    _params := TStringStream.Create('', CP_UTF8);
    _result := TStringStream.Create('', CP_UTF8);
    try
    if UseAccount then
    _lkJSONobject.Add('api', AccountName)
    else
    _lkJSONobject.Add('api', '');

    _lkJSONobject.Add('name', FolderName);

    _lkJSONobject.Add('links', MirrorController.DirectlinksMirror[0]);

    if ftWeb in _Foldertypes then
    _lkJSONobject.Add('web', '1')
    else
    _lkJSONobject.Add('web', '0');

    if ftContainer in _Foldertypes then
    begin
    if ctCCF in _Containertypes then
    _lkJSONobject.Add('ccf', '1')
    else
    _lkJSONobject.Add('ccf', '0');
    if ctDLC in _Containertypes then
    _lkJSONobject.Add('dlc', '1')
    else
    _lkJSONobject.Add('dlc', '0');
    if ctRSDF in _Containertypes then
    _lkJSONobject.Add('rsd', '1')
    else
    _lkJSONobject.Add('rsd', '0');
    end
    else
    begin
    _lkJSONobject.Add('ccf', '0');
    _lkJSONobject.Add('dlc', '0');
    _lkJSONobject.Add('rsd', '0');
    end;

    if UseCNL then
    _lkJSONobject.Add('cnl', '1')
    else
    _lkJSONobject.Add('cnl', '0');

    _params.WriteString('data=' + TlkJSON.GenerateText(_lkJSONobject));

    try
    Post('http://data.wcrypt.in/api_intelligen', _params, _result);
    except

    end;

    if (Pos('"key"', _result.DataString) > 0) then
    Result := 'http://wcrypt.in/folder/' + TlkJSON.ParseText(_result.DataString).Field['key'].Value + '/'
    else
    ErrorMsg := _result.DataString + #13#10#13#10 + '-----' + #13#10 + '1 = Keine Daten angegeben' + #13#10 +
    '2 = Ordnername und Link müssen angegeben werden' + #13#10 + '3 = User nicht gefunden' + #13#10 + '4 = Ungültige Links' + #13#10 +
    '5 = Keine Verschlüsselung gewählt' + #13#10 + '6 = Keine Links angegeben';
    finally
    _result.Free;
    _params.Free;
    _lkJSONobject.Free;
    end;
    finally
    Free;
    end;

    }
end;

function TWcryptIn.EditFolder;
begin
  //
end;

function TWcryptIn.DeleteFolder;
begin
  //
end;

function TWcryptIn.GetFolder;
var
  _lkJSONobject: TlkJSONobject;
  _result: TStringStream;
  CrypterFolderInfo: TCrypterFolderInfo;
begin
  with CrypterFolderInfo do
  begin
    Status := 255;
    Size := 0;
    Hoster := '';
    Parts := 0;
  end;

  {

    with TIdHTTPHelper.Create(Self) do
    try
    _result := TStringStream.Create('', CP_UTF8);
    try
    try
    Get('http://wcrypt.in/api_information?folder=' + GetFolderID(FolderURL), _result);
    if not(copy(_result.DataString, 1, 1) = '{') then
    Exit;
    except
    Exit;
    end;
    try
    _lkJSONobject := TlkJSON.ParseText(_result.DataString) as TlkJSONobject;
    case _lkJSONobject.Field['status'].Value of
    1:
    CrypterFolderInfo.Status := 1;
    2, 5, 6:
    CrypterFolderInfo.Status := 0;
    3:
    CrypterFolderInfo.Status := 2;
    4:
    CrypterFolderInfo.Status := 3;
    end;
    CrypterFolderInfo.Size := RoundTo((StrToFloatDef(_lkJSONobject.Field['size'].Value, 0) / 1048576), -2);
    CrypterFolderInfo.Hoster := copy(_lkJSONobject.Field['hoster'].Value, 3, Pos('":', _lkJSONobject.Field['hoster'].Value) - 3);
    CrypterFolderInfo.Parts := StrToIntDef(_lkJSONobject.Field['links_count'].Value, 0);
    finally
    _lkJSONobject.Free;
    end;
    finally
    _result.Free;
    end;
    finally
    Free;
    end;

    }

  // http://wcrypt.in/folder/24ca1fa265d958644757570a5913a72c/
  // http://static.wcrypt.in/folder/image/2/4/24ca1fa265d958644757570a5913a72c.png

 // Result := 'http://static.wcrypt.in/folder/image/2/4/' + GetFolderID(FolderURL) + '.png';

  Result := False;
end;

end.
