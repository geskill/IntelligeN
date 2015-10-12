unit uRelinkUs;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Variants, HTTPApp, Math,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPConst,
  // plugin system
  uPlugInCrypterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uPathUtils, uStringUtils;

type
  TRelinkUs = class(TCrypterPlugIn)
  private
    const
      VIEW_SNIPPET = 'view.php?id=';
      website = 'http://api.relink.us/';

    function GetFolderID(AFolderName: string): string;
  public
    function GetName: WideString; override;
    function AddFolder(const AMirrorContainer: IDirectlinkContainer; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function EditFolder(const AMirrorContainer: IDirectlinkContainer; var ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
    function DeleteFolder(AFolderIdentifier: WideString): WordBool; override; safecall;
    function GetFolder(AFolderIdentifier: WideString; out ACrypterFolderInfo: TCrypterFolderInfo): WordBool; override; safecall;
  end;

implementation

function TRelinkUs.GetFolderID(AFolderName: string): string;
begin
  if not(Pos('=', AFolderName) = 0) then
    Result := copy(AFolderName, Pos('=', AFolderName) + 1)
  else
    Result := ExtractUrlFileName(AFolderName);
end;

function TRelinkUs.GetName;
begin
  Result := 'Relink.us';
end;

function TRelinkUs.AddFolder;
var
  _Foldertypes: TFoldertypes;
  _Containertypes: TContainertypes;

  HTTPParams: IHTTPParams;

  Links: string;

  RequestID: Double;

  HTTPProcess: IHTTPProcess;
begin
  _Foldertypes := TFoldertypes(TFoldertype(Foldertypes));
  _Containertypes := TContainertypes(TContainertype(ContainerTypes));

  HTTPParams := THTTPParams.Create(ptMultipartFormData);
  with HTTPParams do
  begin
    AddFormField('protect', 'protect');

    if UseAccount then
      if not(AccountPassword = '') then
      begin
        AddFormField('user', AccountName);
        AddFormField('pw', AccountPassword);
      end
      else
        AddFormField('api', AccountName);

    Links := StringReplace(AMirrorContainer.Directlink[0].Value, sLineBreak, ';', [rfReplaceAll]);

    if (length(Links) > 0) and (Links[length(Links)] = ';') then
      System.Delete(Links, length(Links), 1);

    AddFormField('url', Links);

    if ftWeb in _Foldertypes then
      AddFormField('web', 'yes')
    else
      AddFormField('web', 'no');

    if ftContainer in _Foldertypes then
    begin
      if ctDLC in _Containertypes then
        AddFormField('dlc', 'yes')
      else
        AddFormField('dlc', 'no')
    end;

    if UseCNL then
      AddFormField('cnl', 'yes')
    else
      AddFormField('cnl', 'no');

    if UseCaptcha then
      AddFormField('captcha', 'yes')
    else
      AddFormField('captcha', 'no');

    if not(FolderName = '') then
      AddFormField('title', FolderName);

    if UseDescription then
      AddFormField('comment', Description);

    if UseFilePassword then
    begin
      AddFormField('password_zip_public', 'yes');
      AddFormField('password_zip', FilePassword);
    end;

    if UseVisitorPassword then
      AddFormField('password', Visitorpassword);
  end;

  RequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api.php'), HTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  HTTPProcess := HTTPManager.GetResult(RequestID);

  if HTTPProcess.HTTPResult.HasError then
    ErrorMsg := HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage
  else if not(Pos('relink.us', string(HTTPProcess.HTTPResult.SourceCode)) = 0) then
    ErrorMsg := copy(HTTPProcess.HTTPResult.SourceCode, 5)
  else
    ErrorMsg := HTTPProcess.HTTPResult.SourceCode;

  {
    Wie benutze ich die Crypt-API?
    Die API wird über die URL http://api.relink.us/api.php aufgerufen. Die benötigten Argumente werden dabei über POST-Werte übergeben.

    Welche Argumente erfordert die Crypt-API?Wert	Beschreibung
    api	Dein API-Schüssel
    url	Die zu verschlüsselten URLs (per Semikolon getrennt, letzter Link OHNE Semikolon!) (erforderlich)
    title	Titel des Ordners (Falls kein Titel angegeben ist, werden die globalen Einstellungen benutzt. Um Titel leer zu lassen "empty" senden.) (optional)
    comment	Kommentar (Falls kein Kommentar angegeben ist, werden die globalen Einstellungen benutzt. Um Kommentar leer zu lassen "empty" senden.) (optional)
    password	Ordner-Passwort (Falls kein Ordner-Passwort angegeben ist, werden die globalen Einstellungen benutzt. Um Ordner-Passwort leer zu lassen "empty" senden.) (optional)
    web	Web-Container erstellen (yes=Ja, no=Nein) (Falls die Variable nicht angegeben ist, werden die globalen Einstellungen benutzt.) (optional)
    dlc	DLC-Container erstellen (yes=Ja, no=Nein) (Falls die Variable nicht angegeben ist, werden die globalen Einstellungen benutzt.) (optional)
    cnl	CnL-Funktion erstellen (yes=Ja, no=Nein) (Falls die Variable nicht angegeben ist, werden die globalen Einstellungen benutzt.) (optional)


    Alle Werte müssen über die POST-Methode übermittelt werden.

    Was erhalte ich als Rückgabe?
    Durch die Rückgabe lässt sich feststellen, ob die Erstellung des Ordners erfolgreich war. Folgende Rückgabe-Werte sind möglich:
    1 - [Der Link zum neuen Relink-Ordner]
    2 - API Key is invalid.
    3 - Not all Links are valid.
    4 - No Encryption selected.
    5 - Only X querie(s) / X second(s) allowed
    }
end;

function TRelinkUs.EditFolder;
begin
  //
end;

function TRelinkUs.DeleteFolder;
begin
  //
end;

function TRelinkUs.GetFolder;
var
  CrypterFolderInfo: TCrypterFolderInfo;

  RequestID: Double;

  HTTPProcess: IHTTPProcess;

  CompleteList, SingleList: TStrings;
  SizeInBytes: Int64;
  I, unknown, online, offline: Integer;
begin
  with CrypterFolderInfo do
  begin
    Status := csNotChecked;
    Size := 0;
    Hoster := '';
    Parts := 0;
  end;

  RequestID := HTTPManager.Get(THTTPRequest.Create(website + 'container_link_info.php?id=' + GetFolderID(AFolderIdentifier)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  HTTPProcess := HTTPManager.GetResult(RequestID);

  CompleteList := SplittString('|', HTTPProcess.HTTPResult.SourceCode);
  try
    SizeInBytes := 0;
    unknown := 0;
    online := 0;
    offline := 0;
    CrypterFolderInfo.Parts := 0;
    for I := 0 to CompleteList.Count - 1 do
    begin
      if not(CompleteList[I] = '') then
      begin
        Inc(CrypterFolderInfo.Parts);
        SingleList := SplittString(';', CompleteList[I]);
        try
          try
            case IndexText(SingleList[1], ['unknown', 'online', 'offline']) of
              0:
                Inc(unknown);
              1:
                Inc(online);
              2:
                Inc(offline);
            end;
            SizeInBytes := SizeInBytes + StrToIntDef(SingleList[4], 0);
            CrypterFolderInfo.Hoster := SingleList[2];
          except

          end;
        finally
          SingleList.Free;
        end;
      end;
    end;
  finally
    CompleteList.Free;
  end;
  if (unknown = 0) and (online = 0) then
    CrypterFolderInfo.Status := csOffline
  else if (unknown = 0) and (offline = 0) then
    CrypterFolderInfo.Status := csOnline
  else if (offline > 0) and (online > 0) then
    CrypterFolderInfo.Status := csMixedOnOffline
  else
    CrypterFolderInfo.Status := csUnknown;

  CrypterFolderInfo.Size := RoundTo((SizeInBytes / 1048576), -2);

  Result := False; // TODO

  {
    Wie benutze ich die Container Link-API?
    Die Container Link-API wird über die URL http://api.relink.us/container_link_info.php aufgerufen.
    Das benötigte Argument wird dabei mit GET übergeben.

    Welches Argument erfordert die Container Link-API?Wert	Beschreibung
    id	Container ID (erforderlich)


    Was erhalte ich als Rückgabe?
    Als Rückgabe bekommt man folgende Informationen getrennt mit ";", einzelne Links werden mit "|" getrennt:
    fortlaufende Nummer (1, 2, 3, ...);status (unknown, online, offline);hoster;filename;size in bytes|
    }


  (*

var
  l_view_snippet: string;
begin
  l_view_snippet := 'f/';
  if Pos(VIEW_SNIPPET, string(FolderURL)) > 0 then
    l_view_snippet := VIEW_SNIPPET;

  case Small of
    True:
      Result := StringReplace(FolderURL, l_view_snippet, 'st/', []) + '.png';
    False:
      Result := StringReplace(FolderURL, l_view_snippet, 'std/', []) + '.png';
  end;

    case Small of
    True:
    Result := StringReplace(FolderURL, 'view.php?id=', 'forumstatus.php?id=', []);
    False:
    Result := StringReplace(FolderURL, 'view.php?id=', 'forumstatus.php?id=', []) + '&detail=1';
    end;
    *)
end;

end.
