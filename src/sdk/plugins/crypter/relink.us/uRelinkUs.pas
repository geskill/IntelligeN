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
  uPathUtils, uStringUtils, uURLUtils;

type
  TRelinkUs = class(TCrypterPlugIn)
  private const
    VIEW_SNIPPET = 'view.php?id=';
    website = 'http://api.relink.us/';

    function GetFolderID(AFolderName: string): string;
    function GetStatusImageLink(AFolderIdentifier: WideString; Small: WordBool = True): WideString;
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

function TRelinkUs.GetStatusImageLink(AFolderIdentifier: WideString; Small: WordBool = True): WideString;
const
  OLD_VIEW_SNIPPET = 'view.php?id=';
var
  LViewSnippet: string;
begin
  LViewSnippet := 'f/';
  if Pos(VIEW_SNIPPET, string(AFolderIdentifier)) > 0 then
    LViewSnippet := OLD_VIEW_SNIPPET;

  case Small of
    True:
      Result := StringReplace(AFolderIdentifier, LViewSnippet, 'st/', []) + '.png';
    False:
      Result := StringReplace(AFolderIdentifier, LViewSnippet, 'std/', []) + '.png';
  end;
end;

function TRelinkUs.GetName;
begin
  Result := 'Relink.us';
end;

function TRelinkUs.AddFolder;
var
  LFoldertypes: TFoldertypes;
  LContainertypes: TContainertypes;

  LDirectlinkIndex: Integer;

  LHTTPParams: IHTTPParams;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
begin
  Result := False;

  with ACrypterFolderInfo do
  begin
    Link := '';
    Status := csNotChecked;
    Size := 0;
    PartSize := 0;
    Hoster := '';
    HosterShort := '';
    Parts := 0;
    StatusImage := '';
    StatusImageText := '';
  end;

  LFoldertypes := TFoldertypes(TFoldertype(Foldertypes));
  LContainertypes := TContainertypes(TContainertype(ContainerTypes));

  LHTTPParams := THTTPParams.Create(ptMultipartFormData);
  with LHTTPParams do
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

    AddFormField('url', TrimRight(StringReplace(AMirrorContainer.Directlink[0].Value, sLineBreak, ';', [rfReplaceAll]), ';'));

    for LDirectlinkIndex := 1 to AMirrorContainer.DirectlinkCount - 1 do
      AddFormField('url[]', TrimRight(StringReplace(AMirrorContainer.Directlink[0].Value, sLineBreak, ';', [rfReplaceAll]), ';'));

    if not(FolderName = '') then
      AddFormField('title', FolderName);

    if UseDescription then
      AddFormField('comment', Description);

    if UseVisitorPassword then
      AddFormField('password', Visitorpassword);

    AddFormField('web', IfThen(ftWeb in LFoldertypes, 'yes', 'no'));

    if ftContainer in LFoldertypes then
    begin
      AddFormField('dlc', IfThen(ctDLC in LContainertypes, 'yes', 'no'));
    end;

    AddFormField('cnl', IfThen(UseCNL, 'yes', 'no'));

    AddFormField('captcha', IfThen(UseCaptcha, 'yes', 'no'));

    if UseFilePassword then
    begin
      AddFormField('password_zip_public', 'yes');
      AddFormField('password_zip', FilePassword);
    end;
  end;

  LRequestID := HTTPManager.Post(THTTPRequest.Create(website + 'api.php'), LHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else if not(Pos('relink.us', string(LHTTPProcess.HTTPResult.SourceCode)) = 0) then
  begin
    ACrypterFolderInfo.Link := copy(LHTTPProcess.HTTPResult.SourceCode, 5);
    ACrypterFolderInfo.StatusImage := GetStatusImageLink(ACrypterFolderInfo.Link);
    ACrypterFolderInfo.StatusImageText := GetStatusImageLink(ACrypterFolderInfo.Link, False);
    Result := True;
  end
  else
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.SourceCode;
  end;

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
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;

  LCompleteList, LSingleList: TStrings;
  LSizeInBytes, LPartSizeInBytes: Int64;
  I, unknown, online, offline: Integer;
begin
  Result := False;

  with ACrypterFolderInfo do
  begin
    Status := csNotChecked;
    Size := 0;
    PartSize := 0;
    Hoster := '';
    HosterShort := '';
    Parts := 0;
    StatusImage := '';
    StatusImageText := '';
  end;

  LSizeInBytes := 0;
  LPartSizeInBytes := 0;
  unknown := 0;
  online := 0;
  offline := 0;

  LRequestID := HTTPManager.Get(THTTPRequest.Create(website + 'container_link_info.php?id=' + GetFolderID(AFolderIdentifier)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else if Pos(';', LHTTPProcess.HTTPResult.SourceCode) > 0 then
  begin
    LCompleteList := SplittString('|', LHTTPProcess.HTTPResult.SourceCode);
    try
      for I := 0 to LCompleteList.Count - 1 do
      begin
        if not(LCompleteList[I] = '') then
        begin
          Inc(ACrypterFolderInfo.Parts);
          LSingleList := SplittString(';', LCompleteList[I]);
          try
            try
              case IndexText(LSingleList[1], ['unknown', 'online', 'offline']) of
                0:
                  Inc(unknown);
                1:
                  Inc(online);
                2:
                  Inc(offline);
              end;
              if StrToIntDef(LSingleList[4], 0) > LPartSizeInBytes then
              begin
                LPartSizeInBytes := StrToIntDef(LSingleList[4], 0);
              end;
              LSizeInBytes := LSizeInBytes + StrToIntDef(LSingleList[4], 0);
              ACrypterFolderInfo.Hoster := LSingleList[2];
            except
              on E: Exception do
              begin
                ErrorMsg := 'The result from ' + GetName + ' was invaild: ' + E.message;
              end;
            end;
          finally
            LSingleList.Free;
          end;
        end;
      end;
    finally
      LCompleteList.Free;
    end;
    if (unknown = 0) and (online = 0) then
      ACrypterFolderInfo.Status := csOffline
    else if (unknown = 0) and (offline = 0) then
      ACrypterFolderInfo.Status := csOnline
    else if (offline > 0) and (online > 0) then
      ACrypterFolderInfo.Status := csMixedOnOffline
    else
      ACrypterFolderInfo.Status := csUnknown;
      
    ACrypterFolderInfo.Link := AFolderIdentifier;
    ACrypterFolderInfo.Size := RoundTo((LSizeInBytes / 1048576), -2);
    ACrypterFolderInfo.PartSize := RoundTo((LPartSizeInBytes / 1048576), -2);
    ACrypterFolderInfo.StatusImage := GetStatusImageLink(ACrypterFolderInfo.Link);
    ACrypterFolderInfo.StatusImageText := GetStatusImageLink(ACrypterFolderInfo.Link, False);

    Result := True;
  end
  else
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.SourceCode;
  end;

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
end;

end.
