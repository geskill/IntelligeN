unit uRelinkUs;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Variants, HTTPApp, Math,
  // Indy
  IdMultiPartFormData,
  // Common
  uConst, uAppInterface,
  // Utils
  uPathUtils, uSpecialStringUtils,
  // plugin system
  uPlugInCrypterClass, uPlugInConst, uIdHTTPHelper;

type
  TRelinkUs = class(TCrypterPlugIn)
  private
    const
      VIEW_SNIPPET = 'view.php?id=';

    function GetFolderID(AFolderName: string): string;
  public
    function GetName: WideString; override; safecall;
    function GenerateFolder(MirrorController: IMirrorControl): WideString; override;
    function GetFolderInfo(FolderURL: WideString): TCrypterFolderInfo; override;
    procedure GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: Boolean = True); override; stdcall;
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

function TRelinkUs.GenerateFolder;
var
  Params: TIdMultiPartFormDataStream;
  Links: string;
  _Foldertypes: TFoldertypes;
  _Containertypes: TContainertypes;
  ReplyData: TStringStream;
begin
  Result := '';

  _Foldertypes := TFoldertypes(TFoldertype(Foldertypes));
  _Containertypes := TContainertypes(TContainertype(ContainerTypes));

  with TIdHTTPHelper.Create(Self) do
    try
      Params := TIdMultiPartFormDataStream.Create;
      ReplyData := TStringStream.Create('', CP_UTF8);
      try
        Params.AddFormField('protect', 'protect');

        if UseAccount then
          if not(AccountPassword = '') then
          begin
            Params.AddObject('user', '', '', TStringStream.Create(AccountName, TEncoding.UTF8));
            Params.AddObject('pw', '', '', TStringStream.Create(AccountPassword, TEncoding.UTF8));
          end
          else
            Params.AddObject('api', '', '', TStringStream.Create(AccountName, TEncoding.UTF8));

        Links := StringReplace(MirrorController.DirectlinksMirror[0], sLineBreak, ';', [rfReplaceAll]);

        if (length(Links) > 0) and (Links[length(Links)] = ';') then
          System.Delete(Links, length(Links), 1);

        Params.AddObject('url', '', '', TStringStream.Create(Links, TEncoding.UTF8));

        if ftWeb in _Foldertypes then
          Params.AddFormField('web', 'yes')
        else
          Params.AddFormField('web', 'no');

        if ftContainer in _Foldertypes then
        begin
          if ctDLC in _Containertypes then
            Params.AddFormField('dlc', 'yes')
          else
            Params.AddFormField('dlc', 'no')
        end;

        if UseCNL then
          Params.AddFormField('cnl', 'yes')
        else
          Params.AddFormField('cnl', 'no');

        if UseCaptcha then
          Params.AddFormField('captcha', 'yes')
        else
          Params.AddFormField('captcha', 'no');

        if not(FolderName = '') then
          Params.AddObject('title', '', '', TStringStream.Create(FolderName, TEncoding.UTF8));

        if UseDescription then
          Params.AddObject('comment', '', '', TStringStream.Create(Description, TEncoding.UTF8));

        if UseFilePassword then
        begin
          Params.AddObject('password_zip_public', '', '', TStringStream.Create('yes', TEncoding.UTF8));
          Params.AddObject('password_zip', '', '', TStringStream.Create(FilePassword, TEncoding.UTF8));
        end;

        if UseVisitorPassword then
          Params.AddObject('password', '', '', TStringStream.Create(Visitorpassword, TEncoding.UTF8));

        try
          Post('http://api.relink.us/api.php', Params, ReplyData);
        except

        end;

        if not(Pos('relink.us', ReplyData.DataString) = 0) then
          Result := copy(ReplyData.DataString, 5)
        else
          ErrorMsg := ReplyData.DataString;
      finally
        Params.Free;
        ReplyData.Free;
      end;
    finally
      Free;
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

function TRelinkUs.GetFolderInfo;
var
  CompleteList, SingleList: TStrings;
  CrypterFolderInfo: TCrypterFolderInfo;
  SizeInBytes: Int64;
  I, unknown, online, offline: Integer;
begin
  with CrypterFolderInfo do
  begin
    Status := 255;
    Size := 0;
    Hoster := '';
    Parts := 0;
  end;

  with TIdHTTPHelper.Create(Self) do
    try
      try
        CompleteList := SplittString('|', Get('http://api.relink.us/container_link_info.php?id=' + GetFolderID(FolderURL)));
      except
        Exit;
      end;

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
        CrypterFolderInfo.Status := 0
      else if (unknown = 0) and (offline = 0) then
        CrypterFolderInfo.Status := 1
      else if (offline > 0) and (online > 0) then
        CrypterFolderInfo.Status := 4
      else
        CrypterFolderInfo.Status := 2;

      CrypterFolderInfo.Size := RoundTo((SizeInBytes / 1048576), -2);
      Result := CrypterFolderInfo;
    finally
      Free;
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

procedure TRelinkUs.GetFolderPicture(FolderURL: WideString; out Result: WideString; Small: Boolean);
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
  (*
    case Small of
    True:
    Result := StringReplace(FolderURL, 'view.php?id=', 'forumstatus.php?id=', []);
    False:
    Result := StringReplace(FolderURL, 'view.php?id=', 'forumstatus.php?id=', []) + '&detail=1';
    end;
    *)
end;

end.
