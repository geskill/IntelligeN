unit uApiDirectoryMonitor;

interface

uses
  // Delphi
  Windows, ShlObj, SysUtils, Classes, Dialogs, Generics.Collections, TypInfo,
  // OmniThreadLibrary
  OtlParallel, OtlTaskControl, OtlSync, OtlTask,
  // DirectoryWatch
  DirectoryWatch,
  // Generic TThreadList
  hThreadList,
  // Api
  uApiMultiCastEvent, uApiSettings,
  // Utils
  uPathUtils;

type
  // TODO Re-work in BUILD >= 130
  TCMSInformation = class
  private
    FWebsiteXMLs, FSubjectTXTs, FMessageTXTs: TThreadList<string>;
    FName, FPath: string;
    FCMSCollectionItem: TCMSCollectionItem;
    FWebsiteChangeEventHandler, FSubjectChangeEventHandler, FMessageChangeEventHandler: ICMSItemChangeEventHandler;
    procedure WebsiteUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex: Integer; AParam: Integer);
    procedure SubjectUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex: Integer; AParam: Integer);
    procedure MessageUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex: Integer; AParam: Integer);
  protected
    function Find(AName: string; AThreadStringList: TThreadList<string>): Integer;
    function FindPath(APath: string; AThreadStringList: TThreadList<string>; AStartIndex: Integer = 0): Integer;

    function FindWebsite(AName: string): Integer;
    function FindWebsitePath(APath: string): Integer;

    function FindSubject(AName: string): Integer;
    function FindSubjectPath(APath: string; AStartIndex: Integer = 0): Integer;

    function FindMessage(AName: string): Integer;
    function FindMessagePath(APath: string; AStartIndex: Integer = 0): Integer;
  public
    constructor Create(const AName: string; const APath: string; ACMSCollectionItem: TCMSCollectionItem);
    property Name: string read FName;
    property Path: string read FPath;
    property CMSCollectionItem: TCMSCollectionItem read FCMSCollectionItem;
    destructor Destroy; override;
  end;

  TCMSFileType = (cftUndefined, cftFolder, cftPluginDLL, cftWebsiteXML, cftSubjectTXT, cftMessageTXT);

  RFileInformation = record
    InFileSystem: Boolean;
    FileType: TCMSFileType;
    FileName: string;
    PluginName, PluginSubName: string; // for DLL, XML
    PluginNames: array of array of string; // for TXT
  end;

  TMonitorManager = class
  private
    FDirectoryWatchList: TList<TDirectoryWatch>;
    FCMSList: TThreadList<TCMSInformation>;
    FOldFileInformation: RFileInformation;
    FOldFileInformationLock: TOmniMREW;
    FPluginChangeEventHandler :IPluginChangeEventHandler;
    procedure FCMSListAdd(CMSCollectionItem: TCMSCollectionItem);
    function FCMSListFind(AName: string): TCMSInformation;
    function FCMSListFindPath(APath: string): TCMSInformation;
    function FileofInterest(const ABaseDir: string; const AFileName: string): RFileInformation;
    procedure HandleFile(AFileInformation: RFileInformation; const Action: TWatchAction);
    procedure HandleFileRename(AFileInformation: RFileInformation; const ANewFileName: string);
  protected
    procedure Notify(const Sender: TObject; const Action: TWatchAction; const FileName: string);
    procedure CMSUpdate(ACMSChangeType: TPluginChangeType; AIndex: Integer; AParam: Integer);
    function FindDirectory(const ADirectory: string): Integer;
    function AddDirectory(const ADirectory: string; AWatchSubTree: Boolean): Boolean; overload;
  public
    constructor Create;
    function AddDirectory(const ADirectory: string): Boolean; overload;
    function RemoveDirectory(const ADirectory: string): Boolean;
    destructor Destroy; override;
  end;

implementation

uses
  uMain, uSettings;

{ TCMSInformation }

procedure TCMSInformation.WebsiteUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex, AParam: Integer);
var
  Index: Integer;
  CMSWebsitesCollectionItem: TCMSWebsitesCollectionItem;
begin
  CMSWebsitesCollectionItem := TCMSWebsitesCollectionItem(FCMSCollectionItem.Websites.Items[AIndex]);

  case ACMSItemChangeType of
    cctAdd:
      begin
        Index := FindWebsite(CMSWebsitesCollectionItem.name);
        if (Index = -1) then
          FWebsiteXMLs.Add(CMSWebsitesCollectionItem.name + FWebsiteXMLs.NameValueSeparator + CMSWebsitesCollectionItem.GetPath);
      end;
    cctDelete:
      begin
        Index := FindWebsite(CMSWebsitesCollectionItem.name);
        if not(Index = -1) then
          FWebsiteXMLs.Delete(Index);
      end;
  end;
end;

procedure TCMSInformation.SubjectUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex, AParam: Integer);
var
  Index: Integer;
  CMSWebsitesCollectionItem: TCMSWebsitesCollectionItem;
begin
  CMSWebsitesCollectionItem := TCMSWebsitesCollectionItem(FCMSCollectionItem.Websites.Items[AIndex]);

  case ACMSItemChangeType of
    cctAdd:
      begin
        Index := FindSubject(CMSWebsitesCollectionItem.name);
        if (Index = -1) then
          FSubjectTXTs.Add(CMSWebsitesCollectionItem.name + FSubjectTXTs.NameValueSeparator + CMSWebsitesCollectionItem.GetSubjectFileName);
      end;
    cctDelete:
      begin
        Index := FindSubject(CMSWebsitesCollectionItem.name);
        if not(Index = -1) then
          FSubjectTXTs.Delete(Index);
      end;
  end;
end;

procedure TCMSInformation.MessageUpdate(ACMSItemChangeType: TCMSItemChangeType; AIndex, AParam: Integer);
var
  Index: Integer;
  CMSWebsitesCollectionItem: TCMSWebsitesCollectionItem;
begin
  CMSWebsitesCollectionItem := TCMSWebsitesCollectionItem(FCMSCollectionItem.Websites.Items[AIndex]);

  case ACMSItemChangeType of
    cctAdd:
      begin
        Index := FindMessage(CMSWebsitesCollectionItem.name);
        if (Index = -1) then
          FMessageTXTs.Add(CMSWebsitesCollectionItem.name + FMessageTXTs.NameValueSeparator + CMSWebsitesCollectionItem.GetMessageFileName);
      end;
    cctDelete:
      begin
        Index := FindMessage(CMSWebsitesCollectionItem.name);
        if not(Index = -1) then
          FMessageTXTs.Delete(Index);
      end;
  end;
end;

function TCMSInformation.Find(AName: string; AThreadStringList: TThreadStringList): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to AThreadStringList.Count - 1 do
    if SameText(AName, AThreadStringList.Names[I]) then
      Exit(I);
end;

function TCMSInformation.FindPath(APath: string; AThreadStringList: TThreadStringList; AStartIndex: Integer = 0): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := AStartIndex to AThreadStringList.Count - 1 do
    if SameText(APath, AThreadStringList.ValueFromIndex[I]) then
      Exit(I);
end;

function TCMSInformation.FindWebsite(AName: string): Integer;
begin
  Result := Find(AName, FWebsiteXMLs);
end;

function TCMSInformation.FindWebsitePath(APath: string): Integer;
begin
  Result := FindPath(APath, FWebsiteXMLs);
end;

function TCMSInformation.FindSubject(AName: string): Integer;
begin
  Result := Find(AName, FSubjectTXTs);
end;

function TCMSInformation.FindSubjectPath(APath: string; AStartIndex: Integer = 0): Integer;
begin
  Result := FindPath(APath, FSubjectTXTs, AStartIndex);
end;

function TCMSInformation.FindMessage(AName: string): Integer;
begin
  Result := Find(AName, FMessageTXTs);
end;

function TCMSInformation.FindMessagePath(APath: string; AStartIndex: Integer = 0): Integer;
begin
  Result := FindPath(APath, FMessageTXTs, AStartIndex);
end;

constructor TCMSInformation.Create(const AName, APath: string; ACMSCollectionItem: TCMSCollectionItem);
var
  I: Integer;
begin
  FWebsiteXMLs := TThreadStringList.Create;
  FSubjectTXTs := TThreadStringList.Create;
  FMessageTXTs := TThreadStringList.Create;

  FName := AName;
  FPath := APath;
  FCMSCollectionItem := ACMSCollectionItem;

  with FCMSCollectionItem do
  begin
    for I := 0 to Websites.Count - 1 do
      with TCMSWebsitesCollectionItem(Websites.Items[I]) do
      begin
        FWebsiteXMLs.Add(name + FWebsiteXMLs.NameValueSeparator + GetPath);
        FSubjectTXTs.Add(name + FSubjectTXTs.NameValueSeparator + GetSubjectFileName);
        FMessageTXTs.Add(name + FMessageTXTs.NameValueSeparator + GetMessageFileName);
      end;
    FWebsiteChangeEventHandler := TICMSItemChangeEventHandler.Create(WebsiteUpdate);
    FSubjectChangeEventHandler := TICMSItemChangeEventHandler.Create(SubjectUpdate);
    FMessageChangeEventHandler := TICMSItemChangeEventHandler.Create(MessageUpdate);
    OnWebsitesChange.Add(FWebsiteChangeEventHandler);
    OnSubjectsChange.Add(FSubjectChangeEventHandler);
    OnMessagesChange.Add(FMessageChangeEventHandler);
  end;
end;

destructor TCMSInformation.Destroy;
begin
  with FCMSCollectionItem do
  begin
    OnMessagesChange.Remove(FMessageChangeEventHandler);
    OnSubjectsChange.Remove(FSubjectChangeEventHandler);
    OnWebsitesChange.Remove(FWebsiteChangeEventHandler);
  end;

  FMessageTXTs.Free;
  FSubjectTXTs.Free;
  FWebsiteXMLs.Free;

  inherited Destroy;
end;

{ TMonitorManager }

(*
  TMonitorManager registriert sich bei CMS Plugins (multi event) und schaut dort, wenn ein CMS hinzugefügt wird, kommt es mit in die Liste.
  Bei Änderung dieser Datei wird in den Settings die ausgegraut, SettingsManager Datei benachrichtigt.
  -> Dort überall auf Enabeld und zusätzlich FileExists(...) prüfen. Durch die Benachtichtigung wird Datei Ausgrauung gemeldet.

  TMonitorManager hat Liste:TStringList von CMS Plugins mit dessen Pfaden, auf welche im Notify geprüft wird.
  *)

procedure TMonitorManager.FCMSListAdd(CMSCollectionItem: TCMSCollectionItem);
begin
  FCMSList.Add(TCMSInformation.Create(CMSCollectionItem.name, CMSCollectionItem.GetPath, CMSCollectionItem));
end;

function TMonitorManager.FCMSListFind(AName: string): TCMSInformation;
var
  CMSInformation: TCMSInformation;
begin
  Result := nil;

  for CMSInformation in FCMSList do
    if SameText(AName, CMSInformation.Name) then
      Exit(CMSInformation);
end;

function TMonitorManager.FCMSListFindPath(APath: string): TCMSInformation;
var
  CMSInformation: TCMSInformation;
begin
  Result := nil;

  for CMSInformation in FCMSList do
    if SameText(APath, CMSInformation.Path) then
      Exit(CMSInformation);
end;

function TMonitorManager.FileofInterest(const ABaseDir: string; const AFileName: string): RFileInformation;
var
  FileInformation: RFileInformation;

  CMSInformation: TCMSInformation;

  Index: Integer;
begin
  with FileInformation do
  begin
    InFileSystem := True;
    FileName := ABaseDir + AFileName;
    SetLength(FileInformation.PluginNames, 0);
  end;

  if IsDirectory(ABaseDir + AFileName) then
  begin
    FileInformation.FileType := cftFolder;
    Exit(FileInformation);
  end;

  CMSInformation := FCMSListFindPath(ABaseDir + AFileName);
  if Assigned(CMSInformation) then
    with FileInformation do
    begin
      PluginName := CMSInformation.Name;
      FileType := cftPluginDLL;
      Exit(FileInformation);
    end;

  for CMSInformation in FCMSList do
  begin
    Index := CMSInformation.FindWebsitePath(ABaseDir + AFileName);
    if not(Index = -1) then
      with FileInformation do
      begin
        PluginName := CMSInformation.Name;
        PluginSubName := CMSInformation.FWebsiteXMLs.Names[Index];
        FileType := cftWebsiteXML;
        Exit(FileInformation);
      end;
  end;

  for CMSInformation in FCMSList do
  begin
    if (length(FileInformation.PluginNames) > 0) then
      if (length(FileInformation.PluginNames[length(FileInformation.PluginNames) - 1]) = 1) then
        SetLength(FileInformation.PluginNames, length(FileInformation.PluginNames));

    SetLength(FileInformation.PluginNames, length(FileInformation.PluginNames) + 1);
    SetLength(FileInformation.PluginNames[length(FileInformation.PluginNames) - 1], 1);

    FileInformation.PluginNames[length(FileInformation.PluginNames) - 1][0] := CMSInformation.Name;

    Index := 0;
    repeat
      Index := CMSInformation.FindSubjectPath(ABaseDir + AFileName, Index);

      if not(Index = -1) then
        with FileInformation do
        begin
          FileType := cftSubjectTXT;

          SetLength(FileInformation.PluginNames[length(FileInformation.PluginNames) - 1],
            length(FileInformation.PluginNames[length(FileInformation.PluginNames) - 1]) + 1);

          FileInformation.PluginNames[length(FileInformation.PluginNames) - 1][length(FileInformation.PluginNames[length(FileInformation.PluginNames) - 1])
            - 1] := CMSInformation.FSubjectTXTs.Names[Index];
        end;

    until (Index = -1);
  end;

  if not(length(FileInformation.PluginNames) = 0) then
    Exit(FileInformation);

  for CMSInformation in FCMSList do
  begin
    if (length(FileInformation.PluginNames) > 0) then
      if (length(FileInformation.PluginNames[length(FileInformation.PluginNames) - 1]) = 1) then
        SetLength(FileInformation.PluginNames, length(FileInformation.PluginNames));

    SetLength(FileInformation.PluginNames, length(FileInformation.PluginNames) + 1);
    SetLength(FileInformation.PluginNames[length(FileInformation.PluginNames) - 1], 1);

    FileInformation.PluginNames[length(FileInformation.PluginNames) - 1][0] := CMSInformation.Name;

    Index := 0;
    repeat
      Index := CMSInformation.FindMessagePath(ABaseDir + AFileName, Index);

      if not(Index = -1) then
        with FileInformation do
        begin
          FileType := cftMessageTXT;

          SetLength(FileInformation.PluginNames[length(FileInformation.PluginNames) - 1],
            length(FileInformation.PluginNames[length(FileInformation.PluginNames) - 1]) + 1);

          FileInformation.PluginNames[length(FileInformation.PluginNames) - 1][length(FileInformation.PluginNames[length(FileInformation.PluginNames) - 1])
            - 1] := CMSInformation.FMessageTXTs.Names[Index];
        end;

    until (Index = -1);
  end;

  if (length(FileInformation.PluginNames) = 0) then
    FileInformation.InFileSystem := False;
  Result := FileInformation;
end;

procedure TMonitorManager.HandleFile(AFileInformation: RFileInformation; const Action: TWatchAction);
var
  CMSInformation: TCMSInformation;

  CMSWebsitesCollectionItem: TCMSWebsitesCollectionItem;

  I, J: Integer;
begin
  case AFileInformation.FileType of
    cftPluginDLL, cftWebsiteXML:
      begin
        CMSInformation := FCMSListFind(AFileInformation.PluginName);

        case AFileInformation.FileType of
          cftPluginDLL:
            begin

            end;
          cftWebsiteXML:
            begin
              CMSWebsitesCollectionItem := CMSInformation.CMSCollectionItem.FindCMSWebsite(AFileInformation.PluginSubName);

              case Action of
                waRemoved:
                  MessageDlg('It works as expected: ' + AFileInformation.PluginSubName, mtWarning, [mbOK], 0);
                waModified:
                  begin
                    CMSWebsitesCollectionItem.UpdateWebsiteInformation;
                    with Main.fMain do
                      if TabSheetCount > 0 then
                        ActiveTabSheetController.PublishController.OnUpdateCMSList.Invoke(ActiveTabSheetController.PublishController);
                  end;
              end;
            end;
        end;
      end;
    cftSubjectTXT, cftMessageTXT:
      begin
        for I := 0 to length(AFileInformation.PluginNames) - 1 do
        begin
          CMSInformation := FCMSListFind(AFileInformation.PluginNames[I][0]);

          for J := 1 to length(AFileInformation.PluginNames[I]) - 1 do
          begin
            CMSWebsitesCollectionItem := CMSInformation.CMSCollectionItem.FindCMSWebsite(AFileInformation.PluginNames[I][J]);

            case AFileInformation.FileType of
              cftSubjectTXT:
                begin

                  case Action of
                    waAdded:
                      ;
                    waRemoved:
                      ;
                    waModified:
                      CMSInformation.FCMSCollectionItem.OnSubjectsChange.Invoke(cctChange, CMSWebsitesCollectionItem.Index, -1);
                  end;

                end;
              cftMessageTXT:
                begin

                  case Action of
                    waAdded:
                      ;
                    waRemoved:
                      ;
                    waModified:
                      CMSInformation.FCMSCollectionItem.OnMessagesChange.Invoke(cctChange, CMSWebsitesCollectionItem.Index, -1);
                  end;

                end;
            end;

          end;
        end;

      end;
  end;

  case Action of
    waAdded:
      { Schauen ob eine DLL aus den deaktivierten hinzugefügt wird } ;
    waRemoved:
      { Alle CMS Plugins, Webseiten XML, Subject, Message } ;
    waModified:
      { Webseiten XML, Subject, Message } ;
  end;
end;

procedure TMonitorManager.HandleFileRename(AFileInformation: RFileInformation; const ANewFileName: string);
begin
  //
end;

procedure TMonitorManager.Notify(const Sender: TObject; const Action: TWatchAction; const FileName: string);
var
  BaseDir: string;
  FileInformation: RFileInformation;
  OldFileofInterest: Boolean;
begin
  BaseDir := TDirectoryWatch(Sender).Directory;

  CreateTask(
    { } procedure(const task: IOmniTask)
    { } begin
    { . } if (Action in [waAdded, waRemoved, waModified, waRenamedOld]) then
    { . } begin
    { ... } FileInformation := FileofInterest(BaseDir, FileName);
    { ... } if (Action = waRenamedOld) then
    { ... } begin
    { ..... } FOldFileInformationLock.EnterWriteLock;
    { ..... } try
    { ....... } FOldFileInformation := FileInformation;
    { ..... } finally
    { ....... } FOldFileInformationLock.ExitWriteLock;
    { ..... } end;
    { ... } end;
    { . } end;
    { . } if (Action in [waAdded, waRemoved, waModified]) and FileInformation.InFileSystem then
    { . } begin
    { ... } task.Invoke(
      { ..... } procedure
      { ..... } begin
      { ....... } HandleFile(FileInformation, Action);
      { ..... } end);
    { ... }
    { . } end
    { . } else if (Action = waRenamedNew) then
    { . } begin
    { ... } OldFileofInterest := False;
    { ... } FOldFileInformationLock.EnterReadLock;
    { ... } try
    { ..... } OldFileofInterest := FOldFileInformation.InFileSystem;
    { ... } finally
    { ..... } FOldFileInformationLock.ExitReadLock;
    { ... } end;
    { ... } if OldFileofInterest then
    { ..... } task.Invoke(
      { ..... } procedure
      { ..... } begin
      { ....... } HandleFileRename(FOldFileInformation, FileName);
      { ..... } end);
    { . } end;
    { } end, 'TMonitorManager ' + BaseDir + FileName + ' (' + GetEnumName(TypeInfo(TWatchAction), Integer(Action)) + ')').Run;
end;

procedure TMonitorManager.CMSUpdate(ACMSChangeType: TPluginChangeType; AIndex, AParam: Integer);
var
  CMSInformation: TCMSInformation;
  CMSCollectionItem: TCMSCollectionItem;
begin
  CMSCollectionItem := TCMSCollectionItem(SettingsManager.Settings.Plugins.CMS.Items[AIndex]);

  case ACMSChangeType of
    pctAdd:
      begin
        CMSInformation := FCMSListFind(CMSCollectionItem.name);
        if not Assigned(CMSInformation) then
          FCMSListAdd(CMSCollectionItem);
      end;
    pctDelete:
      begin
        CMSInformation := FCMSListFind(CMSCollectionItem.name);
        if Assigned(CMSInformation) then
          FCMSList.Remove(CMSInformation);
      end;
  end;
end;

function TMonitorManager.FindDirectory(const ADirectory: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FDirectoryWatchList.Count - 1 do
    if SameStr(ADirectory, FDirectoryWatchList[I].Directory) then
      Exit(I);
end;

function TMonitorManager.AddDirectory(const ADirectory: string; AWatchSubTree: Boolean): Boolean;
var
  DirectoryWatch: TDirectoryWatch;
begin
  Result := False;

  if FindDirectory(ADirectory) = -1 then
  begin
    DirectoryWatch := TDirectoryWatch.Create;
    DirectoryWatch.WatchSubTree := AWatchSubTree;
    DirectoryWatch.WatchOptions := [woFileName, woDirName, woLastWrite];
    DirectoryWatch.Directory := ADirectory;
    DirectoryWatch.OnNotify := Notify;
    DirectoryWatch.Start;

    FDirectoryWatchList.Add(DirectoryWatch);

    Result := True;
  end;
end;

constructor TMonitorManager.Create;
var
  I: Integer;
begin
  inherited Create;
  FDirectoryWatchList := TList<TDirectoryWatch>.Create();
  FCMSList := TThreadList<TCMSInformation>.Create(True);

  AddDirectory(ExtractFilePath(ParamStr(0)), True);

  with SettingsManager.Settings.Plugins do
  begin
    for I := 0 to CMS.Count - 1 do
      FCMSListAdd(TCMSCollectionItem(CMS.Items[I]));

    FPluginChangeEventHandler := TIPluginChangeEventHandler.Create(CMSUpdate);
    OnCMSChange.Add(FPluginChangeEventHandler);
  end;
end;

function TMonitorManager.AddDirectory(const ADirectory: string): Boolean;
begin
  Result := AddDirectory(ADirectory, False);
end;

function TMonitorManager.RemoveDirectory(const ADirectory: string): Boolean;
var
  Index: Integer;
begin
  Result := False;
  Index := FindDirectory(ADirectory);

  if not(Index = -1) then
  begin
    with FDirectoryWatchList[Index] do
    begin
      Stop;
      Free;
    end;
    FDirectoryWatchList.Delete(Index);
    Result := True;
  end;
end;

destructor TMonitorManager.Destroy;
var
  DirectoryWatch: TDirectoryWatch;
begin
  SettingsManager.Settings.Plugins.OnCMSChange.Remove(FPluginChangeEventHandler);

  for DirectoryWatch in FDirectoryWatchList do
  begin
    DirectoryWatch.Stop;
    DirectoryWatch.Free;
  end;

  FCMSList.Free;

  FDirectoryWatchList.Free;

  inherited Destroy;
end;

end.
