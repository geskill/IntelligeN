unit uApiPluginsAdd;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Dialogs,
  // Dev Express
  cxCheckListBox,
  // Spring Framework
  Spring.Utils,
  // Utils
  uFileUtils, uPathUtils,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // DLLs
  uExport,
  // Api
  uApiConst, uApiMultiCastEvent, uApiMain, uApiSettings,
  // Plugin system
  uPlugInConst, uPlugInInterface, uPlugInInterfaceAdv;

type
  TOnPluginLoaded = procedure(aSender: TObject; aPlugIn: IPlugIn; aPlugInCollectionItem: TPlugInCollectionItem; aHandle: Integer) of object;

  TAddPlugin = class
  private
    FOnPluginLoaded: TOnPluginLoaded;
    function LoadPlugin(aPlugIn: TGUID; ACollection: TCollection; ACheckListBox: TcxCheckListBox; APlugInName, APlugInNameFile: string; AErrorMsg: Boolean = True): Boolean;
  public
    constructor Create;

    procedure CrawlerPluginLoaded(aSender: TObject; aPlugIn: IPlugIn; aPlugInCollectionItem: TPlugInCollectionItem; aHandle: Integer);

    function Execute(aPlugIn: TGUID; ACollection: TCollection; ACheckListBox: TcxCheckListBox; APlugInName: string; APlugInPath: string = ''): Boolean;
    function ExecuteFolder(aPlugIn: TGUID; ACollection: TCollection; ACheckListBox: TcxCheckListBox; APlugInName, AFolderPath: string): Boolean;
    property OnPluginLoaded: TOnPluginLoaded read FOnPluginLoaded write FOnPluginLoaded;
    destructor Destroy; override;
  end;

implementation

{ ****************************************************************************** }
{$REGION 'TAddPlugin'}

function TAddPlugin.LoadPlugin;
var
  LPluginMinorVersion: Integer;

  hLib: Cardinal;
  MLoadPlugIn: TLoadPlugIn;
  PlugIn: IPlugIn;
  PlugInCollectionItem: TPlugInCollectionItem;
  OverridePlugin: Boolean;
  CheckListBoxItem: TcxCheckListBoxItem;
begin
  LPluginMinorVersion := TFileVersionInfo.GetVersionInfo(APlugInNameFile).FileVersionNumber.Minor;

  OverridePlugin := False;

  if (MINOR_VERSION = LPluginMinorVersion) then
  begin
    hLib := LoadLibrary(PChar(APlugInNameFile));
    try
      if not(hLib = 0) then
      begin
        @MLoadPlugIn := GetProcAddress(hLib, 'LoadPlugIn');
        if not(@MLoadPlugIn = nil) then
        begin
          MLoadPlugIn(PlugIn);
          try
            if Supports(PlugIn, aPlugIn) then
            begin
              // plugin hinzufügen
              PlugInCollectionItem := SettingsManager.Settings.Plugins.FindPlugInCollectionItemFromCollection(PlugIn.GetName, ACollection);

              if not Assigned(PlugInCollectionItem) then
                PlugInCollectionItem := TPlugInCollectionItem(ACollection.Add)
              else if AErrorMsg and (MessageDlg(Format(StrOverrideSPlugin, [APlugInName]), mtConfirmation, [mbyes, mbno, mbcancel], 0) = ID_YES) then
              begin
                OverridePlugin := True;
                with SettingsManager.Settings.Plugins do
                  if Assigned(OnCMSChange) then
                    OnCMSChange.Invoke(pctEnabled, PlugInCollectionItem.Index, 0);
              end
              else
                Exit;

              with PlugInCollectionItem do
              begin
                name := PlugIn.GetName;
                Enabled := False;
                Path := ExtractRelativePath(GetPluginFolder, APlugInNameFile);

                if Assigned(FOnPluginLoaded) then
                  FOnPluginLoaded(self, PlugIn, PlugInCollectionItem, hLib);
              end;

              if Supports(PlugIn, ICMSPlugIn) then
                with SettingsManager.Settings.Plugins do
                  if Assigned(OnCMSChange) and not OverridePlugin then
                    OnCMSChange.Invoke(pctAdd, PlugInCollectionItem.Index, -1);

              if Assigned(ACheckListBox) then
              begin
                if OverridePlugin then
                  CheckListBoxItem := ACheckListBox.Items[ACheckListBox.Items.IndexOf(PlugInCollectionItem.name)]
                else
                  CheckListBoxItem := ACheckListBox.Items.Add;

                with CheckListBoxItem do
                begin
                  Checked := PlugInCollectionItem.Enabled;
                  Text := PlugInCollectionItem.name;
                  ImageIndex := ACheckListBox.Images.AddIcon(PlugInCollectionItem.Icon);

                  if not FileExists(PlugInCollectionItem.GetPath) then
                    Enabled := False;
                end;
              end;

              Result := True;
            end
            else if AErrorMsg then
              raise Exception.Create(StrThisPluginBelongs);
          finally
            PlugIn := nil;
          end;
        end
        else if AErrorMsg then
          raise Exception.Create(Format(StrUnknownSPlugin, [APlugInName]));
      end
      else if AErrorMsg then
        raise Exception.Create(Format(StrPluginDamaged, [SysErrorMessage(GetLastError())]));
    finally
      FreeLibrary(hLib);
    end;
  end
  else if AErrorMsg then
    raise Exception.Create(Format(StrThisPluginIsIncom, [MINOR_VERSION, LPluginMinorVersion]))
end;

constructor TAddPlugin.Create;
begin
  inherited Create;
end;

procedure TAddPlugin.CrawlerPluginLoaded;
var
  I, J: Integer;
  _TemplateTypeIDs: TTypeIDs;
  _ComponentIDs: TControlIDs;
begin
  with aPlugInCollectionItem as TCrawlerCollectionItem do
  begin
    Limit := ICrawlerPlugIn(aPlugIn).GetResultsLimitDefaultValue;
    Contingent.Clear;

    Word(_TemplateTypeIDs) := ICrawlerPlugIn(aPlugIn).GetAvailableTypeIDs;

    for I := Ord( low(TTypeID)) to Ord( high(TTypeID)) do
      if TTypeID(I) in _TemplateTypeIDs then
      begin
        Longword(_ComponentIDs) := ICrawlerPlugIn(aPlugIn).GetAvailableControlIDs(I);

        for J := Ord( low(TControlID)) to Ord( high(TControlID)) do
          if TControlID(J) in _ComponentIDs then
            with TCrawlerContingentCollectionItem(Contingent.Add) do
            begin
              TypeID := TTypeID(I);
              ControlID := TControlID(J);
              Status := ICrawlerPlugIn(aPlugIn).GetControlIDDefaultValue(I, J);
            end;
      end;
  end;
end;

function TAddPlugin.Execute;
var
  I: Integer;
begin
  Result := True;

  if SameStr('', APlugInPath) then
  begin
    with TOpenDialog.Create(nil) do
      try
        Options := Options + [ofAllowMultiSelect];
        Filter := APlugInName + ' plugin (*.dll)|*.dll';
        InitialDir := ExcludeTrailingPathDelimiter(GetPluginFolder);
        if Execute then
        begin
          for I := 0 to Files.Count - 1 do
            if not LoadPlugin(aPlugIn, ACollection, ACheckListBox, APlugInName, Files.Strings[I], True) then
              Result := False;
        end;
      finally
        Free;
      end;
  end
  else
    Result := LoadPlugin(aPlugIn, ACollection, ACheckListBox, APlugInName, APlugInPath, False);
end;

function TAddPlugin.ExecuteFolder;
var
  _StringList: TStringList;
  I: Integer;
begin
  Result := True;

  _StringList := TStringList.Create;
  try
    GetFilesInDirectory(AFolderPath, '*.dll', _StringList, True, True, True, True);

    for I := 0 to _StringList.Count - 1 do
      if not LoadPlugin(aPlugIn, ACollection, ACheckListBox, APlugInName, _StringList.Strings[I], False) then
        Result := False;
  finally
    _StringList.Free;
  end;
end;

destructor TAddPlugin.Destroy;
begin
  inherited Destroy;
end;
{$ENDREGION}

end.
