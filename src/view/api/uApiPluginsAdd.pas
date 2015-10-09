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
  uFileUtils,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // DLLs
  uExport,
  // Api
  uApiConst, uApiPluginsBase, uApiPlugins, uApiMultiCastEvent, uApiSettings,
  // Plugin system
  uPlugInConst, uPlugInInterface;

type
  TPluginOnLoadedFunc = reference to function(const APluginType: TPlugInType; var ACollection: TCollection; var ACheckListBox: TcxCheckListBox): Boolean;

  TAddPlugin = class(TApiPlugin)
  private
    class function AddPlugin(const APluginFile: string; const APluginHandle: Cardinal; const APlugin: IPlugIn; const ACollection: TCollection; const ACheckListBox: TcxCheckListBox; AHandleOverride: Boolean = True): Boolean;
  protected
    class function ExecuteBase(const APluginFile: string; APluginOnLoadedFunc: TPluginOnLoadedFunc; AHandleOverride: Boolean = True; const ARestrictToType: TPlugInType = ptNone; AErrorProc: TPluginErrorProc = nil): Boolean;
  public
    class function Execute(APluginOnLoadedFunc: TPluginOnLoadedFunc; const ARestrictToType: TPlugInType; APluginFile: string = ''; AHandleOverride: Boolean = True; AErrorProc: TPluginErrorProc = nil): Boolean;
    class function ExecuteFolder(APluginOnLoadedFunc: TPluginOnLoadedFunc; AFolderPath: string; const ARestrictToType: TPlugInType = ptNone): Boolean;
  end;

implementation

class function TAddPlugin.AddPlugin(const APluginFile: string; const APluginHandle: Cardinal; const APlugin: IPlugIn; const ACollection: TCollection; const ACheckListBox: TcxCheckListBox; AHandleOverride: Boolean = True): Boolean;
var
  LPlugInCollectionItem: TPlugInCollectionItem;
  LReplacedEntry: Boolean;
  LCheckListBoxItem: TcxCheckListBoxItem;

  LCrawlerPlugin: ICrawlerPlugIn;
  LCrawlerCollectionItem: TCrawlerCollectionItem;

  LTypeID: TTypeID;
  LTypeIDs: TTypeIDs;
  LControlID: TControlID;
  LControlIDs: TControlIDs;
begin
  LPlugInCollectionItem := SettingsManager.Settings.Plugins.FindPlugInCollectionItemFromCollection(APlugin.GetName, ACollection);

  LReplacedEntry := False;
  if not Assigned(LPlugInCollectionItem) then
  begin
    LPlugInCollectionItem := TPlugInCollectionItem(ACollection.Add);
  end
  else if AHandleOverride and (MessageDlg(Format(StrPluginOverrideSettings, [APlugin.GetName]), mtConfirmation, [mbyes, mbno, mbcancel], 0) = ID_YES) then
  begin
    LReplacedEntry := True;
  end
  else
  begin
    Exit(False);
  end;

  // For the internal data storage
  with LPlugInCollectionItem do
  begin
    Name := APlugin.GetName;
    Enabled := False;
    Path := ExtractRelativePath(GetPluginFolder, APluginFile);
    IconHandle := LoadImage(APluginHandle, MakeIntResource('icon'), IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
  end;

  if Supports(APlugin, ICMSPlugIn) then
  begin
    with SettingsManager.Settings.Plugins do
      if Assigned(OnCMSChange) then
      begin
        if LReplacedEntry then
        begin
          OnCMSChange.Invoke(pctEnabled, LPlugInCollectionItem.Index, 0);
        end
        else
        begin
          OnCMSChange.Invoke(pctAdd, LPlugInCollectionItem.Index, -1);
        end;
      end;
  end;

  if Supports(APlugin, ICrawlerPlugIn) then
  begin
    LCrawlerCollectionItem := LPlugInCollectionItem as TCrawlerCollectionItem;
    with LCrawlerCollectionItem do
      if APlugin.QueryInterface(ICrawlerPlugIn, LCrawlerPlugin) = 0 then
      begin
        // Reset content
        Contingent.Clear;

        // Load content from crawler plugin
        Word(LTypeIDs) := LCrawlerPlugin.GetAvailableTypeIDs;
        for LTypeID := Low(TTypeID) to High(TTypeID) do
        begin
          if LTypeID in LTypeIDs then
          begin
            Longword(LControlIDs) := LCrawlerPlugin.GetAvailableControlIDs(Integer(LTypeID));

            for LControlID := Low(TControlID) to High(TControlID) do
              if LControlID in LControlIDs then
                with TCrawlerContingentCollectionItem(Contingent.Add) do
                begin
                  TypeID := LTypeID;
                  ControlID := LControlID;
                  Status := LCrawlerPlugin.GetControlIDDefaultValue(Integer(LTypeID), Integer(LControlID));
                end;
          end;
        end;
        Limit := LCrawlerPlugin.GetResultsLimitDefaultValue;

        // Free crawler plugin variable
        LCrawlerPlugin := nil;
      end;
  end;

  // For the GUI
  if Assigned(ACheckListBox) then
  begin
    if LReplacedEntry then
      LCheckListBoxItem := ACheckListBox.Items[ACheckListBox.Items.IndexOf(LPlugInCollectionItem.Name)]
    else
      LCheckListBoxItem := ACheckListBox.Items.Add;

    with LCheckListBoxItem do
    begin
      Checked := LPlugInCollectionItem.Enabled;
      Text := LPlugInCollectionItem.Name;
      ImageIndex := ACheckListBox.Images.AddIcon(LPlugInCollectionItem.Icon);
      Enabled := FileExists(LPlugInCollectionItem.GetPath);
    end;
  end;
end;

class function TAddPlugin.ExecuteBase(const APluginFile: string; APluginOnLoadedFunc: TPluginOnLoadedFunc; AHandleOverride: Boolean = True; const ARestrictToType: TPlugInType = ptNone; AErrorProc: TPluginErrorProc = nil): Boolean;
var
  LResult: Boolean;
  LPluginHandle: Cardinal;
begin
  LResult := False;
  TAddPlugin.LoadPluginBase(APluginFile, LPluginHandle,
    { } procedure(var APlugin: IPlugIn)
    { } var
    { . } LCollection: TCollection;
    { . } LCheckListBox: TcxCheckListBox;
    { } begin
    { . } if (ARestrictToType = ptNone) or ((not(ARestrictToType = ptNone)) and (ARestrictToType = APlugin.GetType)) then
    { . } begin
    { ... } if APluginOnLoadedFunc(APlugin.GetType, LCollection, LCheckListBox) then
    { ... } begin
    { ..... } LResult := TAddPlugin.AddPlugin(APluginFile, LPluginHandle, APlugin, LCollection, LCheckListBox, AHandleOverride);
    { ... } end;
    { . } end
    { . } else
    { . } begin
    { ... } TAddPlugin.ReturnError(TAddPlugin.IncorrectPluginTypeErrorMsg(APluginFile, APlugin, ARestrictToType), AErrorProc);
    { . } end;
    { } end, True, AErrorProc);

  Result := LResult;
end;

class function TAddPlugin.Execute(APluginOnLoadedFunc: TPluginOnLoadedFunc; const ARestrictToType: TPlugInType; APluginFile: string = ''; AHandleOverride: Boolean = True; AErrorProc: TPluginErrorProc = nil): Boolean;
var
  LFileIndex: Integer;
begin
  Result := True;

  if not FileExists(APluginFile) then
  begin
    with TOpenDialog.Create(nil) do
      try
        Options := Options + [ofAllowMultiSelect];
        Filter := TAddPlugin.PluginTypeToString(ARestrictToType) + ' plugin (*.dll)|*.dll';
        InitialDir := ExcludeTrailingPathDelimiter(GetPluginFolder);
        if Execute then
        begin
          for LFileIndex := 0 to Files.Count - 1 do
            if not TAddPlugin.ExecuteBase(Files.Strings[LFileIndex], APluginOnLoadedFunc, True, ARestrictToType, AErrorProc) then
            begin
              Result := False;
            end;
        end;
      finally
        Free;
      end;
  end
  else
  begin
    Result := TAddPlugin.ExecuteBase(APluginFile, APluginOnLoadedFunc, False, ARestrictToType);
  end;
end;

class function TAddPlugin.ExecuteFolder(APluginOnLoadedFunc: TPluginOnLoadedFunc; AFolderPath: string; const ARestrictToType: TPlugInType = ptNone): Boolean;
var
  LStringList: TStringList;
  LFileIndex: Integer;
begin
  Result := True;

  LStringList := TStringList.Create;
  try
    GetFilesInDirectory(AFolderPath, '*.dll', LStringList, True, True, True, True);

    for LFileIndex := 0 to LStringList.Count - 1 do
      if not TAddPlugin.ExecuteBase(LStringList.Strings[LFileIndex], APluginOnLoadedFunc, False, ARestrictToType, {make the error message silent} procedure(AErrorMsg: string)begin end) then
      begin
        Result := False;
      end;
  finally
    LStringList.Free;
  end;
end;

end.
