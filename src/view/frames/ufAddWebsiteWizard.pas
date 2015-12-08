unit ufAddWebsiteWizard;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Menus, StdCtrls,
  // Dev Express
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxCheckBox, cxTextEdit, cxLabel,
  cxButtons, cxMaskEdit, cxDropDownEdit, cxRadioGroup, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage,
  cxGridCustomTableView, cxGridTableView, cxGridCustomView, cxClasses, cxGridLevel, cxGrid, cxHyperLinkEdit, cxNavigator,
  // RegEx
  RegExpr,
  // OmniThreadLibrary
  OtlParallel, OtlTaskControl,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface, uFileInterface,
  // DLLs
  uExport,
  // API
  uApiConst, uApiFile, uApiPlugins, uApiSettings, uApiXml,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPIndyHelper, uHTTPManager,
  // Utils
  uFileUtils, uStringUtils, uURLUtils;

type
  TfAddWebsiteWizard = class(TFrame)
    cxLAddWebsiteWizard: TcxLabel;
    pStart: TPanel;
    cxRBNew: TcxRadioButton;
    cxRBLoad: TcxRadioButton;
    cxRBImport: TcxRadioButton;
    pNewWebsite: TPanel;
    cxLWebsiteURL: TcxLabel;
    cxTEPageURL: TcxTextEdit;
    cxLFormattedURL: TcxLabel;
    cxTEFormattedURL: TcxTextEdit;
    cxCBEditFormattedURL: TcxCheckBox;
    cxLURLCMS: TcxLabel;
    cxCOBURLCMS: TcxComboBox;
    cxBDetectCMS: TcxButton;
    cxBCancel: TcxButton;
    cxBNext: TcxButton;
    cxLPageURLInfo: TcxLabel;
    pLoad: TPanel;
    cxGLoadLevel1: TcxGridLevel;
    cxGLoad: TcxGrid;
    cxGLoadTableView1: TcxGridTableView;
    cxGLoadTableView1Column1: TcxGridColumn;
    cxGLoadTableView1Column2: TcxGridColumn;
    cxGLoadTableView1Column3: TcxGridColumn;
    pImport: TPanel;
    cxGImportLevel1: TcxGridLevel;
    cxGImport: TcxGrid;
    cxGImportTableView1: TcxGridTableView;
    cxGImportTableView1Column1: TcxGridColumn;
    cxGImportTableView1Column2: TcxGridColumn;
    cxGImportTableView1Column3: TcxGridColumn;
    cxRBImportCopy: TcxRadioButton;
    cxRBImportReplace: TcxRadioButton;
    cxLEncoding: TcxLabel;
    cxCOBEncoding: TcxComboBox;
    procedure cxBCancelClick(Sender: TObject);
    procedure cxBNextClick(Sender: TObject);
    procedure cxTEPageURLPropertiesChange(Sender: TObject);
    procedure cxTEFormattedURLPropertiesChange(Sender: TObject);
    procedure cxCBEditFormattedURLPropertiesChange(Sender: TObject);
    procedure cxCOBURLCMSPropertiesChange(Sender: TObject);
    procedure cxCOBEncodingPropertiesChange(Sender: TObject);
    procedure cxBDetectCMSClick(Sender: TObject);
    procedure cxGLoadTableView1Column2PropertiesEditValueChanged(Sender: TObject);
    procedure cxGImportTableView1Column2PropertiesEditValueChanged(Sender: TObject);
  private
    procedure VerifyNewWebsiteInputs;
    procedure UpdateWebsiteInformations(const HTTPResult: IHTTPResult);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  uMain, uSettings;
{$R *.dfm}

procedure TfAddWebsiteWizard.cxBCancelClick(Sender: TObject);
begin
  if (cxBCancel.Caption = 'Back') then
  begin
    if cxRBNew.Checked then
    begin
      pNewWebsite.Visible := False;
    end;
    if cxRBLoad.Checked then
    begin
      pLoad.Visible := False;
    end;
    if cxRBImport.Checked then
    begin
      pImport.Visible := False;
    end;
    cxBCancel.Visible := Settings.CMSPluginsCheckListBox.InnerCheckListBox.ItemIndex <> -1;
    pStart.Visible := True;
    cxBCancel.Caption := 'Cancel';
    cxBNext.Enabled := True;
    cxBNext.Caption := 'Next';
  end
  else
    with Settings do
    begin
      fAddWebsiteWizard.Visible := False;
      pCMSSettings.Visible := not fAddWebsiteWizard.Visible;
    end;
end;

procedure TfAddWebsiteWizard.cxBNextClick(Sender: TObject);

  procedure SetCMSList(AcxComboBoxProperties: TcxComboBoxProperties);
  var
    I: Integer;
  begin
    AcxComboBoxProperties.Items.Clear;
    with Settings.CMSPluginsCheckListBox.InnerCheckListBox do
      for I := 0 to Count - 1 do
        if Items[I].Enabled then
          AcxComboBoxProperties.Items.Add(Items[I].Text);
  end;

  function FixCase(AWebsiteType: string): string;
  var
    I: Integer;
  begin
    Result := '';
    with Settings.CMSPluginsCheckListBox.InnerCheckListBox do
      for I := 0 to Count - 1 do
        if SameText(AWebsiteType, Items[I].Text) then
        begin
          Result := Items[I].Text;
          break;
        end;
  end;

var
  I: Integer;
  AllWebsiteTypesDefined: Boolean;
  WebsiteConfigurationFile: IWebsiteConfigurationFile;
begin
  if (cxBNext.Caption = 'Next') then
  begin
    AllWebsiteTypesDefined := True;
    if cxRBNew.Checked then
    begin
      SetCMSList(cxCOBURLCMS.Properties);
      pNewWebsite.Visible := True;
      cxTEPageURL.SetFocus;
    end;
    if cxRBLoad.Checked then
    begin
      with TOpenDialog.Create(nil) do
        try
          Options := Options + [ofAllowMultiSelect];
          Filter := 'Website Templates (*.xml)|*.xml';
          InitialDir := ExcludeTrailingPathDelimiter(GetTemplatesSiteFolder);

          if Execute then
          begin
            SetCMSList(cxGLoadTableView1Column2.Properties as TcxComboBoxProperties);

            with cxGLoadTableView1.DataController do
            begin
              BeginUpdate;
              try
                RecordCount := Files.Count;
                for I := 0 to Files.Count - 1 do
                begin
                  with TWebsiteTemplateHelper.Load(Files.Strings[I]) do
                  begin
                    Values[I, cxGLoadTableView1Column1.index] := WebsiteURL;
                    Values[I, cxGLoadTableView1Column2.index] := FixCase(WebsiteType);
                    Values[I, cxGLoadTableView1Column3.index] := Files.Strings[I];
                    if WebsiteType = '' then
                      AllWebsiteTypesDefined := False;
                  end;
                end;
              finally
                EndUpdate;
              end;
            end;
            pLoad.Visible := True;
          end
          else
            Exit;
        finally
          Free;
        end;
    end;
    if cxRBImport.Checked then
    begin
      with TOpenDialog.Create(nil) do
        try
          Options := Options + [ofAllowMultiSelect];
          Filter := 'Multi-Poster Templates (*.mpu;*.ep4)|*.mpu;*.ep4';

          if Execute then
          begin
            SetCMSList(cxGImportTableView1Column2.Properties as TcxComboBoxProperties);

            with cxGImportTableView1.DataController do
            begin
              BeginUpdate;
              try
                RecordCount := Files.Count;
                for I := 0 to Files.Count - 1 do
                begin
                  with TWebsiteTemplateHelper.Import(Files.Strings[I]) do
                  begin
                    Values[I, cxGImportTableView1Column1.index] := WebsiteURL;
                    Values[I, cxGImportTableView1Column2.index] := WebsiteType;
                    Values[I, cxGImportTableView1Column3.index] := Files.Strings[I];
                    if WebsiteType = '' then
                      AllWebsiteTypesDefined := False;
                  end;
                end;
              finally
                EndUpdate;
              end;
            end;
            pImport.Visible := True;
          end
          else
            Exit;
        finally
          Free;
        end;
    end;
    pStart.Visible := False;
    cxBCancel.Visible := True;
    cxBCancel.Caption := 'Back';
    cxBNext.Enabled := False or (AllWebsiteTypesDefined and (cxRBLoad.Checked or cxRBImport.Checked));
    cxBNext.Caption := 'OK';
  end
  else
  begin
    if cxRBNew.Checked then
    begin
      with TSaveDialog.Create(nil) do
        try
          Filter := 'Website Templates (*.xml)|*.xml';
          InitialDir := ExcludeTrailingPathDelimiter(GetTemplatesSiteFolder);
          FileName := TrueFilename(RemoveW(ExtractUrlHost(cxTEFormattedURL.Text)));

          if Execute then
          begin
            if not(ExtractFileExt(FileName) = '.xml') then
              FileName := FileName + '.xml';
            WebsiteConfigurationFile := TIntelligeNConfigurationFile.Create;
            with WebsiteConfigurationFile do
            begin
              WebsiteURL := cxTEFormattedURL.Text;
              WebsiteType := cxCOBURLCMS.Text;
              WebsiteCharset := cxCOBEncoding.Text;
            end;
            TWebsiteTemplateHelper.Save(FileName, WebsiteConfigurationFile);
            Settings.AddCMSWebsite(FileName, cxTEFormattedURL.Text, cxCOBURLCMS.Text);
          end;
        finally
          Free;
        end;
    end;
    if cxRBLoad.Checked then
    begin
      with cxGLoadTableView1.DataController do
      begin
        for I := 0 to RecordCount - 1 do
        begin
          WebsiteConfigurationFile := TWebsiteTemplateHelper.Load(Values[I, cxGLoadTableView1Column3.index]);
          with WebsiteConfigurationFile do
          begin
            if not SameStr(WebsiteURL, Values[I, cxGLoadTableView1Column1.index]) then
              WebsiteURL := Values[I, cxGLoadTableView1Column1.index];
            if not SameStr(WebsiteType, Values[I, cxGLoadTableView1Column2.index]) then
              WebsiteType := Values[I, cxGLoadTableView1Column2.index];
          end;
          TWebsiteTemplateHelper.Save(Values[I, cxGLoadTableView1Column3.index], WebsiteConfigurationFile);
          Settings.AddCMSWebsite(Values[I, cxGLoadTableView1Column3.index], Values[I, cxGLoadTableView1Column1.index], Values[I, cxGLoadTableView1Column2.index]);
        end;

        RecordCount := 0;

        pLoad.Visible := False;
      end;
    end;
    if cxRBImport.Checked then
    begin
      with cxGImportTableView1.DataController do
      begin
        for I := 0 to RecordCount - 1 do
        begin
          WebsiteConfigurationFile := TWebsiteTemplateHelper.Import(Values[I, cxGLoadTableView1Column3.index]);
          with WebsiteConfigurationFile do
          begin
            WebsiteURL := Values[I, cxGImportTableView1Column1.index];
            WebsiteType := Values[I, cxGImportTableView1Column2.index];
          end;
          TWebsiteTemplateHelper.Save(ChangeFileExt(Values[I, cxGImportTableView1Column3.index], '.xml'), WebsiteConfigurationFile, True);
          if cxRBImportReplace.Checked and FileExists(Values[I, cxGLoadTableView1Column3.index]) then
            DeleteFile(Values[I, cxGLoadTableView1Column3.index]);
          Settings.AddCMSWebsite(ChangeFileExt(Values[I, cxGImportTableView1Column3.index], '.xml'), Values[I, cxGImportTableView1Column1.index], Values[I, cxGImportTableView1Column2.index]);
        end;
        RecordCount := 0;

        pImport.Visible := False;
      end;
    end;
    cxBCancel.Visible := Settings.CMSPluginsCheckListBox.InnerCheckListBox.ItemIndex <> -1;
    pStart.Visible := True;
    cxBCancel.Caption := 'Cancel';
    cxBNext.Enabled := True;
    cxBNext.Caption := 'Next';
    with Settings do
    begin
      fAddWebsiteWizard.Visible := Settings.CMSPluginsCheckListBox.InnerCheckListBox.ItemIndex = -1;
      pCMSSettings.Visible := not fAddWebsiteWizard.Visible;
    end;
  end;
end;

procedure TfAddWebsiteWizard.cxTEPageURLPropertiesChange(Sender: TObject);
var
  LUrl: string;
begin
  cxCOBURLCMS.Clear;
  cxCOBEncoding.Clear;

  LUrl := BuildWebsiteUrl(cxTEPageURL.Text);

  if not cxCBEditFormattedURL.Checked then
    cxTEFormattedURL.Text := LUrl;
  VerifyNewWebsiteInputs;
end;

procedure TfAddWebsiteWizard.cxTEFormattedURLPropertiesChange(Sender: TObject);
begin
  VerifyNewWebsiteInputs;
end;

procedure TfAddWebsiteWizard.cxCBEditFormattedURLPropertiesChange(Sender: TObject);
begin
  cxTEFormattedURL.Properties.ReadOnly := not cxCBEditFormattedURL.Checked;
end;

procedure TfAddWebsiteWizard.cxCOBURLCMSPropertiesChange(Sender: TObject);
begin
  VerifyNewWebsiteInputs;
end;

procedure TfAddWebsiteWizard.cxCOBEncodingPropertiesChange(Sender: TObject);
begin
  VerifyNewWebsiteInputs;
end;

procedure TfAddWebsiteWizard.cxBDetectCMSClick(Sender: TObject);
var
  HTTPRequest: IHTTPRequest;
  HTTPOptions: IHTTPOptions;
  RequestID: Double;
begin
  HTTPRequest := THTTPRequest.Create(cxTEFormattedURL.Text);
  HTTPRequest.Referer := cxTEFormattedURL.Text;

  HTTPOptions := THTTPOptions.Create(SettingsManager.Settings.http.GetProxy(psaCMS));
  HTTPOptions.ConnectTimeout := SettingsManager.Settings.http.ConnectTimeout;
  HTTPOptions.ReadTimeout := SettingsManager.Settings.http.ReadTimeout;

  RequestID := THTTPManager.Instance().Get(HTTPRequest, HTTPOptions);

  Parallel.Async(
    { } procedure
    { } begin
    { . } THTTPManager.Wait(RequestID);
    { } end,
    { } Parallel.TaskConfig.OnTerminated(
      { } procedure(const task: IOmniTaskControl)
      { } var
      { . } HTTPProcess: IHTTPProcess;
      { } begin
      { . } HTTPProcess := THTTPManager.Instance().GetResult(RequestID);
      { . } if HTTPProcess.HTTPResult.HasError then
      { ... } MessageDlg(HTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage, mtError, [mbOK], 0)
      { . } else
      { ... } UpdateWebsiteInformations(HTTPProcess.HTTPResult);
      { } end
      { } ));
end;

procedure TfAddWebsiteWizard.VerifyNewWebsiteInputs;
begin
  cxBNext.Enabled := (length(cxTEFormattedURL.Text) > 12) and (cxCOBURLCMS.Text <> '') and (cxCOBEncoding.Text <> '');
end;

procedure TfAddWebsiteWizard.UpdateWebsiteInformations(const HTTPResult: IHTTPResult);
var
  I: Integer;
  NewURL, CharSet: string;
  NeedWWW: Boolean;
begin
  if (HTTPResult.HTTPResponseInfo.RedirectCount > 0) then
    NewURL := HTTPResult.HTTPResponseInfo.LastRedirect
  else if not SameStr('', HTTPResult.HTTPResponse.Refresh) then
    NewURL := HTTPResult.HTTPResponse.Refresh;

  if not SameStr('', NewURL) then
  begin
    // Handle result: /content/index instead of http://example.org/content/index
    if not BeginsWithHTTP(NewURL) then
    begin
      if not(copy(NewURL, 1, 1) = '/') then
        NewURL := IncludeTrailingUrlDelimiter(cxTEPageURL.Text) + NewURL
      else
        NewURL := ExcludeTrailingUrlDelimiter(cxTEPageURL.Text) + NewURL;
    end;

    cxTEPageURL.Text := NewURL;
  end;

  NewURL := cxTEPageURL.Text;
  if (Pos('www.', NewURL) = 0) then
  begin
    NeedWWW := False;
    with TRegExpr.Create do
      try
        ModifierS := False;

        InputString := HTTPResult.SourceCode;
        Expression := StringReplace(NewURL, '://', '://(.*?)', []);

        if Exec(InputString) then
        begin
          repeat
            NeedWWW := SameText('www.', Match[1]);
          until not(ExecNext and NeedWWW);
        end;
      finally
        Free;
      end;
    if NeedWWW then
      cxTEPageURL.Text := StringReplace(NewURL, '://', '://www.', []);
  end;

  CharSet := HTTPResult.HTTPResponse.CharSet;
  if not SameStr('', CharSet) then
    cxCOBEncoding.Text := CharSet;

  with SettingsManager.Settings.Plugins.CMS do
    for I := 0 to Count - 1 do
      try
        if TPluginBasic.CMSBelongsTo(TCMSCollectionItem(Items[I]).Path, HTTPResult.SourceCode) then
        begin
          cxCOBURLCMS.Text := TCMSCollectionItem(Items[I]).name;
          if SameStr('', CharSet) then
            cxCOBEncoding.Text := TPluginBasic.CMSDefaultCharset(TCMSCollectionItem(Items[I]).Path);
          break;
        end;
      except
        // ignore plugin not found exeption
      end;
end;

procedure TfAddWebsiteWizard.cxGLoadTableView1Column2PropertiesEditValueChanged(Sender: TObject);
var
  I: Integer;
  AllWebsiteTypesDefined: Boolean;
begin
  AllWebsiteTypesDefined := True;
  with cxGLoadTableView1.DataController do
    for I := 0 to RecordCount - 1 do
      if Values[I, cxGLoadTableView1Column2.index] = '' then
      begin
        AllWebsiteTypesDefined := False;
        break;
      end;
  cxBNext.Enabled := AllWebsiteTypesDefined;
end;

procedure TfAddWebsiteWizard.cxGImportTableView1Column2PropertiesEditValueChanged(Sender: TObject);
var
  I: Integer;
  AllWebsiteTypesDefined: Boolean;
begin
  AllWebsiteTypesDefined := True;
  with cxGImportTableView1.DataController do
    for I := 0 to RecordCount - 1 do
      if Values[I, cxGImportTableView1Column2.index] = '' then
      begin
        AllWebsiteTypesDefined := False;
        break;
      end;
  cxBNext.Enabled := AllWebsiteTypesDefined;
end;

constructor TfAddWebsiteWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  cxCOBEncoding.Properties.Items.Text := THTTPIndyHelper.Charsets;
end;

end.
