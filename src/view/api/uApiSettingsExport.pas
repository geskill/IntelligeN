unit uApiSettingsExport;

interface

uses
  // Delphi
  Windows, SysUtils, Classes,
  // AB
  AbArcTyp, AbZipper,
  // Api
  uApiMain, uApiSettings,
  // DLLs
  uExport;

type
  TApiSettingsExport = class
  private
    FAbZipper: TAbZipper;
  public
    constructor Create;
    procedure ExportSettings(const AFileName: TFileName);
    destructor Destroy; override;
  end;

implementation

{ TApiSettingsExport }

constructor TApiSettingsExport.Create;
begin
  FAbZipper := TAbZipper.Create(nil);
  with FAbZipper do
  begin
    AutoSave := True;
    DOSMode := False;
    StoreOptions := StoreOptions + [soRecurse];
  end;
end;

procedure TApiSettingsExport.ExportSettings(const AFileName: TFileName);
var
  I, J: Integer;
  _FileStream: TFileStream;
  _templates_website_outside, _templates_cms_outside: Boolean;
  _StringStream_templates_website, _StringStream_templates_cms: TStringStream;
begin
  with FAbZipper do
  begin
    Filename := AFileName;
    BaseDirectory := ExtractFilePath(ParamStr(0));

    AddFiles('configuration\*.xml', 0);

    AddFiles('settings\settings.xml', 0);

    AddFiles('templates_cms\*.js', 0);
    AddFiles('templates_cms\*.txt', 0);

    AddFiles('templates_site\*.xml', 0);

    AddFiles('templates_type\*.xml', 0);

    _templates_website_outside := False;
    _templates_cms_outside := False;

    _StringStream_templates_website := TStringStream.Create;
    _StringStream_templates_cms := TStringStream.Create;
    try
      // if website-templates or message-templates are outside of main program folder
      with SettingsManager.Settings.Plugins.CMS do
        for I := 0 to Count - 1 do
          for J := 0 to TCMSCollectionItem(Items[I]).Websites.Count - 1 do
            with TCMSWebsitesCollectionItem(TCMSCollectionItem(Items[I]).Websites.Items[J]) do
            begin
              if Pos(LowerCase(GetTemplatesSiteFolder), LowerCase(GetPath)) = 0 then
              begin
                _templates_website_outside := True;
                if FileExists(GetPath) then
                begin
                  _FileStream := TFileStream.Create(GetPath, OF_SHARE_DENY_NONE);
                  try
                    _StringStream_templates_website.WriteString(ExtractFileName(GetPath) + ':' + sLineBreak + ExtractFilePath(GetPath) + sLineBreak + sLineBreak);
                    AddFromStream('!templates_site\' + ExtractFileName(GetPath), _FileStream);
                  finally
                    _FileStream.Free;
                  end;
                end
                else
                  _StringStream_templates_website.WriteString(ExtractFileName(GetPath) + ' (FILE IS MISSING!):' + sLineBreak + ExtractFilePath(GetPath) + sLineBreak + sLineBreak);
              end;
              if Pos(LowerCase(GetTemplatesCMSFolder), LowerCase(GetMessageFileName)) = 0 then
              begin
                _templates_cms_outside := True;
                if FileExists(GetMessageFileName) then
                begin
                  _FileStream := TFileStream.Create(GetMessageFileName, OF_SHARE_DENY_NONE);
                  try
                    _StringStream_templates_cms.WriteString(ExtractFileName(GetMessageFileName) + ':' + sLineBreak + ExtractFilePath(GetMessageFileName) + sLineBreak + sLineBreak);
                    AddFromStream('!templates_cms\' + ExtractFileName(GetMessageFileName), _FileStream);
                  finally
                    _FileStream.Free;
                  end;
                end
                else
                  _StringStream_templates_cms.WriteString(ExtractFileName(GetMessageFileName) + ' (FILE IS MISSING!):' + sLineBreak + ExtractFilePath(GetMessageFileName) + sLineBreak + sLineBreak);
              end;
            end;

      // add logs to restore file-system
      if _templates_website_outside then
        AddFromStream('!templates_site\!log.txt', _StringStream_templates_website);
      if _templates_cms_outside then
        AddFromStream('!templates_cms\!log.txt', _StringStream_templates_cms);
    finally
      _StringStream_templates_cms.Free;
      _StringStream_templates_website.Free;
    end;

    ZipfileComment := 'created with v' + IntToStr(MINOR_VERSION);

    CloseArchive;
  end;

end;

destructor TApiSettingsExport.Destroy;
begin
  FAbZipper.Free;
  inherited;
end;

end.
