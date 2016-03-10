unit uPluginWizard;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Controls, Forms, StdCtrls, Spin,
  // DevExpress
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxTextEdit, cxMaskEdit,
  // Utils
  uFileUtils, uStringUtils;

type
  TfPluginWizard = class(TForm)
    lFullName: TLabel;
    eFullName: TcxMaskEdit;
    lBasicName: TLabel;
    eBasicName: TEdit;
    lCompanyName: TLabel;
    cobCompanyName: TComboBox;
    lMajorVer: TLabel;
    seMajorVer: TSpinEdit;
    lMinorVer: TLabel;
    seMinorVer: TSpinEdit;
    lRelease: TLabel;
    seRelease: TSpinEdit;
    lBuild: TLabel;
    seBuild: TSpinEdit;
    bCreate: TButton;
    lCopyright: TLabel;
    cobCopyright: TComboBox;
    lWebsite: TLabel;
    eWebsite: TEdit;
    lFolderName: TLabel;
    eFolderName: TcxMaskEdit;
    procedure eFolderNamePropertiesChange(Sender: TObject);
    procedure eFullNamePropertiesChange(Sender: TObject);
    procedure bCreateClick(Sender: TObject);
  private
    FTemplatePath: TFileName;
  public
    property TemplatePath: TFileName read FTemplatePath write FTemplatePath;

    function Replace(AString: string): string;
  end;

var
  fPluginWizard: TfPluginWizard;

implementation

{$R *.dfm}

procedure TfPluginWizard.bCreateClick(Sender: TObject);
var
  Path, NewPath, NewFileName: string;
  StringList, FileStringList: TStringList;
  LFileIndex: Integer;
begin
  Path := IncludeTrailingPathDelimiter(TemplatePath);
  NewPath := Path + IncludeTrailingPathDelimiter(eFolderName.Text);
  ForceDirectories(NewPath);

  StringList := TStringList.Create;
  try
    GetFilesInDirectory(Path + '!template', '*.*', StringList, False, True, True, True);

    for LFileIndex := 0 to StringList.Count - 1 do
    begin
      NewFileName := Replace(StringList.Strings[LFileIndex]);

      if not(IndexText(ExtractFileExt(StringList.Strings[LFileIndex]), ['.pas', '.dpr', '.dproj']) = -1) then
      begin
        FileStringList := TStringList.Create;
        try
          FileStringList.LoadFromFile(IncludeTrailingPathDelimiter(Path + '!template') + StringList.Strings[LFileIndex]);
          FileStringList.Text := Replace(FileStringList.Text);
          FileStringList.SaveToFile(IncludeTrailingPathDelimiter(NewPath) + NewFileName, TEncoding.UTF8);
        finally
          FileStringList.Free;
        end;
      end
      else
      begin
        CopyFile(PChar(IncludeTrailingPathDelimiter(Path + '!template') + StringList.Strings[LFileIndex]), PChar(IncludeTrailingPathDelimiter(NewPath) + NewFileName), False);
      end;
    end;
  finally
    StringList.Free;
  end;

  Close;
end;

procedure TfPluginWizard.eFolderNamePropertiesChange(Sender: TObject);

  function ConvertName(const AStr: string): string;
  var
    LStringIndex: Integer;
  begin
    Result := AStr;
    for LStringIndex := Length(Result) downto 1 do
      if (Result[LStringIndex] in ['.']) then
      begin
        // Upper case every char after '-'
        if LStringIndex < Length(Result) then
          Result[LStringIndex + 1] := UpCase(Result[LStringIndex + 1]);
        Delete(Result, LStringIndex, 1);
      end;
    // Upper case first char
    if Length(Result) > 0 then
      Result[1] := UpCase(Result[1]);
  end;

begin
  eFullName.Text := ConvertName(eFolderName.Text);

  bCreate.Enabled := (Length(eFolderName.Text) > 2) and (Length(eFullName.Text) > 2);
end;

procedure TfPluginWizard.eFullNamePropertiesChange(Sender: TObject);
begin
  eBasicName.Text := LowerCase(eFullName.Text);

  bCreate.Enabled := (Length(eFolderName.Text) > 2) and (Length(eFullName.Text) > 2);
end;

function TfPluginWizard.Replace(AString: string): string;
var
  NewGUID: TGUID;
begin
  CreateGUID(NewGUID);
  Result := StringReplaceMultiple(AString, ['%ProjectGuid%', '%BasicName%', '%FullName%', '%FolderName%', '%Website%', '%CompanyName%', '%Copyright%', '%MajorVer%', '%MinorVer%', '%Release%', '%Build%'], { . }
    [GUIDToString(NewGUID), eBasicName.Text, eFullName.Text, eFolderName.Text, eWebsite.Text, cobCompanyName.Text, cobCopyright.Text, seMajorVer.Text, seMinorVer.Text, seRelease.Text, seBuild.Text], False);
end;

end.
