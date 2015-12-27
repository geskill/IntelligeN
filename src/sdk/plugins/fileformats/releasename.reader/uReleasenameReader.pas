unit uReleasenameReader;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants, StrUtils, XMLDoc, XMLIntf,
  // Common
  uBaseConst, uAppInterface,
  // DLLs
  uExport,
  // Plugin system
  uPlugInFileFormatClass;

type
  TReleasenameReader = class(TFileFormatPlugIn)
  public
    function GetName: WideString; override; safecall;
    function GetFileFormatName: WideString; override; safecall;
    function CanSaveControls: WordBool; override; safecall;
    procedure SaveControls(const AFileName, ATemplateFileName: WideString; const ATabSheetController: ITabSheetController); override; safecall;
    function CanLoadControls: WordBool; override; safecall;
    function LoadControls(const AFileName, ATemplateDirectory: WideString; const APageController: IPageController): Integer; override; safecall;
  end;

implementation

uses
  uSelectTemplateFileName;

{ TExtractedPostCode }

function TReleasenameReader.GetName;
begin
  result := 'releasename.reader';
end;

function TReleasenameReader.GetFileFormatName;
begin
  result := 'releasename list %s (*.txt)|*.txt|';
end;

function TReleasenameReader.CanSaveControls;
begin
  result := False;
end;

procedure TReleasenameReader.SaveControls;
begin
  //
end;

function TReleasenameReader.CanLoadControls;
begin
  result := True;
end;

function TReleasenameReader.LoadControls;
var
  I: Integer;
  TemplateType: TTypeID;
  _TemplateFileName: TFileName;
begin
  result := -1;
  with TStringlist.Create do
    try
      LoadFromFile(AFileName);

      with TSelectTemplateFileName.Create(nil) do
        try
          if Execute then
          begin
            TemplateType := GetFileInfo(ATemplateDirectory + TemplateFileName + '.xml').TemplateType;
            _TemplateFileName := TemplateFileName + '.xml';
          end
          else
            raise Exception.Create('');
        finally
          Free;
        end;

      for I := 0 to Count - 1 do
        if length(Strings[I]) > 12 then
        begin
          with APageController.TabSheetController[APageController.Add(ATemplateDirectory + _TemplateFileName, TemplateType)] do
          begin
            ControlController.FindControl(cReleaseName).Value := Strings[I];
            Result := TabSheetIndex;
          end;
        end;
    finally
      Free;
    end;

end;

end.
