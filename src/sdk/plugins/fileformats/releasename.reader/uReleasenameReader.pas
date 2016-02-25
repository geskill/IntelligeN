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

    function GetFileExtension: WideString; override; safecall;
    function GetFileFilter: WideString; override; safecall;

    function CanSaveFiles: WordBool; override; safecall;
    function SaveFile(const AFileName: WideString; const ATabSheetController: ITabSheetController): WordBool; override; safecall;

    function CanLoadFiles: WordBool; override; safecall;
    function LoadFile(const AFileName: WideString; const APageController: IPageController): Integer; override; safecall;
  end;

implementation

uses
  uSelectTemplateFileName;

{ TExtractedPostCode }

function TReleasenameReader.GetName;
begin
  Result := 'releasename.reader';
end;

function TReleasenameReader.GetFileExtension;
begin
  Result := '.txt';
end;

function TReleasenameReader.GetFileFilter;
begin
  Result := 'releasename list %s (*.txt)|*.txt|';
end;

function TReleasenameReader.CanSaveFiles;
begin
  Result := False;
end;

function TReleasenameReader.SaveFile;
begin
  Result := False;
end;

function TReleasenameReader.CanLoadFiles;
begin
  Result := True;
end;

function TReleasenameReader.LoadFile;
var
  I: Integer;
  LTemplateFileName: TFileName;
begin
  Result := -1;
  with TStringlist.Create do
    try
      LoadFromFile(AFileName);

      with TSelectTemplateFileName.Create(nil) do
        try
          if Execute then
            LTemplateFileName := TemplateFileName;
        finally
          Free;
        end;

      for I := 0 to Count - 1 do
        if length(Strings[I]) > 12 then
        begin
          with APageController.TabSheetController[APageController.NewTabSheet(LTemplateFileName)] do
          begin
            ReleaseName := Strings[I];
            Result := TabSheetIndex;
          end;
        end;
    finally
      Free;
    end;
end;

end.
