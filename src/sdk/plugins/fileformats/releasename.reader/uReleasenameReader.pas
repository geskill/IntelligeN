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
  uPlugInInterfaceAdv, uPlugInFileFormatClass;

type
  TReleasenameReader = class(TFileFormatPlugIn)
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function GetFileExtension: WideString; override;
    function GetFileFilter: WideString; override;

    function CanSaveFiles: WordBool; override;
    function SaveFile(const AFileName: WideString; const ATabSheetController: ITabSheetController): WordBool; override;

    function CanLoadFiles: WordBool; override;
    function LoadFile(const AFileFormatData: IFileFormatData; const AFileName: WideString; const APageController: IPageController): Integer; override;
  end;

implementation

uses
  uSelectTemplateFileName;

{ TReleasenameReader }

function TReleasenameReader.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TReleasenameReader.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TReleasenameReader.GetDescription;
begin
  Result := GetName + ' file formats plug-in.';
end;

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
