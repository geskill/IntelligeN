unit u%FullName%;

interface

uses
  // Delphi
  SysUtils, Variants,
  // Common
  uBaseConst, uAppConst, uAppInterface,
  // DLLs
  uExport,
  // Plugin system
  uPlugInInterfaceAdv, uPlugInFileFormatClass,
  // Utils
  uFileUtils, uStringUtils, uVariantUtils;

type
  T%FullName% = class(TImageHosterPlugIn)
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

{ T%FullName% }

function T%FullName%.GetAuthor;
begin
  Result := '%CompanyName%';
end;

function T%FullName%.GetAuthorURL;
begin
  Result := 'http://example.com/';
end;

function T%FullName%.GetDescription;
begin
  Result := GetName + ' file formats plug-in.';
end;

function T%FullName%.GetName;
begin
  Result := '%FullName%';
end;

function T%FullName%.GetFileExtension;
begin
  Result := '.TODO-ADD-FILE-EXT';
end;

function T%FullName%.GetFileFilter;
begin
  Result := 'TODO-ADD-NAME %s (*.TODO-ADD-FILE-EXT)|*.TODO-ADD-FILE-EXT|';
end;

function T%FullName%.CanSaveFiles;
begin
  { TODO : change? }
  Result := True;
end;

function T%FullName%.SaveFile;
begin
  { TODO : implement }
end;

function T%FullName%.CanLoadFiles;
begin
  { TODO : change? }
  Result := False;
end;

function T%FullName%.LoadFile;
begin
  { TODO : implement }
end;

end.
