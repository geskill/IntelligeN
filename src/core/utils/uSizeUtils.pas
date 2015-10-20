unit uSizeUtils;

interface

uses
  // Delphi
  Windows, SysUtils;

type
  TSizeFormatter = class
    class function SizeToByte(ASizeString: string; ASizeType: string = ''; ADotAsDecimalSeparator: Boolean = True): Int64;
  end;

implementation

{ TSizeFormatter }

class function TSizeFormatter.SizeToByte(ASizeString: string; ASizeType: string = ''; ADotAsDecimalSeparator: Boolean = True): Int64;
var
  LFormatSettings: TFormatSettings;
  LSize: Extended;
begin
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, LFormatSettings);
  if ADotAsDecimalSeparator then
    LFormatSettings.DecimalSeparator := '.'
  else
    LFormatSettings.DecimalSeparator := ',';

  LSize := StrToFloatDef(ASizeString, 0, LFormatSettings);

  if (Pos('KB', UpperCase(ASizeType)) > 0) then
    Result := Round(LSize * 1024)
  else if (Pos('MB', UpperCase(ASizeType)) > 0) then
    Result := Round(LSize * 1048576)
  else if (Pos('GB', UpperCase(ASizeType)) > 0) then
    Result := Round(LSize * 1073741824)
  else
    Result := Round(LSize);
end;

end.
