unit uSizeUtils;

interface

uses
  Windows, SysUtils;

type
  TSizeFormatter = class
    class function SizeToByte(ASizeString: string; ASizeType: string = ''; ADotDecimalSeparator: Boolean = True): Int64;
  end;

implementation

{ TSizeFormatter }

class function TSizeFormatter.SizeToByte(ASizeString: string; ASizeType: string = ''; ADotDecimalSeparator: Boolean = True): Int64;
var
  FormatSettings: TFormatSettings;
  Size: Extended;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
  if ADotDecimalSeparator then
    FormatSettings.DecimalSeparator := '.'
  else
    FormatSettings.DecimalSeparator := ',';

  Size := StrToFloatDef(ASizeString, 0, FormatSettings);

  if (Pos('KB', UpperCase(ASizeType)) > 0) then
    Result := Round(Size * 1024)
  else if (Pos('MB', UpperCase(ASizeType)) > 0) then
    Result := Round(Size * 1048576)
  else if (Pos('GB', UpperCase(ASizeType)) > 0) then
    Result := Round(Size * 1073741824)
  else
    Result := Round(Size);
end;

end.
