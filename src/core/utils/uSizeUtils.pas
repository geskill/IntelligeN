unit uSizeUtils;

interface

uses
  // Delphi
  Windows, SysUtils;

type
  TSizeFormatter = class
    class function SizeToByte(const ASizeString: string; const ASizeType: string = ''; ADotAsDecimalSeparator: Boolean = True): Int64;
  end;

implementation

{ TSizeFormatter }

class function TSizeFormatter.SizeToByte(const ASizeString: string; const ASizeType: string = ''; ADotAsDecimalSeparator: Boolean = True): Int64;
var
  LFormatSettings: TFormatSettings;
  LSizeString: string;
  LSize: Extended;
begin
  // TextToFloat ignores ThousandSeparator
  // see: http://qc.embarcadero.com/wc/qcmain.aspx?d=92265
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, LFormatSettings);
  if ADotAsDecimalSeparator then
  begin
    LFormatSettings.DecimalSeparator := '.';
    LSizeString := StringReplace(ASizeString, ',', '', [rfReplaceAll]);
  end
  else
  begin
    LFormatSettings.DecimalSeparator := ',';
    LSizeString := StringReplace(ASizeString, '.', '', [rfReplaceAll]);
  end;

  LSize := StrToFloatDef(LSizeString, 0, LFormatSettings);
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
