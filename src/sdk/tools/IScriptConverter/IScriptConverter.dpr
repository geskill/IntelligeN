program IScriptConverter;
{$APPTYPE CONSOLE}

uses
  SysUtils,
  uConvertToNewIScript in 'uConvertToNewIScript.pas',
  uApiIScriptFormatter in 'uApiIScriptFormatter.pas';

var
  OldFile, NewFile: TFileName;

begin
  try
    Writeln('IntelligeN 2009 IScript Converter for BUILD 129 Release C');
    Writeln('(c) 2013 Sebastian Klatte');
    Writeln('');
    Writeln('');
    Writeln('Usage: <OldFile> <NewFile> [-format]');

    if ParamCount > 1 then
    begin
      OldFile := ParamStr(1);
      if not FileExists(OldFile) then
      begin
        raise Exception.Create('IScript file not found.');
      end
      else
      begin
        NewFile := ParamStr(2);

        if FileExists(NewFile) then
        begin
          raise Exception.Create('New IScript file already exists.');
        end
        else
          Convert(OldFile, NewFile, (ParamCount > 2) and SameStr('-format', ParamStr(3)));
      end;
    end
    else
      raise Exception.Create('No IScript file assigned.');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
