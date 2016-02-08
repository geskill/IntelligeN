unit uConvertToNewIScript;

interface

uses
  // Delphi
  SysUtils, Classes, StrUtils,
  
  RegExpr,
  // API
  uApiIScriptFormatter;

procedure Convert(AOldFile, ANewFile: TFileName; AFormat: Boolean);

implementation

procedure Convert(AOldFile, ANewFile: TFileName; AFormat: Boolean);
const
  ParseTagStart = '<I';
  ParseTagEnd = 'I>';

var
  StringList: TStringList;

  StartPoint, Offset, CopyLength, EndTagPoint: Integer;
  PlainCode, FullScriptCode, ScriptCode: string;
  EndofFile: Boolean;
begin
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(AOldFile);

    EndofFile := False;

    // Convert all .. <I __ I> ..
    PlainCode := '';
    FullScriptCode := '';
    ScriptCode := '';
    StartPoint := 1;
    // erster Starttag
    Offset := Pos(ParseTagStart, StringList.Text);

    // ist Text schon im neuen Format ohne <I  I>
    if not(Offset = 0) then
    begin

      // solange nicht am Ende der Datei
      repeat
        if PosEx(ParseTagStart, StringList.Text, Offset) > PosEx(ParseTagEnd, StringList.Text, Offset) then
        begin
          // Zwei Script Starts hintereinander
          raise Exception.Create('IScript close tag expected (2).');
        end;

        CopyLength := Offset - StartPoint;

        if not(CopyLength = 0) then
        begin
          PlainCode := copy(StringList.Text, StartPoint, CopyLength);

          PlainCode := StringReplaceMultiple(PlainCode, ['"', '\'], ['\"', '\\']);
          PlainCode := StringReplace(PlainCode, sLineBreak, '" + "\r\n" + "', [rfReplaceAll]);
          PlainCode := StringReplace(PlainCode, #10, '" + "\r\n" + "', [rfReplaceAll]);
          PlainCode := StringReplace(PlainCode, #13, '" + "\r\n" + "', [rfReplaceAll]);

          PlainCode := '"' + PlainCode + '"';

          if StartsStr('"" +', PlainCode) then
          begin
            Delete(PlainCode, 1, 4);
            PlainCode := TrimLeft(PlainCode);
          end;

          if EndsStr('+ ""', PlainCode) then
          begin
            Delete(PlainCode, Length(PlainCode) - 3, 4);
            PlainCode := TrimRight(PlainCode);
          end;

          FullScriptCode := FullScriptCode + 'print(' + StringReplace(PlainCode, '+ "" +', '+', [rfReplaceAll]) + ');';
        end;

        EndTagPoint := PosEx(ParseTagEnd, StringList.Text, Offset);

        if (EndTagPoint = 0) then
        begin
          // Kein Script Ende
          Break;
        end;

        ScriptCode := copy(StringList.Text, Offset, EndTagPoint - Offset + Length(ParseTagEnd));

        // Script Tag Anfang löschen
        Delete(ScriptCode, 1, Length(ParseTagStart));
        // Script Tag Ende löschen
        Delete(ScriptCode, Length(ScriptCode) - Length(ParseTagEnd) + 1, Length(ParseTagEnd));

        ScriptCode := sLineBreak + Trim(ScriptCode);

        if EndsStr(')', ScriptCode) then
          ScriptCode := ScriptCode + ';';

        FullScriptCode := FullScriptCode + ScriptCode + sLineBreak;

        StartPoint := EndTagPoint + Length(ParseTagEnd);
        Offset := PosEx(ParseTagStart, StringList.Text, StartPoint);
        if Offset = 0 then
          Offset := Length(StringList.Text);
      until EndofFile;

      StringList.Text := FullScriptCode;
    end;

    StringList.Text := StringReplaceMultiple(StringList.Text, ['IMirrorController.MirrorCount', 'DirectlinksMirrorCount', 'IMirrorController', '].Link'], ['IMirror.Count', 'DirectlinkCount', 'IMirror', '].Value'], False);

    StringList.Text := StringReplace(StringList.Text, 'IMirror.Mirror', 'IMirror', [rfReplaceAll, rfIgnoreCase]);

    with TRegExpr.Create do
      try
        ModifierI := True;

        Expression := '\.DirectlinksMirror\[(.*?)\]';
        StringList.Text := Replace(StringList.Text, '.Directlink[$1].Value', True);
      finally
        Free;
      end;

    StringList.Text := StringReplace(StringList.Text, 'IARTIST', 'ICREATOR', [rfReplaceAll, rfIgnoreCase]);

    StringList.Text := StringReplace(StringList.Text, 'print(#13#10);', 'print("\r\n");', [rfReplaceAll, rfIgnoreCase]);

    if AFormat then
      StringList.Text := TIScriptFormatter.Format(StringList.Text);

    StringList.SaveToFile(ANewFile);
  finally
    StringList.Free;
  end;
end;

end.
