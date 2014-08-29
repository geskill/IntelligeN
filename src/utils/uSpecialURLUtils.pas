unit uSpecialURLUtils;

interface

uses
  SysUtils,
  IdHTTP;

  FUNCTION IsURL(const s:String):Boolean;

  FUNCTION ExtractUrlFileName(const AUrl:String):String;

  FUNCTION UrlFilesize(Link:String):Int64;

implementation

FUNCTION IsURL(const s:String):Boolean;
BEGIN
  result := Pos('://', s) > 0;
END;

FUNCTION ExtractUrlFileName(const AUrl:String):String;
VAR I:Integer;
BEGIN
  I := LastDelimiter('/', AUrl);
  result := copy(AUrl, I + 1, Length(AUrl) - (i));
END;

FUNCTION UrlFilesize(Link:String):Int64;
VAR IdHTTP:TIdHTTP;
BEGIN
  IdHTTP := TIdHTTP.Create(NIL);
  TRY
    IdHTTP.Request.ContentType := 'application/octet-stream';

    TRY
      IdHTTP.Head(Link);
    EXCEPT
      result := 0;
      Exit;
    END;
    
    result := IdHTTP.Response.ContentLength;
  FINALLY
    IdHTTP.Free;
  END;
END;

end.
